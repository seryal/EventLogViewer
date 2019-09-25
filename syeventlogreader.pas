unit syeventlogreader;

{$mode objfpc}{$H+}



interface

uses
  Classes, SysUtils, Windows, registry, dateutils;

const
  EVENT_LOG_KEY = 'SYSTEM\CurrentControlSet\Services\EventLog\%s\%s';
  DATE_01_01_1970 = 25569;

type


  TsyEventLogRecord = record
    DateTime: TDateTime;
    EventType: cardinal;
    Category: cardinal;
    Source: string;
    Computer: string;
    ID: DWORD;
    StringCount: DWORD;
    SID: PSID;
    MessageText: string;
    UserName: string;
  end;
  PsyEventLogRecord = ^TsyEventLogRecord;

  TsyOnEventLogRecord = procedure(Sender: TObject; ALogRecord: TsyEventLogRecord) of object;

  { TsyEventLogReader }

  TsyEventLogReader = class
  private
    FEventCount: cardinal;
    FEventLog: THandle;
    FComputerName: string;
    FLogName: string;
    FOnEventLogRecord: TsyOnEventLogRecord;
    FOnBeginUpdate: TNotifyEvent;
    FOnEndUpdate: TNotifyEvent;
    procedure SetComputerName(AValue: string);
    procedure SetLogName(AValue: string);
    procedure OpenLog;
    procedure CloseLog;
    procedure ReadLog();
    function GetMessageDllPath(ASource: string): string;
    function GetFormatMessage(AID: DWORD; var AArgs: PPWideChar; const ADllName: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    property ComputerName: string read FComputerName write SetComputerName;
    property LogName: string read FLogName write SetLogName;
    procedure Start;
    procedure Stop;
    property EventCount: cardinal read FEventCount write FEventCount;
    property OnEventLogRecord: TsyOnEventLogRecord read FOnEventLogRecord write FOnEventLogRecord;
    property OnBeginUpdate: TNotifyEvent read FOnBeginUpdate write FOnBeginUpdate;
    property OnEndUpdate: TNotifyEvent read FOnEndUpdate write FOnEndUpdate;
  end;

implementation

function FormatMessageW(dwFlags: DWORD; lpSource: LPCVOID; dwMessageId: DWORD; dwLanguageId: DWORD; lpBuffer: LPWSTR;
  nSize: DWORD; Arguments: PWideChar): DWORD; external 'kernel32' Name 'FormatMessageW';

{ TsyEventLogReader }

procedure TsyEventLogReader.SetComputerName(AValue: string);
begin
  if FComputerName = AValue then
    Exit;
  FComputerName := AValue;
end;

procedure TsyEventLogReader.SetLogName(AValue: string);
begin
  if FLogName = AValue then
    Exit;
  FLogName := AValue;

end;

procedure TsyEventLogReader.OpenLog;
begin
  if FEventLog = 0 then
    FEventLog := OpenEventLogW(PWideChar(ComputerName), PWideChar(FLogName));
end;

procedure TsyEventLogReader.CloseLog;
begin
  if FEventLog <> 0 then
  begin
    CloseEventLog(FEventLog);
    FEventLog := 0;
  end;

end;

procedure TsyEventLogReader.ReadLog();
const
  MAX_BUFFER = $7FFFF;
var
  Res: WINBOOL;
  pnBytesRead: DWORD;
  pnMinNumberOfBytesNeeded: DWORD;
  pnBytesNeeded: DWORD;
  Buffer: Pointer;
  LastErr: DWORD;
  Count: integer;
  i, j: integer;
  pRecord: PEVENTLOGRECORD;
  pStr: PWideChar;
  OutStr: string;
  Source: string;
  Args, PArgs: ^PWideChar;
  MessageDllPath: string;
  LogRecord: TsyEventLogRecord;
  tmp: string;

begin
  getMem(Buffer, MAX_BUFFER);
  // First Read for get required buffer size
  res := True;
  FEventCount := 0;
  if Assigned(FOnBeginUpdate) then
    OnBeginUpdate(Self);
  while res do
  begin
    Res := ReadEventLogW(FEventLog, EVENTLOG_SEQUENTIAL_READ or EVENTLOG_BACKWARDS_READ, 0, Buffer, MAX_BUFFER,
      pnBytesRead, pnMinNumberOfBytesNeeded);
    if not res then
    begin
      LastErr := GetLastError;
      case LastErr of
        ERROR_INSUFFICIENT_BUFFER:
        begin
          pnBytesNeeded := pnMinNumberOfBytesNeeded;
          ReallocMem(Buffer, pnBytesNeeded);
          res := True;
        end;
        ERROR_HANDLE_EOF:
        begin

        end;
      end;
    end
    else
    begin
      // Data processing
      pRecord := Buffer;
      Args := nil;
      while pRecord < Buffer + pnBytesRead do
      begin
        OutStr := '';
        pStr := PWideChar(pointer(pRecord) + pRecord^.StringOffset);
        GetMem(Args, pRecord^.NumStrings * SizeOf(PWideChar));

        PArgs := Args;
        for i := 1 to pRecord^.NumStrings do
        begin
          PArgs^ := pStr;
          OutStr := OutStr + pStr + #10#13;
          j := strlen(pStr);
          pStr := pStr + j + 1;
          Inc(PArgs);
        end;
        Source := PWideChar(pointer(pRecord) + SizeOf(TEventLogRecord));
        MessageDllPath := GetMessageDllPath(Source);
        if MessageDllPath <> '' then
          OutStr := GetFormatMessage(pRecord^.EventID, Args, MessageDllPath);

        Freemem(Args);
        // Fire EVENT OnEventLogRecord;
        if Assigned(OnEventLogRecord) then
        begin
          LogRecord.MessageText := OutStr;
          LogRecord.Source := Source;
          LogRecord.DateTime := UniversalTimeToLocal(IncSecond(DATE_01_01_1970, pRecord^.TimeWritten));
          LogRecord.ID := pRecord^.EventID;
          LogRecord.Category := pRecord^.EventCategory;
          LogRecord.EventType := pRecord^.EventType;
          FOnEventLogRecord(Self, LogRecord);
        end;
        pRecord := PEVENTLOGRECORD(pointer(pRecord) + pRecord^.Length);
        Inc(FEventCount);
      end;
    end;
  end;
  Freemem(Buffer);
  if Assigned(FOnEndUpdate) then
    OnEndUpdate(Self);
end;


function TsyEventLogReader.GetMessageDllPath(ASource: string): string;
var
  reg: TRegistry;
  res: boolean;
  tmp: string;
begin
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    tmp := Format(EVENT_LOG_KEY, [FLogName, ASource]);
    res := reg.OpenKey(Format(EVENT_LOG_KEY, [FLogName, ASource]), False);
    Result := reg.ReadString('EventMessageFile');
  finally
    reg.Free;
  end;
end;

function TsyEventLogReader.GetFormatMessage(AID: DWORD; var AArgs: PPWideChar; const ADllName: string): string;
var
  DllModule: THandle;
  Buffer: array [0..4095] of widechar;
  Buffer2: PWideChar;
  FullDLLName: array [0..MAX_PATH] of char;
  wDllName: array [0..MAX_PATH] of char;
  OutString: WideString;
begin
  OutString := '';
  if ADllName <> '' then
  begin
    ExpandEnvironmentStrings(PChar(ADllName), FullDLLName, MAX_PATH);
    DllModule := LoadLibraryEx(FullDLLName, 0, LOAD_LIBRARY_AS_DATAFILE);
    if DllModule <> 0 then
    begin
      try
        try
          if FormatMessageW(FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_ARGUMENT_ARRAY, Pointer(DllModule), AID,
            MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), @Buffer[0], 4096, @AArgs^) > 0 then
          begin
            OutString := PWideChar(Buffer);
          end;
        except
          on e: Exception do
        end;
      finally
        FreeLibrary(DllModule);
      end;
    end;
  end;
  Result := OutString;

end;

constructor TsyEventLogReader.Create;
begin
  ComputerName := '';
  LogName := 'Application';
  FEventLog := 0;
end;

destructor TsyEventLogReader.Destroy;
begin
  CloseEventLog(FEventLog);
  inherited Destroy;
end;

procedure TsyEventLogReader.Start;
begin
  OpenLog;
  ReadLog;
end;

procedure TsyEventLogReader.Stop;
begin
  CloseLog;
end;

end.
