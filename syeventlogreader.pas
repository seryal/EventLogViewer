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
    FLastEventRecord: cardinal;
    FEventLogHandle: THandle;
    FComputerName: string;
    FLogName: string;
    FOnEventLogRecord: TsyOnEventLogRecord;
    FOnBeginUpdate: TNotifyEvent;
    FOnEndUpdate: TNotifyEvent;
    procedure SetComputerName(AValue: string);
    procedure SetLogName(AValue: string);
    procedure OpenLog;
    procedure CloseLog;
    procedure ReadLog(ARecNumber: integer);
    function GetMessageDllPath(ASource: string): string;
    function GetFormatMessage(AID: DWORD; var AArgs: PPWideChar; const ADllName: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    property ComputerName: string read FComputerName write SetComputerName;
    property LogName: string read FLogName write SetLogName;
    procedure Start;
    procedure Proceed;
    procedure Stop;
    property EventCount: cardinal read FEventCount write FEventCount;
    property OnEventLogRecord: TsyOnEventLogRecord read FOnEventLogRecord write FOnEventLogRecord;
    property OnBeginUpdate: TNotifyEvent read FOnBeginUpdate write FOnBeginUpdate;
    property OnEndUpdate: TNotifyEvent read FOnEndUpdate write FOnEndUpdate;
  end;

implementation

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
var
  ALogName: WideString;
  AComputerName: WideString;
  err: DWORD;
begin
  FEventCount := 0;
  ALogName := FLogName;
  AComputerName := FComputerName;
  if FEventLogHandle = 0 then
    FEventLogHandle := OpenEventLogW(PWideChar(AComputerName), PWideChar(ALogName));
  err := GetLastError;
  if err <> 0 then
  begin
    RaiseLastWin32Error;
  end;
end;

procedure TsyEventLogReader.CloseLog;
begin
  if FEventLogHandle <> 0 then
  begin
    CloseEventLog(FEventLogHandle);
    FEventLogHandle := 0;
  end;

end;

procedure TsyEventLogReader.ReadLog(ARecNumber: integer);
const
  MAX_BUFFER = $7FFFF;
var
  Flags: DWORD;
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
  _ReadRec: integer;
begin
  buffer := nil;
  getMem(Buffer, MAX_BUFFER);
  // First Read for get required buffer size
  res := True;
  if ARecNumber = 0 then
    Flags := EVENTLOG_SEQUENTIAL_READ or EVENTLOG_FORWARDS_READ
  else
    Flags := EVENTLOG_SEEK_READ or EVENTLOG_FORWARDS_READ;
  _ReadRec := ARecNumber;
  while res do
  begin
    if Assigned(FOnBeginUpdate) then
      OnBeginUpdate(Self);
    Res := ReadEventLogW(FEventLogHandle, flags, _ReadRec, Buffer, MAX_BUFFER, pnBytesRead, pnMinNumberOfBytesNeeded);
    if not res then
    begin
      LastErr := GetLastError;
      case LastErr of
        ERROR_INSUFFICIENT_BUFFER:
        begin
          pnBytesNeeded := pnMinNumberOfBytesNeeded;
          ReallocMem(Buffer, pnBytesNeeded);
          Res := ReadEventLogW(FEventLogHandle, flags, 0, Buffer, pnBytesNeeded, pnBytesRead, pnMinNumberOfBytesNeeded);

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
      while pointer(pRecord) < Buffer + pnBytesRead do
      begin
        OutStr := '';
        pStr := PWideChar(pointer(pRecord) + pRecord^.StringOffset);
        GetMem(Args, pRecord^.NumStrings * SizeOf(PWideChar));

        PArgs := Args;
        for i := 1 to pRecord^.NumStrings do
        begin
          PArgs^ := pStr;
          OutStr := OutStr + pStr;
          if i < pRecord^.NumStrings then
            OutStr := OutStr + ' ';// #10#13;
          j := strlen(pStr);
          pStr := pStr + j + 1;
          Inc(PArgs);
        end;
        Source := PWideChar(pointer(pRecord) + SizeOf(TEventLogRecord));
        MessageDllPath := GetMessageDllPath(Source);
        if MessageDllPath <> '' then
        begin
          tmp := '';
          repeat
            I := Pos(';', MessageDllPath);
            if I <> 0 then
            begin
              tmp := tmp + GetFormatMessage(pRecord^.EventID, Args, Copy(MessageDllPath, 1, I - 1));
              MessageDllPath := Copy(MessageDllPath, I + 1, MaxInt);
            end
            else
              tmp := tmp + GetFormatMessage(pRecord^.EventID, Args, MessageDllPath);
          until I = 0;
        end;
        if tmp <> '' then
          OutStr := tmp;
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
        FLastEventRecord := pRecord^.RecordNumber;
        _ReadRec := FLastEventRecord + 1;
        pRecord := PEVENTLOGRECORD(pointer(pRecord) + pRecord^.Length);

        Inc(FEventCount);
      end;
    end;

    if Assigned(FOnEndUpdate) then
      OnEndUpdate(Self);
  end;
  Freemem(Buffer);
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
  err: integer;
  FormatFlag: DWORD;
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
          FormatFlag := FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ARGUMENT_ARRAY;
          //          FormatFlag := FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_ARGUMENT_ARRAY;
          if FormatMessageW(FormatFlag, Pointer(DllModule), AID, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
            @Buffer[0], 4096, @AArgs^) > 0 then
          begin
            OutString := PWideChar(Buffer);
          end
          else
            err := GetLastError;
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
  LogName := 'System';
  FEventLogHandle := 0;
end;

destructor TsyEventLogReader.Destroy;
begin
  Stop;
  inherited Destroy;
end;

procedure TsyEventLogReader.Start;
begin
  OpenLog;
  ReadLog(0);
end;

procedure TsyEventLogReader.Proceed;
var
  Offset: DWORD;
  Recno: integer;
begin
  GetNumberOfEventLogRecords(FEventLogHandle, @Offset);
  if FEventCount >= Offset then
    exit;
  GetOldestEventLogRecord(FEventLogHandle, @Offset);
  RecNo := FEventCount + Offset;
  ReadLog(Recno);




end;

procedure TsyEventLogReader.Stop;
begin
  CloseLog;
end;

end.
