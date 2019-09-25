unit syeventlog;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, Windows;

type
  TNotifyChangeEventLog = class;
  TsyEventLogRecord = class;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TsyEventLog = class(TComponent)
  private
    FLogHandle: THandle;
    FLog: string;
    FServer: string;
    FSource: string;
    FActive: boolean;
    FLastError: cardinal;
    FOnChange: TNotifyEvent;
    FNotifyThread: TNotifyChangeEventLog;
    FEventRecord: TsyEventLogRecord;
    procedure SetActive(Value: boolean);
    procedure SetServer(const Value: string);
    procedure SetSource(const Value: string);
    procedure SetLog(const Value: string);
    function GetEventCount: cardinal;
    procedure SeekRecord(N: cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    procedure First;
    procedure Last;
    function EOF: boolean;
    procedure Next;
    procedure Seek(N: cardinal);
    procedure ReadEventLogs(AStrings: TStrings);
    property EventCount: cardinal read GetEventCount;
    property EventRecord: TsyEventLogRecord read FEventRecord;
  published
    property Server: string read FServer write SetServer;
    property Source: string read FSource write SetSource;
    property Log: string read FLog write SetLog;
    property Active: boolean read FActive write SetActive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TNotifyChangeEventLog = class(TThread)
  private
    FEventLog: TsyEventLog;
    FEventHandle: THandle;
    procedure DoChange;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TComponent);
  end;

  TsyEventLogRecord = class(TObject)
  private
    FEventLog: TsyEventLog;
    FCurrentRecord: Pointer;
    FOwner: TComponent;
    function GetRecordNumber: cardinal;
    function GetDateTime: TDateTime;
    function GetID: DWORD;
    function GetType: string;
    function GetStringCount: DWORD;
    function GetCategory: cardinal;
    function GetSource: string;
    function GetComputer: string;
    function GetSID: PSID;
    function GetString(Index: cardinal): string;
    function GetMessageText: string;
    function GetUsername: string;
  public
    constructor Create(AOwner: TComponent);
    property RecordNumber: cardinal read GetRecordNumber;
    property DateTime: TDateTime read GetDateTime;
    property EventType: string read GetType;
    property Category: cardinal read GetCategory;
    property Source: string read GetSource;
    property Computer: string read GetComputer;
    property ID: DWORD read GetID;
    property StringCount: DWORD read GetStringCount;
    property SID: PSID read GetSID;
    property EventString[Index: cardinal]: string read GetString;
    property MessageText: string read GetMessageText;
    property UserName: string read GetUsername;
    property Owner: TComponent read FOwner;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jvcl.svn.sourceforge.net/svnroot/jvcl/branches/JVCL3_47_PREPARATION/run/JvNTEventLog.pas $';
    Revision: '$Revision: 13104 $';
    Date: '$Date: 2011-09-07 08:50:43 +0200 (mer. 07 sept. 2011) $';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  Registry;

const
  EVENTLOG_SEQUENTIAL_READ = $0001;
  EVENTLOG_SEEK_READ = $0002;
  EVENTLOG_FORWARDS_READ = $0004;
  EVENTLOG_BACKWARDS_READ = $0008;

  cEventLogBaseKey = 'SYSTEM\CurrentControlSet\Services\EventLog';

type
  PEventLogRecord = ^TEventLogRecord;

  TEventLogRecord = record
    Length: DWORD; // Length of full record
    Reserved: DWORD; // Used by the service
    RecordNumber: DWORD; // Absolute record number
    TimeGenerated: DWORD; // Seconds since 1-1-1970
    TimeWritten: DWORD; // Seconds since 1-1-1970
    EventID: DWORD;
    EventType: word;
    NumStrings: word;
    EventCategory: word;
    ReservedFlags: word; // For Use with paired events (auditing)
    ClosingRecordNumber: DWORD; // For Use with paired events (auditing)
    StringOffset: DWORD; // Offset from beginning of record
    UserSidLength: DWORD;
    UserSidOffset: DWORD;
    DataLength: DWORD;
    DataOffset: DWORD; // Offset from beginning of record
  end;

//=== { TsyEventLog } ======================================================

constructor TsyEventLog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLog := '';
  FSource := '';
  FOnChange := nil;
  FNotifyThread := nil;
  FEventRecord := TsyEventLogRecord.Create(Self);
end;

destructor TsyEventLog.Destroy;
begin
  Close;
  FEventRecord.Free;
  inherited Destroy;
end;

procedure TsyEventLog.SetActive(Value: boolean);
begin
  if Value <> FActive then
  begin
    if csDesigning in ComponentState then
      FActive := Value
    else
    if Value then
      Open
    else
      Close;
  end;
end;

procedure TsyEventLog.SetServer(const Value: string);
var
  OldActive: boolean;
begin
  if FServer <> Value then
  begin
    OldActive := Active;
    Active := False;
    FServer := Value;
    Active := OldActive;
  end;
end;

procedure TsyEventLog.SetSource(const Value: string);
var
  OldActive: boolean;
begin
  if FSource <> Value then
  begin
    OldActive := Active;
    Active := False;
    FSource := Value;
    Active := OldActive;
  end;
end;

procedure TsyEventLog.SetLog(const Value: string);
var
  OldActive: boolean;
begin
  if FLog <> Value then
  begin
    OldActive := Active;
    Active := False;
    FLog := Value;
    Active := OldActive;
  end;
end;

function TsyEventLog.GetEventCount: cardinal;
begin
  if Active then
    GetNumberOfEventLogRecords(FLogHandle, @Result)
  else
    Result := 0;
end;

procedure TsyEventLog.Open;
begin
  if Source <> '' then
  begin
    Close;
    FLogHandle := OpenEventLogW(PWideChar(Server), PWideChar(Source));
    if FLogHandle = 0 then
      RaiseLastOSError;
    FNotifyThread := TNotifyChangeEventLog.Create(Self);
    FActive := True;
  end;
end;

procedure TsyEventLog.Close;
begin
  if FLogHandle <> 0 then
  begin
    if FNotifyThread <> nil then
      FNotifyThread.Terminate;
    CloseEventLog(FLogHandle);
    FLogHandle := 0;
    FreeAndNil(FNotifyThread);
  end;
  FreeMem(FEventRecord.FCurrentRecord);
  FEventRecord.FCurrentRecord := nil;
  FActive := False;
end;

procedure TsyEventLog.First;
begin
  SeekRecord(0);
end;

procedure TsyEventLog.Last;
begin
  SeekRecord(GetEventCount - 1);
end;

function TsyEventLog.EOF: boolean;
begin
  Result := (EventRecord.FCurrentRecord = nil) or (EventRecord.RecordNumber = GetEventCount) or (FLastError = ERROR_HANDLE_EOF);
end;

procedure TsyEventLog.Next;
var
  BytesRead, BytesNeeded, Flags: DWORD;
  Dummy: char;
begin
  Flags := EVENTLOG_SEQUENTIAL_READ;
  Flags := Flags or EVENTLOG_FORWARDS_READ;

  ReadEventLogw(FLogHandle, Flags, 0, @Dummy, 0, BytesRead, BytesNeeded);
  FLastError := GetLastError;
  if FLastError = ERROR_INSUFFICIENT_BUFFER then
  begin
    ReallocMem(FEventRecord.FCurrentRecord, BytesNeeded);
    if not ReadEventLogW(FLogHandle, Flags, 0, FEventRecord.FCurrentRecord, BytesNeeded, BytesRead, BytesNeeded) then
      RaiseLastOSError;
  end
  else
  if FLastError <> ERROR_HANDLE_EOF then
    RaiseLastOSError;
end;

procedure TsyEventLog.SeekRecord(N: cardinal);
var
  Offset, Flags: DWORD;
  BytesRead, BytesNeeded: cardinal;
  Dummy: char;
  RecNo: integer;
  pRecord: PEventLogRecord;
begin
  GetOldestEventLogRecord(FLogHandle, @Offset);
  RecNo := N + Offset;

  Flags := EVENTLOG_SEEK_READ;
  Flags := Flags or EVENTLOG_FORWARDS_READ;

  ReadEventLogW(FLogHandle, Flags, RecNo, @Dummy, 0, BytesRead, BytesNeeded);
  FLastError := GetLastError;
  if FLastError = ERROR_INSUFFICIENT_BUFFER then
  begin
    ReallocMem(FEventRecord.FCurrentRecord, BytesNeeded);
    if not ReadEventLogW(FLogHandle, Flags, RecNo, FEventRecord.FCurrentRecord, BytesNeeded, BytesRead, BytesNeeded) then
      RaiseLastOSError;
  end
  else
  if FLastError <> ERROR_HANDLE_EOF then
    RaiseLastOSError;
end;

procedure TsyEventLog.Seek(N: cardinal);
begin
  if N <> FEventRecord.RecordNumber then
    SeekRecord(N);
end;

procedure TsyEventLog.ReadEventLogs(AStrings: TStrings);
begin
  with TRegistry.Create do
  begin
    AStrings.BeginUpdate;
    try
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKey(cEventLogBaseKey, False);
      GetKeyNames(AStrings);
    finally
      Free;
      AStrings.EndUpdate;
    end;
  end;
end;

//=== { TNotifyChangeEventLog } ==============================================

constructor TNotifyChangeEventLog.Create(AOwner: TComponent);
begin
  inherited Create(False);

  // initialize system events
  FEventLog := TsyEventLog(AOwner);
  FEventHandle := CreateEvent(nil, True, False, nil);
  NotifyChangeEventLog(FEventLog.FLogHandle, FEventHandle);
  //ThreadName := Format('%s: %s', [ClassName, AOwner.Name]);
end;

procedure TNotifyChangeEventLog.DoChange;
begin
  if Assigned(FEventLog.FOnChange) then
    FEventLog.FOnChange(FEventLog);
end;

procedure TNotifyChangeEventLog.Execute;
var
  LResult: DWORD;
begin
  // (rom) secure thread against exceptions
  //NameThread(ThreadName);
  LResult := WAIT_OBJECT_0;
  try
    while not Terminated do
    begin
      // reset event signal if we're here for any other reason than a
      // timeout, so we can get it again
      if LResult <> WAIT_TIMEOUT then
        ResetEvent(FEventHandle);
      // wait for event to happen
      LResult := WaitForSingleObject(FEventHandle, 100);
      if LResult = WAIT_OBJECT_0 then
        Synchronize(@DoChange);
    end;
  except
  end;
end;

//=== { TsyEventLogRecord } ================================================

constructor TsyEventLogRecord.Create(AOwner: TComponent);
begin
  // (rom) added inherited Create
  inherited Create;
  FEventLog := TsyEventLog(AOwner);
  FCurrentRecord := nil;
  FOwner := AOwner;
end;

function TsyEventLogRecord.GetRecordNumber: cardinal;
begin
  Result := PEventLogRecord(FCurrentRecord)^.RecordNumber;
end;

function TsyEventLogRecord.GetMessageText: string;
var
  MessagePath: string;
  Count, I: integer;
  pStr: PWideChar;
  Args, PArgs: ^PWideChar;
  St: string;
  reg: TRegistry;
  res: boolean;
  Data: string;
  pRecord: PEVENTLOGRECORD;

  function FormatMessageFrom(const DllName: string): boolean;
  var
    DllModule: THandle;
    Buffer: array [0..2047] of widechar;
    FullDLLName: array [0..MAX_PATH] of widechar;
  begin
    Result := False;
    ExpandEnvironmentStringsW(PWideChar(DllName), FullDLLName, MAX_PATH);
    DllModule := LoadLibraryExW(FullDLLName, 0, LOAD_LIBRARY_AS_DATAFILE);
    if DllModule <> 0 then
    begin
      try
        // (rom) memory leak fixed
        try
          if FormatMessageW(FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_ARGUMENT_ARRAY, Pointer(DllModule), ID,
            0, Buffer, SizeOf(Buffer), va_list(Args)) > 0 then
          begin
            Buffer[StrLen(Buffer) - 2] := #0;
            St := Buffer;
            Result := True;
          end;
        except
          on e: Exception do
        end;
      finally
        FreeLibrary(DllModule);
      end;
    end
    else
    begin
      st := Data;
    end;

  end;

begin
  St := '';
  Count := StringCount;

  GetMem(Args, Count * SizeOf(PWideChar));
  try
    pRecord := FCurrentRecord;
    PArgs := Args;
    //P := PWideChar(FCurrentRecord + PEventLogRecord(FCurrentRecord)^.StringOffset);
    pStr := PWideChar(pointer(pRecord) + pRecord^.StringOffset);

    Data := '';
    for I := 0 to Count - 1 do
    begin
      //      PArgs^ := Pstr;
      Data := Data + string(pStr);
      pstr := pstr + Length(string(pstr)) + 1;
      if i < Count - 1 then
        Data := Data + #13#10;
      //      Inc(P, lstrlenW(P) + 1);

      Inc(PArgs);
    end;

    reg := TRegistry.Create(KEY_READ);
    try
      reg.RootKey := HKEY_LOCAL_MACHINE;
      res := reg.OpenKey(Format('%s\%s\%s', [cEventLogBaseKey, FEventLog.Log, Source]), False); {rw}
      //      OpenKey(Format('SYSTEM\CurrentControlSet\Services\EventLog\%s\%s', [FEventLog.Log, FEventLog.Source]), False);
      MessagePath := reg.ReadString('EventMessageFile');
    finally
      reg.Free;
    end;

    repeat
      I := Pos(';', MessagePath);
      if I <> 0 then
      begin
        if FormatMessageFrom(Copy(MessagePath, 1, I - 1)) then {rw}
          //          if FormatMessageFrom(Copy(MessagePath, 1, I)) then
          Break;
        MessagePath := Copy(MessagePath, I + 1, MaxInt); {rw}
        //          MessagePath := Copy(MessagePath, I, MaxInt);
      end
      else
        FormatMessageFrom(MessagePath);
    until I = 0;
  finally
    FreeMem(Args)
  end;
  Result := St;
end;

function TsyEventLogRecord.GetUsername: string;
var
  UserName_: array [0..512] of char;
  UserNameLen: cardinal;
  DomainName: array [0..512] of char;
  DomainNameLen: cardinal;
  Use: SID_NAME_USE;
  UserSID: PSID;

begin
  Result := '';
{
  UserSID := SID;
  if Assigned(UserSID) then
  begin
    UserNameLen := SizeOf(UserName);
    DomainNameLen := SizeOf(DomainName);
    if LookupAccountSID(nil, UserSID, UserName_, UserNameLen, DomainName, DomainNameLen, Use) then
      Result := string(DomainName) + '\' + string(UserName_);
  end
  else
  begin
    Result := RsLogUserSIDNotFound;
  end;
}
  Result := 'USER';
end;

function TsyEventLogRecord.GetType: string;
begin
  case PEventLogRecord(FCurrentRecord)^.EventType of
    EVENTLOG_ERROR_TYPE:
      Result := 'Error';
    EVENTLOG_WARNING_TYPE:
      Result := 'Warning';
    EVENTLOG_INFORMATION_TYPE:
      Result := 'Information';
    EVENTLOG_AUDIT_SUCCESS:
      Result := 'Audit';
    EVENTLOG_AUDIT_FAILURE:
      Result := 'Failure Audit';
    else
      Result := '';
  end;
end;

function TsyEventLogRecord.GetSource: string;
begin
  Result := PWideChar(FCurrentRecord + SizeOf(TEventLogRecord));
end;

function TsyEventLogRecord.GetComputer: string;
var
  P: PWideChar;
begin
  P := PWideChar(FCurrentRecord + SizeOf(TEventLogRecord));
  Result := P + StrLen(P) + 1;
end;

function TsyEventLogRecord.GetID: DWORD;
begin
  Result := PEventLogRecord(FCurrentRecord)^.EventID;
end;

function TsyEventLogRecord.GetStringCount: DWORD;
begin
  Result := PEventLogRecord(FCurrentRecord)^.NumStrings;
end;

function TsyEventLogRecord.GetCategory: cardinal;
begin
  Result := PEventLogRecord(FCurrentRecord)^.EventCategory;
end;

function TsyEventLogRecord.GetSID: PSID;
begin
  Result := nil;
  if PEventLogRecord(FCurrentRecord)^.UserSidLength > 0 then
    Result := PSID(PWideChar(FCurrentRecord) + PEventLogRecord(FCurrentRecord)^.UserSidOffset);
end;

function TsyEventLogRecord.GetString(Index: cardinal): string;
var
  P: PWideChar;
begin
  Result := '';
  if Index < StringCount then
  begin
    P := PWideChar(FCurrentRecord + PEventLogRecord(FCurrentRecord)^.StringOffset);
    while Index > 0 do
    begin
      Inc(P, StrLen(P) + 1);
      Dec(Index);
    end;
    Result := StrPas(P);
  end;
end;

function TsyEventLogRecord.GetDateTime: TDateTime;
const
  StartPoint: TDateTime = 25569.0; // January 1, 1970 00:00:00
begin
  // Result := IncSecond(StartPoint, PEventLogRecord(FCurrentRecord)^.TimeGenerated);
  //  Result := IncSecond(StartPoint, PEventLogRecord(FCurrentRecord)^.TimeWritten);
  Result := ((StartPoint * 86400.0) + PEventLogRecord(FCurrentRecord)^.TimeWritten) / 86400.0;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
