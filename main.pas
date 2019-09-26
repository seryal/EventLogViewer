unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, dateutils, Dialogs,
  ComCtrls, ExtCtrls, Menus, ActnList, PairSplitter, Windows, syeventlogreader,
  VirtualTrees, AnchorDockPanel;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    acSystemLog: TAction;
    acOpenFile: TAction;
    acSaveLog: TAction;
    acClearLog: TAction;
    acExit: TAction;
    acGotoTime: TAction;
    acFind: TAction;
    acFilter: TAction;
    acSetTimeZone: TAction;
    acOnTop: TAction;
    acShowHeaders: TAction;
    acShowStatus: TAction;
    acDescription: TAction;
    acApplicationLog: TAction;
    acSecurityLog: TAction;
    acView1_4: TAction;
    acView1_3: TAction;
    acView1_2: TAction;
    acView1_6: TAction;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    N3: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    N2: TMenuItem;
    N1: TMenuItem;
    pnlDescription: TPanel;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    vtEventLog: TVirtualStringTree;
    procedure acApplicationLogExecute(Sender: TObject);
    procedure acClearLogExecute(Sender: TObject);
    procedure acDescriptionExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acOnTopExecute(Sender: TObject);
    procedure acOpenFileExecute(Sender: TObject);
    procedure acSecurityLogExecute(Sender: TObject);
    procedure acShowHeadersExecute(Sender: TObject);
    procedure acShowStatusExecute(Sender: TObject);
    procedure acSystemLogExecute(Sender: TObject);
    procedure acView1_2Execute(Sender: TObject);
    procedure acView1_3Execute(Sender: TObject);
    procedure acView1_4Execute(Sender: TObject);
    procedure acView1_6Execute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure vtEventLogBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vtEventLogDblClick(Sender: TObject);
    procedure vtEventLogFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vtEventLogFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtEventLogGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
  private
    FMenuVisible: boolean;
    procedure SetMenuVisible(AValue: boolean);
    procedure SetWindowsHeigth(APart: integer);
    procedure StartLog;
  private
    FEventlog: TsyEventLogReader;
    FFormatSettings: TFormatSettings;
    FLastFocusFlag: boolean;
    procedure OnBeginUpdate(Sender: TObject);
    procedure OnEndUpdate(Sender: TObject);
    procedure OnEventLogRecord(Sender: TObject; ALogRecord: TsyEventLogRecord);
    property MenuVisible: boolean read FMenuVisible write SetMenuVisible;

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  vtEventLog.NodeDataSize := sizeof(TsyEventLogRecord);
  FFormatSettings := DefaultFormatSettings;
  FFormatSettings.LongTimeFormat := 'hh:mm:ss';
  FLastFocusFlag := True;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (ssAlt in Shift) then
  begin
    MenuVisible := not MenuVisible;
    vtEventLog.SetFocus;
  end;
  if (key = VK_ESCAPE) then
  begin
    acDescription.Checked := False;
    acDescriptionExecute(self);
    MenuVisible := False;
  end;
end;



procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(FEventlog);
end;

procedure TfrmMain.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.acOnTopExecute(Sender: TObject);
begin
  if acOnTop.Checked then
    FormStyle := fsSystemStayOnTop
  else
    FormStyle := fsNormal;

end;

procedure TfrmMain.acOpenFileExecute(Sender: TObject);
begin

end;

procedure TfrmMain.acSecurityLogExecute(Sender: TObject);
begin
  FEventlog.Stop;
  vtEventLog.Clear;
  FEventlog.LogName := 'Security';
    StartLog;
end;


procedure TfrmMain.acShowHeadersExecute(Sender: TObject);
begin
  if acShowHeaders.Checked then
    vtEventLog.Header.Options := vtEventLog.Header.Options + [hoVisible]
  else
    vtEventLog.Header.Options := vtEventLog.Header.Options - [hoVisible];

end;

procedure TfrmMain.acShowStatusExecute(Sender: TObject);
begin
  StatusBar1.Visible := acShowStatus.Checked;
end;

procedure TfrmMain.acSystemLogExecute(Sender: TObject);
begin
  FEventlog.Stop;
  vtEventLog.Clear;
  FEventlog.LogName := 'System';
  StartLog;

end;

procedure TfrmMain.acView1_2Execute(Sender: TObject);
begin
  SetWindowsHeigth(2);
end;

procedure TfrmMain.acView1_3Execute(Sender: TObject);
begin
  SetWindowsHeigth(3);

end;

procedure TfrmMain.acView1_4Execute(Sender: TObject);
begin
  SetWindowsHeigth(4);

end;

procedure TfrmMain.acView1_6Execute(Sender: TObject);
begin
  SetWindowsHeigth(6);

end;

procedure TfrmMain.acDescriptionExecute(Sender: TObject);
begin
  pnlDescription.Visible := acDescription.Checked;
  //  pnlDescription.Height := 0;
  if acDescription.Checked then
    Splitter1.Height := 5
  else
    Splitter1.Height := 1;

end;

procedure TfrmMain.acApplicationLogExecute(Sender: TObject);
begin
  FEventlog.Stop;
  vtEventLog.Clear;
  FEventlog.LogName := 'Application';
  StartLog;
end;

procedure TfrmMain.acClearLogExecute(Sender: TObject);
begin

end;



procedure TfrmMain.FormShow(Sender: TObject);
begin
  FEventlog := TsyEventLogReader.Create;
  FEventlog.ComputerName := '';
  FEventlog.LogName := 'System';
  FEventlog.OnEventLogRecord := @OnEventLogRecord;
  FEventlog.OnBeginUpdate := @OnBeginUpdate;
  FEventlog.OnEndUpdate := @OnEndUpdate;
  StartLog;
  MenuVisible := False;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  FEventlog.Proceed;
end;

procedure TfrmMain.vtEventLogBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  Data: PsyEventLogRecord;
begin
  Data := vtEventLog.GetNodeData(node);
  if not assigned(Data) then
    exit;
  case Data^.EventType of
    EVENTLOG_ERROR_TYPE:
      TargetCanvas.Brush.Color := $CCCCFF;
    EVENTLOG_AUDIT_FAILURE:
      TargetCanvas.Brush.Color := $FFCCFF;
    EVENTLOG_AUDIT_SUCCESS:
      TargetCanvas.Brush.Color := $CCFFFF;
    EVENTLOG_INFORMATION_TYPE:
      TargetCanvas.Brush.Color := $FFCCCC;
    EVENTLOG_WARNING_TYPE:
      TargetCanvas.Brush.Color := $CCFFCC;

  end;

  TargetCanvas.Brush.Style := bsSolid;


  TargetCanvas.FillRect(CellRect);
end;

procedure TfrmMain.vtEventLogDblClick(Sender: TObject);
begin
  if not acDescription.Checked then
  begin
    acDescription.Checked := True;
    acDescriptionExecute(self);
  end;
end;

procedure TfrmMain.vtEventLogFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PsyEventLogRecord;
begin
  if Node = vtEventLog.GetLast() then
    FLastFocusFlag := True
  else
    FLastFocusFlag := False;
  Data := vtEventLog.GetNodeData(Node);
  if Assigned(Data) then
    Memo1.Lines.Text := Data^.MessageText;
end;

procedure TfrmMain.vtEventLogFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PsyEventLogRecord;
begin
  Data := vtEventLog.GetNodeData(Node);
  if not Assigned(Data) then
    exit;
  Data^.Source := '';
  Data^.MessageText := '';
end;

procedure TfrmMain.vtEventLogGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  Data: PsyEventLogRecord;
begin
  Data := vtEventLog.GetNodeData(Node);
  if not Assigned(Data) then
    exit;
  case column of
    0:
      CellText := Data^.Source;
    1:
    begin

      CellText := DateTimeToStr(Data^.DateTime, FFormatSettings);

    end;
    2:
      CellText := Data^.MessageText;
  end;
end;

procedure TfrmMain.SetMenuVisible(AValue: boolean);
begin
  FMenuVisible := AValue;
  MenuItem1.Visible := AValue;
  MenuItem6.Visible := AValue;
  MenuItem11.Visible := AValue;
end;

procedure TfrmMain.SetWindowsHeigth(APart: integer);
var
  mW, mH: integer;
  clSize: integer;
  clCaption: integer;
begin
  clSize := GetSystemMetrics(SM_CYSIZEFRAME);
  clCaption := GetSystemMetrics(SM_CYCAPTION);
  mh := Screen.WorkAreaHeight - clSize - clCaption;
  mw := screen.WorkAreaWidth;
  Left := 0 - clSize;
  top := mh - trunc(mH / APart);
  Width := mw;
  Height := trunc(mH / APart);

end;

procedure TfrmMain.StartLog;
begin
  try
    FEventlog.Start;

  except
    on e: Exception do
      ShowMessage(e.Message);
  end;
end;



procedure TfrmMain.OnEventLogRecord(Sender: TObject; ALogRecord: TsyEventLogRecord);
var
  Node: PVirtualNode;
  Data: PsyEventLogRecord;
begin
  node := vtEventLog.AddChild(nil);
  Data := vtEventLog.GetNodeData(Node);
  //  vtEventLog.;
  Data^ := ALogRecord;
  //  Memo1.Lines.Add(AMessage);
end;

procedure TfrmMain.OnBeginUpdate(Sender: TObject);
begin
  vtEventLog.BeginUpdate;
end;

procedure TfrmMain.OnEndUpdate(Sender: TObject);
begin
  vtEventLog.EndUpdate;
  if FLastFocusFlag then
  begin
    vtEventLog.ClearSelection;
    vtEventLog.FocusedNode := vtEventLog.GetLast();
    vtEventLog.Selected[vtEventLog.GetLast()] := True;
  end;
  vtEventLog.Refresh;
  StatusBar1.Panels.Items[0].Text := 'Events: ' + IntToStr(FEventlog.EventCount);

end;

end.










