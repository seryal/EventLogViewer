unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, dateutils, Dialogs,
  ComCtrls, ExtCtrls, Menus, ActnList, Windows, syeventlogreader, VirtualTrees;

type

  { TForm1 }

  TForm1 = class(TForm)
    acOpenLog: TAction;
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
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
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
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    vtEventLog: TVirtualStringTree;
    procedure acExitExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure vtEventLogBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vtEventLogCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
    procedure vtEventLogFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vtEventLogFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtEventLogGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
  private
    FMenuVisible: boolean;
    procedure SetMenuVisible(AValue: boolean);
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
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  vtEventLog.NodeDataSize := sizeof(TsyEventLogRecord);
  FFormatSettings := DefaultFormatSettings;
  FFormatSettings.LongTimeFormat := 'hh:mm:ss';
  FLastFocusFlag := True;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (ssAlt in Shift) then
  begin
    MenuVisible := not MenuVisible;
    vtEventLog.SetFocus;
  end;
end;



procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(FEventlog);
end;

procedure TForm1.acExitExecute(Sender: TObject);
begin
  Close;
end;



procedure TForm1.FormShow(Sender: TObject);
begin
  FEventlog := TsyEventLogReader.Create;
  FEventlog.ComputerName := '';
  FEventlog.LogName := 'Application';
  FEventlog.OnEventLogRecord := @OnEventLogRecord;
  FEventlog.OnBeginUpdate := @OnBeginUpdate;
  FEventlog.OnEndUpdate := @OnEndUpdate;
  FEventlog.Start;
  MenuVisible := False;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  FEventlog.Proceed;
end;

procedure TForm1.vtEventLogBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
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

procedure TForm1.vtEventLogCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
begin
  if column = 1 then
    Result := -1;
end;

procedure TForm1.vtEventLogFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  if Node = vtEventLog.GetLast() then
    FLastFocusFlag := True
  else
    FLastFocusFlag := False;

end;

procedure TForm1.vtEventLogFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PsyEventLogRecord;
begin
  Data := vtEventLog.GetNodeData(Node);
  if not Assigned(Data) then
    exit;
  Data^.Source := '';
  Data^.MessageText := '';
end;

procedure TForm1.vtEventLogGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
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

procedure TForm1.SetMenuVisible(AValue: boolean);
begin
  FMenuVisible := AValue;
  MenuItem1.Visible := AValue;
  MenuItem6.Visible := AValue;
  MenuItem11.Visible := AValue;
end;



procedure TForm1.OnEventLogRecord(Sender: TObject; ALogRecord: TsyEventLogRecord);
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

procedure TForm1.OnBeginUpdate(Sender: TObject);
begin
  vtEventLog.BeginUpdate;
end;

procedure TForm1.OnEndUpdate(Sender: TObject);
begin
  vtEventLog.EndUpdate;
  if FLastFocusFlag then
  begin
    vtEventLog.ClearSelection;
    vtEventLog.FocusedNode := vtEventLog.GetLast();
    vtEventLog.Selected[vtEventLog.GetLast()] := True;
  end;
  vtEventLog.Refresh;
  StatusBar1.Panels.Items[0].Text := IntToStr(FEventlog.EventCount);

end;

end.












