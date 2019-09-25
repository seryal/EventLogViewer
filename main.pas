unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, dateutils,
  Dialogs, ComCtrls, ExtCtrls, Windows, syeventlogreader, VirtualTrees;

type

  { TForm1 }

  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    vtEventLog: TVirtualStringTree;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
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
    FEventlog: TsyEventLogReader;
    FFormatSettings: TFormatSettings;
    FLastFocusFlag: boolean;
    procedure OnBeginUpdate(Sender: TObject);
    procedure OnEndUpdate(Sender: TObject);
    procedure OnEventLogRecord(Sender: TObject; ALogRecord: TsyEventLogRecord);
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

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(FEventlog);
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












