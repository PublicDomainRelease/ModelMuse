unit frameStreamLinkUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls, JvExStdCtrls, JvCombobox, JvListComb,
  PhastModelUnit, UndoItems, DisplaySettingsUnit;

type
  TframeStreamLink = class(TFrame)
    shpStreamColor: TShape;
    dlgLinkColor: TColorDialog;
    btnStreamColor: TButton;
    shpDiversionColor: TShape;
    btnDiversionColor: TButton;
    rgItemsToPlot: TRadioGroup;
    cbStreams: TCheckBox;
    cbPlotDiversions: TCheckBox;
    lblTimeToPlot: TLabel;
    comboTimeToPlot: TJvComboBox;
    cbPlotUnconnected: TCheckBox;
    btnUnconnectedColor: TButton;
    shpUnconnectedColor: TShape;
    procedure btnStreamColorClick(Sender: TObject);
    procedure btnDiversionColorClick(Sender: TObject);
    procedure cbStreamsClick(Sender: TObject);
    procedure cbPlotDiversionsClick(Sender: TObject);
    procedure btnUnconnectedColorClick(Sender: TObject);
    procedure cbPlotUnconnectedClick(Sender: TObject);
  private
    FSfrStreamLinkPlot: TSfrStreamLinkPlot;
    procedure SetShapeColor(AShape: TShape);
    { Private declarations }
  public
    destructor Destroy; override;
    procedure GetData;
    procedure SetData;
    { Public declarations }
  end;

  TUndoStreamLinks = class(TCustomUndo)
  private
    FOldSfrStreamLinkPlot: TSfrStreamLinkPlot;
    FNewSfrStreamLinkPlot: TSfrStreamLinkPlot;
  protected
    function Description: string; override;
  public
    procedure DoCommand; override;
    procedure Undo; override;
  public
    constructor Create(var SfrStreamLinkPlot: TSfrStreamLinkPlot);
    destructor Destroy; override;
  end;

implementation

uses
  frmGoPhastUnit;

{$R *.dfm}

procedure TframeStreamLink.btnDiversionColorClick(Sender: TObject);
begin
  SetShapeColor(shpDiversionColor);
end;

procedure TframeStreamLink.btnStreamColorClick(Sender: TObject);
begin
  SetShapeColor(shpStreamColor);
end;

procedure TframeStreamLink.btnUnconnectedColorClick(Sender: TObject);
begin
  SetShapeColor(shpUnconnectedColor);
end;

procedure TframeStreamLink.cbPlotDiversionsClick(Sender: TObject);
begin
  btnDiversionColor.Enabled := cbPlotDiversions.Checked;
  shpDiversionColor.Enabled := cbPlotDiversions.Checked;
end;

procedure TframeStreamLink.cbPlotUnconnectedClick(Sender: TObject);
begin
  btnUnconnectedColor.Enabled := cbPlotUnconnected.Checked;
  shpUnconnectedColor.Enabled := cbPlotUnconnected.Checked;
end;

procedure TframeStreamLink.cbStreamsClick(Sender: TObject);
begin
  btnStreamColor.Enabled := cbStreams.Checked;
  shpStreamColor.Enabled := cbStreams.Checked;
end;

destructor TframeStreamLink.Destroy;
begin
  FSfrStreamLinkPlot.Free;
  inherited;
end;

procedure TframeStreamLink.GetData;
var
  EndTime: double;
begin
  FSfrStreamLinkPlot.Free;
  FSfrStreamLinkPlot:= TSfrStreamLinkPlot.Create(nil);
  frmGoPhast.PhastModel.ModflowStressPeriods.
    FillStringsWithStartTimes(comboTimeToPlot.Items);
  EndTime := frmGoPhast.PhastModel.ModflowStressPeriods[
    frmGoPhast.PhastModel.ModflowStressPeriods.Count-1].EndTime;
  comboTimeToPlot.Items.Add(FloatToStr(EndTime));

  FSfrStreamLinkPlot.Assign(frmGoPhast.PhastModel.SfrStreamLinkPlot);
  cbStreams.Checked := FSfrStreamLinkPlot.PlotStreamConnections;
  cbPlotDiversions.Checked := FSfrStreamLinkPlot.PlotDiversions;
  cbPlotUnconnected.Checked := FSfrStreamLinkPlot.PlotUnconnected;
  shpStreamColor.Brush.Color := FSfrStreamLinkPlot.StreamColor;
  shpDiversionColor.Brush.Color := FSfrStreamLinkPlot.DiversionColor;
  shpUnconnectedColor.Brush.Color := FSfrStreamLinkPlot.UnconnectedColor;
  rgItemsToPlot.ItemIndex := Ord(FSfrStreamLinkPlot.StreamsToPlot);
  comboTimeToPlot.Text := FloatToStr(FSfrStreamLinkPlot.TimeToPlot);
end;

procedure TframeStreamLink.SetData;
var
  Undo: TUndoStreamLinks;
begin
  if FSfrStreamLinkPlot = nil then
  begin
    FSfrStreamLinkPlot:= TSfrStreamLinkPlot.Create(nil);
    FSfrStreamLinkPlot.Assign(frmGoPhast.PhastModel.SfrStreamLinkPlot);
  end;
  FSfrStreamLinkPlot.PlotStreamConnections := cbStreams.Checked;
  FSfrStreamLinkPlot.PlotDiversions := cbPlotDiversions.Checked;
  FSfrStreamLinkPlot.PlotUnconnected := cbPlotUnconnected.Checked;
  FSfrStreamLinkPlot.StreamColor := shpStreamColor.Brush.Color;
  FSfrStreamLinkPlot.DiversionColor := shpDiversionColor.Brush.Color;
  FSfrStreamLinkPlot.UnconnectedColor := shpUnconnectedColor.Brush.Color;
  FSfrStreamLinkPlot.StreamsToPlot := TStreamsToPlot(rgItemsToPlot.ItemIndex);
  FSfrStreamLinkPlot.TimeToPlot := StrToFloat(comboTimeToPlot.Text);
  Undo := TUndoStreamLinks.Create(FSfrStreamLinkPlot);
  frmGoPhast.UndoStack.Submit(Undo);
end;

procedure TframeStreamLink.SetShapeColor(AShape: TShape);
begin
  dlgLinkColor.Color := AShape.Brush.Color;
  if dlgLinkColor.Execute then
  begin
    AShape.Brush.Color := dlgLinkColor.Color;
  end;
end;

{ TUndoStreamLinks }

constructor TUndoStreamLinks.Create(var SfrStreamLinkPlot: TSfrStreamLinkPlot);
begin
  inherited Create;
  FNewSfrStreamLinkPlot := SfrStreamLinkPlot;
  SfrStreamLinkPlot := nil;
  FOldSfrStreamLinkPlot := TSfrStreamLinkPlot.Create(nil);
  FOldSfrStreamLinkPlot.Assign(frmGoPhast.PhastModel.SfrStreamLinkPlot);
end;

function TUndoStreamLinks.Description: string;
begin
  result := 'display stream links';
end;

destructor TUndoStreamLinks.Destroy;
begin
  FNewSfrStreamLinkPlot.Free;
  FOldSfrStreamLinkPlot.Free;
  inherited;
end;

procedure TUndoStreamLinks.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.SfrStreamLinkPlot := FNewSfrStreamLinkPlot;
  frmGoPhast.frameTopView.ModelChanged := True;
  frmGoPhast.frameTopView.ZoomBox.Image32.Invalidate;
end;

procedure TUndoStreamLinks.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.SfrStreamLinkPlot := FOldSfrStreamLinkPlot;
  frmGoPhast.frameTopView.ModelChanged := True;
  frmGoPhast.frameTopView.ZoomBox.Image32.Invalidate;
end;

end.
