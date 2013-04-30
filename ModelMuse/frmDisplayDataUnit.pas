unit frmDisplayDataUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, frameStreamLinkUnit, JvPageList, ExtCtrls,
  ComCtrls, JvExComCtrls, JvPageListTreeView, JvExControls, StdCtrls, Buttons,
  frameHeadObservationResultsUnit, frameModpathDisplayUnit,
  frameModpathTimeSeriesDisplayUnit, frameModpathEndpointDisplayUnit,
  frameCustomColorUnit, frameColorGridUnit, frameContourDataUnit,
  frameVectorsUnit;

type
  TPostPages = (ppColorGrid, ppContourData, ppPathline, ppEndPoints,
    ppTimeSeries, ppHeadObs, ppSfrStreamLink, ppStrStreamLink, ppVectors);

  TfrmDisplayData = class(TfrmCustomGoPhast)
    pglstMain: TJvPageList;
    tvpglstMain: TJvPageListTreeView;
    splSplit: TSplitter;
    jvspSfrStreamLinks: TJvStandardPage;
    frameSfrStreamLink: TframeStreamLink;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnApply: TBitBtn;
    btnClose: TBitBtn;
    jvspHeadObsResults: TJvStandardPage;
    frameHeadObservationResults: TframeHeadObservationResults;
    jvspModpathPathline: TJvStandardPage;
    frameModpathDisplay: TframeModpathDisplay;
    jvspModpathTimeSeries: TJvStandardPage;
    frameModpathTimeSeriesDisplay: TframeModpathTimeSeriesDisplay;
    jvspModpathEndpoints: TJvStandardPage;
    frameModpathEndpointDisplay1: TframeModpathEndpointDisplay;
    jvspColorGrid: TJvStandardPage;
    frameColorGrid: TframeColorGrid;
    jvspContourData: TJvStandardPage;
    frameContourData: TframeContourData;
    jvspVectors: TJvStandardPage;
    frameVectors: TframeVectors;
    jvspStrStreamLinks: TJvStandardPage;
    frameStrStreamLink: TframeStreamLink;
    procedure btnApplyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure pglstMainChange(Sender: TObject);
    procedure tvpglstMainChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure tvpglstMainCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure frameContourDatavirttreecomboDataSetsChange(Sender: TObject);
  private
    FShouldUpdate: Boolean;
    procedure SetData;
    { Private declarations }
  public
    procedure GetData;
    procedure SetPage(Page: TPostPages);
    property ShouldUpdate: Boolean read FShouldUpdate write FShouldUpdate;
    procedure UpdateLabelsAndLegend;
    procedure UpdateColorSchemes;
    procedure NilDisplay;
    { Public declarations }
  end;

var
  frmDisplayData: TfrmDisplayData = nil;

procedure UpdateFrmDisplayData(Force: boolean = false);

implementation

uses
  frmGoPhastUnit, PhastModelUnit, GoPhastTypes, ModflowPackagesUnit;

resourcestring
  StrColorGrid = 'Color Grid';
  StrContourData = 'Contour Data';
  StrMODPATHPathlines = 'MODPATH Pathlines';
  StrMODPATHEndPoints = 'MODPATH End Points';
  StrMODPATHTimeSeries = 'MODPATH Time Series';
  StrHeadObservationRes = 'Head Observation Results';
  StrStreamLinks = 'SFR Stream Links';
  StrStreamStrLinks = 'STR Stream Links';
  StrVectors = 'Vectors';

{$R *.dfm}

procedure UpdateFrmDisplayData(Force: boolean = false);
begin
  if (frmDisplayData <> nil) and (Force or frmDisplayData.Visible)
    and not (csDestroying in frmGoPhast.ComponentState) then
  begin
    frmDisplayData.GetData;
    frmDisplayData.UpdateLabelsAndLegend;
  end;
end;

procedure TfrmDisplayData.btnApplyClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmDisplayData.FormCreate(Sender: TObject);
var
  Node: TJvPageIndexNode;
begin
  inherited;
  AdjustFormPosition(dpRight);
  Handle;
  tvpglstMain.Handle;

  tvpglstMain.Items.Clear;
  Node := tvpglstMain.Items.Add(nil, StrColorGrid) as TJvPageIndexNode;
  Node.PageIndex := 5;
  Node := tvpglstMain.Items.Add(nil, StrContourData) as TJvPageIndexNode;
  Node.PageIndex := 6;
  Node := tvpglstMain.Items.Add(nil, StrMODPATHPathlines) as TJvPageIndexNode;
  Node.PageIndex := 0;
  Node := tvpglstMain.Items.Add(nil, StrMODPATHEndPoints) as TJvPageIndexNode;
  Node.PageIndex := 4;
  Node := tvpglstMain.Items.Add(nil, StrMODPATHTimeSeries) as TJvPageIndexNode;
  Node.PageIndex := 3;
  Node := tvpglstMain.Items.Add(nil, StrHeadObservationRes) as TJvPageIndexNode;
  Node.PageIndex := 2;
  Node := tvpglstMain.Items.Add(nil, StrStreamLinks) as TJvPageIndexNode;
  Node.PageIndex := 1;
  Node := tvpglstMain.Items.Add(nil, StrStreamStrLinks) as TJvPageIndexNode;
  Node.PageIndex := 8;
  Node := tvpglstMain.Items.Add(nil, StrVectors) as TJvPageIndexNode;
  Node.PageIndex := 7;
end;

procedure TfrmDisplayData.frameContourDatavirttreecomboDataSetsChange(
  Sender: TObject);
begin
  inherited;
  frameContourData.virttreecomboDataSetsChange(Sender);
end;

procedure TfrmDisplayData.GetData;
var
  LocalModel: TPhastModel;
  ModflowSelected: Boolean;
  ModpathSelected: Boolean;
//  LocalPackages: TModflowPackages;
  SfrSelected: Boolean;
  StrSelected: boolean;
  HeadObsSelected: Boolean;
  VectorSelected: Boolean;
//  Node: TTreeNode;
begin
  Handle;
  tvpglstMain.Handle;
  LocalModel := frmGoPhast.PhastModel;
  ModflowSelected := LocalModel.ModelSelection in ModflowSelection;
//  LocalPackages := LocalModel.ModflowPackages;
  ModpathSelected := ModflowSelected and LocalModel.MODPATHIsSelected;
  SfrSelected := ModflowSelected and LocalModel.SfrIsSelected;
  StrSelected := ModflowSelected and LocalModel.StrIsSelected;
  HeadObsSelected := ModflowSelected and LocalModel.HobIsSelected;
  VectorSelected := LocalModel.ModelSelection = msSutra22;

  if Ord(High(TPostPages)) <> tvpglstMain.Items.Count-1 then
  begin
    Beep;
    Exit
  end;
  Assert(Ord(High(TPostPages)) = tvpglstMain.Items.Count-1);

  tvpglstMain.Items[Ord(ppPathline)].Enabled :=
    ModpathSelected
    or (LocalModel.PathLines.Lines.Count > 0)
    or (LocalModel.PathLines.LinesV6.Count > 0);
  tvpglstMain.Items[Ord(ppEndPoints)].Enabled :=
    ModpathSelected
    or (LocalModel.EndPoints.Points.Count > 0)
    or (LocalModel.EndPoints.PointsV6.Count > 0);
  tvpglstMain.Items[Ord(ppTimeSeries)].Enabled :=
    ModpathSelected
    or (LocalModel.TimeSeries.Series.Count > 0)
    or (LocalModel.TimeSeries.SeriesV6.Count > 0);
  tvpglstMain.Items[Ord(ppHeadObs)].Enabled :=
    HeadObsSelected or (LocalModel.HeadObsResults.Count > 0);
  tvpglstMain.Items[Ord(ppSfrStreamLink)].Enabled := SfrSelected;
  tvpglstMain.Items[Ord(ppStrStreamLink)].Enabled := StrSelected;

  tvpglstMain.Items[Ord(ppVectors)].Enabled := VectorSelected;



  if frmGoPhast.ModelSelection in ModflowSelection then
  begin
    frameStrStreamLink.GetData(stSTR);
    frameSfrStreamLink.GetData(stSFR);
    frameHeadObservationResults.GetData;
    frameModpathDisplay.GetData;
    frameModpathTimeSeriesDisplay.GetData;
    frameModpathEndpointDisplay1.GetData;
  end
  else if frmGoPhast.ModelSelection = msSutra22 then
  begin
    frameVectors.GetData;
  end;
  frameColorGrid.GetData;
  frameContourData.GetData;

//  frameColorGrid.UpdateLabelsAndLegend;
//  frameContourData.UpdateLabelsAndLegend;
end;

procedure TfrmDisplayData.NilDisplay;
begin
  frameColorGrid.LegendDataSource := nil;
  frameContourData.LegendDataSource := nil;
end;

procedure TfrmDisplayData.pglstMainChange(Sender: TObject);
begin
  inherited;
  HelpKeyword := pglstMain.ActivePage.HelpKeyword
end;

procedure TfrmDisplayData.SetData;
begin
  if pglstMain.ActivePage = jvspSfrStreamLinks then
  begin
    if frmGoPhast.ModelSelection in ModflowSelection then
    begin
      frameSfrStreamLink.SetData;
    end;
  end
  else if pglstMain.ActivePage = jvspStrStreamLinks then
  begin
    if frmGoPhast.ModelSelection in ModflowSelection then
    begin
      frameStrStreamLink.SetData;
    end;
  end
  else if pglstMain.ActivePage = jvspHeadObsResults then
  begin
    if frmGoPhast.ModelSelection in ModflowSelection then
    begin
      frameHeadObservationResults.SetData;
    end;
  end
  else if pglstMain.ActivePage = jvspModpathPathline then
  begin
    if frmGoPhast.ModelSelection in ModflowSelection then
    begin
      frameModpathDisplay.SetData;
    end;
  end
  else if pglstMain.ActivePage = jvspModpathTimeSeries then
  begin
    if frmGoPhast.ModelSelection in ModflowSelection then
    begin
      frameModpathTimeSeriesDisplay.SetData;
    end;
  end
  else if pglstMain.ActivePage = jvspModpathEndpoints then
  begin
    if frmGoPhast.ModelSelection in ModflowSelection then
    begin
      frameModpathEndpointDisplay1.SetData;
    end;
  end
  else if pglstMain.ActivePage = jvspColorGrid then
  begin
    frameColorGrid.SetData;
  end
  else if pglstMain.ActivePage = jvspContourData then
  begin
    frameContourData.SetData;
  end
  else if pglstMain.ActivePage = jvspVectors then
  begin
    frameVectors.SetData;
  end;
end;

procedure TfrmDisplayData.SetPage(Page: TPostPages);
begin
  tvpglstMain.Items[Ord(Page)].Selected := True;
end;

procedure TfrmDisplayData.tvpglstMainChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  inherited;
  AllowChange := tvpglstMain.Items[Node.Index].Enabled;
end;

procedure TfrmDisplayData.tvpglstMainCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  inherited;
  if Node.Enabled then
  begin
    tvpglstMain.Canvas.Font.Color := clBlack;
  end
  else
  begin
    tvpglstMain.Canvas.Font.Color := clBtnShadow;
  end;
  if Node.Selected and not Sender.Focused then
  begin
    Sender.Canvas.Brush.Color := clMenuHighlight;
  end;
end;

procedure TfrmDisplayData.UpdateColorSchemes;
begin
  frameColorGrid.UpdateColorSchemes;
  frameContourData.UpdateColorSchemes;
end;

procedure TfrmDisplayData.UpdateLabelsAndLegend;
begin
  frameColorGrid.UpdateLabelsAndLegend;
  frameContourData.UpdateLabelsAndLegend;
  ShouldUpdate := False;
end;

end.
