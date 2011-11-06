unit frmDisplayDataUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, frameStreamLinkUnit, JvPageList, ExtCtrls,
  ComCtrls, JvExComCtrls, JvPageListTreeView, JvExControls, StdCtrls, Buttons,
  frameHeadObservationResultsUnit, frameModpathDisplayUnit,
  frameModpathTimeSeriesDisplayUnit, frameModpathEndpointDisplayUnit,
  frameCustomColorUnit, frameColorGridUnit, frameContourDataUnit;

type
  TPostPages = (ppColorGrid, ppContourData, ppPathline, ppEndPoints,
    ppTimeSeries, ppHeadObs, ppStreamLink);

  TfrmDisplayData = class(TfrmCustomGoPhast)
    pglstMain: TJvPageList;
    tvpglstMain: TJvPageListTreeView;
    splSplit: TSplitter;
    jvspStreamLinks: TJvStandardPage;
    frameStreamLink: TframeStreamLink;
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
    procedure btnApplyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure pglstMainChange(Sender: TObject);
    procedure tvpglstMainChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure tvpglstMainCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    procedure SetData;
    { Private declarations }
  public
    procedure GetData;
    procedure SetPage(Page: TPostPages);
    procedure UpdateLabelsAndLegend;
    { Public declarations }
  end;

var
  frmDisplayData: TfrmDisplayData;

procedure UpdateFrmDisplayData(Force: boolean = false);

implementation

uses
  frmGoPhastUnit, PhastModelUnit, GoPhastTypes, ModflowPackagesUnit;

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
begin
  inherited;
  AdjustFormPosition(dpRight);
end;

procedure TfrmDisplayData.GetData;
var
  LocalModel: TPhastModel;
  ModflowSelected: Boolean;
  ModpathSelected: Boolean;
  LocalPackages: TModflowPackages;
  SfrSelected: Boolean;
  HeadObsSelected: Boolean;
begin
  LocalModel := frmGoPhast.PhastModel;
  ModflowSelected := LocalModel.ModelSelection in [msModflow, msModflowLGR, msModflowNWT];
  LocalPackages := LocalModel.ModflowPackages;
  ModpathSelected := ModflowSelected and LocalPackages.ModPath.IsSelected;
  SfrSelected := ModflowSelected and LocalPackages.SfrPackage.IsSelected;
  HeadObsSelected := ModflowSelected and LocalPackages.HobPackage.IsSelected;
  tvpglstMain.Items[Ord(ppPathline)].Enabled :=
    ModpathSelected or (LocalModel.PathLines.Lines.Count > 0);
  tvpglstMain.Items[Ord(ppEndPoints)].Enabled :=
    ModpathSelected or (LocalModel.EndPoints.Points.Count > 0);
  tvpglstMain.Items[Ord(ppTimeSeries)].Enabled :=
    ModpathSelected or (LocalModel.TimeSeries.Series.Count > 0);
  tvpglstMain.Items[Ord(ppHeadObs)].Enabled :=
    HeadObsSelected or (LocalModel.HeadObsResults.Count > 0);
  tvpglstMain.Items[Ord(ppStreamLink)].Enabled := SfrSelected;


  frameStreamLink.GetData;
  frameHeadObservationResults.GetData;
  frameModpathDisplay.GetData;
  frameModpathTimeSeriesDisplay.GetData;
  frameModpathEndpointDisplay1.GetData;
  frameColorGrid.GetData;
  frameContourData.GetData;
end;

procedure TfrmDisplayData.pglstMainChange(Sender: TObject);
begin
  inherited;
  HelpKeyword := pglstMain.ActivePage.HelpKeyword
end;

procedure TfrmDisplayData.SetData;
begin
  if pglstMain.ActivePage = jvspStreamLinks then
  begin
    frameStreamLink.SetData;
  end
  else if pglstMain.ActivePage = jvspHeadObsResults then
  begin
    frameHeadObservationResults.SetData;
  end
  else if pglstMain.ActivePage = jvspModpathPathline then
  begin
    frameModpathDisplay.SetData;
  end
  else if pglstMain.ActivePage = jvspModpathTimeSeries then
  begin
    frameModpathTimeSeriesDisplay.SetData;
  end
  else if pglstMain.ActivePage = jvspModpathEndpoints then
  begin
    frameModpathEndpointDisplay1.SetData;
  end
  else if pglstMain.ActivePage = jvspColorGrid then
  begin
    frameColorGrid.SetData;
  end
  else if pglstMain.ActivePage = jvspContourData then
  begin
    frameContourData.SetData;
  end
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
end;

procedure TfrmDisplayData.UpdateLabelsAndLegend;
begin
  frameColorGrid.UpdateLabelsAndLegend;
  frameContourData.UpdateLabelsAndLegend;
end;

end.
