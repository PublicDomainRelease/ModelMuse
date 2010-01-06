unit frameModpathSelectionUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, JvExStdCtrls, JvCombobox,
  JvListComb, ArgusDataEntry, ModflowPackageSelectionUnit, ExtCtrls, Buttons,
  Grids, RbwDataGrid4, Mask, JvExMask, JvSpin, JvGroupBox, ComCtrls;

type
  TframeModpathSelection = class(TframePackage)
    pcModpath: TPageControl;
    tabInput: TTabSheet;
    rdeMaxSize: TRbwDataEntry;
    comboEvtSink: TJvImageComboBox;
    comboRchSource: TJvImageComboBox;
    cbCompact: TCheckBox;
    cbBinary: TCheckBox;
    rdeBeginningTime: TRbwDataEntry;
    rdeEndingTime: TRbwDataEntry;
    lblEndingTime: TLabel;
    lblBeginningTime: TLabel;
    lblRchSource: TLabel;
    lblEvtSink: TLabel;
    lblMaxSize: TLabel;
    tabResponse: TTabSheet;
    cbStopAfterMaxTime: TCheckBox;
    rdeMaxTime: TRbwDataEntry;
    lblMaxTime: TLabel;
    comboTrackingDirection: TJvImageComboBox;
    lblTrackingDirection: TLabel;
    lblWeakSinkTreatment: TLabel;
    comboWeakSinkTreatment: TJvImageComboBox;
    rdeWeakSinkThreshold: TRbwDataEntry;
    lblWeakSinkThreshold: TLabel;
    cbStopInZone: TCheckBox;
    rdeStopZone: TRbwDataEntry;
    lblStopZone: TLabel;
    comboWhichEndpoints: TJvImageComboBox;
    lblWhichEndpoints: TLabel;
    cbComputeBudget: TCheckBox;
    rdeErrorTolerance: TRbwDataEntry;
    lblErrorTolerance: TLabel;
    cbSummarize: TCheckBox;
    cbBigBudget: TCheckBox;
    rgOutputMode: TRadioGroup;
    lblReferenceTime: TLabel;
    rdeReferenceTime: TRbwDataEntry;
    tabOutputTimes: TTabSheet;
    gbTime: TJvGroupBox;
    lblTimeCount: TLabel;
    sbAddRow: TSpeedButton;
    sbInsertRow: TSpeedButton;
    sbDeleteRow: TSpeedButton;
    rdgTimes: TRbwDataGrid4;
    seTimeCount: TJvSpinEdit;
    comboTimeMethod: TJvImageComboBox;
    lblTimeMethod: TLabel;
    rdeParticleInterval: TRbwDataEntry;
    lblParticleInterval: TLabel;
    lblMaxTimes: TLabel;
    rdeMaxTimes: TRbwDataEntry;
    lblReleaseTime: TLabel;
    rdeReleaseTime: TRbwDataEntry;
    procedure seTimeCountChange(Sender: TObject);
    procedure rdgTimesEndUpdate(Sender: TObject);
    procedure sbAddRowClick(Sender: TObject);
    procedure sbInsertRowClick(Sender: TObject);
    procedure sbDeleteRowClick(Sender: TObject);
    procedure rdgTimesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rgOutputModeClick(Sender: TObject);
    procedure rdgTimesBeforeDrawCell(Sender: TObject; ACol, ARow: Integer);
    procedure cbStopAfterMaxTimeClick(Sender: TObject);
    procedure comboWeakSinkTreatmentChange(Sender: TObject);
    procedure cbStopInZoneClick(Sender: TObject);
    procedure cbComputeBudgetClick(Sender: TObject);
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
    procedure comboTimeMethodChange(Sender: TObject);
    procedure comboTrackingDirectionChange(Sender: TObject);
  private
    FGettingData: Boolean;
    procedure UpdateTimes;
    procedure SetTimeControlsEnabled;
    procedure EnableStopZone;
    procedure EnableMaxTime;
    procedure EnableSinkThreshold;
    procedure EnableErrorTolerance;
    procedure UpdateMaxTimes;
    procedure EnableWhichEndPointsRecorded;
    procedure EnableTimeControls;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  frameModpathSelection: TframeModpathSelection;

implementation

uses
  ModpathParticleUnit, frmGoPhastUnit, ModflowTimeUnit;

{$R *.dfm}

{ TframeModpathSelection }

procedure TframeModpathSelection.cbComputeBudgetClick(Sender: TObject);
begin
  inherited;
  EnableErrorTolerance;
end;

procedure TframeModpathSelection.cbStopAfterMaxTimeClick(Sender: TObject);
begin
  inherited;
  EnableMaxTime;
end;

procedure TframeModpathSelection.cbStopInZoneClick(Sender: TObject);
begin
  inherited;
  EnableStopZone;
  EnableWhichEndPointsRecorded;
end;

procedure TframeModpathSelection.comboTimeMethodChange(Sender: TObject);
begin
  inherited;
  SetTimeControlsEnabled;
  UpdateMaxTimes;
end;

procedure TframeModpathSelection.comboTrackingDirectionChange(Sender: TObject);
begin
  inherited;
  EnableTimeControls;
end;

procedure TframeModpathSelection.comboWeakSinkTreatmentChange(Sender: TObject);
begin
  inherited;
  EnableSinkThreshold;
end;

constructor TframeModpathSelection.Create(AOwner: TComponent);
begin
  inherited;
  rdgTimes.Cells[0,0] := 'N';
  rdgTimes.Cells[1,0] := 'Time';
end;

procedure TframeModpathSelection.GetData(Package: TModflowPackageSelection);
var
  ModpathSource: TModpathSelection;
  Index: Integer;
  Item: TModpathTimeItem;
  StressPeriod: TModflowStressPeriod;
begin
  inherited;
  FGettingData := True;
  try
    ModpathSource := Package as TModpathSelection;
    pcModpath.ActivePageIndex := 0;
    if not frmGoPhast.PhastModel.ModflowStressPeriods.TransientModel then
    begin
      cbStopAfterMaxTime.Caption :=
        'Stop computing paths after a specified maximum time';
      lblMaxTime.Caption := 'Maximum time';
    end
    else
    begin
      cbStopAfterMaxTime.Caption :=
        'Stop computing paths after a specified tracking time';
      lblMaxTime.Caption := 'Maximum tracking time';
    end;
    comboTrackingDirection.ItemIndex := Ord(ModpathSource.TrackingDirection);
    EnableTimeControls;
    rdeBeginningTime.Enabled :=
      frmGoPhast.PhastModel.ModflowStressPeriods.TransientModel;
    rdeEndingTime.Enabled :=
      frmGoPhast.PhastModel.ModflowStressPeriods.TransientModel;

    StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods.Items[0];
    rdeBeginningTime.Min := StressPeriod.StartTime;
    rdeEndingTime.Min := StressPeriod.StartTime;
    StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods.Items[
      frmGoPhast.PhastModel.ModflowStressPeriods.Count-1];
    rdeBeginningTime.Max := StressPeriod.EndTime;
    rdeEndingTime.Max := StressPeriod.EndTime;

    rdgTimes.FixedCols := 1;

    rdeMaxSize.Text := IntToStr(ModpathSource.MaximumSize);
    comboEvtSink.ItemIndex := Ord(ModpathSource.EVT_Sink);
    comboRchSource.ItemIndex := Ord(ModpathSource.RCH_Source);
    cbCompact.Checked := ModpathSource.Compact;
    cbBinary.Checked := ModpathSource.Binary;
    rdeBeginningTime.Text := FloatToStr(ModpathSource.BeginningTime);
    rdeEndingTime.Text := FloatToStr(ModpathSource.EndingTime);
    rdeReferenceTime.Text := FloatToStr(ModpathSource.ReferenceTime);
    rgOutputMode.ItemIndex := Ord(ModpathSource.OutputMode);
    SetTimeControlsEnabled;
    seTimeCount.AsInteger := ModpathSource.OutputTimes.Count;
    UpdateTimes;
    for Index := 0 to ModpathSource.OutputTimes.Count - 1 do
    begin
      Item := ModpathSource.OutputTimes.Items[Index] as TModpathTimeItem;
      rdgTimes.Cells[1,Index+1] := FloatToStr(Item.Time);
    end;

    cbStopAfterMaxTime.Checked := ModpathSource.StopAfterMaxTime;
    rdeMaxTime.Text := FloatToStr(ModpathSource.MaxTime);
    comboWeakSinkTreatment.ItemIndex := Ord(ModpathSource.WeakSink);
    rdeWeakSinkThreshold.Text := FloatToStr(ModpathSource.WeakSinkThreshold);
    cbStopInZone.Checked := ModpathSource.StopInZone;
    rdeStopZone.Text := IntToStr(ModpathSource.StopZoneNumber);
    comboWhichEndpoints.ItemIndex := Ord(ModpathSource.EndpointWrite);
    cbComputeBudget.Checked := ModpathSource.ComputeBudgetInAllCells;
    rdeErrorTolerance.Text := FloatToStr(ModpathSource.ErrorTolerance);
    cbSummarize.Checked := ModpathSource.Summarize;
    cbBigBudget.Checked := ModpathSource.MakeBigBudgetFile;

    comboTimeMethod.ItemIndex := Ord(ModpathSource.TimeSeriesMethod);
    rdeParticleInterval.Text := FloatToStr(ModpathSource.TimeSeriesInterval);
    rdeMaxTimes.Text := IntToStr(ModpathSource.TimeSeriesMaxCount);

    rdeReleaseTime.Text := FloatToStr(ModpathSource.BackwardsTrackingReleaseTime);

    EnableErrorTolerance;
    EnableMaxTime;
    EnableSinkThreshold;
    EnableStopZone;
    SetTimeControlsEnabled;
    EnableWhichEndPointsRecorded;
  finally
    FGettingData := False;
  end;
end;

procedure TframeModpathSelection.rcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  rdeBeginningTime.Enabled := rcSelectionController.Enabled
    and frmGoPhast.PhastModel.ModflowStressPeriods.TransientModel;
  rdeEndingTime.Enabled := rdeBeginningTime.Enabled;

  SetTimeControlsEnabled;
  EnableStopZone;
  EnableMaxTime;
  EnableSinkThreshold;
  EnableErrorTolerance;
  EnableWhichEndPointsRecorded;
end;

procedure TframeModpathSelection.rdgTimesBeforeDrawCell(Sender: TObject; ACol,
  ARow: Integer);
begin
  inherited;
  if not rdgTimes.Enabled then
  begin
    rdgTimes.Canvas.Brush.Color := clBtnFace;
  end;
end;

procedure TframeModpathSelection.rdgTimesEndUpdate(Sender: TObject);
begin
  inherited;
  seTimeCount.AsInteger := rdgTimes.RowCount-1;
  UpdateTimes;
end;

procedure TframeModpathSelection.rdgTimesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ([ssShift, ssCtrl] * Shift) = [] then
  begin
    rdgTimes.Options := rdgTimes.Options + [goEditing];
  end
  else
  begin
    rdgTimes.Options := rdgTimes.Options - [goEditing];
  end;
end;

procedure TframeModpathSelection.rgOutputModeClick(Sender: TObject);
begin
  inherited;
  SetTimeControlsEnabled;
  EnableWhichEndPointsRecorded;
//  EnableStopZone;
  UpdateMaxTimes;
end;

procedure TframeModpathSelection.sbAddRowClick(Sender: TObject);
begin
  inherited;
  seTimeCount.AsInteger := seTimeCount.AsInteger + 1;
  UpdateTimes;
end;

procedure TframeModpathSelection.sbDeleteRowClick(Sender: TObject);
begin
  inherited;
  if rdgTimes.Row >= 1 then
  begin
    if seTimeCount.AsInteger > 1 then
    begin
      rdgTimes.DeleteRow(rdgTimes.Row);
    end
    else
    begin
      rdgTimes.Cells[1,1] := '';
    end;
    seTimeCount.AsInteger := seTimeCount.AsInteger - 1;
    UpdateTimes;
  end;
end;

procedure TframeModpathSelection.sbInsertRowClick(Sender: TObject);
begin
  inherited;
  if rdgTimes.Row >= 1 then
  begin
    rdgTimes.InsertRow(rdgTimes.Row);
    seTimeCount.AsInteger := seTimeCount.AsInteger + 1;
    UpdateTimes;
  end;
end;

procedure TframeModpathSelection.SetData(Package: TModflowPackageSelection);
var
  ModpathSource: TModpathSelection;
  Index: Integer;
  Item: TModpathTimeItem;
  ItemIndex: Integer;
  ATime: double;
begin
  inherited;
  ModpathSource := Package as TModpathSelection;
  ModpathSource.MaximumSize := StrToInt(rdeMaxSize.Text);
  ModpathSource.EVT_Sink :=
    TSurfaceApplicationPosition(comboEvtSink.ItemIndex);
  ModpathSource.RCH_Source :=
    TSurfaceApplicationPosition(comboRchSource.ItemIndex);
  ModpathSource.Compact := cbCompact.Checked;
  ModpathSource.Binary := cbBinary.Checked;
  ModpathSource.BeginningTime := StrToFloat(rdeBeginningTime.Text);
  ModpathSource.EndingTime := StrToFloat(rdeEndingTime.Text);
  ModpathSource.ReferenceTime := StrToFloat(rdeReferenceTime.Text);
  ModpathSource.OutputMode := TModpathOutputMode(rgOutputMode.ItemIndex);
  ItemIndex := 0;
  for Index := 0 to seTimeCount.AsInteger - 1 do
  begin
    if TryStrToFloat(rdgTimes.Cells[1,Index+1], ATime) then
    begin
      while ItemIndex >= ModpathSource.OutputTimes.Count do
      begin
        ModpathSource.OutputTimes.Add;
      end;
      Item := ModpathSource.OutputTimes.Items[ItemIndex] as TModpathTimeItem;
      Item.Time := ATime;
      Inc(ItemIndex);
    end;
  end;
  While ModpathSource.OutputTimes.Count > ItemIndex do
  begin
    ModpathSource.OutputTimes.Delete(ModpathSource.OutputTimes.Count-1);
  end;

  ModpathSource.StopAfterMaxTime := cbStopAfterMaxTime.Checked;
  ModpathSource.MaxTime := StrToFloat(rdeMaxTime.Text);
  ModpathSource.TrackingDirection := TTrackingDirection(comboTrackingDirection.ItemIndex);
  ModpathSource.WeakSink := TWeakSink(comboWeakSinkTreatment.ItemIndex);
  ModpathSource.WeakSinkThreshold := StrToFloat(rdeWeakSinkThreshold.Text);
  ModpathSource.StopInZone := cbStopInZone.Checked;
  ModpathSource.StopZoneNumber := StrToInt(rdeStopZone.Text);
  ModpathSource.EndpointWrite := TEndpointWrite(comboWhichEndpoints.ItemIndex);
  ModpathSource.ComputeBudgetInAllCells := cbComputeBudget.Checked;
  ModpathSource.ErrorTolerance := StrToFloat(rdeErrorTolerance.Text);
  ModpathSource.Summarize := cbSummarize.Checked;
  ModpathSource.MakeBigBudgetFile := cbBigBudget.Checked;

  ModpathSource.TimeSeriesMethod := TTimeSeriesMethod(comboTimeMethod.ItemIndex);
  ModpathSource.TimeSeriesInterval := StrToFloat(rdeParticleInterval.Text);
  ModpathSource.TimeSeriesMaxCount := StrToInt(rdeMaxTimes.Text);
  ModpathSource.BackwardsTrackingReleaseTime := StrToFloat(rdeReleaseTime.Text);
end;

procedure TframeModpathSelection.EnableTimeControls;
begin
  rdeReferenceTime.Enabled :=
    frmGoPhast.PhastModel.ModflowStressPeriods.TransientModel
    and (comboTrackingDirection.ItemIndex = 0);
  rdeReleaseTime.Enabled :=
    frmGoPhast.PhastModel.ModflowStressPeriods.TransientModel
    and (comboTrackingDirection.ItemIndex = 1);
end;

procedure TframeModpathSelection.EnableWhichEndPointsRecorded;
begin
  comboWhichEndpoints.Enabled := cbStopInZone.Checked and (rgOutputMode.ItemIndex = 0) and rcSelectionController.Enabled;
end;

procedure TframeModpathSelection.UpdateMaxTimes;
begin
  if not FGettingData
    and (rgOutputMode.ItemIndex = 2) // time series
    and (comboTimeMethod.ItemIndex = 0) // individual times
    and (StrToInt(rdeMaxTimes.Text) = 0) then
  begin
    rdeMaxTimes.Text := '1000';
  end;
end;

procedure TframeModpathSelection.EnableErrorTolerance;
begin
  rdeErrorTolerance.Enabled := cbComputeBudget.Checked
    and rcSelectionController.Enabled;
end;

procedure TframeModpathSelection.EnableSinkThreshold;
begin
  rdeWeakSinkThreshold.Enabled :=
    (comboWeakSinkTreatment.ItemIndex = 2)
    and rcSelectionController.Enabled;
end;

procedure TframeModpathSelection.EnableMaxTime;
begin
  rdeMaxTime.Enabled := cbStopAfterMaxTime.Checked
    and rcSelectionController.Enabled;
end;

procedure TframeModpathSelection.EnableStopZone;
begin
  rdeStopZone.Enabled := cbStopInZone.Checked
//    and (rgOutputMode.ItemIndex = 0)
    and rcSelectionController.Enabled;
end;

procedure TframeModpathSelection.SetTimeControlsEnabled;
begin
  seTimeCount.Enabled := (rgOutputMode.ItemIndex in [1, 2])
    and (comboTimeMethod.ItemIndex = 1)
    and rcSelectionController.Enabled;
  rdgTimes.Enabled := seTimeCount.Enabled;
  if seTimeCount.Enabled then
  begin
    seTimeCount.Color := clWindow;
  end
  else
  begin
    seTimeCount.Color := clBtnFace;
  end;
  rdgTimes.Color := seTimeCount.Color;
  sbAddRow.Enabled := seTimeCount.Enabled;
  sbInsertRow.Enabled := seTimeCount.Enabled;
  UpdateTimes;

  comboTimeMethod.Enabled := (rgOutputMode.ItemIndex in [1, 2])
    and rcSelectionController.Enabled;
  rdeParticleInterval.Enabled := (rgOutputMode.ItemIndex in [1, 2])
    and (comboTimeMethod.ItemIndex = 0)
    and rcSelectionController.Enabled;
  rdeMaxTimes.Enabled := rdeParticleInterval.Enabled;
end;

procedure TframeModpathSelection.UpdateTimes;
var
  Index: Integer;
begin
  if (seTimeCount.AsInteger = 0) then
  begin
    rdgTimes.Enabled := False;
    rdgTimes.RowCount := 2;
  end
  else
  begin
    rdgTimes.Enabled :=  seTimeCount.Enabled;
    rdgTimes.RowCount := seTimeCount.AsInteger + 1;
  end;
  sbDeleteRow.Enabled := seTimeCount.Enabled
    and (seTimeCount.AsInteger > 0);
  for Index := 1 to rdgTimes.RowCount - 1 do
  begin
    rdgTimes.Cells[0,Index] := IntToStr(Index);
  end;
end;

procedure TframeModpathSelection.seTimeCountChange(Sender: TObject);
begin
  inherited;
  UpdateTimes;
end;

end.
