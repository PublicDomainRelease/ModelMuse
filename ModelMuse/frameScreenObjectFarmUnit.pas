unit frameScreenObjectFarmUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameScreenObjectUnit, StdCtrls,
  Mask, JvExMask, JvSpin, ExtCtrls, ComCtrls, frameGridUnit,
  frameFormulaGridUnit, JvgPage, frameDeliveryGridUnit, frameFarmDiversionUnit,
  UndoItemsScreenObjects, ModflowFmpFarmUnit, RbwDataGrid4, ModflowFmpCropUnit,
  ClassificationUnit;

type
  TframeScreenObjectFarm = class(TframeScreenObject)
    tabCrops: TTabSheet;
    tabDiversionLocation: TTabSheet;
    tabReturnFlowLocation: TTabSheet;
    tabNonRoutedDelivery: TTabSheet;
    pnlCaption: TPanel;
    seFarmId: TJvSpinEdit;
    lblFarmId: TLabel;
    tabWaterRights: TTabSheet;
    frameFormulaGridCrops: TframeFormulaGrid;
    frameFormulaGridDiversion: TframeFarmDiversion;
    frameFormulaGridReturnFlow: TframeFarmDiversion;
    frameFormulaGridWaterRights: TframeFormulaGrid;
    tabCosts: TTabSheet;
    pcMain: TJvgPageControl;
    frameFormulaGridCosts: TframeFormulaGrid;
    frameDelivery: TframeDeliveryGrid;
    pnlTop: TPanel;
    procedure frameFormulaGridCropsedFormulaChange(Sender: TObject);
    procedure frameFormulaGridCropsGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameFormulaGridCropsseNumberChange(Sender: TObject);
    procedure frameFormulaGridCropssbAddClick(Sender: TObject);
    procedure frameFormulaGridCropssbInsertClick(Sender: TObject);
    procedure frameFormulaGridCropssbDeleteClick(Sender: TObject);
    procedure frameFormulaGridCostsedFormulaChange(Sender: TObject);
    procedure frameFormulaGridCostsseNumberChange(Sender: TObject);
    procedure frameFormulaGridCostsGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameFormulaGridCostssbAddClick(Sender: TObject);
    procedure frameFormulaGridCostssbInsertClick(Sender: TObject);
    procedure frameFormulaGridCostssbDeleteClick(Sender: TObject);
    procedure frameFormulaGridWaterRightsedFormulaChange(Sender: TObject);
    procedure frameFormulaGridWaterRightsGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameFormulaGridWaterRightsseNumberChange(Sender: TObject);
    procedure frameFormulaGridWaterRightssbAddClick(Sender: TObject);
    procedure frameFormulaGridWaterRightssbInsertClick(Sender: TObject);
    procedure frameFormulaGridWaterRightssbDeleteClick(Sender: TObject);
    procedure seFarmIdChange(Sender: TObject);
    procedure frameDeliveryGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure frameFormulaGridDiversionGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameFormulaGridReturnFlowGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
  private
    FChangedCrops: boolean;
    FChangedCosts: boolean;
    FChangedWaterRights: boolean;
    FOnChange: TNotifyEvent;
    FChanging: Boolean;
    FChangedID: Boolean;
{$IFDEF FMP}
    procedure InitializeControls;
    procedure GetCropEffForFirstFarm(FarmItem: TScreenObjectEditItem);
    procedure GetCostsForFirstFarm(FarmItem: TScreenObjectEditItem);
    procedure GetWaterRightsForFirstFarm(FarmItem: TScreenObjectEditItem);
    procedure GetMaxTimeAndCountForCrops(var MaxIndex, MaxTimeCount: Integer;
      AFarm: TFarm);
    procedure ClearGrid(Grid: TRbwDataGrid4);
    procedure SetCropEfficiencies(Farm: TFarm; Crops: TCropCollection);
    procedure SetFarmCosts(Farm: TFarm);
    procedure SetWaterRights(Farm: TFarm);
    procedure Change(Sender: TObject);
    property Changing: Boolean read FChanging write FChanging;
{$ENDIF}
    procedure DoChange;
    { Private declarations }
  public
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean; FarmIdEdit: TScreenObjectDataEdit);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    { Public declarations }
  end;

var
  frameScreenObjectFarm: TframeScreenObjectFarm;

implementation

uses
  GoPhastTypes, frmGoPhastUnit,
  ModflowTimeUnit, Generics.Collections, ScreenObjectUnit, DataSetUnit,
  PhastModelUnit, ModflowPackagesUnit, ModflowPackageSelectionUnit;

resourcestring
  StrGWBaseMaintenance = 'GW base maintenance costs / volume (GWCost1)';
  StrGWPumpingCostsV = 'GW pumping costs / volume / lift (GWCost2)';
  StrGWVerticalLiftCos = 'GW vertical lift costs / volume / lift (GWCost3)';
  StrGWDeliveryCosts = 'GW delivery costs / volume / distance (GWCost4)';
  StrFixedPriceOfSemi = 'Fixed price of (semi-) routed SW / volume (SWCost1)';
  StrVerticalLiftCosts = 'Vertical lift costs of (semi-) routed SW / volume ' +
  '/ lift (SWCost2)';
  StrDeliveryCostsOfS = 'Delivery costs of (semi-) routed SW / volume / dist' +
  'ance (SWCost3)';
  StrFixedPriceOfNonr = 'Fixed price of non-routed SW / volume (SWCost4)';
  StrWaterRightsCallC = 'Water Rights Call (CALL)';
  StrCropEfficiency = '%s on-farm efficiency (OFE)';

type
  TCropColumns = (ccStartTime, ccEndTime, ccCrop);
  TWaterCostColumns = (wccStartTime, wccEndTime, wccGWCost1, wccGWCost2,
    wccGWCost3, wccGWCost4, wccSWCost1, wccSWCost2, wccSWCost3, wccSWCost4);
  TWaterRightsCallColumns = (wrccStartTime, wrccEndTime, wrccCall);

{$R *.dfm}

{ TframeScreenObjectFarm }

procedure TframeScreenObjectFarm.DoChange;
begin
{$IFDEF FMP}
  if Changing then
  begin
    Exit;
  end;
  if Assigned(OnChange) then
  begin
    OnChange(Self);
  end;
{$ENDIF}
end;

{$IFDEF FMP}
procedure TframeScreenObjectFarm.Change(Sender: TObject);
begin
  DoChange;
end;

procedure TframeScreenObjectFarm.ClearGrid(Grid: TRbwDataGrid4);
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
  begin
    for ColIndex := Grid.FixedCols to Grid.ColCount - 1 do
    begin
      Grid.Cells[ColIndex,RowIndex] := ''
    end;
  end;
end;
{$ENDIF}

procedure TframeScreenObjectFarm.GetData(
  ScreenObjectList: TScreenObjectEditCollection);
{$IFDEF FMP}
var
  FarmObjects: TScreenObjectEditCollection;
  ItemIndex: Integer;
  SourceItem: TScreenObjectEditItem;
  AFarm: TFarm;
  NewItem: TScreenObjectEditItem;
  AFarmItem: TScreenObjectEditItem;
//  ShouldContinue: Boolean;
  FirstFarm: TFarm;
  FarmProcess: TFarmProcess;
  Packages: TModflowPackages;
  SfrPackage: TSfrPackageSelection;
{$ENDIF}
begin
{$IFDEF FMP}
  Changing := True;
  try
    InitializeControls;

    Packages := frmGoPhast.PhastModel.ModflowPackages;
    FarmProcess := Packages.FarmProcess;
    tabCosts.TabVisible :=
      (FarmProcess.DeficiencyPolicy in
      [dpAcreageOptimization, dpAcreageOptimizationWithConservationPool])
      and (FarmProcess.DeficiencyPolicy in
      [dpAcreageOptimization, dpAcreageOptimizationWithConservationPool]);


    SfrPackage := Packages.SfrPackage;
    tabDiversionLocation.TabVisible := SfrPackage.IsSelected;
    tabReturnFlowLocation.TabVisible := SfrPackage.IsSelected;

    tabWaterRights.TabVisible :=
      FarmProcess.SurfaceWaterAllotment = swaPriorWithCalls;

    FarmObjects := TScreenObjectEditCollection.Create;
    try
      FarmObjects.OwnScreenObject := True;
      FarmObjects.Capacity := ScreenObjectList.Count;
      for ItemIndex := 0 to ScreenObjectList.Count-1 do
      begin
        SourceItem := ScreenObjectList[ItemIndex];
        AFarm := SourceItem.ScreenObject.ModflowFmpFarm;
        if (AFarm <> nil) and AFarm.Used then
        begin
          NewItem := FarmObjects.Add;
          NewItem.ScreenObject := TScreenObject.Create(nil);
          NewItem.Assign(SourceItem);
        end;
      end;
      if FarmObjects.Count = 0 then
      begin
        Exit;
      end;


      frameFormulaGridCrops.Grid.BeginUpdate;
      frameFormulaGridCosts.Grid.BeginUpdate;
      frameFormulaGridWaterRights.Grid.BeginUpdate;
      try
        AFarmItem := FarmObjects[0];
        GetCropEffForFirstFarm(AFarmItem);
        GetCostsForFirstFarm(AFarmItem);
        GetWaterRightsForFirstFarm(AFarmItem);
        FirstFarm := AFarmItem.ScreenObject.ModflowFmpFarm;
        if FarmObjects.Count = 1 then
        begin
          seFarmId.AsInteger := FirstFarm.FarmId;
          seFarmId.Enabled := True;
        end
        else
        begin
          seFarmId.AsInteger := 0;
          seFarmId.Enabled := False;
        end;

        for ItemIndex := 1 to FarmObjects.Count - 1 do
        begin
          AFarmItem := FarmObjects[ItemIndex];
          AFarm := AFarmItem.ScreenObject.ModflowFmpFarm;
          if not AFarm.FarmEfficiencyCollection.IsSame(
            FirstFarm.FarmEfficiencyCollection) then
          begin
            ClearGrid(frameFormulaGridCrops.Grid);
            frameFormulaGridCrops.seNumber.AsInteger := 0;
          end;
        end;

        for ItemIndex := 1 to FarmObjects.Count - 1 do
        begin
          AFarmItem := FarmObjects[ItemIndex];
          AFarm := AFarmItem.ScreenObject.ModflowFmpFarm;
          if not AFarm.FarmCostsCollection.IsSame(
            FirstFarm.FarmCostsCollection) then
          begin
            ClearGrid(frameFormulaGridCosts.Grid);
            frameFormulaGridCosts.seNumber.AsInteger := 0;
          end;
        end;

        for ItemIndex := 1 to FarmObjects.Count - 1 do
        begin
          AFarmItem := FarmObjects[ItemIndex];
          AFarm := AFarmItem.ScreenObject.ModflowFmpFarm;
          if not AFarm.WaterRights.IsSame(
            FirstFarm.WaterRights) then
          begin
            ClearGrid(frameFormulaGridWaterRights.Grid);
            frameFormulaGridWaterRights.seNumber.AsInteger := 0;
          end;
        end;

      finally
        frameFormulaGridCrops.Grid.EndUpdate;
        frameFormulaGridCosts.Grid.EndUpdate;
        frameFormulaGridWaterRights.Grid.EndUpdate;
      end;

      frameFormulaGridDiversion.GetData(FarmObjects, dtDiversion);
      frameFormulaGridReturnFlow.GetData(FarmObjects, dtReturnFlow);

      frameDelivery.GetData(FarmObjects);

    finally
      FarmObjects.Free;
      FChangedCrops := False;
      FChangedCosts := False;
      FChangedWaterRights := False;
      FChangedID := False;
    end;
  finally
    Changing := False;
  end;
{$ENDIF}
end;

procedure TframeScreenObjectFarm.frameDeliveryGridSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  inherited;
  frameDelivery.GridSetEditText(Sender, ACol, ARow, Value);
  UpdateNextTimeCell(frameDelivery.Grid, ACol, ARow);

end;

procedure TframeScreenObjectFarm.frameFormulaGridCostsedFormulaChange(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridCosts.edFormulaChange(Sender);
  FChangedCosts := True;
end;

procedure TframeScreenObjectFarm.frameFormulaGridCostsGridSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  FChangedCosts := True;
  UpdateNextTimeCell(frameFormulaGridCosts.Grid, ACol, ARow);
  DoChange;
end;

procedure TframeScreenObjectFarm.frameFormulaGridCostssbAddClick(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridCosts.sbAddClick(Sender);
  FChangedCosts := True;
  DoChange;
end;

procedure TframeScreenObjectFarm.frameFormulaGridCostssbDeleteClick(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridCosts.sbDeleteClick(Sender);
  FChangedCosts := True;
  DoChange;
end;

procedure TframeScreenObjectFarm.frameFormulaGridCostssbInsertClick(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridCosts.sbInsertClick(Sender);
  FChangedCosts := True;
  DoChange;
end;

procedure TframeScreenObjectFarm.frameFormulaGridCostsseNumberChange(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridCosts.seNumberChange(Sender);
  FChangedCosts := True;
  DoChange;
end;

procedure TframeScreenObjectFarm.frameFormulaGridCropsedFormulaChange(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridCrops.edFormulaChange(Sender);
  FChangedCrops := True;
end;

procedure TframeScreenObjectFarm.frameFormulaGridCropsGridSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  FChangedCrops := True;
  UpdateNextTimeCell(frameFormulaGridCrops.Grid, ACol, ARow);
  DoChange;
end;

procedure TframeScreenObjectFarm.frameFormulaGridCropssbAddClick(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridCrops.sbAddClick(Sender);
  FChangedCrops := True;
  DoChange;
end;

procedure TframeScreenObjectFarm.frameFormulaGridCropssbDeleteClick(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridCrops.sbDeleteClick(Sender);
  FChangedCrops := True;
  DoChange;
end;

procedure TframeScreenObjectFarm.frameFormulaGridCropssbInsertClick(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridCrops.sbInsertClick(Sender);
  FChangedCrops := True;
  DoChange;
end;

procedure TframeScreenObjectFarm.frameFormulaGridCropsseNumberChange(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridCrops.seNumberChange(Sender);
  FChangedCrops := True;
  DoChange;
end;

procedure TframeScreenObjectFarm.frameFormulaGridDiversionGridSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  frameFormulaGridDiversion.GridSetEditText(Sender, ACol, ARow, Value);
  UpdateNextTimeCell(frameFormulaGridDiversion.Grid, ACol, ARow);

end;

procedure TframeScreenObjectFarm.frameFormulaGridReturnFlowGridSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  frameFormulaGridReturnFlow.GridSetEditText(Sender, ACol, ARow, Value);
  UpdateNextTimeCell(frameFormulaGridReturnFlow.Grid, ACol, ARow);
end;

procedure TframeScreenObjectFarm.frameFormulaGridWaterRightsedFormulaChange(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridWaterRights.edFormulaChange(Sender);
  FChangedWaterRights := True;
end;

procedure TframeScreenObjectFarm.frameFormulaGridWaterRightsGridSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  FChangedWaterRights := True;
  UpdateNextTimeCell(frameFormulaGridWaterRights.Grid, ACol, ARow);
  DoChange;
end;

procedure TframeScreenObjectFarm.frameFormulaGridWaterRightssbAddClick(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridWaterRights.sbAddClick(Sender);
  FChangedWaterRights := True;
  DoChange;
end;

procedure TframeScreenObjectFarm.frameFormulaGridWaterRightssbDeleteClick(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridWaterRights.sbDeleteClick(Sender);
  FChangedWaterRights := True;
  DoChange;
end;

procedure TframeScreenObjectFarm.frameFormulaGridWaterRightssbInsertClick(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridWaterRights.sbInsertClick(Sender);
  FChangedWaterRights := True;
  DoChange;
end;

procedure TframeScreenObjectFarm.frameFormulaGridWaterRightsseNumberChange(
  Sender: TObject);
begin
  inherited;
  frameFormulaGridWaterRights.seNumberChange(Sender);
  FChangedWaterRights := True;
  DoChange;
end;

{$IFDEF FMP}
procedure TframeScreenObjectFarm.GetCostsForFirstFarm(
  FarmItem: TScreenObjectEditItem);
var
  AFarm: TFarm;
  TimeIndex: Integer;
  Grid: TRbwDataGrid4;
  TimeItem: TFarmCostsItem;
begin
  AFarm := FarmItem.ScreenObject.ModflowFmpFarm;
  frameFormulaGridCosts.seNumber.AsInteger := AFarm.FarmCostsCollection.Count;
  frameFormulaGridCosts.seNumber.OnChange(frameFormulaGridCosts.seNumber);
  Grid := frameFormulaGridCosts.Grid;
  for TimeIndex := 0 to AFarm.FarmCostsCollection.Count - 1 do
  begin
    TimeItem := AFarm.FarmCostsCollection[TimeIndex];
    Grid.Cells[Ord(wccStartTime), TimeIndex+1] := FloatToStr(TimeItem.StartTime);
    Grid.Cells[Ord(wccEndTime), TimeIndex+1] := FloatToStr(TimeItem.EndTime);
    Grid.Cells[Ord(wccGWCost1), TimeIndex+1] := TimeItem.GWCost1;
    Grid.Cells[Ord(wccGWCost2), TimeIndex+1] := TimeItem.GWCost2;
    Grid.Cells[Ord(wccGWCost3), TimeIndex+1] := TimeItem.GWCost3;
    Grid.Cells[Ord(wccGWCost4), TimeIndex+1] := TimeItem.GWCost4;
    Grid.Cells[Ord(wccSWCost1), TimeIndex+1] := TimeItem.SWCost1;
    Grid.Cells[Ord(wccSWCost2), TimeIndex+1] := TimeItem.SWCost2;
    Grid.Cells[Ord(wccSWCost3), TimeIndex+1] := TimeItem.SWCost3;
    Grid.Cells[Ord(wccSWCost4), TimeIndex+1] := TimeItem.SWCost4;
  end;
end;
{$ENDIF}

{$IFDEF FMP}
procedure TframeScreenObjectFarm.GetCropEffForFirstFarm(
  FarmItem: TScreenObjectEditItem);
var
  AFarm: TFarm;
  CropIndex: Integer;
  FarmEff: TFarmEfficienciesItem;
  MaxIndex: Integer;
  TimeIndex: Integer;
  TimeItem: TCropEfficiencyItem;
  Grid: TRbwDataGrid4;
  MaxTimeCount: Integer;
begin
  AFarm := FarmItem.ScreenObject.ModflowFmpFarm;
  GetMaxTimeAndCountForCrops(MaxIndex, MaxTimeCount, AFarm);
  frameFormulaGridCrops.seNumber.AsInteger := MaxTimeCount;
  frameFormulaGridCrops.seNumber.OnChange(frameFormulaGridCrops.seNumber);

  FarmEff := AFarm.FarmEfficiencyCollection[MaxIndex];
  Grid := frameFormulaGridCrops.Grid;
  for TimeIndex := 0 to FarmEff.CropEfficiency.Count - 1 do
  begin
    TimeItem := FarmEff.CropEfficiency[TimeIndex];
    Grid.Cells[Ord(ccStartTime), TimeIndex+1] := FloatToStr(TimeItem.StartTime);
    Grid.Cells[Ord(ccEndTime), TimeIndex+1] := FloatToStr(TimeItem.EndTime);
  end;

  for CropIndex := 0 to AFarm.FarmEfficiencyCollection.Count - 1 do
  begin
    FarmEff := AFarm.FarmEfficiencyCollection[CropIndex];
    for TimeIndex := 0 to FarmEff.CropEfficiency.Count - 1 do
    begin
      TimeItem := FarmEff.CropEfficiency[TimeIndex];
      Grid.Cells[Ord(ccCrop) + CropIndex, TimeIndex+1] := TimeItem.Efficiency;
    end;
  end;
end;
{$ENDIF}


{$IFDEF FMP}
procedure TframeScreenObjectFarm.GetWaterRightsForFirstFarm(
  FarmItem: TScreenObjectEditItem);
var
  AFarm: TFarm;
  TimeIndex: Integer;
  ATimeItem: TWaterRightsItem;
  Grid: TRbwDataGrid4;
begin
  AFarm := FarmItem.ScreenObject.ModflowFmpFarm;
  frameFormulaGridWaterRights.seNumber.AsInteger := AFarm.WaterRights.Count;
  frameFormulaGridWaterRights.seNumber.OnChange(frameFormulaGridWaterRights.seNumber);
  Grid := frameFormulaGridWaterRights.Grid;
  for TimeIndex := 0 to AFarm.WaterRights.Count - 1 do
  begin
    ATimeItem := AFarm.WaterRights[TimeIndex];
    Grid.Cells[Ord(wrccStartTime), TimeIndex+1] := FloatToStr(ATimeItem.StartTime);
    Grid.Cells[Ord(wrccEndTime), TimeIndex+1] := FloatToStr(ATimeItem.EndTime);
    Grid.Cells[Ord(wrccCall), TimeIndex+1] := ATimeItem.WaterRights;
  end;
end;
{$ENDIF}

{$IFDEF FMP}
procedure TframeScreenObjectFarm.InitializeControls;
var
  Grid: TRbwDataGrid4;
  Crops: TCropCollection;
  CropIndex: integer;
  ACrop: TCropItem;
  StressPeriods: TModflowStressPeriods;
  StartTimes: TStringList;
  EndTimes: TStringList;
  ColIndex: Integer;
begin
  seFarmId.AsInteger := 0;

  frameFormulaGridDiversion.OnChange := Change;
  frameFormulaGridReturnFlow.OnChange := Change;
  frameDelivery.OnChange := Change;

  tabDiversionLocation.TabVisible := frmGoPhast.PhastModel.SfrIsSelected;
  tabReturnFlowLocation.TabVisible := tabDiversionLocation.TabVisible;

  pcMain.ActivePageIndex := 0;
  StressPeriods := frmGoPhast.PhastModel.ModflowStressPeriods;

  StartTimes := TStringList.Create;
  EndTimes := TStringList.Create;
  try
    // set up Crops grid.
    StressPeriods.FillStringsWithStartTimes(StartTimes);
    StressPeriods.FillStringsWithEndTimes(EndTimes);
    frameFormulaGridCrops.FirstFormulaColumn := Ord(ccCrop);
    Grid := frameFormulaGridCrops.Grid;
    ClearGrid(Grid);
    Crops := frmGoPhast.PhastModel.FmpCrops;
    Grid.ColCount := Crops.Count + 2;
    Grid.BeginUpdate;
    try
      Grid.Cells[Ord(ccStartTime), 0] := StrStartingTime;
      Grid.Cells[Ord(ccEndTime), 0] := StrEndingTime;
      Grid.Columns[Ord(ccStartTime)].PickList := StartTimes;
      Grid.Columns[Ord(ccEndTime)].PickList := EndTimes;
      for CropIndex := 0 to Crops.Count - 1 do
      begin
        ACrop := Crops[CropIndex];
        Grid.Cells[Ord(ccCrop) + CropIndex, 0] :=
          Format(StrCropEfficiency, [ACrop.CropName]);
        Grid.Columns[Ord(ccCrop) + CropIndex].UseButton := True;
        Grid.Columns[Ord(ccCrop) + CropIndex].ButtonCaption := StrF;
        Grid.Columns[Ord(ccCrop) + CropIndex].ButtonWidth := 35;
        Grid.Columns[Ord(ccCrop) + CropIndex].WordWrapCaptions := True;
        Grid.Columns[Ord(ccCrop) + CropIndex].AutoAdjustColWidths := True;
        Grid.Columns[Ord(ccCrop) + CropIndex].AutoAdjustRowHeights := True;
      end;
    finally
      Grid.EndUpdate;
    end;
    Grid.BeginUpdate;
    try
      for CropIndex := 0 to Crops.Count - 1 do
      begin
        Grid.Columns[Ord(ccCrop) + CropIndex].AutoAdjustColWidths := False;
      end;
    finally
      Grid.EndUpdate;
    end;
    frameFormulaGridCrops.LayoutMultiRowEditControls;

    Grid := frameFormulaGridCosts.Grid;
    ClearGrid(Grid);
    Grid.BeginUpdate;
    try
      frameFormulaGridCosts.FirstFormulaColumn := Ord(wccGWCost1);
      Grid.Cells[Ord(wccStartTime), 0] := StrStartingTime;
      Grid.Cells[Ord(wccEndTime), 0] := StrEndingTime;
      Grid.Cells[Ord(wccGWCost1), 0] := StrGWBaseMaintenance;
      Grid.Cells[Ord(wccGWCost2), 0] := StrGWPumpingCostsV;
      Grid.Cells[Ord(wccGWCost3), 0] := StrGWVerticalLiftCos;
      Grid.Cells[Ord(wccGWCost4), 0] := StrGWDeliveryCosts;
      Grid.Cells[Ord(wccSWCost1), 0] := StrFixedPriceOfSemi;
      Grid.Cells[Ord(wccSWCost2), 0] := StrVerticalLiftCosts;
      Grid.Cells[Ord(wccSWCost3), 0] := StrDeliveryCostsOfS;
      Grid.Cells[Ord(wccSWCost4), 0] := StrFixedPriceOfNonr;

      Grid.Columns[Ord(wccStartTime)].ComboUsed := True;
      Grid.Columns[Ord(wccEndTime)].ComboUsed := True;
      Grid.Columns[Ord(ccStartTime)].PickList := StartTimes;
      Grid.Columns[Ord(ccEndTime)].PickList := EndTimes;

      for ColIndex := Ord(wccGWCost1) to Grid.ColCount - 1 do
      begin
        Grid.Columns[ColIndex].ButtonUsed := True;
        Grid.Columns[ColIndex].ButtonCaption := StrF;
        Grid.Columns[ColIndex].ButtonWidth := 35;
      end;

      for ColIndex := 0 to Grid.ColCount - 1 do
      begin
        Grid.Columns[ColIndex].AutoAdjustColWidths := True;
        Grid.Columns[ColIndex].AutoAdjustRowHeights := True;
        Grid.Columns[ColIndex].WordWrapCaptions := True;
      end;

    finally
      Grid.EndUpdate;
    end;
    frameFormulaGridCosts.LayoutMultiRowEditControls;

    frameFormulaGridDiversion.InitializeControls;
    frameFormulaGridReturnFlow.InitializeControls;

//    frameDelivery
    frameDelivery.InitializeControls;

    Grid := frameFormulaGridWaterRights.Grid;
    ClearGrid(Grid);
    Grid.BeginUpdate;
    try
      frameFormulaGridWaterRights.FirstFormulaColumn := Ord(wrccCall);
      Grid.Cells[Ord(wrccStartTime), 0] := StrStartingTime;
      Grid.Cells[Ord(wrccEndTime), 0] := StrEndingTime;
      Grid.Cells[Ord(wrccCall), 0] := StrWaterRightsCallC;
      Grid.Columns[Ord(wrccStartTime)].PickList := StartTimes;
      Grid.Columns[Ord(wrccEndTime)].PickList := EndTimes;
      Grid.Columns[Ord(wrccStartTime)].ComboUsed := True;
      Grid.Columns[Ord(wrccEndTime)].ComboUsed := True;

    //wrccStartTime, wrccEndTime, wrccCall
    finally
      Grid.EndUpdate;
    end;
    frameFormulaGridWaterRights.LayoutMultiRowEditControls;

  finally
    EndTimes.Free;
    StartTimes.Free;
  end;
end;
{$ENDIF}

procedure TframeScreenObjectFarm.seFarmIdChange(Sender: TObject);
begin
  inherited;
  FChangedID := True;
  DoChange;
end;


procedure TframeScreenObjectFarm.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean; FarmIdEdit: TScreenObjectDataEdit);
{$IFDEF FMP}
var
  index: Integer;
  Item: TScreenObjectEditItem;
  Farm: TFarm;
  Crops: TCropCollection;
  FarmCreated: Boolean;
  IntValue: Integer;
  DataArray: TDataArray;
  DataSetIndex: Integer;
{$ENDIF}
begin
{$IFDEF FMP}
  if ClearAll then
  begin
    for index := 0 to List.Count - 1 do
    begin
      Item := List[index];
      Item.ScreenObject.ModflowFmpFarm := nil;
    end;
  end
  else
  begin
    FarmCreated := False;
    if SetAll or FChangedID or FChangedCrops or FChangedCosts or FChangedWaterRights
      or frameFormulaGridDiversion.DataChanged
      or frameFormulaGridReturnFlow.DataChanged or frameDelivery.DataChanged then
    begin
      for index := 0 to List.Count - 1 do
      begin
        Item := List[index];
        Farm := Item.ScreenObject.ModflowFmpFarm;
        if (Farm = nil) and SetAll then
        begin
          Item.ScreenObject.CreateFarm;
          FarmCreated := True;
        end;
      end;
    end;
    if FarmCreated or FChangedID then
    begin
      IntValue := seFarmId.AsInteger;
//      if IntValue > 0 then
      begin
        for Index := 0 to List.Count - 1 do
        begin
          Item := List[Index];
          Farm := Item.ScreenObject.ModflowFmpFarm;
          if (Farm <> nil) then


          { TODO : See if UpdateScreenObjectData can be made to do this. }
//          DataSetIndex := self.GetDataSetIndexByName(rsLakeID);
//          Edit := FDataEdits[DataSetIndex];
          DataArray := FarmIdEdit.DataArray;
          Assert(AnsiSameText(DataArray.Name, KFarmID));
          if (Farm = nil) or (IntValue = 0) then
          begin
            Item.ScreenObject.RemoveDataSet(DataArray)
          end
          else
          begin
            DataSetIndex := Item.ScreenObject.AddDataSet(DataArray);
            Item.ScreenObject.DataSetFormulas[DataSetIndex] := IntToStr(IntValue)
          end;
        end;
      end;

    end;
    if FarmCreated or FChangedCrops then
    begin
      Crops := frmGoPhast.PhastModel.FmpCrops;
      for index := 0 to List.Count - 1 do
      begin
        Item := List[index];
        Farm := Item.ScreenObject.ModflowFmpFarm;
        if Farm <> nil then
        begin
          SetCropEfficiencies(Farm, Crops);
        end;
      end;
    end;
    if FarmCreated or FChangedCosts then
    begin
      for index := 0 to List.Count - 1 do
      begin
        Item := List[index];
        Farm := Item.ScreenObject.ModflowFmpFarm;
        if Farm <> nil then
        begin
          SetFarmCosts(Farm);
        end;
      end;
    end;
    if FarmCreated or FChangedWaterRights then
    begin
      for index := 0 to List.Count - 1 do
      begin
        Item := List[index];
        Farm := Item.ScreenObject.ModflowFmpFarm;
        if Farm <> nil then
        begin
          SetWaterRights(Farm);
        end;
      end;
    end;
    if FarmCreated or frameFormulaGridDiversion.DataChanged then
    begin
      frameFormulaGridDiversion.SetData(List, dtDiversion);
    end;
    if FarmCreated or frameFormulaGridReturnFlow.DataChanged then
    begin
      frameFormulaGridReturnFlow.SetData(List, dtReturnFlow);
    end;
    if FarmCreated or frameDelivery.DataChanged then
    begin
      frameDelivery.SetData(List);
    end;
  end;
{$ENDIF}
end;

{$IFDEF FMP}
procedure TframeScreenObjectFarm.SetWaterRights(Farm: TFarm);
var
  Grid: TRbwDataGrid4;
  WaterRightsItem: TWaterRightsItem;
  WaterRights: TWaterRightsCollection;
  StartTime: Double;
  Count: Integer;
  RowIndex: Integer;
  EndTime: Double;
begin
  WaterRights := Farm.WaterRights;
  Grid := frameFormulaGridWaterRights.Grid;
  Count := 0;
  for RowIndex := 1 to frameFormulaGridWaterRights.seNumber.AsInteger do
  begin
    if TryStrToFloat(Grid.Cells[Ord(wrccStartTime), RowIndex], StartTime) and TryStrToFloat(Grid.Cells[Ord(wrccEndTime), RowIndex], EndTime) then
    begin
      if Count < WaterRights.Count then
      begin
        WaterRightsItem := WaterRights[Count];
      end
      else
      begin
        WaterRightsItem := WaterRights.Add;
      end;
      Inc(Count);
      WaterRightsItem.StartTime := StartTime;
      WaterRightsItem.EndTime := EndTime;
      WaterRightsItem.WaterRights := Grid.Cells[Ord(wrccCall), RowIndex];
    end;
  end;
  while WaterRights.Count > Count do
  begin
    WaterRights.Last.Free;
  end;
end;
{$ENDIF}

{$IFDEF FMP}
procedure TframeScreenObjectFarm.SetFarmCosts(Farm: TFarm);
var
  EndTime: Double;
  Grid: TRbwDataGrid4;
  FarmCosts: TFarmCostsCollection;
  CostItem: TFarmCostsItem;
  RowIndex: Integer;
  Count: Integer;
  StartTime: Double;
begin
  FarmCosts := Farm.FarmCostsCollection;
  Grid := frameFormulaGridCosts.Grid;
  Count := 0;
  for RowIndex := 1 to frameFormulaGridCosts.seNumber.AsInteger do
  begin
    if TryStrToFloat(Grid.Cells[Ord(wccStartTime), RowIndex], StartTime)
      and TryStrToFloat(Grid.Cells[Ord(wccEndTime), RowIndex], EndTime) then
    begin
      if Count < FarmCosts.Count then
      begin
        CostItem := FarmCosts[Count];
      end
      else
      begin
        CostItem := FarmCosts.Add;
      end;
      Inc(Count);
      CostItem.StartTime := StartTime;
      CostItem.EndTime := EndTime;
      CostItem.GWcost1 := Grid.Cells[Ord(wccGWCost1), RowIndex];
      CostItem.GWcost2 := Grid.Cells[Ord(wccGWCost2), RowIndex];
      CostItem.GWcost3 := Grid.Cells[Ord(wccGWCost3), RowIndex];
      CostItem.GWcost4 := Grid.Cells[Ord(wccGWCost4), RowIndex];
      CostItem.SWcost1 := Grid.Cells[Ord(wccSWCost1), RowIndex];
      CostItem.SWcost2 := Grid.Cells[Ord(wccSWCost2), RowIndex];
      CostItem.SWcost3 := Grid.Cells[Ord(wccSWCost3), RowIndex];
      CostItem.SWcost4 := Grid.Cells[Ord(wccSWCost4), RowIndex];
    end;
  end;
  while FarmCosts.Count > Count do
  begin
    FarmCosts.Last.Free;
  end;
end;
{$ENDIF}

{$IFDEF FMP}
procedure TframeScreenObjectFarm.SetCropEfficiencies(Farm: TFarm; Crops: TCropCollection);
var
  EndTime: Double;
  EfficienciesItem: TFarmEfficienciesItem;
  ColIndex: Integer;
  StartTime: Double;
  Grid: TRbwDataGrid4;
  EfficiencyItem: TCropEfficiencyItem;
  Rows: Generics.Collections.TList<Integer>;
  EfficiencyCollection: TFarmEfficiencyCollection;
  CropEfficiency: TCropEfficiencyCollection;
  StartTimes: Generics.Collections.TList<Double>;
  CropIndex: Integer;
  EndTimes: Generics.Collections.TList<Double>;
  RowIndex: Integer;
  ARow: Integer;
begin
  if seFarmId.AsInteger > 0 then
  begin
    Farm.FarmId := seFarmId.AsInteger;
  end;
  EfficiencyCollection := Farm.FarmEfficiencyCollection;
  for CropIndex := EfficiencyCollection.Count to Crops.Count - 1 do
  begin
    EfficienciesItem := EfficiencyCollection.Add;
    EfficienciesItem.CropEfficiency.CropName := Crops[CropIndex].CropName;
  end;
  while EfficiencyCollection.Count > Crops.Count do
  begin
    EfficiencyCollection.Last.Free;
  end;
  StartTimes := TList<Double>.Create;
  EndTimes := TList<Double>.Create;
  Rows := TList<Integer>.Create;
  try
    Grid := frameFormulaGridCrops.Grid;
    for RowIndex := 1 to frameFormulaGridCrops.seNumber.AsInteger do
    begin
      if TryStrToFloat(Grid.Cells[Ord(ccStartTime), RowIndex], StartTime)
        and TryStrToFloat(Grid.Cells[Ord(ccEndTime), RowIndex], EndTime) then
      begin
        Rows.Add(RowIndex);
        StartTimes.Add(StartTime);
        EndTimes.Add(EndTime);
      end;
    end;
    for CropIndex := 0 to EfficiencyCollection.Count - 1 do
    begin
      EfficienciesItem := EfficiencyCollection[CropIndex];
      CropEfficiency := EfficienciesItem.CropEfficiency;
      while CropEfficiency.Count > Rows.Count do
      begin
        CropEfficiency.Last.Free;
      end;
      while CropEfficiency.Count < Rows.Count do
      begin
        CropEfficiency.Add;
      end;
      ColIndex := CropIndex + Ord(ccCrop);
      for RowIndex := 0 to Rows.Count - 1 do
      begin
        ARow := Rows[RowIndex];
        EfficiencyItem := CropEfficiency[RowIndex];
        EfficiencyItem.StartTime := StartTimes[RowIndex];
        EfficiencyItem.EndTime := EndTimes[RowIndex];
        EfficiencyItem.Efficiency := Grid.Cells[ColIndex, ARow];
      end;
    end;
  finally
    StartTimes.Free;
    EndTimes.Free;
    Rows.Free;
  end;
end;
{$ENDIF}

{$IFDEF FMP}
procedure TframeScreenObjectFarm.GetMaxTimeAndCountForCrops(
  var MaxIndex: Integer; var MaxTimeCount: Integer; AFarm: TFarm);
var
  FarmEff: TFarmEfficienciesItem;
  CropIndex: Integer;
begin
  MaxTimeCount := 0;
  Assert(frameFormulaGridCrops.Grid.ColCount = AFarm.FarmEfficiencyCollection.Count + 2);
  Assert(AFarm.FarmEfficiencyCollection.Count > 0);
  MaxIndex := 0;
  for CropIndex := 0 to AFarm.FarmEfficiencyCollection.Count - 1 do
  begin
    FarmEff := AFarm.FarmEfficiencyCollection[CropIndex];
    if MaxTimeCount <= FarmEff.CropEfficiency.Count then
    begin
      MaxTimeCount := FarmEff.CropEfficiency.Count;
      MaxIndex := CropIndex;
    end;
  end;
end;
{$ENDIF}

end.
