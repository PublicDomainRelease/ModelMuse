unit framePackageFrmUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, framePackageUnit, Grids,
  RbwDataGrid4, RbwController, StdCtrls, ModflowPackageSelectionUnit, ExtCtrls;

type
  TFarmRows = (frRootingDepth, frConsumptiveUse, frPrecipitation,
    frFractionOfInefficiencyLoses, frEfficiencyGroundwaterFunction,
    frEfficiencyReset, frDeficiencyPolicy, frWaterCostCoefficients,
    frCropConsumptiveConcept, frCropConsumptiveLinkage, frSurfaceWaterAllotment,
    frSaveWellFlowRates, frSaveNetRecharge, frSupplyAndDemand,
    frFarmBudgetPrintFlags, frPrintRouting, frAcerageOptimizationPrintChoice,
    frAcerageOptimizationPrintLocation, frDiversionBudgetLocation,
    frCropIrrigationRequirement);

  TframePkgFarm = class(TframePackage)
    rrdgOptions: TRbwRowDataGrid;
    rgAssignmentMethod: TRadioGroup;
  private
    procedure InitializeGrid;
    { Private declarations }
  protected
    procedure Loaded; override;
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePkgFarm: TframePkgFarm;

implementation

uses
  GoPhastTypes;

{$R *.dfm}

{ TframePkgFarm }

procedure TframePkgFarm.GetData(Package: TModflowPackageSelection);
var
  FarmProcess: TFarmProcess;
begin
  inherited;
  FarmProcess := Package as TFarmProcess;

  rrdgOptions.ItemIndex[1, Ord(frRootingDepth)] := Ord(FarmProcess.RootingDepth);
  rrdgOptions.ItemIndex[1, Ord(frConsumptiveUse)] := Ord(FarmProcess.ConsumptiveUse);
  rrdgOptions.ItemIndex[1, Ord(frPrecipitation)] := Ord(FarmProcess.Precipitation);
  rrdgOptions.ItemIndex[1, Ord(frFractionOfInefficiencyLoses)] := Ord(FarmProcess.FractionOfInefficiencyLoses);
  rrdgOptions.ItemIndex[1, Ord(frEfficiencyGroundwaterFunction)] := Ord(FarmProcess.EfficiencyGroundwaterFunction);
  rrdgOptions.ItemIndex[1, Ord(frEfficiencyReset)] := Ord(FarmProcess.EfficiencyReset);
  rrdgOptions.ItemIndex[1, Ord(frDeficiencyPolicy)] := Ord(FarmProcess.DeficiencyPolicy);
  rrdgOptions.ItemIndex[1, Ord(frWaterCostCoefficients)] := Ord(FarmProcess.WaterCostCoefficients);
  rrdgOptions.ItemIndex[1, Ord(frCropConsumptiveConcept)] := Ord(FarmProcess.CropConsumptiveConcept);
  rrdgOptions.ItemIndex[1, Ord(frCropConsumptiveLinkage)] := Ord(FarmProcess.CropConsumptiveLinkage);
  rrdgOptions.ItemIndex[1, Ord(frSurfaceWaterAllotment)] := Ord(FarmProcess.SurfaceWaterAllotment);
  rrdgOptions.ItemIndex[1, Ord(frSaveWellFlowRates)] := Ord(FarmProcess.SaveWellFlowRates);
  rrdgOptions.ItemIndex[1, Ord(frSaveNetRecharge)] := Ord(FarmProcess.SaveNetRecharge);
  rrdgOptions.ItemIndex[1, Ord(frSupplyAndDemand)] := Ord(FarmProcess.SupplyAndDemand);
  rrdgOptions.ItemIndex[1, Ord(frFarmBudgetPrintFlags)] := Ord(FarmProcess.FarmBudgetPrintFlags);
  rrdgOptions.ItemIndex[1, Ord(frPrintRouting)] := Ord(FarmProcess.PrintRouting);
  rrdgOptions.ItemIndex[1, Ord(frAcerageOptimizationPrintChoice)] := Ord(FarmProcess.AcerageOptimizationPrintChoice);
  rrdgOptions.ItemIndex[1, Ord(frAcerageOptimizationPrintLocation)] := Ord(FarmProcess.AcerageOptimizationPrintLocation);
  rrdgOptions.ItemIndex[1, Ord(frDiversionBudgetLocation)] := Ord(FarmProcess.DiversionBudgetLocation);
  rrdgOptions.ItemIndex[1, Ord(frCropIrrigationRequirement)] := Ord(FarmProcess.CropIrrigationRequirement);

  rgAssignmentMethod.ItemIndex := Ord(FarmProcess.AssignmentMethod);
end;

procedure TframePkgFarm.InitializeGrid;
var
  Select: TGridRect;
begin
  Select.Top := 0;
  Select.Left := 1;
  Select.BottomRight := Select.TopLeft;
  rrdgOptions.Selection := Select;
  rrdgOptions.FixedCols := 1;
  Assert(rrdgOptions.RowCount = Ord(High(TFarmRows))+1);

  rrdgOptions.Cells[0, Ord(frRootingDepth)] := 'Rooting depth (IRTFL)';
  rrdgOptions.Cells[0, Ord(frConsumptiveUse)] := 'Consumptive use (ICUFL)';
  rrdgOptions.Cells[0, Ord(frPrecipitation)] := 'Precipitation (IPFL)';
  rrdgOptions.Cells[0, Ord(frFractionOfInefficiencyLoses)] := 'Fraction of inefficiency loses (IIESWFL)';
  rrdgOptions.Cells[0, Ord(frEfficiencyGroundwaterFunction)] := 'Efficiency groundwater runction (IEBFL)';
  rrdgOptions.Cells[0, Ord(frEfficiencyReset)] := 'Efficiency reset (IEBFL)';
  rrdgOptions.Cells[0, Ord(frDeficiencyPolicy)] := 'Deficiency policy (IDEFFL)';
  rrdgOptions.Cells[0, Ord(frWaterCostCoefficients)] := 'Water cost coefficients (ICOST)';
  rrdgOptions.Cells[0, Ord(frCropConsumptiveConcept)] := 'Crop consumptive concept (ICCFL)';
  rrdgOptions.Cells[0, Ord(frCropConsumptiveLinkage)] := 'Crop consumptive linkage (ICCFL)';
  rrdgOptions.Cells[0, Ord(frSurfaceWaterAllotment)] := 'Surface water allotment (IALLOT)';
  rrdgOptions.Cells[0, Ord(frSaveWellFlowRates)] := 'Save well flow rates (IFWLCB)';
  rrdgOptions.Cells[0, Ord(frSaveNetRecharge)] := 'Save net recharge (IFNRCB)';
  rrdgOptions.Cells[0, Ord(frSupplyAndDemand)] := 'Supply and demand (ISDPFL)';
  rrdgOptions.Cells[0, Ord(frFarmBudgetPrintFlags)] := 'Farm budget print flags (IFBPFL)';
  rrdgOptions.Cells[0, Ord(frPrintRouting)] := 'Print routing (IRTPFL)';
  rrdgOptions.Cells[0, Ord(frAcerageOptimizationPrintChoice)] := 'Acerage optimization print choice (IOPFL)';
  rrdgOptions.Cells[0, Ord(frAcerageOptimizationPrintLocation)] := 'Acerage optimization print location (IOPFL)';
  rrdgOptions.Cells[0, Ord(frDiversionBudgetLocation)] := 'Diversion budget location (IPAPFL)';
  rrdgOptions.Cells[0, Ord(frCropIrrigationRequirement)] := 'Crop irrigation requirement (AUX NOCIRNOQ)';
end;

procedure TframePkgFarm.Loaded;
begin
  inherited;
  InitializeGrid;
end;

procedure TframePkgFarm.SetData(Package: TModflowPackageSelection);
var
  FarmProcess: TFarmProcess;
begin
  inherited;
  FarmProcess := Package as TFarmProcess;

  FarmProcess.RootingDepth := TRootingDepth(rrdgOptions.ItemIndex[1, Ord(frRootingDepth)]);
  FarmProcess.ConsumptiveUse := TConsumptiveUse(rrdgOptions.ItemIndex[1, Ord(frConsumptiveUse)]);
  FarmProcess.Precipitation := TPrecipitation(rrdgOptions.ItemIndex[1, Ord(frPrecipitation)]);
  FarmProcess.FractionOfInefficiencyLoses := TFractionOfInefficiencyLoses(rrdgOptions.ItemIndex[1, Ord(frFractionOfInefficiencyLoses)]);
  FarmProcess.EfficiencyGroundwaterFunction := TEfficiencyGroundwaterFunction(rrdgOptions.ItemIndex[1, Ord(frEfficiencyGroundwaterFunction)]);
  FarmProcess.EfficiencyReset := TEfficiencyReset(rrdgOptions.ItemIndex[1, Ord(frEfficiencyReset)]);
  FarmProcess.DeficiencyPolicy := TDeficiencyPolicy(rrdgOptions.ItemIndex[1, Ord(frDeficiencyPolicy)]);
  FarmProcess.WaterCostCoefficients := TWaterCostCoefficients(rrdgOptions.ItemIndex[1, Ord(frWaterCostCoefficients)]);
  FarmProcess.CropConsumptiveConcept := TCropConsumptiveConcept(rrdgOptions.ItemIndex[1, Ord(frCropConsumptiveConcept)]);
  FarmProcess.CropConsumptiveLinkage := TCropConsumptiveLinkage(rrdgOptions.ItemIndex[1, Ord(frCropConsumptiveLinkage)]);
  FarmProcess.SurfaceWaterAllotment := TSurfaceWaterAllotment(rrdgOptions.ItemIndex[1, Ord(frSurfaceWaterAllotment)]);
  FarmProcess.SaveWellFlowRates := TSaveWellFlowRates(rrdgOptions.ItemIndex[1, Ord(frSaveWellFlowRates)]);
  FarmProcess.SaveNetRecharge := TSaveNetRecharge(rrdgOptions.ItemIndex[1, Ord(frSaveNetRecharge)]);
  FarmProcess.SupplyAndDemand := TSupplyAndDemand(rrdgOptions.ItemIndex[1, Ord(frSupplyAndDemand)]);
  FarmProcess.FarmBudgetPrintFlags := TFarmBudgetPrintFlags(rrdgOptions.ItemIndex[1, Ord(frFarmBudgetPrintFlags)]);
  FarmProcess.PrintRouting := TPrintRouting(rrdgOptions.ItemIndex[1, Ord(frPrintRouting)]);
  FarmProcess.AcerageOptimizationPrintChoice := TAcerageOptimizationPrintChoice(rrdgOptions.ItemIndex[1, Ord(frAcerageOptimizationPrintChoice)]);
  FarmProcess.AcerageOptimizationPrintLocation := TAcerageOptimizationPrintLocation(rrdgOptions.ItemIndex[1, Ord(frAcerageOptimizationPrintLocation)]);
  FarmProcess.DiversionBudgetLocation := TDiversionBudgetLocation(rrdgOptions.ItemIndex[1, Ord(frDiversionBudgetLocation)]);
  FarmProcess.CropIrrigationRequirement := TCropIrrigationRequirement(rrdgOptions.ItemIndex[1, Ord(frCropIrrigationRequirement)]);

  FarmProcess.AssignmentMethod := TUpdateMethod(rgAssignmentMethod.ItemIndex);
end;

end.
