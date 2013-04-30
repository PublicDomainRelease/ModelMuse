unit ModflowHfbUnit;

interface

uses Classes, RbwParser, GoPhastTypes, ModflowBoundaryUnit, SubscriptionUnit,
  FormulaManagerUnit, Contnrs, SysUtils;

type
  TAdjustmentMethod = (amNone, amAllEdges, amNearlyParallel);

  THfbBoundary = class(TModflowSteadyBoundary)
  private
    FAdjustmentMethod: TAdjustmentMethod;
    FThicknessFormula: TFormulaObject;
    FHydraulicConductivityFormula: TFormulaObject;
    FParameterName: string;
    FHydraulicConductivityObserver: TObserver;
    FThicknessObserver: TObserver;
    FParameterNameObserver: TObserver;
    FAdjustmentMethodObserver: TObserver;
    procedure SetAdjustmentMethod(const Value: TAdjustmentMethod);
    procedure SetHydraulicConductivity(Value: string);
    procedure SetParameterName(const Value: string);
    procedure SetThickness(const Value: string);
    function GetHydraulicConductivityObserver: TObserver;
    function GetThicknessObserver: TObserver;
    function GetParameterNameObserver: TObserver;
    function GetAdjustmentMethodObserver: TObserver;
    function GetHydraulicConductivity: string;
    function GetThickness: string;
  protected
    procedure HandleChangedValue(Observer: TObserver); override;
    function GetUsedObserver: TObserver; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure CreateFormulaObjects; override;
    property HydraulicConductivityObserver: TObserver
      read GetHydraulicConductivityObserver;
    property ThicknessObserver: TObserver read GetThicknessObserver;
    property ParameterNameObserver: TObserver read GetParameterNameObserver;
    property AdjustmentMethodObserver: TObserver read GetAdjustmentMethodObserver;
    function BoundaryObserverPrefix: string; override;
    procedure CreateObservers; override;
  public
    Procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    procedure HandleChangedParameterValue;
    procedure InvalidateDisplay;
  published
    property ParameterName: string read FParameterName write SetParameterName;
    property HydraulicConductivityFormula: string read GetHydraulicConductivity
      write SetHydraulicConductivity;
    property ThicknessFormula: string read GetThickness write SetThickness;
    property AdjustmentMethod: TAdjustmentMethod read FAdjustmentMethod
      write SetAdjustmentMethod;
  end;

implementation

uses PhastModelUnit, ScreenObjectUnit, frmGoPhastUnit;

const
  ThicknessPosition = 0;
  HydraulicConductivityPosition = 1;

//procedure RemoveHfbModflowBoundarySubscription(Sender: TObject; Subject: TObject;
//  const AName: string);
//begin
//  (Subject as THfbBoundary).RemoveSubscription(Sender, AName);
//end;
//
//procedure RestoreHfbModflowBoundarySubscription(Sender: TObject; Subject: TObject;
//  const AName: string);
//begin
//  (Subject as THfbBoundary).RestoreSubscription(Sender, AName);
//end;


{ THfbBoundary }

procedure THfbBoundary.Assign(Source: TPersistent);
var
  SourecHFB: THfbBoundary;
begin
  if Source is THfbBoundary then
  begin
    SourecHFB := THfbBoundary(Source);
    ParameterName := SourecHFB.ParameterName;
    HydraulicConductivityFormula := SourecHFB.HydraulicConductivityFormula;
    ThicknessFormula := SourecHFB.ThicknessFormula;
    AdjustmentMethod := SourecHFB.AdjustmentMethod;
    IsUsed := SourecHFB.IsUsed;
  end
  else
  begin
    inherited;
  end;
end;

function THfbBoundary.BoundaryObserverPrefix: string;
begin
  result := 'HfbBoundary_';
end;

constructor THfbBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;

  ThicknessFormula := '1';
  HydraulicConductivityFormula := '1e-8';
end;

procedure THfbBoundary.CreateFormulaObjects;
begin
  FThicknessFormula := CreateFormulaObject(dso3D);
  FHydraulicConductivityFormula := CreateFormulaObject(dso3D);
end;

destructor THfbBoundary.Destroy;
begin
  HydraulicConductivityFormula := '0';
  ThicknessFormula := '0';

  FParameterNameObserver.Free;
  FAdjustmentMethodObserver.Free;
  inherited;
end;

function THfbBoundary.GetAdjustmentMethodObserver: TObserver;
begin
  if FAdjustmentMethodObserver = nil then
  begin
    CreateObserver('HFB_AdjustmentMethod_', FAdjustmentMethodObserver);
  end;
  result := FAdjustmentMethodObserver;
end;

function THfbBoundary.GetHydraulicConductivity: string;
begin
  Result := FHydraulicConductivityFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(HydraulicConductivityPosition);
  end;
end;

function THfbBoundary.GetHydraulicConductivityObserver: TObserver;
begin
  if FHydraulicConductivityObserver = nil then
  begin
    CreateObserver('HFB_HydraulicConductivity_',
      FHydraulicConductivityObserver);
  end;
  result := FHydraulicConductivityObserver;
end;

function THfbBoundary.GetParameterNameObserver: TObserver;
begin
  if FParameterNameObserver = nil then
  begin
    CreateObserver('HFB_ParameterName_', FParameterNameObserver);
  end;
  result := FParameterNameObserver;
end;

procedure THfbBoundary.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FThicknessFormula then
  begin
    List.Add(FObserverList[ThicknessPosition]);
  end;
  if Sender = FHydraulicConductivityFormula then
  begin
    List.Add(FObserverList[HydraulicConductivityPosition]);
  end;
end;

function THfbBoundary.GetThickness: string;
begin
  Result := FThicknessFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(ThicknessPosition);
  end;
end;

function THfbBoundary.GetThicknessObserver: TObserver;
begin
  if FThicknessObserver = nil then
  begin
    CreateObserver('HFB_Thickness_', FThicknessObserver);
  end;
  result := FThicknessObserver;
end;

function THfbBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('HFB_Used_', FUsedObserver);
  end;
  result := FUsedObserver;
end;

procedure THfbBoundary.SetAdjustmentMethod(const Value: TAdjustmentMethod);
var
  ScreenObject: TScreenObject;
begin
  if FAdjustmentMethod <> Value then
  begin
    if FScreenObject <> nil then
    begin
      ScreenObject := FScreenObject as TScreenObject;
      if ScreenObject.CanInvalidateModel then
      begin
        HandleChangedValue(AdjustmentMethodObserver);
      end;
    end;
    FAdjustmentMethod := Value;
    InvalidateModel;
  end;
end;

procedure THfbBoundary.SetHydraulicConductivity(Value: string);
begin
  UpdateFormula(Value, HydraulicConductivityPosition, FHydraulicConductivityFormula);
end;

procedure THfbBoundary.SetParameterName(const Value: string);
var
  ScreenObject: TScreenObject;
begin
  if FParameterName <> Value then
  begin
    ScreenObject := FScreenObject as TScreenObject;
    if FScreenObject <> nil then
    begin
      if ScreenObject.CanInvalidateModel then
      begin
        HandleChangedValue(ParameterNameObserver);
      end;
    end;

    FParameterName := Value;
    InvalidateModel;
  end;
end;

procedure THfbBoundary.SetThickness(const Value: string);
begin
  UpdateFormula(Value, ThicknessPosition, FThicknessFormula);
end;

procedure THfbBoundary.HandleChangedValue(Observer: TObserver);
var
  Model: TPhastModel;
  ChildIndex: Integer;
begin
  Model := FModel as TPhastModel;
  if not (csDestroying in Model.ComponentState) then
  begin
    Observer.UpToDate := True;
    Observer.UpToDate := False;
    Model.HfbDisplayer.Invalidate;
    for ChildIndex := 0 to Model.ChildModels.Count - 1 do
    begin
      Model.ChildModels[ChildIndex].ChildModel.HfbDisplayer.Invalidate;
    end;
    Observer.UpToDate := True;
  end;
end;

procedure THfbBoundary.InvalidateDisplay;
begin
  if Used and (ParentModel <> nil) then
  begin
    HandleChangedValue(HydraulicConductivityObserver);
    HandleChangedValue(ThicknessObserver);
  end;
end;

procedure THfbBoundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(ThicknessObserver);
    FObserverList.Add(HydraulicConductivityObserver);
  end;
end;


procedure THfbBoundary.HandleChangedParameterValue;
var
  ScreenObject: TScreenObject;
begin
  ScreenObject := FScreenObject as TScreenObject;
  if ScreenObject.CanInvalidateModel then
  begin
    HandleChangedValue(ParameterNameObserver);
  end;
end;

initialization
  RegisterClass(THfbBoundary);

end.
