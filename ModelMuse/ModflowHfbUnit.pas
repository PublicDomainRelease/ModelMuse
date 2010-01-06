unit ModflowHfbUnit;

interface

uses Classes, RbwParser, GoPhastTypes, ModflowBoundaryUnit, SubscriptionUnit;

type
  TAdjustmentMethod = (amNone, amAllEdges, amNearlyParallel);

  THfbBoundary = class(TModflowScreenObjectProperty)
  private
    FAdjustmentMethod: TAdjustmentMethod;
    FThickness: string;
    FHydraulicConductivity: string;
    FParameterName: string;
    FUsed: boolean;
    FHfbObserver: TObserver;
    FHydraulicConductivityObserver: TObserver;
    FThicknessObserver: TObserver;
    FParameterNameObserver: TObserver;
    FAdjustmentMethodObserver: TObserver;
    FUsedObserver: TObserver;
    procedure SetAdjustmentMethod(const Value: TAdjustmentMethod);
    procedure SetHydraulicConductivity(Value: string);
    procedure SetParameterName(const Value: string);
    procedure SetThickness(const Value: string);
    procedure SetUsed(const Value: boolean);
    procedure HandleChangedFormula(OldFormula: string;
      var NewFormula: string; Observer: TObserver);
    function GetHydraulicConductivityObserver: TObserver;
    function GetThicknessObserver: TObserver;
    function GetParameterNameObserver: TObserver;
    procedure HandleChangedValue(Observer: TObserver);
    function GetAdjustmentMethodObserver: TObserver;
    procedure CreateObserver(ObserverNameRoot: string; var Observer: TObserver);
    function GetUsedObserver: TObserver;
  protected
    FModel: TObject;
    FScreenObject: TObject;
    property HydraulicConductivityObserver: TObserver
      read GetHydraulicConductivityObserver;
    property ThicknessObserver: TObserver read GetThicknessObserver;
    property ParameterNameObserver: TObserver read GetParameterNameObserver;
    property AdjustmentMethodObserver: TObserver read GetAdjustmentMethodObserver;
    property UsedObserver: TObserver read GetUsedObserver;
    procedure InvalidateModel;
  public
    Procedure Assign(Source: TPersistent); override;
    Constructor Create(Model, ScreenObject: TObject);
    destructor Destroy; override;
    function Used: boolean; override;
    property HfbObserver: TObserver read FHfbObserver;
    procedure HandleChangedParameterValue;
  published
    property ParameterName: string read FParameterName write SetParameterName;
    property HydraulicConductivityFormula: string read FHydraulicConductivity
      write SetHydraulicConductivity;
    property ThicknessFormula: string read FThickness write SetThickness;
    property AdjustmentMethod: TAdjustmentMethod read FAdjustmentMethod
      write SetAdjustmentMethod;
    property IsUsed: boolean read FUsed write SetUsed;

  end;

implementation

uses PhastModelUnit, ScreenObjectUnit;

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

constructor THfbBoundary.Create(Model, ScreenObject: TObject);
begin
  inherited Create;
  Assert((Model = nil) or (Model is TPhastModel));
  Assert((ScreenObject <> nil) and (ScreenObject is TScreenObject));
  FScreenObject := ScreenObject;
  FModel := Model;
  if TScreenObject(FScreenObject).CanInvalidateModel then
  begin
    FHfbObserver := TObserver.Create(nil);
    FHfbObserver.UpdateWithName('HfbBoundary_'
      + TScreenObject(FScreenObject).Name);
    TScreenObject(FScreenObject).TalksTo(FHfbObserver);
    FHfbObserver.UpToDate:= True;
  end;
  FThickness := '1';
  HydraulicConductivityFormula := '1e-8';
end;

destructor THfbBoundary.Destroy;
begin
  FHfbObserver.Free;
  FHydraulicConductivityObserver.Free;
  FThicknessObserver.Free;
  FParameterNameObserver.Free;
  FAdjustmentMethodObserver.Free;
  FUsedObserver.Free;
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

procedure THfbBoundary.InvalidateModel;
begin
  if FModel <> nil then
  begin
    (FModel as TPhastModel).Invalidate;
  end;
end;

procedure THfbBoundary.SetAdjustmentMethod(const Value: TAdjustmentMethod);
var
  ScreenObject: TScreenObject;
begin
  if FAdjustmentMethod <> Value then
  begin
    ScreenObject := FScreenObject as TScreenObject;
    if ScreenObject.CanInvalidateModel then
    begin
      HandleChangedValue(AdjustmentMethodObserver);
    end;

    FAdjustmentMethod := Value;
    InvalidateModel;
  end;
end;

procedure THfbBoundary.SetHydraulicConductivity(Value: string);
var
  ScreenObject: TScreenObject;
begin
  if FHydraulicConductivity <> Value then
  begin
    ScreenObject := FScreenObject as TScreenObject;
    if ScreenObject.CanInvalidateModel then
    begin
      HandleChangedFormula(FHydraulicConductivity, Value,
        HydraulicConductivityObserver);
    end;
    FHydraulicConductivity := Value;
    InvalidateModel;
  end;
end;

procedure THfbBoundary.SetParameterName(const Value: string);
var
  ScreenObject: TScreenObject;
begin
  if FParameterName <> Value then
  begin
    ScreenObject := FScreenObject as TScreenObject;
    if ScreenObject.CanInvalidateModel then
    begin
      HandleChangedValue(ParameterNameObserver);
    end;

    FParameterName := Value;
    InvalidateModel;
  end;
end;

procedure THfbBoundary.SetThickness(const Value: string);
var
  ScreenObject: TScreenObject;
begin
  if FThickness <> Value then
  begin
    ScreenObject := FScreenObject as TScreenObject;
    if ScreenObject.CanInvalidateModel then
    begin
      HandleChangedFormula(Value, FThickness, ThicknessObserver);
    end;
    FThickness := Value;
    InvalidateModel;
  end;
end;

procedure THfbBoundary.SetUsed(const Value: boolean);
var
  ScreenObject: TScreenObject;
begin
  if FUsed <> Value then
  begin
    ScreenObject := FScreenObject as TScreenObject;
    if ScreenObject.CanInvalidateModel then
    begin
      HandleChangedValue(UsedObserver);
    end;

    FUsed := Value;
    InvalidateModel;
  end;
end;

function THfbBoundary.Used: boolean;
begin
  result := IsUsed;
end;

procedure THfbBoundary.CreateObserver(ObserverNameRoot: string;
  var Observer: TObserver);
var
  ScreenObject: TScreenObject;
  Model: TPhastModel;
begin
  ScreenObject := FScreenObject as TScreenObject;
  Model := FModel as TPhastModel;
  Assert(ScreenObject.CanInvalidateModel);
  Assert(Model <> nil);
  Observer := TObserver.Create(nil);
  Observer.UpdateWithName(ObserverNameRoot + ScreenObject.Name);
  FHfbObserver.TalksTo(Model.HfbDisplayer);
  Observer.TalksTo(Model.HfbDisplayer);
  FHfbObserver.TalksTo(Observer);
  Model.HfbDisplayer.Invalidate;
end;

procedure THfbBoundary.HandleChangedValue(Observer: TObserver);
var
  Model: TPhastModel;
begin
  Model := FModel as TPhastModel;
  if not (csDestroying in Model.ComponentState) then
  begin
    Observer.UpToDate := True;
    Observer.UpToDate := False;
    Model.HfbDisplayer.Invalidate;
    Observer.UpToDate := True;
  end;
end;

procedure THfbBoundary.HandleChangedFormula(OldFormula: string;
  var NewFormula: string; Observer: TObserver);
var
  AFunction: string;
  UseIndex: Integer;
  OtherIndex: Integer;
  UsedVariable: TObserver;
  Model: TPhastModel;
  OldUseList: TStringList;
  NewUseList: TStringList;
  Compiler: TRbwParser;
begin
  Model := FModel as TPhastModel;
  InvalidateModel;
  if OldFormula = '' then
  begin
    OldFormula := '0';
  end;
  OldUseList := TStringList.Create;
  NewUseList := TStringList.Create;
  try
    Compiler := Model.GetCompiler(dso3D, eaBlocks);
    try
      Compiler.Compile(OldFormula);
      OldUseList.Assign(Compiler.CurrentExpression.VariablesUsed);
    except
      on E: ERbwParserError do
        OldUseList.Clear;
    end;
    AFunction := NewFormula;
    Compiler.Compile(AFunction);
    NewUseList.Assign(Compiler.CurrentExpression.VariablesUsed);
    NewFormula := Compiler.CurrentExpression.Decompile;
    for UseIndex := OldUseList.Count - 1 downto 0 do
    begin
      OtherIndex := NewUseList.IndexOf(OldUseList[UseIndex]);
      if OtherIndex >= 0 then
      begin
        OldUseList.Delete(UseIndex);
        NewUseList.Delete(OtherIndex);
      end;
    end;
    for UseIndex := 0 to OldUseList.Count - 1 do
    begin
      UsedVariable := (FModel as TPhastModel).
        GetObserverByName(OldUseList[UseIndex]);
      Assert(UsedVariable <> nil);
      UsedVariable.StopsTalkingTo(Observer);
    end;
    for UseIndex := 0 to NewUseList.Count - 1 do
    begin
      UsedVariable := (FModel as TPhastModel).
        GetObserverByName(NewUseList[UseIndex]);
      Assert(UsedVariable <> nil);
      UsedVariable.TalksTo(Observer);
    end;
  finally
    OldUseList.Free;
    NewUseList.Free;
  end;
  HandleChangedValue(Observer)
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
