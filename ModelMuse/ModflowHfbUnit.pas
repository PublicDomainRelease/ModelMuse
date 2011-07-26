unit ModflowHfbUnit;

interface

uses Classes, RbwParser, GoPhastTypes, ModflowBoundaryUnit, SubscriptionUnit,
  FormulaManagerUnit, Contnrs, SysUtils;

type
  TAdjustmentMethod = (amNone, amAllEdges, amNearlyParallel);

  THfbBoundary = class(TModflowScreenObjectProperty)
  private
    FAdjustmentMethod: TAdjustmentMethod;
    FThicknessFormula: TFormulaObject;
    FHydraulicConductivityFormula: TFormulaObject;
    FParameterName: string;
    FUsed: boolean;
    FHfbObserver: TObserver;
    FHydraulicConductivityObserver: TObserver;
    FThicknessObserver: TObserver;
    FParameterNameObserver: TObserver;
    FAdjustmentMethodObserver: TObserver;
    FUsedObserver: TObserver;
    FObserverList: TList;
    procedure SetAdjustmentMethod(const Value: TAdjustmentMethod);
    procedure SetHydraulicConductivity(Value: string);
    procedure SetParameterName(const Value: string);
    procedure SetThickness(const Value: string);
    procedure SetUsed(const Value: boolean);
    function GetHydraulicConductivityObserver: TObserver;
    function GetThicknessObserver: TObserver;
    function GetParameterNameObserver: TObserver;
    procedure HandleChangedValue(Observer: TObserver);
    function GetAdjustmentMethodObserver: TObserver;
    procedure CreateObserver(ObserverNameRoot: string; var Observer: TObserver);
    function GetUsedObserver: TObserver;
    function CreateFormulaObject(Orientation:
      TDataSetOrientation): TFormulaObject;
    procedure UpdateFormula(Value: string; Position: Integer;
      var FormulaObject: TFormulaObject);
    procedure UpdateFormulaDependencies(OldFormula: string; var
      NewFormula: string; Observer: TObserver; Compiler: TRbwParser);
    function GetHydraulicConductivity: string;
    function GetThickness: string;
    procedure ResetItemObserver(Index: integer);
    procedure RemoveSubscription(Sender: TObject; const AName: string);
    procedure RestoreSubscription(Sender: TObject; const AName: string);
    procedure GetPropertyObserver(Sender: TObject; List: TList);
  protected
    procedure CreateFormulaObjects;
    property HydraulicConductivityObserver: TObserver
      read GetHydraulicConductivityObserver;
    property ThicknessObserver: TObserver read GetThicknessObserver;
    property ParameterNameObserver: TObserver read GetParameterNameObserver;
    property AdjustmentMethodObserver: TObserver read GetAdjustmentMethodObserver;
    property UsedObserver: TObserver read GetUsedObserver;
  public
    Procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    function Used: boolean; override;
    property HfbObserver: TObserver read FHfbObserver;
    procedure HandleChangedParameterValue;
    procedure InvalidateDisplay;
  published
    property ParameterName: string read FParameterName write SetParameterName;
    property HydraulicConductivityFormula: string read GetHydraulicConductivity
      write SetHydraulicConductivity;
    property ThicknessFormula: string read GetThickness write SetThickness;
    property AdjustmentMethod: TAdjustmentMethod read FAdjustmentMethod
      write SetAdjustmentMethod;
    property IsUsed: boolean read FUsed write SetUsed;

  end;

procedure RemoveHfbModflowBoundarySubscription(Sender: TObject; Subject: TObject;
  const AName: string);
procedure RestoreHfbModflowBoundarySubscription(Sender: TObject; Subject: TObject;
  const AName: string);

implementation

uses PhastModelUnit, ScreenObjectUnit, frmGoPhastUnit;

const
  ThicknessPositon = 0;
  HydraulicConductivityPositon = 1;

procedure RemoveHfbModflowBoundarySubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as THfbBoundary).RemoveSubscription(Sender, AName);
end;

procedure RestoreHfbModflowBoundarySubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as THfbBoundary).RestoreSubscription(Sender, AName);
end;


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

constructor THfbBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  CreateFormulaObjects;
  FObserverList := TObjectList.Create;

  Assert((ScreenObject = nil) or (ScreenObject is TScreenObject));
  FModel := Model;
  if ScreenObject <> nil then
  begin
    if TScreenObject(FScreenObject).CanInvalidateModel then
    begin
      FHfbObserver := TObserver.Create(nil);
      FHfbObserver.UpdateWithName('HfbBoundary_'
        + TScreenObject(FScreenObject).Name);
      TScreenObject(FScreenObject).TalksTo(FHfbObserver);
      FHfbObserver.UpToDate:= True;
    end;
  end;
  FObserverList.Add(ThicknessObserver);
  FObserverList.Add(HydraulicConductivityObserver);

  ThicknessFormula := '1';
  HydraulicConductivityFormula := '1e-8';
end;

function THfbBoundary.CreateFormulaObject(
  Orientation: TDataSetOrientation): TFormulaObject;
begin
  result := frmGoPhast.PhastModel.FormulaManager.Add;
  case Orientation of
    dsoTop:
      begin
        result.Parser := frmGoPhast.PhastModel.rpTopFormulaCompiler;
      end;
    dso3D:
      begin
        result.Parser := frmGoPhast.PhastModel.rpThreeDFormulaCompiler;
      end;
    else Assert(False);
  end;
  result.AddSubscriptionEvents(
    RemoveHfbModflowBoundarySubscription,
    RestoreHfbModflowBoundarySubscription, self);
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

  FHfbObserver.Free;

  FParameterNameObserver.Free;
  FAdjustmentMethodObserver.Free;
  FUsedObserver.Free;
  FObserverList.Free;
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
  ResetItemObserver(HydraulicConductivityPositon);
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
    List.Add(FObserverList[ThicknessPositon]);
  end;
  if Sender = FHydraulicConductivityFormula then
  begin
    List.Add(FObserverList[HydraulicConductivityPositon]);
  end;
end;

function THfbBoundary.GetThickness: string;
begin
  Result := FThicknessFormula.Formula;
  ResetItemObserver(ThicknessPositon);
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
  UpdateFormula(Value, HydraulicConductivityPositon, FHydraulicConductivityFormula);
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
  UpdateFormula(Value, ThicknessPositon, FThicknessFormula);
end;

procedure THfbBoundary.SetUsed(const Value: boolean);
var
  ScreenObject: TScreenObject;
begin
  if FUsed <> Value then
  begin
    ScreenObject := FScreenObject as TScreenObject;
    if FScreenObject <> nil then
    begin

      if ScreenObject.CanInvalidateModel then
      begin
        HandleChangedValue(UsedObserver);
      end;
    end;

    FUsed := Value;
    InvalidateModel;
  end;
end;

procedure THfbBoundary.UpdateFormula(Value: string; Position: Integer;
  var FormulaObject: TFormulaObject);
var
  LocalModel: TPhastModel;
  Compiler: TRbwParser;
  Observer: TObserver;
begin
  if FormulaObject.Formula <> Value then
  begin
    LocalModel := ParentModel as TPhastModel;
    if LocalModel <> nil then
    begin
      Compiler := LocalModel.rpThreeDFormulaCompiler;
      Observer := FObserverList[Position];
      UpdateFormulaDependencies(FormulaObject.Formula, Value, Observer, Compiler);
    end;
    InvalidateModel;

    if not (csDestroying in frmGoPhast.PhastModel.ComponentState) then
    begin
      frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
        FormulaObject, Value, frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
        RemoveHfbModflowBoundarySubscription,
        RestoreHfbModflowBoundarySubscription, self);
    end;
  end;
end;

procedure THfbBoundary.UpdateFormulaDependencies(OldFormula: string;
  var NewFormula: string; Observer: TObserver; Compiler: TRbwParser);
var
  OldUses: TStringList;
  NewUses: TStringList;
  Position: Integer;
  DS: TObserver;
  ParentScreenObject: TScreenObject;
  Index: integer;
  procedure CompileFormula(var AFormula: string;
    UsesList: TStringList);
  begin
    if AFormula <> '' then
    begin
      try
        Compiler.Compile(AFormula);
        UsesList.Assign(Compiler.CurrentExpression.VariablesUsed);
      except on E: ERbwParserError do
        begin
        end;
      end;
    end;
  end;
begin
  OldFormula := Trim(OldFormula);
  NewFormula := Trim(NewFormula);
  if OldFormula = NewFormula then
  begin
    Exit;
  end;
  if (frmGoPhast.PhastModel <> nil)
    and ((frmGoPhast.PhastModel.ComponentState * [csLoading, csReading]) <> []) then
  begin
    Exit;
  end;
  ParentScreenObject := ScreenObject as TScreenObject;
  if (ParentScreenObject = nil)
//    or not ParentScreenObject.CanInvalidateModel then
    // 3
        {or not ParentScreenObject.CanInvalidateModel} then
  begin
    Exit;
  end;
  OldUses := TStringList.Create;
  NewUses := TStringList.Create;
  try
    CompileFormula(OldFormula, OldUses);
    CompileFormula(NewFormula, NewUses);
    for Index := OldUses.Count - 1 downto 0 do
    begin
      Position := NewUses.IndexOf(OldUses[Index]);
      if Position >= 0 then
      begin
        OldUses.Delete(Index);
        NewUses.Delete(Position);
      end;
    end;
    for Index := 0 to OldUses.Count - 1 do
    begin
      DS := frmGoPhast.PhastModel.GetObserverByName(OldUses[Index]);
      Assert(DS <> nil);
      DS.StopsTalkingTo(Observer);
    end;
    for Index := 0 to NewUses.Count - 1 do
    begin
      DS := frmGoPhast.PhastModel.GetObserverByName(NewUses[Index]);
      Assert(DS <> nil);
      DS.TalksTo(Observer);
    end;
  finally
    NewUses.Free;
    OldUses.Free;
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
  Observer := TObserver.Create(nil);
  Observer.UpdateWithName(ObserverNameRoot + ScreenObject.Name);
  if ScreenObject.CanInvalidateModel then
  begin
    Model := FModel as TPhastModel;
    Assert(Model <> nil);
    FHfbObserver.TalksTo(Model.HfbDisplayer);
    Observer.TalksTo(Model.HfbDisplayer);
    FHfbObserver.TalksTo(Observer);
    Model.HfbDisplayer.Invalidate;
  end;
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

procedure THfbBoundary.RemoveSubscription(Sender: TObject; const AName: string);
var
  Observer: TObserver;
  DS: TObserver;
  Observers: TList;
  ObserverIndex: Integer;
begin
  Observers := TList.Create;
  try
    GetPropertyObserver(Sender, Observers);
    for ObserverIndex := 0 to Observers.Count - 1 do
    begin
      Observer := Observers[ObserverIndex];
      DS := frmGoPhast.PhastModel.GetObserverByName(AName);
      DS.StopsTalkingTo(Observer);
    end;
  finally
    Observers.Free;
  end;
end;

procedure THfbBoundary.ResetItemObserver(Index: integer);
var
  Observer: TObserver;
begin
  Observer := FObserverList[Index];
  Observer.UpToDate := True;
end;

procedure THfbBoundary.RestoreSubscription(Sender: TObject;
  const AName: string);
var
  Observer: TObserver;
  DS: TObserver;
  Observers: TList;
  ObserverIndex: Integer;
begin
  Observers := TList.Create;
  try
    GetPropertyObserver(Sender, Observers);
    for ObserverIndex := 0 to Observers.Count - 1 do
    begin
      Observer := Observers[ObserverIndex];
      DS := frmGoPhast.PhastModel.GetObserverByName(AName);
      DS.TalksTo(Observer);
      Observer.UpToDate := False;
    end;
  finally
    Observers.Free;
  end;
end;

//procedure THfbBoundary.HandleChangedFormula(OldFormula: string;
//  var NewFormula: string; Observer: TObserver);
//var
//  AFunction: string;
//  UseIndex: Integer;
//  OtherIndex: Integer;
//  UsedVariable: TObserver;
//  Model: TPhastModel;
//  OldUseList: TStringList;
//  NewUseList: TStringList;
//  Compiler: TRbwParser;
//begin
//  Model := FModel as TPhastModel;
//  InvalidateModel;
//  if OldFormula = '' then
//  begin
//    OldFormula := '0';
//  end;
//  OldUseList := TStringList.Create;
//  NewUseList := TStringList.Create;
//  try
//    Compiler := Model.GetCompiler(dso3D, eaBlocks);
//    try
//      Compiler.Compile(OldFormula);
//      OldUseList.Assign(Compiler.CurrentExpression.VariablesUsed);
//    except
//      on E: ERbwParserError do
//        OldUseList.Clear;
//    end;
//    AFunction := NewFormula;
//    try
//      Compiler.Compile(AFunction);
//      NewUseList.Assign(Compiler.CurrentExpression.VariablesUsed);
//      NewFormula := Compiler.CurrentExpression.Decompile;
//    except
//      on E: ERbwParserError do
//      begin
//        if not (frmGoPhast.PhastModel.ComponentState = [csLoading, csReading]) then
//        begin
//          raise;
//        end
//        else
//        begin
//          NewUseList.Clear;
//        end;
//      end;
//    end;
//    for UseIndex := OldUseList.Count - 1 downto 0 do
//    begin
//      OtherIndex := NewUseList.IndexOf(OldUseList[UseIndex]);
//      if OtherIndex >= 0 then
//      begin
//        OldUseList.Delete(UseIndex);
//        NewUseList.Delete(OtherIndex);
//      end;
//    end;
//    for UseIndex := 0 to OldUseList.Count - 1 do
//    begin
//      UsedVariable := (FModel as TPhastModel).
//        GetObserverByName(OldUseList[UseIndex]);
//      Assert(UsedVariable <> nil);
//      UsedVariable.StopsTalkingTo(Observer);
//    end;
//    for UseIndex := 0 to NewUseList.Count - 1 do
//    begin
//      UsedVariable := (FModel as TPhastModel).
//        GetObserverByName(NewUseList[UseIndex]);
//      Assert(UsedVariable <> nil);
//      UsedVariable.TalksTo(Observer);
//    end;
//  finally
//    OldUseList.Free;
//    NewUseList.Free;
//  end;
//  HandleChangedValue(Observer)
//end;

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
