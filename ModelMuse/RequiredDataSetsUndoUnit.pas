unit RequiredDataSetsUndoUnit;

interface

uses Classes, GoPhastTypes, UndoItems, PhastModelUnit, frmShowHideObjectsUnit,
  ModflowParameterUnit, HufDefinition, ModflowTransientListParameterUnit,
  UndoItemsScreenObjects;

type
  TCustomCreateRequiredDataSetsUndo = class(TCustomUndo)
  private
    FNewSteadyModflowParameterDataSets: TList;
    FNewPackageDataSets: TList;
  // If a @link(TDataArray) whose name is DataSetName does not exist and
  // ArrayNeeded returns @true, then @name will create a new
  // @link(TDataArray). Orientation, DataType, DataSetName, ArrayNeeded,
  // NewFormula, and Classification will be used to assign properties to
  // the new @link(TDataArray).
  //
  // @name is called by @link(UpdatePackageLayers).
    procedure UpdateDataArray(Model: TPhastModel; Index: integer);
    // @name checks if all the data sets for the selected packages
    // exist and creates them if they do not.
    procedure UpdatePackageLayers;
    procedure UpdateOnPostInitialize;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
    // causes all the @link(TDataArray)s that are required to be created
    // if they do not already exist.
    procedure UpdatedRequiredDataSets;
  end;

  TCustomUndoChangeParameters = class(TCustomCreateRequiredDataSetsUndo)
  private
    FNewSteadyParameters: TModflowSteadyParameters;
    FOldSteadyParameters: TModflowSteadyParameters;
    FNewTransientParameters: TModflowTransientListParameters;
    FOldTransientParameters: TModflowTransientListParameters;
    FOldHufModflowParameters: THufModflowParameters;
    FNewHufModflowParameters: THufModflowParameters;
//    FExistingScreenObjects: TScreenObjectEditCollection;
  protected
    function Description: string; override;
  public
    Constructor Create(var NewSteadyParameters: TModflowSteadyParameters;
      var NewTransientParameters: TModflowTransientListParameters;
      var NewHufModflowParameters: THufModflowParameters);
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TUndoModelSelectionChange = class(TCustomCreateRequiredDataSetsUndo)
  private
    FOldModelSelection: TModelSelection;
    FNewModelSelection: TModelSelection;
  protected
    function Description: string; override;
  public
    Constructor Create(NewModelSelection: TModelSelection);
    procedure DoCommand; override;
    procedure Undo; override;
  end;

implementation

uses DataSetUnit, RbwParser, frmGoPhastUnit, frmGridColorUnit, 
  frmContourDataUnit;

constructor TCustomCreateRequiredDataSetsUndo.Create;
begin
  FNewSteadyModflowParameterDataSets := TList.Create;
  FNewPackageDataSets := TList.Create;
end;

destructor TCustomCreateRequiredDataSetsUndo.Destroy;
begin
  FNewPackageDataSets.Free;
  FNewSteadyModflowParameterDataSets.Free;

  inherited;
end;

procedure TCustomCreateRequiredDataSetsUndo.DoCommand;
begin
  frmGoPhast.PhastModel.ModflowSteadyParameters.NewDataSets := FNewSteadyModflowParameterDataSets;
end;

procedure TCustomCreateRequiredDataSetsUndo.Undo;
begin
  frmGoPhast.PhastModel.ModflowSteadyParameters.NewDataSets := FNewSteadyModflowParameterDataSets;
end;

procedure TCustomCreateRequiredDataSetsUndo.UpdateDataArray(Model: TPhastModel; Index: integer);
var
  DataArray: TDataArray;
//  DataArrayIndex: Integer;
  DataSetName: string;
  Orientation: TDataSetOrientation;
  DataType: TRbwDataType;
  ArrayNeeded, CreateDataSet: TObjectUsedEvent;
  NewFormula, Classification: string;
  Lock: TDataLock;
begin
  { TODO : Find a way to extract common code from
TPhastModel.CreateModflowDataSets and
TCustomCreateRequiredDataSetsUndo.UpdateDataArray}
  DataSetName := Model.FDataArrayCreationRecords[Index].Name;
  Orientation := Model.FDataArrayCreationRecords[Index].Orientation;
  DataType := Model.FDataArrayCreationRecords[Index].DataType;
  ArrayNeeded := Model.FDataArrayCreationRecords[Index].DataSetNeeded;
  CreateDataSet := Model.FDataArrayCreationRecords[Index].DataSetShouldBeCreated;
  NewFormula := Model.FDataArrayCreationRecords[Index].Formula;
  Classification := Model.FDataArrayCreationRecords[Index].Classification;
  Lock := Model.FDataArrayCreationRecords[Index].Lock;

  DataArray := Model.GetDataSetByName(DataSetName);
//  DataArray := nil;
  Assert(Assigned(ArrayNeeded));
  if DataArray <> nil then
  begin
//    DataArray := Model.DataSets[DataArrayIndex];
    DataArray.Name := DataSetName;
    DataArray.Lock := Lock;
  end
  else if ArrayNeeded(self)
    or (Assigned(CreateDataSet) and CreateDataSet(self)) then
  begin
    DataArray := Model.CreateNewDataArray(TDataArray, DataSetName, NewFormula,
      Lock, DataType, Model.FDataArrayCreationRecords[Index].EvaluatedAt,
      Orientation, Classification);
    DataArray.OnDataSetUsed := ArrayNeeded;
    DataArray.Lock := Lock;
    DataArray.CheckMax := Model.FDataArrayCreationRecords[Index].CheckMax;
    DataArray.CheckMin := Model.FDataArrayCreationRecords[Index].CheckMin;
    DataArray.Max := Model.FDataArrayCreationRecords[Index].Max;
    DataArray.Min := Model.FDataArrayCreationRecords[Index].Min;

    FNewPackageDataSets.Add(DataArray);
  end;
  if DataArray <> nil then
  begin
    DataArray.AssociatedDataSets :=
      Model.FDataArrayCreationRecords[Index].AssociatedDataSets;
  end;
  if (DataArray <> nil) and (Model.Grid <> nil) then
  begin
    DataArray.UpdateDimensions(Model.Grid.LayerCount, Model.Grid.RowCount,
      Model.Grid.ColumnCount);
  end;
  Model.UpdateDataArrayParameterUsed;
end;

procedure TCustomCreateRequiredDataSetsUndo.UpdatePackageLayers;
var
  Model: TPhastModel;
  Index: Integer;
begin
  Model:= frmGoPhast.PhastModel;

  for Index := 0 to Length(Model.FDataArrayCreationRecords) - 1 do
  begin
    UpdateDataArray(Model, Index);
  end;

  UpdateFrmGridColor;
  UpdateFrmContourData;
end;

procedure TCustomCreateRequiredDataSetsUndo.UpdateOnPostInitialize;
begin
  frmGoPhast.PhastModel.UpdateOnPostInitialize;
end;

procedure TCustomCreateRequiredDataSetsUndo.UpdatedRequiredDataSets;
begin
  UpdatePackageLayers;
  UpdateOnPostInitialize;
  if frmShowHideObjects <> nil then
  begin
    frmShowHideObjects.UpdateScreenObjects;
  end;
end;

{ TUndoModelSelectionChange }

constructor TUndoModelSelectionChange.Create(NewModelSelection: TModelSelection);
begin
  inherited Create;
  FOldModelSelection := frmGoPhast.ModelSelection;
  FNewModelSelection := NewModelSelection;
end;

function TUndoModelSelectionChange.Description: string;
begin
  result := 'change model selection';
end;

procedure TUndoModelSelectionChange.DoCommand;
begin
  inherited;
  frmGoPhast.ModelSelection := FNewModelSelection;
  UpdatedRequiredDataSets;
end;

procedure TUndoModelSelectionChange.Undo;
begin
  inherited;
  frmGoPhast.ModelSelection := FOldModelSelection;
  UpdatedRequiredDataSets;
end;

{ TUndoChangeParameters }

constructor TCustomUndoChangeParameters.Create(
  var NewSteadyParameters: TModflowSteadyParameters;
  var NewTransientParameters: TModflowTransientListParameters;
  var NewHufModflowParameters: THufModflowParameters);
begin
  inherited Create;
  FNewSteadyParameters:= NewSteadyParameters;
  // TUndoDefineLayers takes ownership of NewSteadyParameters.
  NewSteadyParameters := nil;

  FNewTransientParameters := NewTransientParameters;
  // TUndoDefineLayers takes ownership of NewTransientParameters.
  NewTransientParameters := nil;

  // TUndoDefineLayers takes ownership of NewHufModflowParameters.
  FNewHufModflowParameters := NewHufModflowParameters;
  NewHufModflowParameters := nil;

  FOldSteadyParameters:= TModflowSteadyParameters.Create(nil);
  FOldSteadyParameters.Assign(frmGoPhast.PhastModel.ModflowSteadyParameters);
  FOldTransientParameters:= TModflowTransientListParameters.Create(nil);
  FOldTransientParameters.Assign(frmGoPhast.PhastModel.ModflowTransientParameters);
  FOldHufModflowParameters := THufModflowParameters.Create(nil);
  FOldHufModflowParameters.Assign(frmGoPhast.PhastModel.HufParameters);

//  FExistingScreenObjects := TScreenObjectEditCollection.Create;
//  FExistingScreenObjects.OwnScreenObject := False;


end;

function TCustomUndoChangeParameters.Description: string;
begin
  result := 'Change parameters';
end;

destructor TCustomUndoChangeParameters.Destroy;
begin
  FOldHufModflowParameters.Free;
  FNewHufModflowParameters.Free;
  FNewSteadyParameters.Free;
  FOldSteadyParameters.Free;
  FNewTransientParameters.Free;
  FOldTransientParameters.Free;
//  FExistingScreenObjects.Free;
  inherited;
end;

procedure TCustomUndoChangeParameters.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.ModflowSteadyParameters.ClearNewDataSets;
  frmGoPhast.PhastModel.ModflowSteadyParameters := FNewSteadyParameters;
  frmGoPhast.PhastModel.ModflowSteadyParameters.RemoveOldDataSetVariables;
  frmGoPhast.PhastModel.ModflowTransientParameters := FNewTransientParameters;
  frmGoPhast.PhastModel.ModflowSteadyParameters.NewDataSets := nil;
  frmGoPhast.PhastModel.HufParameters := FNewHufModflowParameters;
  UpdatedRequiredDataSets;
end;

procedure TCustomUndoChangeParameters.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.ModflowSteadyParameters  := FOldSteadyParameters;
  frmGoPhast.PhastModel.ModflowSteadyParameters.RemoveNewDataSets;
  frmGoPhast.PhastModel.ModflowSteadyParameters.RemoveOldDataSetVariables;
  frmGoPhast.PhastModel.ModflowTransientParameters := FOldTransientParameters;
  frmGoPhast.PhastModel.ModflowSteadyParameters.NewDataSets := nil;
  frmGoPhast.PhastModel.HufParameters := FOldHufModflowParameters;
  UpdatedRequiredDataSets;
end;

end.
