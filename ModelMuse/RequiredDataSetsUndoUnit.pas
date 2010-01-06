unit RequiredDataSetsUndoUnit;

interface

uses Classes, GoPhastTypes, UndoItems, PhastModelUnit, frmShowHideObjectsUnit;

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
  ArrayNeeded: TObjectUsedEvent;
  NewFormula, Classification: string;
  Lock: TDataLock;
begin
  { TODO : Find a way to extract common code from
TPhastModel.CreateModflowDataSets and
TCustomCreateRequiredDataSetsUndo.UpdateDataArray}
  DataSetName := Model.DataArrayCreationRecords[Index].Name;
  Orientation := Model.DataArrayCreationRecords[Index].Orientation;
  DataType := Model.DataArrayCreationRecords[Index].DataType;
  ArrayNeeded := Model.DataArrayCreationRecords[Index].DataSetNeeded;
  NewFormula := Model.DataArrayCreationRecords[Index].Formula;
  Classification := Model.DataArrayCreationRecords[Index].Classification;
  Lock := Model.DataArrayCreationRecords[Index].Lock;

  DataArray := Model.GetDataSetByName(DataSetName);
//  DataArray := nil;
  Assert(Assigned(ArrayNeeded));
  if DataArray <> nil then
  begin
//    DataArray := Model.DataSets[DataArrayIndex];
    DataArray.Name := DataSetName;
    DataArray.Lock := Lock;
  end
  else if ArrayNeeded(self) then
  begin
    DataArray := Model.CreateNewDataArray(TDataArray, DataSetName, NewFormula,
      Lock, DataType, Model.DataArrayCreationRecords[Index].EvaluatedAt,
      Orientation, Classification);
    DataArray.OnDataSetUsed := ArrayNeeded;
    DataArray.Lock := Lock;
    DataArray.CheckMax := Model.DataArrayCreationRecords[Index].CheckMax;
    DataArray.CheckMin := Model.DataArrayCreationRecords[Index].CheckMin;
    DataArray.Max := Model.DataArrayCreationRecords[Index].Max;
    DataArray.Min := Model.DataArrayCreationRecords[Index].Min;

    FNewPackageDataSets.Add(DataArray);
  end;
  if DataArray <> nil then
  begin
    DataArray.AssociatedDataSets :=
      Model.DataArrayCreationRecords[Index].AssociatedDataSets;
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

  for Index := 0 to Length(Model.DataArrayCreationRecords) - 1 do
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

end.
