unit ModflowDrnUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, ModflowBoundaryUnit,
  OrderedCollectionUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, RbwParser;

type
  TDrnRecord = record
    Cell: TCellLocation;
    Conductance: double;
    Elevation: double;
    StartingTime: double;
    EndingTime: double;
    ConductanceAnnotation: string;
    ElevationAnnotation: string;
    procedure Cache(Comp: TCompressionStream);
    procedure Restore(Decomp: TDecompressionStream);
  end;

  TDrnArray = array of TDrnRecord;

  TDrnStorage = class(TCustomBoundaryStorage)
  private
    FDrnArray: TDrnArray;
    function GetDrnArray: TDrnArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property DrnArray: TDrnArray read GetDrnArray;
  end;

  // @name represents a MODFLOW Drain boundary for one time interval.
  // @name is stored by @link(TDrnCollection).
  TDrnItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(Elevation).
    procedure SetElevation(const Value: string);
    // See @link(Conductance).
    procedure SetConductance(const Value: string);
    function GetConductance: string;
    function GetElevation: string;
  protected
    // See @link(Elevation).
    FElevation: TFormulaObject;
    // See @link(Conductance).
    FConductance: TFormulaObject;
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    // See @link(TCustomModflowBoundaryItem.BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(TCustomModflowBoundaryItem.BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    // @name checks whether AnotherItem is the same as the current @classname.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure InvalidateModel; override;
    function BoundaryFormulaCount: integer; override;
  public
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent); override;
    Destructor Destroy; override;
  published
    // @name is the formula used to set the elevation
    // of this boundary.
    property Elevation: string read GetElevation write SetElevation;
    // @name is the formula used to set the conductance
    // or the conductance multiplier of this boundary.
    property Conductance: string read GetConductance write SetConductance;
  end;

  // @name represents MODFLOW Drain boundaries
  // for a series of time intervals.
  TDrnCollection = class(TCustomMF_ListBoundColl)
  private
    // @name is used to perform notifications of the Elevations for a series of
    // Drain Boundaries over a series of time intervals.
    FElevationData: TModflowTimeList;
    // @name is used to perform notifications of the Conductances
    // for a series of
    // Drain Boundaries over a series of time intervals.
    FConductanceData: TModflowTimeList;
    procedure InvalidateElevationData(Sender: TObject);
    procedure InvalidateConductanceData(Sender: TObject);
  protected
    procedure AssignCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList); override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
      override;
    procedure AddSpecificBoundary; override;
    procedure TestIfObservationsPresent(var EndOfLastStressPeriod: Double;
      var StartOfFirstStressPeriod: Double;
      var ObservationsPresent: Boolean); override;

    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TMF_BoundItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TDrnStorage.DrnArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer); override;
  public
    // @name creates an instance of @classname
    constructor Create(Boundary: TModflowBoundary; Model,
      ScreenObject: TObject); override;
    // @name destroys the current instance of @classname.
    // Do not call @name; call Free instead.
    destructor Destroy; override;
  end;

  // Each @name stores a @link(TDrnCollection).
  // @classname is stored by @link(TModflowParameters).
  TDrnParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;

  TDrn_Cell = class(TValueCell)
  private
    Values: TDrnRecord;
    StressPeriod: integer;
    function GetElevation: double;
    function GetConductance: double;
    function GetConductanceAnnotation: string;
    function GetElevationAnnotation: string;
  protected
    function GetColumn: integer; override;
    function GetLayer: integer; override;
    function GetRow: integer; override;
    function GetIntegerValue(Index: integer): integer; override;
    function GetRealValue(Index: integer): double; override;
    function GetRealAnnotation(Index: integer): string; override;
    function GetIntegerAnnotation(Index: integer): string; override;
    procedure Cache(Comp: TCompressionStream); override;
    procedure Restore(Decomp: TDecompressionStream); override;
    function GetSection: integer; override;
  public
    property Conductance: double read GetConductance;
    property Elevation: double read GetElevation;
    property ConductanceAnnotation: string read GetConductanceAnnotation;
    property ElevationAnnotation: string read GetElevationAnnotation;
  end;

  // @name represents the MODFLOW Drain boundaries associated with
  // a single @link(TScreenObject).
  //
  // FormulaInterpretation determines whether the @Link(TDrnItem.Conductance
  // TDrnItem.Conductance) formulas represent
  // @unorderedlist(
  //   @item(fiSpecific - Conductance / the length or area of
  //     intersection between the @link(TScreenObject) and grid cell.)
  //   @item(fiTotal - Conductance.)
  // )
  // @seealso(TDrnCollection)
  TDrnBoundary = class(TSpecificModflowBoundary)
  private
    procedure TestIfObservationsPresent(var EndOfLastStressPeriod: Double;
      var StartOfFirstStressPeriod: Double;
      var ObservationsPresent: Boolean); 
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TDrn_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    // See @link(TModflowParamBoundary.ModflowParamItemClass
    // TModflowParamBoundary.ModflowParamItemClass).
    class function ModflowParamItemClass: TModflowParamItemClass; override;
  public
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TDrnStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW DRN parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TDrnStorage) in @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList);
      override;
    procedure InvalidateDisplay; override;
  end;

implementation

uses PhastModelUnit, ScreenObjectUnit, ModflowTimeUnit, TempFiles,
  GoPhastTypes, frmGoPhastUnit;

const
  ElevationPosition = 0;
  ConductancePosition = 1;
{ TDrnItem }

procedure TDrnItem.Assign(Source: TPersistent);
var
  Drn: TDrnItem;
begin
  if Source is TDrnItem then
  begin
    Drn := TDrnItem(Source);
    Elevation := Drn.Elevation;
    Conductance := Drn.Conductance;
  end;
  inherited;
end;

procedure TDrnItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FConductance,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FElevation,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);
end;

procedure TDrnItem.CreateFormulaObjects;
begin
  FElevation := CreateFormulaObject(dso3D);
  FConductance := CreateFormulaObject(dso3D);
end;

destructor TDrnItem.Destroy;
begin
  Elevation := '0';
  Conductance := '0';
  inherited;
end;

procedure TDrnItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TDrnCollection;
  ElevationObserver: TObserver;
  ConductanceObserver: TObserver;
begin
  ParentCollection := Collection as TDrnCollection;
  ElevationObserver := FObserverList[ElevationPosition];
  ElevationObserver.OnUpToDateSet := ParentCollection.InvalidateElevationData;
  ConductanceObserver := FObserverList[ConductancePosition];
  ConductanceObserver.OnUpToDateSet := ParentCollection.InvalidateConductanceData;
end;

procedure TDrnItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FConductance then
  begin
    List.Add(FObserverList[ConductancePosition]);
  end;
  if Sender = FElevation then
  begin
    List.Add(FObserverList[ElevationPosition]);
  end;
end;

function TDrnItem.BoundaryFormulaCount: integer;
begin
  result := 2;
end;

function TDrnItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    ElevationPosition: result := Elevation;
    ConductancePosition: result := Conductance;
    else Assert(False);
  end;
end;

function TDrnItem.GetConductance: string;
begin
  Result := FConductance.Formula;
  ResetItemObserver(ConductancePosition);
end;

function TDrnItem.GetElevation: string;
begin
  Result := FElevation.Formula;
  ResetItemObserver(ElevationPosition);
end;

procedure TDrnItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState) then
  begin
    PhastModel.InvalidateMfDrnConductance(self);
    PhastModel.InvalidateMfDrnElevation(self);
  end;
end;

function TDrnItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TDrnItem;
begin
  result := (AnotherItem is TDrnItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TDrnItem(AnotherItem);
    result := (Item.Elevation = Elevation)
      and (Item.Conductance = Conductance);
  end;
end;

procedure TDrnItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    ElevationPosition: Elevation := Value;
    ConductancePosition: Conductance := Value;
    else Assert(False);
  end;
end;

procedure TDrnItem.SetElevation(const Value: string);
begin
  UpdateFormula(Value, ElevationPosition, FElevation);
end;

procedure TDrnItem.SetConductance(const Value: string);
begin
  UpdateFormula(Value, ConductancePosition, FConductance);
end;

{ TDrnCollection }

constructor TDrnCollection.Create(Boundary: TModflowBoundary;
  Model, ScreenObject: TObject);
begin
  inherited Create(Boundary, Model, ScreenObject);
  FElevationData := TModflowTimeList.Create(Model);
  FConductanceData := TModflowTimeList.Create(Model);

  FElevationData.NonParamDescription := 'Elevation';
  FElevationData.ParamDescription := ' elevation';
  FConductanceData.NonParamDescription := 'Conductance';
  FConductanceData.ParamDescription := ' conductance multiplier';
  if Model <> nil then
  begin
    FElevationData.OnInvalidate := (Model as TPhastModel).InvalidateMfDrnElevation;
    FConductanceData.OnInvalidate := (Model as TPhastModel).InvalidateMfDrnConductance;
  end;

  AddTimeList(FElevationData);
  AddTimeList(FConductanceData);
end;

destructor TDrnCollection.Destroy;
begin
  FElevationData.Free;
  FConductanceData.Free;
  inherited;
end;

procedure TDrnCollection.TestIfObservationsPresent(
  var EndOfLastStressPeriod: Double; var StartOfFirstStressPeriod: Double;
  var ObservationsPresent: Boolean);
var
  Boundary: TDrnBoundary;
begin
  // If observations exist, the list of cells must
  // be identical in every stress period.
  // To do that, introduce dummy values in BoundaryValues
  // for times that are not defined explicitly.
  // Set their conductances to zero so they have no effect
  // on the model.
  Boundary := BoundaryGroup as TDrnBoundary;
  Boundary.TestIfObservationsPresent(EndOfLastStressPeriod,
    StartOfFirstStressPeriod, ObservationsPresent);
end;

procedure TDrnCollection.AddSpecificBoundary;
begin
  AddBoundary(TDrnStorage.Create);
end;

function TDrnCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Boundary: TDrnBoundary;
  ScreenObject: TScreenObject;
  Item: TDrnItem;
begin
  Item := Items[ItemIndex] as TDrnItem;
  if FormulaIndex = ConductancePosition then
  begin
    Boundary := BoundaryGroup as TDrnBoundary;
    ScreenObject := Boundary.ScreenObject as TScreenObject;
    case Boundary.FormulaInterpretation of
      fiSpecific:
        begin
          if ScreenObject.ScreenObjectLength = 0 then
          begin
            result := Item.Conductance;
          end
          else if ScreenObject.Closed then
          begin
            result := '(' + Item.Conductance
              + ') * ' + StrObjectIntersectArea;
          end
          else
          begin
            result := '(' + Item.Conductance
              + ') * ' + StrObjectSectionIntersectLength;
          end;
        end;
      fiDirect:
        begin
          result := Item.Conductance;
        end;
      fiTotal:
        begin
          if ScreenObject.ScreenObjectLength = 0 then
          begin
            result := Item.Conductance;
          end
          else if ScreenObject.Closed then
          begin
            result := '((' + Item.Conductance
              + ') * ' + StrObjectIntersectArea + ') / ' + StrObjectArea;
          end
          else
          begin
            result := '((' + Item.Conductance
              + ') * ' + StrObjectSectionIntersectLength+ ') / ' + StrObjectLength;
          end;
        end;
      else Assert(False);
    end;
  end
  else
  begin
    result := Item.BoundaryFormula[FormulaIndex];
  end;
end;

procedure TDrnCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList);
var
  DrnStorage: TDrnStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  Assert(BoundaryFunctionIndex in [ElevationPosition, ConductancePosition]);
  Assert(Expression <> nil);

  DrnStorage := BoundaryStorage as TDrnStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdataRequiredData(DataSets, Variables, ACell);
    // 2. update locations
    Expression.Evaluate;
    with DrnStorage.DrnArray[Index] do
    begin
      case BoundaryFunctionIndex of
        ElevationPosition:
          begin
            Elevation := Expression.DoubleResult;
            ElevationAnnotation := ACell.Annotation;
          end;
        ConductancePosition:
          begin
            Conductance := Expression.DoubleResult;
            ConductanceAnnotation := ACell.Annotation;
          end;
        else
          Assert(False);
      end;
    end;
  end;
end;

procedure TDrnCollection.AssignCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  DrnStorage: TDrnStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  DrnStorage := BoundaryStorage as TDrnStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    with DrnStorage.DrnArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;
procedure TDrnCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer);
begin
  SetLength((Boundaries[ItemIndex] as TDrnStorage).FDrnArray, BoundaryCount);
  inherited;
end;

procedure TDrnCollection.InvalidateConductanceData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FConductanceData.Invalidate;
  end;
end;

procedure TDrnCollection.InvalidateElevationData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FElevationData.Invalidate;
  end;
end;

class function TDrnCollection.ItemClass: TMF_BoundItemClass;
begin
  result := TDrnItem;
end;

{ TDrnParamItem }

class function TDrnParamItem.BoundaryClass: TMF_BoundCollClass;
begin
  result := TDrnCollection;
end;

{ TDrn_Cell }

function TDrn_Cell.GetElevation: double;
begin
  result := Values.Elevation;
end;

function TDrn_Cell.GetElevationAnnotation: string;
begin
  result := Values.ElevationAnnotation;
end;

function TDrn_Cell.GetIntegerAnnotation(Index: integer): string;
begin
  result := '';
  Assert(False);
end;

function TDrn_Cell.GetIntegerValue(Index: integer): integer;
begin
  result := 0;
  Assert(False);
end;

procedure TDrn_Cell.Cache(Comp: TCompressionStream);
begin
  inherited;
  Values.Cache(Comp);
  WriteCompInt(Comp, StressPeriod);
end;

function TDrn_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TDrn_Cell.GetConductance: double;
begin
  result := Values.Conductance;
end;

function TDrn_Cell.GetConductanceAnnotation: string;
begin
  result := Values.ConductanceAnnotation;
end;

function TDrn_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TDrn_Cell.GetRealAnnotation(Index: integer): string;
begin
  result := '';
  case Index of
    ElevationPosition: result := ElevationAnnotation;
    ConductancePosition: result := ConductanceAnnotation;
    else Assert(False);
  end;
end;

function TDrn_Cell.GetRealValue(Index: integer): double;
begin
  result := 0;
  case Index of
    ElevationPosition: result := Elevation;
    ConductancePosition: result := Conductance;
    else Assert(False);
  end;
end;

function TDrn_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TDrn_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

procedure TDrn_Cell.Restore(Decomp: TDecompressionStream);
begin
  inherited;
  Values.Restore(Decomp);
  StressPeriod := ReadCompInt(Decomp);
end;

{ TDrnBoundary }
procedure TDrnBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList);
var
  Cell: TDrn_Cell;
  BoundaryValues: TDrnRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TDrnStorage;
begin
  LocalBoundaryStorage := BoundaryStorage as TDrnStorage;
  for TimeIndex := 0 to
    (PhastModel as TPhastModel).ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TDrn_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := (PhastModel as TPhastModel).ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime <= LocalBoundaryStorage.EndingTime) then
    begin
      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.DrnArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.DrnArray[BoundaryIndex];
        Cell := TDrn_Cell.Create;
        Assert(ScreenObject <> nil);
        Cell.IFace := (ScreenObject as TScreenObject).IFace;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
        Cell.ScreenObject := ScreenObject;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TDrnBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TDrnCollection;
end;

procedure TDrnBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList);
var
  ValueIndex: Integer;
  BoundaryStorage: TDrnStorage;
  ParamIndex: Integer;
  Param: TModflowParamItem;
  Times: TList;
  Position: integer;
  ParamName: string;
  EndOfLastStressPeriod: Double;
  StartOfFirstStressPeriod: Double;
  ObservationsPresent: Boolean;
  PriorTime: Double;
  ValueCount: Integer;
  Item: TCustomModflowBoundaryItem;
begin
  EvaluateListBoundaries;
  TestIfObservationsPresent(EndOfLastStressPeriod, StartOfFirstStressPeriod,
    ObservationsPresent);
  PriorTime := StartOfFirstStressPeriod;
  ValueCount := 0;
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    Item := Values[ValueIndex];
    if ObservationsPresent then
    begin
      if PriorTime < Item.StartTime then
      begin
        BoundaryStorage := Values.Boundaries[ValueCount] as TDrnStorage;
        AssignCells(BoundaryStorage, ValueTimeList);
        Inc(ValueCount);
      end;
      PriorTime := Item.EndTime;
    end;
    BoundaryStorage := Values.Boundaries[ValueCount] as TDrnStorage;
    AssignCells(BoundaryStorage, ValueTimeList);
    Inc(ValueCount);
    if (ValueIndex = Values.Count - 1) and ObservationsPresent then
    begin
      if Item.EndTime < EndOfLastStressPeriod then
      begin
        BoundaryStorage := Values.Boundaries[ValueCount] as TDrnStorage;
        AssignCells(BoundaryStorage, ValueTimeList);
        Inc(ValueCount);
      end;
    end;
  end;
  for ParamIndex := 0 to Parameters.Count - 1 do
  begin
    Param := Parameters[ParamIndex];
    ParamName := Param.Param.ParamName;
    Position := ParamList.IndexOf(ParamName);
    if Position < 0 then
    begin
      Times := TObjectList.Create;
      ParamList.AddObject(ParamName, Times);
    end
    else
    begin
      Times := ParamList.Objects[Position] as TList;
    end;
    PriorTime := StartOfFirstStressPeriod;
    ValueCount := 0;
    for ValueIndex := 0 to Param.Param.Count - 1 do
    begin
      Item := Param.Param[ValueIndex];
      if ObservationsPresent then
      begin
        if PriorTime < Item.StartTime then
        begin
          BoundaryStorage := Param.Param.Boundaries[ValueCount] as TDrnStorage;
          AssignCells(BoundaryStorage, Times);
          Inc(ValueCount);
        end;
        PriorTime := Item.EndTime;
      end;
      BoundaryStorage := Param.Param.Boundaries[ValueCount] as TDrnStorage;
      AssignCells(BoundaryStorage, Times);
      Inc(ValueCount);
      if (ValueIndex = Param.Param.Count - 1) and ObservationsPresent then
      begin
        if Item.EndTime < EndOfLastStressPeriod then
        begin
          BoundaryStorage := Param.Param.Boundaries[ValueCount] as TDrnStorage;
          AssignCells(BoundaryStorage, Times);
          Inc(ValueCount);
        end;
      end;
    end;
  end;
end;

procedure TDrnBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (PhastModel <> nil) then
  begin
    Model := PhastModel as TPhastModel;
    Model.InvalidateMfDrnConductance(self);
    Model.InvalidateMfDrnElevation(self);
  end;
end;

class function TDrnBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := TDrnParamItem;
end;

procedure TDrnBoundary.TestIfObservationsPresent(var EndOfLastStressPeriod,
  StartOfFirstStressPeriod: Double; var ObservationsPresent: Boolean);
var
  LocalScreenObject: TScreenObject;
  LocalPhastModel: TPhastModel;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  LocalPhastModel := LocalScreenObject.Model as TPhastModel;
  Assert(LocalPhastModel <> nil);
  ObservationsPresent := LocalPhastModel.ModflowPackages.DrobPackage.IsSelected
    and (LocalPhastModel.DrainObservations.Count > 0);
  StartOfFirstStressPeriod := 0;
  EndOfLastStressPeriod := 0;
  if ObservationsPresent then
  begin
    StartOfFirstStressPeriod := LocalPhastModel.ModflowStressPeriods[0].StartTime;
    EndOfLastStressPeriod := LocalPhastModel.ModflowStressPeriods[
      LocalPhastModel.ModflowStressPeriods.Count - 1].EndTime;
  end;
end;

{ TDrnRecord }

procedure TDrnRecord.Cache(Comp: TCompressionStream);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, Conductance);
  WriteCompReal(Comp, Elevation);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompString(Comp, ConductanceAnnotation);
  WriteCompString(Comp, ElevationAnnotation);
end;

procedure TDrnRecord.Restore(Decomp: TDecompressionStream);
begin
  Cell := ReadCompCell(Decomp);
  Conductance := ReadCompReal(Decomp);
  Elevation := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  ConductanceAnnotation := ReadCompString(Decomp);
  ElevationAnnotation := ReadCompString(Decomp);
end;

{ TDrnStorage }

procedure TDrnStorage.Clear;
begin
  SetLength(FDrnArray, 0);
  FCleared := True;
end;

procedure TDrnStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
begin
  Count := Length(FDrnArray);
  Compressor.Write(Count, SizeOf(Count));
  for Index := 0 to Count - 1 do
  begin
    FDrnArray[Index].Cache(Compressor);
  end;
end;

procedure TDrnStorage.Restore(DecompressionStream: TDecompressionStream);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FDrnArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FDrnArray[Index].Restore(DecompressionStream);
  end;
end;

function TDrnStorage.GetDrnArray: TDrnArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FDrnArray;
end;

end.
