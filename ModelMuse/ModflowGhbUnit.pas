unit ModflowGhbUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, ModflowBoundaryUnit,
  OrderedCollectionUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, RbwParser;

type
  TGhbRecord = record
    Cell: TCellLocation;
    Conductance: double;
    BoundaryHead: double;
    StartingTime: double;
    EndingTime: double;
    ConductanceAnnotation: string;
    BoundaryHeadAnnotation: string;
    procedure Cache(Comp: TCompressionStream);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
  end;

  TGhbArray = array of TGhbRecord;

  TGhbStorage = class(TCustomBoundaryStorage)
  private
    FGhbArray: TGhbArray;
    function GetGhbArray: TGhbArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property GhbArray: TGhbArray read GetGhbArray;
  end;

  // @name represents a MODFLOW General Head boundary for one time interval.
  // @name is stored by @link(TGhbCollection).
  TGhbItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(BoundaryHead).
    FBoundaryHead: TFormulaObject;
    // See @link(Conductance).
    FConductance: TFormulaObject;
    // See @link(BoundaryHead).
    procedure SetBoundaryHead(const Value: string);
    // See @link(Conductance).
    procedure SetConductance(const Value: string);
    function GetBoundaryHead: string;
    function GetConductance: string;
  protected
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
    Destructor Destroy; override;
  published
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    // @name is the formula used to set the boundary head
    // of this boundary.
    property BoundaryHead: string read GetBoundaryHead write SetBoundaryHead;
    // @name is the formula used to set the conductance
    // or the conductance multiplier of this boundary.
    property Conductance: string read GetConductance write SetConductance;
  end;

  // @name represents MODFLOW General Head boundaries
  // for a series of time intervals.
  TGhbCollection = class(TCustomMF_ListBoundColl)
  private
    // @name is used to compute the Boundary Heads for a series of
    // General Head Boundaries over a series of time intervals.
    FBoundaryHeadData: TModflowTimeList;
    // @name is used to compute the Conductances for a series of
    // General Head Boundaries over a series of time intervals.
    FConductanceData: TModflowTimeList;
    procedure InvalidateHeadData(Sender: TObject);
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
    // the @link(TGhbStorage.GhbArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer); override;
    procedure InvalidateModel; override;
  public
    // @name creates an instance of @classname
    constructor Create(Boundary: TModflowBoundary; Model,
      ScreenObject: TObject); override;
    // @name destroys the current instance of @classname.
    // Do not call @name; call Free instead.
    destructor Destroy; override;
  end;

  // Each @name stores a @link(TGhbCollection).
  // @classname is stored by @link(TModflowParameters).
  TGhbParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;

  TGhb_Cell = class(TValueCell)
  private
    Values: TGhbRecord;
    StressPeriod: integer;
    function GetBoundaryHead: double;
    function GetConductance: double;
    function GetBoundaryHeadAnnotation: string;
    function GetConductanceAnnotation: string;
  protected
    function GetColumn: integer; override;
    function GetLayer: integer; override;
    function GetRow: integer; override;
    function GetIntegerValue(Index: integer): integer; override;
    function GetRealValue(Index: integer): double; override;
    function GetRealAnnotation(Index: integer): string; override;
    function GetIntegerAnnotation(Index: integer): string; override;
    procedure Cache(Comp: TCompressionStream); override;
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList); override;
    function GetSection: integer; override;
  public
    property Conductance: double read GetConductance;
    property BoundaryHead: double read GetBoundaryHead;
    property ConductanceAnnotation: string read GetConductanceAnnotation;
    property BoundaryHeadAnnotation: string read GetBoundaryHeadAnnotation;
  end;

  // @name represents the MODFLOW General-Head boundaries associated with
  // a single @link(TScreenObject).
  //
  // FormulaInterpretation determines whether the @Link(TGhbItem.Conductance
  // TGhbItem.Conductance) formulas represent
  // @unorderedlist(
  //   @item(fiSpecific - Conductance / the length or area of
  //     intersection between the @link(TScreenObject) and grid cell.)
  //   @item(fiTotal - Conductance.)
  // )
  // @seealso(TGhbCollection)
  TGhbBoundary = class(TSpecificModflowBoundary)
  private
    procedure TestIfObservationsPresent(var EndOfLastStressPeriod: Double;
      var StartOfFirstStressPeriod: Double;
      var ObservationsPresent: Boolean);
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TGhb_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    // See @link(TModflowParamBoundary.ModflowParamItemClass
    // TModflowParamBoundary.ModflowParamItemClass).
    class function ModflowParamItemClass: TModflowParamItemClass; override;
    function ParameterType: TParameterType; override;
  public
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TGhbStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW GHB parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TGhbStorage) in @link(TCustomMF_BoundColl.Boundaries
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
  HeadPosition = 0;
  ConductancePosition = 1;

{ TGhbItem }

procedure TGhbItem.Assign(Source: TPersistent);
var
  Ghb: TGhbItem;
begin
  if Source is TGhbItem then
  begin
    Ghb := TGhbItem(Source);
    BoundaryHead := Ghb.BoundaryHead;
    Conductance := Ghb.Conductance;
  end;
  inherited;
end;

procedure TGhbItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TGhbCollection;
  HeadObserver: TObserver;
  ConductanceObserver: TObserver;
begin
  ParentCollection := Collection as TGhbCollection;
  HeadObserver := FObserverList[HeadPosition];
  HeadObserver.OnUpToDateSet := ParentCollection.InvalidateHeadData;
  ConductanceObserver := FObserverList[ConductancePosition];
  ConductanceObserver.OnUpToDateSet := ParentCollection.InvalidateConductanceData;
end;

function TGhbItem.BoundaryFormulaCount: integer;
begin
  result := 2;
end;

procedure TGhbItem.CreateFormulaObjects;
begin
  FBoundaryHead := CreateFormulaObject(dso3D);
  FConductance := CreateFormulaObject(dso3D);
end;

destructor TGhbItem.Destroy;
begin
  BoundaryHead := '0';
  Conductance := '0';
  inherited;
end;

function TGhbItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    HeadPosition: result := BoundaryHead;
    ConductancePosition: result := Conductance;
    else Assert(False);
  end;
end;

function TGhbItem.GetBoundaryHead: string;
begin
  Result := FBoundaryHead.Formula;
  ResetItemObserver(HeadPosition);
end;

function TGhbItem.GetConductance: string;
begin
  Result := FConductance.Formula;
  ResetItemObserver(ConductancePosition);
end;

procedure TGhbItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FConductance then
  begin
    List.Add(FObserverList[ConductancePosition]);
  end;
  if Sender = FBoundaryHead then
  begin
    List.Add(FObserverList[HeadPosition]);
  end;
end;

procedure TGhbItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState) then
  begin
    PhastModel.InvalidateMfGhbConductance(self);
    PhastModel.InvalidateMfGhbBoundaryHead(self);
  end;
end;

function TGhbItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TGhbItem;
begin
  result := (AnotherItem is TGhbItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TGhbItem(AnotherItem);
    result := (Item.BoundaryHead = BoundaryHead)
      and (Item.Conductance = Conductance);
  end;
end;

procedure TGhbItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FConductance,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FBoundaryHead,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
end;

procedure TGhbItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  inherited;
  case Index of
    HeadPosition: BoundaryHead := Value;
    ConductancePosition: Conductance := Value;
    else Assert(False);
  end;
end;

procedure TGhbItem.SetBoundaryHead(const Value: string);
begin
  UpdateFormula(Value, HeadPosition, FBoundaryHead);
end;

procedure TGhbItem.SetConductance(const Value: string);
begin
  UpdateFormula(Value, ConductancePosition, FConductance);
end;

{ TGhbCollection }

constructor TGhbCollection.Create(Boundary: TModflowBoundary;
  Model, ScreenObject: TObject);
begin
  inherited Create(Boundary, Model, ScreenObject);
  FBoundaryHeadData := TModflowTimeList.Create(Model);
  FConductanceData := TModflowTimeList.Create(Model);
  FBoundaryHeadData.NonParamDescription := 'Boundary head';
  FBoundaryHeadData.ParamDescription := ' boundary head';
  FConductanceData.NonParamDescription := 'Conductance';
  FConductanceData.ParamDescription := ' conductance multiplier';

  if Model <> nil then
  begin
    FConductanceData.OnInvalidate :=
      (Model as TPhastModel).InvalidateMfGhbConductance;

    FBoundaryHeadData.OnInvalidate :=
      (Model as TPhastModel).InvalidateMfGhbBoundaryHead;
  end;

  AddTimeList(FBoundaryHeadData);
  AddTimeList(FConductanceData);
end;

destructor TGhbCollection.Destroy;
begin
  FBoundaryHeadData.Free;
  FConductanceData.Free;
  inherited;
end;

procedure TGhbCollection.TestIfObservationsPresent(
  var EndOfLastStressPeriod: Double; var StartOfFirstStressPeriod: Double;
  var ObservationsPresent: Boolean);
var
  Boundary: TGhbBoundary;
begin
  // If observations exist, the list of cells must
  // be identical in every stress period.
  // To do that, introduce dummy values in BoundaryValues
  // for times that are not defined explicitly.
  // Set their conductances to zero so they have no effect
  // on the model.
  Boundary := BoundaryGroup as TGhbBoundary;
  Boundary.TestIfObservationsPresent(EndOfLastStressPeriod,
    StartOfFirstStressPeriod, ObservationsPresent);
end;

procedure TGhbCollection.AddSpecificBoundary;
begin
  AddBoundary(TGhbStorage.Create);
end;

function TGhbCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Boundary: TGhbBoundary;
  ScreenObject: TScreenObject;
  Item: TGhbItem;
begin
  Item := Items[ItemIndex] as TGhbItem;
  if FormulaIndex = ConductancePosition then
  begin
    Boundary := BoundaryGroup as TGhbBoundary;
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

procedure TGhbCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList);
var
  GhbStorage: TGhbStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  Assert(BoundaryFunctionIndex in [HeadPosition,ConductancePosition]);
  Assert(Expression <> nil);

  GhbStorage := BoundaryStorage as TGhbStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdataRequiredData(DataSets, Variables, ACell);

    Expression.Evaluate;
    with GhbStorage.GhbArray[Index] do
    begin
      case BoundaryFunctionIndex of
        HeadPosition:
          begin
            BoundaryHead := Expression.DoubleResult;
            BoundaryHeadAnnotation := ACell.Annotation;
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

procedure TGhbCollection.AssignCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  GhbStorage: TGhbStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  GhbStorage := BoundaryStorage as TGhbStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    with GhbStorage.GhbArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

procedure TGhbCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer);
begin
  SetLength((Boundaries[ItemIndex] as TGhbStorage).FGhbArray, BoundaryCount);
  inherited;
end;

procedure TGhbCollection.InvalidateConductanceData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FConductanceData.Invalidate;
  end;
end;

procedure TGhbCollection.InvalidateHeadData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FBoundaryHeadData.Invalidate;
  end;
end;

procedure TGhbCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState) then
  begin
    PhastModel.InvalidateMfGhbConductance(self);
    PhastModel.InvalidateMfGhbBoundaryHead(self);
  end;
end;

class function TGhbCollection.ItemClass: TMF_BoundItemClass;
begin
  result := TGhbItem;
end;

{ TGhbParamItem }

class function TGhbParamItem.BoundaryClass: TMF_BoundCollClass;
begin
  result := TGhbCollection;
end;

{ TGhb_Cell }

procedure TGhb_Cell.Cache(Comp: TCompressionStream);
begin
  inherited;
  Values.Cache(Comp);
  WriteCompInt(Comp, StressPeriod);
end;

function TGhb_Cell.GetBoundaryHead: double;
begin
  result := Values.BoundaryHead;
end;

function TGhb_Cell.GetBoundaryHeadAnnotation: string;
begin
  result := Values.BoundaryHeadAnnotation;
end;

function TGhb_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TGhb_Cell.GetConductance: double;
begin
  result := Values.Conductance;
end;

function TGhb_Cell.GetConductanceAnnotation: string;
begin
  result := Values.ConductanceAnnotation;
end;

function TGhb_Cell.GetIntegerAnnotation(Index: integer): string;
begin
  result := '';
  Assert(False);
end;

function TGhb_Cell.GetIntegerValue(Index: integer): integer;
begin
  result := 0;
  Assert(False);
end;

function TGhb_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TGhb_Cell.GetRealAnnotation(Index: integer): string;
begin
  case Index of
    HeadPosition:
      begin
        result := BoundaryHeadAnnotation;
      end;
    ConductancePosition:
      begin
        result := ConductanceAnnotation;
      end;
    else Assert(False);
  end;
end;

function TGhb_Cell.GetRealValue(Index: integer): double;
begin
  result := 0;
  case Index of
    HeadPosition:
      begin
        result := BoundaryHead;
      end;
    ConductancePosition:
      begin
        result := Conductance;
      end;
    else Assert(False);
  end;
end;

function TGhb_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TGhb_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

procedure TGhb_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

{ TGhbBoundary }

procedure TGhbBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList);
var
  Cell: TGhb_Cell;
  BoundaryValues: TGhbRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TGhbStorage;
begin
  LocalBoundaryStorage := BoundaryStorage as TGhbStorage;
  for TimeIndex := 0 to
    (PhastModel as TPhastModel).ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TGhb_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := (PhastModel as TPhastModel).ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime <= LocalBoundaryStorage.EndingTime) then
    begin
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.GhbArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.GhbArray[BoundaryIndex];
        Cell := TGhb_Cell.Create;
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

class function TGhbBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TGhbCollection;
end;

procedure TGhbBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList);
var
  ValueIndex: Integer;
  BoundaryStorage: TGhbStorage;
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
        if ValueCount < Values.BoundaryCount then
        begin
          BoundaryStorage := Values.Boundaries[ValueCount] as TGhbStorage;
          AssignCells(BoundaryStorage, ValueTimeList);
          Inc(ValueCount);
        end;
      end;
      PriorTime := Item.EndTime;
    end;
    if ValueCount < Values.BoundaryCount then
    begin
      BoundaryStorage := Values.Boundaries[ValueCount] as TGhbStorage;
      AssignCells(BoundaryStorage, ValueTimeList);
      Inc(ValueCount);
    end;
    if (ValueIndex = Values.Count - 1) and ObservationsPresent then
    begin
      if Item.EndTime < EndOfLastStressPeriod then
      begin
        if ValueCount < Values.BoundaryCount then
        begin
          BoundaryStorage := Values.Boundaries[ValueCount] as TGhbStorage;
          AssignCells(BoundaryStorage, ValueTimeList);
          Inc(ValueCount);
        end;
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
          if ValueCount < Param.Param.BoundaryCount then
          begin
            BoundaryStorage := Param.Param.Boundaries[ValueCount] as TGhbStorage;
            AssignCells(BoundaryStorage, Times);
            Inc(ValueCount);
          end;
        end;
        PriorTime := Item.EndTime;
      end;
      if ValueCount < Param.Param.BoundaryCount then
      begin
        BoundaryStorage := Param.Param.Boundaries[ValueCount] as TGhbStorage;
        AssignCells(BoundaryStorage, Times);
        Inc(ValueCount);
      end;
      if {(ValueIndex = Param.Param.Count - 1) and} ObservationsPresent then
      begin
        if Item.EndTime < EndOfLastStressPeriod then
        begin
          if ValueCount < Param.Param.BoundaryCount then
          begin
            BoundaryStorage := Param.Param.Boundaries[ValueCount] as TGhbStorage;
            AssignCells(BoundaryStorage, Times);
            Inc(ValueCount);
          end;
        end;
      end;
    end;
  end;
end;

procedure TGhbBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (PhastModel <> nil) then
  begin
    Model := PhastModel as TPhastModel;
    Model.InvalidateMfGhbConductance(self);
    Model.InvalidateMfGhbBoundaryHead(self);
  end;
end;

class function TGhbBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := TGhbParamItem;
end;

function TGhbBoundary.ParameterType: TParameterType;
begin
  result := ptGHB;
end;

procedure TGhbBoundary.TestIfObservationsPresent(var EndOfLastStressPeriod,
  StartOfFirstStressPeriod: Double; var ObservationsPresent: Boolean);
var
  LocalPhastModel: TPhastModel;
  LocalScreenObject: TScreenObject;
begin
  // If observations exist, the list of cells must
  // be identical in every stress period.
  // To do that, introduce dummy values in BoundaryValues
  // for times that are not defined explicitly.
  // Set their conductances to zero so they have no effect
  // on the model.
  LocalScreenObject := ScreenObject as TScreenObject;
  LocalPhastModel := LocalScreenObject.Model as TPhastModel;
  Assert(LocalPhastModel <> nil);
  ObservationsPresent := LocalPhastModel.ModflowPackages.GbobPackage.IsSelected
    and (LocalPhastModel.GhbObservations.Count > 0);
  StartOfFirstStressPeriod := 0;
  EndOfLastStressPeriod := 0;
  if ObservationsPresent then
  begin
    StartOfFirstStressPeriod := LocalPhastModel.ModflowStressPeriods[0].StartTime;
    EndOfLastStressPeriod := LocalPhastModel.ModflowStressPeriods[
      LocalPhastModel.ModflowStressPeriods.Count - 1].EndTime;
  end;
end;

{ TGhbRecord }

procedure TGhbRecord.Cache(Comp: TCompressionStream);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, Conductance);
  WriteCompReal(Comp, BoundaryHead);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompString(Comp, ConductanceAnnotation);
  WriteCompString(Comp, BoundaryHeadAnnotation);
end;

procedure TGhbRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  Conductance := ReadCompReal(Decomp);
  BoundaryHead := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  ConductanceAnnotation := ReadCompString(Decomp, Annotations);
  BoundaryHeadAnnotation := ReadCompString(Decomp, Annotations);
end;

{ TGhbStorage }


procedure TGhbStorage.Clear;
begin
  SetLength(FGhbArray, 0);
  FCleared := True;
end;

procedure TGhbStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
begin
  Count := Length(FGhbArray);
  Compressor.Write(Count, SizeOf(Count));
  for Index := 0 to Count - 1 do
  begin
    FGhbArray[Index].Cache(Compressor);
  end;
end;

procedure TGhbStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Count: Integer;
  Index: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FGhbArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FGhbArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TGhbStorage.GetGhbArray: TGhbArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FGhbArray;
end;

end.
