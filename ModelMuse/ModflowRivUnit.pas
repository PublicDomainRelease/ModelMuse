unit ModflowRivUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, ModflowBoundaryUnit,
  OrderedCollectionUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, RbwParser;

type
  TRivRecord = record
    Cell: TCellLocation;
    Conductance: double;
    RiverStage: double;
    RiverBottom: double;
    StartingTime: double;
    EndingTime: double;
    ConductanceAnnotation: string;
    RiverStageAnnotation: string;
    RiverBottomAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList); 
  end;

  TRivArray = array of TRivRecord;

  TRivStorage = class(TCustomBoundaryStorage)
  private
    FRivArray: TRivArray;
    function GetRivArray: TRivArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property RivArray: TRivArray read GetRivArray;
  end;

  // @name represents a MODFLOW River boundary for one time interval.
  // @name is stored by @link(TRivCollection).
  TRivItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(RiverBottom).
    FRiverBottom: TFormulaObject;
    // See @link(RiverStage).
    FRiverStage: TFormulaObject;
    // See @link(Conductance).
    FConductance: TFormulaObject;
    // See @link(RiverBottom).
    procedure SetRiverBottom(const Value: string);
    // See @link(RiverStage).
    procedure SetRiverStage(const Value: string);
    // See @link(Conductance).
    procedure SetConductance(const Value: string);
    function GetConductance: string;
    function GetRiverBottom: string;
    function GetRiverStage: string;
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
    // @name is the formula used to set the river stage
    // of this boundary.
    property RiverBottom: string read GetRiverBottom write SetRiverBottom;
    // @name is the formula used to set the river stage
    // of this boundary.
    property RiverStage: string read GetRiverStage write SetRiverStage;
    // @name is the formula used to set the conductance
    // or the conductance multiplier of this boundary.
    property Conductance: string read GetConductance write SetConductance;
  end;

  // @name represents MODFLOW River boundaries
  // for a series of time intervals.
  TRivCollection = class(TCustomMF_ListBoundColl)
  private
    // @name is used to compute the River Bottoms for a series of
    // River Boundaries over a series of time intervals.
    FRiverBottomData: TModflowTimeList;
    // @name is used to compute the River Stages for a series of
    // River Boundaries over a series of time intervals.
    FRiverStageData: TModflowTimeList;
    // @name is used to compute the Conductances for a series of
    // River Boundaries over a series of time intervals.
    FConductanceData: TModflowTimeList;
    procedure InvalidateStageData(Sender: TObject);
    procedure InvalidateConductanceData(Sender: TObject);
    procedure InvalidateRiverBottomData(Sender: TObject);
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
    // the @link(TRivStorage.RivArray) at ItemIndex in
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

  // Each @name stores a @link(TRivCollection).
  // @classname is stored by @link(TModflowParameters).
  TRivParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;

  TRiv_Cell = class(TValueCell)
  private
    Values: TRivRecord;
    StressPeriod: integer;
    function GetRiverStage: double;
    function GetConductance: double;
    function GetRiverBottom: double;
    function GetConductanceAnnotation: string;
    function GetRiverBottomAnnotation: string;
    function GetRiverStageAnnotation: string;
  protected
    function GetColumn: integer; override;
    function GetLayer: integer; override;
    function GetRow: integer; override;
    function GetIntegerValue(Index: integer): integer; override;
    function GetRealValue(Index: integer): double; override;
    function GetRealAnnotation(Index: integer): string; override;
    function GetIntegerAnnotation(Index: integer): string; override;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList); override;
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList); override;
    function GetSection: integer; override;
    procedure RecordStrings(Strings: TStringList); override;
  public
    property Conductance: double read GetConductance;
    property RiverBottom: double read GetRiverBottom;
    property RiverStage: double read GetRiverStage;
    property ConductanceAnnotation: string read GetConductanceAnnotation;
    property RiverBottomAnnotation: string read GetRiverBottomAnnotation;
    property RiverStageAnnotation: string read GetRiverStageAnnotation;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
  end;

  // @name represents the MODFLOW River boundaries associated with
  // a single @link(TScreenObject).
  //
  // @link(TSpecificModflowBoundary.FormulaInterpretation) determines whether the @Link(TRivItem.Conductance
  // TRivItem.Conductance) formulas represent
  // @unorderedlist(
  //   @item(fiSpecific - Conductance / the length or area of
  //     intersection between the @link(TScreenObject) and grid cell.)
  //   @item(fiTotal - Conductance.)
  // )
  // @seealso(TRivCollection)
  TRivBoundary = class(TSpecificModflowBoundary)
  private
    procedure TestIfObservationsPresent(var EndOfLastStressPeriod: Double;
      var StartOfFirstStressPeriod: Double;
      var ObservationsPresent: Boolean);
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TRiv_Cell)s for that stress period.
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
    // link  @link(TRivStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW RIV parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TRivStorage) in @link(TCustomMF_BoundColl.Boundaries
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
  StagePosition = 0;
  ConductancePosition = 1;
  BottomPosition = 2;

{ TRivItem }

procedure TRivItem.Assign(Source: TPersistent);
var
  Riv: TRivItem;
begin
  if Source is TRivItem then
  begin
    Riv := TRivItem(Source);
    RiverStage := Riv.RiverStage;
    Conductance := Riv.Conductance;
    RiverBottom := Riv.RiverBottom;
  end;
  inherited;
end;

procedure TRivItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TRivCollection;
  StageObserver: TObserver;
  ConductanceObserver: TObserver;
  BottomObserver: TObserver;
begin
  ParentCollection := Collection as TRivCollection;
  StageObserver := FObserverList[StagePosition];
  StageObserver.OnUpToDateSet := ParentCollection.InvalidateStageData;
  ConductanceObserver := FObserverList[ConductancePosition];
  ConductanceObserver.OnUpToDateSet := ParentCollection.InvalidateConductanceData;
  BottomObserver := FObserverList[BottomPosition];
  BottomObserver.OnUpToDateSet := ParentCollection.InvalidateRiverBottomData;
end;

function TRivItem.BoundaryFormulaCount: integer;
begin
  result := 3;
end;

procedure TRivItem.CreateFormulaObjects;
begin
  FRiverStage := CreateFormulaObject(dso3D);
  FConductance := CreateFormulaObject(dso3D);
  FRiverBottom := CreateFormulaObject(dso3D);
end;

destructor TRivItem.Destroy;
begin
  RiverBottom := '0';
  RiverStage := '0';
  Conductance := '0';
  inherited;
end;

function TRivItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    StagePosition: result := RiverStage;
    ConductancePosition: result := Conductance;
    BottomPosition: result := RiverBottom;
    else Assert(False);
  end;
end;

function TRivItem.GetConductance: string;
begin
  Result := FConductance.Formula;
  ResetItemObserver(ConductancePosition);
end;

procedure TRivItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FRiverStage then
  begin
    List.Add( FObserverList[StagePosition]);
  end;
  if Sender = FConductance then
  begin
    List.Add( FObserverList[ConductancePosition]);
  end;
  if Sender = FRiverBottom then
  begin
    List.Add( FObserverList[BottomPosition]);
  end;
end;

function TRivItem.GetRiverBottom: string;
begin
  Result := FRiverBottom.Formula;
  ResetItemObserver(BottomPosition);
end;

function TRivItem.GetRiverStage: string;
begin
  Result := FRiverStage.Formula;
  ResetItemObserver(StagePosition);
end;

procedure TRivItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState) then
  begin
    PhastModel.InvalidateMfRivConductance(self);
    PhastModel.InvalidateMfRivStage(self);
    PhastModel.InvalidateMfRivBottom(self);
  end;
end;

function TRivItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TRivItem;
begin
  result := (AnotherItem is TRivItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TRivItem(AnotherItem);
    result := (Item.RiverStage = RiverStage)
      and (Item.Conductance = Conductance)
      and (Item.RiverBottom = RiverBottom);
  end;
end;

procedure TRivItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FRiverBottom,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FConductance,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRiverStage,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
end;

procedure TRivItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  inherited;
  case Index of
    StagePosition: RiverStage := Value;
    ConductancePosition: Conductance := Value;
    BottomPosition: RiverBottom := Value;
    else Assert(False);
  end;
end;

procedure TRivItem.SetRiverBottom(const Value: string);
begin
  UpdateFormula(Value, BottomPosition, FRiverBottom);
end;

procedure TRivItem.SetRiverStage(const Value: string);
begin
  UpdateFormula(Value, StagePosition, FRiverStage);
end;

procedure TRivItem.SetConductance(const Value: string);
begin
  UpdateFormula(Value, ConductancePosition, FConductance);
end;

{ TRivCollection }

constructor TRivCollection.Create(Boundary: TModflowBoundary;
  Model, ScreenObject: TObject);
begin
  inherited Create(Boundary, Model, ScreenObject);
  FRiverBottomData := TModflowTimeList.Create(Model, ScreenObject);
  FRiverStageData := TModflowTimeList.Create(Model, ScreenObject);
  FConductanceData := TModflowTimeList.Create(Model, ScreenObject);

  FRiverStageData.NonParamDescription := 'River stage';
  FRiverStageData.ParamDescription := ' river stage';
  FConductanceData.NonParamDescription := 'Conductance';
  FConductanceData.ParamDescription := ' conductance multiplier';
  FRiverBottomData.NonParamDescription := 'River bottom';
  FRiverBottomData.ParamDescription := ' river bottom';

  if Model <> nil then
  begin
    FRiverStageData.OnInvalidate := (Model as TPhastModel).InvalidateMfRivStage;
    FConductanceData.OnInvalidate := (Model as TPhastModel).InvalidateMfRivConductance;
    FRiverBottomData.OnInvalidate := (Model as TPhastModel).InvalidateMfRivBottom;
  end;

  AddTimeList(FRiverStageData);
  AddTimeList(FConductanceData);
  AddTimeList(FRiverBottomData);
end;

destructor TRivCollection.Destroy;
begin
  FRiverStageData.Free;
  FConductanceData.Free;
  FRiverBottomData.Free;
  inherited;
end;

procedure TRivCollection.TestIfObservationsPresent(
  var EndOfLastStressPeriod: Double; var StartOfFirstStressPeriod: Double;
  var ObservationsPresent: Boolean);
var
  Boundary: TRivBoundary;
begin
  // If observations exist, the list of cells must
  // be identical in every stress period.
  // To do that, introduce dummy values in BoundaryValues
  // for times that are not defined explicitly.
  // Set their conductances to zero so they have no effect
  // on the model.
  Boundary := BoundaryGroup as TRivBoundary;
  Boundary.TestIfObservationsPresent(EndOfLastStressPeriod,
    StartOfFirstStressPeriod, ObservationsPresent);
end;

procedure TRivCollection.AddSpecificBoundary;
begin
  AddBoundary(TRivStorage.Create);
end;

function TRivCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Boundary: TRivBoundary;
  ScreenObject: TScreenObject;
  Item: TRivItem;
begin
  Item := Items[ItemIndex] as TRivItem;
  if FormulaIndex = ConductancePosition then
  begin
    Boundary := BoundaryGroup as TRivBoundary;
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

procedure TRivCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList);
var
  RivStorage: TRivStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  Assert(BoundaryFunctionIndex in [StagePosition,ConductancePosition, BottomPosition]);
  Assert(Expression <> nil);

  RivStorage := BoundaryStorage as TRivStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdataRequiredData(DataSets, Variables, ACell);

    Expression.Evaluate;
    with RivStorage.RivArray[Index] do
    begin
      case BoundaryFunctionIndex of
        StagePosition:
          begin
            RiverStage := Expression.DoubleResult;
            RiverStageAnnotation := ACell.Annotation;
          end;
        ConductancePosition:
          begin
            Conductance := Expression.DoubleResult;
            ConductanceAnnotation := ACell.Annotation;
          end;
        BottomPosition:
          begin
            RiverBottom := Expression.DoubleResult;
            RiverBottomAnnotation := ACell.Annotation;
          end;
        else
          Assert(False);
      end;
    end;
  end;
end;

procedure TRivCollection.AssignCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  RivStorage: TRivStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  RivStorage := BoundaryStorage as TRivStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    with RivStorage.RivArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

procedure TRivCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer);
begin
  SetLength((Boundaries[ItemIndex] as TRivStorage).FRivArray, BoundaryCount);
  inherited;
end;

procedure TRivCollection.InvalidateConductanceData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FConductanceData.Invalidate;
  end;
end;

procedure TRivCollection.InvalidateRiverBottomData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FRiverBottomData.Invalidate;
  end;
end;

procedure TRivCollection.InvalidateStageData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FRiverStageData.Invalidate;
  end;
end;

class function TRivCollection.ItemClass: TMF_BoundItemClass;
begin
  result := TRivItem;
end;

{ TRivParamItem }

class function TRivParamItem.BoundaryClass: TMF_BoundCollClass;
begin
  result := TRivCollection;
end;

{ TRiv_Cell }

function TRiv_Cell.GetRealAnnotation(Index: integer): string;
begin
  result := '';
  case Index of
    StagePosition: result := RiverStageAnnotation;
    ConductancePosition: result := ConductanceAnnotation;
    BottomPosition: result := RiverBottomAnnotation;
    else Assert(False);
  end;
end;

function TRiv_Cell.GetRealValue(Index: integer): double;
begin
  result := 0;
  case Index of
    StagePosition: result := RiverStage;
    ConductancePosition: result := Conductance;
    BottomPosition: result := RiverBottom;
    else Assert(False);
  end;
end;

function TRiv_Cell.GetRiverBottom: double;
begin
  result := Values.RiverBottom;
end;

function TRiv_Cell.GetRiverBottomAnnotation: string;
begin
  result := Values.RiverBottomAnnotation;
end;

function TRiv_Cell.GetRiverStage: double;
begin
  result := Values.RiverStage;
end;

function TRiv_Cell.GetRiverStageAnnotation: string;
begin
  result := Values.RiverStageAnnotation;
end;

procedure TRiv_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TRiv_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TRiv_Cell.GetConductance: double;
begin
  result := Values.Conductance;
end;

function TRiv_Cell.GetConductanceAnnotation: string;
begin
  result := Values.ConductanceAnnotation;
end;

function TRiv_Cell.GetIntegerAnnotation(Index: integer): string;
begin
  result := '';
  Assert(False);
end;

function TRiv_Cell.GetIntegerValue(Index: integer): integer;
begin
  result := 0;
  Assert(False);
end;

function TRiv_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TRiv_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TRiv_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TRiv_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  RIV_Cell: TRiv_Cell;
begin
  result := AnotherCell is TRiv_Cell;
  if result then
  begin
    RIV_Cell := TRiv_Cell(AnotherCell);
    result :=
      (Conductance = RIV_Cell.Conductance)
      and (RiverBottom = RIV_Cell.RiverBottom)
      and (RiverStage = RIV_Cell.RiverStage)
      and (IFace = RIV_Cell.IFace);
  end;
end;

procedure TRiv_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TRiv_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

{ TRivBoundary }

procedure TRivBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList);
var
  Cell: TRiv_Cell;
  BoundaryValues: TRivRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TRivStorage;
begin
  LocalBoundaryStorage := BoundaryStorage as TRivStorage;
  for TimeIndex := 0 to
    (PhastModel as TPhastModel).ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TRiv_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := (PhastModel as TPhastModel).ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime <= LocalBoundaryStorage.EndingTime) then
    begin
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.RivArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.RivArray[BoundaryIndex];
        Cell := TRiv_Cell.Create;
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

class function TRivBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TRivCollection;
end;

procedure TRivBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList);
var
  ValueIndex: Integer;
  BoundaryStorage: TRivStorage;
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
          BoundaryStorage := Values.Boundaries[ValueCount] as TRivStorage;
          AssignCells(BoundaryStorage, ValueTimeList);
          Inc(ValueCount);
        end;
      end;
      PriorTime := Item.EndTime;
    end;
    if ValueCount < Values.BoundaryCount then
    begin
      BoundaryStorage := Values.Boundaries[ValueCount] as TRivStorage;
      AssignCells(BoundaryStorage, ValueTimeList);
      Inc(ValueCount);
    end;
    if (ValueIndex = Values.Count - 1) and ObservationsPresent then
    begin
      if Item.EndTime < EndOfLastStressPeriod then
      begin
        if ValueCount < Values.BoundaryCount then
        begin
          BoundaryStorage := Values.Boundaries[ValueCount] as TRivStorage;
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
            BoundaryStorage := Param.Param.Boundaries[ValueCount] as TRivStorage;
            AssignCells(BoundaryStorage, Times);
            Inc(ValueCount);
          end;
        end;
        PriorTime := Item.EndTime;
      end;
      if ValueCount < Param.Param.BoundaryCount then
      begin
        BoundaryStorage := Param.Param.Boundaries[ValueCount] as TRivStorage;
        AssignCells(BoundaryStorage, Times);
        Inc(ValueCount);
      end;
      if {(ValueIndex = Param.Param.Count - 1) and} ObservationsPresent then
      begin
        if Item.EndTime < EndOfLastStressPeriod then
        begin
          if ValueCount < Param.Param.BoundaryCount then
          begin
            BoundaryStorage := Param.Param.Boundaries[ValueCount] as TRivStorage;
            AssignCells(BoundaryStorage, Times);
            Inc(ValueCount);
          end;
        end;
      end;
    end;
  end;
end;

procedure TRivBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (PhastModel <> nil) then
  begin
    Model := PhastModel as TPhastModel;
    Model.InvalidateMfRivConductance(self);
    Model.InvalidateMfRivStage(self);
    Model.InvalidateMfRivBottom(self);
  end;
end;

class function TRivBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := TRivParamItem;
end;

function TRivBoundary.ParameterType: TParameterType;
begin
  result := ptRIV;
end;

procedure TRivBoundary.TestIfObservationsPresent(var EndOfLastStressPeriod,
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
  ObservationsPresent := LocalPhastModel.ModflowPackages.RvobPackage.IsSelected
    and (LocalPhastModel.RiverObservations.Count > 0);
  StartOfFirstStressPeriod := 0;
  EndOfLastStressPeriod := 0;
  if ObservationsPresent then
  begin
    StartOfFirstStressPeriod := LocalPhastModel.ModflowStressPeriods[0].StartTime;
    EndOfLastStressPeriod := LocalPhastModel.ModflowStressPeriods[
      LocalPhastModel.ModflowStressPeriods.Count - 1].EndTime;
  end;
end;

{ TRivRecord }

procedure TRivRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, Conductance);
  WriteCompReal(Comp, RiverStage);
  WriteCompReal(Comp, RiverBottom);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(ConductanceAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(RiverStageAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(RiverBottomAnnotation));
//  WriteCompString(Comp, ConductanceAnnotation);
//  WriteCompString(Comp, RiverStageAnnotation);
//  WriteCompString(Comp, RiverBottomAnnotation);
end;

procedure TRivRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(ConductanceAnnotation);
  Strings.Add(RiverStageAnnotation);
  Strings.Add(RiverBottomAnnotation);
end;

procedure TRivRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  Conductance := ReadCompReal(Decomp);
  RiverStage := ReadCompReal(Decomp);
  RiverBottom := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  ConductanceAnnotation := Annotations[ReadCompInt(Decomp)];
  RiverStageAnnotation := Annotations[ReadCompInt(Decomp)];
  RiverBottomAnnotation := Annotations[ReadCompInt(Decomp)];
//  ConductanceAnnotation := ReadCompString(Decomp, Annotations);
//  RiverStageAnnotation := ReadCompString(Decomp, Annotations);
//  RiverBottomAnnotation := ReadCompString(Decomp, Annotations);
end;

{ TRivStorage }

procedure TRivStorage.Clear;
begin
  SetLength(FRivArray, 0);
  FCleared := True;
end;

procedure TRivStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FRivArray);
    for Index := 0 to Count - 1 do
    begin
      FRivArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FRivArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TRivStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Count: Integer;
  Index: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FRivArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FRivArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TRivStorage.GetRivArray: TRivArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FRivArray;
end;

end.
