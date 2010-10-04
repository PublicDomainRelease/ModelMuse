unit ModflowWellUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, RbwParser;

type
  {
    @longcode(
  TWellRecord = record
    Cell: TCellLocation;
    PumpingRate: double;
    StartingTime: double;
    EndingTime: double;
    PumpingAnnotation: string;
  end;
    )
    @name stores, the location, time and pumping rate for a well boundary.
  }
  TWellRecord = record
    Cell: TCellLocation;
    PumpingRate: double;
    StartingTime: double;
    EndingTime: double;
    PumpingRateAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList); 
  end;

  // @name is an array of @link(TWellRecord)s.
  TWellArray = array of TWellRecord;

  // @name extends @link(TCustomBoundaryStorage) by adding the locations
  // and values of series of wells.
  TWellStorage = class(TCustomBoundaryStorage)
  private
    FWellArray: TWellArray;
    function GetWellArray: TWellArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property WellArray: TWellArray read GetWellArray;
  end;

  // @name represents a MODFLOW well for one time interval.
  // @name is stored by @link(TWellCollection).
  TWellItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(PumpingRate).
    FPumpingRate: TFormulaObject;
    // See @link(PumpingRate).
    procedure SetPumpingRate(const Value: string);
    function GetPumpingRate: string;
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
    // @name is the formula used to set the pumping rate
    // or the pumping rate multiplier of this boundary.
    property PumpingRate: string read GetPumpingRate write SetPumpingRate;
  end;

  // @name represents MODFLOW Well boundaries
  // for a series of time intervals.
  TWellCollection = class(TCustomMF_ListBoundColl)
  private
    // @name is used to compute the pumping rates for a series of
    // Well over a series of time intervals.
    FPumpingRateData: TModflowTimeList;
    procedure InvalidatePumpingRateData(Sender: TObject);
  protected
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string; override;
    procedure AddSpecificBoundary; override;

    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TMF_BoundItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TWellStorage.WellArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer); override;
    procedure InvalidateModel; override;
    procedure AssignCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList); override;
  public
    // @name creates an instance of @classname
    constructor Create(Boundary: TModflowBoundary; Model,
      ScreenObject: TObject); override;
    // @name destroys the current instance of @classname.
    // Do not call @name; call Free instead.
    destructor Destroy; override;
  end;

  // Each @name stores a @link(TWellCollection).
  // @classname is stored by @link(TModflowParameters).
  TWellParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;

  TWell_Cell = class(TValueCell)
  private
    Values: TWellRecord;
    StressPeriod: integer;
    function GetPumpingRate: double;
    function GetPumpingRateAnnotation: string;
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
    property PumpingRate: double read GetPumpingRate;
    property PumpingRateAnnotation: string read GetPumpingRateAnnotation;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
  end;

  // @name represents the MODFLOW Well boundaries associated with
  // a single @link(TScreenObject).
  //
  // FormulaInterpretation determines whether the @Link(TWellItem.PumpingRate
  // TWellItem.PumpingRate) formulas represent
  // @unorderedlist(
  //   @item(fiSpecific - Pumping Rate / the length or area of
  //     intersection between the @link(TScreenObject) and grid cell.)
  //   @item(fiTotal - Pumping Rate.)
  // )
  // @seealso(TWellCollection)
  TMfWellBoundary = class(TSpecificModflowBoundary)
//  protected
//    FFormulaInterpretation: TFormulaInterpretation;
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TWell_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    // See @link(TModflowParamBoundary.ModflowParamItemClass
    // TModflowParamBoundary.ModflowParamItemClass).
    class function ModflowParamItemClass: TModflowParamItemClass; override;
//    procedure EvaluateCellListBoundaries;
    function ParameterType: TParameterType; override;
  public
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TWellStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW Well parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TWellStorage) in @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList);
      override;
    // ultimately make this virtual;
    procedure GetCellListValues(ValueTimeList: TList; ParamList: TStringList);
    procedure InvalidateDisplay; override;
  end;



implementation

uses ScreenObjectUnit, ModflowTimeUnit, PhastModelUnit, TempFiles, 
  GoPhastTypes, frmGoPhastUnit, GIS_Functions;

const
  PumpingRatePosition = 0;  

{ TWellItem }

procedure TWellItem.Assign(Source: TPersistent);
begin
  if Source is TWellItem then
  begin
    PumpingRate := TWellItem(Source).PumpingRate;
  end;
  inherited;
end;

procedure TWellItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TWellCollection;
  PumpingRateObserver: TObserver;
begin
  ParentCollection := Collection as TWellCollection;
  PumpingRateObserver := FObserverList[PumpingRatePosition];
  PumpingRateObserver.OnUpToDateSet := ParentCollection.InvalidatePumpingRateData;
end;

function TWellItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

procedure TWellItem.CreateFormulaObjects;
begin
  FPumpingRate := CreateFormulaObject(dso3D);
end;

destructor TWellItem.Destroy;
begin
  PumpingRate := '0';
  inherited;
end;

function TWellItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    0: result := PumpingRate;
    else Assert(False);
  end;
end;

procedure TWellItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  Assert(Sender = FPumpingRate);
  List.Add(FObserverList[PumpingRatePosition]);
end;

function TWellItem.GetPumpingRate: string;
begin
  Result := FPumpingRate.Formula;
  ResetItemObserver(PumpingRatePosition);
end;

procedure TWellItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState) then
  begin
    PhastModel.InvalidateMfWellPumpage(self);
  end;
end;

function TWellItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TWellItem;
begin
  result := (AnotherItem is TWellItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TWellItem(AnotherItem);
    result := (Item.PumpingRate = PumpingRate)
  end;
end;

procedure TWellItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FPumpingRate,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
end;

procedure TWellItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  inherited;
  case Index of
    0: PumpingRate := Value;
    else Assert(False);
  end;
end;

procedure TWellItem.SetPumpingRate(const Value: string);
begin
  UpdateFormula(Value, PumpingRatePosition, FPumpingRate);
end;

{ TWellCollection }

procedure TWellCollection.AddSpecificBoundary;
begin
  AddBoundary(TWellStorage.Create);
end;

procedure TWellCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList);
var
  WellStorage: TWellStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  Assert(BoundaryFunctionIndex = 0);
  Assert(Expression <> nil);

  WellStorage := BoundaryStorage as TWellStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdataRequiredData(DataSets, Variables, ACell);
    // 2. update locations
    Expression.Evaluate;
    with WellStorage.WellArray[Index] do
    begin
      PumpingRate := Expression.DoubleResult;
      PumpingRateAnnotation := ACell.Annotation;
    end;
  end;
end;

procedure TWellCollection.AssignCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  WellStorage: TWellStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  WellStorage := BoundaryStorage as TWellStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    with WellStorage.WellArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

{procedure TWellCollection.AssignCellValues(DataSets: TList;
  Sections: T3DSparseIntegerArray; ItemIndex: Integer);
var
  PumpingRateArray: TDataArray;
  Boundary: TWellStorage;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  BoundaryIndex: Integer;
  LocalModel: TPhastModel;
  LayerMin: Integer;
  RowMin: Integer;
  ColMin: Integer;
  LayerMax: Integer;
  RowMax: Integer;
  ColMax: Integer;
begin
  LocalModel := Model as TPhastModel;
  BoundaryIndex := 0;
  PumpingRateArray := DataSets[0];
  Boundary := Boundaries[ItemIndex] as TWellStorage;
  PumpingRateArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
    LayerMax, RowMax, ColMax);
  if LayerMin >= 0 then
  begin
    for LayerIndex := LayerMin to LayerMax do
    begin
      if LocalModel.LayerStructure.IsLayerSimulated(LayerIndex) then
      begin
        for RowIndex := RowMin to RowMax do
        begin
          for ColIndex := ColMin to ColMax do
          begin
            if PumpingRateArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              with Boundary.WellArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                PumpingRate := PumpingRateArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                PumpingRateAnnotation := PumpingRateArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  PumpingRateArray.CacheData;
  Boundary.CacheData;
end;  }

constructor TWellCollection.Create(Boundary: TModflowBoundary; Model,
  ScreenObject: TObject);
begin
  inherited Create(Boundary, Model, ScreenObject);
  FPumpingRateData := TModflowTimeList.Create(Model, ScreenObject);
  FPumpingRateData.NonParamDescription := 'Pumping rate';
  FPumpingRateData.ParamDescription := ' pumping rate multiplier';
  if Model <> nil then
  begin
    FPumpingRateData.OnInvalidate := (Model as TPhastModel).InvalidateMfWellPumpage;
  end;
  
  AddTimeList(FPumpingRateData);
end;

destructor TWellCollection.Destroy;
begin
  FPumpingRateData.Free;
  inherited;
end;

function TWellCollection.AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
var
  Boundary: TMfWellBoundary;
  Item: TWellItem;
  ScreenObject: TScreenObject;
begin
  result := '';
  if FormulaIndex = 0 then
  begin
    Boundary := BoundaryGroup as TMfWellBoundary;
    ScreenObject := Boundary.ScreenObject as TScreenObject;
    Item := Items[ItemIndex] as TWellItem;
    case Boundary.FormulaInterpretation of
      fiSpecific:
        begin
          if ScreenObject.ScreenObjectLength = 0 then
          begin
            result := Item.PumpingRate;
          end
          else if ScreenObject.Closed then
          begin
            result := '(' + Item.PumpingRate
              + ') * ' + StrObjectIntersectArea;
          end
          else
          begin
            result := '(' + Item.PumpingRate
              + ') * ' + StrObjectSectionIntersectLength;
          end;
        end;
      fiDirect:
        begin
          result := Item.PumpingRate;
        end;
      fiTotal:
        begin
          if ScreenObject.ScreenObjectLength = 0 then
          begin
            result := Item.PumpingRate;
          end
          else if ScreenObject.Closed then
          begin
            result := '((' + Item.PumpingRate
              + ') * ' + StrObjectIntersectArea + ') / ' + StrObjectArea;
          end
          else
          begin
            result := '((' + Item.PumpingRate
              + ') * ' + StrObjectSectionIntersectLength+ ') / ' + StrObjectLength;
          end;
        end;
      else Assert(False);
    end;
  end
  else
  begin
    Assert(False);
  end;
end;

{procedure TWellCollection.InitializeTimeLists(ListOfTimeLists: TList);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TWellItem;
  Boundary: TMfWellBoundary;
  ScreenObject: TScreenObject;
begin
  // This procedure is no longer used.
  Assert(False);

  Boundary := BoundaryGroup as TMfWellBoundary;
  ScreenObject := Boundary.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);
//  FPumpingRateData.Initialize(BoundaryValues, ScreenObject);
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TWellItem;
    BoundaryValues[Index].Time := Item.StartTime;
    case Boundary.FormulaInterpretation of
      fiSpecific:
        begin
          if ScreenObject.ScreenObjectLength = 0 then
          begin
            BoundaryValues[Index].Formula := Item.PumpingRate;
          end
          else if ScreenObject.Closed then
          begin
            BoundaryValues[Index].Formula := '(' + Item.PumpingRate
              + ') * ' + StrObjectIntersectArea;
          end
          else
          begin
            BoundaryValues[Index].Formula := '(' + Item.PumpingRate
              + ') * ' + StrObjectIntersectLength;
          end;
        end;
      fiDirect:
        begin
          BoundaryValues[Index].Formula := Item.PumpingRate;
        end;
      fiTotal:
        begin
          if ScreenObject.ScreenObjectLength = 0 then
          begin
            BoundaryValues[Index].Formula := Item.PumpingRate;
          end
          else if ScreenObject.Closed then
          begin
            BoundaryValues[Index].Formula := '((' + Item.PumpingRate
              + ') * ' + StrObjectIntersectArea + ') / ' + StrObjectArea;
          end
          else
          begin
            BoundaryValues[Index].Formula := '((' + Item.PumpingRate
              + ') * ' + StrObjectIntersectLength+ ') / ' + StrObjectLength;
          end;
        end;
      else Assert(False);
    end;
  end;
  FPumpingRateData.Initialize(BoundaryValues, ScreenObject, dsumAdd);
  Assert(FPumpingRateData.Count = Count);
  ClearBoundaries;
  SetBoundaryCapacity(FPumpingRateData.Count);
  for TimeIndex := 0 to FPumpingRateData.Count - 1 do
  begin
    AddBoundary(TWellStorage.Create);
  end;
  ListOfTimeLists.Add(FPumpingRateData);
end; }

procedure TWellCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState) then
  begin
    PhastModel.InvalidateMfWellPumpage(self);
  end;
end;

procedure TWellCollection.InvalidatePumpingRateData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FPumpingRateData.Invalidate;
  end;
end;

class function TWellCollection.ItemClass: TMF_BoundItemClass;
begin
  result := TWellItem;
end;

procedure TWellCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer);
begin
  SetLength((Boundaries[ItemIndex] as TWellStorage).FWellArray, BoundaryCount);
  inherited;
end;

{ TWellParamItem }

class function TWellParamItem.BoundaryClass: TMF_BoundCollClass;
begin
  result := TWellCollection;
end;

{ TWell_Cell }

procedure TWell_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TWell_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TWell_Cell.GetIntegerAnnotation(Index: integer): string;
begin
  result := '';
  Assert(False);
end;

function TWell_Cell.GetIntegerValue(Index: integer): integer;
begin
  result := 0;
  Assert(False);
end;

function TWell_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TWell_Cell.GetPumpingRate: double;
begin
  result := Values.PumpingRate;
end;

function TWell_Cell.GetPumpingRateAnnotation: string;
begin
  result := Values.PumpingRateAnnotation;
end;

function TWell_Cell.GetRealAnnotation(Index: integer): string;
begin
  result := '';
  case Index of
    0: result := PumpingRateAnnotation;
    else Assert(False);
  end;
end;

function TWell_Cell.GetRealValue(Index: integer): double;
begin
  result := 0;
  case Index of
    0: result := PumpingRate;
    else Assert(False);
  end;
end;

function TWell_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TWell_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TWell_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  WEL_Cell: TWell_Cell;
begin
  result := AnotherCell is TWell_Cell;
  if result then
  begin
    WEL_Cell := TWell_Cell(AnotherCell);
    result :=
      (PumpingRate = WEL_Cell.PumpingRate)
      and (IFace = WEL_Cell.IFace);
  end;
end;

procedure TWell_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TWell_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

{ TMfWellBoundary }

procedure TMfWellBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList);
var
  Cell: TWell_Cell;
  BoundaryValues: TWellRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TWellStorage;
begin
  LocalBoundaryStorage := BoundaryStorage as TWellStorage;
  for TimeIndex := 0 to
    (PhastModel as TPhastModel).ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TWell_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := (PhastModel as TPhastModel).ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime <= LocalBoundaryStorage.EndingTime) then
    begin
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.WellArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.WellArray[BoundaryIndex];
        Cell := TWell_Cell.Create;
        Assert(ScreenObject <> nil);
        Cell.IFace := (ScreenObject as TScreenObject).IFace;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TMfWellBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TWellCollection;
end;

//procedure TMfWellBoundary.EvaluateCellListBoundaries;
//begin
//  Parameters.EvaluateListBoundaries;
//  Values.EvaluateListBoundaries;
//end;

procedure TMfWellBoundary.GetCellListValues(ValueTimeList: TList;
  ParamList: TStringList);
var
  ValueIndex: Integer;
  BoundaryStorage: TWellStorage;
  ParamIndex: Integer;
  Param: TModflowParamItem;
  Times: TList;
  Position: integer;
  ParamName: string;
begin
  EvaluateListBoundaries;
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    BoundaryStorage := Values.Boundaries[ValueIndex] as TWellStorage;
    AssignCells(BoundaryStorage, ValueTimeList);
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
    for ValueIndex := 0 to Param.Param.Count - 1 do
    begin
      BoundaryStorage := Param.Param.Boundaries[ValueIndex] as TWellStorage;
      AssignCells(BoundaryStorage, Times);
    end;
  end;
end;

procedure TMfWellBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList);
var
  ValueIndex: Integer;
  BoundaryStorage: TWellStorage;
  ParamIndex: Integer;
  Param: TModflowParamItem;
  Times: TList;
  Position: integer;
  ParamName: string;
begin
//  EvaluateArrayBoundaries;
  EvaluateListBoundaries;
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex] as TWellStorage;
      AssignCells(BoundaryStorage, ValueTimeList);
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
    for ValueIndex := 0 to Param.Param.Count - 1 do
    begin
      if ValueIndex < Param.Param.BoundaryCount then
      begin
        BoundaryStorage := Param.Param.Boundaries[ValueIndex] as TWellStorage;
        AssignCells(BoundaryStorage, Times);
      end;
    end;
  end;
end;

procedure TMfWellBoundary.InvalidateDisplay;
begin
  inherited;
  if Used and (PhastModel <> nil) then
  begin
    (PhastModel as TPhastModel).InvalidateMfWellPumpage(self);
  end;
end;

class function TMfWellBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := TWellParamItem;
end;

function TMfWellBoundary.ParameterType: TParameterType;
begin
  result := ptQ;
end;

{ TWellRecord }

procedure TWellRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, PumpingRate);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(PumpingRateAnnotation));
//  WriteCompString(Comp, PumpingRateAnnotation);
end;

procedure TWellRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(PumpingRateAnnotation);
end;

procedure TWellRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  PumpingRate := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  PumpingRateAnnotation := Annotations[ReadCompInt(Decomp)];
//  PumpingRateAnnotation := ReadCompString(Decomp, Annotations);
end;

{ TWellStorage }

procedure TWellStorage.Clear;
begin
  SetLength(FWellArray, 0);
  FCleared := True;
end;

procedure TWellStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FWellArray);
    for Index := 0 to Count - 1 do
    begin
      FWellArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FWellArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TWellStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FWellArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FWellArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TWellStorage.GetWellArray: TWellArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FWellArray;
end;

end.
