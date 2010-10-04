unit ModflowRchUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets;

type
  {
    @longcode(
  TRchRecord = record
    Cell: TCellLocation;
    RechargeRate: double;
    StartingTime: double;
    EndingTime: double;
    RechargeRateAnnotation: string;
  end;
    )
    @name stores the location, time and recharge rate for a cell.
  }
  TRchRecord = record
    Cell: TCellLocation;
    RechargeRate: double;
    StartingTime: double;
    EndingTime: double;
    RechargeRateAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TRchLayerRecord = record
    Cell: TCellLocation;
    RechargeLayer: integer;
    StartingTime: double;
    EndingTime: double;
    RechargeLayerAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  // @name is an array of @link(TRchRecord)s.
  TRchArray = array of TRchRecord;
  TRchLayerArray = array of TRchLayerRecord;

  // @name extends @link(TCustomBoundaryStorage) by adding the locations
  // and values of series of recharge cells.
  TRchStorage = class(TCustomBoundaryStorage)
  private
    FRchArray: TRchArray;
    function GetRchArray: TRchArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property RchArray: TRchArray read GetRchArray;
  end;

  TRchLayerStorage = class(TCustomBoundaryStorage)
  private
    FRchLayerArray: TRchLayerArray;
    function GetRchLayerArray: TRchLayerArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property RchLayerArray: TRchLayerArray read GetRchLayerArray;
  end;

  // @name represents a MODFLOW recharge for one time interval.
  // @name is stored by @link(TRchCollection).
  TRchItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(RechargeRate).
    FRechargeRate: TFormulaObject;
    // See @link(RechargeRate).
    procedure SetRechargeRate(const Value: string);
    function GetRechargeRate: string;
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
    function BoundaryFormulaCount: integer; override;
  public
    Destructor Destroy; override;
  published
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    // @name is the formula used to set the recharge rate
    // or the recharge rate multiplier of this boundary.
    property RechargeRate: string read GetRechargeRate write SetRechargeRate;
  end;

  // @name represents a MODFLOW recharge layer for one time interval.
  // @name is stored by @link(TRchLayerCollection).
  TRchLayerItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(RechargeLayer).
    FRechargeLayer: TFormulaObject;
    // See @link(RechargeLayer).
    procedure SetRechargeLayer(const Value: string);
    function GetRechargeLayer: string;
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
    function BoundaryFormulaCount: integer; override;
  public
    Destructor Destroy; override;
  published
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    // @name is the formula used to set the recharge rate
    // or the recharge rate multiplier of this boundary.
    property RechargeLayer: string read GetRechargeLayer write SetRechargeLayer;
  end;

  // @name represents MODFLOW Recharge boundaries
  // for a series of time intervals.
  TRchCollection = class(TCustomMF_ArrayBoundColl)
  private
    procedure InvalidateRechargeData(Sender: TObject);
  protected
    // @name is used to compute the recharge rates for a series of
    // cells over a series of time intervals.
    FRechargeRateData: TModflowTimeList;
    procedure AddSpecificBoundary; override;
    // See @link(TCustomMF_ArrayBoundColl.AssignCellValues
    // TCustomMF_ArrayBoundColl.AssignCellValues)
    procedure AssignCellValues(DataSets: TList; ItemIndex: Integer); override;
    // See @link(TCustomMF_ArrayBoundColl.InitializeTimeLists
    // TCustomMF_ArrayBoundColl.InitializeTimeLists)
    procedure InitializeTimeLists(ListOfTimeLists: TList); override;
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TMF_BoundItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TRchStorage.RchArray) at ItemIndex in
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

  TRchLayerCollection = class(TCustomMF_ArrayBoundColl)
  private
    // @name is used to compute the recharge rates for a series of
    // cells over a series of time intervals.
    FRechargeLayerData: TModflowTimeList;
    procedure InvalidateRechLayerData(Sender: TObject);
  protected
    procedure AddSpecificBoundary; override;
    // See @link(TCustomMF_ArrayBoundColl.AssignCellValues
    // TCustomMF_ArrayBoundColl.AssignCellValues)
    procedure AssignCellValues(DataSets: TList; ItemIndex: Integer); override;
    // See @link(TCustomMF_ArrayBoundColl.InitializeTimeLists
    // TCustomMF_ArrayBoundColl.InitializeTimeLists)
    procedure InitializeTimeLists(ListOfTimeLists: TList); override;
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TMF_BoundItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TRchStorage.RchArray) at ItemIndex in
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


  // Each @name stores a @link(TRchCollection).
  // @classname is stored by @link(TModflowParameters).
  TRchParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;

  TRechargeCell = class(TValueCell);

  TRch_Cell = class(TRechargeCell)
  private
    FValues: TRchRecord;
    FStressPeriod: integer;
    function GetRechargeRate: double;
    function GetRechargeRateAnnotation: string;
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
    property StressPeriod: integer read FStressPeriod write FStressPeriod;
    property Values: TRchRecord read FValues write FValues;
    property RechargeRate: double read GetRechargeRate;
    property RechargeRateAnnotation: string read GetRechargeRateAnnotation;
  end;

  TRechargeLayerCell = class(TRechargeCell)
  private
    Values: TRchLayerRecord;
    StressPeriod: integer;
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
  end;


  // @name represents the MODFLOW Recharge boundaries associated with
  // a single @link(TScreenObject).
  //
  // @seealso(TRchCollection)
  TRchBoundary = class(TModflowParamBoundary)
  private
    FRechargeLayers: TRchLayerCollection;
    procedure SetRechargeLayers(const Value: TRchLayerCollection);
    function GetTimeVaryingRechargeLayers: boolean;
    procedure AssignRechargeLayerCells(BoundaryStorage: TRchLayerStorage;
      ValueTimeList: TList);
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TRch_Cell)s for that stress period.
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
    procedure Assign(Source: TPersistent);override;

    Constructor Create(Model, ScreenObject: TObject);
    Destructor Destroy; override;
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TRchStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW Recharge parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TRchStorage) in @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList);
      override;
    function Used: boolean; override;
    procedure EvaluateArrayBoundaries; override;
    function NonParameterColumns: integer; override;
    property TimeVaryingRechargeLayers: boolean
      read GetTimeVaryingRechargeLayers;
    procedure GetRechargeLayerCells(LayerTimeList: TList);
    procedure InvalidateDisplay; override;
    procedure Clear; override;
  published
    property RechargeLayers: TRchLayerCollection read FRechargeLayers
      write SetRechargeLayers;
  end;

implementation

uses RbwParser, ScreenObjectUnit, PhastModelUnit, ModflowTimeUnit,
  ModflowTransientListParameterUnit, frmGoPhastUnit, TempFiles, GoPhastTypes;

const
  StrAssignedFromTheCe = 'assigned from the cell''s layer';
  RechPosition = 0;
  LayerPosition = 0;

{ TRchItem }

procedure TRchItem.Assign(Source: TPersistent);
begin
  if Source is TRchItem then
  begin
    RechargeRate := TRchItem(Source).RechargeRate;
  end;
  inherited;
end;

procedure TRchItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TRchCollection;
  RechObserver: TObserver;
begin
  ParentCollection := Collection as TRchCollection;
  RechObserver := FObserverList[RechPosition];
  RechObserver.OnUpToDateSet := ParentCollection.InvalidateRechargeData;
end;

function TRchItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

procedure TRchItem.CreateFormulaObjects;
begin
  inherited;
  FRechargeRate := CreateFormulaObject(dsoTop);
end;

destructor TRchItem.Destroy;
begin
  RechargeRate := '0';
  inherited;
end;

function TRchItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    RechPosition: result := RechargeRate;
    else Assert(False);
  end;
end;

procedure TRchItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  Assert(Sender = FRechargeRate);
  List.Add(FObserverList[RechPosition]);
end;

function TRchItem.GetRechargeRate: string;
begin
  Result := FRechargeRate.Formula;
  ResetItemObserver(RechPosition);
end;

function TRchItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TRchItem;
begin
  result := (AnotherItem is TRchItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TRchItem(AnotherItem);
    result := (Item.RechargeRate = RechargeRate)
  end;
end;

procedure TRchItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FRechargeRate,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
end;

procedure TRchItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    RechPosition: RechargeRate := Value;
    else Assert(False);
  end;
end;

procedure TRchItem.SetRechargeRate(const Value: string);
begin
  UpdateFormula(Value, RechPosition, FRechargeRate);
end;

{ TRchCollection }

procedure TRchCollection.AddSpecificBoundary;
begin
  AddBoundary(TRchStorage.Create);
end;

procedure TRchCollection.AssignCellValues(DataSets: TList; ItemIndex: Integer);
var
  RechargeRateArray: TDataArray;
  Boundary: TRchStorage;
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
  RechargeRateArray := DataSets[RechPosition];
  Boundary := Boundaries[ItemIndex] as TRchStorage;
  RechargeRateArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
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
            if RechargeRateArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              with Boundary.RchArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                RechargeRate := RechargeRateArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                RechargeRateAnnotation := RechargeRateArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  RechargeRateArray.CacheData;
  Boundary.CacheData;
end;

constructor TRchCollection.Create(Boundary: TModflowBoundary; Model,
  ScreenObject: TObject);
begin
  inherited Create(Boundary, Model, ScreenObject);
  FRechargeRateData := TModflowTimeList.Create(Model, ScreenObject);
  FRechargeRateData.NonParamDescription := 'Recharge rate';
  FRechargeRateData.ParamDescription := ' recharge rate multiplier';
  AddTimeList(FRechargeRateData);
  if Model <> nil then
  begin
    FRechargeRateData.OnInvalidate :=
      (Model as TPhastModel).InvalidateMfRchRate;
  end;
end;

destructor TRchCollection.Destroy;
begin
  FRechargeRateData.Free;
  inherited;
end;

procedure TRchCollection.InitializeTimeLists(ListOfTimeLists: TList);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TRchItem;
  ScreenObject: TScreenObject;
begin
  ScreenObject := BoundaryGroup.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TRchItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.RechargeRate;
  end;
  FRechargeRateData.Initialize(BoundaryValues, ScreenObject);
  Assert(FRechargeRateData.Count = Count);
  ClearBoundaries;
  SetBoundaryCapacity(FRechargeRateData.Count);
  for TimeIndex := 0 to FRechargeRateData.Count - 1 do
  begin
    AddBoundary(TRchStorage.Create);
  end;
  ListOfTimeLists.Add(FRechargeRateData);
end;

procedure TRchCollection.InvalidateRechargeData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FRechargeRateData.Invalidate;
  end;
end;

class function TRchCollection.ItemClass: TMF_BoundItemClass;
begin
  result := TRchItem;
end;

procedure TRchCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer);
begin
  SetLength((Boundaries[ItemIndex] as TRchStorage).FRchArray, BoundaryCount);
  inherited;
end;

{ TRchParamItem }

class function TRchParamItem.BoundaryClass: TMF_BoundCollClass;
begin
  result := TRchCollection;
end;

{ TRch_Cell }

procedure TRch_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TRch_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TRch_Cell.GetIntegerAnnotation(Index: integer): string;
begin
  result := '';
  case Index of
    RechPosition: result := StrAssignedFromTheCe;
    else Assert(False);
  end;
end;

function TRch_Cell.GetIntegerValue(Index: integer): integer;
begin
  result := 0;
  case Index of
    RechPosition: result := frmGoPhast.PhastModel.LayerStructure.
         DataSetLayerToModflowLayer(Layer);
    else Assert(False);
  end;
end;

function TRch_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TRch_Cell.GetRealAnnotation(Index: integer): string;
begin
  result := '';
  case Index of
    RechPosition: result := RechargeRateAnnotation;
    else Assert(False);
  end;
end;

function TRch_Cell.GetRealValue(Index: integer): double;
begin
  result := 0;
  case Index of
    RechPosition: result := RechargeRate;
    else Assert(False);
  end;
end;

function TRch_Cell.GetRechargeRate: double;
begin
  result := Values.RechargeRate;
end;

function TRch_Cell.GetRechargeRateAnnotation: string;
begin
  result := Values.RechargeRateAnnotation;
end;

function TRch_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TRch_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

procedure TRch_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TRch_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

{ TRchBoundary }

procedure TRchBoundary.Assign(Source: TPersistent);
var
  SourceBoundary: TRchBoundary;
begin
  if Source is TRchBoundary then
  begin
    SourceBoundary := TRchBoundary(Source);
    RechargeLayers := SourceBoundary.RechargeLayers;
  end;
  inherited;
end;

procedure TRchBoundary.AssignRechargeLayerCells(
  BoundaryStorage: TRchLayerStorage; ValueTimeList: TList);
var
  Cell: TRechargeLayerCell;
  BoundaryValues: TRchLayerRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TRchLayerStorage;
begin
  LocalBoundaryStorage := BoundaryStorage;// as TRchStorage;
  for TimeIndex := 0 to
    (PhastModel as TPhastModel).ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TRechargeLayerCell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := (PhastModel as TPhastModel).
      ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime <= LocalBoundaryStorage.EndingTime) then
    begin
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to
        Length(LocalBoundaryStorage.RchLayerArray) - 1 do
      begin
//        Cells.Cached := False;
        BoundaryValues := LocalBoundaryStorage.RchLayerArray[BoundaryIndex];
        Cell := TRechargeLayerCell.Create;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;


procedure TRchBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList);
var
  Cell: TRch_Cell;
  BoundaryValues: TRchRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TRchStorage;
begin
  LocalBoundaryStorage := BoundaryStorage as TRchStorage;
  for TimeIndex := 0 to
    (PhastModel as TPhastModel).ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TRch_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := (PhastModel as TPhastModel).
      ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime <= LocalBoundaryStorage.EndingTime) then
    begin
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.RchArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.RchArray[BoundaryIndex];
        Cell := TRch_Cell.Create;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TRchBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TRchCollection;
end;

procedure TRchBoundary.Clear;
begin
  inherited;
  RechargeLayers.Clear;
end;

constructor TRchBoundary.Create(Model, ScreenObject: TObject);
begin
  inherited Create(Model, ScreenObject);
  FRechargeLayers := TRchLayerCollection.Create(self, Model, ScreenObject);
end;

destructor TRchBoundary.Destroy;
begin
  FRechargeLayers.Free;
  inherited;
end;

procedure TRchBoundary.EvaluateArrayBoundaries;
begin
  inherited;
  if (PhastModel as TPhastModel).
    ModflowPackages.RchPackage.TimeVaryingLayers then
  begin
    RechargeLayers.EvaluateArrayBoundaries;
  end;
end;

procedure TRchBoundary.GetRechargeLayerCells(LayerTimeList: TList);
var
  ValueIndex: Integer;
  BoundaryStorage: TRchLayerStorage;
begin
  if not (PhastModel as TPhastModel).ModflowPackages.
    RchPackage.TimeVaryingLayers then
  begin
    Exit;
  end;
  for ValueIndex := 0 to RechargeLayers.Count - 1 do
  begin
    BoundaryStorage := RechargeLayers.Boundaries[ValueIndex]
      as TRchLayerStorage;
    AssignRechargeLayerCells(BoundaryStorage, LayerTimeList);
  end;
end;

procedure TRchBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList);
var
  ValueIndex: Integer;
  BoundaryStorage: TRchStorage;
  ParamIndex: Integer;
  Param: TModflowParamItem;
  Times: TList;
  Position: integer;
  ParamName: string;
  Model: TPhastModel;
begin
  EvaluateArrayBoundaries;
  Model := PhastModel as TPhastModel;
  if Model.ModflowTransientParameters.CountParam(ParameterType) = 0 then
  begin
    for ValueIndex := 0 to Values.Count - 1 do
    begin
      if ValueIndex < Values.BoundaryCount then
      begin
        BoundaryStorage := Values.Boundaries[ValueIndex] as TRchStorage;
        AssignCells(BoundaryStorage, ValueTimeList);
      end;
    end;
  end
  else
  begin
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
          BoundaryStorage := Param.Param.Boundaries[ValueIndex] as TRchStorage;
          AssignCells(BoundaryStorage, Times);
        end;
      end;
    end;
  end;
  ClearBoundaries
end;

function TRchBoundary.GetTimeVaryingRechargeLayers: boolean;
begin
  if PhastModel = nil then
  begin
    result := frmGoPhast.PhastModel.ModflowPackages.
      RchPackage.TimeVaryingLayers;
  end
  else
  begin
    result := (PhastModel as TPhastModel).ModflowPackages.
      RchPackage.TimeVaryingLayers;
  end;
end;

procedure TRchBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (PhastModel <> nil) then
  begin
    Model := PhastModel as TPhastModel;
    Model.InvalidateMfRchRate(self);
    Model.InvalidateMfRchLayer(self);
  end;
end;

class function TRchBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := TRchParamItem;
end;

function TRchBoundary.NonParameterColumns: integer;
begin
  result := inherited NonParameterColumns;
  if TimeVaryingRechargeLayers then
  begin
    result := result + RechargeLayers.TimeListCount;
  end;
end;

function TRchBoundary.ParameterType: TParameterType;
begin
  result := ptRCH;
end;

procedure TRchBoundary.SetRechargeLayers(const Value: TRchLayerCollection);
begin
  FRechargeLayers.Assign(Value);
end;

function TRchBoundary.Used: boolean;
var
  Model: TPhastModel;
  ParamIndex: Integer;
  Param: TModflowTransientListParameter;
begin
  if PhastModel <> nil then
  begin
    Model := PhastModel as TPhastModel;
    result := Model.ModflowPackages.RchPackage.TimeVaryingLayers
      and RechargeLayers.Used;
  end
  else
  begin
    result := RechargeLayers.Used;
  end;
  if result then Exit;
  result := inherited Used;
  if result and (PhastModel <> nil) then
  begin
    Model := PhastModel as TPhastModel;
    for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
    begin
      Param := Model.ModflowTransientParameters[ParamIndex];
      if Param.ParameterType = ptRCH then
      begin
        result := Parameters.Used;
        Exit;
      end;
    end;
    result := Values.Used;
  end;
end;

{ TRchLayerItem }

procedure TRchLayerItem.Assign(Source: TPersistent);
begin
  if Source is TRchLayerItem then
  begin
    RechargeLayer := TRchLayerItem(Source).RechargeLayer;
  end;
  inherited;
end;

procedure TRchLayerItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TRchLayerCollection;
  RechLayerObserver: TObserver;
begin
  ParentCollection := Collection as TRchLayerCollection;
  RechLayerObserver := FObserverList[LayerPosition];
  RechLayerObserver.OnUpToDateSet := ParentCollection.InvalidateRechLayerData;
end;

function TRchLayerItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

procedure TRchLayerItem.CreateFormulaObjects;
begin
  FRechargeLayer := CreateFormulaObject(dsoTop);
end;

destructor TRchLayerItem.Destroy;
begin
  RechargeLayer := '0';
  inherited;
end;

function TRchLayerItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    LayerPosition: result := RechargeLayer;
    else Assert(False);
  end;
end;

procedure TRchLayerItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  Assert(Sender = FRechargeLayer);
  List.Add(FObserverList[LayerPosition]);
end;

function TRchLayerItem.GetRechargeLayer: string;
begin
  Result := FRechargeLayer.Formula;
  ResetItemObserver(LayerPosition);
end;

function TRchLayerItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TRchLayerItem;
begin
  result := (AnotherItem is TRchItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TRchLayerItem(AnotherItem);
    result := (Item.RechargeLayer = RechargeLayer)
  end;
end;

procedure TRchLayerItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FRechargeLayer,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
end;

procedure TRchLayerItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    LayerPosition: RechargeLayer := Value;
    else Assert(False);
  end;
end;

procedure TRchLayerItem.SetRechargeLayer(const Value: string);
begin
  UpdateFormula(Value, LayerPosition, FRechargeLayer);
end;

{ TRchLayerCollection }

procedure TRchLayerCollection.AddSpecificBoundary;
begin
  AddBoundary(TRchLayerStorage.Create);
end;

procedure TRchLayerCollection.AssignCellValues(DataSets: TList;
  ItemIndex: Integer);
var
  RechargeLayerArray: TDataArray;
  Boundary: TRchLayerStorage;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  BoundaryIndex: Integer;
  LayerMin: Integer;
  RowMin: Integer;
  ColMin: Integer;
  LayerMax: Integer;
  RowMax: Integer;
  ColMax: Integer;
begin
  BoundaryIndex := 0;
  RechargeLayerArray := DataSets[LayerPosition];
  Boundary := Boundaries[ItemIndex] as TRchLayerStorage;
  RechargeLayerArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
    LayerMax, RowMax, ColMax);
  if LayerMin >= 0 then
  begin
    for LayerIndex := LayerMin to LayerMax do
    begin
      for RowIndex := RowMin to RowMax do
      begin
        for ColIndex := ColMin to ColMax do
        begin
          if RechargeLayerArray.IsValue[LayerIndex, RowIndex, ColIndex] then
          begin
            with Boundary.RchLayerArray[BoundaryIndex] do
            begin
              Cell.Layer := LayerIndex;
              Cell.Row := RowIndex;
              Cell.Column := ColIndex;
//              Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
              RechargeLayer := RechargeLayerArray.
                IntegerData[LayerIndex, RowIndex, ColIndex];
              RechargeLayerAnnotation := RechargeLayerArray.
                Annotation[LayerIndex, RowIndex, ColIndex];
            end;
            Inc(BoundaryIndex);
          end;
        end;
      end;
    end;
  end;
  RechargeLayerArray.CacheData;
  Boundary.CacheData;
end;

constructor TRchLayerCollection.Create(Boundary: TModflowBoundary; Model,
  ScreenObject: TObject);
begin
  inherited Create(Boundary, Model, ScreenObject);
  FRechargeLayerData := TModflowTimeList.Create(Model, ScreenObject);
  FRechargeLayerData.NonParamDescription := 'Recharge layer';
  FRechargeLayerData.ParamDescription := ' recharge layer';
  FRechargeLayerData.DataType := rdtInteger;
  AddTimeList(FRechargeLayerData);
  if Model <> nil then
  begin
    FRechargeLayerData.OnInvalidate :=
      (Model as TPhastModel).InvalidateMfRchLayer;
  end;
end;

destructor TRchLayerCollection.Destroy;
begin
  FRechargeLayerData.Free;
  inherited;
end;

procedure TRchLayerCollection.InitializeTimeLists(ListOfTimeLists: TList);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TRchLayerItem;
  Boundary: TRchBoundary;
  ScreenObject: TScreenObject;
begin
  Boundary := BoundaryGroup as TRchBoundary;
  ScreenObject := Boundary.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TRchLayerItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.RechargeLayer;
  end;
  FRechargeLayerData.Initialize(BoundaryValues, ScreenObject);
  Assert(FRechargeLayerData.Count = Count);
  ClearBoundaries;
  SetBoundaryCapacity(FRechargeLayerData.Count);
  for TimeIndex := 0 to FRechargeLayerData.Count - 1 do
  begin
    AddBoundary(TRchLayerStorage.Create);
  end;
  ListOfTimeLists.Add(FRechargeLayerData);
end;

procedure TRchLayerCollection.InvalidateRechLayerData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FRechargeLayerData.Invalidate;
  end;
end;

class function TRchLayerCollection.ItemClass: TMF_BoundItemClass;
begin
  result := TRchLayerItem;
end;

procedure TRchLayerCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer);
begin
  SetLength((Boundaries[ItemIndex] as TRchLayerStorage).FRchLayerArray,
    BoundaryCount);
  inherited;
end;

{ TRechargeLayerCell }

procedure TRechargeLayerCell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TRechargeLayerCell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TRechargeLayerCell.GetIntegerAnnotation(Index: integer): string;
begin
  result := '';
  case Index of
    LayerPosition: result := Values.RechargeLayerAnnotation;
    else Assert(False);
  end;
end;

function TRechargeLayerCell.GetIntegerValue(Index: integer): integer;
begin
  result := 0;
  case Index of
    LayerPosition: result := Layer + 1;
    else Assert(False);
  end;
end;

function TRechargeLayerCell.GetLayer: integer;
begin
  // 1 is subtractred from RechargeLayer in order to compensate
  // for 1 being added to the layer in TModflowRCH_Writer.WriteRechargeLayer.
  result := Values.RechargeLayer-1;
end;

function TRechargeLayerCell.GetRealAnnotation(Index: integer): string;
begin
  result := '';
  Assert(False);
end;

function TRechargeLayerCell.GetRealValue(Index: integer): double;
begin
  result := 0;
  Assert(False);
end;

function TRechargeLayerCell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TRechargeLayerCell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

procedure TRechargeLayerCell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TRechargeLayerCell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

{ TRchStorage }

procedure TRchStorage.Clear;
begin
  SetLength(FRchArray, 0);
  FCleared := True;
end;

procedure TRchStorage.Store(Compressor: TCompressionStream);
var
  Count: Integer;
  Index: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FRchArray);
    for Index := 0 to Count - 1 do
    begin
      FRchArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FRchArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TRchStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FRchArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FRchArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TRchStorage.GetRchArray: TRchArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FRchArray;
end;

{ TRchLayerStorage }

procedure TRchLayerStorage.Clear;
begin
  SetLength(FRchLayerArray, 0);
  FCleared := True;
end;

procedure TRchLayerStorage.Store(Compressor: TCompressionStream);
var
  Count: Integer;
  Index: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FRchLayerArray);
    for Index := 0 to Count - 1 do
    begin
      FRchLayerArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FRchLayerArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TRchLayerStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Count: Integer;
  Index: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FRchLayerArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FRchLayerArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TRchLayerStorage.GetRchLayerArray: TRchLayerArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FRchLayerArray;
end;

{ TRchRecord }

procedure TRchRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, RechargeRate);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(RechargeRateAnnotation));
//  WriteCompString(Comp, RechargeRateAnnotation);
end;

procedure TRchRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(RechargeRateAnnotation);
end;

procedure TRchRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  RechargeRate := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  RechargeRateAnnotation := Annotations[ReadCompInt(Decomp)];
//  RechargeRateAnnotation := ReadCompString(Decomp, Annotations);
end;

{ TRchLayerRecord }

procedure TRchLayerRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompInt(Comp, RechargeLayer);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(RechargeLayerAnnotation));
//  WriteCompString(Comp, RechargeLayerAnnotation);
end;

procedure TRchLayerRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(RechargeLayerAnnotation);
end;

procedure TRchLayerRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  RechargeLayer := ReadCompInt(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  RechargeLayerAnnotation := Annotations[ReadCompInt(Decomp)];
//  RechargeLayerAnnotation := ReadCompString(Decomp, Annotations);
end;

end.
