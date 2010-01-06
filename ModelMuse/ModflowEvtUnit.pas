{@name defines how evapotranspiration package data for one @link(TScreenObject)
 is stored and processed. }
unit ModflowEvtUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets;

type
  {
    @longcode(
  TEvtRecord = record
    Cell: TCellLocation;
    EvapotranspirationRate: double;
    StartingTime: double;
    EndingTime: double;
    EvapotranspirationRateAnnotation: string;
  end;
    )
    @name stores the location, time and evapotranspiration rate for a cell.
  }
  TEvtRecord = record
    Cell: TCellLocation;
    EvapotranspirationRate: double;
    StartingTime: double;
    EndingTime: double;
    EvapotranspirationRateAnnotation: string;
    procedure Cache(Comp: TCompressionStream);
    procedure Restore(Decomp: TDecompressionStream);
  end;

  {
    @longcode(
  TEvtSurfDepthRecord = record
    Cell: TCellLocation;
    EvapotranspirationSurface: double;
    EvapotranspirationDepth: double;
    StartingTime: double;
    EndingTime: double;
    EvapotranspirationSurfaceAnnotation: string;
    EvapotranspirationDepthAnnotation: string;
  end;
    )
    @name stores the location, time, evaporation surface and evaporation depth for a cell.
  }
  TEvtSurfDepthRecord = record
    Cell: TCellLocation;
    EvapotranspirationSurface: double;
    EvapotranspirationDepth: double;
    StartingTime: double;
    EndingTime: double;
    EvapotranspirationSurfaceAnnotation: string;
    EvapotranspirationDepthAnnotation: string;
    procedure Cache(Comp: TCompressionStream);
    procedure Restore(Decomp: TDecompressionStream);
  end;

  {
    @longcode(
  TEvtLayerRecord = record
    Cell: TCellLocation;
    EvapotranspirationLayer: integer;
    StartingTime: double;
    EndingTime: double;
    EvapotranspirationLayerAnnotation: string;
  end;
    )
    @name stores the location, and evaporation layer for a cell.
  }
  TEvtLayerRecord = record
    Cell: TCellLocation;
    EvapotranspirationLayer: integer;
    StartingTime: double;
    EndingTime: double;
    EvapotranspirationLayerAnnotation: string;
    procedure Cache(Comp: TCompressionStream);
    procedure Restore(Decomp: TDecompressionStream);
  end;

  // @name is an array of @link(TEvtRecord)s.
  TEvtArray = array of TEvtRecord;
  // @name is an array of @link(TEvtLayerRecord)s.
  TEvtLayerArray = array of TEvtLayerRecord;
  // @name is an array of @link(TEvtSurfDepthRecord)s.
  TEvtSurfDepthArray = array of TEvtSurfDepthRecord;

  // @name extends @link(TCustomBoundaryStorage) by adding the locations
  // and evapotranspiration rates of series of evapotranspiration cells.
  TEvtStorage = class(TCustomBoundaryStorage)
  private
    FEvtArray: TEvtArray;
    function GetEvtArray: TEvtArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property EvtArray: TEvtArray read GetEvtArray;
  end;

  // @name extends @link(TCustomBoundaryStorage) by adding the locations
  // and evapotranspiration layers of series of evapotranspiration cells.
  TEvtLayerStorage = class(TCustomBoundaryStorage)
  private
    FEvtLayerArray: TEvtLayerArray;
    function GetEvtLayerArray: TEvtLayerArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property EvtLayerArray: TEvtLayerArray read GetEvtLayerArray;
  end;

  // @name extends @link(TCustomBoundaryStorage) by adding the locations
  // and evapotranspiration surfaces and depths of series of
  // evapotranspiration cells.
  TEvtSurfDepthStorage = class(TCustomBoundaryStorage)
  private
    FEvtSurfDepthArray: TEvtSurfDepthArray;
    function GetEvtSurfDepthArray: TEvtSurfDepthArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property EvtSurfDepthArray: TEvtSurfDepthArray read GetEvtSurfDepthArray;
  end;

  // @name represents a MODFLOW evapotranspiration for one time interval.
  // @name is stored by @link(TEvtCollection).
  TEvtItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(EvapotranspirationRate).
    FEvapotranspirationRate: TFormulaObject;
    // See @link(EvapotranspirationRate).
    procedure SetEvapotranspirationRate(const Value: string);
    function GetEvapotranspirationRate: string;
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
    property EvapotranspirationRate: string read GetEvapotranspirationRate
      write SetEvapotranspirationRate;
  end;

  // @name represents a MODFLOW ET layer for one time interval.
  // @name is stored by @link(TEvtLayerCollection).
  TEvtLayerItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(EvapotranspirationLayer).
    FEvapotranspirationLayer: TFormulaObject;
    // See @link(EvapotranspirationLayer).
    procedure SetEvapotranspirationLayer(const Value: string);
    function GetEvapotranspirationLayer: string;
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
    property EvapotranspirationLayer: string read GetEvapotranspirationLayer write SetEvapotranspirationLayer;
  end;

  // @name represents a MODFLOW ET layer for one time interval.
  // @name is stored by @link(TEvtSurfDepthCollection).
  TEvtSurfDepthItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(EvapotranspirationSurface).
    FEvapotranspirationSurface: TFormulaObject;
    FEvapotranspirationDepth: TFormulaObject;
    // See @link(EvapotranspirationSurface).
    procedure SetEvapotranspirationSurface(const Value: string);
    procedure SetEvapotranspirationDepth(const Value: string);
    function GetEvapotranspirationDepth: string;
    function GetEvapotranspirationSurface: string;
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
    property EvapotranspirationSurface: string read GetEvapotranspirationSurface
      write SetEvapotranspirationSurface;
    property EvapotranspirationDepth: string read GetEvapotranspirationDepth
      write SetEvapotranspirationDepth;
  end;


  // @name represents MODFLOW Evapotranspiration boundaries
  // for a series of time intervals.
  TEvtCollection = class(TCustomMF_ArrayBoundColl)
  protected
    // @name is used to compute the recharge rates for a series of
    // cells over a series of time intervals.
    FEvapotranspirationRateData: TModflowTimeList;
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
    // the @link(TEvtStorage.EvtArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer); override;
    procedure InvalidateEtRateData(Sender: TObject);
  public
    // @name creates an instance of @classname
    constructor Create(Boundary: TModflowBoundary; Model,
      ScreenObject: TObject); override;
    // @name destroys the current instance of @classname.
    // Do not call @name; call Free instead.
    destructor Destroy; override;
  end;

  TEvtLayerCollection = class(TCustomMF_ArrayBoundColl)
  protected
    // @name is used to compute the recharge rates for a series of
    // cells over a series of time intervals.
    FEvapotranspirationLayerData: TModflowTimeList;
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
    // the @link(TEvtStorage.EvtArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer); override;
    procedure InvalidateEtLayer(Sender: TObject);
  public
    // @name creates an instance of @classname
    constructor Create(Boundary: TModflowBoundary; Model,
      ScreenObject: TObject); override;
    // @name destroys the current instance of @classname.
    // Do not call @name; call Free instead.
    destructor Destroy; override;
  end;

  TEvtSurfDepthCollection = class(TCustomMF_ArrayBoundColl)
  private
    // @name is used to compute the evapotranspiration surface for a series of
    // cells over a series of time intervals.
    FEvapotranspirationSurfaceData: TModflowTimeList;
    // @name is used to compute the evapotranspiration
    // cutoff depth for a series of
    // cells over a series of time intervals.
    FEvapotranspirationDepthData: TModflowTimeList;
    procedure InvalidateSurfaceData(Sender: TObject);
    procedure InvalidateDepthData(Sender: TObject);
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
    // the @link(TEvtStorage.EvtArray) at ItemIndex in
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

  // Each @name stores a @link(TEvtCollection).
  // @classname is stored by @link(TModflowParameters).
  TEvtParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;

  TEvapotranspirationCell = class(TValueCell);

  TEvt_Cell = class(TEvapotranspirationCell)
  private
    FValues: TEvtRecord;
    FStressPeriod: integer;
    function GetEvapotranspirationRate: double;
    function GetEvapotranspirationRateAnnotation: string;
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
    property EvapotranspirationRate: double read GetEvapotranspirationRate;
    property EvapotranspirationRateAnnotation: string read GetEvapotranspirationRateAnnotation;
    property StressPeriod: integer read FStressPeriod write FStressPeriod;
    property Values: TEvtRecord read FValues write FValues;
  end;

  TEvapotranspirationLayerCell = class(TEvapotranspirationCell)
  private
    FValues: TEvtLayerRecord;
    FStressPeriod: integer;
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
    property StressPeriod: integer read FStressPeriod write FStressPeriod;
    property Values: TEvtLayerRecord read FValues write FValues;
  end;

  TEvtSurfDepth_Cell = class(TEvapotranspirationCell)
  private
    Values: TEvtSurfDepthRecord;
    StressPeriod: integer;
    function GetEvapotranspirationSurface: double;
    function GetEvapotranspirationDepth: double;
    function GetEvapotranspirationDepthAnnotation: string;
    function GetEvapotranspirationSurfaceAnnotation: string;
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
    property EvapotranspirationSurface: double read GetEvapotranspirationSurface;
    property EvapotranspirationDepth: double read GetEvapotranspirationDepth;
    property EvapotranspirationSurfaceAnnotation: string read GetEvapotranspirationSurfaceAnnotation;
    property EvapotranspirationDepthAnnotation: string read GetEvapotranspirationDepthAnnotation;
  end;


  // @name represents the MODFLOW Evapotranspiration boundaries associated with
  // a single @link(TScreenObject).
  //
  // @seealso(TEvtCollection)
  TEvtBoundary = class(TModflowParamBoundary)
  private
    FEvapotranspirationLayers: TEvtLayerCollection;
    FEvtSurfDepthCollection: TEvtSurfDepthCollection;
    procedure SetEvapotranspirationLayers(const Value: TEvtLayerCollection);
    procedure SetEvtSurfDepthCollection(const Value: TEvtSurfDepthCollection);
    function GetTimeVaryingEvapotranspirationLayers: boolean;
    procedure AssignEvapotranspirationLayerCells(BoundaryStorage: TEvtLayerStorage;
      ValueTimeList: TList);
    procedure AssignSurfaceDepthCells(BoundaryStorage: TEvtSurfDepthStorage;
      ValueTimeList: TList);
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TEvt_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    // See @link(TModflowParamBoundary.ModflowParamItemClass
    // TModflowParamBoundary.ModflowParamItemClass).
    class function ModflowParamItemClass: TModflowParamItemClass; override;
  public
    procedure Assign(Source: TPersistent);override;

    Constructor Create(Model, ScreenObject: TObject);
    Destructor Destroy; override;
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TEvtStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW Evapotranspiration parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TEvtStorage) in @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList);
      override;
    function Used: boolean; override;
    procedure EvaluateArrayBoundaries; override;
    function NonParameterColumns: integer; override;
    property TimeVaryingEvapotranspirationLayers: boolean
      read GetTimeVaryingEvapotranspirationLayers;
    procedure GetEvapotranspirationLayerCells(LayerTimeList: TList);
    procedure GetEvapotranspirationSurfaceDepthCells(LayerTimeList: TList);
    procedure Clear; override;
  published
    property EvapotranspirationLayers: TEvtLayerCollection
      read FEvapotranspirationLayers write SetEvapotranspirationLayers;
    property EvtSurfDepthCollection: TEvtSurfDepthCollection
      read FEvtSurfDepthCollection write SetEvtSurfDepthCollection;
    procedure InvalidateDisplay; override;
  end;

implementation

uses RbwParser, ScreenObjectUnit, PhastModelUnit, ModflowTimeUnit,
  ModflowTransientListParameterUnit, frmGoPhastUnit, TempFiles, GoPhastTypes;

const
  RatePosition = 0;
  LayerPosition = 0;
  SurfacePosition = 0;
  DepthPosition = 1;

{ TEvtItem }

procedure TEvtItem.Assign(Source: TPersistent);
begin
  if Source is TEvtItem then
  begin
    EvapotranspirationRate := TEvtItem(Source).EvapotranspirationRate;
  end;
  inherited;
end;

procedure TEvtItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TEvtCollection;
  RateObserver: TObserver;
begin
  ParentCollection := Collection as TEvtCollection;
  RateObserver := FObserverList[RatePosition];
  RateObserver.OnUpToDateSet := ParentCollection.InvalidateEtRateData;
end;

function TEvtItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

procedure TEvtItem.CreateFormulaObjects;
begin
  FEvapotranspirationRate := CreateFormulaObject(dsoTop);
end;

destructor TEvtItem.Destroy;
begin
  EvapotranspirationRate := '0';
  inherited;
end;

function TEvtItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    RatePosition: result := EvapotranspirationRate;
    else Assert(False);
  end;
end;

function TEvtItem.GetEvapotranspirationRate: string;
begin
  Result := FEvapotranspirationRate.Formula;
  ResetItemObserver(RatePosition);
end;

procedure TEvtItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  Assert(Sender = FEvapotranspirationRate);
  List.Add(FObserverList[RatePosition]);
end;

function TEvtItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TEvtItem;
begin
  result := (AnotherItem is TEvtItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TEvtItem(AnotherItem);
    result := (Item.EvapotranspirationRate = EvapotranspirationRate)
  end;
end;

procedure TEvtItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FEvapotranspirationRate,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
end;

procedure TEvtItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    RatePosition: EvapotranspirationRate := Value;
    else Assert(False);
  end;
end;

procedure TEvtItem.SetEvapotranspirationRate(const Value: string);
begin
  UpdateFormula(Value, RatePosition, FEvapotranspirationRate);
end;

{ TEvtCollection }

procedure TEvtCollection.AddSpecificBoundary;
begin
  AddBoundary(TEvtStorage.Create);
end;

procedure TEvtCollection.AssignCellValues(DataSets: TList; ItemIndex: Integer);
var
  EvapotranspirationRateArray: TDataArray;
  Boundary: TEvtStorage;
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
  EvapotranspirationRateArray := DataSets[RatePosition];
  Boundary := Boundaries[ItemIndex] as TEvtStorage;
  EvapotranspirationRateArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
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
            if EvapotranspirationRateArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              with Boundary.EvtArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                EvapotranspirationRate := EvapotranspirationRateArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                EvapotranspirationRateAnnotation := EvapotranspirationRateArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  EvapotranspirationRateArray.CacheData;
  Boundary.CacheData;
end;

constructor TEvtCollection.Create(Boundary: TModflowBoundary; Model,
  ScreenObject: TObject);
begin
  inherited Create(Boundary, Model, ScreenObject);
  FEvapotranspirationRateData := TModflowTimeList.Create(Model);
  FEvapotranspirationRateData.NonParamDescription := 'Evapo- transpiration rate';
  FEvapotranspirationRateData.ParamDescription := ' evapo- transpiration rate multiplier';
  AddTimeList(FEvapotranspirationRateData);
  if Model <> nil then
  begin
    FEvapotranspirationRateData.OnInvalidate :=
      (Model as TPhastModel).InvalidateMfEvtEvapRate;
  end;
end;

destructor TEvtCollection.Destroy;
begin
  FEvapotranspirationRateData.Free;
  inherited;
end;

procedure TEvtCollection.InitializeTimeLists(ListOfTimeLists: TList);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TEvtItem;
  ScreenObject: TScreenObject;
begin
  ScreenObject := BoundaryGroup.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TEvtItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.EvapotranspirationRate;
  end;
  FEvapotranspirationRateData.Initialize(BoundaryValues, ScreenObject);
  Assert(FEvapotranspirationRateData.Count = Count);
  ClearBoundaries;
  SetBoundaryCapacity(FEvapotranspirationRateData.Count);
  for TimeIndex := 0 to FEvapotranspirationRateData.Count - 1 do
  begin
    AddBoundary(TEvtStorage.Create);
  end;
  ListOfTimeLists.Add(FEvapotranspirationRateData);
end;

procedure TEvtCollection.InvalidateEtRateData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FEvapotranspirationRateData.Invalidate;
  end;
end;

class function TEvtCollection.ItemClass: TMF_BoundItemClass;
begin
  result := TEvtItem;
end;

procedure TEvtCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer);
begin
  SetLength((Boundaries[ItemIndex] as TEvtStorage).FEvtArray, BoundaryCount);
  inherited;
end;

{ TEvtParamItem }

class function TEvtParamItem.BoundaryClass: TMF_BoundCollClass;
begin
  result := TEvtCollection;
end;

{ TEvt_Cell }

procedure TEvt_Cell.Cache(Comp: TCompressionStream);
begin
  inherited;
  Values.Cache(Comp);
  WriteCompInt(Comp, StressPeriod);
end;

function TEvt_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TEvt_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TEvt_Cell.GetEvapotranspirationRate: double;
begin
  result := Values.EvapotranspirationRate;
end;

function TEvt_Cell.GetEvapotranspirationRateAnnotation: string;
begin
  result := Values.EvapotranspirationRateAnnotation;
end;

function TEvt_Cell.GetIntegerAnnotation(Index: integer): string;
begin
  result := '';
  case Index of
    RatePosition: result := 'Assigned from the cell''s layer';
    else Assert(False);
  end;
end;

function TEvt_Cell.GetIntegerValue(Index: integer): integer;
begin
  result := 0;
  case Index of
    RatePosition: result := frmGoPhast.PhastModel.LayerStructure.
         DataSetLayerToModflowLayer(Layer);
    else Assert(False);
  end;
end;

function TEvt_Cell.GetRealAnnotation(Index: integer): string;
begin
  result := '';
  case Index of
    RatePosition: result := EvapotranspirationRateAnnotation;
    else Assert(False);
  end;
end;

function TEvt_Cell.GetRealValue(Index: integer): double;
begin
  result := 0;
  case Index of
    RatePosition: result := EvapotranspirationRate;
    else Assert(False);
  end;
end;

function TEvt_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TEvt_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

procedure TEvt_Cell.Restore(Decomp: TDecompressionStream);
begin
  inherited;
  Values.Restore(Decomp); 
  StressPeriod := ReadCompInt(Decomp);
end;

{ TEvtBoundary }

procedure TEvtBoundary.Assign(Source: TPersistent);
var
  SourceBoundary: TEvtBoundary;
begin
  if Source is TEvtBoundary then
  begin
    SourceBoundary := TEvtBoundary(Source);
    EvapotranspirationLayers := SourceBoundary.EvapotranspirationLayers;
    EvtSurfDepthCollection := SourceBoundary.EvtSurfDepthCollection;
  end;
  inherited;
end;

procedure TEvtBoundary.AssignEvapotranspirationLayerCells(BoundaryStorage: TEvtLayerStorage;
  ValueTimeList: TList);
var
  Cell: TEvapotranspirationLayerCell;
  BoundaryValues: TEvtLayerRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TEvtLayerStorage;
begin
  LocalBoundaryStorage := BoundaryStorage;// as TEvtStorage;
  for TimeIndex := 0 to
    (PhastModel as TPhastModel).ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TEvapotranspirationLayerCell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := (PhastModel as TPhastModel).ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime <= LocalBoundaryStorage.EndingTime) then
    begin
      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.EvtLayerArray) - 1 do
      begin
//        Cells.Cached := False;
        BoundaryValues := LocalBoundaryStorage.EvtLayerArray[BoundaryIndex];
        Cell := TEvapotranspirationLayerCell.Create;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;


procedure TEvtBoundary.AssignSurfaceDepthCells(
  BoundaryStorage: TEvtSurfDepthStorage; ValueTimeList: TList);
var
  Cell: TEvtSurfDepth_Cell;
  BoundaryValues: TEvtSurfDepthRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TEvtSurfDepthStorage;
begin
  LocalBoundaryStorage := BoundaryStorage;
  for TimeIndex := 0 to
    (PhastModel as TPhastModel).ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TEvtSurfDepth_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := (PhastModel as TPhastModel).ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime <= LocalBoundaryStorage.EndingTime) then
    begin
      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.EvtSurfDepthArray) - 1 do
      begin
//        Cells.Cached := False;
        BoundaryValues := LocalBoundaryStorage.EvtSurfDepthArray[BoundaryIndex];
        Cell := TEvtSurfDepth_Cell.Create;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

procedure TEvtBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList);
var
  Cell: TEvt_Cell;
  BoundaryValues: TEvtRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TEvtStorage;
begin
  LocalBoundaryStorage := BoundaryStorage as TEvtStorage;
  for TimeIndex := 0 to
    (PhastModel as TPhastModel).ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TEvt_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := (PhastModel as TPhastModel).ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime <= LocalBoundaryStorage.EndingTime) then
    begin
      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.EvtArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.EvtArray[BoundaryIndex];
        Cell := TEvt_Cell.Create;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TEvtBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TEvtCollection;
end;

procedure TEvtBoundary.Clear;
begin
  inherited;
  EvapotranspirationLayers.Clear;
  FEvtSurfDepthCollection.Clear;
end;

constructor TEvtBoundary.Create(Model, ScreenObject: TObject);
begin
  inherited Create(Model, ScreenObject);
  FEvapotranspirationLayers := TEvtLayerCollection.Create(self, Model, ScreenObject);
  FEvtSurfDepthCollection := TEvtSurfDepthCollection.Create(self, Model, ScreenObject);
end;

destructor TEvtBoundary.Destroy;
begin
  FEvtSurfDepthCollection.Free;
  FEvapotranspirationLayers.Free;
  inherited;
end;

procedure TEvtBoundary.EvaluateArrayBoundaries;
begin
  inherited;
  EvtSurfDepthCollection.EvaluateArrayBoundaries;
  if (PhastModel as TPhastModel).
    ModflowPackages.EvtPackage.TimeVaryingLayers then
  begin
    EvapotranspirationLayers.EvaluateArrayBoundaries;
  end;
end;

procedure TEvtBoundary.GetEvapotranspirationLayerCells(LayerTimeList: TList);
var
  ValueIndex: Integer;
  BoundaryStorage: TEvtLayerStorage;
begin
  if not (PhastModel as TPhastModel).ModflowPackages.
    EvtPackage.TimeVaryingLayers then
  begin
    Exit;
  end;
  for ValueIndex := 0 to EvapotranspirationLayers.Count - 1 do
  begin
    BoundaryStorage := EvapotranspirationLayers.Boundaries[ValueIndex] as TEvtLayerStorage;
    AssignEvapotranspirationLayerCells(BoundaryStorage, LayerTimeList);
  end;
end;

procedure TEvtBoundary.GetEvapotranspirationSurfaceDepthCells(
  LayerTimeList: TList);
var
  ValueIndex: Integer;
  BoundaryStorage: TEvtSurfDepthStorage;
begin
  for ValueIndex := 0 to EvtSurfDepthCollection.Count - 1 do
  begin
    BoundaryStorage := EvtSurfDepthCollection.Boundaries[ValueIndex] as TEvtSurfDepthStorage;
    AssignSurfaceDepthCells(BoundaryStorage, LayerTimeList);
  end;
end;

procedure TEvtBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList);
var
  ValueIndex: Integer;
  BoundaryStorage: TEvtStorage;
  ParamIndex: Integer;
  Param: TModflowParamItem;
  Times: TList;
  Position: integer;
  ParamName: string;
begin
  EvaluateArrayBoundaries;
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    BoundaryStorage := Values.Boundaries[ValueIndex] as TEvtStorage;
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
      BoundaryStorage := Param.Param.Boundaries[ValueIndex] as TEvtStorage;
      AssignCells(BoundaryStorage, Times);
    end;
  end;
end;

function TEvtBoundary.GetTimeVaryingEvapotranspirationLayers: boolean;
begin
  if PhastModel = nil then
  begin
    result := frmGoPhast.PhastModel.ModflowPackages.
      EvtPackage.TimeVaryingLayers;
  end
  else
  begin
    result := (PhastModel as TPhastModel).ModflowPackages.
      EvtPackage.TimeVaryingLayers;
  end;
end;

procedure TEvtBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (PhastModel <> nil) then
  begin
    Model := PhastModel as TPhastModel;
    Model.InvalidateMfEvtEvapRate(self);
    Model.InvalidateMfEvtEvapSurface(self);
    Model.InvalidateMfEvtEvapDepth(self);
    Model.InvalidateMfEvtEvapLayer(self);
  end;
end;

class function TEvtBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := TEvtParamItem;
end;

function TEvtBoundary.NonParameterColumns: integer;
begin
  result := inherited NonParameterColumns + 2;
  if TimeVaryingEvapotranspirationLayers then
  begin
    result := result + EvapotranspirationLayers.TimeListCount;
  end;
end;

procedure TEvtBoundary.SetEvapotranspirationLayers(const Value: TEvtLayerCollection);
begin
  FEvapotranspirationLayers.Assign(Value);
end;

procedure TEvtBoundary.SetEvtSurfDepthCollection(
  const Value: TEvtSurfDepthCollection);
begin
  FEvtSurfDepthCollection.Assign(Value);
end;

function TEvtBoundary.Used: boolean;
var
  Model: TPhastModel;
  ParamIndex: Integer;
  Param: TModflowTransientListParameter;
begin
  if PhastModel <> nil then
  begin
    Model := PhastModel as TPhastModel;
    result := Model.ModflowPackages.EvtPackage.TimeVaryingLayers
      and EvapotranspirationLayers.Used;
  end
  else
  begin
    result := EvapotranspirationLayers.Used;
  end;
  if result then Exit;
  result := EvtSurfDepthCollection.Used;
  if result then Exit;
  result := inherited Used;
  if result and (PhastModel <> nil) then
  begin
    Model := PhastModel as TPhastModel;
    for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
    begin
      Param := Model.ModflowTransientParameters[ParamIndex];
      if Param.ParameterType = ptEVT then
      begin
        result := Parameters.Used;
        Exit;
      end;
    end;
    result := Values.Used;
  end;
end;

{ TEvtLayerItem }

procedure TEvtLayerItem.Assign(Source: TPersistent);
begin
  if Source is TEvtLayerItem then
  begin
    EvapotranspirationLayer := TEvtLayerItem(Source).EvapotranspirationLayer;
  end;
  inherited;
end;

procedure TEvtLayerItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TEvtLayerCollection;
  LayerObserver: TObserver;
begin
  ParentCollection := Collection as TEvtLayerCollection;
  LayerObserver := FObserverList[LayerPosition];
  LayerObserver.OnUpToDateSet := ParentCollection.InvalidateEtLayer;
end;

function TEvtLayerItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

procedure TEvtLayerItem.CreateFormulaObjects;
begin
  FEvapotranspirationLayer := CreateFormulaObject(dsoTop);
end;

destructor TEvtLayerItem.Destroy;
begin
  EvapotranspirationLayer := '0';
  inherited;
end;

function TEvtLayerItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    LayerPosition: result := EvapotranspirationLayer;
    else Assert(False);
  end;
end;

function TEvtLayerItem.GetEvapotranspirationLayer: string;
begin
  Result := FEvapotranspirationLayer.Formula;
  ResetItemObserver(LayerPosition);
end;

procedure TEvtLayerItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  Assert(Sender = FEvapotranspirationLayer);
  List.Add(FObserverList[LayerPosition]);
end;

function TEvtLayerItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TEvtLayerItem;
begin
  result := (AnotherItem is TEvtItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TEvtLayerItem(AnotherItem);
    result := (Item.EvapotranspirationLayer = EvapotranspirationLayer)
  end;
end;

procedure TEvtLayerItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FEvapotranspirationLayer,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
end;

procedure TEvtLayerItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    LayerPosition: EvapotranspirationLayer := Value;
    else Assert(False);
  end;
end;

procedure TEvtLayerItem.SetEvapotranspirationLayer(const Value: string);
begin
  UpdateFormula(Value, LayerPosition, FEvapotranspirationLayer);
end;

{ TEvtLayerCollection }

procedure TEvtLayerCollection.AddSpecificBoundary;
begin
  AddBoundary(TEvtLayerStorage.Create);
end;

procedure TEvtLayerCollection.AssignCellValues(DataSets: TList;
  ItemIndex: Integer);
var
  EvapotranspirationLayerArray: TDataArray;
  Boundary: TEvtLayerStorage;
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
  EvapotranspirationLayerArray := DataSets[LayerPosition];
  Boundary := Boundaries[ItemIndex] as TEvtLayerStorage;
  EvapotranspirationLayerArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
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
            if EvapotranspirationLayerArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              with Boundary.EvtLayerArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                EvapotranspirationLayer := EvapotranspirationLayerArray.
                  IntegerData[LayerIndex, RowIndex, ColIndex];
                EvapotranspirationLayerAnnotation := EvapotranspirationLayerArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  EvapotranspirationLayerArray.CacheData;
  Boundary.CacheData;
end;

constructor TEvtLayerCollection.Create(Boundary: TModflowBoundary; Model,
  ScreenObject: TObject);
begin
  inherited Create(Boundary, Model, ScreenObject);
  FEvapotranspirationLayerData := TModflowTimeList.Create(Model);
  FEvapotranspirationLayerData.NonParamDescription := 'Evapo- transpiration layer';
  FEvapotranspirationLayerData.ParamDescription := ' evapo- transpiration layer';
  FEvapotranspirationLayerData.DataType := rdtInteger;
  AddTimeList(FEvapotranspirationLayerData);
  if Model <> nil then
  begin
    FEvapotranspirationLayerData.OnInvalidate :=
      (Model as TPhastModel).InvalidateMfEvtEvapLayer;
  end;
end;

destructor TEvtLayerCollection.Destroy;
begin
  FEvapotranspirationLayerData.Free;
  inherited;
end;

procedure TEvtLayerCollection.InitializeTimeLists(ListOfTimeLists: TList);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TEvtLayerItem;
  ScreenObject: TScreenObject;
begin
  ScreenObject := BoundaryGroup.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TEvtLayerItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.EvapotranspirationLayer;
  end;
  FEvapotranspirationLayerData.Initialize(BoundaryValues, ScreenObject);
  Assert(FEvapotranspirationLayerData.Count = Count);
  ClearBoundaries;
  SetBoundaryCapacity(FEvapotranspirationLayerData.Count);
  for TimeIndex := 0 to FEvapotranspirationLayerData.Count - 1 do
  begin
    AddBoundary(TEvtLayerStorage.Create);
  end;
  ListOfTimeLists.Add(FEvapotranspirationLayerData);
end;

procedure TEvtLayerCollection.InvalidateEtLayer(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FEvapotranspirationLayerData.Invalidate;
  end;
end;

class function TEvtLayerCollection.ItemClass: TMF_BoundItemClass;
begin
  result := TEvtLayerItem;
end;

procedure TEvtLayerCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer);
begin
  SetLength((Boundaries[ItemIndex] as TEvtLayerStorage).FEvtLayerArray,
    BoundaryCount);
  inherited;
end;

{ TEvapotranspirationLayerCell }

procedure TEvapotranspirationLayerCell.Cache(Comp: TCompressionStream);
begin
  inherited;
  Values.Cache(Comp);
  WriteCompInt(Comp, StressPeriod);
end;

function TEvapotranspirationLayerCell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TEvapotranspirationLayerCell.GetIntegerAnnotation(
  Index: integer): string;
begin
  result := '';
  case Index of
    LayerPosition: result := Values.EvapotranspirationLayerAnnotation;
    else Assert(False);
  end;
end;

function TEvapotranspirationLayerCell.GetIntegerValue(Index: integer): integer;
begin
  result := 0;
  case Index of
    LayerPosition: result := Layer + 1;
    else Assert(False);
  end;
end;

function TEvapotranspirationLayerCell.GetLayer: integer;
begin
  // 1 is subtractred from EvapotranspirationLayer in order to compensate
  // for 1 being added to the layer in TModflowEVT_Writer.WriteEvapotranspirationLayer.
  result := Values.EvapotranspirationLayer-1;
end;

function TEvapotranspirationLayerCell.GetRealAnnotation(Index: integer): string;
begin
  result := '';
  Assert(False);
end;

function TEvapotranspirationLayerCell.GetRealValue(Index: integer): double;
begin
  result := 0;
  Assert(False);
end;

function TEvapotranspirationLayerCell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;
function TEvapotranspirationLayerCell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

procedure TEvapotranspirationLayerCell.Restore(Decomp: TDecompressionStream);
begin
  inherited;
  Values.Restore(Decomp);
  StressPeriod := ReadCompInt(Decomp);
end;

{ TEvtSurfDepthItem }

procedure TEvtSurfDepthItem.Assign(Source: TPersistent);
var
  SourceItem: TEvtSurfDepthItem;
begin
  if Source is TEvtSurfDepthItem then
  begin
    SourceItem := TEvtSurfDepthItem(Source);
    EvapotranspirationSurface := SourceItem.EvapotranspirationSurface;
    EvapotranspirationDepth := SourceItem.EvapotranspirationDepth;
  end;
  inherited;
end;

procedure TEvtSurfDepthItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TEvtSurfDepthCollection;
  SurfaceObserver: TObserver;
  DepthObserver: TObserver;
begin
  ParentCollection := Collection as TEvtSurfDepthCollection;
  SurfaceObserver := FObserverList[SurfacePosition];
  SurfaceObserver.OnUpToDateSet := ParentCollection.InvalidateSurfaceData;
  DepthObserver := FObserverList[DepthPosition];
  DepthObserver.OnUpToDateSet := ParentCollection.InvalidateDepthData;
end;

function TEvtSurfDepthItem.BoundaryFormulaCount: integer;
begin
  result := 2;
end;

procedure TEvtSurfDepthItem.CreateFormulaObjects;
begin
  inherited;
  FEvapotranspirationSurface := CreateFormulaObject(dsoTop);
  FEvapotranspirationDepth := CreateFormulaObject(dsoTop);
end;

destructor TEvtSurfDepthItem.Destroy;
begin
  EvapotranspirationSurface := '0';
  EvapotranspirationDepth := '0';
  inherited;
end;

function TEvtSurfDepthItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    SurfacePosition: result := EvapotranspirationSurface;
    DepthPosition: result := EvapotranspirationDepth;
    else Assert(False);
  end;
end;

function TEvtSurfDepthItem.GetEvapotranspirationDepth: string;
begin
  Result := FEvapotranspirationDepth.Formula;
  ResetItemObserver(DepthPosition);
end;

function TEvtSurfDepthItem.GetEvapotranspirationSurface: string;
begin
  Result := FEvapotranspirationSurface.Formula;
  ResetItemObserver(SurfacePosition);
end;

procedure TEvtSurfDepthItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FEvapotranspirationSurface then
  begin
    List.Add( FObserverList[SurfacePosition]);
  end;
  if Sender = FEvapotranspirationDepth then
  begin
    List.Add( FObserverList[DepthPosition]);
  end;
end;

function TEvtSurfDepthItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TEvtSurfDepthItem;
begin
  result := (AnotherItem is TEvtSurfDepthItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TEvtSurfDepthItem(AnotherItem);
    result :=
      (Item.EvapotranspirationSurface = EvapotranspirationSurface)
      and (Item.EvapotranspirationDepth = EvapotranspirationDepth)
  end;
end;

procedure TEvtSurfDepthItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FEvapotranspirationDepth,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FEvapotranspirationSurface,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
end;

procedure TEvtSurfDepthItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    SurfacePosition: EvapotranspirationSurface := Value;
    DepthPosition: EvapotranspirationDepth := Value;
    else Assert(False);
  end;
end;

procedure TEvtSurfDepthItem.SetEvapotranspirationDepth(const Value: string);
begin
  UpdateFormula(Value, DepthPosition, FEvapotranspirationDepth);
end;

procedure TEvtSurfDepthItem.SetEvapotranspirationSurface(const Value: string);
begin
  UpdateFormula(Value, SurfacePosition, FEvapotranspirationSurface);
end;

{ TEvtSurfDepthCollection }

procedure TEvtSurfDepthCollection.AddSpecificBoundary;
begin
  AddBoundary(TEvtSurfDepthStorage.Create);
end;

procedure TEvtSurfDepthCollection.AssignCellValues(DataSets: TList;
  ItemIndex: Integer);
var
  EvapotranspirationSurfaceArray: TDataArray;
  EvapotranspirationDepthArray: TDataArray;
  Boundary: TEvtSurfDepthStorage;
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
  EvapotranspirationSurfaceArray := DataSets[SurfacePosition];
  EvapotranspirationDepthArray := DataSets[DepthPosition];
  Boundary := Boundaries[ItemIndex] as TEvtSurfDepthStorage;
  EvapotranspirationSurfaceArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
    LayerMax, RowMax, ColMax);
  if LayerMin >= 0 then
  begin
    for LayerIndex := 0 to EvapotranspirationSurfaceArray.LayerCount - 1 do
    begin
      if LocalModel.LayerStructure.IsLayerSimulated(LayerIndex) then
      begin
        for RowIndex := 0 to EvapotranspirationSurfaceArray.RowCount - 1 do
        begin
          for ColIndex := 0 to EvapotranspirationSurfaceArray.ColumnCount - 1 do
          begin
            if EvapotranspirationSurfaceArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              Assert(EvapotranspirationDepthArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              with Boundary.EvtSurfDepthArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                EvapotranspirationSurface := EvapotranspirationSurfaceArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                EvapotranspirationSurfaceAnnotation := EvapotranspirationSurfaceArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                EvapotranspirationDepth := EvapotranspirationDepthArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                EvapotranspirationDepthAnnotation := EvapotranspirationDepthArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  EvapotranspirationSurfaceArray.CacheData;
  EvapotranspirationDepthArray.CacheData;
  Boundary.CacheData;
end;

constructor TEvtSurfDepthCollection.Create(Boundary: TModflowBoundary; Model,
  ScreenObject: TObject);
begin
  inherited Create(Boundary, Model, ScreenObject);
  FEvapotranspirationSurfaceData := TModflowTimeList.Create(Model);
  FEvapotranspirationSurfaceData.NonParamDescription := 'Evapo- transpiration surface';
  FEvapotranspirationSurfaceData.ParamDescription := ' evapo- transpiration surface';
  AddTimeList(FEvapotranspirationSurfaceData);

  FEvapotranspirationDepthData := TModflowTimeList.Create(Model);
  FEvapotranspirationDepthData.NonParamDescription := 'Evapo- transpiration depth';
  FEvapotranspirationDepthData.ParamDescription := ' evapo- transpiration depth';
  AddTimeList(FEvapotranspirationDepthData);

  if Model <> nil then
  begin
    FEvapotranspirationSurfaceData.OnInvalidate := (Model as TPhastModel).InvalidateMfEvtEvapSurface;
    FEvapotranspirationDepthData.OnInvalidate := (Model as TPhastModel).InvalidateMfEvtEvapDepth;
  end;
end;

destructor TEvtSurfDepthCollection.Destroy;
begin
  FEvapotranspirationDepthData.Free;
  FEvapotranspirationSurfaceData.Free;
  inherited;
end;

procedure TEvtSurfDepthCollection.InitializeTimeLists(ListOfTimeLists: TList);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TEvtSurfDepthItem;
  Boundary: TEvtBoundary;
  ScreenObject: TScreenObject;
begin
  Boundary := BoundaryGroup as TEvtBoundary;
  ScreenObject := Boundary.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TEvtSurfDepthItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.EvapotranspirationSurface;
  end;
  FEvapotranspirationSurfaceData.Initialize(BoundaryValues, ScreenObject);
  Assert(FEvapotranspirationSurfaceData.Count = Count);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TEvtSurfDepthItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.EvapotranspirationDepth;
  end;
  FEvapotranspirationDepthData.Initialize(BoundaryValues, ScreenObject);
  Assert(FEvapotranspirationDepthData.Count = Count);

  ClearBoundaries;
  SetBoundaryCapacity(FEvapotranspirationSurfaceData.Count);
  for TimeIndex := 0 to FEvapotranspirationSurfaceData.Count - 1 do
  begin
    AddBoundary(TEvtSurfDepthStorage.Create);
  end;
  ListOfTimeLists.Add(FEvapotranspirationSurfaceData);
  ListOfTimeLists.Add(FEvapotranspirationDepthData);
end;

procedure TEvtSurfDepthCollection.InvalidateDepthData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FEvapotranspirationDepthData.Invalidate;
  end;
end;

procedure TEvtSurfDepthCollection.InvalidateSurfaceData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FEvapotranspirationSurfaceData.Invalidate;
  end;
end;

class function TEvtSurfDepthCollection.ItemClass: TMF_BoundItemClass;
begin
  result := TEvtSurfDepthItem;
end;

procedure TEvtSurfDepthCollection.SetBoundaryStartAndEndTime(
  BoundaryCount: Integer; Item: TCustomModflowBoundaryItem; ItemIndex: Integer);
begin
  SetLength((Boundaries[ItemIndex] as TEvtSurfDepthStorage).FEvtSurfDepthArray,
    BoundaryCount);
  inherited;

end;

{ TEvtSurfDepth_Cell }

procedure TEvtSurfDepth_Cell.Cache(Comp: TCompressionStream);
begin
  inherited;
  Values.Cache(Comp);
  WriteCompInt(Comp, StressPeriod);
end;

function TEvtSurfDepth_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TEvtSurfDepth_Cell.GetEvapotranspirationDepth: double;
begin
  result := Values.EvapotranspirationDepth;
end;

function TEvtSurfDepth_Cell.GetEvapotranspirationDepthAnnotation: string;
begin
  result := Values.EvapotranspirationDepthAnnotation;
end;

function TEvtSurfDepth_Cell.GetEvapotranspirationSurface: double;
begin
  result := Values.EvapotranspirationSurface;
end;

function TEvtSurfDepth_Cell.GetEvapotranspirationSurfaceAnnotation: string;
begin
  result := Values.EvapotranspirationSurfaceAnnotation;
end;

function TEvtSurfDepth_Cell.GetIntegerAnnotation(Index: integer): string;
begin
  result := '';
  Assert(False);
end;

function TEvtSurfDepth_Cell.GetIntegerValue(Index: integer): integer;
begin
  result := 0;
  Assert(False);
end;

function TEvtSurfDepth_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TEvtSurfDepth_Cell.GetRealAnnotation(Index: integer): string;
begin
  result := '';
  case Index of
    SurfacePosition: result := EvapotranspirationSurfaceAnnotation;
    DepthPosition: result := EvapotranspirationDepthAnnotation;
    else Assert(False);
  end;
end;

function TEvtSurfDepth_Cell.GetRealValue(Index: integer): double;
begin
  result := 0;
  case Index of
    SurfacePosition: result := EvapotranspirationSurface;
    DepthPosition: result := EvapotranspirationDepth;
    else Assert(False);
  end;
end;

function TEvtSurfDepth_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TEvtSurfDepth_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

procedure TEvtSurfDepth_Cell.Restore(Decomp: TDecompressionStream);
begin
  inherited;
  Values.Restore(Decomp);
  StressPeriod := ReadCompInt(Decomp);
end;

{ TEvtStorage }

procedure TEvtStorage.Clear;
begin
  SetLength(FEvtArray, 0);
  FCleared := True;
end;

procedure TEvtStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
begin
  Count := Length(FEvtArray);
  Compressor.Write(Count, SizeOf(Count));
  for Index := 0 to Count - 1 do
  begin
    FEvtArray[Index].Cache(Compressor);
  end;
end;

procedure TEvtStorage.Restore(DecompressionStream: TDecompressionStream);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FEvtArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FEvtArray[Index].Restore(DecompressionStream);
  end;
end;

function TEvtStorage.GetEvtArray: TEvtArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FEvtArray;
end;

{ TEvtLayerStorage }

procedure TEvtLayerStorage.Clear;
begin
  SetLength(FEvtLayerArray, 0);
  FCleared := True;
end;

procedure TEvtLayerStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
begin
  Count := Length(FEvtLayerArray);
  Compressor.Write(Count, SizeOf(Count));
  for Index := 0 to Count - 1 do
  begin
    FEvtLayerArray[Index].Cache(Compressor);
  end;
end;

procedure TEvtLayerStorage.Restore(DecompressionStream: TDecompressionStream);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FEvtLayerArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FEvtLayerArray[Index].Restore(DecompressionStream);
  end;
end;

function TEvtLayerStorage.GetEvtLayerArray: TEvtLayerArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FEvtLayerArray;
end;

{ TEvtSurfDepthStorage }

procedure TEvtSurfDepthStorage.Clear;
begin
  SetLength(FEvtSurfDepthArray, 0);
  FCleared := True;
end;

procedure TEvtSurfDepthStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
begin
  Count := Length(FEvtSurfDepthArray);
  Compressor.Write(Count, SizeOf(Count));
  for Index := 0 to Count - 1 do
  begin
    FEvtSurfDepthArray[Index].Cache(Compressor);
  end;
end;

procedure TEvtSurfDepthStorage.Restore(DecompressionStream: TDecompressionStream);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FEvtSurfDepthArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FEvtSurfDepthArray[Index].Restore(DecompressionStream);
  end;
end;

function TEvtSurfDepthStorage.GetEvtSurfDepthArray: TEvtSurfDepthArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FEvtSurfDepthArray;
end;

{ TEvtRecord }

procedure TEvtRecord.Cache(Comp: TCompressionStream);
var
  CommentLength: Integer;
begin
  Comp.Write(Cell, SizeOf(Cell));
  Comp.Write(EvapotranspirationRate, SizeOf(EvapotranspirationRate));
  Comp.Write(StartingTime, SizeOf(StartingTime));
  Comp.Write(EndingTime, SizeOf(EndingTime));

  CommentLength := Length(EvapotranspirationRateAnnotation);
  Comp.Write(CommentLength, SizeOf(CommentLength));
  Comp.WriteBuffer(Pointer(EvapotranspirationRateAnnotation)^,
    CommentLength * SizeOf(Char));
end;

procedure TEvtRecord.Restore(Decomp: TDecompressionStream);
var
  CommentLength: Integer;
  Comment: string;
begin
  Decomp.Read(Cell, SizeOf(Cell));
  Decomp.Read(EvapotranspirationRate, SizeOf(EvapotranspirationRate));
  Decomp.Read(StartingTime, SizeOf(StartingTime));
  Decomp.Read(EndingTime, SizeOf(EndingTime));
  Decomp.Read(CommentLength, SizeOf(CommentLength));
  SetString(Comment, nil, CommentLength);
  Decomp.Read(Pointer(Comment)^, CommentLength * SizeOf(Char));
  EvapotranspirationRateAnnotation := Comment;
end;

{ TEvtSurfDepthRecord }

procedure TEvtSurfDepthRecord.Cache(Comp: TCompressionStream);
var
  CommentLength: Integer;
begin
  Comp.Write(Cell, SizeOf(Cell));
  Comp.Write(EvapotranspirationSurface, SizeOf(EvapotranspirationSurface));
  Comp.Write(EvapotranspirationDepth, SizeOf(EvapotranspirationDepth));
  Comp.Write(StartingTime, SizeOf(StartingTime));
  Comp.Write(EndingTime, SizeOf(EndingTime));

  CommentLength := Length(EvapotranspirationSurfaceAnnotation);
  Comp.Write(CommentLength, SizeOf(CommentLength));
  Comp.WriteBuffer(Pointer(EvapotranspirationSurfaceAnnotation)^,
    CommentLength * SizeOf(Char));

  CommentLength := Length(EvapotranspirationDepthAnnotation);
  Comp.Write(CommentLength, SizeOf(CommentLength));
  Comp.WriteBuffer(Pointer(EvapotranspirationDepthAnnotation)^,
    CommentLength * SizeOf(Char));
end;

procedure TEvtSurfDepthRecord.Restore(Decomp: TDecompressionStream);
var
  CommentLength: Integer;
  Comment: string;
begin
  Decomp.Read(Cell, SizeOf(Cell));
  Decomp.Read(EvapotranspirationSurface, SizeOf(EvapotranspirationSurface));
  Decomp.Read(EvapotranspirationDepth, SizeOf(EvapotranspirationDepth));
  Decomp.Read(StartingTime, SizeOf(StartingTime));
  Decomp.Read(EndingTime, SizeOf(EndingTime));

  Decomp.Read(CommentLength, SizeOf(CommentLength));
  SetString(Comment, nil, CommentLength);
  Decomp.Read(Pointer(Comment)^, CommentLength * SizeOf(Char));
  EvapotranspirationSurfaceAnnotation := Comment;
  
  Decomp.Read(CommentLength, SizeOf(CommentLength));
  SetString(Comment, nil, CommentLength);
  Decomp.Read(Pointer(Comment)^, CommentLength * SizeOf(Char));
  EvapotranspirationDepthAnnotation := Comment;
end;

{ TEvtLayerRecord }

procedure TEvtLayerRecord.Cache(Comp: TCompressionStream);
var
  CommentLength: Integer;
begin

  Comp.Write(Cell, SizeOf(Cell));
  Comp.Write(EvapotranspirationLayer, SizeOf(EvapotranspirationLayer));
  Comp.Write(StartingTime, SizeOf(StartingTime));
  Comp.Write(EndingTime, SizeOf(EndingTime));

  CommentLength := Length(EvapotranspirationLayerAnnotation);
  Comp.Write(CommentLength, SizeOf(CommentLength));
  Comp.WriteBuffer(Pointer(EvapotranspirationLayerAnnotation)^,
    CommentLength * SizeOf(Char));
end;

procedure TEvtLayerRecord.Restore(Decomp: TDecompressionStream);
var
  CommentLength: Integer;
  Comment: string;
begin
  Decomp.Read(Cell, SizeOf(Cell));
  Decomp.Read(EvapotranspirationLayer, SizeOf(EvapotranspirationLayer));
  Decomp.Read(StartingTime, SizeOf(StartingTime));
  Decomp.Read(EndingTime, SizeOf(EndingTime));
  Decomp.Read(CommentLength, SizeOf(CommentLength));
  SetString(Comment, nil, CommentLength);
  Decomp.Read(Pointer(Comment)^, CommentLength * SizeOf(Char));
  EvapotranspirationLayerAnnotation := Comment;
end;

end.
