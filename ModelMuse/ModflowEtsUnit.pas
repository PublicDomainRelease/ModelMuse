unit ModflowEtsUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit, ModflowEvtUnit,
  FormulaManagerUnit, SubscriptionUnit, RbwParser, SparseDataSets;

type

  TEtsSurfDepthRecord = record
    Cell: TCellLocation;
    EvapotranspirationSurface: double;
    EvapotranspirationDepth: double;
    DepthFractions: array of double;
    EtFractions: array of double;
    StartingTime: double;
    EndingTime: double;
    EvapotranspirationSurfaceAnnotation: string;
    EvapotranspirationDepthAnnotation: string;
    DepthFractionAnnotations: array of string;
    EtFractionAnnotations: array of string;
    procedure Assign(const Item: TEtsSurfDepthRecord);
    procedure Cache(Comp: TCompressionStream);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList); 
  end;

  // @name is an array of @link(TEtsSurfDepthRecord)s.
  TEtsSurfDepthArray = array of TEtsSurfDepthRecord;

  TEtsSurfDepthStorage = class(TCustomBoundaryStorage)
  private
    FEtsSurfDepthArray: TEtsSurfDepthArray;
    function GetEtsSurfDepthArray: TEtsSurfDepthArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property EtsSurfDepthArray: TEtsSurfDepthArray read GetEtsSurfDepthArray;
  end;

  TStringCollection = class;

  TStringValueItem = class(TOrderedItem)
  private
    FValue: TFormulaObject;
    Observer: TObserver;
    procedure SetValue(const Value: string);
    function StringCollection: TStringCollection;
    function GetValue: string;
    procedure RemoveSubscription(Sender: TObject; const AName: string);
    procedure RestoreSubscription(Sender: TObject; const AName: string);
    procedure UpdateFormula(Value: string;
      var FormulaObject: TFormulaObject);
    procedure UpdateFormulaDependencies(OldFormula: string; var
      NewFormula: string; Observer: TObserver; Compiler: TRbwParser);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Value: string read GetValue write SetValue;
  end;

  TStringCollectionPurpose = (scpDepthFractions, scpEtFractions);
  TEtsSurfDepthCollection = class;

  TStringCollection = class(TOrderedCollection)
  private
    FPurpose: TStringCollectionPurpose;
    FScreenObject: TObject;
    FEtsSurfDepthCollection: TCollection;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Model: TObject; ScreenObject: TObject;
      EtsSurfDepthCollection: TCollection);
    property Purpose: TStringCollectionPurpose read FPurpose write FPurpose;
  end;

  // @name represents a MODFLOW ET layer for one time interval.
  // @name is stored by @link(TEtsSurfDepthCollection).
  TEtsSurfDepthItem = class(TCustomModflowBoundaryItem)
  private
    FEvapotranspirationSurface: TFormulaObject;
    FEvapotranspirationDepth: TFormulaObject;
    FDepthFractions: TStringCollection;
    FEtFractions: TStringCollection;
    // See @link(EvapotranspirationSurface).
    procedure SetEvapotranspirationSurface(const Value: string);
    procedure SetEvapotranspirationDepth(const Value: string);
    procedure SetDepthFractions(const Value: TStringCollection);
    procedure SetEtFractions(const Value: TStringCollection);
    function GetEvapotranspirationDepth: string;
    function GetEvapotranspirationSurface: string;
  protected
    procedure RemoveFormulaObjects; override;
    procedure CreateFormulaObjects; override;
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    // See @link(TCustomModflowBoundaryItem.BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(TCustomModflowBoundaryItem.BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    // @name checks whether AnotherItem is the same as the current @classname.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function BoundaryFormulaCount: integer; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    // @name is the formula used to set the recharge rate
    // or the recharge rate multiplier of this boundary.
    property EvapotranspirationSurface: string read GetEvapotranspirationSurface
      write SetEvapotranspirationSurface;
    property EvapotranspirationDepth: string read GetEvapotranspirationDepth
      write SetEvapotranspirationDepth;
    property EtFractions: TStringCollection read FEtFractions
      write SetEtFractions;
    property DepthFractions: TStringCollection read FDepthFractions
      write SetDepthFractions;
  end;

  TEtsSurfDepthCollection = class(TCustomMF_ArrayBoundColl)
  private
    FTimeListCount: integer;
    FListOfEtFractionLists: TList;
    FListOfDepthFractionLists: TList;
    // @name is used to compute the evapotranspiration rates for a series of
    // cells over a series of time intervals.
    FEvapotranspirationSurfaceData: TModflowTimeList;
    FEvapotranspirationDepthData: TModflowTimeList;
    procedure InvalidateEtFractions(Sender: TObject);
    procedure InvalidateDepthFractions(Sender: TObject);
    procedure InvalidateEtSurface(Sender: TObject);
    procedure InvalidateEtDepth(Sender: TObject);
  protected
    procedure AddSpecificBoundary; override;
    function GetTimeList(Index: integer): TModflowTimeList; override;
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
    // the @link(TEtsSurfDepthStorage.EtsSurfDepthArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // It also sets the length of each member of  to
    // @link(TEtsPackageSelection.SegmentCount
    // TPhastModel.ModflowPackages.EtsPackage.SegmentCount).
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer); override;
  public
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname
    constructor Create(Boundary: TModflowBoundary; Model,
      ScreenObject: TObject); override;
    // @name destroys the current instance of @classname.
    // Do not call @name; call Free instead.
    destructor Destroy; override;
    function TimeListCount: integer; override;
  end;

  TEtsLayerCollection = class(TEvtLayerCollection)
  public
    constructor Create(Boundary: TModflowBoundary; Model,
      ScreenObject: TObject); override;
  end;

  TEtsSurfDepth_Cell = class(TValueCell)
  private
    Values: TEtsSurfDepthRecord;
    StressPeriod: integer;
    function GetEvapotranspirationSurface: double;
    function GetEvapotranspirationDepth: double;
    function GetDepthFractions(const Index: integer): double;
    function GetEtFractions(const Index: integer): double;
    function GetDepthFractionAnnotations(const Index: integer): string;
    function GetEtFractionAnnotations(const Index: integer): string;
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
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList); override;
    function GetSection: integer; override;
  public
    property EvapotranspirationSurface: double read GetEvapotranspirationSurface;
    property EvapotranspirationDepth: double read GetEvapotranspirationDepth;
    property DepthFractions[const Index: integer]: double read GetDepthFractions;
    property EtFractions[const Index: integer]: double read GetEtFractions;
    property EvapotranspirationSurfaceAnnotation: string read GetEvapotranspirationSurfaceAnnotation;
    property EvapotranspirationDepthAnnotation: string read GetEvapotranspirationDepthAnnotation;
    property DepthFractionAnnotations[const Index: integer]: string read GetDepthFractionAnnotations;
    property EtFractionAnnotations[const Index: integer]: string read GetEtFractionAnnotations;
  end;

  TEtsCollection = class(TEvtCollection)
  public
    constructor Create(Boundary: TModflowBoundary; Model,
      ScreenObject: TObject); override;
  end;

  // @name represents the MODFLOW Evapotranspiration boundaries associated with
  // a single @link(TScreenObject).
  //
  // @seealso(TEvtCollection)
  // @seealso(TEtsSurfDepthCollection)
  TEtsBoundary = class(TModflowParamBoundary)
  private
    FEvapotranspirationLayers: TEtsLayerCollection;
    FEvtSurfDepthCollection: TEtsSurfDepthCollection;
    FNonParameterColumns: integer;
    procedure SetEvapotranspirationLayers(const Value: TEtsLayerCollection);
    procedure SetEvtSurfDepthCollection(const Value: TEtsSurfDepthCollection);
    function GetTimeVaryingEvapotranspirationLayers: boolean;
    procedure AssignEvapotranspirationLayerCells(BoundaryStorage: TEvtLayerStorage;
      ValueTimeList: TList);
    procedure AssignSurfaceDepthCells(BoundaryStorage: TEtsSurfDepthStorage;
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
    procedure InvalidateDisplay; override;
    procedure Clear; override;
  published
    property EvapotranspirationLayers: TEtsLayerCollection
      read FEvapotranspirationLayers write SetEvapotranspirationLayers;
    property EtsSurfDepthCollection: TEtsSurfDepthCollection
      read FEvtSurfDepthCollection write SetEvtSurfDepthCollection;
  end;

procedure StringValueRemoveSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

procedure StringValueRestoreSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

implementation

uses ScreenObjectUnit, PhastModelUnit, ModflowTimeUnit, TempFiles, GoPhastTypes,
  ModflowTransientListParameterUnit, frmGoPhastUnit;

const
  SurfacePosition = 0;
  DepthPosition = 1;

{ TEtsBoundary }

procedure TEtsBoundary.Assign(Source: TPersistent);
var
  SourceBoundary: TEtsBoundary;
begin
  if Source is TEtsBoundary then
  begin
    SourceBoundary := TEtsBoundary(Source);
    EvapotranspirationLayers := SourceBoundary.EvapotranspirationLayers;
//    TimeVaryingEvapotranspirationLayers := SourceBoundary.TimeVaryingEvapotranspirationLayers;
    EtsSurfDepthCollection := SourceBoundary.EtsSurfDepthCollection;
    FNonParameterColumns := SourceBoundary.NonParameterColumns;
  end;
  inherited;
end;

procedure TEtsBoundary.AssignEvapotranspirationLayerCells(BoundaryStorage: TEvtLayerStorage;
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
//      Cells.CheckRestore;
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

procedure TEtsBoundary.AssignSurfaceDepthCells(
  BoundaryStorage: TEtsSurfDepthStorage; ValueTimeList: TList);
var
  Cell: TEtsSurfDepth_Cell;
  BoundaryValues: TEtsSurfDepthRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TEtsSurfDepthStorage;
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
      Cells := TValueCellList.Create(TEtsSurfDepth_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := (PhastModel as TPhastModel).ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime <= LocalBoundaryStorage.EndingTime) then
    begin
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.EtsSurfDepthArray) - 1 do
      begin
//        Cells.Cached := False;
        BoundaryValues := LocalBoundaryStorage.EtsSurfDepthArray[BoundaryIndex];
        Cell := TEtsSurfDepth_Cell.Create;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values.Assign(BoundaryValues);
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

procedure TEtsBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
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
//      Cells.CheckRestore;
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
//      Cells.CheckRestore;
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

class function TEtsBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TEtsCollection;
end;

procedure TEtsBoundary.Clear;
begin
  inherited;
  EvapotranspirationLayers.Clear;
  EtsSurfDepthCollection.Clear;
end;

constructor TEtsBoundary.Create(Model, ScreenObject: TObject);
begin
  inherited Create(Model, ScreenObject);
  FEvapotranspirationLayers := TEtsLayerCollection.Create(self, Model, ScreenObject);
  FEvtSurfDepthCollection := TEtsSurfDepthCollection.Create(self, Model, ScreenObject);
end;

destructor TEtsBoundary.Destroy;
begin
  FEvtSurfDepthCollection.Free;
  FEvapotranspirationLayers.Free;
  inherited;
end;

procedure TEtsBoundary.EvaluateArrayBoundaries;
begin
  inherited;
  EtsSurfDepthCollection.EvaluateArrayBoundaries;
  if (PhastModel as TPhastModel).
    ModflowPackages.EtsPackage.TimeVaryingLayers then
  begin
    EvapotranspirationLayers.EvaluateArrayBoundaries;
  end;
end;

procedure TEtsBoundary.GetEvapotranspirationLayerCells(LayerTimeList: TList);
var
  ValueIndex: Integer;
  BoundaryStorage: TEvtLayerStorage;
begin
  if not (PhastModel as TPhastModel).ModflowPackages.
    EtsPackage.TimeVaryingLayers then
  begin
    Exit;
  end;
  for ValueIndex := 0 to EvapotranspirationLayers.Count - 1 do
  begin
    BoundaryStorage := EvapotranspirationLayers.Boundaries[ValueIndex] as TEvtLayerStorage;
    AssignEvapotranspirationLayerCells(BoundaryStorage, LayerTimeList);
  end;
end;

procedure TEtsBoundary.GetEvapotranspirationSurfaceDepthCells(
  LayerTimeList: TList);
var
  ValueIndex: Integer;
  BoundaryStorage: TEtsSurfDepthStorage;
begin
  for ValueIndex := 0 to EtsSurfDepthCollection.Count - 1 do
  begin
    BoundaryStorage := EtsSurfDepthCollection.Boundaries[ValueIndex] as TEtsSurfDepthStorage;
    AssignSurfaceDepthCells(BoundaryStorage, LayerTimeList);
  end;
end;

procedure TEtsBoundary.GetCellValues(ValueTimeList: TList;
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

function TEtsBoundary.GetTimeVaryingEvapotranspirationLayers: boolean;
begin
  if PhastModel = nil then
  begin
    result := frmGoPhast.PhastModel.ModflowPackages.
      EtsPackage.TimeVaryingLayers;
  end
  else
  begin
    result := (PhastModel as TPhastModel).ModflowPackages.
      EtsPackage.TimeVaryingLayers;
  end;
end;

procedure TEtsBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (PhastModel <> nil) then
  begin
    Model := PhastModel as TPhastModel;
    Model.InvalidateMfEtsEvapRate(self);
    Model.InvalidateMfEtsEvapSurface(self);
    Model.InvalidateMfEtsEvapDepth(self);
    Model.InvalidateMfEtsEvapLayer(self);
    Model.InvalidateEtsDepthFractions(self);
    Model.InvalidateEtsRateFractions(self);
  end;
end;

class function TEtsBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := TEvtParamItem;
end;

function TEtsBoundary.NonParameterColumns: integer;
begin
  if PhastModel = nil then
  begin
    result := inherited NonParameterColumns + 2
      + (frmGoPhast.PhastModel.ModflowPackages.EtsPackage.SegmentCount-1) * 2;
    if TimeVaryingEvapotranspirationLayers then
    begin
      result := result + EvapotranspirationLayers.TimeListCount;
    end;
  end
  else
  begin
    result := inherited NonParameterColumns + 2
      + ((PhastModel as TPhastModel).ModflowPackages.EtsPackage.SegmentCount-1) * 2;
    if TimeVaryingEvapotranspirationLayers then
    begin
      result := result + EvapotranspirationLayers.TimeListCount;
    end;
  end;
end;

procedure TEtsBoundary.SetEvapotranspirationLayers(const Value: TEtsLayerCollection);
begin
  FEvapotranspirationLayers.Assign(Value);
end;

procedure TEtsBoundary.SetEvtSurfDepthCollection(
  const Value: TEtsSurfDepthCollection);
begin
  FEvtSurfDepthCollection.Assign(Value);
end;


function TEtsBoundary.Used: boolean;
var
  Model: TPhastModel;
  ParamIndex: Integer;
  Param: TModflowTransientListParameter;
begin
  if PhastModel <> nil then
  begin
    Model := PhastModel as TPhastModel;
    result := Model.ModflowPackages.EtsPackage.TimeVaryingLayers
      and EvapotranspirationLayers.Used;
  end
  else
  begin
    result := EvapotranspirationLayers.Used;
  end;
  if result then Exit;
  result := EtsSurfDepthCollection.Used;
  if result then Exit;
  result := inherited Used;
  if result and (PhastModel <> nil) then
  begin
    Model := PhastModel as TPhastModel;
    for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
    begin
      Param := Model.ModflowTransientParameters[ParamIndex];
      if Param.ParameterType = ptETS then
      begin
        result := Parameters.Used;
        Exit;
      end;
    end;
    result := Values.Used;
  end;
end;

{ TEtsSurfDepthItem }

procedure TEtsSurfDepthItem.Assign(Source: TPersistent);
var
  SourceItem: TEtsSurfDepthItem;
begin
  if Source is TEtsSurfDepthItem then
  begin
    SourceItem := TEtsSurfDepthItem(Source);
    EvapotranspirationSurface := SourceItem.EvapotranspirationSurface;
    EvapotranspirationDepth := SourceItem.EvapotranspirationDepth;
    EtFractions := SourceItem.EtFractions;
    DepthFractions := SourceItem.DepthFractions;
  end;
  inherited;
end;

procedure TEtsSurfDepthItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TEtsSurfDepthCollection;
  SurfaceObserver: TObserver;
  DepthObserver: TObserver;
begin
  ParentCollection := Collection as TEtsSurfDepthCollection;
  SurfaceObserver := FObserverList[SurfacePosition];
  SurfaceObserver.OnUpToDateSet := ParentCollection.InvalidateEtSurface;
  DepthObserver := FObserverList[DepthPosition];
  DepthObserver.OnUpToDateSet := ParentCollection.InvalidateEtDepth;
end;

function TEtsSurfDepthItem.BoundaryFormulaCount: integer;
begin
  result := 2;
  if (EtFractions <> nil) and (DepthFractions <> nil) then
  begin
    result := result + EtFractions.Count + DepthFractions.Count;
  end;

end;

constructor TEtsSurfDepthItem.Create(Collection: TCollection);
var
  Model: TObject;
begin
  inherited;
  Model := (Collection as TOrderedCollection).Model;
  FEtFractions := TStringCollection.Create(Model, ScreenObject, Collection);
  FEtFractions.Purpose := scpEtFractions;
  FDepthFractions := TStringCollection.Create(Model, ScreenObject, Collection);
  FDepthFractions.Purpose := scpDepthFractions;
end;

procedure TEtsSurfDepthItem.CreateFormulaObjects;
begin
  FEvapotranspirationSurface := CreateFormulaObject(dsoTop);
  FEvapotranspirationDepth := CreateFormulaObject(dsoTop);
end;

destructor TEtsSurfDepthItem.Destroy;
begin
  EvapotranspirationSurface := '0';
  EvapotranspirationDepth := '0';
  FEtFractions.Free;
  FDepthFractions.Free;
  inherited;
end;

function TEtsSurfDepthItem.GetBoundaryFormula(Index: integer): string;
var
  Item: TStringValueItem;
  Collection: TStringCollection;
begin
  case Index of
    SurfacePosition: result := EvapotranspirationSurface;
    DepthPosition: result := EvapotranspirationDepth;
    else
      begin
        Dec(Index, 2);
        if Odd(Index) then
        begin
          Collection := EtFractions;
        end
        else
        begin
          Collection := DepthFractions;
        end;
        Index := Index div 2;
        while Collection.Count <= Index do
        begin
          Collection.Add;
        end;
        Item := Collection.Items[Index] as TStringValueItem;
        result := Item.Value;
      end;
  end;
end;

function TEtsSurfDepthItem.GetEvapotranspirationDepth: string;
begin
  Result := FEvapotranspirationDepth.Formula;
  ResetItemObserver(DepthPosition);
end;

function TEtsSurfDepthItem.GetEvapotranspirationSurface: string;
begin
  Result := FEvapotranspirationSurface.Formula;
  ResetItemObserver(SurfacePosition);
end;

procedure TEtsSurfDepthItem.GetPropertyObserver(Sender: TObject; List: TList);
var
  Index: integer;
  Item: TStringValueItem;
begin
  if Sender = FEvapotranspirationSurface then
  begin
    List.Add(FObserverList[SurfacePosition]);
  end;
  if Sender = FEvapotranspirationDepth then
  begin
    List.Add(FObserverList[DepthPosition]);
  end;

  for Index := 0 to EtFractions.Count - 1 do
  begin
    Item := EtFractions.Items[Index] as TStringValueItem;
    if Item.FValue = Sender then
    begin
      List.Add(Item.Observer);
    end;
  end;
  for Index := 0 to DepthFractions.Count - 1 do
  begin
    Item := DepthFractions.Items[Index] as TStringValueItem;
    if Item.FValue = Sender then
    begin
      List.Add(Item.Observer);
    end;
  end;
end;

function TEtsSurfDepthItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TEtsSurfDepthItem;
begin
  result := (AnotherItem is TEtsSurfDepthItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TEtsSurfDepthItem(AnotherItem);
    result :=
      (Item.EvapotranspirationSurface = EvapotranspirationSurface)
      and (Item.EvapotranspirationDepth = EvapotranspirationDepth)
      and Item.EtFractions.IsSame(EtFractions)
      and Item.DepthFractions.IsSame(DepthFractions)
  end;
end;

procedure TEtsSurfDepthItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FEvapotranspirationDepth,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FEvapotranspirationSurface,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
end;

procedure TEtsSurfDepthItem.SetBoundaryFormula(Index: integer;
  const Value: string);
var
  Item: TStringValueItem;
  Collection: TStringCollection;
begin
  case Index of
    0: EvapotranspirationSurface := Value;
    1: EvapotranspirationDepth := Value;
    else
      begin
        if Odd(Index) then
        begin
          Collection := EtFractions;
        end
        else
        begin
          Collection := DepthFractions;
        end;
        Index := (Index -2) div 2;
        while Index >= Collection.Count do
        begin
          Collection.Add;
        end;
        Item := Collection.Items[Index] as TStringValueItem;
        Item.Value := Value;
      end;
  end;
end;

procedure TEtsSurfDepthItem.SetDepthFractions(const Value: TStringCollection);
begin
  FDepthFractions.Assign(Value);
end;

procedure TEtsSurfDepthItem.SetEtFractions(const Value: TStringCollection);
begin
  FEtFractions.Assign(Value);
end;

procedure TEtsSurfDepthItem.SetEvapotranspirationDepth(const Value: string);
begin
  UpdateFormula(Value, DepthPosition, FEvapotranspirationDepth);
end;

procedure TEtsSurfDepthItem.SetEvapotranspirationSurface(const Value: string);
begin
  UpdateFormula(Value, SurfacePosition, FEvapotranspirationSurface);
end;

{ TEtsSurfDepthCollection }

procedure TEtsSurfDepthCollection.AddSpecificBoundary;
begin
  AddBoundary(TEtsSurfDepthStorage.Create);
end;

procedure TEtsSurfDepthCollection.Assign(Source: TPersistent);
begin
  if Source is TEtsSurfDepthCollection then
  begin
    FTimeListCount := TEtsSurfDepthCollection(Source).TimeListCount;
  end;
  inherited;
end;

procedure TEtsSurfDepthCollection.AssignCellValues(DataSets: TList;
  ItemIndex: Integer);
var
  EvapotranspirationSurfaceArray: TDataArray;
  EvapotranspirationDepthArray: TDataArray;
  Boundary: TEtsSurfDepthStorage;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  BoundaryIndex: Integer;
  SegmentCount: integer;
  SegmentIndex: integer;
  FractionalDepthArray: TDataArray;
  FractionalRateArray: TDataArray;
  LocalModel: TPhastModel;
  LayerMin: Integer;
  RowMin: Integer;
  ColMin: Integer;
  LayerMax: Integer;
  RowMax: Integer;
  ColMax: Integer;
begin
  LocalModel := Model as TPhastModel;
  SegmentCount := (Model as TPhastModel).
    ModflowPackages.EtsPackage.SegmentCount;
  BoundaryIndex := 0;
  EvapotranspirationSurfaceArray := DataSets[0];
  EvapotranspirationDepthArray := DataSets[1];
  Boundary := Boundaries[ItemIndex] as TEtsSurfDepthStorage;
  EvapotranspirationSurfaceArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
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
            if EvapotranspirationSurfaceArray.IsValue[
              LayerIndex, RowIndex, ColIndex] then
            begin
              Assert(EvapotranspirationDepthArray.IsValue[
                LayerIndex, RowIndex, ColIndex]);
              with Boundary.EtsSurfDepthArray[BoundaryIndex] do
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

                for SegmentIndex := 1 to SegmentCount - 1 do
                begin
                  FractionalDepthArray := DataSets[SegmentIndex*2];
                  Assert(FractionalDepthArray.IsValue[LayerIndex, RowIndex, ColIndex]);
                  DepthFractions[SegmentIndex-1] := FractionalDepthArray.
                    RealData[LayerIndex, RowIndex, ColIndex];
                  DepthFractionAnnotations[SegmentIndex-1] := FractionalDepthArray.
                    Annotation[LayerIndex, RowIndex, ColIndex];

                  FractionalRateArray := DataSets[SegmentIndex*2+1];
                  Assert(FractionalRateArray.IsValue[LayerIndex, RowIndex, ColIndex]);
                  EtFractions[SegmentIndex-1] := FractionalRateArray.
                    RealData[LayerIndex, RowIndex, ColIndex];
                  EtFractionAnnotations[SegmentIndex-1] := FractionalRateArray.
                    Annotation[LayerIndex, RowIndex, ColIndex];
                end;
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
  for SegmentIndex := 1 to SegmentCount - 1 do
  begin
    FractionalDepthArray := DataSets[SegmentIndex*2];
    FractionalRateArray := DataSets[SegmentIndex*2+1];

    FractionalDepthArray.CacheData;
    FractionalRateArray.CacheData;
  end;
  Boundary.CacheData;
end;

constructor TEtsSurfDepthCollection.Create(Boundary: TModflowBoundary; Model,
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
    FEvapotranspirationSurfaceData.OnInvalidate := (Model as TPhastModel).InvalidateMfEtsEvapSurface;
    FEvapotranspirationDepthData.OnInvalidate := (Model as TPhastModel).InvalidateMfEtsEvapDepth;
  end;

  FListOfEtFractionLists:= TObjectList.Create;
  FListOfDepthFractionLists:= TObjectList.Create;
end;

destructor TEtsSurfDepthCollection.Destroy;
begin
  FEvapotranspirationDepthData.Free;
  FEvapotranspirationSurfaceData.Free;
  FListOfEtFractionLists.Free;
  FListOfDepthFractionLists.Free;
  inherited;
end;

function TEtsSurfDepthCollection.GetTimeList(Index: integer): TModflowTimeList;
var
  TimeList: TModflowTimeList;
  Count: integer;
  FractionIndex: string;
begin
  While Index >= inherited TimeListCount do
  begin
    TimeList := TModflowTimeList.Create(Model);
    Count := inherited TimeListCount;
    FractionIndex := IntToStr(Count div 2);
    if Odd(Count)  then
    begin
      TimeList.NonParamDescription := 'Fractional rate ' + FractionIndex;
      TimeList.ParamDescription := ' fractional rate ' + FractionIndex;
      FListOfEtFractionLists.Add(TimeList);
      if Model <> nil then
      begin
        TimeList.OnInvalidate := (Model as TPhastModel).InvalidateEtsRateFractions;
      end;
    end
    else
    begin
      TimeList.NonParamDescription := 'Fractional depth ' + FractionIndex;
      TimeList.ParamDescription := ' fractional depth ' + FractionIndex;
      FListOfDepthFractionLists.Add(TimeList);
      if Model <> nil then
      begin
        TimeList.OnInvalidate := (Model as TPhastModel).InvalidateEtsDepthFractions;
      end;
    end;
    AddTimeList(TimeList);
  end;
  result := Inherited GetTimeList(index);
end;

procedure TEtsSurfDepthCollection.InitializeTimeLists(ListOfTimeLists: TList);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TEtsSurfDepthItem;
  Boundary: TEtsBoundary;
  ScreenObject: TScreenObject;
  SegmentIndex: Integer;
  FractionalDepthTimeList: TModflowTimeList;
  FractionalRateTimeList: TModflowTimeList;
begin
  Boundary := BoundaryGroup as TEtsBoundary;
  ScreenObject := Boundary.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TEtsSurfDepthItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.EvapotranspirationSurface;
  end;
  FEvapotranspirationSurfaceData.Initialize(BoundaryValues, ScreenObject);
  Assert(FEvapotranspirationSurfaceData.Count = Count);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TEtsSurfDepthItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.EvapotranspirationDepth;
  end;
  FEvapotranspirationDepthData.Initialize(BoundaryValues, ScreenObject);
  Assert(FEvapotranspirationDepthData.Count = Count);

  // assign fractional depths and rates.

  if Count > 0 then
  begin
    for SegmentIndex := 1 to (Model as TPhastModel).
      ModflowPackages.EtsPackage.SegmentCount - 1 do
    begin
      FractionalDepthTimeList := TimeLists[SegmentIndex*2];
      for Index := 0 to Count - 1 do
      begin
        Item := Items[Index] as TEtsSurfDepthItem;
        BoundaryValues[Index].Time := Item.StartTime;
        BoundaryValues[Index].Formula := (Item.DepthFractions.
          Items[SegmentIndex-1] as TStringValueItem).Value;
        if BoundaryValues[Index].Formula = '' then
        begin
          BoundaryValues[Index].Formula := '1';
        end;
      end;
      FractionalDepthTimeList.Initialize(BoundaryValues, ScreenObject);
      Assert(FractionalDepthTimeList.Count = Count);

      FractionalRateTimeList := TimeLists[SegmentIndex*2+1];
      for Index := 0 to Count - 1 do
      begin
        Item := Items[Index] as TEtsSurfDepthItem;
        BoundaryValues[Index].Time := Item.StartTime;
        BoundaryValues[Index].Formula := (Item.EtFractions.
          Items[SegmentIndex-1] as TStringValueItem).Value;
        if BoundaryValues[Index].Formula = '' then
        begin
          BoundaryValues[Index].Formula := '0';
        end;
      end;
      FractionalRateTimeList.Initialize(BoundaryValues, ScreenObject);
      Assert(FractionalRateTimeList.Count = Count);
    end;
  end;


  ClearBoundaries;
  SetBoundaryCapacity(FEvapotranspirationSurfaceData.Count);
  for TimeIndex := 0 to FEvapotranspirationSurfaceData.Count - 1 do
  begin
    AddBoundary(TEtsSurfDepthStorage.Create);
  end;
  ListOfTimeLists.Add(FEvapotranspirationSurfaceData);
  ListOfTimeLists.Add(FEvapotranspirationDepthData);
  if Count > 0 then
  begin
    for SegmentIndex := 1 to (Model as TPhastModel).
      ModflowPackages.EtsPackage.SegmentCount - 1 do
    begin
      FractionalDepthTimeList := FListOfDepthFractionLists[SegmentIndex-1];
      ListOfTimeLists.Add(FractionalDepthTimeList);
      FractionalRateTimeList := FListOfEtFractionLists[SegmentIndex-1];
      ListOfTimeLists.Add(FractionalRateTimeList);
    end;
  end;
end;

procedure TEtsSurfDepthCollection.InvalidateDepthFractions(Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowTimeList;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    for Index := 0 to FListOfDepthFractionLists.Count - 1 do
    begin
      TimeList := FListOfDepthFractionLists[Index];
      TimeList.Invalidate;
    end;
  end;
end;

procedure TEtsSurfDepthCollection.InvalidateEtDepth(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FEvapotranspirationDepthData.Invalidate;
  end;
end;

procedure TEtsSurfDepthCollection.InvalidateEtFractions(Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowTimeList;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    for Index := 0 to FListOfEtFractionLists.Count - 1 do
    begin
      TimeList := FListOfEtFractionLists[Index];
      TimeList.Invalidate;
    end;
  end;
end;

procedure TEtsSurfDepthCollection.InvalidateEtSurface(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FEvapotranspirationSurfaceData.Invalidate;
  end;
end;

class function TEtsSurfDepthCollection.ItemClass: TMF_BoundItemClass;
begin
  result := TEtsSurfDepthItem;
end;

procedure TEtsSurfDepthCollection.SetBoundaryStartAndEndTime(
  BoundaryCount: Integer; Item: TCustomModflowBoundaryItem; ItemIndex: Integer);
var
  EtsSurfDepthStorage : TEtsSurfDepthStorage;
  SegmentCount: integer;
  Index: integer;
begin
  EtsSurfDepthStorage := Boundaries[ItemIndex] as TEtsSurfDepthStorage;
  SetLength(EtsSurfDepthStorage.FEtsSurfDepthArray, BoundaryCount);
  SegmentCount := (Model as TPhastModel).
    ModflowPackages.EtsPackage.SegmentCount;
  for Index := 0 to BoundaryCount - 1 do
  begin
    SetLength(EtsSurfDepthStorage.EtsSurfDepthArray[Index].
      DepthFractions, SegmentCount-1);
    SetLength(EtsSurfDepthStorage.EtsSurfDepthArray[Index].
      EtFractions, SegmentCount-1);
    SetLength(EtsSurfDepthStorage.EtsSurfDepthArray[Index].
      DepthFractionAnnotations, SegmentCount-1);
    SetLength(EtsSurfDepthStorage.EtsSurfDepthArray[Index].
      EtFractionAnnotations, SegmentCount-1);
  end;
  inherited;

end;

function TEtsSurfDepthCollection.TimeListCount: integer;
begin
  if Model = nil then
  begin
    result := FTimeListCount;
  end
  else
  begin
    result := 2
      + ((Model as TPhastModel).ModflowPackages.EtsPackage.SegmentCount-1) * 2;
  end;
end;

{ TEtsSurfDepth_Cell }

procedure TEtsSurfDepth_Cell.Cache(Comp: TCompressionStream);
begin
  inherited;
  Values.Cache(Comp);
  WriteCompInt(Comp, StressPeriod);
end;

function TEtsSurfDepth_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TEtsSurfDepth_Cell.GetDepthFractionAnnotations(
  const Index: integer): string;
begin
  result := Values.DepthFractionAnnotations[Index];
end;

function TEtsSurfDepth_Cell.GetDepthFractions(const Index: integer): double;
begin
  result := Values.DepthFractions[Index];
end;

function TEtsSurfDepth_Cell.GetEtFractionAnnotations(
  const Index: integer): string;
begin
  result := Values.EtFractionAnnotations[Index];
end;

function TEtsSurfDepth_Cell.GetEtFractions(const Index: integer): double;
begin
  result := Values.EtFractions[Index];
end;

function TEtsSurfDepth_Cell.GetEvapotranspirationDepth: double;
begin
  result := Values.EvapotranspirationDepth;
end;

function TEtsSurfDepth_Cell.GetEvapotranspirationDepthAnnotation: string;
begin
  result := Values.EvapotranspirationDepthAnnotation;
end;

function TEtsSurfDepth_Cell.GetEvapotranspirationSurface: double;
begin
  result := Values.EvapotranspirationSurface;
end;

function TEtsSurfDepth_Cell.GetEvapotranspirationSurfaceAnnotation: string;
begin
  result := Values.EvapotranspirationSurfaceAnnotation;
end;

function TEtsSurfDepth_Cell.GetIntegerAnnotation(Index: integer): string;
begin
  result := '';
  Assert(False);
end;

function TEtsSurfDepth_Cell.GetIntegerValue(Index: integer): integer;
begin
  result := 0;
  Assert(False);
end;

function TEtsSurfDepth_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TEtsSurfDepth_Cell.GetRealAnnotation(Index: integer): string;
begin
  case Index of
    0: result := EvapotranspirationSurfaceAnnotation;
    1: result := EvapotranspirationDepthAnnotation;
    else
      begin
        Dec(Index, 2);
        if Odd(Index) then
        begin
          result := EtFractionAnnotations[Index div 2];
        end
        else
        begin
          result := DepthFractionAnnotations[Index div 2];
        end;
      end;
  end;
end;

function TEtsSurfDepth_Cell.GetRealValue(Index: integer): double;
begin
  case Index of
    0: result := EvapotranspirationSurface;
    1: result := EvapotranspirationDepth;
    else
      begin
        Dec(Index, 2);
        if Odd(Index) then
        begin
          result := EtFractions[Index div 2];
        end
        else
        begin
          result := DepthFractions[Index div 2];
        end;
      end;
  end;
end;

function TEtsSurfDepth_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TEtsSurfDepth_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

procedure TEtsSurfDepth_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

{ TEtsSurfDepthRecord }

procedure TEtsSurfDepthRecord.Assign(const Item: TEtsSurfDepthRecord);
begin
  self := Item;
  SetLength(DepthFractions, Length(DepthFractions));
  SetLength(EtFractions, Length(EtFractions));
  SetLength(DepthFractionAnnotations, Length(DepthFractionAnnotations));
  SetLength(EtFractionAnnotations, Length(EtFractionAnnotations));
end;

procedure TEtsSurfDepthRecord.Cache(Comp: TCompressionStream);
var
  Count: integer;
  Index: Integer;
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, EvapotranspirationSurface);
  WriteCompReal(Comp, EvapotranspirationDepth);
  Count := Length(DepthFractions);
  WriteCompInt(Comp, Count);
  for Index := 0 to Count - 1 do
  begin
    WriteCompReal(Comp, DepthFractions[Index]);
  end;
  for Index := 0 to Count - 1 do
  begin
    WriteCompReal(Comp, EtFractions[Index]);
  end;
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompString(Comp, EvapotranspirationSurfaceAnnotation);
  WriteCompString(Comp, EvapotranspirationDepthAnnotation);
  for Index := 0 to Count - 1 do
  begin
    WriteCompString(Comp, DepthFractionAnnotations[Index]);
  end;
  for Index := 0 to Count - 1 do
  begin
    WriteCompString(Comp, EtFractionAnnotations[Index]);
  end;
end;

procedure TEtsSurfDepthRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
var
  Count: integer;
  Index: Integer;
begin
  Cell := ReadCompCell(Decomp);
  EvapotranspirationSurface := ReadCompReal(Decomp);
  EvapotranspirationDepth := ReadCompReal(Decomp);
  Count := ReadCompInt(Decomp);
  SetLength(DepthFractions, Count);
  for Index := 0 to Count - 1 do
  begin
    DepthFractions[Index] := ReadCompReal(Decomp);
  end;
  SetLength(EtFractions, Count);
  for Index := 0 to Count - 1 do
  begin
    EtFractions[Index] := ReadCompReal(Decomp);
  end;
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  EvapotranspirationSurfaceAnnotation := ReadCompString(Decomp, Annotations);
  EvapotranspirationDepthAnnotation := ReadCompString(Decomp, Annotations);
  SetLength(DepthFractionAnnotations, Count);
  for Index := 0 to Count - 1 do
  begin
    DepthFractionAnnotations[Index] := ReadCompString(Decomp, Annotations);
  end;
  SetLength(EtFractionAnnotations, Count);
  for Index := 0 to Count - 1 do
  begin
    EtFractionAnnotations[Index] := ReadCompString(Decomp, Annotations);
  end;
end;

{ TStringValueItem }

procedure TStringValueItem.Assign(Source: TPersistent);
begin
  if Source is TStringValueItem then
  begin
    Value := TStringValueItem(Source).Value;
  end;
  inherited;
end;

procedure StringValueRemoveSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TStringValueItem).RemoveSubscription(Sender, AName);
end;

procedure StringValueRestoreSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TStringValueItem).RestoreSubscription(Sender, AName);
end;

constructor TStringValueItem.Create(Collection: TCollection);
var
  SCollection: TStringCollection;
  EtsSurfDepth: TEtsSurfDepthCollection;
  LocalScreenObject: TScreenObject;
begin
  inherited;
  Observer:= TObserver.Create(nil);
  SCollection := StringCollection;
  EtsSurfDepth := SCollection.FEtsSurfDepthCollection as TEtsSurfDepthCollection;
  case SCollection.Purpose of
    scpDepthFractions:
      begin
        Observer.OnUpToDateSet := EtsSurfDepth.InvalidateDepthFractions;
      end;
    scpEtFractions:
      begin
        Observer.OnUpToDateSet := EtsSurfDepth.InvalidateEtFractions;
      end;
    else
      Assert(False);
  end;
  LocalScreenObject := EtsSurfDepth.ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(Observer);
  end;

  FValue := frmGoPhast.PhastModel.FormulaManager.Add;
  FValue.Parser := frmGoPhast.PhastModel.rpTopFormulaCompiler;
  FValue.AddSubscriptionEvents(StringValueRemoveSubscription,
  StringValueRestoreSubscription, self);
end;

destructor TStringValueItem.Destroy;
var
  LocalScreenObject: TScreenObject;
  SCollection: TStringCollection;
  EtsSurfDepth: TEtsSurfDepthCollection;
begin
  Value := '0';
  SCollection := StringCollection;
  EtsSurfDepth := SCollection.FEtsSurfDepthCollection as TEtsSurfDepthCollection;
  LocalScreenObject := EtsSurfDepth.ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.StopsTalkingTo(Observer);
  end;
  frmGoPhast.PhastModel.FormulaManager.Remove(FValue,
    StringValueRemoveSubscription,
    StringValueRestoreSubscription, self);
  Observer.Free;
  inherited;
end;

function TStringValueItem.GetValue: string;
begin
  Result := FValue.Formula;
  Observer.UpToDate := True;
end;

function TStringValueItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  if AnotherItem is TStringValueItem then
  begin
    result := Value = TStringValueItem(AnotherItem).Value;
  end
  else
  begin
    result := false;
  end;
end;

procedure TStringValueItem.RemoveSubscription(Sender: TObject;
  const AName: string);
var
  DS: TObserver;
begin
  DS := frmGoPhast.PhastModel.GetObserverByName(AName);
  DS.StopsTalkingTo(Observer);
end;

procedure TStringValueItem.RestoreSubscription(Sender: TObject;
  const AName: string);
var
  DS: TObserver;
begin
  DS := frmGoPhast.PhastModel.GetObserverByName(AName);
  DS.TalksTo(Observer);
  Observer.UpToDate := False;
end;

procedure TStringValueItem.SetValue(const Value: string);
begin
  UpdateFormula(Value, FValue);
end;

function TStringValueItem.StringCollection: TStringCollection;
begin
  result := Collection as TStringCollection;
end;

procedure TStringValueItem.UpdateFormula(Value: string;
  var FormulaObject: TFormulaObject);
var
  ParentModel: TPhastModel;
  Compiler: TRbwParser;
begin
  if FormulaObject.Formula <> Value then
  begin
    ParentModel := Model as TPhastModel;
    if ParentModel <> nil then
    begin
      Compiler := ParentModel.rpThreeDFormulaCompiler;
      UpdateFormulaDependencies(FormulaObject.Formula, Value, Observer, Compiler);
    end;
    InvalidateModel;
    frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
      FormulaObject, Value,
      frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
      StringValueRemoveSubscription,
      StringValueRestoreSubscription, self);
  end;
end;

procedure TStringValueItem.UpdateFormulaDependencies(OldFormula: string;
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
  ParentScreenObject := StringCollection.FScreenObject as TScreenObject;
  if (ParentScreenObject = nil)
    or not ParentScreenObject.CanInvalidateModel then
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

{ TStringCollection }

procedure TStringCollection.Assign(Source: TPersistent);
begin
  if Source is TStringCollection then
  begin
    Purpose := TStringCollection(Source).Purpose;
  end;
  inherited;
end;

constructor TStringCollection.Create(Model: TObject; ScreenObject: TObject;
  EtsSurfDepthCollection: TCollection);
begin
  inherited Create(TStringValueItem, Model);
  FScreenObject := ScreenObject;
  FEtsSurfDepthCollection := EtsSurfDepthCollection;
end;

{ TEtsLayerCollection }

constructor TEtsLayerCollection.Create(Boundary: TModflowBoundary; Model,
  ScreenObject: TObject);
begin
  inherited;
  if Model <> nil then
  begin
    FEvapotranspirationLayerData.OnInvalidate :=
      (Model as TPhastModel).InvalidateMfEtsEvapLayer;
  end;
end;

{ TEtsCollection }

constructor TEtsCollection.Create(Boundary: TModflowBoundary; Model,
  ScreenObject: TObject);
begin
  inherited;
  if Model <> nil then
  begin
    FEvapotranspirationRateData.OnInvalidate :=
      (Model as TPhastModel).InvalidateMfEtsEvapRate;
  end;
end;

{ TEtsSurfDepthStorage }

procedure TEtsSurfDepthStorage.Clear;
begin
  SetLength(FEtsSurfDepthArray, 0);
  FCleared := True;
end;

procedure TEtsSurfDepthStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
begin
  Count := Length(FEtsSurfDepthArray);
  Compressor.Write(Count, SizeOf(Count));
  for Index := 0 to Count - 1 do
  begin
    FEtsSurfDepthArray[Index].Cache(Compressor);
  end;
end;

procedure TEtsSurfDepthStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Count: Integer;
  Index: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FEtsSurfDepthArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FEtsSurfDepthArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TEtsSurfDepthStorage.GetEtsSurfDepthArray: TEtsSurfDepthArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FEtsSurfDepthArray;
end;

end.
