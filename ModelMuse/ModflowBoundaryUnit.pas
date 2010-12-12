unit ModflowBoundaryUnit;

interface

uses Windows, SysUtils, Classes, ZLib, RbwParser, GoPhastTypes,
  OrderedCollectionUnit, ModflowTransientListParameterUnit, DataSetUnit,
  RealListUnit, TempFiles, SubscriptionUnit, FormulaManagerUnit, SparseDataSets;

type
    // @name defines how a formula is interpreted.
    // @unorderedlist(
    //   @item(fiSpecific - Formula / the length or area of
    //     intersection between the @link(TScreenObject) and grid cell.)
    //   @item(fiTotal - Formula.)
    // )
    // When fiSpecific is used, the formula will be multiplied by
    // ObjectIntersectLength or ObjectIntersectArea.
    // fiSpecific has no effect for @link(TScreenObject)s that have
    // only one vertex.
  TFormulaInterpretation = (fiSpecific, fiDirect, fiTotal);

  // @@name defines the starting and ending time for a particular
  // boundary condition.  Descendants add an array of records that
  // defining where and with what values the boundary condition applies.
  TCustomBoundaryStorage = class(TObject)
  protected
    FCached: boolean;
    FCleared: Boolean;
    FTempFileName : string;
    procedure Clear; virtual; abstract;
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); virtual; abstract;
    procedure Store(Compressor: TCompressionStream); virtual; abstract;
    procedure RestoreData;
  public
    StartingTime: double;
    EndingTime: double;
    destructor Destroy; override;
    procedure CacheData;
  end;

  // @name defines a time and a formula used in
  // @link(TModflowTimeList.Initialize).
  TBoundaryValue = record
    Time: double;
    Formula: string;
  end;

  // @name defines an array of @link(TBoundaryValue)s used in
  // @link(TModflowTimeList.Initialize).
  TBoundaryValueArray = array of TBoundaryValue;

  // @name represents a boundary for one time interval.
  // @name is stored by @link(TCustomMF_BoundColl).
  TCustomModflowBoundaryItem = class(TOrderedItem)
  private
    // See @link(EndTime).
    FEndTime: double;
    // See @link(StartTime).
    FStartTime: double;
    // See @link(EndTime).
    procedure SetEndTime(const Value: double);
    // See @link(StartTime).
    procedure SetStartTime(const Value: double);
    function GetScreenObject: TObject;
  protected
    FObserverList: TList;
    procedure AssignObserverEvents(Collection: TCollection); virtual; abstract;
    procedure CreateFormulaObjects; virtual; abstract;
    procedure GetPropertyObserver(Sender: TObject; List: TList); virtual; abstract;
    procedure RemoveFormulaObjects; virtual; abstract;
    procedure ResetItemObserver(Index: integer);
    procedure UpdateFormulaDependencies(OldFormula: string; var
      NewFormula: string; Observer: TObserver; Compiler: TRbwParser);
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; virtual; abstract;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string);
      virtual; abstract;
    // @name returns @true if AnotherItem is a @classname and
    // @link(StartTime) and (EndTime) are the same in the current
    // @classname and in AnotherItem.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function NonBlankFormulas: boolean;
    function BoundaryFormulaCount: integer; virtual; abstract;
    procedure UpdateFormula(Value: string; Position: Integer;
      var FormulaObject: TFormulaObject);
    procedure RemoveSubscription(Sender: TObject; const AName: string);
    procedure RestoreSubscription(Sender: TObject; const AName: string);
    function CreateFormulaObject(Orientation:
      TDataSetOrientation): TFormulaObject;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property ScreenObject: TObject read GetScreenObject;
    procedure Assign(Source: TPersistent); override;
    // Descendants of @classname define string properties that are the
    // formulas for the unique features of each boundary condition.
    // @name provides access to those properties without knowing what
    // they are through the virtual abstract methods @link(GetBoundaryFormula)
    // and @link(SetBoundaryFormula).  Descendants must override those
    // methods.  @name is used in
    // @link(TfrmScreenObjectProperties.StoreModflowBoundary
    // TfrmScreenObjectProperties.StoreModflowBoundary) and
    // @link(TfrmScreenObjectProperties.GetModflowBoundary
    // TfrmScreenObjectProperties.GetModflowBoundary).
    // @seealso(TCustomMF_BoundColl)
    property BoundaryFormula[Index: integer]: string read GetBoundaryFormula
      write SetBoundaryFormula;
  published
    // @name indicates the starting time of this boundary.
    property StartTime: double read FStartTime write SetStartTime;
    // @name indicates the ending time of this boundary.
    property EndTime: double read FEndTime write SetEndTime;
  end;

  TMF_BoundItemClass = class of TCustomModflowBoundaryItem;

  TNoFormulaItem = class(TCustomModflowBoundaryItem)
  protected
    procedure RemoveFormulaObjects; override;
    procedure CreateFormulaObjects; override;
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string);
      override;
    function BoundaryFormulaCount: integer; override;
  end;

  TModflowParamBoundary = class;
  TModflowTimeList = class;
  TModflowBoundary = class;

  // @name is an abstract ancestor used with MODFLOW boundary conditions
  // that may or may not be defined at cells.
  // For instance @link(TSfrBoundary.EquationValues EquationValues)
  // of the @link(TSfrBoundary SFR boundary) is defined using a direct
  // descendant of @name.  Boundaries that are defined at cells typically
  // descend from @link(TCustomMF_BoundColl) which descends from @name.
  TCustomNonSpatialBoundColl = class(TEnhancedOrderedCollection)
  private
    // See @link(ScreenObject)
    FScreenObject: TObject;
    FBoundary: TModflowBoundary;
    // See @link(Items).
    procedure SetItem(Index: Integer; const Value: TCustomModflowBoundaryItem);
    // See @link(Items).
    function GetItem(Index: Integer): TCustomModflowBoundaryItem;
  protected
    // @name is the @link(TScreenObject) for this boundary.
    // @name provides access to @link(TCustomModflowBoundaryItem) representing
    // the boundary conditions for different time periods.
    property ScreenObject: TObject read FScreenObject;
    // @name is the @link(TModflowBoundary) that owns @classname.
    property BoundaryGroup: TModflowBoundary read FBoundary;
    // @name is the descendant of @link(TCustomModflowBoundaryItem)
    // stored by classname.
    class function ItemClass: TMF_BoundItemClass; virtual; abstract;

  public
    // @name is the @link(TScreenObject) for this boundary.
    // @name provides access to @link(TCustomModflowBoundaryItem) representing
    // the boundary conditions for different time periods.
    property Items[Index: Integer]: TCustomModflowBoundaryItem read GetItem
      write SetItem; default;
    procedure Assign(Source: TPersistent); override;
    // @name is the @link(TScreenObject) for this boundary.
    constructor Create(Boundary: TModflowBoundary;
      Model, ScreenObject: TObject); virtual;
    function Used: boolean;
  end;

  // @name represents MODFLOW boundaries for a series of time intervals.
  // Descendants define one or more @link(TModflowTimeList)s  which must be
  // stored in @link(FTimeLists) in the same order as the order used to access
  // the corresponding @link(TCustomModflowBoundaryItem.BoundaryFormula
  // TCustomModflowBoundaryItem.BoundaryFormula)s.
  TCustomMF_BoundColl = class(TCustomNonSpatialBoundColl)
  private
    // See @link(ParamName).
    FParamName: string;
    // @name stores instances of @link(TCustomBoundaryStorage).
    // Although declared as TList, it is actually a TObjectList.
    // @Seealso(AddBoundary)
    // @Seealso(ClearBoundaries)
    // @Seealso(GetBoundaries)
    // @Seealso(Boundaries)
    // @Seealso(SetBoundaryCapacity)
    FBoundaries: TList;
    // @name is the @link(TModflowBoundary) that owns the current @classname.
    // @name stores a series of @link(TModflowTimeList)s.  They
    // must be in the same order as the order used to access
    // the corresponding @link(TCustomModflowBoundaryItem.BoundaryFormula
    // TCustomModflowBoundaryItem.BoundaryFormula)s.
    // @seealso(AddTimeList)
    // @seealso(GetTimeList)
    // @seealso(TimeLists)
    FTimeLists: TList;

    // See @link(Boundaries).
    function GetBoundaries(const Index: integer): TCustomBoundaryStorage;
    // See @link(ParamName).
    function GetParamName: string;
    // See @link(ParamName).
    procedure SetParamName(Value: string);
    // See @link(Param).
    function GetParam: TModflowTransientListParameter;
    // See @link(Param).
    procedure SetParam(const Value: TModflowTransientListParameter);
    function GetBoundaryCount: integer;
//    procedure AssignCellsWithItem(Item: TCustomModflowBoundaryItem;
//      ItemIndex: Integer; DataSets: TList; ListOfTimeLists: TList);
  protected
    procedure TestIfObservationsPresent(var EndOfLastStressPeriod: Double;
      var StartOfFirstStressPeriod: Double;
      var ObservationsPresent: Boolean); virtual;
//    procedure CountBoundaryCells(var BoundaryCount: Integer;
//      DataArray1: TDataArray; DataSets: TList); virtual;

    // See @link(TimeLists).
    function GetTimeList(Index: integer): TModflowTimeList; virtual;
    // @name adds a @link(TCustomBoundaryStorage) to those owned by
    // @classname
    // @Seealso(SetBoundaryCapacity)
    procedure AddBoundary(Value: TCustomBoundaryStorage);
    procedure AddSpecificBoundary; virtual; abstract;
    // @name adds a @link(TModflowTimeList) to those that
    // can be accessed through @link(TimeLists).  The order in which
    // @link(TModflowTimeList)s are added must correspond to the
    // order in which the corresponding
    // @link(TCustomModflowBoundaryItem.BoundaryFormula
    // TCustomModflowBoundaryItem.BoundaryFormula)s are accessed.
    procedure AddTimeList(List: TModflowTimeList);
    // @name is the @link(TModflowBoundary) that owns @classname.
    // @name is used to set the capacity of @link(FBoundaries)
    // before calling @link(AddBoundary).
    procedure SetBoundaryCapacity(Value: integer);
    // @name sets the @link(TCustomBoundaryStorage.StartingTime
    // TCustomBoundaryStorage.StartingTime) and
    // @link(TCustomBoundaryStorage.StartingTime
    // TCustomBoundaryStorage.EndingTime) of the
    // @link(TCustomBoundaryStorage) at ItemIndex in @link(Boundaries)
    // to the values of @link(TCustomModflowBoundaryItem.StartTime
    // TCustomModflowBoundaryItem.StartTime) and
    // @link(TCustomModflowBoundaryItem.EndTime
    // TCustomModflowBoundaryItem.EndTime)
    // Descendants used BoundaryCount to set the length of array of records
    // that define where and with what values the boundary condition apply.
    // for the item in @link(Boundaries) at ItemIndex.
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer); virtual;
    procedure ClearTimeLists;
  public
    // @name frees all the @link(TCustomBoundaryStorage) owned by
    // @classname.
    procedure ClearBoundaries;
    // @name copies @link(ParamName) from Source and calls inherited Assign.
    procedure Assign(Source: TPersistent);override;
    // @name provides access to @link(TCustomBoundaryStorage) for different
    // time periods.  In descendants, these @link(TCustomBoundaryStorage)
    // define the locations, values, and times for the boundaries.
    property Boundaries[const Index: integer]: TCustomBoundaryStorage
      read GetBoundaries;
    property BoundaryCount: integer read GetBoundaryCount;
    // @name creates an instance of @classname.
    constructor Create(Boundary: TModflowBoundary;
      Model, ScreenObject: TObject); override;
    // @name destroys the current instance of @classname.
    // Do not call @name; call Free instead.
    destructor Destroy; override;
    // @name returns @true if AnOrderedCollection is a @classname and
    // Param = the Param of AnOrderedCollection and the inherited @name
    // returns @true.
    function IsSame(AnOrderedCollection: TOrderedCollection): boolean; override;
    // @name is the @link(TModflowTransientListParameter) (if any) of the
    // current @classname.
    property Param: TModflowTransientListParameter read GetParam write SetParam;
    // @name is the number of @link(TModflowTimeList)s that can be accessed
    // in @link(TimeLists).
    function TimeListCount: integer; virtual;
    // @name provides access to the @link(TModflowTimeList)s defined in
    // descendants. @name is used in
    // @link(TfrmScreenObjectProperties.InitializeModflowBoundaryFrames
    // TfrmScreenObjectProperties.InitializeModflowBoundaryFrames).
    property TimeLists[Index: integer]: TModflowTimeList read GetTimeList;
    // @name returns @true if Count > 0.
    function DataSetUsed(DataArray: TDataArray): boolean; virtual;
    function GetBoundaryByStartTime(StartTime: double): TCustomBoundaryStorage;
  published
    // @name is the name of the @link(TModflowTransientListParameter)
    // (if any) associated with this @classname.
    property ParamName: string read GetParamName write SetParamName;
  end;

  TMF_BoundCollClass = class of TCustomMF_BoundColl;

  // @name is used for boundary conditions in which the boundary conditions
  // are either in the form of an array of values or where the individual
  // boundary condition cells are linked into a larger structure.
  // @name is used for the EVT, ETS, RCH, RES, LAK, SFR, and UZF packages.
  TCustomMF_ArrayBoundColl = class(TCustomMF_BoundColl)
  private
    procedure AssignCellsWithItem(Item: TCustomModflowBoundaryItem;
      ItemIndex: Integer; DataSets: TList; ListOfTimeLists: TList);
  protected
    // @name is a virtual abstract method used to set the values of the
    // cell locations in @link(Boundaries) for a particular time period.
    procedure AssignCellValues(DataSets: TList; ItemIndex: Integer);
      virtual; abstract;
    procedure CountBoundaryCells(var BoundaryCount: Integer;
      DataArray1: TDataArray; DataSets: TList); virtual;
    // @name is a virtual abstract method that descendants use to
    // call (TModflowTimeList.Initialize TModflowTimeList.Initialize).
    procedure InitializeTimeLists(ListOfTimeLists: TList); virtual; abstract;
  public
    // @name determines the locations, times, and values of
    // the boundary condition associated with @classname.  These boundaries
    // are first evaluated in @link(TModflowTimeList)s defined by
    // descedents and accessed through @link(TimeLists). Those data
    // are then transfered to descendants of @link(TCustomBoundaryStorage)
    // by calls to @link(AssignCellValues).
    procedure EvaluateArrayBoundaries;
  end;

  // @name is used for boundary conditions in which each section of an object
  // is used to define a separate set of boundary conditions.
  // It is used for the
  // CHD, DRN, DRT, GHB, RIV, and WEL packages.
  TCustomMF_ListBoundColl = class(TCustomMF_BoundColl)
  protected
    // @name should be called just before a formula is about
    // to be evaluated to make sure that all the required
    // information is up-to-date.  ACell is a @link(TCellAssignment).
    procedure UpdataRequiredData(DataSets: TList; Variables: TList;
      ACell: TObject);
    // @name is called in @link(EvaluateListBoundaries).
    // @name stores the locations of the @link(TCellAssignment)s in ACellList
    // (which is a @link(TCellAssignmentList)) in BoundaryStorage.
    procedure AssignCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); virtual; abstract;
    // @name assigns values to the contents of
    // @link(TCustomBoundaryStorage BoundaryStorage)
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList); virtual; abstract;
    // @name when the formula assigned by the user needs to be
    // expanded by the program @name is used to do that.
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
      virtual; abstract;
  public
    // @name determines the locations, times, and values of
    // the boundary condition associated with @classname.
    procedure EvaluateListBoundaries;
  end;


  // @name is used to store a series of @link(TDataArray)s for boundary
  // conditions in MODFLOW.
  TModflowTimeList = class(TCustomTimeList)
  private
    // See @link(NonParamDescription).
    FNonParamDescription: string;
    // See @link(ParamDescription).
    FParamDescription: string;
    // See @link(DataType).
    FDataType: TRbwDataType;
    // See @link(OnInvalidate).
    FOnInvalidate: TNotifyEvent;
    FVariableInterpretation: boolean;
    FScreenObject: TObject;
  protected
    // @name calls the inherited @link(TCustomTimeList.SetUpToDate)
    // and then calls @link(OnInvalidate) if @link(OnInvalidate) is assigned.
    procedure SetUpToDate(const Value: boolean); override;
  public
    procedure Invalidate; override;
    constructor Create(Model: TObject; ScreenObject: TObject);
    // @name takes the times and formulas in BoundaryValues and uses them
    // to determine the locations and values for those times.  These
    // locations and values are stored in @link(TRealSparseDataSet)s
    // accessed through @link(TCustomTimeList.Items Items).
    procedure Initialize(BoundaryValues: TBoundaryValueArray;
      ScreenObject: TObject;
      AssignmentLocation: TAssignmentLocation = alAll);
      reintroduce;
    // @name is a description of what this @classname represents when @name is
    // not associated with a parameter.
    // @name is used in
    // @link(TfrmScreenObjectProperties.InitializeModflowBoundaryFrames
    // TfrmScreenObjectProperties.InitializeModflowBoundaryFrames).
    property NonParamDescription: string read FNonParamDescription
      write FNonParamDescription;
    // @name is a description of what this @classname represents when @name is
    // associated with a parameter.
    // @name is used in
    // @link(TfrmScreenObjectProperties.InitializeModflowBoundaryFrames
    // TfrmScreenObjectProperties.InitializeModflowBoundaryFrames).
    property ParamDescription: string read FParamDescription
      write FParamDescription;
    property VariableInterpretation: boolean read FVariableInterpretation
      write FVariableInterpretation;
    // @name is the @link(TRbwDataType) of the @link(TDataArray)s contained
    // by @classname
    property DataType: TRbwDataType read FDataType write FDataType;
    // If assigned, @name is called with @link(UpToDate) is set to False.
    property OnInvalidate: TNotifyEvent read FOnInvalidate write FOnInvalidate;
//    property Sections[Index: integer]: T3DSparseIntegerArray read GetSection;
  end;

  // Each @name stores a @link(TCustomMF_BoundColl).
  // @classname is stored by @link(TModflowParameters).
  TModflowParamItem = class(TOrderedItem)
  private
    // See @link(Param).
    FParam: TCustomMF_BoundColl;
    // See @link(Param).
    procedure SetParam(const Value: TCustomMF_BoundColl);
  protected
    // @name is used in @link(Create) to create @link(FParam).
    class function BoundaryClass: TMF_BoundCollClass; virtual; abstract;
  public
    // @name copies a @classname from Source to this @Classname.
    procedure Assign(Source: TPersistent);override;
    // @name creates an instance of @classname.
    Constructor Create(Collection: TCollection); override;
    // @name destroys the current instance of @classname.  Do not call
    // @name directly.  Call Free instead.
    Destructor Destroy; override;
    // @name tests whether AnotherItem is the same as this @Classname.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    // @name returns @link(TCustomNonSpatialBoundColl.Used Param.Used).
    function Used: boolean;
    function DataSetUsed(DataArray: TDataArray): boolean; virtual;
  published
    // @name is the @link(TCustomMF_BoundColl) used to store a single
    // parameter and its associated data.
    property Param: TCustomMF_BoundColl read FParam write SetParam;
  end;

  TModflowParamItemClass = class of TModflowParamItem;

  // @name stores a series of MODFLOW boundaries
  // associated with a series of MODFLOW parameters.
  // @seealso(TModflowParamItem)
  TModflowParameters = class(TEnhancedOrderedCollection)
  private
    // See @link(ScreenObject).
    FScreenObject: TObject;
    // @name is the @link(TModflowBoundary) that owns @classname.
    FBoundary: TModflowParamBoundary;
    // See @link(Items).
    function GetItem(Index: Integer): TModflowParamItem;
    // See @link(Items).
    procedure SetItem(Index: Integer; const Value: TModflowParamItem);
  public
    // @name calls @link(TCustomMF_ArrayBoundColl.EvaluateArrayBoundaries
    // TCustomMF_ArrayBoundColl.EvaluateArrayBoundaries) for each
    // @link(Items)[Index].@link(TCustomMF_ArrayBoundColl Param).
    procedure EvaluateArrayBoundaries;
    procedure EvaluateListBoundaries;
    // @name adds as new descendant of @link(TModflowParamItem);
    function Add: TModflowParamItem;
    // @name creates an instance of @classname.
    constructor Create(Boundary: TModflowParamBoundary;
      ItemClass: TModflowParamItemClass; Model, ScreenObject: TObject); virtual;
    // @name returns the @link(TModflowParamItem) whose
    // @link(TCustomMF_BoundColl.ParamName TModflowParamItem.Param.ParamName)
    // matches AName.
    function GetParamByName(const AName: string): TModflowParamItem;
    // @name inserts a new @link(TModflowParamItem)
    // at the position specified by
    // Index.
    function Insert(Index: Integer): TModflowParamItem;
    // @name provides access to the @link(TModflowParamItem)s stored by this
    // @classname
    property Items[Index: Integer]: TModflowParamItem read GetItem
      write SetItem; default;
    // @name returns the position of the @link(TModflowParamItem)
    // associated with
    // the @link(TModflowTransientListParameter).  It is used in
    // @link(TModflowParamBoundary.DeleteParam
    // TModflowParamBoundary.DeleteParam).
    function IndexOfParam(AParam: TModflowTransientListParameter): integer;
    // @name is @nil or the @link(TScreenObject) that owns this @classname.
    property ScreenObject: TObject read FScreenObject;
    // @name returns @true if any @link(TModflowParamItem) in @link(Items)
    // returns @true.
    function Used: boolean;
    function DataSetUsed(DataArray: TDataArray): boolean; virtual;
  end;

  TModflowParametersClass = class of TModflowParameters;

  TModflowScreenObjectProperty = class(TPersistent)
  protected
    // See @link(PhastModel).
    FPhastModel: TObject;
    // See @link(ScreenObject).
    FScreenObject: TObject;
    procedure InvalidateModel;
  public
    function Used: boolean; virtual; abstract;
    // @name is either nil or the the current @link(TPhastModel).
    property PhastModel: TObject read FPhastModel;
    // @name is either @nil or the @link(TScreenObject) that owns
    // this @classname.
    property ScreenObject: TObject read FScreenObject;
    Constructor Create(Model, ScreenObject: TObject);
  end;

  // @name represents the MODFLOW boundaries associated with
  // a single @link(TScreenObject).
  // @seealso(TCustomMF_BoundColl)
  TModflowBoundary = class(TModflowScreenObjectProperty)
  private

    // See @link(Values).
    FValues: TCustomMF_BoundColl;

    // See @link(Values).
    procedure SetValues(const Value: TCustomMF_BoundColl);

  protected
    procedure AddBoundaryTimes(BoundCol: TCustomNonSpatialBoundColl;
      Times: TRealList); virtual;
    // In descendants, @name fills ValueTimeList with a series of TObjectLists
    // - one for
    // each stress period.  Each such TObjectList is filled with
    // descendants of
    // @link(TValueCell) representing the boundaray condition locations and values
    // for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList); virtual; abstract;
    // @name is used in @link(Create) to create @link(FValues).
    class function BoundaryCollectionClass: TMF_BoundCollClass;
      virtual; abstract;
    procedure ClearBoundaries; virtual;
  public
    procedure ClearTimeLists; virtual;
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    Constructor Create(Model, ScreenObject: TObject);
    // @name destroys the current instance of @classname.  Do not call
    // @name directly.  Call Free instead.
    Destructor Destroy; override;
    // @name calls @link(TCustomMF_ArrayBoundColl.EvaluateArrayBoundaries
    // Values.EvaluateArrayBoundaries)
    // Descendents also call @link(TModflowParameters.EvaluateArrayBoundaries
    // Parameters.EvaluateArrayBoundaries).
    procedure EvaluateArrayBoundaries; virtual;
    procedure EvaluateListBoundaries; virtual;
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TCustomBoundaryStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    //
    // @name fills ParamList with the names of the
    // MODFLOW parameters for the current boundary condition that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TCustomBoundaryStorage) in
    // @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList);
      virtual; abstract;

    function NonParameterColumns: integer; virtual;
    procedure UpdateTimes(Times: TRealList); virtual;
    function Used: boolean; override;
    procedure InvalidateDisplay; virtual;
    function DataSetUsed(DataArray: TDataArray): boolean; virtual;
    procedure Clear; virtual;
  published
    // @name stores the MODFLOW boundaries that are NOT
    // associated with parameters.
    property Values: TCustomMF_BoundColl read FValues write SetValues;
  end;

  TModflowParamBoundary = class(TModflowBoundary)
  private
    // See @link(Parameters).
    FParameters: TModflowParameters;
    // See @link(Parameters).
    procedure SetParameters(const Value: TModflowParameters);

  protected
    // @name is used in @link(Create) when creating @link(FParameters).
    // @name is passed to the constructor of @link(FParameters).
    class function ModflowParamItemClass: TModflowParamItemClass;
      virtual; abstract;
    function ParameterType: TParameterType; virtual; abstract;
    procedure ClearBoundaries; override;
  public
    procedure ClearTimeLists; override;
    // @name copies @link(Values) and @link(Parameters) from the Source
    // @classname to this @classname.
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    Constructor Create(Model, ScreenObject: TObject);
    // @name destroys the current instance of @classname.  Do not call
    // @name directly.  Call Free instead.
    Destructor Destroy; override;
    // @name deletes the @link(TModflowParameter) associated with Param from
    // @link(Parameters).
    procedure DeleteParam(Param: TModflowParameter);
    // @name calls @link(TCustomMF_ArrayBoundColl.EvaluateArrayBoundaries
    // Values.EvaluateArrayBoundaries) and
    // @link(TModflowParameters.EvaluateArrayBoundaries
    // Parameters.EvaluateArrayBoundaries).
    procedure EvaluateArrayBoundaries; override;
    procedure EvaluateListBoundaries; override;
      // @name returns @true if either Values.Used is @true
      // or Parameters.Used is @true.
    function Used: boolean; override;
    procedure UpdateTimes(Times: TRealList); override;
    function DataSetUsed(DataArray: TDataArray): boolean; override;
    procedure Clear; override;
  published
    // @name stores the MODFLOW boundaries that ARE
    // associated with parameters.
    property Parameters: TModflowParameters read FParameters
      write SetParameters;
  end;

  TSpecificModflowBoundary = class(TModflowParamBoundary)
  private
    FFormulaInterpretation: TFormulaInterpretation;
    procedure SetFormulaInterpretation(const Value: TFormulaInterpretation);
  published
    // @name determines whether the a formula represents
    // @unorderedlist(
    //   @item(fiSpecific - formula / the length or area of
    //     intersection between the @link(TScreenObject) and grid cell.)
    //   @item(fiTotal - formula.)
    // )
    property FormulaInterpretation: TFormulaInterpretation
      read FFormulaInterpretation write SetFormulaInterpretation;

  public
    // @name copies @link(FormulaInterpretation) from the Source
    // @classname to this @classname and then calls inherited Assign.
    procedure Assign(Source: TPersistent);override;
  end;

procedure GlobalRemoveModflowBoundarySubscription(Sender: TObject; Subject: TObject;
  const AName: string);

procedure GlobalRestoreModflowBoundarySubscription(Sender: TObject; Subject: TObject;
  const AName: string);

implementation

uses Math, Contnrs, ScreenObjectUnit, PhastModelUnit, ModflowGridUnit,
  frmFormulaErrorsUnit, frmGoPhastUnit, SparseArrayUnit, GlobalVariablesUnit,
  GIS_Functions, IntListUnit, ModflowCellUnit, frmProgressUnit;

function SortBoundaryItems(Item1, Item2: pointer): integer;
var
  Bound1: TCustomModflowBoundaryItem;
  Bound2: TCustomModflowBoundaryItem;
  Index: Integer;
begin
  Bound1 := Item1;
  Bound2 := Item2;
  result := Sign(Bound1.StartTime - Bound2.StartTime);
  if result = 0 then
  begin
    result := Sign(Bound1.EndTime - Bound2.EndTime);
  end;
  if result = 0 then
  begin
    for Index := 0 to Bound1.BoundaryFormulaCount - 1 do
    begin
      if Bound1.BoundaryFormula[Index] <> Bound2.BoundaryFormula[Index] then
      begin
        if Bound1.BoundaryFormula[Index] = '' then
        begin
          result := 1;
          Exit;
        end
        else if Bound2.BoundaryFormula[Index] = '' then
        begin
          result := -1;
          Exit;
        end;
      end;
    end;
  end;
end;



procedure TCustomModflowBoundaryItem.SetStartTime(const Value: double);
begin
  if FStartTime <> Value then
  begin
    FStartTime := Value;
    InvalidateModel;
  end;
end;

procedure TCustomModflowBoundaryItem.UpdateFormulaDependencies(
  OldFormula: string; var NewFormula: string; Observer: TObserver;
  Compiler: TRbwParser);
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

procedure TCustomModflowBoundaryItem.Assign(Source: TPersistent);
var
  Item: TCustomModflowBoundaryItem;
begin
  if Source is TCustomModflowBoundaryItem then
  begin
    Item := TCustomModflowBoundaryItem(Source);
    StartTime := Item.StartTime;
    EndTime := Item.EndTime;
  end;
  inherited;
end;

function TCustomModflowBoundaryItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TCustomModflowBoundaryItem;
begin
  result := (AnotherItem is TCustomModflowBoundaryItem);
  if result then
  begin
    Item := TCustomModflowBoundaryItem(AnotherItem);
    result := (Item.EndTime = EndTime)
      and (Item.StartTime = StartTime);
  end;
end;

function TCustomModflowBoundaryItem.NonBlankFormulas: boolean;
var
  Index: integer;
begin
  result := True;
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    result := BoundaryFormula[Index] <> '';
    if not result then
    begin
      Exit;
    end;
  end;
end;

procedure TCustomModflowBoundaryItem.ResetItemObserver(Index: integer);
var
  Observer: TObserver;
begin
  Observer := FObserverList[Index];
  Observer.UpToDate := True;
end;

procedure TCustomModflowBoundaryItem.SetEndTime(const Value: double);
begin
  if FEndTime <> Value then
  begin
    FEndTime := Value;
    InvalidateModel;
  end;
end;

{ TCustomModflowBoundaryCollection }

procedure TCustomMF_BoundColl.AddBoundary(
  Value: TCustomBoundaryStorage);
begin
  FBoundaries.Add(Value);
end;

procedure TCustomMF_BoundColl.AddTimeList(List: TModflowTimeList);
begin
  FTimeLists.Add(List);
end;

procedure TCustomMF_BoundColl.Assign(Source: TPersistent);
var
  Index: Integer;
begin
  if Source is TCustomMF_BoundColl then
  begin
    ParamName :=
      TCustomMF_BoundColl(Source).ParamName;
  end;
  inherited;
  for Index := Count - 1 downto 0 do
  begin
    if not Items[Index].NonBlankFormulas then
    begin
      Delete(Index);
    end;
  end;
end;

procedure TCustomMF_BoundColl.ClearBoundaries;
begin
  FBoundaries.Clear;
end;

procedure TCustomMF_BoundColl.ClearTimeLists;
var
  Index: Integer;
begin
  for Index := 0 to TimeListCount - 1 do
  begin
    TimeLists[Index].Clear;
  end;
end;

constructor TCustomMF_BoundColl.Create(Boundary: TModflowBoundary;
  Model, ScreenObject: TObject);
begin
  inherited ;//Create(ItemClass, Model);
  FTimeLists := TList.Create;
//  FBoundary := Boundary;
  FBoundaries:= TObjectList.Create;
//  Assert((ScreenObject = nil) or (ScreenObject is TScreenObject));
//  FScreenObject := ScreenObject;
end;

function TCustomMF_BoundColl.DataSetUsed(DataArray: TDataArray): boolean;
var
  TimeListIndex: Integer;
  TimeList: TModflowTimeList;
begin
  result := False;
  for TimeListIndex := 0 to TimeListCount - 1 do
  begin
    TimeList := TimeLists[TimeListIndex];
    result := TimeList.IndexOfDataSet(DataArray) >= 0;
    if result then
    begin
      Exit;
    end;
  end;
end;

destructor TCustomMF_BoundColl.Destroy;
begin
  inherited;
  FBoundaries.Free;
  FTimeLists.Free;
end;

procedure TCustomMF_ArrayBoundColl.EvaluateArrayBoundaries;
var
  ItemIndex: integer;
  Item: TCustomModflowBoundaryItem;
  ListOfTimeLists: TList;
  DataSets: TList;
  EndOfLastStressPeriod: Double;
  StartOfFirstStressPeriod: Double;
  ObservationsPresent: Boolean;
  PriorTime: Double;
  ItemCount: Integer;
  ExtraItem: TNoFormulaItem;
  PhastModel: TPhastModel;
  FirstUsedTime: Double;
  LastUsedTime: Double;
  TimeIndex: Integer;
  TimeList1: TModflowTimeList;
begin
  if Count = 0 then
  begin
    Exit;
  end;

  PhastModel := Model as TPhastModel;
  FirstUsedTime := PhastModel.ModflowFullStressPeriods[0].StartTime;
  LastUsedTime := PhastModel.ModflowFullStressPeriods[
    PhastModel.ModflowFullStressPeriods.Count - 1].EndTime;

  Item := Items[0];
  if Item.StartTime >= LastUsedTime  then
  begin
    Exit;
  end;

  Item := Items[Count-1];
  if Item.EndTime <= FirstUsedTime  then
  begin
    Exit;
  end;

  FirstUsedTime := Max(FirstUsedTime,
    PhastModel.ModflowStressPeriods[0].StartTime);
  LastUsedTime := Min(LastUsedTime, PhastModel.ModflowStressPeriods[
    PhastModel.ModflowStressPeriods.Count - 1].EndTime);

  ListOfTimeLists := TList.Create;
  DataSets := TList.Create;
  try
    InitializeTimeLists(ListOfTimeLists);
    TestIfObservationsPresent(EndOfLastStressPeriod, StartOfFirstStressPeriod,
      ObservationsPresent);
    PriorTime := StartOfFirstStressPeriod;
    ItemCount := 0;
    for ItemIndex := 0 to Count - 1 do
    begin
      Item := Items[ItemIndex];
      if (Item.StartTime > LastUsedTime)
        or (Item.EndTime < FirstUsedTime) then
      begin
        for TimeIndex := 0 to ListOfTimeLists.Count - 1 do
        begin
          TimeList1 := ListOfTimeLists[TimeIndex];
          TimeList1.FreeItem(ItemCount);
        end;
        Inc(ItemCount);
        Continue;
      end;

      if ObservationsPresent then
      begin
        if PriorTime < Item.StartTime then
        begin
          ExtraItem := TNoFormulaItem.Create(nil);
          try
            ExtraItem.FStartTime := PriorTime;
            ExtraItem.FEndTime := Item.StartTime;
            DataSets.Clear;
            AssignCellsWithItem(ExtraItem, ItemCount, DataSets, ListOfTimeLists);
            Inc(ItemCount);
          finally
            ExtraItem.Free;
          end;
        end;
        PriorTime := Item.EndTime;
      end;
      DataSets.Clear;
      AssignCellsWithItem(Item, ItemCount, DataSets, ListOfTimeLists);
      Inc(ItemCount);
      if (ItemIndex = Count - 1) and ObservationsPresent then
      begin
        if Item.EndTime < EndOfLastStressPeriod then
        begin
          ExtraItem := TNoFormulaItem.Create(nil);
          try
            ExtraItem.FStartTime := Item.EndTime;
            ExtraItem.FEndTime := EndOfLastStressPeriod;
            DataSets.Clear;
            AssignCellsWithItem(ExtraItem, ItemCount, DataSets, ListOfTimeLists);
            Inc(ItemCount);
          finally
            ExtraItem.Free;
          end;
        end;
      end;
    end;

  finally
    DataSets.Free;
    ListOfTimeLists.Free;
  end;
end;

procedure TCustomMF_ListBoundColl.EvaluateListBoundaries;
var
  ItemIndex: integer;
  Item: TCustomModflowBoundaryItem;
//  ListOfTimeLists: TList;
//  DataSets: TList;
  EndOfLastStressPeriod: Double;
  StartOfFirstStressPeriod: Double;
  ObservationsPresent: Boolean;
  PriorTime: Double;
  ItemCount: Integer;
  ExtraItem: TNoFormulaItem;
  ScreenObject: TScreenObject;
  PhastModel: TPhastModel;
  Grid: TModflowGrid;
  CellList: TCellAssignmentList;
  BoundaryFunctionIndex: Integer;
  Formula: string;
  AnItem: TCustomModflowBoundaryItem;
  NextItem: TCustomModflowBoundaryItem;
  Compiler: TRbwParser;
  Expression: TExpression;
  UsedVariables: TStringList;
  VarIndex: Integer;
  VarName: string;
  VarPosition: Integer;
  Variable: TCustomValue;
  AnotherDataSet: TDataArray;
  GlobalVariable: TGlobalVariable;
  CellIndex: Integer;
  ACell: TCellAssignment;
  SparseArrays: TList;
  EliminateIndicies: TIntegerList;
  SectionIndex: Integer;
  SparseArray: T3DSparseBooleanArray;
  Section: Integer;
  Index: Integer;
  Layer: Integer;
  Variables: TList;
  DataSets: TList;
  FirstUsedTime: Double;
  LastUsedTime: Double;
begin
  if Count = 0 then
  begin
    Exit;
  end;

  PhastModel := Model as TPhastModel;

  FirstUsedTime := PhastModel.ModflowFullStressPeriods[0].StartTime;
  LastUsedTime := PhastModel.ModflowFullStressPeriods[
    PhastModel.ModflowFullStressPeriods.Count - 1].EndTime;

  AnItem := Items[0];
  if AnItem.StartTime >= LastUsedTime  then
  begin
    Exit;
  end;

  AnItem := Items[Count-1];
  if AnItem.EndTime <= FirstUsedTime  then
  begin
    Exit;
  end;

  CellList:= TCellAssignmentList.Create;
  UsedVariables:= TStringList.Create;
  EliminateIndicies := TIntegerList.Create;
  try
    ScreenObject := FScreenObject as TScreenObject;

    Grid := PhastModel.ModflowGrid;
    Compiler := PhastModel.rpThreeDFormulaCompiler;

    FirstUsedTime := Max(FirstUsedTime,
      PhastModel.ModflowStressPeriods[0].StartTime);
    LastUsedTime := Min(LastUsedTime, PhastModel.ModflowStressPeriods[
      PhastModel.ModflowStressPeriods.Count - 1].EndTime);


    ScreenObject.GetCellsToAssign(Grid, '0', nil, nil, CellList, alAll);

    // eliminate cells that are at the same location and are part of the same section;
    SparseArrays := TObjectList.Create;
    try
      for SectionIndex := 0 to ScreenObject.SectionCount - 1 do
      begin
        SparseArray := T3DSparseBooleanArray.Create(SPASmall);
        SparseArrays.Add(SparseArray)
      end;
      for CellIndex := CellList.Count - 1 downto 0 do
      begin
        ACell := CellList[CellIndex];
        if PhastModel.LayerStructure.IsLayerSimulated(ACell.Layer) then
        begin
          SparseArray := SparseArrays[ACell.Section];
          Layer := PhastModel.LayerStructure.
            DataSetLayerToModflowLayer(ACell.Layer);
          if SparseArray.IsValue[Layer, ACell.Row, ACell.Column] then
          begin
            EliminateIndicies.Add(CellIndex);
          end
          else
          begin
            SparseArray.Items[Layer, ACell.Row, ACell.Column] := True;
          end;
        end
        else
        begin
          EliminateIndicies.Add(CellIndex);
        end;
      end;
    finally
      SparseArrays.Free;
    end;

    for Index := 0 to EliminateIndicies.Count - 1  do
    begin
      CellList.Delete(EliminateIndicies[Index]);
    end;

    ClearBoundaries;


    TestIfObservationsPresent(EndOfLastStressPeriod, StartOfFirstStressPeriod,
      ObservationsPresent);
    PriorTime := StartOfFirstStressPeriod;
    ItemCount := 0;

    for ItemIndex := 0 to Count - 1 do
    begin
      AnItem := Items[ItemIndex];

      // Skip times earlier than the first time or after
      // the last time.
      if (AnItem.StartTime > LastUsedTime)
        or (AnItem.EndTime <= FirstUsedTime) then
      begin
        if ObservationsPresent then
        begin
          if PriorTime < AnItem.StartTime then
          begin
            AddSpecificBoundary;
            Inc(ItemCount);
          end;
          Inc(ItemCount);
          AddSpecificBoundary;
          if AnItem.EndTime < EndOfLastStressPeriod then
          begin
            Inc(ItemCount);
            AddSpecificBoundary;
          end;
        end
        else
        begin
          Inc(ItemCount);
          AddSpecificBoundary;
        end;
        Continue;
      end;

      //  Add extra items if this boundary skips a stress period.
      if ObservationsPresent then
      begin
        if PriorTime < AnItem.StartTime then
        begin
          ExtraItem := TNoFormulaItem.Create(nil);
          try
            ExtraItem.FStartTime := PriorTime;
            ExtraItem.FEndTime := AnItem.StartTime;

            Variables := TList.Create;
            DataSets := TList.Create;
            try
              AddSpecificBoundary;
              SetBoundaryStartAndEndTime(CellList.Count, ExtraItem, 0);
              AssignCellLocation(Boundaries[ItemCount],  CellList);
              for BoundaryFunctionIndex := 0 to AnItem.BoundaryFormulaCount - 1 do
              begin
                Formula := '0';
                Compiler.Compile(Formula);
                Expression := Compiler.CurrentExpression;

                CellList.Clear;
                ScreenObject.GetCellsToAssign(Grid, Formula, nil, nil, CellList, alAll);
                for Index := 0 to EliminateIndicies.Count - 1  do
                begin
                  CellList.Delete(EliminateIndicies[Index]);
                end;
  //              AssignCellsWithItem(ExtraItem, ItemCount, DataSets, ListOfTimeLists);
                UpdateCurrentScreenObject(ScreenObject);

                AssignCellList(Expression, CellList, Boundaries[ItemCount],
                  BoundaryFunctionIndex, Variables, DataSets);

                PhastModel.DataArrayManager.CacheDataArrays;
              end;
            finally
              Variables.Free;
              DataSets.Free;
            end;
            Inc(ItemCount);
          finally
            ExtraItem.Free;
          end;
        end;
        PriorTime := AnItem.EndTime;
      end;

      AddSpecificBoundary;
      SetBoundaryStartAndEndTime(CellList.Count, AnItem, ItemCount);
      AssignCellLocation(Boundaries[ItemCount],  CellList);
      for BoundaryFunctionIndex := 0 to AnItem.BoundaryFormulaCount - 1 do
      begin
        Formula := AdjustedFormula(BoundaryFunctionIndex, ItemIndex);
        try
          Compiler.Compile(Formula)
        except on E: ERbwParserError do
          begin
            Formula := '0';
            Compiler.Compile(Formula);
            // send error message
            AnItem.BoundaryFormula[BoundaryFunctionIndex] := Formula;
          end;
        end;
        Expression := Compiler.CurrentExpression;
        if not (Expression.ResultType in [rdtDouble, rdtInteger]) then
        begin
          Formula := '0';
          Compiler.Compile(Formula);
          // send error message
          AnItem.BoundaryFormula[BoundaryFunctionIndex] := Formula;
          Expression := Compiler.CurrentExpression;
        end;
        CellList.Clear;
        ScreenObject.GetCellsToAssign(Grid, Formula, nil, nil, CellList, alAll);
        for Index := 0 to EliminateIndicies.Count - 1  do
        begin
          CellList.Delete(EliminateIndicies[Index]);
        end;



        Variables := TList.Create;
        DataSets := TList.Create;
        try
          UsedVariables.Assign(Expression.VariablesUsed);
          for VarIndex := 0 to UsedVariables.Count - 1 do
          begin
            VarName := UsedVariables[VarIndex];
            VarPosition := Compiler.IndexOfVariable(VarName);
            Variable := Compiler.Variables[VarPosition];
            AnotherDataSet := PhastModel.DataArrayManager.GetDataSetByName(VarName);
            if AnotherDataSet <> nil then
            begin
              Assert(AnotherDataSet.DataType = Variable.ResultType);
              AnotherDataSet.Initialize;
              PhastModel.DataArrayManager.AddDataSetToCache(AnotherDataSet);
              Variables.Add(Variable);
              DataSets.Add(AnotherDataSet);
            end
            else
            begin
              GlobalVariable := PhastModel.GlobalVariables.GetVariableByName(VarName);
              Assert(GlobalVariable <> nil);
              Assert(Variable.ResultType = GlobalVariable.Format);
            end;
          end;

          UpdateCurrentScreenObject(ScreenObject);

          AssignCellList(Expression, CellList, Boundaries[ItemCount],
            BoundaryFunctionIndex, Variables, DataSets);
        finally
          Variables.Free;
          DataSets.Free;
        end;
      end;
      Inc(ItemCount);

      if ObservationsPresent then
      begin
        if AnItem.EndTime < EndOfLastStressPeriod then
        begin
          if (ItemIndex+1 < Count) then
          begin
            NextItem := Items[ItemIndex+1];
          end
          else
          begin
            NextItem := nil;
          end;

          if (NextItem = nil) or (AnItem.EndTime < NextItem.StartTime) then
          begin
            ExtraItem := TNoFormulaItem.Create(nil);
            try
              ExtraItem.FStartTime := AnItem.EndTime;
              ExtraItem.FEndTime := EndOfLastStressPeriod;

              Variables := TList.Create;
              DataSets := TList.Create;
              try
                AddSpecificBoundary;
                SetBoundaryStartAndEndTime(CellList.Count, ExtraItem, ItemCount);
                AssignCellLocation(Boundaries[ItemCount],  CellList);
                for BoundaryFunctionIndex := 0 to AnItem.BoundaryFormulaCount - 1 do
                begin
                  Formula := '0';
                  Compiler.Compile(Formula);
                  Expression := Compiler.CurrentExpression;

                  CellList.Clear;
                  ScreenObject.GetCellsToAssign(Grid, Formula, nil, nil, CellList, alAll);
                  for Index := 0 to EliminateIndicies.Count - 1  do
                  begin
                    CellList.Delete(EliminateIndicies[Index]);
                  end;
    //              AssignCellsWithItem(ExtraItem, ItemCount, DataSets, ListOfTimeLists);
                  UpdateCurrentScreenObject(ScreenObject);

                  AssignCellList(Expression, CellList, Boundaries[ItemCount],
                    BoundaryFunctionIndex, Variables, DataSets);
                end;
              finally
                Variables.Free;
                DataSets.Free;
              end;
              Inc(ItemCount);
            finally
              ExtraItem.Free;
            end;
          end;
        end;
        PriorTime := AnItem.EndTime;
      end

    end;




//    InitializeTimeLists(ListOfTimeLists);

{

    TestIfObservationsPresent(EndOfLastStressPeriod, StartOfFirstStressPeriod,
      ObservationsPresent);
    PriorTime := StartOfFirstStressPeriod;
    ItemCount := 0;
    for ItemIndex := 0 to Count - 1 do
    begin
      AnItem := Items[ItemIndex];
      if ObservationsPresent then
      begin
        if PriorTime < AnItem.StartTime then
        begin
          ExtraItem := TNoFormulaItem.Create(nil);
          try
            ExtraItem.FStartTime := PriorTime;
            ExtraItem.FEndTime := AnItem.StartTime;
//            DataSets.Clear;
//            AssignCellsWithItem(ExtraItem, ItemCount, DataSets, ListOfTimeLists);
            Inc(ItemCount);
          finally
            ExtraItem.Free;
          end;
        end;
        PriorTime := AnItem.EndTime;
      end;
//      DataSets.Clear;
//      AssignCellsWithItem(AnItem, ItemCount, DataSets, ListOfTimeLists);
      Inc(ItemCount);
      if (ItemIndex = Count - 1) and ObservationsPresent then
      begin
        if AnItem.EndTime < EndOfLastStressPeriod then
        begin
          ExtraItem := TNoFormulaItem.Create(nil);
          try
            ExtraItem.FStartTime := AnItem.EndTime;
            ExtraItem.FEndTime := EndOfLastStressPeriod;
//            DataSets.Clear;
//            AssignCellsWithItem(ExtraItem, ItemCount, DataSets, ListOfTimeLists);
            Inc(ItemCount);
          finally
            ExtraItem.Free;
          end;
        end;
      end;
    end;
    }

  finally
    EliminateIndicies.Free;
    UsedVariables.Free;
    CellList.Free;
//    DataSets.Free;
//    ListOfTimeLists.Free;
  end;
end;

function TCustomMF_BoundColl.GetBoundaries(
  const Index: integer): TCustomBoundaryStorage;
begin
  result := FBoundaries[Index];
end;

function TCustomMF_BoundColl.GetBoundaryByStartTime(
  StartTime: double): TCustomBoundaryStorage;
var
  Index: Integer;
  Item: TCustomBoundaryStorage;
begin
  result := nil;
  for Index := 0 to FBoundaries.Count - 1 do
  begin
    Item := FBoundaries[Index];
    if Item.StartingTime = StartTime then
    begin
      result := Item;
      Exit;
    end;
    if Item.StartingTime < StartTime then
    begin
      result := Item;
    end;
    if Item.EndingTime > StartTime then
    begin
      Exit;
    end;

//    if (Item.StartingTime <= StartTime)
//      and (Item.EndingTime > StartTime) then
//    begin
//      result := Item;
//      Exit;
//    end;
  end;
  Assert(result <> nil);
end;

procedure TCustomMF_ArrayBoundColl.AssignCellsWithItem(
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; DataSets: TList;
  ListOfTimeLists: TList);
var
  BoundaryCount: Integer;
  DataArray2: TDataArray;
  TimeList2: TCustomTimeList;
  TimeIndex: Integer;
  DataArray1: TDataArray;
  TimeList1: TModflowTimeList;
//  Sections: T3DSparseIntegerArray;
begin
  Assert(ListOfTimeLists.Count >= 1);
  TimeList1 := ListOfTimeLists[0];
//  Sections := TimeList1.Sections[ItemIndex];
  DataArray1 := TimeList1[ItemIndex];
  DataSets.Add(DataArray1);
  for TimeIndex := 1 to ListOfTimeLists.Count - 1 do
  begin
    TimeList2 := ListOfTimeLists[TimeIndex];
    DataArray2 := TimeList2[ItemIndex];
    DataSets.Add(DataArray2);
    Assert(DataArray1.LayerCount = DataArray2.LayerCount);
    Assert(DataArray1.RowCount = DataArray2.RowCount);
    Assert(DataArray1.ColumnCount = DataArray2.ColumnCount);
  end;
  CountBoundaryCells(BoundaryCount, DataArray1, DataSets);
  SetBoundaryStartAndEndTime(BoundaryCount, Item, ItemIndex);
  AssignCellValues(DataSets, ItemIndex);
  for TimeIndex := 0 to ListOfTimeLists.Count - 1 do
  begin
    TimeList1 := ListOfTimeLists[TimeIndex];
    TimeList1.FreeItem(ItemIndex);
  end;
end;

procedure TCustomMF_ArrayBoundColl.CountBoundaryCells(var BoundaryCount: Integer;
  DataArray1: TDataArray; DataSets: TList);
var
  DSIndex: Integer;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  DataArray2: TDataArray;
  LocalModel: TPhastModel;
  LayerMin: Integer;
  RowMin: Integer;
  ColMin: Integer;
  LayerMax: Integer;
  RowMax: Integer;
  ColMax: Integer;
begin
  LocalModel := Model as TPhastModel;
  BoundaryCount := 0;
  DataArray1.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
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
            if DataArray1.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              Inc(BoundaryCount);
            end;
          end;
        end;
      end;
    end;
  end;
  for DSIndex := 1 to DataSets.Count - 1 do
  begin
    DataArray2 := DataSets[DSIndex];
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
              if DataArray1.IsValue[LayerIndex, RowIndex, ColIndex] then
              begin
                Assert(DataArray2.IsValue[LayerIndex, RowIndex, ColIndex]);
              end;
            end;
          end;
        end;
      end;
    end;
//    DataArray2.CacheData;
  end;
//  DataArray1.CacheData;
end;

function TCustomMF_BoundColl.GetBoundaryCount: integer;
begin
  result := FBoundaries.Count;
end;

function TCustomMF_BoundColl.GetParam: TModflowTransientListParameter;
var
  Model: TPhastModel;
begin
  if (FParamName <> '') and (self.Model <> nil) then
  begin
    Model := self.Model as TPhastModel;
    result := Model.ModflowTransientParameters.GetParamByName(FParamName);
  end
  else
  begin
    result := nil;
  end;
end;

function TCustomMF_BoundColl.GetParamName: string;
begin
  result := FParamName;
end;

function TCustomMF_BoundColl.GetTimeList(Index: integer): TModflowTimeList;
begin
  result := FTimeLists[Index];
end;

function TCustomMF_BoundColl.IsSame(
  AnOrderedCollection: TOrderedCollection): boolean;
var
  Collection: TCustomMF_BoundColl;
begin
  result := (AnOrderedCollection is TCustomMF_BoundColl);
  if result then
  begin
    Collection := TCustomMF_BoundColl(AnOrderedCollection);
    result := (Param = Collection.Param)
      and inherited IsSame(AnOrderedCollection);
  end;
end;

procedure TCustomMF_BoundColl.SetBoundaryCapacity(Value: integer);
begin
  FBoundaries.Capacity := Value;
end;

procedure TCustomMF_BoundColl.SetBoundaryStartAndEndTime(
  BoundaryCount: Integer; Item: TCustomModflowBoundaryItem; ItemIndex: Integer);
begin
  Boundaries[ItemIndex].StartingTime := Item.StartTime;
  Boundaries[ItemIndex].EndingTime := Item.EndTime;
end;

{ TModflowTimeList }
constructor TModflowTimeList.Create(Model, ScreenObject: TObject);
begin
  inherited Create(Model);
  FScreenObject := ScreenObject;
end;

procedure TModflowTimeList.Initialize(BoundaryValues: TBoundaryValueArray;
  ScreenObject: TObject; AssignmentLocation: TAssignmentLocation = alAll);
var
  LocalScreenObject: TScreenObject;
  Index: Integer;
  Time: double;
  DataArray: TCustomSparseDataSet;
  PhastModel: TPhastModel;
  Grid: TModflowGrid;
  Formula: string;
  StoredUpToDate: boolean;
  FirstUsedTime: Double;
  LastUsedTime: Double;
  Time1: Double;
  Time2: Double;
begin
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  if UpToDate then
    Exit;

  LocalScreenObject := ScreenObject as TScreenObject;
  Assert(LocalScreenObject <> nil);
  PhastModel := LocalScreenObject.Model as TPhastModel;
  Assert(PhastModel <> nil);

  FirstUsedTime := PhastModel.ModflowFullStressPeriods[0].StartTime;
  LastUsedTime := PhastModel.ModflowFullStressPeriods[
    PhastModel.ModflowFullStressPeriods.Count - 1].EndTime;

  FirstUsedTime := Math.Max(FirstUsedTime,
    PhastModel.ModflowStressPeriods[0].StartTime);
  LastUsedTime := Math.Min(LastUsedTime, PhastModel.ModflowStressPeriods[
    PhastModel.ModflowStressPeriods.Count - 1].EndTime);

  StoredUpToDate := PhastModel.UpToDate;
  try

    Clear;
    Grid := PhastModel.ModflowGrid;
    Assert(Grid <> nil);

    for Index := 0 to Length(BoundaryValues) - 1 do
    begin
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      Time := BoundaryValues[Index].Time;
      Formula := BoundaryValues[Index].Formula;
      DataArray := nil;
      case DataType of
        rdtDouble:
          begin
            DataArray := TTransientRealSparseDataSet.Create(PhastModel);
            DataArray.DataType := rdtDouble;
          end;
        rdtInteger:
          begin
            DataArray := TTransientIntegerSparseDataSet.Create(PhastModel);
            DataArray.DataType := rdtInteger;
          end;
        else Assert(False);
      end;
      DataArray.Name := ValidName(NonParamDescription) + '_' + IntToStr(Index+1);
      Add(Time, DataArray);
      DataArray.EvaluatedAt := eaBlocks;
      DataArray.Orientation := dso3D;
      DataArray.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
        Grid.ColumnCount);

//      Sections := T3DSparseIntegerArray.Create(SPASmall);
//      FSectionArrays.Add(Sections);

      Time1 := Time;
      Time2 := Time;
      if Index < Length(BoundaryValues) - 1 then
      begin
        Time2 := BoundaryValues[Index+1].Time;
      end;
      if (Time2 >= FirstUsedTime)
        and (Time1 <= LastUsedTime) then
      begin
        try
          LocalScreenObject.AssignValuesToModflowDataSet(Grid, DataArray, Formula,
            AssignmentLocation);
        except on E: ErbwParserError do
          begin
            frmFormulaErrors.AddError(LocalScreenObject.Name, Name, Formula,
              E.Message);
            Formula := '0';
            BoundaryValues[Index].Formula := Formula;
            LocalScreenObject.AssignValuesToModflowDataSet(Grid, DataArray,
              Formula, AssignmentLocation);
          end;
        end;
      end;
      PhastModel.DataArrayManager.CacheDataArrays;
      DataArray.UpToDate := True;
      DataArray.CacheData;
    end;

  finally
    PhastModel.UpToDate := StoredUpToDate;
  end;
end;

procedure TModflowTimeList.Invalidate;
begin
  if (FScreenObject <> nil)
    and (FScreenObject as TScreenObject).CanInvalidateModel then
  begin
    inherited;
  end;
end;

procedure TCustomMF_BoundColl.SetParam(
  const Value: TModflowTransientListParameter);
begin
  if Value = nil then
  begin
    ParamName := '';
  end
  else
  begin
    ParamName := Value.ParameterName;
  end;
//  FParamNameStorage.Param := Value;
end;

procedure TCustomMF_BoundColl.SetParamName(
  Value: string);
begin
  Value := StringReplace(Value, '"', '', [rfReplaceAll, rfIgnoreCase]);
  if FParamName <> Value then
  begin
    FParamName := Value;
    InvalidateModel;
  end;
end;

procedure TCustomMF_BoundColl.TestIfObservationsPresent(
  var EndOfLastStressPeriod, StartOfFirstStressPeriod: Double;
  var ObservationsPresent: Boolean);
begin
  ObservationsPresent := False;
  StartOfFirstStressPeriod := 0;
  EndOfLastStressPeriod := 0;
end;

function TCustomMF_BoundColl.TimeListCount: integer;
begin
  result := FTimeLists.Count;
end;

procedure TCustomMF_ListBoundColl.UpdataRequiredData(DataSets, Variables: TList;
  ACell: TObject);
var
  ADataSet: TDataArray;
  Variable: TCustomValue;
  VarIndex: Integer;
  Layer: Integer;
  Cell: TCellAssignment;
begin
  Cell := ACell as TCellAssignment;
  UpdateGlobalLocations(Cell.Column, Cell.Row, Cell.Layer, eaBlocks);
  UpdateCurrentSegment(Cell.Segment);
  UpdateCurrentSection(Cell.Section);
  for VarIndex := 0 to Variables.Count - 1 do
  begin
    Variable := Variables[VarIndex];
    ADataSet := DataSets[VarIndex];
    Layer := -1;
    case ADataSet.Orientation of
      dsoTop:
        begin
          Layer := 0;
        end;
      dso3D:
        begin
          Layer := Cell.Layer;
        end;
    else
      begin
        Assert(False);
      end;
    end;
    case ADataSet.DataType of
      rdtDouble:
        begin
          (Variable as TRealVariable).Value := ADataSet.RealData[Layer, Cell.Row, Cell.Column];
        end;
      rdtInteger:
        begin
          (Variable as TIntegerVariable).Value := ADataSet.IntegerData[Layer, Cell.Row, Cell.Column];
        end;
      rdtBoolean:
        begin
          (Variable as TBooleanVariable).Value := ADataSet.BooleanData[Layer, Cell.Row, Cell.Column];
        end;
      rdtString:
        begin
          (Variable as TStringVariable).Value := ADataSet.StringData[Layer, Cell.Row, Cell.Column];
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TModflowTimeList.SetUpToDate(const Value: boolean);
begin
  inherited;
  if not Value then
  begin
    if Assigned(OnInvalidate) then
    begin
      OnInvalidate(Self);
    end;
  end;

end;

{ TModflowParamItem }

procedure TModflowParamItem.Assign(Source: TPersistent);
begin
  if Source is TModflowParamItem then
  begin
    Param := TModflowParamItem(Source).Param;
  end;
  inherited;
end;

constructor TModflowParamItem.Create(Collection: TCollection);
var
  BC: TMF_BoundCollClass;
  ParameterCollection: TModflowParameters;
begin
  inherited;
  ParameterCollection := Collection as TModflowParameters;
  AlwaysAssignForeignId := True;
  BC := BoundaryClass;
  FParam := BC.Create(ParameterCollection.FBoundary,
    ParameterCollection.Model, ParameterCollection.ScreenObject);
end;

function TModflowParamItem.DataSetUsed(DataArray: TDataArray): boolean;
begin
  result := Param.DataSetUsed(DataArray);
end;

destructor TModflowParamItem.Destroy;
begin
  FParam.Free;
  inherited;
end;

function TModflowParamItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TModflowParamItem);
  if result then
  begin
    result := FParam.IsSame(TModflowParamItem(AnotherItem).FParam);
  end;
end;

procedure TModflowParamItem.SetParam(
  const Value: TCustomMF_BoundColl);
begin
  FParam.Assign(Value);
end;

function TModflowParamItem.Used: boolean;
begin
  result := Param.Used;
end;

{ TModflowParameters }

function TModflowParameters.Add: TModflowParamItem;
begin
  result := inherited Add as TModflowParamItem;
end;

constructor TModflowParameters.Create(Boundary: TModflowParamBoundary;
  ItemClass: TModflowParamItemClass; Model, ScreenObject: TObject);
begin
  inherited Create(ItemClass, Model);
  FBoundary := Boundary;
  Assert((ScreenObject = nil) or (ScreenObject is TScreenObject));
  FScreenObject := ScreenObject;
end;

function TModflowParameters.DataSetUsed(DataArray: TDataArray): boolean;
var
  Index: Integer;
begin
  result := false;
  for Index := 0 to Count - 1 do
  begin
    result := Items[Index].DataSetUsed(DataArray);
    if result then
    begin
      Exit;
    end;
  end;
end;

procedure TModflowParameters.EvaluateArrayBoundaries;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    (Items[Index].Param as TCustomMF_ArrayBoundColl).EvaluateArrayBoundaries;
  end;
end;

procedure TModflowParameters.EvaluateListBoundaries;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    (Items[Index].Param as TCustomMF_ListBoundColl).EvaluateListBoundaries;
  end;
end;

function TModflowParameters.GetItem(Index: Integer): TModflowParamItem;
begin
  result := inherited Items[Index] as TModflowParamItem;
end;

function TModflowParameters.GetParamByName(
  const AName: string): TModflowParamItem;
var
  ParamIndex: Integer;
  Item: TModflowParamItem;
begin
  result := nil;
  for ParamIndex := 0 to Count - 1 do
  begin
    Item := Items[ParamIndex];
    if Item.Param.ParamName = AName then
    begin
      result := Item;
      Exit;
    end;
  end;
end;

function TModflowParameters.IndexOfParam(
  AParam: TModflowTransientListParameter): integer;
var
  ParamIndex: Integer;
  Item: TModflowParamItem;
begin
  result := -1;
  for ParamIndex := 0 to Count - 1 do
  begin
    Item := Items[ParamIndex];
    if Item.Param.ParamName = AParam.ParameterName then
    begin
      result := ParamIndex;
      Exit;
    end;
  end;
end;

function TModflowParameters.Insert(Index: Integer): TModflowParamItem;
begin
  result := inherited Insert(Index) as TModflowParamItem;
end;

procedure TModflowParameters.SetItem(Index: Integer;
  const Value: TModflowParamItem);
begin
  inherited Items[Index] := Value
end;

function TModflowParameters.Used: boolean;
var
  Index: Integer;
  Item: TModflowParamItem;
begin
  result := False;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    result := Item.Used;
    if result then
    begin
      Exit;
    end;
  end;
end;

{ TModflowBoundary }

procedure TModflowParamBoundary.Assign(Source: TPersistent);
var
  Boundary: TModflowParamBoundary;
begin
  if Source is TModflowParamBoundary then
  begin
    Boundary := TModflowParamBoundary(Source);
//    Values := Boundary.Values;
    Parameters := Boundary.Parameters;
  end;
  inherited;
end;

procedure TModflowParamBoundary.Clear;
begin
  inherited;
  Parameters.Clear;
end;

procedure TModflowParamBoundary.ClearBoundaries;
var
  Index: Integer;
  ParamItem: TModflowParamItem;
begin
  inherited;
  for Index := 0 to Parameters.Count - 1 do
  begin
    ParamItem := Parameters[Index];
    ParamItem.Param.ClearBoundaries;
  end;
end;

procedure TModflowParamBoundary.ClearTimeLists;
var
  Index: Integer;
  ParamItem: TModflowParamItem;
begin
  inherited;
  for Index := 0 to Parameters.Count - 1 do
  begin
    ParamItem := Parameters[Index];
    ParamItem.Param.ClearTimeLists;
  end;
end;

constructor TModflowParamBoundary.Create(Model, ScreenObject: TObject);
begin
  inherited Create(Model, ScreenObject);
//  Assert((ScreenObject = nil) or (ScreenObject is TScreenObject));
//  FScreenObject := ScreenObject;
//  Assert((Model = nil) or (Model is TPhastModel));
//  FPhastModel := Model;
//  FValues:= BoundaryCollectionClass.Create(self, Model,
//    ScreenObject);
  FParameters := TModflowParameters.Create(self, ModflowParamItemClass, Model,
    ScreenObject);
end;

function TModflowParamBoundary.DataSetUsed(DataArray: TDataArray): boolean;
begin
  result := inherited DataSetUsed(DataArray)
    or FParameters.DataSetUsed(DataArray);
end;

procedure TModflowParamBoundary.DeleteParam(Param: TModflowParameter);
var
  Index: integer;
begin
  Index := FParameters.IndexOfParam(Param as TModflowTransientListParameter);
  if Index >= 0 then
  begin
    FParameters.Delete(Index);
  end;
end;

destructor TModflowParamBoundary.Destroy;
begin
  FParameters.Free;
//  FValues.Free;
  inherited;
end;

procedure TModflowParamBoundary.EvaluateArrayBoundaries;
var
  Model: TPhastModel;
begin
  Model := PhastModel as TPhastModel;
  if Model.ModflowTransientParameters.CountParam(ParameterType) > 0 then
  begin
    Parameters.EvaluateArrayBoundaries;
  end
  else
  begin
    inherited;
  end;
end;

procedure TModflowParamBoundary.EvaluateListBoundaries;
begin
  Parameters.EvaluateListBoundaries;
  inherited;
end;

procedure TModflowParamBoundary.SetParameters(const Value: TModflowParameters);
begin
  FParameters.Assign(Value);
end;

procedure TModflowParamBoundary.UpdateTimes(Times: TRealList);
var
  ParamIndex: Integer;
  Param: TModflowParamItem;
begin
  inherited;
  for ParamIndex := 0 to Parameters.Count - 1 do
  begin
    Param := Parameters[ParamIndex];
    AddBoundaryTimes(Param.Param, Times);
  end;
end;

function TModflowParamBoundary.Used: boolean;
begin
  result := inherited Used or Parameters.Used
end;           

procedure TSpecificModflowBoundary.Assign(Source: TPersistent);
begin
  if Source is TSpecificModflowBoundary then
  begin
    FormulaInterpretation :=
      TSpecificModflowBoundary(Source).FormulaInterpretation;
  end;
  inherited;
end;

procedure TModflowBoundary.Assign(Source: TPersistent);
var
  Boundary:  TModflowBoundary;
begin
  if Source is TModflowBoundary then
  begin
    Boundary := TModflowBoundary(Source);
    Values := Boundary.Values;
  end
  else
  begin
    inherited;
  end;
end;

procedure TModflowBoundary.Clear;
begin
  Values.Clear;
end;

procedure TModflowBoundary.ClearBoundaries;
begin
  FValues.ClearBoundaries;
end;

procedure TModflowBoundary.ClearTimeLists;
begin
  Values.ClearTimeLists;
end;

constructor TModflowBoundary.Create(Model, ScreenObject: TObject);
begin
  inherited;
//  Assert((ScreenObject = nil) or (ScreenObject is TScreenObject));
//  FScreenObject := ScreenObject;
//  Assert((Model = nil) or (Model is TPhastModel));
//  FPhastModel := Model;
  FValues:= BoundaryCollectionClass.Create(self, Model,
    ScreenObject);
end;

function TModflowBoundary.DataSetUsed(DataArray: TDataArray): boolean;
begin
  result := Values.DataSetUsed(DataArray);
end;

destructor TModflowBoundary.Destroy;
begin
  FValues.Free;
  inherited;
end;

procedure TModflowBoundary.EvaluateArrayBoundaries;
begin
  (Values as TCustomMF_ArrayBoundColl).EvaluateArrayBoundaries;
end;

procedure TModflowBoundary.EvaluateListBoundaries;
begin
  (Values as TCustomMF_ListBoundColl).EvaluateListBoundaries;
end;

procedure TModflowBoundary.InvalidateDisplay;
begin
  // do nothing
end;

procedure TModflowBoundary.SetValues(const Value: TCustomMF_BoundColl);
begin
  FValues.Assign(Value);
end;

procedure TModflowBoundary.UpdateTimes(Times: TRealList);
begin
  AddBoundaryTimes(Values, Times);
end;

function TModflowBoundary.Used: boolean;
begin
  result := Values.Used;
end;

procedure TModflowBoundary.AddBoundaryTimes(
  BoundCol: TCustomNonSpatialBoundColl; Times: TRealList);
var
  BoundaryIndex: Integer;
  Boundary: TCustomModflowBoundaryItem;
begin
  for BoundaryIndex := 0 to BoundCol.Count - 1 do
  begin
    Boundary := BoundCol[BoundaryIndex];
    Times.AddUnique(Boundary.StartTime);
    Times.AddUnique(Boundary.EndTime);
  end;
end;

function TModflowBoundary.NonParameterColumns: integer;
begin
  result := 2 + Values.TimeListCount;
end;

procedure TSpecificModflowBoundary.SetFormulaInterpretation(
  const Value: TFormulaInterpretation);
begin
  if FFormulaInterpretation <> Value then
  begin
    FFormulaInterpretation := Value;
    InvalidateModel;
    InvalidateDisplay;
  end;
end;

{ TCustomNonSpatialBoundColl }

procedure TCustomNonSpatialBoundColl.Assign(Source: TPersistent);
var
  List: TList;
  Index: Integer;
  Item1, Item2: TCustomModflowBoundaryItem;
begin
  inherited;
  List := TList.Create;
  try
    for Index := 0 to Count - 1 do
    begin
      List.Add(Items[Index]);
    end;
    List.Sort(SortBoundaryItems);
    for Index := 0 to List.Count - 1 do
    begin
      Item1 := List[Index];
      Item1.Index := Index;
    end;
    for Index := Count - 1 downto 1 do
    begin
      Item1 := Items[Index-1];
      Item2 := Items[Index];
      if Item2.StartTime < Item1.EndTime then
      begin
        Item2.StartTime := Item1.EndTime;
      end;
      if Item2.StartTime >= Item2.EndTime then
      begin
        Delete(Index);
      end;
    end;
  finally
    List.Free;
  end;
end;

constructor TCustomNonSpatialBoundColl.Create(Boundary: TModflowBoundary; Model,
  ScreenObject: TObject);
begin
  inherited Create(ItemClass, Model);
  FBoundary := Boundary;
  Assert((ScreenObject = nil) or (ScreenObject is TScreenObject));
  FScreenObject := ScreenObject;
end;

function TCustomNonSpatialBoundColl.Used: boolean;
begin
  result := Count > 0;
end;

procedure TCustomNonSpatialBoundColl.SetItem(Index: Integer;
  const Value: TCustomModflowBoundaryItem);
begin
  inherited Items[Index] := Value;
end;

function TCustomNonSpatialBoundColl.GetItem(
  Index: Integer): TCustomModflowBoundaryItem;
begin
  result := inherited Items[Index] as TCustomModflowBoundaryItem
end;

constructor TCustomModflowBoundaryItem.Create(Collection: TCollection);
var
  Index: integer;
  Observer: TObserver;
  LocalScreenObject: TScreenObject;
begin
  inherited;
  CreateFormulaObjects;
  FObserverList := TObjectList.Create;
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    Observer := TObserver.Create(nil);
    FObserverList.Add(Observer);
    LocalScreenObject := ScreenObject as TScreenObject;
    if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
    begin
      LocalScreenObject.TalksTo(Observer);
    end;
    BoundaryFormula[Index] := '0';
  end;
  AssignObserverEvents(Collection);
end;

destructor TCustomModflowBoundaryItem.Destroy;
var
  LocalScreenObject: TScreenObject;
  Observer: TObserver;
  Index: integer;
  PhastModel: TPhastModel;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    PhastModel := Model as TPhastModel;
    if not PhastModel.Clearing and not (csDestroying in PhastModel.ComponentState) then
    begin
      for Index := 0 to FObserverList.Count - 1 do
      begin
        Observer := FObserverList[Index];
        LocalScreenObject.StopsTalkingTo(Observer);
      end;
    end;
  end;
  RemoveFormulaObjects;
  FObserverList.Free;
  inherited;
end;

function TCustomModflowBoundaryItem.GetScreenObject: TObject;
begin
  result := nil;
  if Collection = nil then
  begin
    Exit;
  end;
  result := (Collection as TCustomNonSpatialBoundColl).FScreenObject;
end;

{ TCustomBoundaryStorage }

destructor TCustomBoundaryStorage.Destroy;
begin
  if FileExists(FTempFileName) then
  begin
    DeleteFile(FTempFileName);
  end;
  inherited;
end;

procedure TCustomBoundaryStorage.RestoreData;
var
  DecompressionStream: TDecompressionStream;
  Annotations: TStringList;
  MemStream: TMemoryStream;
  Count: Integer;
  Index: Integer;
begin
  Assert(FCached);
  Assert(FCleared);
  Annotations := TStringList.Create;
  MemStream := TMemoryStream.Create;
  try
    ExtractAFile(FTempFileName, MemStream);
    DecompressionStream := TDecompressionStream.Create(MemStream);
    try
      Annotations.Sorted := True;
      Annotations.Duplicates := dupIgnore;
      DecompressionStream.Read(Count, SizeOf(Count));
      Annotations.Capacity := Count;
      for Index := 0 to Count - 1 do
      begin
        Annotations.Add(ReadCompStringSimple(DecompressionStream));
      end;

      Restore(DecompressionStream, Annotations);
    finally
      DecompressionStream.Free;
    end;
  finally
    MemStream.Free;
    Annotations.Free;
    FCleared := False;
  end;
end;

procedure TCustomBoundaryStorage.CacheData;
var
  MemStream: TMemoryStream;
  Compressor: TCompressionStream;
begin
  if not FCached then
  begin
    if FTempFileName = '' then
    begin
      FTempFileName := TempFileName;
    end;
    MemStream:= TMemoryStream.Create;
    try
      Compressor := TCompressionStream.Create(clDefault, MemStream);
      try
        MemStream.Position := 0;
        Store(Compressor);
      finally
        Compressor.Free;
      end;
      MemStream.Position := 0;
      ZipAFile(FTempFileName, MemStream);
    finally
      MemStream.Free;
    end;
    FCached := True;
  end;
  Clear;
end;

{ TNoFormulaItem }

procedure TNoFormulaItem.AssignObserverEvents(Collection: TCollection);
begin
  // do nothing.
end;

function TNoFormulaItem.BoundaryFormulaCount: integer;
begin
  result := 0;
end;

procedure TNoFormulaItem.CreateFormulaObjects;
begin
  // do nothing.
end;

function TNoFormulaItem.GetBoundaryFormula(Index: integer): string;
begin
  result := '';
  Assert(False);
end;

procedure TNoFormulaItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  // do nothing
end;

procedure TNoFormulaItem.RemoveFormulaObjects;
begin
  // do nothing.
end;

procedure TNoFormulaItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  Assert(False);
end;

procedure TCustomModflowBoundaryItem.UpdateFormula(Value: string;
  Position: Integer; var FormulaObject: TFormulaObject);
var
  ParentModel: TPhastModel;
  Compiler: TRbwParser;
  Observer: TObserver;
begin
  if FormulaObject.Formula <> Value then
  begin
    ParentModel := Model as TPhastModel;
    if ParentModel <> nil then
    begin
      Compiler := ParentModel.rpThreeDFormulaCompiler;
      Observer := FObserverList[Position];
      UpdateFormulaDependencies(FormulaObject.Formula, Value, Observer, Compiler);
    end;
    InvalidateModel;

    if not (csDestroying in frmGoPhast.PhastModel.ComponentState) then
    begin
      frmGoPhast.PhastModel.FormulaManager.ChangeFormula(
        FormulaObject, Value, frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
        GlobalRemoveModflowBoundarySubscription,
        GlobalRestoreModflowBoundarySubscription, self);
    end;
  end;
end;

procedure GlobalRemoveModflowBoundarySubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TCustomModflowBoundaryItem).RemoveSubscription(Sender, AName);
end;

procedure GlobalRestoreModflowBoundarySubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TCustomModflowBoundaryItem).RestoreSubscription(Sender, AName);
end;

procedure TCustomModflowBoundaryItem.RemoveSubscription(Sender: TObject; const AName: string);
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

procedure TCustomModflowBoundaryItem.RestoreSubscription(Sender: TObject; const AName: string);
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

function TCustomModflowBoundaryItem.CreateFormulaObject(
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
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);
end;

{ TModflowScreenObjectProperty }

constructor TModflowScreenObjectProperty.Create(Model, ScreenObject: TObject);
begin
  inherited Create;
  Assert((ScreenObject = nil) or (ScreenObject is TScreenObject));
  FScreenObject := ScreenObject;
  Assert((Model = nil) or (Model is TPhastModel));
  FPhastModel := Model;
end;

procedure TModflowScreenObjectProperty.InvalidateModel;
begin
  if (ScreenObject <> nil)
      and (ScreenObject as TScreenObject).CanInvalidateModel
      and (FPhastModel <> nil) then
  begin
    (FPhastModel as TPhastModel).Invalidate
  end;
end;

end.
