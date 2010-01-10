{@abstract(The main purposes of @name is to define @link(TDataArray) which
  provides an interface to a 3D array of data and
  @link(TCustom2DInterpolater) which provides an interface
  for 2D interpolation.)
  @seealso(PhastDataSets)
  @seealso(ModflowConstantHeadBoundaryUnit)

  @author(Richard B. Winston <rbwinst@usgs.gov>)
  }
unit DataSetUnit;

interface

uses Windows, Math, ZLib, GR32, TempFiles, IntListUnit, RealListUnit, SysUtils,
  Classes, Forms, RbwParser, FastGEO, GoPhastTypes, SubscriptionUnit,
  SparseDataSets, ObserverIntfU, FormulaManagerUnit;

{ TODO :
Consider making dual data sets that can be evaluated at both elements 
and nodes. }  

type
  // @name is a 3D array of real numbers.
  T3DRealDataSet = array of array of array of real;

  // @name is a 3D array of integers.
  T3DIntegerDataSet = array of array of array of integer;

  // @name is a 3D array of booleans.
  T3DBooleanDataSet = array of array of array of boolean;

  // @name is a 3D array of strings.
  T3DStringDataSet = array of array of array of string;

  // @abstract(@name is raised if a formula is assigned that, when compiled
  // gives a type of data that is incompatible with the type of data
  // stored in the @link(TDataArray).)
  EInvalidDataType = class(Exception);

  // @abstract(@name is raised if an error occurs during interpolation.)
  EInterpolationException = class(Exception);

  // @abstract(@name is raised if, when
  // assigning values to a data set, a circular
  // reference is encountered so that the @link(TDataArray) depends on itself.)
  ECircularReference = class(Exception);

  // @name is the type of the TDataArray.@link(TDataArray.OnDataSetUsed) event.
  // TDataArray.@link(TDataArray.OnDataSetUsed)
  // is called by TDataArray.@link(TDataArray.UsedByModel)
  TObjectUsedEvent = function(Sender: TObject): boolean of object;

  TCustom2DInterpolater = class;

  // @name is used in @link(TDataArray.ChangeAFormula)
  // where it is used to get the @link(TObserver)s that
  // will affect the @link(TDataArray) whose formula is
  // being changed.
  TUseListFunction = function: TStringList of Object;

  {@abstract(When the grid is colored by a @link(TDataArray), the
   colors can be restricted to lie within a range.
   @name is used to represent one end of the range.
   See @link(TColoringLimits).)}
  TColoringLimit = class(TPersistent)
  private
    // See @link(BooleanLimitValue).
    FBooleanLimitValue: boolean;
    // See @link(DataType).
    FDataType: TRbwDataType;
    // See @link(DefaultBooleanLimitValue).
    FDefaultBooleanLimitValue: boolean;
    // See @link(IntegerLimitValue).
    FIntegerLimitValue: integer;
    // See @link(RealLimitValue).
    FRealLimitValue: double;
    // See @link(StringLimitValue).
    FStringLimitValue: string;
    // See @link(UseLimit).
    FUseLimit: boolean;
    // See @link(OnChange).
    FOnChange: TNotifyEvent;
    // See @link(BooleanLimitValue).
    procedure SetBooleanLimitValue(const Value: boolean);
    // See @link(DataType).
    procedure SetDataType(const Value: TRbwDataType);
    // See @link(IntegerLimitValue).
    procedure SetIntegerLimitValue(const Value: integer);
    // See @link(RealLimitValue).
    procedure SetRealLimitValue(const Value: double);
    // See @link(StringLimitValue).
    procedure SetStringLimitValue(const Value: string);
    // See @link(UseLimit).
    procedure SetUseLimit(const Value: boolean);
  public
    // @name copies those parts of Value that are used
    // to the current instance of @classname (except for OnChange).
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    constructor Create;
    // @name is the value to used for @link(BooleanLimitValue) when
    // @link(UseLimit) is @false.
    property DefaultBooleanLimitValue: boolean read FDefaultBooleanLimitValue
      write FDefaultBooleanLimitValue;
    // If there is a change in @link(UseLimit),
    // @link(BooleanLimitValue), @link(IntegerLimitValue),
    // @link(RealLimitValue), or @link(StringLimitValue),
    // @name can be used to respond to that change.
    // Changing to @link(DataType) does not cause an @name event to occur.
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    // If @link(DataType) is rdtBoolean and @link(UseLimit) is @True,
    // @name is the boolean value used to limit what values will
    // be used to color the grid.
    property BooleanLimitValue: boolean read FBooleanLimitValue write
      SetBooleanLimitValue;
    // @name indicates the data type (real, integer, boolean, or string)
    //  of the @link(TDataArray) to
    // which this limit applies.
    property DataType: TRbwDataType read FDataType write SetDataType;
    // If @link(DataType) is rdtInteger and @link(UseLimit) is @True,
    // @name is the integer value used to limit what values will
    // be used to color the grid.
    property IntegerLimitValue: integer read FIntegerLimitValue
      write SetIntegerLimitValue;
    // If @link(DataType) is rdtDouble and @link(UseLimit) is @True,
    // @name is the real value used to limit what values will
    // be used to color the grid.
    property RealLimitValue: double read FRealLimitValue write
      SetRealLimitValue;
    // If @link(DataType) is rdtString and @link(UseLimit) is @True,
    // @name is the string value used to limit what values will
    // be used to color the grid.
    property StringLimitValue: string read FStringLimitValue write
      SetStringLimitValue;
    // @name indicates whether this @classname should be used to limit
    // what values are used to color the grid.
    property UseLimit: boolean read FUseLimit write SetUseLimit;
  end;

  TSkipReal = class(TCollectionItem)
  private
    FRealValue: double;
    procedure SetRealValue(const Value: double);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property RealValue: double read FRealValue write SetRealValue;
  end;

  TSkipRealCollection = class(TCollection)
  public
    constructor Create;
  end;

  TSkipInteger = class(TCollectionItem)
  private
    FIntegerValue: integer;
    procedure SetIntegerValue(const Value: integer);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property IntegerValue: integer read FIntegerValue write SetIntegerValue;
  end;

  TSkipIntegerCollection = class(TCollection)
  public
    constructor Create;
  end;

  {@abstract(When the grid is colored by a @link(TDataArray), the
   colors can be restricted to lie within a range.
   @name is used to represent both ends of the range.
   See @link(TColoringLimit).)}
  TColoringLimits = class(TPersistent)
  private
    // See @link(LowerLimit).
    FLowerLimit: TColoringLimit;
    // See @link(UpperLimit).
    FUpperLimit: TColoringLimit;
    FActiveOnly: boolean;
    FIntegerValuesToSkip: TSkipIntegerCollection;
    FRealValuesToSkip: TSkipRealCollection;
    FStringValuesToSkip: TStrings;
    FLogTransform: boolean;
    // See @link(LowerLimit).
    procedure SetLowerLimit(const Value: TColoringLimit);
    // See @link(UpperLimit).
    procedure SetUpperLimit(const Value: TColoringLimit);
    procedure SetActiveOnly(const Value: boolean);
    procedure SetIntegerValuesToSkip(const Value: TSkipIntegerCollection);
    procedure SetRealValuesToSkip(const Value: TSkipRealCollection);
    procedure SetStringValuesToSkip(const Value: TStrings);
    function StoreRealSkipValues: boolean;
    function StoreIntegerSkipValues: boolean;
    procedure SetLogTransform(const Value: boolean);
  public
    // @name calls TColoringLimit.@link(TColoringLimit.Assign)
    // for @link(LowerLimit) and @link(UpperLimit) and then calls
    // @link(Update).
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    constructor Create;
    // @name destroys the the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    // If both the @link(LowerLimit) and @link(UpperLimit) are used
    // (see TColoringLimit.@link(TColoringLimit.UseLimit)),
    // @name insures that the upper limit is greater than or
    // equal to the lower limit.
    procedure Update;
    function ValueOk(AValue: double): boolean; overload;
    function ValueOk(AValue: integer): boolean; overload;
    function ValueOk(const AValue: String): boolean; overload;

  published
    property ActiveOnly: boolean read FActiveOnly write SetActiveOnly stored FActiveOnly;
    // @name is the lower limit on what values should be used to color
    // the grid.
    property LowerLimit: TColoringLimit read FLowerLimit write SetLowerLimit;
    // @name is the upper limit on what values should be used to color
    // the grid.
    property UpperLimit: TColoringLimit read FUpperLimit write SetUpperLimit;
    property RealValuesToSkip: TSkipRealCollection
      read FRealValuesToSkip write SetRealValuesToSkip
      stored StoreRealSkipValues;
    property IntegerValuesToSkip: TSkipIntegerCollection
      read FIntegerValuesToSkip write SetIntegerValuesToSkip
      stored StoreIntegerSkipValues;
    property StringValuesToSkip: TStrings read FStringValuesToSkip
      write SetStringValuesToSkip;
    property LogTransform: boolean read FLogTransform write SetLogTransform;
  end;

  // @name is the data type of @link(TDataArray.IsUniform).
  TIsUniform = (iuUnknown, iuFalse, iuTrue);

  TCustomTimeList = Class;

  TContours = class(TObject)
  private
    FSpecifyContours: boolean;
    FLineThicknesses: TOneDRealArray;
    FContourColors: TArrayOfColor32;
    FContourValues: TOneDRealArray;
    FAutomaticColors: boolean;
    FLogTransform: boolean;
    function GetCount: integer;
    procedure SetCount(const Value: integer);
  public
    Constructor Create;
    property AutomaticColors: boolean read FAutomaticColors
      write FAutomaticColors;
    property Count: integer read GetCount write SetCount;
    procedure Assign(Source: TContours);
    property SpecifyContours: boolean read FSpecifyContours
      write FSpecifyContours;
    property ContourValues: TOneDRealArray read FContourValues
      write FContourValues;
    property LineThicknesses: TOneDRealArray read FLineThicknesses
      write FLineThicknesses;
    property ContourColors: TArrayOfColor32 read FContourColors
      write FContourColors;
    property LogTransform: boolean read FLogTransform write FLogTransform;
  end;

  {@abstract(@name provides an interface to a 3D array of data.)

  All descendants of @name that are instantiated must be registered
  with the system using RegisterClass in the initialization section
  of the unit in which they are declared.
  }
  TDataArray = class(TObserver)
  private
    // See @link(Annotation).
    FAnnotation: T3DStringDataSet;
    // See @link(CheckMax).
    FCheckMax: boolean;
    // See @link(CheckMin).
    FCheckMin: boolean;
    // See @link(ColumnCount).
    FColumnCount: integer;
    // Depending on @link(Datatype), @name will be either a
    // @link(T3DRealDataSet), @link(T3DIntegerDataSet),
    // @link(T3DBooleanDataSet), or @link(T3DStringDataSet).
    //
    // See @link(RealData), @link(IntegerData),
    // @link(BooleanData), and @link(StringData),
    FDataArray: pointer;
    // See @link(Datatype).
    FDataType: TRbwDataType;
    // See @link(DimensionsChanged).
    FDimensionsChanged: boolean;
    // See @link(EvaluatedAt).
    FEvaluatedAt: TEvaluatedAt;
    // @name is used as temporary storage when changing a formula.
    // See @link(Formula).
    FFormula: string;
    // See @link(LayerCount).
    FLayerCount: integer;
    // See @link(Limits).
    FLimits: TColoringLimits;
    // See @link(Lock).
    FLock: TDataLock;
    // See @link(Max).
    FMax: double;
    // See @link(Min).
    FMin: double;
    // See @link(Orientation).
    FOrientation: TDataSetOrientation;
    // See @link(RowCount).
    FRowCount: integer;
    // See @link(TwoDInterpolator).
    FTwoDInterpolator: TCustom2DInterpolater;
    // See @link(Units).
    FUnits: string;
    // See @link(UseList).
    FUseList: TStringList;
    // @name is used to indicate whether the values in @link(UseList)
    // need to be regenerated.  Changing @link(Formula) causes
    // @name to be set to @false. Calling @link(UpdateUseList) causes
    // @name to be set to @true.
    FUseListUpToDate: boolean;
    // See @link(Visible).
    FVisible: boolean;
    // See @link(OnDataSetUsed).
    FOnDataSetUsed: TObjectUsedEvent;
    // See @link(TCustomTimeList).
    FTimeList: TCustomTimeList;
    // See @link(Classification).
    FClassification: string;
    // See @link(OnPostInitialize).
    FOnPostInitialize: TNotifyEvent;
    // See @link(OnDestroy).
    FOnDestroy: TNotifyEvent;
    // See @link(ParameterUsed).
    FParameterUsed: boolean;
    // See @link(ParameterFormula).
    FParameterFormula: string;
    // See @link(UniformStringValue).
    FUniformStringValue: string;
    // See @link(UniformIntegerValue).
    FUniformIntegerValue: integer;
    // See @link(UniformAnnotation).
    FUniformAnnotation: string;
    // See @link(UniformBooleanValue).
    FUniformBooleanValue: boolean;
    // See @link(UniformRealValue).
    FUniformRealValue: double;
    // When the @Classname is cached, @name stores the number of layers.
    FCachedLayerCount: Integer;
    // When the @Classname is cached, @name stores the number of rows.
    FCachedRowCount: Integer;
    // When the @Classname is cached, @name stores the number of columns.
    FCachedColumnCount: Integer;
    // see @link(Comment).
    FComment: string;
    // see @link(AssociatedDataSets).
    FAssociatedDataSets: string;
    // See @link(MaxValue).
    FMaxValue: string;
    // See @link(MinValue).
    FMinValue: string;
    FMinReal: double;
    FMaxReal: double;
    FMinInteger: Integer;
    FMaxInteger: Integer;
    FMinBoolean: Boolean;
    FMaxBoolean: Boolean;
    FMinString: string;
    FMaxString: string;
    FContourLimits: TColoringLimits;
    FContours: TContours;
    FFormulaObject: TFormulaObject;
    FHash: longint;
    // See @link(TwoDInterpolatorClass).
    function GetTwoDInterpolatorClass: string;
    // @name is called if an invalid formula has been specified.
    // @name changes the formula to something that is sure to work.
    procedure ResetFormula(const Compiler: TRbwParser;
      const ErrorMessage: string);
    // See @link(Formula).
    procedure SetFormula(const Value: string);
    // See @link(Limits).
    procedure SetLimits(const Value: TColoringLimits);
    // See @link(Lock).
    procedure SetLock(const Value: TDataLock);
    // See @link(TwoDInterpolator).
    procedure SetTwoDInterpolator(const Value: TCustom2DInterpolater);
    // See @link(TwoDInterpolatorClass).
    procedure SetTwoDInterpolatorClass(const Value: string);
    // See @link(Units).
    procedure SetUnits(const Value: string);
    // See @link(Visible).
    procedure SetVisible(const Value: boolean);
    // @name updates @link(UseList) with the variables
    // used by @link(Formula).
    procedure UpdateUseList;
    // See @link(Classification).
    procedure SetClassification(const Value: string);
    // See @link(Classification).
    function GetClassification: string;
    // See @link(ParameterFormula).
    procedure SetParameterFormula(const Value: string);
    // See @link(ParameterUsed).
    procedure SetParameterUsed(const Value: boolean);
    // @name gets the array used to store the boolean data.
    procedure GetBoolArray(var AnArray: T3DBooleanDataSet);
    // @name gets the array used to store the string data.
    procedure GetStringArray(var AnArray: T3DStringDataSet);
    // @name gets the array used to store the real-number data.
    procedure GetRealArray(var AnArray: T3DRealDataSet);
    // @name gets the array used to store the integer data.
    procedure GetIntegerArray(var AnArray: T3DIntegerDataSet);
    // See @link(LayerCount).
    function GetLayerCount: integer;
    // See @link(ColumnCount).
    function GetColumnCount: integer;
    // See @link(RowCount).
    function GetRowCount: integer;
    // See @link(Comment).
    procedure SetComment(const Value: string);
    // See @link(MaxValue).
    function GetMaxValue: string;
    // See @link(MinValue).
    function GetMinValue: string;
    procedure SetContourLimits(const Value: TColoringLimits);
    procedure ContourLimitsChanged(Sender: TObject);
    procedure SetContours(const Value: TContours);
    function GetFormula: string;
    function ValueOK(const Layer, Row, Col: Integer;
      LocalLimits: TColoringLimits): Boolean;
    function GetHash: longint;
  protected
    // See @link(IsUniform).
    FIsUniform: TIsUniform;
    // @name is the @link(TPhastModel) that owns the @classname.
    FPhastModel: TComponent;
    // @name is @true if the @classname has been cleared.
    FCleared: boolean;
    // @name is true if the @classname data has been stored in a temporary file.
    FDataCached: boolean;
    // @name is the name of the temporary file
    // used to store the @classname data.
    FTempFileName: string;
    // When values are beginning to be assigned to a @classname,
    // @name is set to to the current time.
    FEvalTime: Extended;
    // See @link(EvaluatedAt).
    procedure SetEvaluatedAt(const Value: TEvaluatedAt); virtual;
    function GetCompiler: TRbwParser;
    // See @link(Orientation).
    procedure SetOrientation(const Value: TDataSetOrientation); virtual;
    // @name sets @link(FEvalTime) to the current time.
    procedure UpdateEvalTime;
    // @name returns the dimensions of the @classname.
    procedure GetLimits(out ColLimit, RowLimit, LayerLimit: Integer);
    // @name reads the data that has been previously stored in a temporary file.
    procedure ReadData(DecompressionStream: TDecompressionStream); virtual;
    // @name returns the dimensions of the @classname and the number of values
    // stored in the @classname.
    procedure CountValues(out LayerLimit, RowLimit, ColLimit, Count: Integer);
    // @name stores the data in @classname to a temporary file.
    procedure StoreData(Compressor: TCompressionStream); virtual;
    // @name restores the dimensions of the @classname to what they should be.
    procedure RestoreArraySize;
    // @name changes OldFormula to NewFormula and in the process updates
    // the list of @link(TObserver)s that affect the @classname.
    procedure ChangeAFormula(const NewFormula: string; var OldFormula: string;
      var UseListUpToDate: boolean; UseListFunction: TUseListFunction);
    // @name indicates that the dimensions of the data set have changed.
    // This can happen either by the number of rows, columns, or layers
    // in the grid has changed, because @link(EvaluatedAt) has changed,
    // or because the @link(Orientation) has changed.
    property DimensionsChanged: boolean read FDimensionsChanged;
    // See @link(Annotation).
    function GetAnnotation(const Layer, Row, Col: integer): string; virtual;
    // See @link(BooleanData).
    function GetBooleanData(const Layer, Row, Col: integer): boolean; virtual;
    // See @link(IntegerData).
    function GetIntegerData(const Layer, Row, Col: integer): integer; virtual;
    // See @link(IsValue).
    function GetIsValue(const Layer, Row, Col: Integer): boolean;
      virtual;
    // @name returns frmGoPhast.@link(TfrmGoPhast.PhastModel).
    function GetOwner: TPersistent; override;
    // See @link(RealData).
    function GetRealData(const Layer, Row, Col: integer): double; virtual;
    // @name gets the number of dimensions that @classname needs to have.
    procedure GetRequiredDimensions(out NumberOfLayers, NumberOfRows,
      NumberOfColumns: integer);
    // See @link(StringData).
    function GetStringData(const Layer, Row, Col: integer): string; virtual;
    // See @link(UseList).
    function GetUseList: TStringList; virtual;
    // @name is the event handler for
    // TColoringLimit.@link(TColoringLimit.OnChange).
    // If this @classname is being used to color the grid,
    // @name causes the colors displayed on the grid to be
    // recalculated and the display to be updated.
    procedure LimitsChanged(Sender: TObject);
    // See @link(Annotation).
    procedure SetAnnotation(const Layer, Row, Col: integer;
      const Value: string); virtual;
    // See @link(BooleanData).
    procedure SetBooleanData(const Layer, Row, Col: integer;
      const Value: boolean); virtual;
    // See @link(Datatype).
    procedure SetDataType(const Value: TRbwDataType); virtual;
    // @name sets the size of the array of data stored by
    // @classname to be set to the correct size.  If SetToZero
    // is true, the dimensions are all set to zero.
    procedure SetDimensions(const SetToZero: boolean = False); virtual;
    // See @link(IntegerData).
    procedure SetIntegerData(const Layer, Row, Col, Value: integer); virtual;
    // See @link(IsValue).
    procedure SetIsValue(const Layer, Row, Col: Integer;
      const Value: boolean); virtual;
    // @name calls inherited @name and then invalidates the model.
    procedure SetName(const Value: TComponentName); override;
    // See @link(RealData).
    procedure SetRealData(const Layer, Row, Col: integer;
      const Value: double); virtual;
    // See @link(StringData).
    procedure SetStringData(const Layer, Row, Col: integer;
      const Value: string); virtual;
    // @name calls inherited and also notifies the model that it has
    // changed.  If the @classname is newly set to be up-to-date, the
    // minimum and maximum values are calculated.
    procedure SetUpToDate(const Value: boolean); override;
    // @name is used to set values of some cells in a @classname
    // in some special way after it has been set in @link(Initialize).
    // @name calls @link(OnPostInitialize) if @link(OnPostInitialize)
    // is assigned.
    //
    // If a @classname overrides @link(Initialize), it should call @name
    // just before setting @link(TObserver.UpToDate TObserver.UpToDate)
    // to @true at the end of @link(Initialize).
    procedure PostInitialize;
    // @name sets the array size to (0,0,0).
    procedure Clear; virtual;
    // @name restores data from a temporary file.
    procedure RestoreData;
    // When appropriate, @name restores data from a temporary file.
    procedure CheckRestoreData;
    {@name sets @link(FIsUniform), @link(FUniformAnnotation),
     and depending on @link(DataType) it also
     sets one of the following:
     @unorderedlist(
       @item(@link(FUniformRealValue),)
       @item(@link(FUniformIntegerValue),)
       @item(@link(FUniformBooleanValue), or)
       @item(@link(FUniformStringValue).)
     )
    }
    procedure RemoveSubscription(Sender: TObject; const AName: string);
    procedure RestoreSubscription(Sender: TObject; const AName: string);
  public
    function ColorGridValueOK(const Layer, Row, Col: integer): boolean;
    function ContourGridValueOK(const Layer, Row, Col: integer): boolean;
    procedure CheckIfUniform; virtual;
    procedure GetMinMaxStoredLimits(out LayerMin, RowMin, ColMin,
      LayerMax, RowMax, ColMax: integer); virtual;
    // @name updates @link(MinValue) and @link(MaxValue).
    procedure UpdateMinMaxValues;
    // @name does nothing is the Source is a @classname.
    // otherwise, it raises an exception.
    procedure Assign(Source: TPersistent); override;
    // @name stores the data in @classname in a temporary file.
    procedure CacheData;
    // @name adds to @link(Classification) information about whether a
    // @classname is required for the current model or not.
    function FullClassification: string;
    // When a value is assigned to a location in a @classname,
    // @name is used to specify how that value was assigned.
    // This can help the user understand why the value is what it is.
    property Annotation[const Layer, Row, Col: integer]: string read
      GetAnnotation write SetAnnotation;
    // @name gives the boolean value at the location specified by
    // Layer, Row, and Col.
    property BooleanData[const Layer, Row, Col: integer]: boolean read
      GetBooleanData write SetBooleanData;
    // @name gives the number of columns of data in the @classname.
    property ColumnCount: integer read GetColumnCount;
    // @name creates an instance of @classname.
    constructor Create(AnOwner: TComponent); override;
    // @name destroys the the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    // @name is used when coloring the grid to specify that
    // the real number value should be
    // used when coloring the grid instead of the integer value.
    // In @classname, @name always returns false.
    // This is overridden in @link(PhastDataSets.TIntegerPhastDataSet)
    // and @link(PhastDataSets.TSparseIntegerPhastDataSet).
    function DisplayRealValue: boolean; virtual;
    // @name is the formula used to assign values to the @classname.
    property Formula: string read GetFormula write SetFormula;
    // @name is the formula that is used to simulate how
    // MODFLOW parameters are used in MODFLOW.
    property ParameterFormula: string read FParameterFormula
      write SetParameterFormula;
    // @name indicates whether MODFLOW parameters
    // are used to set the @classname.
    property ParameterUsed: boolean read FParameterUsed write SetParameterUsed;
    // @name fills AStringList with the names of all the @classname
    // that depend on the current @classname.
    procedure FullUseList(const AStringList: TStringList);
    // @name assigns values to each location in
    // @link(RealData), @link(IntegerData),
    // @link(BooleanData), or @link(StringData) in @classname.
    // It first uses either @link(Formula) or @link(TwoDInterpolator)
    // to assign values and then assigns values using @link(TScreenObject)s.
    // See @link(TCustomPhastDataSet.Initialize
    // TCustomPhastDataSet.Initialize)
    // for a flow chart.
    // @seealso(PostInitialize)
    procedure Initialize; virtual;
    // @name gives the integer value at the location specified by
    // Layer, Row, and Col.
    property IntegerData[const Layer, Row, Col: integer]: integer read
      GetIntegerData write SetIntegerData;
    // @name is called when something has happened that would change
    // the values assigned to the @classname.
    procedure Invalidate; virtual;
    // @name indicates that a value is present at the location specified
    // by Layer, Row, Col. In @classname, @name is always true.
    // In some descendants of @classname, @name can be false.
    property IsValue[const Layer, Row, Col: Integer]: boolean read
      GetIsValue write SetIsValue;
    // @name indicates whether or not a @classname is uniform or not.
    property IsUniform: TIsUniform read FIsUniform;
    // If the @classname is uniform (see @link(IsUniform)
    // and @link(DataType) is rdtReal,
    // @name is the uniform value in the @classname.
    property UniformRealValue: double read FUniformRealValue;
    // If the @classname is uniform (see @link(IsUniform)
    // and @link(DataType) is rdtInteger,
    // @name is the uniform value in the @classname.
    property UniformIntegerValue: integer read FUniformIntegerValue;
    // If the @classname is uniform (see @link(IsUniform)
    // and @link(DataType) is rdtBoolean,
    // @name is the uniform value in the @classname.
    property UniformBooleanValue: boolean read FUniformBooleanValue;
    // If the @classname is uniform (see @link(IsUniform)
    // and @link(DataType) is rdtString,
    // @name is the uniform value in the @classname.
    property UniformStringValue: string read FUniformStringValue;
    // If the @classname is uniform (see @link(IsUniform),
    // @name is the annotation in the @classname.
    property UniformAnnotation: string read FUniformAnnotation;
    // @name gives the number of layers of data in the @classname.
    property LayerCount: integer read GetLayerCount;
    // @name gives the real number value at the location specified by
    // Layer, Row, and Col.
    property RealData[const Layer, Row, Col: integer]: double read GetRealData
      write SetRealData;
    // @name gives the number of rows of data in the @classname.
    property RowCount: integer read GetRowCount;
    // @name gives the string value at the location specified by
    // Layer, Row, and Col.
    property StringData[const Layer, Row, Col: integer]: string read
      GetStringData write SetStringData;
    // @name updates the number of dimensions that need to be in
    // the @classname.  It does not actually change the dimensions
    // of the array used to hold the data.  See @link(SetDimensions).
    procedure UpdateDimensions(NumberOfLayers, NumberOfRows,
      NumberOfColumns: integer);
    // @name returns true unless @link(OnDataSetUsed) is assigned.
    // in which case it calls @link(OnDataSetUsed) and returns its result.
    // See TPhastModel.@link(TPhastModel.ChemistryUsed).
    // See TPhastModel.@link(TPhastModel.EquilibriumPhasesUsed).
    // See TPhastModel.@link(TPhastModel.ExchangeUsed).
    // See TPhastModel.@link(TPhastModel.GasPhaseUsed).
    // See TPhastModel.@link(TPhastModel.InitialHeadUsed).
    // See TPhastModel.@link(TPhastModel.KineticsUsed).
    // See TPhastModel.@link(TPhastModel.SolidSolutionUsed).
    // See TPhastModel.@link(TPhastModel.SurfacesUsed).
    // See TPhastModel.@link(TPhastModel.InitialWaterTableUsed).
    function UsedByModel: boolean;
    // @name is a list of the variables used by the @link(Formula).
    property UseList: TStringList read GetUseList;
    // See @link(UsedByModel).
    property OnDataSetUsed: TObjectUsedEvent read FOnDataSetUsed write
      FOnDataSetUsed;

    // @name is the @link(TPhastTimeList) that controls this @classname.
    property ATimeList: TCustomTimeList read FTimeList write FTimeList;
    // @name is used to set values of some cells in a @classname
    // in some special way after it has been set in @link(Initialize).
    // @name is called in @link(PostInitialize) if @name
    // is assigned.
    //
    // If a @classname overrides @link(Initialize), it should call
    // @link(PostInitialize)
    // just before setting @link(TObserver.UpToDate TObserver.UpToDate)
    // to @true at the end of @link(Initialize).
    // @seealso(PostInitialize)
    // @seealso(Initialize)
    property OnPostInitialize: TNotifyEvent read FOnPostInitialize
      write FOnPostInitialize;
    // @name is called when a @classname is being destroyed.
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    // @name indicates the names of the data sets in the
    // MODFLOW and/or PHAST model input files
    // that are associated with the @classname.
    property AssociatedDataSets: string read FAssociatedDataSets
      write FAssociatedDataSets;
    // @name is a string representation of the maximum value in the @classname.
    // See @link(UpdateMinMaxValues).
    property MaxValue: string read GetMaxValue;
    // @name is a string representation of the minimum value in the @classname.
    // See @link(UpdateMinMaxValues).
    property MinValue: string read GetMinValue;
    property MinReal: double read FMinReal;
    property MaxReal: double read FMaxReal;
    property MinInteger: integer read FMinInteger;
    property MaxInteger: integer read FMaxInteger;
    property MinBoolean: boolean read FMinBoolean;
    property MaxBoolean: boolean read FMaxBoolean;
    property MinString: string read FMinString;
    property MaxString: string read FMaxString;
    // @name records the contouring data used to contour the data set.
    // @name may be nil.
    property Contours: TContours read FContours write SetContours;
    procedure RefreshFormula; virtual;
    procedure ComputeHash;
    property Hash: longint read GetHash;
    function IdenticalDataArrayContents(ADataArray: TDataArray): boolean;
  published
    // @name indicates the hierarchical position of this instance of
    // @classname when it is required by MODFLOW.
    property Classification: string read GetClassification
      write SetClassification;
    {If @name is @true, attempting to set
     @link(IntegerData) or @link(RealData) to a value
     greater than @link(Max) will cause
     it to be set to Trunc(@link(Max)) or @link(Max). }
    property CheckMax: boolean read FCheckMax write FCheckMax;
    {If @name is @true, attempting to set
     @link(IntegerData) or @link(RealData) to a value
     less than @link(Min) will cause
     it to be set to Trunc(@link(Min)) or @link(Min). }
    property CheckMin: boolean read FCheckMin write FCheckMin;
    // @name is a user-defined comment about the @classname.
    // When importing a @classname, @name may be assigned automatically.
    property Comment: string read FComment write SetComment;
    // @name indicates whether the data stored in @classname are
    // real numbers, integers, booleans, or strings.
    property DataType: TRbwDataType read FDataType write SetDataType;
    // @name indicates whether the data in @classname are evaluated at
    // element centers or nodes.
    property EvaluatedAt: TEvaluatedAt read FEvaluatedAt write SetEvaluatedAt;
    // @name specifies how (or if) the values used to color the grid should
    // be limited.  See @link(TColoringLimits).
    property Limits: TColoringLimits read FLimits write SetLimits;
    // @name specifies how (or if) the values used to color the grid should
    // be limited.  See @link(TColoringLimits).
    property ContourLimits: TColoringLimits read FContourLimits write SetContourLimits;
    // Members of @name are things the user ought not to be able to edit.
    property Lock: TDataLock read FLock write SetLock;
    {See @Link(CheckMax).}
    property Max: double read FMax write FMax;
    {See @Link(CheckMin).}
    property Min: double read FMin write FMin;
    // @name is used to
    // indicate whether the @classname is a 2D or 3D data set and,
    // if it is 2D, which face of the grid it is associated with.
    property Orientation: TDataSetOrientation read FOrientation
      write SetOrientation;
    // @name is used only for backwards compatibility.
    // @name must be before @link(TwoDInterpolator).
    // See @link(TwoDInterpolatorClass).
    property TwoInterpolatorClass: string read GetTwoDInterpolatorClass
      write SetTwoDInterpolatorClass stored False;
    // @name is the name of the Class of @link(TCustom2DInterpolater)
    // used with this @classname.  Assigning @name will cause an instance
    // of @link(TCustom2DInterpolater) to be created and assigned to
    // @link(TwoDInterpolator).
    //
    // Important: when reading a @classname from a stream
    // @name must be assigned
    // before @link(TwoDInterpolator) so that an instance of
    // @link(TCustom2DInterpolater) of the correct type will be created.
    // To achieve this, @name must appear before @link(TwoDInterpolator)
    // in the list of published properties.
    property TwoDInterpolatorClass: string read GetTwoDInterpolatorClass
      write SetTwoDInterpolatorClass;
    {@name is the @link(TCustom2DInterpolater) that is used to interpolate
     among @link(TScreenObject)s when assigning values to @classname.}
    property TwoDInterpolator: TCustom2DInterpolater read FTwoDInterpolator
      write SetTwoDInterpolator;
    // @name is used to indicate the units for the data in @classname.
    // @name is only a label; it has no significance for the model.
    property Units: string read FUnits write SetUnits;
    // @name indicates whether the @classname will be shown or not
    // when editing a @link(TScreenObject).
    { TODO : Eliminate commented out code for Visible property. }

    // @name has no effect.  It is maintained only for backwards compatibility.
    property Visible: boolean read FVisible write SetVisible stored False;
  end;

  {@name is used in creating descendants of @link(TDataArray) of the
   correct type when reading the model from a stream.
   See TDataSetItem.@link(PhastModelUnit.TDataSetItem.DataSetClass). }
  TDataArrayType = class of TDataArray;

  {@name is used in creating descendants of @link(TCustom2DInterpolater)
    of the
   correct type when reading a @link(TDataArray) from a stream.
   See TDataArray.@link(TDataArray.TwoDInterpolatorClass). }
  TInterpolatorType = class of TCustom2DInterpolater;

  // See TCustom2DInterpolater.@link(TCustom2DInterpolater.OnInitialize)
  // and TCustom2DInterpolater.@link(TCustom2DInterpolater.OnFinalize).
  TInitializeDataSetInterpolator = procedure(Sender: TObject;
    const DataSet: TDataArray) of object;

  {@abstract(@name provides an abstract interface for 2D interpolation.
   Descendants of @name provide concrete implementations.

   Call RegisterClass in the initialization section for any
   descendants that are instantiated.)

   Descendants of @name include @link(TCustomAnisotropicInterpolator),
   @link(TInvDistSq2DInterpolator), @link(TNearest2DInterpolator), and
   @link(TNearestPoint2DInterpolator).
   }
  TCustom2DInterpolater = class(TComponent)
  private
    // See @link(DataSet).
    FDataSet: TDataArray;
    // See @link(OnEdit).
    FOnEdit: TNotifyEvent;
    // See @link(OnFinalize).
    FOnFinalize: TInitializeDataSetInterpolator;
    // See @link(OnInitialize).
    FOnInitialize: TInitializeDataSetInterpolator;
  protected
    // @name fills ListOfScreenObjects with all the @link(TScreenObject)s
    // that should be used to assign values to cells with
    // this @classname.  The order of @link(TScreenObject)s
    // is the same as the order in
    // frmGoPhast.Model.@link(TPhastModel.ScreenObjects).
    procedure FillScreenObjectList(const ListOfScreenObjects: TList);
    procedure EvaluateExpression(Compiler: TRbwParser;
      var Expression: TExpression; AScreenObject: TObject);

  public
    // @name copies the values from Source to the current @classname.
    procedure Assign(Source: TPersistent); override;
    // @name returns a boolean value at Location.
    function BooleanResult(const Location: TPoint2D): boolean; virtual;
    // @name creates an instance of @classname.
    // If Owner is a @link(TDataArray) @name
    // checks the @link(TDataArray.DataType).
    // @name makes itself a subcomponent of Owner.
    constructor Create(AOwner: TComponent); override;
    // @name is the @link(TDataArray) with which this
    // @classname will be used.  Normally @name is set
    // by passing a @link(TDataArray) as AOwner in @link(Create).
    property DataSet: TDataArray read FDataSet;
    // If @link(OnEdit) is assigned, @name calls @link(OnEdit).
    procedure Edit;
    // If @link(OnFinalize) is assigned, @name calls @link(OnFinalize).
    // @name is called just after the interpolation process ends.
    procedure Finalize(const DataSet: TDataArray); virtual;
    // If @link(OnInitialize) is assigned, @name calls @link(OnInitialize).
    // @name is called just before the interpolation process begins.
    procedure Initialize(const DataSet: TDataArray);
    // @name returns a integer value at Location.
    function IntegerResult(const Location: TPoint2D): integer; virtual;
    // @name is the name of the interpolator as displayed to the user.
    class function InterpolatorName: string; virtual; abstract;
    // @name returns a real-number value at Location.
    function RealResult(const Location: TPoint2D): real; virtual;
    // @name returns @true if AnotherInterpolator has the same parameters
    // as the @classname being called.
    function SameAs(AnotherInterpolator: TCustom2DInterpolater): boolean;
      virtual;
    // @name returns true if there are any @link(TScreenObject)s
    // that can be used for interpolation with this data set.
    function ShouldInterpolate: boolean; virtual;
    // @name returns a string value at Location.
    function StringResult(const Location: TPoint2D): string; virtual;
    // @name indicates the types of data that the current @classname
    // can be used with.
    class function ValidReturnTypes: TRbwDataTypes; virtual; abstract;
    class Function ValidOrientations: TDataSetOrientations; virtual;
  published
    // @name can be used to respond to a change in the interpolator.
    property OnEdit: TNotifyEvent read FOnEdit write FOnEdit;
    // If the @classname has to do something to finish up after the
    // interpolation process use OnFinalize to do it.
    property OnFinalize: TInitializeDataSetInterpolator read FOnFinalize
      write FOnFinalize;
    // If the @classname has to do something to get ready to start the
    // interpolation process use OnInitialize to do it.
    property OnInitialize: TInitializeDataSetInterpolator read FOnInitialize
      write FOnInitialize;
  end;

  // @abstract(@name is used to create
  // descendants of @link(TCustom2DInterpolater)
  // when reading a @link(TDataArray) from a file.)
  TInterpolatorClass = class of TCustom2DInterpolater;

  // @abstract(@name is the abstract ancestor of classes that use descendants
  // of @link(T3DSparsePointerArray) to store data.)
  // These data sets won't necessarily have a value defined at all locations.
  // Related data such as @link(TDataArray.Annotation) are affected too
  // so those are stored in a @link(T3DSparseStringArray).
  // The @link(TDataArray.Formula) is not used; only @link(TScreenObject)s
  // are used to assign values.
  TCustomSparseDataSet = class(TDataArray)
  private
    // See @link(BoundaryTypeDataSet).
    FBoundaryTypeDataSet: TDataArray;
    // @name is used to store the
    // @link(TDataArray.Annotation) at each location.
    FAnnotation: T3DSparseStringArray;
    FPriorLayer: Integer;
    FPriorRow: Integer;
    FPriorCol: Integer;
    FPriorResult: Boolean;
  protected
    procedure Clear; override;
    // See @link(TDataArray.Annotation).
    function GetAnnotation(const Layer, Row, Col: integer): string; override;
    // @name checks whether @link(TDataArray.Annotation) has been
    // assigned at Layer, Row, Col.
    // If @link(BoundaryTypeDataSet) has been assigned, it checks
    // it too.
    function GetIsValue(const Layer, Row, Col: Integer): boolean;override;
    // See @link(TDataArray.Annotation).
    procedure SetAnnotation(const Layer, Row, Col: integer;
      const Value: string); override;
    // @name clears @link(FAnnotation).
    procedure SetDimensions(const SetToZero: boolean); override;
    // If @link(BoundaryTypeDataSet) is assigned,
    // @name sets BoundaryTypeDataSet.IsValue to true at
    // Layer, Row, Col.
    procedure SetIsValue(const Layer, Row, Col: Integer;
      const Value: boolean);override;
  public
    procedure Invalidate; override;
    // @name creates an instance of @classname and sets
    // @link(TDataArray.EvaluatedAt) to eaNodes.
    constructor Create(AnOwner: TComponent); override;
    // @name destroys the the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    // @name assigns values to some location in
    // @link(TDataArray.RealData), @link(TDataArray.IntegerData),
    // @link(TDataArray.BooleanData), or @link(TDataArray.StringData)
    // in @classname.
    // Unlike @link(TDataArray) It doesn't uses either
    // @link(TDataArray.Formula) or @link(TDataArray.TwoDInterpolator)
    // to assign values. It only assigns values using @link(TScreenObject)s.
    procedure Initialize; override;

    // If this data set is used with a boundary condition
    // and several boundary conditions are mutually exclusive.
    // @name is used to check that the correct type of boundary
    // condition is stored at a
    // location when determining @link(TDataArray.IsValue).
    property BoundaryTypeDataSet: TDataArray read FBoundaryTypeDataSet
      write FBoundaryTypeDataSet;
  end;

  {@abstract(@name is used to store real numbers in a sparse array.)}
  TRealSparseDataSet = class(TCustomSparseDataSet)
  private
    // @name is used to store the real numbers in @classname.
    FRealValues: T3DSparseRealArray;
  protected
    procedure Clear; override;
    // @name checks whether @link(FRealValues) has been
    // assigned at Layer, Row, Col as well as inherited @name.
    function GetIsValue(const Layer, Row, Col: Integer): boolean;
      override;
    // @name retrieves the real number at Layer, Row, Col.
    function GetRealData(const Layer, Row, Col: integer): double; override;
    // @name checks that Value is rdtDouble.
    procedure SetDataType(const Value: TRbwDataType); override;
    // @name clears FRealValues and calls
    // inherited @Link(TCustomSparseDataSet.SetDimensions).
    procedure SetDimensions(const SetToZero: boolean); override;
    // @name stores a real number at Layer, Row, Col.
    procedure SetRealData(const Layer, Row, Col: integer;
      const Value: double); override;
  public
    procedure GetMinMaxStoredLimits(out LayerMin, RowMin, ColMin,
      LayerMax, RowMax, ColMax: integer); override;
    // @name creates an instance of @classname and sets
    // @link(TDataArray.DataType) to rdtDouble.
    constructor Create(AnOwner: TComponent); override;
    // @name destroys the the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
  end;

  {@abstract(@name is used to store integers in a sparse array.)}
  TIntegerSparseDataSet = class(TCustomSparseDataSet)
  private
    // @name is used to store the integers in @classname.
    FIntegerValues: T3DSparseIntegerArray;
    // @name is used to indicate that this data set is being
    // used to define the type of boundary condition at various
    // locations.
    FIsBoundary: boolean;
  protected
    // @name removes all the data from @classname.
    procedure Clear; override;
    function GetIntegerData(const Layer, Row, Col: integer): integer; override;
    // @name checks whether @link(FIntegerValues) has been
    // assigned at Layer, Row, Col as well as inherited @name.
    function GetIsValue(const Layer, Row, Col: Integer): boolean;
      override;
    // @name checks that Value is rdtInteger.
    procedure SetDataType(const Value: TRbwDataType); override;
    // @name clears @link(FIntegerValues) and calls
    // inherited @Link(TCustomSparseDataSet.SetDimensions).
    procedure SetDimensions(const SetToZero: boolean); override;
    // @name stores an integer at Layer, Row, Col.
    procedure SetIntegerData(const Layer, Row, Col: integer;
      const Value: integer); override;
    // @name if BoundaryTypeDataSet = self then FIntegerValues.IsValue is set
    // to Value. Otherwise inherited IsValue is set.
    procedure SetIsValue(const Layer, Row, Col: Integer;
      const Value: boolean); override;
  public
    // @name creates an instance of @classname and sets
    // @link(TDataArray.DataType) to rdtInteger.
    constructor Create(AnOwner: TComponent); override;
    // @name destroys the the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    // @name is true if this data set is used to record the type of
    // boundary condition at a location.
    property IsBoundaryTypeDataSet: boolean read FIsBoundary write FIsBoundary;
    procedure GetMinMaxStoredLimits(out LayerMin, RowMin, ColMin,
      LayerMax, RowMax, ColMax: integer); override;
  end;

  // @name is used to store a series of @link(TDataArray)s. Each is associated
  // with a specific time.
  TCustomTimeList = Class(TObject)
  private
    // @name is either nil or a @link(TPhastModel).
    FModel: TObject;
    // @name stores the @link(TDataArray)s.
    // @name is instantiated as a TObjectList.
    FData: TList;
    // See @link(Times).
    FTimes: TRealList;
    // See @link(Direction).
    FDirection: TDataSetOrientation;
    // See @link(Limits).
    FLimits: TColoringLimits;
    // See @link(Name).
    FName: string;
    // See @link(Orientation).
    FOrientation: TDataSetOrientation;
    // See @link(UpToDate).
    FUpToDate: boolean;
    // See @link(Classification).
    FClassification: string;
    // See @link(CheckMax).
    FCheckMax: boolean;
    // See @link(Max).
    FMax: double;
    // See @link(CheckMin).
    FCheckMin: boolean;
    // See @link(Min).
    FMin: double;
    // See @link(DataType).
    FDataType: TRbwDataType;
    // See @link(OnTimeListUsed).
    FOnTimeListUsed: TObjectUsedEvent;
    // See @link(Count).
    function GetCount: integer;
    // See @link(Items).
    function GetItems(const Index: integer): TDataArray;
    // See @link(Times).
    function GetTimes(const Index: integer): double;
    // See @link(Items).
    procedure SetItems(const Index: integer; const Value: TDataArray);
    // See @link(Limits).
    procedure SetLimits(const Value: TColoringLimits);
    // See @link(Name).
    procedure SetName(const Value: string);
    // See @link(Orientation).
    procedure SetOrientation(const Value: TDataSetOrientation);
    // See @link(Classification).
    procedure SetClassification(const Value: string);
    // See @link(Classification).
    function GetClassification: string;
    // See @link(CheckMax).
    procedure SetCheckMax(const Value: boolean);
    // See @link(CheckMin).
    procedure SetCheckMin(const Value: boolean);
    // See @link(Max).
    procedure SetMax(const Value: double);
    // See @link(Min).
    procedure SetMin(const Value: double);

  protected
    // See @link(UpToDate).
    function GetUpToDate: boolean; virtual;
    // See @link(UpToDate).
    procedure SetUpToDate(const Value: boolean); virtual;
  public
    // @name is used to indicate whether or not a particular @classname
    // is used by the model.
    // See @link(UsedByModel).
    property OnTimeListUsed: TObjectUsedEvent read FOnTimeListUsed write
      FOnTimeListUsed;
    // If @link(OnTimeListUsed) is assigned, @name calls @link(OnTimeListUsed)
    // and returns its result. Otherwise @name returns @true.
    function UsedByModel: boolean;
    // @name adds a @link(TDataArray) and its associated
    // time to the @classname.
    function Add(const ATime: double; const Data: TDataArray): integer;

    // @name removes all the @link(TDataArray)s from
    // the @classname.
    procedure Clear; virtual;

    // @name is the number of @link(TDataArray)s stored in
    // the @classname.
    property Count: integer read GetCount;

    // @name creates an instance of @classname.
    constructor Create(Model: TObject);
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;

    // @name returns the position in @link(Times) of the first time that
    // is greater than ATime.
    function FirstTimeGreaterThan(const ATime: double): integer;

    // @name returns the position of ATime in @link(Times).
    function IndexOf(const ATime: double): integer;

    // @name returns the position of Data in @link(Items).
    function IndexOfDataSet(const Data: TDataArray): integer;

    // @name assigns values to all the @link(TDataArray)s in @classname.
    procedure Initialize; virtual; abstract;
    // @name sets UpToDate to false;
    procedure Invalidate; 

    // @name is the Name of the @classname.
    property Name: string read FName write SetName;

    // @name provides access to the time for each
    // @link(TSparseArrayPhastInterpolationDataSet) in @link(Items).
    property Times[const Index: integer]: double read GetTimes;

    // @name indicates whether all of the
    // @link(TSparseArrayPhastInterpolationDataSet)s are up-to-date.
    property UpToDate: boolean read GetUpToDate;

    { TODO : Document why Direction and Orientation are sometimes different. }
    // @name is the @link(TDataSetOrientation) of the
    // @link(TSparseArrayPhastInterpolationDataSet)s in the @classname.
    property Orientation: TDataSetOrientation read FOrientation write
      SetOrientation;

    // @name is an event handler. One place it is used is
    // TCustomPhastBoundaryCondition.
    // @link(TCustomPhastBoundaryCondition.AddMixtureSubscriptions).
    procedure Changed(Sender: TObject);

    // @name sets the item at Index to nil.
    // Because FData is a TObjectList, this also destroys the item at Index.
    procedure FreeItem(Index: integer);
    // @name provides access to the @link(TDataArray)s stored in
    // @classname.
    property Items[const Index: integer]: TDataArray read GetItems
      write SetItems; default;

    // @name represents the @link(TColoringLimits) to be applied
    // to the data sets in @classname when the grid is colored
    // by one of those data sets.
    property Limits: TColoringLimits read FLimits write SetLimits;

    // @name applies @link(Limits) to each of the
    // @link(TSparseArrayPhastInterpolationDataSet) in @classname.
    procedure UpDateLimits;

    { TODO : Document why Direction and Orientation are sometimes different. }
    // @name specifies the @link(TDataSetOrientation) of the
    // @link(TSparseArrayPhastInterpolationDataSet)s in the @classname.
    property Direction: TDataSetOrientation read FDirection write FDirection;
    {@name is used as as suggestion to a GUI on how to select a particular
    // @classname
    from a hierarchical list of @classname's}
    property Classification: string read GetClassification
      write SetClassification;
    // @name is used to set the @link(TDataArray.Max) property of any
    // @link(TDataArray)s stored in @classname
    property Max: double read FMax write SetMax;
    // @name is used to set the @link(TDataArray.Min) property of any
    // @link(TDataArray)s stored in @classname
    property Min: double read FMin write SetMin;
    // @name is used to set the @link(TDataArray.CheckMax) property of any
    // @link(TDataArray)s stored in @classname
    property CheckMax: boolean read FCheckMax write SetCheckMax;
    // @name is used to set the @link(TDataArray.CheckMin) property of any
    // @link(TDataArray)s stored in @classname
    property CheckMin: boolean read FCheckMin write SetCheckMin;

    // @name specifies the type of data stored in the
    // @link(TDataArray)s stored in @classname.
    property DataType: TRbwDataType read FDataType write FDataType;
    // @name is a string representation of the maximum value
    // of the @link(TDataArray) ast Time.
    function MaxValue(Time: double): string;
    // @name is a string representation of the minimum value
    // of the @link(TDataArray) ast Time.
    function MinValue(Time: double): string;
  End;

{ TODO : See if GenerateNewName can be combined with some similar function elsewhere. }
{ @name generates a name for a @link(TDataArray) that is valid
  and does not conflict with the names of any existing @link(TDataArray)s.}

// @name generates a name for a data set that is valid
// and does not conflict with the names of any existing data sets.
function GenerateNewName(Root: string = 'NewDataSet'; Connector: string = ''): string;

{@name is used to generate a valid name from one that may be invalid.
Valid names must begin with a letter or underscore.  The remaining
characters must be letters, digits or the underscore character.}
function GenerateNewRoot(const Root: string): string;

procedure GlobalDataArrayRemoveSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

procedure GlobalDataArrayRestoreSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

var
  // @name Stack is used in @link(TDataArray.Initialize) to
  // detect circular references.  A @link(ECircularReference) is raised
  // if one is found.
  Stack: TStringList = nil;

const
  // @name is used in the classification of data sets.
  StrDataSets = 'Data Sets';
  // @name is used in the classification of data sets.
  StrUserDefined = 'User Defined';
  // @name is used in the classification of data sets.
  StrRequired = StrDataSets + '|Required|';
  // @name is used in the classification of data sets.
  StrOptional = StrDataSets + '|Optional|';
  // @name is used in the classification of data sets.
  strDefaultClassification = StrDataSets + '|' + StrUserDefined;

implementation

uses Contnrs, frmGoPhastUnit, frmConvertChoiceUnit, GIS_Functions,
  ScreenObjectUnit, frmFormulaErrorsUnit, InterpolationUnit, SparseArrayUnit,
  PhastModelUnit, AbstractGridUnit, frmGridColorUnit, frmContourDataUnit;

resourcestring
  StrUnassigned = 'Unassigned';

function GenerateNewRoot(const Root: string): string;
var
  Index: integer;
begin
  result := Trim(Root);
  Assert(result <> '');
  if not (result[1] in ['A'..'Z', 'a'..'z', '_']) then
  begin
    result[1] := '_';
  end;

  for Index := 2 to Length(result) do
  begin
    if not (result[Index] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
    begin
      result[Index] := '_';
    end;
  end;
end;

function GenerateNewName(Root: string = 'NewDataSet'; Connector: string = ''): string;
var
  Names: TStringList;
  Index: integer;
  DataSet: TDataArray;
begin
  Root := Trim(Root);
  if Root = '' then
  begin
    Root := 'NewDataSet';
  end;
  Root := GenerateNewRoot(Root);

  // This function generates a name for a data set that is valid
  // and does not conflict with the names of any existing data sets.
  Names := TStringList.Create;
  try
    for Index := 0 to frmGoPhast.PhastModel.DataSetCount - 1 do
    begin
      DataSet := frmGoPhast.PhastModel.DataSets[Index];
      Names.Add(DataSet.Name);
    end;

    // Don't allow the name to be the same as a deleted data set.
    for Index := 0 to frmGoPhast.DeletedDataSets.Count - 1 do
    begin
      DataSet := frmGoPhast.DeletedDataSets[Index] as TDataArray;
      Names.Add(DataSet.Name);
    end;

    // Names now includes the names of all the data sets.

    // Generate a new name.
    if Names.IndexOf(Root) < 0 then
    begin
      result := Root;
    end
    else
    begin
      Index := 1;
      result := Root + Connector + IntToStr(Index);
      while Names.IndexOf(result) >= 0 do
      begin
        Inc(Index);
        result := Root + Connector + IntToStr(Index);
      end;
    end;
  finally
    Names.Free;
  end;
end;

{ TDataArray }

procedure TDataArray.ResetFormula(const Compiler: TRbwParser;
  const ErrorMessage: string);
var
  TempFormula: string;
begin
  if ParameterUsed then
  begin
    TempFormula := ParameterFormula;
  end
  else
  begin
    TempFormula := Formula;
    if TempFormula = '' then
    begin
      TempFormula := FFormula;
    end;
  end;
  if (FPhastModel <> nil)
    and not (csDestroying in FPhastModel.ComponentState) 
    and not (FPhastModel as TPhastModel).Clearing
    and not ClearingDeletedDataSets then
  begin
    frmFormulaErrors.AddError('', Name, TempFormula, ErrorMessage);
  end;
  case DataType of
    rdtDouble, rdtInteger:
      begin
        TempFormula := '0';
      end;
    rdtBoolean:
      begin
        TempFormula := 'False';
      end;
    rdtString:
      begin
        TempFormula := '"0"';
      end;
  else
    Assert(False);
  end;
  if ParameterUsed then
  begin
    FParameterFormula := TempFormula;
  end
  else
  begin
    FFormula := TempFormula;
    frmGoPhast.PhastModel.FormulaManager.ChangeFormula(FFormulaObject, FFormula,
      GetCompiler, GlobalDataArrayRemoveSubscription,
      GlobalDataArrayRestoreSubscription, self);
  end;
  Compiler.Compile(TempFormula);
end;

destructor TDataArray.Destroy;
begin
  if (FPhastModel <> nil)
    and (not (csDestroying in FPhastModel.ComponentState))
    and not (FPhastModel as TPhastModel).Clearing then
  begin
    if Assigned(OnDestroy) then
    begin
      OnDestroy(self);
    end;
    case DataType of
      rdtDouble: Formula := '0.';
      rdtInteger: Formula := '0';
      rdtBoolean: Formula := 'False';
      rdtString: Formula := '""';
    end;

    if frmGoPhast.PhastGrid <> nil then
    begin
      if frmGoPhast.PhastModel.PhastGrid.TopDataSet = self then
      begin
        frmGoPhast.PhastModel.PhastGrid.TopDataSet := nil
      end;
      if frmGoPhast.PhastModel.PhastGrid.FrontDataSet = self then
      begin
        frmGoPhast.PhastModel.PhastGrid.FrontDataSet := nil
      end;
      if frmGoPhast.PhastModel.PhastGrid.SideDataSet = self then
      begin
        frmGoPhast.PhastModel.PhastGrid.SideDataSet := nil
      end;
      if frmGoPhast.PhastModel.PhastGrid.ThreeDDataSet = self then
      begin
        frmGoPhast.PhastModel.PhastGrid.ThreeDDataSet := nil
      end;

      if frmGoPhast.PhastModel.PhastGrid.TopContourDataSet = self then
      begin
        frmGoPhast.PhastModel.PhastGrid.TopContourDataSet := nil
      end;
      if frmGoPhast.PhastModel.PhastGrid.FrontContourDataSet = self then
      begin
        frmGoPhast.PhastModel.PhastGrid.FrontContourDataSet := nil
      end;
      if frmGoPhast.PhastModel.PhastGrid.SideContourDataSet = self then
      begin
        frmGoPhast.PhastModel.PhastGrid.SideContourDataSet := nil
      end;
      if frmGoPhast.PhastModel.PhastGrid.ThreeDContourDataSet = self then
      begin
        frmGoPhast.PhastModel.PhastGrid.ThreeDContourDataSet := nil
      end;
    end;

    if frmGoPhast.ModflowGrid <> nil then
    begin
      if frmGoPhast.PhastModel.ModflowGrid.TopDataSet = self then
      begin
        frmGoPhast.PhastModel.ModflowGrid.TopDataSet := nil
      end;
      if frmGoPhast.PhastModel.ModflowGrid.FrontDataSet = self then
      begin
        frmGoPhast.PhastModel.ModflowGrid.FrontDataSet := nil
      end;
      if frmGoPhast.PhastModel.ModflowGrid.SideDataSet = self then
      begin
        frmGoPhast.PhastModel.ModflowGrid.SideDataSet := nil
      end;
      if frmGoPhast.PhastModel.ModflowGrid.ThreeDDataSet = self then
      begin
        frmGoPhast.PhastModel.ModflowGrid.ThreeDDataSet := nil
      end;

      if frmGoPhast.PhastModel.ModflowGrid.TopContourDataSet = self then
      begin
        frmGoPhast.PhastModel.ModflowGrid.TopContourDataSet := nil
      end;
      if frmGoPhast.PhastModel.ModflowGrid.FrontContourDataSet = self then
      begin
        frmGoPhast.PhastModel.ModflowGrid.FrontContourDataSet := nil
      end;
      if frmGoPhast.PhastModel.ModflowGrid.SideContourDataSet = self then
      begin
        frmGoPhast.PhastModel.ModflowGrid.SideContourDataSet := nil
      end;
      if frmGoPhast.PhastModel.ModflowGrid.ThreeDContourDataSet = self then
      begin
        frmGoPhast.PhastModel.ModflowGrid.ThreeDContourDataSet := nil
      end;
    end;
  end;

  FLimits.Free;
  FContourLimits.Free;
  SetDimensions(True);
  FUseList.Free;
  FTwoDInterpolator.Free;

  if frmGoPhast.PhastModel <> nil then
  begin
    if frmGoPhast.PhastModel.TopGridObserver <> nil then
    begin
      frmGoPhast.PhastModel.TopGridObserver.StopsTalkingTo(self);
      StopsTalkingTo(frmGoPhast.PhastModel.TopGridObserver);
    end;
    if frmGoPhast.PhastModel.ThreeDGridObserver <> nil then
    begin
      frmGoPhast.PhastModel.ThreeDGridObserver.StopsTalkingTo(self);
      StopsTalkingTo(frmGoPhast.PhastModel.ThreeDGridObserver);
    end;
  end;
  FContours.Free;

  if FileExists(FTempFileName) then
  begin
    DeleteFile(FTempFileName);
  end;

  if FPhastModel <> nil then
  begin
    (FPhastModel as TPhastModel).FormulaManager.Remove(FFormulaObject,
      GlobalDataArrayRemoveSubscription,
      GlobalDataArrayRestoreSubscription, self);
  end;

  inherited;
end;

function TDataArray.FullClassification: string;
begin
  if Pos(strDefaultClassification, Classification) = 1 then
  begin
    result := Classification;
  end
  else if UsedByModel then
  begin
    result := StrRequired + Classification;
  end
  else
  begin
    result := StrOptional + Classification;
  end;
end;

procedure TDataArray.FullUseList(const AStringList: TStringList);
var
  Index: integer;
  DataSet: TDataArray;
begin
  Assert(AStringList <> nil);
  AStringList.Clear;
  for Index := 0 to frmGoPhast.PhastModel.DataSetCount - 1 do
  begin
    DataSet := frmGoPhast.PhastModel.DataSets[Index];
    DataSet.Observed := False;
  end;

  ObserverList.NotifyOnChange(self, ckCheckDependance);
  for Index := 0 to frmGoPhast.PhastModel.DataSetCount - 1 do
  begin
    DataSet := frmGoPhast.PhastModel.DataSets[Index];
    if DataSet.Observed then
    begin
      AStringList.Add(DataSet.Name)
    end;
  end;
end;

function TDataArray.GetUseList: TStringList;
begin
  // FUseListUpToDate is set to False in Invalidate
  // and to True in UpdateUseList.
  if not FUseListUpToDate then
  begin
    UpdateUseList;
  end;
  result := FUseList;
end;

function TDataArray.IdenticalDataArrayContents(ADataArray: TDataArray): boolean;
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  LocalIsValue: Boolean;
begin
  ADataArray.Initialize;
  Result := (DataType = ADataArray.DataType)
    and (Orientation = ADataArray.Orientation)
    and (EvaluatedAt = ADataArray.EvaluatedAt);
  if result then
  begin
    for LayerIndex := 0 to LayerCount - 1 do
    begin
      for RowIndex := 0 to RowCount - 1 do
      begin
        for ColIndex := 0 to ColumnCount - 1 do
        begin
          LocalIsValue := IsValue[LayerIndex,RowIndex,ColIndex];
          result := LocalIsValue
            = ADataArray.IsValue[LayerIndex,RowIndex,ColIndex];
          if not result then
          begin
            Exit;
          end;
          if LocalIsValue then
          begin
            case DataType of
              rdtDouble:
                begin
                  result := RealData[LayerIndex,RowIndex,ColIndex]
                    = ADataArray.RealData[LayerIndex,RowIndex,ColIndex];
                end;
              rdtInteger: 
                begin
                  result := IntegerData[LayerIndex,RowIndex,ColIndex]
                    = ADataArray.IntegerData[LayerIndex,RowIndex,ColIndex];
                end;
              rdtBoolean: 
                begin
                  result := BooleanData[LayerIndex,RowIndex,ColIndex]
                    = ADataArray.BooleanData[LayerIndex,RowIndex,ColIndex];
                end;
              rdtString: 
                begin
                  result := StringData[LayerIndex,RowIndex,ColIndex]
                    = ADataArray.StringData[LayerIndex,RowIndex,ColIndex];
                end;
            end;
            if not result then
            begin
              Exit;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TDataArray.Initialize;
var
  ColIndex, RowIndex, LayerIndex: integer;
  Compiler: TRbwParser;
  Expression: TExpression;
  ResultTypeOK: boolean;
  TempFormula: string;
  VarIndex: integer;
  VarName: string;
  VarPosition: integer;
  Variable: TCustomValue;
  DataSetIndex: integer;
  AnotherDataSet: TDataArray;
  ScreenObjectIndex: integer;
  AScreenObject: TScreenObject;
  LayerToUse, RowToUse, ColToUse: integer;
  TempUseList: TStringList;
  CellCenter, CellCorner: TPoint2D;
  CellCenter3D, CellCorner3D: T3DRealPoint;
  LayerLimit, RowLimit, ColLimit: integer;
  FreeStack: boolean;
  AnnotationString: string;
  VariablePositions: array of integer;
  DataSetIndexes: array of integer;
  InterpAnnString: string;
//  MinReal: double;
//  MaxReal: double;
//  FirstValue: Boolean;
//  MinInteger: Integer;
//  MaxInteger: Integer;
//  MinBoolean: Boolean;
//  MaxBoolean: Boolean;
//  StringValues: TStringList;
  procedure GetLimits;
  begin
    case EvaluatedAt of
      eaBlocks:
        begin
          LayerLimit := LayerCount - 1;
          RowLimit := RowCount - 1;
          ColLimit := ColumnCount - 1;
        end;
      eaNodes:
        begin
          case Orientation of
            dsoTop:
              begin
                LayerLimit := LayerCount - 1;
                RowLimit := RowCount;
                ColLimit := ColumnCount;
              end;
            dsoFront:
              begin
                LayerLimit := LayerCount;
                RowLimit := RowCount - 1;
                ColLimit := ColumnCount;
              end;
            dsoSide:
              begin
                LayerLimit := LayerCount;
                RowLimit := RowCount;
                ColLimit := ColumnCount - 1;
              end;
            dso3D:
              begin
                LayerLimit := LayerCount;
                RowLimit := RowCount;
                ColLimit := ColumnCount;
              end;
          else
            Assert(False);
          end;
        end;
    else
      Assert(False);
    end;
  end;
begin
  if UpToDate and not DimensionsChanged then
  begin
    CheckRestoreData;
    Exit;
  end;
  FDataCached := False;
  FEvalTime := Now;

  InterpAnnString := '';
  FreeStack := (Stack = nil);
  try
    if FreeStack then
    begin
      Stack := TStringList.Create;
    end;
    if Stack.IndexOf(Name) >= 0 then
    begin
      UpToDate := True;
      raise ECircularReference.Create('Circular reference in ' + Name);
    end;
    Stack.Add(Name);

    GlobalEvaluatedAt := EvaluatedAt;

    if DimensionsChanged then
    begin
      SetDimensions(False);
    end
    else
    begin
      RestoreArraySize;
    end;

    GetLimits;
    UpdateCurrentScreenObject(nil);
    if (Orientation <> dso3D) and (TwoDInterpolator <> nil)
      and TwoDInterpolator.ShouldInterpolate then
    begin
      TwoDInterpolator.Initialize(self);
      if InterpAnnString = '' then
      begin
        InterpAnnString := 'set via ' +
          TwoDInterpolator.InterpolatorName + '.';
      end;
      case Orientation of
        dsoTop:
          begin
            case EvaluatedAt of
              eaBlocks:
                begin
                  for RowIndex := 0 to RowLimit do
                  begin
                    for ColIndex := 0 to ColLimit do
                    begin
                      CellCenter :=
                        frmGoPhast.Grid.TwoDElementCenter(ColIndex,
                        RowIndex);

                      UpdateGlobalLocations(ColIndex,
                        RowIndex, 0, EvaluatedAt);

                      case Datatype of
                        rdtDouble:
                          begin
                            RealData[0, RowIndex, ColIndex] :=
                              TwoDInterpolator.RealResult(CellCenter);
                          end;
                        rdtInteger:
                          begin
                            IntegerData[0, RowIndex, ColIndex] :=
                              TwoDInterpolator.IntegerResult(CellCenter);
                          end;
                        rdtBoolean:
                          begin
                            BooleanData[0, RowIndex, ColIndex] :=
                              TwoDInterpolator.BooleanResult(CellCenter);
                          end;
                        rdtString:
                          begin
                            StringData[0, RowIndex, ColIndex] :=
                              TwoDInterpolator.StringResult(CellCenter);
                          end;
                      else
                        Assert(False);
                      end;
                      Annotation[0, RowIndex, ColIndex] := InterpAnnString;
                    end
                  end;
                end;
              eaNodes:
                begin
                  for RowIndex := 0 to RowLimit do
                  begin
                    for ColIndex := 0 to ColLimit do
                    begin
                      CellCorner :=
                        frmGoPhast.Grid.TwoDElementCorner(ColIndex,
                        RowIndex);
                      UpdateGlobalLocations(ColIndex,
                        RowIndex, 0, EvaluatedAt);

                      case Datatype of
                        rdtDouble:
                          begin
                            RealData[0, RowIndex, ColIndex] :=
                              TwoDInterpolator.RealResult(CellCorner);
                          end;
                        rdtInteger:
                          begin
                            IntegerData[0, RowIndex, ColIndex] :=
                              TwoDInterpolator.IntegerResult(CellCorner);
                          end;
                        rdtBoolean:
                          begin
                            BooleanData[0, RowIndex, ColIndex] :=
                              TwoDInterpolator.BooleanResult(CellCorner);
                          end;
                        rdtString:
                          begin
                            StringData[0, RowIndex, ColIndex] :=
                              TwoDInterpolator.StringResult(CellCorner);
                          end;
                      else
                        Assert(False);
                      end;
                      Annotation[0, RowIndex, ColIndex] := InterpAnnString
                    end
                  end;
                end;
            else
              begin
                Assert(False);
              end;
            end;
          end;
        dsoFront:
          begin
            case EvaluatedAt of
              eaBlocks:
                begin
                  for LayerIndex := 0 to LayerLimit do
                  begin
                    for ColIndex := 0 to ColLimit do
                    begin
                      CellCenter3D := frmGoPhast.Grid.ThreeDElementCenter(
                        ColIndex, 0, LayerIndex);

                      UpdateGlobalLocations(ColIndex, 0,
                        LayerIndex, EvaluatedAt);

                      CellCenter.X := CellCenter3D.X;
                      CellCenter.Y := CellCenter3D.Z;

                      case Datatype of
                        rdtDouble:
                          begin
                            RealData[LayerIndex, 0, ColIndex] :=
                              TwoDInterpolator.RealResult(CellCenter);
                          end;
                        rdtInteger:
                          begin
                            IntegerData[LayerIndex, 0, ColIndex] :=
                              TwoDInterpolator.IntegerResult(CellCenter);
                          end;
                        rdtBoolean:
                          begin
                            BooleanData[LayerIndex, 0, ColIndex] :=
                              TwoDInterpolator.BooleanResult(CellCenter);
                          end;
                        rdtString:
                          begin
                            StringData[LayerIndex, 0, ColIndex] :=
                              TwoDInterpolator.StringResult(CellCenter);
                          end;
                      else
                        Assert(False);
                      end;
                      Annotation[LayerIndex, 0, ColIndex] := InterpAnnString;
                    end;
                  end;
                end;
              eaNodes:
                begin
                  for LayerIndex := 0 to LayerLimit do
                  begin
                    for ColIndex := 0 to ColLimit do
                    begin
                      CellCorner3D := frmGoPhast.Grid.ThreeDElementCorner(
                        ColIndex, 0, LayerIndex);

                      UpdateGlobalLocations(ColIndex, 0,
                        LayerIndex, EvaluatedAt);

                      CellCorner.X := CellCorner3D.X;
                      CellCorner.Y := CellCorner3D.Z;

                      case Datatype of
                        rdtDouble:
                          begin
                            RealData[LayerIndex, 0, ColIndex] :=
                              TwoDInterpolator.RealResult(CellCorner);
                          end;
                        rdtInteger:
                          begin
                            IntegerData[LayerIndex, 0, ColIndex] :=
                              TwoDInterpolator.IntegerResult(CellCorner);
                          end;
                        rdtBoolean:
                          begin
                            BooleanData[LayerIndex, 0, ColIndex] :=
                              TwoDInterpolator.BooleanResult(CellCorner);
                          end;
                        rdtString:
                          begin
                            StringData[LayerIndex, 0, ColIndex] :=
                              TwoDInterpolator.StringResult(CellCorner);
                          end;
                      else
                        Assert(False);
                      end;
                      Annotation[LayerIndex, 0, ColIndex] := InterpAnnString;
                    end;
                  end;
                end;
            else
              begin
                Assert(False);
              end;

            end;
          end;
        dsoSide:
          begin
            case EvaluatedAt of
              eaBlocks:
                begin
                  for LayerIndex := 0 to LayerLimit do
                  begin
                    for RowIndex := 0 to RowLimit do
                    begin
                      CellCenter3D := frmGoPhast.Grid.ThreeDElementCenter(
                        0, RowIndex, LayerIndex);

                      UpdateGlobalLocations(0,
                        RowIndex, LayerIndex, EvaluatedAt);

                      CellCenter.X := CellCenter3D.Y;
                      CellCenter.Y := CellCenter3D.Z;

                      case Datatype of
                        rdtDouble:
                          begin
                            RealData[LayerIndex, RowIndex, 0] :=
                              TwoDInterpolator.RealResult(CellCenter);
                          end;
                        rdtInteger:
                          begin
                            IntegerData[LayerIndex, RowIndex, 0] :=
                              TwoDInterpolator.IntegerResult(CellCenter);
                          end;
                        rdtBoolean:
                          begin
                            BooleanData[LayerIndex, RowIndex, 0] :=
                              TwoDInterpolator.BooleanResult(CellCenter);
                          end;
                        rdtString:
                          begin
                            StringData[LayerIndex, RowIndex, 0] :=
                              TwoDInterpolator.StringResult(CellCenter);
                          end;
                      else
                        Assert(False);
                      end;
                      Annotation[LayerIndex, RowIndex, 0] := InterpAnnString;
                    end;
                  end;
                end;
              eaNodes:
                begin
                  for LayerIndex := 0 to LayerLimit do
                  begin
                    for RowIndex := 0 to RowLimit do
                    begin
                      CellCorner3D :=
                        frmGoPhast.Grid.ThreeDElementCorner(
                        0, RowIndex, LayerIndex);

                      UpdateGlobalLocations(0,
                        RowIndex, LayerIndex, EvaluatedAt);

                      CellCorner.X := CellCorner3D.Y;
                      CellCorner.Y := CellCorner3D.Z;

                      case Datatype of
                        rdtDouble:
                          begin
                            RealData[LayerIndex, RowIndex, 0] :=
                              TwoDInterpolator.RealResult(CellCorner);
                          end;
                        rdtInteger:
                          begin
                            IntegerData[LayerIndex, RowIndex, 0] :=
                              TwoDInterpolator.IntegerResult(CellCorner);
                          end;
                        rdtBoolean:
                          begin
                            BooleanData[LayerIndex, RowIndex, 0] :=
                              TwoDInterpolator.BooleanResult(CellCorner);
                          end;
                        rdtString:
                          begin
                            StringData[LayerIndex, RowIndex, 0] :=
                              TwoDInterpolator.StringResult(CellCorner);
                          end;
                      else
                        Assert(False);
                      end;
                      Annotation[LayerIndex, RowIndex, 0] := InterpAnnString;
                    end;
                  end;
                end;
            else
              begin
                Assert(False);
              end;
            end;
          end;
      else
        Assert(False);
      end;
      TwoDInterpolator.Finalize(self);
    end
    else
    begin
      TempUseList := TStringList.Create;
      try
        TempUseList.Duplicates := dupIgnore;
        TempUseList.Sorted := True;
        TempUseList.Capacity := UseList.Count;
        TempUseList.Assign(UseList);

        for VarIndex := 0 to TempUseList.Count - 1 do
        begin
          VarName := TempUseList[VarIndex];
          AnotherDataSet := frmGoPhast.PhastModel.GetDataSetByName(VarName);
          if AnotherDataSet <> nil then
          begin
            Assert(AnotherDataSet <> self);
            AnotherDataSet.Initialize;
            frmGoPhast.PhastModel.AddDataSetToCache(AnotherDataSet);
          end;
        end;
        GlobalEvaluatedAt := EvaluatedAt;

        Compiler := GetCompiler;
        if ParameterUsed then
        begin
          TempFormula := ParameterFormula;
        end
        else
        begin
          TempFormula := Formula;
        end;

        try
          Compiler.Compile(TempFormula);
        except on E: ERbwParserError do
          begin
            ResetFormula(Compiler, E.Message);
          end;
        end;

        if ParameterUsed then
        begin
          TempFormula := ParameterFormula;
        end
        else
        begin
          TempFormula := Formula;
        end;
        Compiler.Compile(TempFormula);
        Expression := Compiler.CurrentExpression;
        ResultTypeOK := (Expression.ResultType = Datatype)
          or ((Expression.ResultType = rdtInteger) and (Datatype = rdtDouble));
        if not ResultTypeOK then
        begin
          ResetFormula(Compiler, 'Invalid data type.');
          if ParameterUsed then
          begin
            TempFormula := ParameterFormula;
          end
          else
          begin
            TempFormula := Formula;
          end;
          Compiler.Compile(TempFormula);
          Expression := Compiler.CurrentExpression;
          ResultTypeOK := (Expression.ResultType = Datatype)
            or ((Expression.ResultType = rdtInteger) and (Datatype = rdtDouble));
          if not ResultTypeOK then
          begin
            raise EInvalidDataType.Create('Invalid data type.');
          end;
        end;

        if ParameterUsed then
        begin
          AnnotationString :=
            'set using parameters via the formula: ' + TempFormula;
        end
        else
        begin
          AnnotationString :=
            'set via default formula: ' + TempFormula;
        end;

        SetLength(VariablePositions, TempUseList.Count);
        SetLength(DataSetIndexes, TempUseList.Count);
        for VarIndex := 0 to TempUseList.Count - 1 do
        begin
          VarName := TempUseList[VarIndex];
          VarPosition := Compiler.IndexOfVariable(VarName);
          VariablePositions[VarIndex] := VarPosition;
          if VarPosition >= 0 then
          begin
            DataSetIndex := frmGoPhast.PhastModel.IndexOfDataSet(VarName);
            DataSetIndexes[VarIndex] := DataSetIndex;
          end
          else
          begin
            DataSetIndexes[VarIndex] := -1;
          end;
        end;

        for LayerIndex := 0 to LayerLimit do
        begin
          for RowIndex := 0 to RowLimit do
          begin
            for ColIndex := 0 to ColLimit do
            begin

              UpdateGlobalLocations(ColIndex, RowIndex, LayerIndex,
                EvaluatedAt);

              for VarIndex := 0 to TempUseList.Count - 1 do
              begin
                VarName := TempUseList[VarIndex];
                VarPosition := VariablePositions[VarIndex];
                if VarPosition >= 0 then
                begin
                  Variable := Compiler.Variables[VarPosition];
                  DataSetIndex := DataSetIndexes[VarIndex];
                  if DataSetIndex >= 0 then
                  begin
                    AnotherDataSet := frmGoPhast.PhastModel.
                      DataSets[DataSetIndex];
                    Assert(AnotherDataSet <> self);
                    Assert(AnotherDataSet.DataType = Variable.ResultType);
                    if AnotherDataSet.Orientation = dsoTop then
                    begin
                      LayerToUse := 0;
                    end
                    else
                    begin
                      LayerToUse := LayerIndex;
                    end;
                    if AnotherDataSet.Orientation = dsoFront then
                    begin
                      RowToUse := 0;
                    end
                    else
                    begin
                      RowToUse := RowIndex;
                    end;
                    if AnotherDataSet.Orientation = dsoSide then
                    begin
                      ColToUse := 0;
                    end
                    else
                    begin
                      ColToUse := ColIndex;
                    end;

                    case Variable.ResultType of
                      rdtDouble:
                        begin
                          TRealVariable(Variable).Value :=
                            AnotherDataSet.RealData[LayerToUse, RowToUse,
                            ColToUse];
                        end;
                      rdtInteger:
                        begin
                          TIntegerVariable(Variable).Value :=
                            AnotherDataSet.IntegerData[LayerToUse, RowToUse,
                            ColToUse];
                        end;
                      rdtBoolean:
                        begin
                          TBooleanVariable(Variable).Value :=
                            AnotherDataSet.BooleanData[LayerToUse, RowToUse,
                            ColToUse];
                        end;
                      rdtString:
                        begin
                          TStringVariable(Variable).Value :=
                            AnotherDataSet.StringData[LayerToUse, RowToUse,
                            ColToUse];
                        end;
                    else
                      Assert(False);
                    end;
                  end;
                end;
              end;
              try
                Expression.Evaluate;
              except on E: ERbwParserError do
                begin
                  ResetFormula(Compiler, E.Message);
                  TempFormula := Formula;
                  Compiler.Compile(TempFormula);
                  Expression := Compiler.CurrentExpression;
                  Expression.Evaluate;
                end;
              end;

              case Datatype of
                rdtDouble:
                  begin
                    RealData[LayerIndex, RowIndex, ColIndex]
                      := Expression.DoubleResult;
                  end;
                rdtInteger:
                  begin
                    IntegerData[LayerIndex, RowIndex, ColIndex]
                      := Expression.IntegerResult;
                  end;
                rdtBoolean:
                  begin
                    BooleanData[LayerIndex, RowIndex, ColIndex]
                      := Expression.BooleanResult;
                  end;
                rdtString:
                  begin
                    StringData[LayerIndex, RowIndex, ColIndex]
                      := Expression.StringResult;
                  end;
              else
                Assert(False);
              end;
              Annotation[LayerIndex, RowIndex, ColIndex] := AnnotationString;
            end;
          end;
        end;
      finally
        TempUseList.Free;
      end;
    end;

    if not ParameterUsed then
    begin
      for ScreenObjectIndex := 0 to
        frmGoPhast.PhastModel.ScreenObjectCount - 1 do
      begin
        AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
        if not AScreenObject.Deleted then
        begin
          AScreenObject.AssignValuesToPhastDataSet(frmGoPhast.Grid, self);
        end;
      end;
    end;
  finally
    if FreeStack then
    begin
      FreeAndNil(Stack);
      frmGoPhast.PhastModel.CacheDataArrays;
    end;
  end;
  PostInitialize;

  UpToDate := True;
  FCleared := False;
  CheckIfUniform;
end;

procedure TDataArray.Invalidate;
begin
  UpToDate := False;
end;

procedure TDataArray.SetDataType(const Value: TRbwDataType);
var
  NumberOfLayers, NumberOfRows, NumberOfColumns: integer;
  Index: integer;
  AScreenObject: TScreenObject;
  Position: integer;
  formula: string;
begin
  if FDataType <> Value then
  begin
    // Adjust the formulas of all the screen objects that
    // set the value of this data set directly.
    // The formulas of screen objects that set the value
    // of this data set indirectly are not changed.

    if frmGoPhast.PhastModel <> nil then
    begin
      for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
      begin
        AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
        Position := AScreenObject.IndexOfDataSet(self);
        if Position >= 0 then
        begin
          formula := AScreenObject.DataSetFormulas[Position];
          formula := AdjustFormula(formula, FDataType, Value);
          AScreenObject.DataSetFormulas[Position] := Formula;
        end;
      end;
      frmGoPhast.InvalidateModel;
    end;

    // Store the current dimensions.
    NumberOfLayers := FLayerCount;
    NumberOfRows := FRowCount;
    NumberOfColumns := FColumnCount;
    // deallocate array
    SetDimensions(True);
    FDataType := Value;
    // reallocate array with new data type.
    UpdateDimensions(NumberOfLayers, NumberOfRows, NumberOfColumns);
    UpToDate := False;
  end;
end;

procedure TDataArray.GetRequiredDimensions(out NumberOfLayers, NumberOfRows,
  NumberOfColumns: integer);
begin
  NumberOfLayers := FLayerCount;
  NumberOfRows := FRowCount;
  NumberOfColumns := FColumnCount;
  case EvaluatedAt of
    eaBlocks:
      begin
        // do nothing
      end;
    eaNodes:
      begin
        case Orientation of
          dsoTop:
            begin
              Inc(NumberOfRows);
              Inc(NumberOfColumns);
            end;
          dsoFront:
            begin
              Inc(NumberOfLayers);
              Inc(NumberOfColumns);
            end;
          dsoSide:
            begin
              Inc(NumberOfLayers);
              Inc(NumberOfRows);
            end;
          dso3D:
            begin
              Inc(NumberOfLayers);
              Inc(NumberOfRows);
              Inc(NumberOfColumns);
            end;
        else
          Assert(False);
        end;
      end;
  else
    begin
      Assert(False);
    end;
  end;
end;

function TDataArray.GetRowCount: integer;
begin
  if (IsUniform = iuTrue) or FCleared then
  begin
    result := FCachedRowCount;
  end
  else
  begin
    result := FRowCount;
  end;
end;

procedure TDataArray.SetDimensions(const SetToZero: boolean);
var
  NumberOfLayers: integer;
  NumberOfRows: integer;
  NumberOfColumns: integer;
begin
  if SetToZero then
  begin
    NumberOfLayers := 0;
    NumberOfRows := 0;
    NumberOfColumns := 0;
    FLayerCount := NumberOfLayers;
    FRowCount := NumberOfRows;
    FColumnCount := NumberOfColumns;
  end
  else
  begin
    GetRequiredDimensions(NumberOfLayers, NumberOfRows, NumberOfColumns);
  end;

  NumberOfLayers := Math.Max(NumberOfLayers, 0);
  NumberOfRows := Math.Max(NumberOfRows, 0);
  NumberOfColumns := Math.Max(NumberOfColumns, 0);

  case FDataType of
    rdtDouble:
      begin
        SetLength(T3DRealDataSet(FDataArray), NumberOfLayers, NumberOfRows,
          NumberOfColumns);
      end;
    rdtInteger:
      begin
        SetLength(T3DIntegerDataSet(FDataArray), NumberOfLayers, NumberOfRows,
          NumberOfColumns);
      end;
    rdtBoolean:
      begin
        SetLength(T3DBooleanDataSet(FDataArray), NumberOfLayers, NumberOfRows,
          NumberOfColumns);
      end;
    rdtString:
      begin
        SetLength(T3DStringDataSet(FDataArray), NumberOfLayers, NumberOfRows,
          NumberOfColumns);
      end;
  else
    Assert(False);
  end;
  SetLength(FAnnotation, NumberOfLayers, NumberOfRows, NumberOfColumns);
  FDimensionsChanged := False;
end;

procedure TDataArray.SetFormula(const Value: string);
var
  P: Pointer;
begin
  FFormula := Formula;
  ChangeAFormula(Value, FFormula, FUseListUpToDate, GetUseList);
  P := Addr(GlobalDataArrayRemoveSubscription);
  if Assigned(P) then
  begin
    TPhastModel(FPhastModel).FormulaManager.ChangeFormula(FFormulaObject,
      FFormula, GetCompiler, GlobalDataArrayRemoveSubscription,
      GlobalDataArrayRestoreSubscription, self);
  end;
end;

procedure TDataArray.SetOrientation(const Value: TDataSetOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    FDimensionsChanged := True;
    UpToDate := False;
    case FOrientation of
      dsoTop:
        begin
          (FPhastModel as TPhastModel).ThreeDGridObserver.StopsTalkingTo(self);
          (FPhastModel as TPhastModel).TopGridObserver.TalksTo(self);
        end;
      dsoFront, dsoSide, dso3D:
        begin
          (FPhastModel as TPhastModel).ThreeDGridObserver.TalksTo(self);
          (FPhastModel as TPhastModel).TopGridObserver.StopsTalkingTo(self);
        end;
      else Assert(False);
    end;
    frmGoPhast.InvalidateModel;
  end;
  FFormulaObject.Parser := GetCompiler;
end;

procedure TDataArray.SetParameterFormula(const Value: string);
begin
  ChangeAFormula(Value, FParameterFormula, FUseListUpToDate, GetUseList)
end;

procedure TDataArray.SetParameterUsed(const Value: boolean);
begin
  if FParameterUsed <> Value then
  begin
    FParameterUsed := Value;
    FUseListUpToDate := False;
    UpToDate := False;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TDataArray.SetContours(const Value: TContours);
begin
  if (FContours = nil) and (Value <> nil) then
  begin
    FContours := TContours.Create;
  end;
  if Value = nil then
  begin
    FreeAndNil(FContours);
  end
  else
  begin
    FContours.Assign(Value);
  end;
end;

procedure TDataArray.SetTwoDInterpolator(const Value: TCustom2DInterpolater);
begin
  if Value = nil then
  begin
    if FTwoDInterpolator <> nil then
    begin
      FTwoDInterpolator.Free;
      FTwoDInterpolator := nil;
      UpToDate := False;
      frmGoPhast.InvalidateModel;
    end;
  end
  else
  begin
    FTwoDInterpolator.Free;
    try
      FTwoDInterpolator := TInterpolatorType(Value.ClassType).Create(self);
      FTwoDInterpolator.SetSubComponent(True);
      FTwoDInterpolator.Assign(Value);
    except on E: EInterpolationException do
      begin
        FTwoDInterpolator := nil;
      end;
    end;
    UpToDate := False;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TDataArray.SetUnits(const Value: string);
begin
  if FUnits <> Value then
  begin
    FUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TDataArray.SetUpToDate(const Value: boolean);
var
  Updated: boolean;
begin
  if (frmGoPhast.PhastModel <> nil)
    and (csDestroying in frmGoPhast.PhastModel.ComponentState) then Exit;

  FIsUniform := iuUnknown;
  frmGoPhast.InvalidateModel;
  FUseListUpToDate := False;

  Updated := Value and not UpToDate;
  inherited;
  // doing this causes to much thrashing when evaluating
  // transient data.
  // ScreenObjectsChanged is called every time a TDataArray
  // is created.
{  if not Value then
  begin
    (FPhastModel as TPhastModel).ScreenObjectsChanged(self);
  end; }
  if Updated then
  begin
    UpdateMinMaxValues;
  end;
  if frmGoPhast.Grid <> nil then
  begin
    if ((frmGoPhast.Grid.TopDataSet <> nil)
      and not frmGoPhast.Grid.TopDataSet.UpToDate)
      or ((frmGoPhast.Grid.TopContourDataSet <> nil)
      and not frmGoPhast.Grid.TopContourDataSet.UpToDate) then
    begin
      frmGoPhast.Grid.NeedToRecalculateTopCellColors := True;
      frmGoPhast.frameTopView.ZoomBox.Image32.Invalidate;
    end;
    if ((frmGoPhast.Grid.FrontDataSet <> nil)
      and not frmGoPhast.Grid.FrontDataSet.UpToDate)
      or ((frmGoPhast.Grid.FrontContourDataSet <> nil)
      and not frmGoPhast.Grid.FrontContourDataSet.UpToDate) then
    begin
      frmGoPhast.Grid.NeedToRecalculateFrontCellColors := True;
      frmGoPhast.frameFrontView.ZoomBox.Image32.Invalidate;
    end;
    if ((frmGoPhast.Grid.SideDataSet <> nil)
      and not frmGoPhast.Grid.SideDataSet.UpToDate)
      or ((frmGoPhast.Grid.SideContourDataSet <> nil)
      and not frmGoPhast.Grid.SideContourDataSet.UpToDate) then
    begin
      frmGoPhast.Grid.NeedToRecalculateSideCellColors := True;
      frmGoPhast.frameSideView.ZoomBox.Image32.Invalidate;
    end;
    if ((frmGoPhast.Grid.ThreeDDataSet <> nil)
      and not frmGoPhast.Grid.ThreeDDataSet.UpToDate)
      or ((frmGoPhast.Grid.ThreeDContourDataSet <> nil)
      and not frmGoPhast.Grid.ThreeDContourDataSet.UpToDate) then
    begin
      frmGoPhast.Grid.NeedToRecalculate3DCellColors := True;
      frmGoPhast.Grid.GridChanged;
    end;
  end;
end;

procedure TDataArray.UpdateDimensions(NumberOfLayers, NumberOfRows,
  NumberOfColumns: integer);
begin
  case Orientation of
    dsoTop:
      begin
        NumberOfLayers := 1;
      end;
    dsoFront:
      begin
        NumberOfRows := 1;
      end;
    dsoSide:
      begin
        NumberOfColumns := 1;
      end;
    dso3D:
      begin
      end;
  else
    Assert(False);
  end;

  if NumberOfColumns < -1 then
  begin
    raise EInvalidDataType.Create(
      'The number of columns in a data set must be '
      + 'greater than or equal to -1.');
  end;
  if NumberOfRows < -1 then
  begin
    raise EInvalidDataType.Create(
      'The number of rows in a data set must be greater than or equal to -1.');
  end;
  if NumberOfLayers < -1 then
  begin
    raise EInvalidDataType.Create(
      'The number of layers in a data set '
      + 'must be greater than or equal to -1.');
  end;
  if (FLayerCount <> NumberOfLayers)
    or (FRowCount <> NumberOfRows)
    or (FColumnCount <> NumberOfColumns) then
  begin
    if FDataCached and FCleared
      and (FCachedLayerCount = NumberOfLayers)
      and (FCachedRowCount = NumberOfRows)
      and (FCachedColumnCount = NumberOfColumns) then
    begin
      // do nothing.
    end
    else
    begin
      FDimensionsChanged := True;
    end;
    FLayerCount := NumberOfLayers;
    FCachedLayerCount := NumberOfLayers;
    FRowCount := NumberOfRows;
    FCachedRowCount := NumberOfRows;
    FColumnCount := NumberOfColumns;
    FCachedColumnCount := NumberOfColumns;
  end;
end;

procedure TDataArray.UpdateUseList;
var
  Compiler: TRbwParser;
  TempFormula: string;
begin
  Compiler := GetCompiler;
  if ParameterUsed then
  begin
    TempFormula := ParameterFormula;
  end
  else
  begin
    TempFormula := FFormula;
  end;
  if TempFormula = '' then
  begin
    TempFormula := '0';
  end;
  try
    Compiler.Compile(TempFormula);
    FUseList.Assign(Compiler.CurrentExpression.VariablesUsed);
  except on E: ERbwParserError do
    begin
      ResetFormula(Compiler, E.Message);
      FUseList.Clear;
      Exit;
    end;
  end;
  FUseListUpToDate := True;
end;

constructor TDataArray.Create(AnOwner: TComponent);
begin
  Assert(AnOwner <> nil);
  Assert(AnOwner is TPhastModel);
  FPhastModel := AnOwner;
  inherited Create(nil);
  FFormulaObject := TPhastModel(AnOwner).FormulaManager.Add;
  FFormulaObject.AddSubscriptionEvents(GlobalDataArrayRemoveSubscription,
      GlobalDataArrayRestoreSubscription, self);
  FLimits := TColoringLimits.Create;
  FLimits.UpperLimit.OnChange := LimitsChanged;
  FLimits.LowerLimit.OnChange := LimitsChanged;
  FContourLimits := TColoringLimits.Create;
  FContourLimits.UpperLimit.OnChange := ContourLimitsChanged;
  FContourLimits.LowerLimit.OnChange := ContourLimitsChanged;
  FUseList := TStringList.Create;
  FUseList.Sorted := True;
  FUseList.Duplicates := dupAccept;
  UpToDate := False;
  FVisible := True;
  FUseListUpToDate := False;
end;

procedure TDataArray.SetEvaluatedAt(const Value: TEvaluatedAt);
begin
  if FEvaluatedAt <> Value then
  begin
    FEvaluatedAt := Value;
    UpToDate := False;
    FDimensionsChanged := True;
    frmGoPhast.InvalidateModel;
  end;
  FFormulaObject.Parser := GetCompiler;
end;

procedure TDataArray.SetLock(const Value: TDataLock);
begin
  FLock := Value;
end;

procedure TDataArray.SetClassification(const Value: string);
begin
  FClassification := Value;
end;

procedure TDataArray.SetComment(const Value: string);
begin
  if FComment <> Value then
  begin
    FComment := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TDataArray.SetContourLimits(const Value: TColoringLimits);
begin
  FContourLimits.Assign(Value) ;
end;

function TDataArray.GetBooleanData(const Layer, Row, Col: integer): boolean;
var
  AnArray: T3DBooleanDataSet;
begin
  if FIsUniform = iuTrue then
  begin
    result := FUniformBooleanValue;
    Exit;
  end;
  CheckRestoreData;
  GetBoolArray(AnArray);
  result := AnArray[Layer, Row, Col];
  UpdateEvalTime;
end;

function TDataArray.GetClassification: string;
begin
  result := FClassification;
  if result = '' then
  begin
    if (Name = rsActive)
      or (Name = rsKx)
      or (Name = rsKy)
      or (Name = rsKz)
      or (Name = rsPorosity)
      or (Name = rsSpecific_Storage)
      or (Name = rsLong_Dispersivity)
      or (Name = rsHorizontal_Transv_Dispersivity)
      or (Name = rsVertical_Transv_Dispersivity)
      or (Name = rsInitial_Head)
      or (Name = rsInitial_Water_Table)
      or (Name = rsModflow_Initial_Head)
      or (Name = rsModflow_CBKz)
      or (Name = rsSpecificYield)
      or (Name = rsWetDryThreshold)
      or (Name = rsWetDryFlag)
      or (Name = rsWetDry)
      or (Name = rsHorizontalAnisotropy)
      or (Name = rsVerticalAnisotropy)
      then
    begin
      result := StrHydrology;
    end
    else if (Name = rsChemistry_Initial_Solution)
      or (Name = rsChemistry_Initial_Equilibrium_Phases)
      or (Name = rsChemistry_Initial_Surface)
      or (Name = rsChemistry_Initial_Exchange)
      or (Name = rsChemistry_Initial_Gas_Phase)
      or (Name = rsChemistry_Initial_Solid_Solutions)
      or (Name = rsChemistry_Initial_Kinetics)
      then
    begin
      result := StrChemistry;
    end else if (Name = rsPrint_Chemistry)
      or (Name = rsPrint_XYZ_Chemistry)
      then
    begin
      result := StrOutput;
    end
    else if (Name = rsTopLeakyHydraulicConductivity)
      or (Name = rsTopLeakyThickness)
      or (Name = rsFrontLeakyHydraulicConductivity)
      or (Name = rsFrontLeakyThickness)
      or (Name = rsSideLeakyHydraulicConductivity)
      or (Name = rsSideLeakyThickness)
      then
    begin
      result := 'PHAST Leaky';
    end
    else if (Name = rsRiverHydraulicConductivity)
      or (Name = rsRiverWidth)
      or (Name = rsRiverDepth)
      or (Name = rsRiverBedThickness)
      then
    begin
      result := 'PHAST River';
    end
    else if (Name = rsSolutionType)
      then
    begin
      result := 'PHAST Specified Head';
    end
    else
    begin
      result := strDefaultClassification;
    end;
    if result <> strDefaultClassification then
    begin
      FClassification := result;
    end;
  end;
end;

function TDataArray.GetColumnCount: integer;
begin
  if (IsUniform = iuTrue) or FCleared {and FDataCached} then
  begin
    result := FCachedColumnCount;
  end
  else
  begin
    result := FColumnCount;
  end;
end;

function TDataArray.GetCompiler: TRbwParser;
var
  LocalModel: TPhastModel;
begin
  LocalModel := FPhastModel as TPhastModel;
  result := LocalModel.GetCompiler(Orientation, EvaluatedAt);
end;

function TDataArray.GetFormula: string;
begin
  result := FFormulaObject.Formula;
end;

function TDataArray.GetHash: longint;
begin
  result := FHash;
end;

function TDataArray.GetIntegerData(const Layer, Row, Col: integer): integer;
var
  AnArray: T3DIntegerDataSet;
begin
  if FIsUniform = iuTrue then
  begin
    result := FUniformIntegerValue;
    Exit;
  end;
  CheckRestoreData;
  GetIntegerArray(AnArray);
  result := AnArray[Layer, Row, Col];
  UpdateEvalTime;
end;

function TDataArray.GetRealData(const Layer, Row, Col: integer): double;
var
  AnArray: T3DRealDataSet;
begin
  if FIsUniform = iuTrue then
  begin
    result := FUniformRealValue;
    Exit;
  end;
  CheckRestoreData;
  GetRealArray(AnArray);
  result := AnArray[Layer, Row, Col];
  UpdateEvalTime;
end;

function TDataArray.GetStringData(const Layer, Row, Col: integer): string;
var
  AnArray: T3DStringDataSet;
begin
  if FIsUniform = iuTrue then
  begin
    result := FUniformStringValue;
    Exit;
  end;
  CheckRestoreData;
  GetStringArray(AnArray);
  result := AnArray[Layer, Row, Col];
  UpdateEvalTime;
end;

procedure TDataArray.SetBooleanData(const Layer, Row, Col: integer;
  const Value: boolean);
var
  AnArray: T3DBooleanDataSet;
begin
  GetBoolArray(AnArray);
  AnArray[Layer, Row, Col] := Value;
  UpdateEvalTime;
end;

procedure TDataArray.SetIntegerData(const Layer, Row, Col, Value: integer);
var
  AnArray: T3DIntegerDataSet;
begin
  GetIntegerArray(AnArray);
  if CheckMin and (Value < Min) then
  begin
    AnArray[Layer, Row, Col] := Trunc(Min);
  end
  else if CheckMax and (Value > Max) then
  begin
    AnArray[Layer, Row, Col] := Trunc(Max);
  end
  else
  begin
    AnArray[Layer, Row, Col] := Value;
  end;
  UpdateEvalTime;
end;

procedure TDataArray.UpdateEvalTime;
const
  TenthSecond = 1/24/3600/10;
begin
  if (Now - FEvalTime) > TenthSecond then
  begin
    FEvalTime := Now;
//    Application.ProcessMessages;
  end;
end;

procedure TDataArray.SetRealData(const Layer, Row, Col: integer;
  const Value: double);
var
  AnArray: T3DRealDataSet;
begin
  GetRealArray(AnArray);
  if CheckMin and (Value < Min) then
  begin
    AnArray[Layer, Row, Col] := Min;
  end
  else if CheckMax and (Value > Max) then
  begin
    AnArray[Layer, Row, Col] := Max;
  end
  else
  begin
    AnArray[Layer, Row, Col] := Value;
  end;
  UpdateEvalTime;
end;

procedure TDataArray.SetStringData(const Layer, Row, Col: integer;
  const Value: string);
var
  AnArray: T3DStringDataSet;
begin
  GetStringArray(AnArray);
  AnArray[Layer, Row, Col] := Value;
  UpdateEvalTime;
end;

function TDataArray.GetIsValue(const Layer, Row, Col: Integer): boolean;
begin
  if IsUniform = iuTrue then
  begin
    result := True;
    Exit;
  end;
  CheckRestoreData;
  result := True;
end;

function TDataArray.GetOwner: TPersistent;
begin
  result := frmGoPhast.PhastModel;
end;

function TDataArray.GetTwoDInterpolatorClass: string;
begin
  if TwoDInterpolator = nil then
  begin
    result := '';
  end
  else
  begin
    result := TwoDInterpolator.ClassName;
  end;
end;

procedure TDataArray.SetTwoDInterpolatorClass(const Value: string);
begin
  FreeAndNil(FTwoDInterpolator);
  try
    FTwoDInterpolator := TInterpolatorClass(GetClass(Value)).Create(self);
    FTwoDInterpolator.SetSubComponent(True);
  except on E: EInterpolationException do
    begin
      FTwoDInterpolator := nil;
    end;
  end;
end;

function TDataArray.GetAnnotation(const Layer, Row, Col: integer): string;
begin
  if FIsUniform = iuTrue then
  begin
    result := FUniformAnnotation;
    Exit;
  end;
  CheckRestoreData;
  result := FAnnotation[Layer, Row, Col];
  UpdateEvalTime;
end;

procedure TDataArray.SetAnnotation(const Layer, Row, Col: integer;
  const Value: string);
begin
  FAnnotation[Layer, Row, Col] := Value;
  UpdateEvalTime;
end;

function TDataArray.DisplayRealValue: boolean;
begin
  Assert(Datatype = rdtInteger);
  result := False;
end;

procedure TDataArray.SetVisible(const Value: boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
//    frmGoPhast.InvalidateModel;
  end;
end;

procedure TDataArray.SetName(const Value: TComponentName);
var
  LocalModel :  TPhastModel;
  MustAdd: boolean;
begin
  LocalModel := FPhastModel as TPhastModel;
  MustAdd := False;
  if Name <> Value then
  begin
    if LocalModel.GetDataSetByName(Name) <> nil then
    begin
      LocalModel.RemoveDataSetFromLookUpList(self);
      MustAdd := True;
    end;
  end;
  if Name <> '' then
  begin
    LocalModel.ThreeDGridObserver.StopsTalkingTo(self);
    LocalModel.TopGridObserver.StopsTalkingTo(self);
  end;
  inherited;
  if MustAdd then
  begin
    LocalModel.AddDataSetToLookUpList(self);
  end;
  case FOrientation of
    dsoTop:
      begin
        LocalModel.TopGridObserver.TalksTo(self);
      end;
    dsoFront, dsoSide, dso3D:
      begin
        LocalModel.ThreeDGridObserver.TalksTo(self);
      end;
    else Assert(False);
  end;
  frmGoPhast.InvalidateModel;
end;

function TDataArray.UsedByModel: boolean;
begin
  if Assigned(OnDataSetUsed) then
  begin
    result := OnDataSetUsed(self);
  end
  else
  begin
    result := False;
  end;
end;

function TDataArray.ColorGridValueOK(const Layer, Row, Col: integer): boolean;
begin
  Result := ValueOK(Layer, Row, Col, Limits);
end;

procedure TDataArray.ComputeHash;
const
  DoubleLimit = 7;
  IntLimit = 3;
Type
  TDoubleBytes = array[0..DoubleLimit] of Byte;
  TIntBytes = array[0..IntLimit] of Byte;
var
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  DoubleBytes: TDoubleBytes;
  ABool: ByteBool;
  IntBytes: TIntBytes;
  ByteIndex: Integer;
  G: longint;
begin
  // modified from PJW hash function in Bucknall, J. M. 2006.
  // The Tomes of Delphi: Algorithms and Data Structures. 
  Initialize;
  case DataType of
    rdtDouble:
      begin
        FHash := 0;
        for LayerIndex := 0 to LayerCount - 1 do
        begin
          for RowIndex := 0 to RowCount - 1 do
          begin
            for ColIndex := 0 to ColumnCount - 1 do
            begin
              if IsValue[LayerIndex, RowIndex, ColIndex] then
              begin
                DoubleBytes := TDoubleBytes(RealData[
                  LayerIndex, RowIndex, ColIndex]);
                for ByteIndex := 0 to DoubleLimit do
                begin
                  FHash := (FHash shl 4) + DoubleBytes[ByteIndex];
                  G := FHash and longint($F0000000);
                  if G <> 0 then
                  begin
                    FHash := FHash xor (G shr 24) xor G;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    rdtInteger:
      begin
        FHash := 0;
        for LayerIndex := 0 to LayerCount - 1 do
        begin
          for RowIndex := 0 to RowCount - 1 do
          begin
            for ColIndex := 0 to ColumnCount - 1 do
            begin
              if IsValue[LayerIndex, RowIndex, ColIndex] then
              begin
                IntBytes := TIntBytes(IntegerData[
                  LayerIndex, RowIndex, ColIndex]);
                for ByteIndex := 0 to IntLimit do
                begin
                  FHash := (FHash shl 4) + IntBytes[ByteIndex];
                  G := FHash and longint($F0000000);
                  if G <> 0 then
                  begin
                    FHash := FHash xor (G shr 24) xor G;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    rdtBoolean:
      begin
        FHash := 0;
        for LayerIndex := 0 to LayerCount - 1 do
        begin
          for RowIndex := 0 to RowCount - 1 do
          begin
            for ColIndex := 0 to ColumnCount - 1 do
            begin
              if IsValue[LayerIndex, RowIndex, ColIndex] then
              begin
                ABool := BooleanData[LayerIndex, RowIndex, ColIndex];
                FHash := (FHash shl 4) + Byte(ABool);
                G := FHash and longint($F0000000);
                if G <> 0 then
                begin
                  FHash := FHash xor (G shr 24) xor G;
                end;
              end;
            end;
          end;
        end;
      end
    else Assert(False)
  end;
end;

procedure TDataArray.UpdateMinMaxValues;
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  FirstValue: Boolean;
  MinimumBoolean: Boolean;
  MaximumBoolean: Boolean;
  StringValues: TStringList;
  MinimumInteger: Integer;
  MaximumInteger: Integer;
  MinimumReal: Double;
  MaximumReal: Double;
  ColLimit: Integer;
  RowLimit: Integer;
  LayerLimit: Integer;
  PhastModel: TPhastModel;
  Grid: TCustomGrid;
  LayerMin: Integer;
  RowMin: Integer;
  ColMin: Integer;
begin
  if not UpToDate then
  begin
    Exit;
  end;

  GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
    LayerLimit, RowLimit, ColLimit);
  case Datatype of
    rdtDouble:
      begin
        MinimumReal := 0;
        MaximumReal := 0;
        FirstValue := True;
        if LayerMin >= 0 then
        begin
          for LayerIndex := LayerMin to LayerLimit do
          begin
            for RowIndex := RowMin to RowLimit do
            begin
              for ColIndex := ColMin to ColLimit do
              begin
                if IsValue[LayerIndex, RowIndex, ColIndex] then
                begin
                  if FirstValue then
                  begin
                    MinimumReal := RealData[LayerIndex, RowIndex, ColIndex];
                    MaximumReal := MinimumReal;
                    FirstValue := False;
                  end
                  else
                  begin
                    if MinimumReal > RealData[LayerIndex, RowIndex, ColIndex] then
                    begin
                      MinimumReal := RealData[LayerIndex, RowIndex, ColIndex];
                    end;
                    if MaximumReal < RealData[LayerIndex, RowIndex, ColIndex] then
                    begin
                      MaximumReal := RealData[LayerIndex, RowIndex, ColIndex];
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
        if FirstValue then
        begin
          FMaxValue := StrUnassigned;
          FMinValue := StrUnassigned;
          FMinReal := 0.0;
          FMaxReal := 0.0;
        end
        else
        begin
          FMaxValue := FloatToStrF(MaximumReal, ffGeneral, 7, 0);
          FMinValue := FloatToStrF(MinimumReal, ffGeneral, 7, 0);
          FMinReal := MinimumReal;
          FMaxReal := MaximumReal;
        end;
      end;
    rdtInteger:
      begin
        MinimumInteger := 0;
        MaximumInteger := 0;
        FirstValue := True;
        if LayerMin >= 0 then
        begin
          for LayerIndex := LayerMin to LayerLimit do
          begin
            for RowIndex := RowMin to RowLimit do
            begin
              for ColIndex := ColMin to ColLimit do
              begin
                if IsValue[LayerIndex, RowIndex, ColIndex] then
                begin
                  if FirstValue then
                  begin
                    MinimumInteger := IntegerData[LayerIndex, RowIndex, ColIndex];
                    MaximumInteger := MinimumInteger;
                    FirstValue := False;
                  end
                  else
                  begin
                    if MinimumInteger > IntegerData[LayerIndex, RowIndex, ColIndex] then
                    begin
                      MinimumInteger := IntegerData[LayerIndex, RowIndex, ColIndex];
                    end;
                    if MaximumInteger < IntegerData[LayerIndex, RowIndex, ColIndex] then
                    begin
                      MaximumInteger := IntegerData[LayerIndex, RowIndex, ColIndex];
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
        if FirstValue then
        begin
          FMaxValue := StrUnassigned;
          FMinValue := StrUnassigned;
          FMinInteger := 0;
          FMaxInteger := 0;
        end
        else
        begin
          FMaxValue := IntToStr(MaximumInteger);
          FMinValue := IntToStr(MinimumInteger);
          FMinInteger := MinimumInteger;
          FMaxInteger := MaximumInteger;
        end;
      end;
    rdtBoolean:
      begin
        MinimumBoolean := False;
        MaximumBoolean := False;
        FirstValue := True;
        if LayerMin >= 0 then
        begin
          for LayerIndex := LayerMin to LayerLimit do
          begin
            for RowIndex := RowMin to RowLimit do
            begin
              for ColIndex := ColMin to ColLimit do
              begin
                if IsValue[LayerIndex, RowIndex, ColIndex] then
                begin
                  if FirstValue then
                  begin
                    MinimumBoolean := BooleanData[LayerIndex, RowIndex, ColIndex];
                    MaximumBoolean := MinimumBoolean;
                    FirstValue := False;
                  end
                  else
                  begin
                    if MinimumBoolean > BooleanData[LayerIndex, RowIndex, ColIndex] then
                    begin
                      MinimumBoolean := BooleanData[LayerIndex, RowIndex, ColIndex];
                    end;
                    if MaximumBoolean < BooleanData[LayerIndex, RowIndex, ColIndex] then
                    begin
                      MaximumBoolean := BooleanData[LayerIndex, RowIndex, ColIndex];
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
        if FirstValue then
        begin
          FMaxValue := StrUnassigned;
          FMinValue := StrUnassigned;
          FMinBoolean := False;
          FMaxBoolean := False;
        end
        else
        begin
          if MaximumBoolean then
          begin
            FMaxValue := 'True';
          end
          else
          begin
            FMaxValue := 'False';
          end;
          if MinimumBoolean then
          begin
            FMinValue := 'True';
          end
          else
          begin
            FMinValue := 'False';
          end;
          FMinBoolean := MinimumBoolean;
          FMaxBoolean := MaximumBoolean;
        end;
      end;
    rdtString:
      begin
        StringValues := TStringList.Create;
        try
          StringValues.Sorted := True;
          StringValues.Duplicates := dupIgnore;
          StringValues.CaseSensitive := True;
          if LayerMin >= 0 then
          begin
            for LayerIndex := LayerMin to LayerLimit do
            begin
              for RowIndex := RowMin to RowLimit do
              begin
                for ColIndex := ColMin to ColLimit do
                begin
                  if IsValue[LayerIndex, RowIndex, ColIndex] then
                  begin
                    StringValues.Add(StringData[LayerIndex, RowIndex, ColIndex]);
                    if StringValues.Count > 2 then
                    begin
                      StringValues.Delete(1);
                    end;
                  end;
                end;
              end;
            end;
          end;
          if StringValues.Count > 0 then
          begin
            FMaxValue := StringValues[StringValues.Count - 1];
            FMinValue := StringValues[0];
            FMinString := FMinValue;
            FMaxString := FMaxValue;
          end
          else
          begin
            FMaxValue := StrUnassigned;
            FMinValue := StrUnassigned;
            FMinString := '';
            FMaxString := '';
          end;
        finally
          StringValues.Free;
        end;
      end;
  else
    Assert(False);
  end;
  if (FPhastModel <> nil) then
  begin
    PhastModel := FPhastModel as TPhastModel;
    Grid := PhastModel.Grid;
    if Grid <> nil then
    begin
      if (frmGridColor <> nil) then
      begin
        if (Grid.TopDataSet = self)
          or (Grid.FrontDataSet = self)
          or (Grid.SideDataSet = self)
          or (Grid.ThreeDDataSet = self) then
        begin
          frmGridColor.SetMinMaxLabels;
        end;
      end;
      if frmContourData <> nil then
      begin
        if (Grid.TopContourDataSet = self)
          or (Grid.FrontContourDataSet = self)
          or (Grid.SideContourDataSet = self)
          or (Grid.ThreeDContourDataSet = self) then
        begin
          frmContourData.SetMinMaxLabels;
        end;
      end;
    end;
  end;
end;

procedure TDataArray.ReadData(DecompressionStream: TDecompressionStream);
var
  AnnotationIndex: Integer;
  StringValue: string;
  ValueLength: Integer;
  BooleanValue: Boolean;
  IntValue: Integer;
  RealValue: Double;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  AnnText: string;
  AnnSize: Integer;
  Index: Integer;
  Count: Integer;
  Annotations: TStringList;
  AnnotationIndexArray: array of Integer;
  LayerArray: array of Integer;
  RowArray: array of Integer;
  ColumnArray: array of Integer;
  RealValues: array of double;
  IntegerValues: array of integer;
  BoooleanValues: array of Boolean;
  StringValues: TStringList;
begin
  StringValues := nil;
  Annotations := TStringList.Create;
  try
    DecompressionStream.Read(Count, SizeOf(Count));
    Annotations.Capacity := Count;
    for Index := 0 to Count - 1 do
    begin
      DecompressionStream.Read(AnnSize, SizeOf(AnnSize));
      SetString(AnnText, nil, AnnSize);
      DecompressionStream.Read(Pointer(AnnText)^, AnnSize * SizeOf(Char));
      Annotations.Add(AnnText);
    end;
    DecompressionStream.Read(Count, SizeOf(Count));
    SetLength(AnnotationIndexArray, Count);
    SetLength(LayerArray, Count);
    SetLength(RowArray, Count);
    SetLength(ColumnArray, Count);
    case DataType of
      rdtDouble: SetLength(RealValues, Count);
      rdtInteger: SetLength(IntegerValues, Count);
      rdtBoolean: SetLength(BoooleanValues, Count);
      rdtString:
        begin
          StringValues := TStringList.Create;
          StringValues.Capacity := Count;
        end
      else Assert(False);
    end;

    if Count > 0 then
    begin

      DecompressionStream.Read(LayerArray[0], Count*SizeOf(integer));
      DecompressionStream.Read(RowArray[0], Count*SizeOf(integer));
      DecompressionStream.Read(ColumnArray[0], Count*SizeOf(integer));
      DecompressionStream.Read(AnnotationIndexArray[0], Count*SizeOf(integer));

      case DataType of
        rdtDouble: DecompressionStream.Read(RealValues[0], Count*SizeOf(double));
        rdtInteger: DecompressionStream.Read(IntegerValues[0], Count*SizeOf(integer));
        rdtBoolean: DecompressionStream.Read(BoooleanValues[0], Count*SizeOf(boolean));
        rdtString:
          begin
            for Index := 0 to Count - 1 do
            begin
              ValueLength := Length(StringValue);
              DecompressionStream.Read(ValueLength, SizeOf(ValueLength));
              SetString(StringValue, nil, ValueLength);
              DecompressionStream.Read(Pointer(StringValue)^, ValueLength * SizeOf(Char));
              StringValues.Add(StringValue);
            end;
          end;
        else Assert(False);
      end;

      for Index := 0 to Count - 1 do
      begin
        LayerIndex := LayerArray[Index];
        RowIndex := RowArray[Index];
        ColIndex := ColumnArray[Index];
        case DataType of
          rdtDouble:
            begin
              RealValue := RealValues[Index];
              RealData[LayerIndex, RowIndex, ColIndex] := RealValue;
            end;
          rdtInteger:
            begin
              IntValue := IntegerValues[Index];
              IntegerData[LayerIndex, RowIndex, ColIndex] := IntValue;
            end;
          rdtBoolean:
            begin
              BooleanValue := BoooleanValues[Index];
              BooleanData[LayerIndex, RowIndex, ColIndex] := BooleanValue;
            end;
          rdtString:
            begin
              StringValue := StringValues[Index];
              StringData[LayerIndex, RowIndex, ColIndex] := StringValue;
            end;
        else
          Assert(False);
        end;
        AnnotationIndex := AnnotationIndexArray[Index];
        Annotation[LayerIndex, RowIndex, ColIndex] := Annotations[AnnotationIndex];
      end;
    end;
  finally
    Annotations.Free;
    StringValues.Free;
  end;
end;

procedure TDataArray.RefreshFormula;
begin
  FFormula := Formula;
end;

function TDataArray.ValueOK(const Layer, Row, Col: Integer;
  LocalLimits: TColoringLimits): Boolean;
begin
  result := True;
  case Datatype of
    rdtDouble:
      begin
        if not LocalLimits.ValueOk(RealData[Layer, Row, Col]) then
        begin
          result := False;
        end;
      end;
    rdtInteger:
      begin
        if not LocalLimits.ValueOk(IntegerData[Layer, Row, Col]) then
        begin
          result := False;
        end;
      end;
    rdtBoolean:
      begin
      end;
    rdtString:
      begin
        if not LocalLimits.ValueOk(StringData[Layer, Row, Col]) then
        begin
          result := False;
        end;
      end;
  else
    Assert(False);
  end;
end;

procedure GlobalDataArrayRemoveSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TDataArray).RemoveSubscription(Sender, AName);
end;

procedure GlobalDataArrayRestoreSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TDataArray).RestoreSubscription(Sender, AName);
end;

procedure TDataArray.RemoveSubscription(Sender: TObject; const AName: string);
var
  Model: TPhastModel;
  ObservedItem: TObserver;
begin
  Model := FPhastModel as TPhastModel;
  ObservedItem := Model.GetObserverByName(AName);
  if ObservedItem <> nil then
  begin
    // DS may be nil when the model is being destroyed.
    ObservedItem.StopsTalkingTo(self);
    Invalidate;
  end;
end;

procedure TDataArray.CountValues(out LayerLimit, RowLimit, ColLimit,
  Count: Integer);
var
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  LayerMin, RowMin, ColMin: integer;
begin
  Count := 0;
  GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
    LayerLimit, RowLimit, ColLimit);
  if LayerLimit < 0 then
  begin
    Exit;
  end;
//  GetLimits(ColLimit, RowLimit, LayerLimit);
  for LayerIndex := LayerMin to LayerLimit do
  begin
    for RowIndex := RowMin to RowLimit do
    begin
      for ColIndex := ColMin to ColLimit do
      begin
        if IsValue[LayerIndex, RowIndex, ColIndex] then
        begin
          Inc(Count);
        end;
      end;
    end;
  end;
end;

procedure TDataArray.StoreData(Compressor: TCompressionStream);
var
  ValueLength: Integer;
  StringValue: string;
  AnnSize: Integer;
  Index: Integer;
  AnnCount: Integer;
  AnnotationIndex: Integer;
  LocalAnnotation: string;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  LayerLimit: Integer;
  RowLimit: Integer;
  ColLimit: Integer;
  LocalAnnotatations: TStringList;
  Count: Integer;
  LayerMin, RowMin, ColMin: integer;
  AnnotationIndexArray: array of Integer;
  LayerArray: array of Integer;
  RowArray: array of Integer;
  ColumnArray: array of Integer;
  RealValues: array of double;
  IntegerValues: array of integer;
  BoooleanValues: array of Boolean;
  StringValues: TStringList;
begin
  Count := 0;
  StringValues := nil;
  LocalAnnotatations := TStringList.Create;
  try
    CountValues(LayerLimit, RowLimit, ColLimit, Count);
    GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
      LayerLimit, RowLimit, ColLimit);
    if Count > 0 then
    begin
      SetLength(AnnotationIndexArray, Count);
      SetLength(LayerArray, Count);
      SetLength(RowArray, Count);
      SetLength(ColumnArray, Count);
      case DataType of
        rdtDouble: SetLength(RealValues, Count);
        rdtInteger: SetLength(IntegerValues, Count);
        rdtBoolean: SetLength(BoooleanValues, Count);
        rdtString:
          begin
            StringValues := TStringList.Create;
            StringValues.Capacity := Count;
          end;
        else Assert(False);
      end;
      Count := 0;
      for LayerIndex := LayerMin to LayerLimit do
      begin
        for RowIndex := RowMin to RowLimit do
        begin
          for ColIndex := ColMin to ColLimit do
          begin
            if IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              LocalAnnotation := Annotation[LayerIndex, RowIndex, ColIndex];
              if LocalAnnotatations.Count < 1000 then
              begin
                AnnotationIndex := LocalAnnotatations.IndexOf(LocalAnnotation);
              end
              else
              begin
                AnnotationIndex := -1;
              end;
              if AnnotationIndex < 0 then
              begin
                AnnotationIndex := LocalAnnotatations.Add(LocalAnnotation);
              end;
              AnnotationIndexArray[Count] := AnnotationIndex;
              LayerArray[Count] := LayerIndex;
              RowArray[Count] := RowIndex;
              ColumnArray[Count] := ColIndex;
              case DataType of
                rdtDouble: RealValues[Count] :=
                  RealData[LayerIndex, RowIndex, ColIndex];

                rdtInteger: IntegerValues[Count] :=
                  IntegerData[LayerIndex, RowIndex, ColIndex];

                rdtBoolean: BoooleanValues[Count] :=
                  BooleanData[LayerIndex, RowIndex, ColIndex];
                    
                rdtString: StringValues.Add(
                  StringData[LayerIndex, RowIndex, ColIndex]);
                Else Assert(False);
              end;
              Inc(Count);
            end;
          end;
        end;
      end;
    end;
    AnnCount := LocalAnnotatations.Count;
    Compressor.Write(AnnCount, SizeOf(AnnCount));
    for Index := 0 to LocalAnnotatations.Count - 1 do
    begin
      LocalAnnotation := LocalAnnotatations[Index];
      AnnSize := Length(LocalAnnotation);
      Compressor.Write(AnnSize, SizeOf(AnnSize));
      Compressor.WriteBuffer(Pointer(LocalAnnotation)^, Length(LocalAnnotation) * SizeOf(Char));
    end;
    Compressor.Write(Count, SizeOf(Count));
    if Count > 0 then
    begin
      Compressor.Write(LayerArray[0], Count*SizeOf(integer));
      Compressor.Write(RowArray[0], Count*SizeOf(integer));
      Compressor.Write(ColumnArray[0], Count*SizeOf(integer));
      Compressor.Write(AnnotationIndexArray[0], Count*SizeOf(integer));
      case DataType of
        rdtDouble: Compressor.Write(RealValues[0], Count*SizeOf(double));
        rdtInteger: Compressor.Write(IntegerValues[0], Count*SizeOf(integer));
        rdtBoolean: Compressor.Write(BoooleanValues[0], Count*SizeOf(Boolean));
        rdtString:
          begin
            for Index := 0 to StringValues.Count - 1 do
            begin
              StringValue := StringValues[Index];
              ValueLength := Length(StringValue);
              Compressor.Write(ValueLength, SizeOf(ValueLength));
              Compressor.WriteBuffer(Pointer(StringValue)^, Length(StringValue) * SizeOf(Char));
            end;
          end;
        else Assert(False);
      end;


    end;
  finally
    LocalAnnotatations.Free;
    StringValues.Free;
  end;
end;

procedure TDataArray.GetIntegerArray(var AnArray: T3DIntegerDataSet);
begin
  if FDataType <> rdtInteger then
  begin
    raise EInvalidDataType.Create('Invalid Data Type');
  end
  else
  begin
    if DimensionsChanged then
    begin
      SetDimensions(False);
    end;
    AnArray := FDataArray;
  end;
end;

procedure TDataArray.GetRealArray(var AnArray: T3DRealDataSet);
begin
  if FDataType <> rdtDouble then
  begin
    raise EInvalidDataType.Create('Invalid Data Type');
  end
  else
  begin
    if DimensionsChanged then
    begin
      SetDimensions(False);
    end;
    AnArray := FDataArray;
  end;
end;

procedure TDataArray.GetStringArray(var AnArray: T3DStringDataSet);
begin
  if FDataType <> rdtString then
  begin
    raise EInvalidDataType.Create('Invalid Data Type');
  end
  else
  begin
    if DimensionsChanged then
    begin
      SetDimensions(False);
    end;
    AnArray := FDataArray;
  end;
end;

procedure TDataArray.GetBoolArray(var AnArray: T3DBooleanDataSet);
begin
  if FDataType <> rdtBoolean then
  begin
    raise EInvalidDataType.Create('Invalid Data Type');
  end
  else
  begin
    if DimensionsChanged then
    begin
      SetDimensions(False);
    end;
    AnArray := FDataArray;
  end;
end;

function TDataArray.GetLayerCount: integer;
begin
  if (IsUniform = iuTrue) or FCleared then
  begin
    result := FCachedLayerCount;
  end
  else
  begin
    result := FLayerCount;
  end;
end;

procedure TDataArray.GetLimits(out ColLimit, RowLimit, LayerLimit: Integer);
begin
  case EvaluatedAt of
    eaBlocks:
      begin
        LayerLimit := LayerCount - 1;
        RowLimit := RowCount - 1;
        ColLimit := ColumnCount - 1;
      end;
    eaNodes:
      begin
        LayerLimit := LayerCount;
        RowLimit := RowCount;
        ColLimit := ColumnCount;
      end;
  else
    Assert(False);
  end;
  case Orientation of
    dsoTop: LayerLimit := 0;
    dsoFront: RowLimit := 0;
    dsoSide: ColLimit := 0;
    dso3D: ; // do nothing
    else Assert(False);
  end;
end;


function TDataArray.GetMaxValue: string;
begin
  if UpToDate then
  begin
    result := FMaxValue
  end
  else
  begin
    result := '?'
  end;
end;

procedure TDataArray.GetMinMaxStoredLimits(out LayerMin, RowMin, ColMin,
  LayerMax, RowMax, ColMax: integer);
begin
  GetLimits(ColMax, RowMax, LayerMax);
  ColMin := 0;
  RowMin := 0;
  LayerMin := 0;
end;

function TDataArray.GetMinValue: string;
begin
  if UpToDate then
  begin
    result := FMinValue
  end
  else
  begin
    result := '?'
  end;
end;

procedure TDataArray.ChangeAFormula(const NewFormula: string;
  var OldFormula: string; var UseListUpToDate: boolean;
  UseListFunction: TUseListFunction);
var
  ObservedItem: TObserver;
  OtherIndex: Integer;
  Index: Integer;
  NewUseList: TStringList;
  OldUseList: TStringList;
  Model: TPhastModel;
begin
  if OldFormula <> NewFormula then
  begin
    Model := FPhastModel as TPhastModel;
    frmGoPhast.InvalidateModel;
    OldUseList := TStringList.Create;
    NewUseList := TStringList.Create;
    try
      try
        OldUseList.Assign(UseListFunction);
      except on ERbwParserError do
        begin
          OldUseList.Clear;
        end;
      end;
      OldFormula := NewFormula;
      UseListUpToDate := False;
      NewUseList.Assign(UseListFunction);
      for Index := OldUseList.Count - 1 downto 0 do
      begin
        OtherIndex := NewUseList.IndexOf(OldUseList[Index]);
        if OtherIndex >= 0 then
        begin
          OldUseList.Delete(Index);
          NewUseList.Delete(OtherIndex);
        end;
      end;
      for Index := 0 to OldUseList.Count - 1 do
      begin
        ObservedItem := Model.GetObserverByName(OldUseList[Index]);
        if ObservedItem <> nil then
        begin
          // DS may be nil when the model is being destroyed.
          ObservedItem.StopsTalkingTo(self);
        end;
      end;
      for Index := 0 to NewUseList.Count - 1 do
      begin
        ObservedItem := Model.GetObserverByName(NewUseList[Index]);
        Assert(ObservedItem <> nil);
        ObservedItem.TalksTo(self);
      end;
      Invalidate;
    finally
      OldUseList.Free;
      NewUseList.Free;
    end;
  end;
end;

procedure TDataArray.Clear;
begin
  SetDimensions(True);
end;

procedure TDataArray.SetIsValue(const Layer, Row, Col: Integer;
  const Value: boolean);
begin
  // do nothing
end;

procedure TDataArray.SetLimits(const Value: TColoringLimits);
begin
  FLimits.Assign(Value);
end;

function TDataArray.ContourGridValueOK(const Layer, Row, Col: integer): boolean;
begin
  Result := ValueOK(Layer, Row, Col, ContourLimits);
end;

procedure TDataArray.ContourLimitsChanged(Sender: TObject);
begin

end;

procedure TDataArray.LimitsChanged(Sender: TObject);
begin
  if (frmGoPhast <> nil) and (frmGoPhast.PhastModel <> nil)
    and (frmGoPhast.PhastModel.Grid <> nil) then
  begin
    if self = frmGoPhast.PhastModel.Grid.TopDataSet then
    begin
      frmGoPhast.PhastModel.Grid.NeedToRecalculateTopCellColors := True;
    end;
    if self = frmGoPhast.PhastModel.Grid.FrontDataSet then
    begin
      frmGoPhast.PhastModel.Grid.NeedToRecalculateFrontCellColors := True;
    end;
    if self = frmGoPhast.PhastModel.Grid.SideDataSet then
    begin
      frmGoPhast.PhastModel.Grid.NeedToRecalculateSideCellColors := True;
    end;
    if self = frmGoPhast.PhastModel.Grid.ThreeDDataSet then
    begin
      frmGoPhast.PhastModel.Grid.NeedToRecalculate3DCellColors := True;
      frmGoPhast.PhastModel.Grid.GridChanged;
    end;
  end;
end;

procedure TDataArray.PostInitialize;
begin
  if Assigned(OnPostInitialize) then
  begin
    OnPostInitialize(self);
  end;
end;

{ TCustom2DInterpolater }

procedure TCustom2DInterpolater.Assign(Source: TPersistent);
begin
  if not (Source is TCustom2DInterpolater) then
  begin
    inherited Assign(Source);
  end;
end;

function TCustom2DInterpolater.BooleanResult(const Location: TPoint2D):
  boolean;
begin
  result := False;
  if not (rdtBoolean in ValidReturnTypes) then
  begin
    raise EInterpolationException.Create(
      'Error: This interpolator can not return a boolean result.');
  end;
end;

constructor TCustom2DInterpolater.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner <> nil then
  begin
    SetSubComponent(True);
  end;
  if not (AOwner is TDataArray) then
  begin
    Exit;
  end;
  FDataSet := AOwner as TDataArray;
  if not (FDataSet.DataType in ValidReturnTypes) then
  begin
    raise EInterpolationException.Create(
      'Error: This interpolator can not return '
      + DataTypeToString(DataSet.DataType)
      + ' result.');
  end;
  if not (FDataSet.Orientation in ValidOrientations) then
  begin
    raise EInterpolationException.Create(
      'Error: This interpolator can not be used with '
      + DataSet.Name + ' because its orientation is wrong.');
  end;
end;


procedure TCustom2DInterpolater.Edit;
begin
  if Assigned(FOnEdit) then
  begin
    FOnEdit(self);
  end
end;

procedure TCustom2DInterpolater.FillScreenObjectList(
  const ListOfScreenObjects: TList);
var
  Index: integer;
  AScreenObject: TScreenObject;
  DataSetIndex: integer;
begin
  ListOfScreenObjects.Clear;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if AScreenObject.Deleted
      or not AScreenObject.SetValuesByInterpolation then
    begin
      continue;
    end;

    case AScreenObject.ViewDirection of
      vdTop:
        begin
          if DataSet.Orientation <> dsoTop then
          begin
            Continue;
          end;
        end;
      vdFront:
        begin
          if DataSet.Orientation <> dsoFront then
          begin
            Continue;
          end;
        end;
      vdSide:
        begin
          if DataSet.Orientation <> dsoSide then
          begin
            Continue;
          end;
        end;
    else Assert(False);
    end;

    DataSetIndex := AScreenObject.IndexOfDataSet(DataSet);
    if DataSetIndex < 0 then
    begin
      continue;
    end;
    
    ListOfScreenObjects.Add(AScreenObject);
  end;
end;

procedure TCustom2DInterpolater.Finalize(const DataSet: TDataArray);
begin
  if Assigned(FOnFinalize) then
  begin
    FOnFinalize(self, DataSet);
  end;
end;

procedure TCustom2DInterpolater.Initialize(const DataSet: TDataArray);
begin
  if Assigned(FOnInitialize) then
  begin
    FOnInitialize(self, DataSet);
  end;
end;

function TCustom2DInterpolater.IntegerResult(const Location: TPoint2D):
  integer;
begin
  result := 0;
  if not (rdtInteger in ValidReturnTypes) then
  begin
    raise EInterpolationException.Create(
      'Error: This interpolator can not return an integer result.');
  end;
end;

function TCustom2DInterpolater.RealResult(const Location: TPoint2D): real;
begin
  result := 0;
  if not (rdtDouble in ValidReturnTypes) then
  begin
    raise EInterpolationException.Create(
      'Error: This interpolator can not return a real-number result.');
  end;
end;

function TCustom2DInterpolater.SameAs(
  AnotherInterpolator: TCustom2DInterpolater): boolean;
begin
  result := (AnotherInterpolator <> nil);
  if result then
  begin
    result := ClassType = AnotherInterpolator.ClassType;
    if result then
    begin
      result := (ValidReturnTypes = AnotherInterpolator.ValidReturnTypes)
        and (ValidOrientations = AnotherInterpolator.ValidOrientations);
    end;
  end;
end;

function TCustom2DInterpolater.ShouldInterpolate: boolean;
var
  Index: integer;
  AScreenObject: TScreenObject;
  DataSetIndex: integer;
begin
  result := False;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if AScreenObject.Deleted or not AScreenObject.SetValuesByInterpolation
      then
    begin
      continue;
    end;
    DataSetIndex := AScreenObject.IndexOfDataSet(FDataSet);
    if DataSetIndex < 0 then
    begin
      continue;
    end;
    result := True;
    break;
  end;
end;

function TCustom2DInterpolater.StringResult(const Location: TPoint2D):
  string;
begin
  result := '';
  if not (rdtString in ValidReturnTypes) then
  begin
    raise EInterpolationException.Create(
      'Error: This interpolator can not return a string result.');
  end;
end;

class function TCustom2DInterpolater.ValidOrientations: TDataSetOrientations;
begin
  if frmGoPhast.ModelSelection = msUndefined then
  begin
    result := [dsoTop, dsoFront, dsoSide, dso3D];
  end
  else if frmGoPhast.ModelSelection = msPhast then
  begin
    result := [dsoTop, dsoFront, dsoSide];
  end
  else
  begin
    result := [dsoTop];
  end;
end;

{ TCustomSparseDataSet }

procedure TCustomSparseDataSet.Clear;
begin
  // don't call inherited Clear.
  FAnnotation.Clear;
end;

constructor TCustomSparseDataSet.Create(AnOwner: TComponent);
begin
  inherited;
  FPriorLayer := -1;
  FPriorRow := -1;
  FPriorCol := -1;
  FAnnotation := T3DSparseStringArray.Create(SPASmall);
  // Sparase Array data sets are only used for boundary conditions in
  // PHAST and the boundary conditions all apply to nodes.
  EvaluatedAt := eaNodes;
end;

destructor TCustomSparseDataSet.Destroy;
begin
  FreeAndNil(FAnnotation);
  inherited;
end;

function TCustomSparseDataSet.GetAnnotation(const Layer, Row,
  Col: integer): string;
begin
  result := FAnnotation[Layer, Row, Col];
  UpdateEvalTime;
end;

procedure TCustomSparseDataSet.Initialize;
var
  ScreenObjectIndex: integer;
  AScreenObject: TScreenObject;
  FreeStack: boolean;
begin
  // Values are assigned only using screen objects. Neither iterpolation nor
  // default expressions are used.

  if UpToDate and not DimensionsChanged then
  begin
    if FDataCached and FCleared then
    begin
      RestoreData;
    end;
    Exit;
  end;
  FDataCached := False;
  FEvalTime := Now;

  FreeStack := (Stack = nil);
  try
    if FreeStack then
    begin
      Stack := TStringList.Create;
    end;
    if Stack.IndexOf(Name) >= 0 then
    begin
      UpToDate := True;
      raise ECircularReference.Create('Circular reference in ' + Name);
    end;
    Stack.Add(Name);

    GlobalEvaluatedAt := EvaluatedAt;

    SetDimensions(False);

    for ScreenObjectIndex := 0 to
      frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
      if not AScreenObject.Deleted then
      begin
        AScreenObject.AssignValuesToPhastDataSet(frmGoPhast.Grid, self);
      end;
    end;

  finally
    if FreeStack then
    begin
      FreeAndNil(Stack);
      frmGoPhast.PhastModel.CacheDataArrays;
    end;
  end;

  PostInitialize;

  UpToDate := True;
end;

procedure TCustomSparseDataSet.Invalidate;
begin
  inherited;
  FPriorLayer := -1;
  FPriorRow := -1;
  FPriorCol := -1;
end;

procedure TCustomSparseDataSet.SetAnnotation(const Layer, Row,
  Col: integer; const Value: string);
begin
  FAnnotation[Layer, Row, Col] := Value;
  FPriorLayer := -1;
  FPriorRow := -1;
  FPriorCol := -1;
end;

procedure TCustomSparseDataSet.SetDimensions(const SetToZero: boolean);
begin
  inherited;
  if FAnnotation <> nil then
  begin
    FAnnotation.Clear;
  end;
end;

{ TRealSparseDataSet }
procedure TRealSparseDataSet.Clear;
begin
  inherited;
  FRealValues.Clear;
end;

constructor TRealSparseDataSet.Create(AnOwner: TComponent);
begin
  inherited;
  FRealValues := T3DSparseRealArray.Create(SPASmall);
  DataType := rdtDouble;
end;

destructor TRealSparseDataSet.Destroy;
begin
  FreeAndNil(FRealValues);
  inherited;
end;

function TRealSparseDataSet.GetIsValue(
  const Layer, Row, Col: Integer): boolean;
begin
  result := inherited GetIsValue(Layer, Row, Col);
  if result then
  begin
    result := FRealValues.IsValue[Layer, Row, Col];
  end;
end;

procedure TRealSparseDataSet.GetMinMaxStoredLimits(out LayerMin, RowMin, ColMin,
  LayerMax, RowMax, ColMax: integer);
begin
  CheckRestoreData;
  LayerMin := FRealValues.MinLayer;
  RowMin := FRealValues.MinRow;
  ColMin := FRealValues.MinCol;
  LayerMax := FRealValues.MaxLayer;
  RowMax := FRealValues.MaxRow;
  ColMax := FRealValues.MaxCol;
end;

function TRealSparseDataSet.GetRealData(const Layer, Row,
  Col: integer): double;
begin
  Assert(IsValue[Layer, Row, Col]);
  result := FRealValues[Layer, Row, Col];
end;

procedure TRealSparseDataSet.SetDataType(const Value: TRbwDataType);
begin
  Assert(Value = rdtDouble);
  inherited;
end;

procedure TRealSparseDataSet.SetDimensions(const SetToZero: boolean);
begin
  inherited;
  if FRealValues <> nil then
  begin
    FRealValues.Clear;
  end;
end;

procedure TRealSparseDataSet.SetRealData(const Layer, Row, Col: integer;
  const Value: double);
begin
  // don't call inherited;
  if CheckMin and (Value < Min) then
  begin
    FRealValues[Layer, Row, Col] := Min;
  end
  else if CheckMax and (Value > Max) then
  begin
    FRealValues[Layer, Row, Col] := Max;
  end
  else
  begin
    FRealValues[Layer, Row, Col] := Value;
  end;
end;

{ TIntegerSparseDataSet }

procedure TIntegerSparseDataSet.Clear;
begin
  inherited;
  FIntegerValues.Clear;
end;

constructor TIntegerSparseDataSet.Create(AnOwner: TComponent);
begin
  inherited;
  FIntegerValues := T3DSparseIntegerArray.Create(SPASmall);
  DataType := rdtInteger;
end;

destructor TIntegerSparseDataSet.Destroy;
begin
  FreeAndNil(FIntegerValues);
  inherited;
end;

function TIntegerSparseDataSet.GetIsValue(
  const Layer, Row, Col: Integer): boolean;
begin
  result := inherited GetIsValue(Layer, Row, Col)
    and FIntegerValues.IsValue[Layer, Row, Col];
end;

procedure TIntegerSparseDataSet.GetMinMaxStoredLimits(out LayerMin, RowMin,
  ColMin, LayerMax, RowMax, ColMax: integer);
begin
  CheckRestoreData;
  LayerMin := FIntegerValues.MinLayer;
  RowMin := FIntegerValues.MinRow;
  ColMin := FIntegerValues.MinCol;
  LayerMax := FIntegerValues.MaxLayer;
  RowMax := FIntegerValues.MaxRow;
  ColMax := FIntegerValues.MaxCol;
end;

function TIntegerSparseDataSet.GetIntegerData(const Layer, Row,
  Col: integer): integer;
begin
  Assert(IsValue[Layer, Row, Col]);
  result := FIntegerValues[Layer, Row, Col];
end;

procedure TIntegerSparseDataSet.SetDataType(const Value: TRbwDataType);
begin
  Assert(Value = rdtInteger);
  inherited;
end;

procedure TIntegerSparseDataSet.SetDimensions(const SetToZero: boolean);
begin
  inherited;
  if FIntegerValues <> nil then
  begin
    FIntegerValues.Clear;
  end;
end;

procedure TIntegerSparseDataSet.SetIntegerData(const Layer, Row, Col: integer;
  const Value: integer);
var
  AValue: integer;
begin
  // don't call inherited;
  AValue := Value;
  if CheckMin and (Value < Min) then
  begin
    AValue := Trunc(Min);
    if AValue < Min then
    begin
      Inc(AValue);
    end
  end
  else if CheckMax and (Value > Max) then
  begin
    AValue := Trunc(Max);
    if AValue > Max then
    begin
      Dec(AValue);
    end
  end;
  FIntegerValues[Layer, Row, Col] := AValue;
end;

procedure TIntegerSparseDataSet.SetIsValue(const Layer, Row, Col: Integer;
  const Value: boolean);
begin
  if (BoundaryTypeDataSet = self) then
  begin
    FIntegerValues.IsValue[Layer, Row, Col] := Value;
  end
  else
  begin
    inherited SetIsValue(Layer, Row, Col, Value)
  end;
end;

{ TColoringLimit }

procedure TColoringLimit.Assign(Source: TPersistent);
var
  Value: TColoringLimit;
begin
  if Source is TColoringLimit then
  begin
    Value := TColoringLimit(Source);
    UseLimit := Value.UseLimit;
    if UseLimit then
    begin
      DataType := Value.DataType;
      case DataType of
        rdtDouble:
          begin
            RealLimitValue := Value.RealLimitValue;
          end;
        rdtInteger:
          begin
            IntegerLimitValue := Value.IntegerLimitValue;
          end;
        rdtBoolean:
          begin
            BooleanLimitValue := Value.BooleanLimitValue;
          end;
        rdtString:
          begin
            StringLimitValue := Value.StringLimitValue;
          end;
      else
        Assert(False);
      end;
    end
    else
    begin
      BooleanLimitValue := Value.DefaultBooleanLimitValue;
    end;
  end
  else
  begin
    inherited;
  end;
end;

constructor TColoringLimit.Create;
begin
  UseLimit := False;
  FBooleanLimitValue := False;
  FIntegerLimitValue := 0;
  FRealLimitValue := 0;
  FStringLimitValue := '';
end;

procedure TColoringLimit.SetBooleanLimitValue(const Value: boolean);
begin
  if FBooleanLimitValue <> Value then
  begin
    FBooleanLimitValue := Value;
    if Assigned(OnChange) then
      OnChange(self);
  end;
end;

procedure TColoringLimit.SetDataType(const Value: TRbwDataType);
begin
  FDataType := Value;
end;

procedure TColoringLimit.SetIntegerLimitValue(const Value: integer);
begin
  if FIntegerLimitValue <> Value then
  begin
    FIntegerLimitValue := Value;
    if Assigned(OnChange) then
      OnChange(self);
  end;
end;

procedure TColoringLimit.SetRealLimitValue(const Value: double);
begin
  if FRealLimitValue <> Value then
  begin
    FRealLimitValue := Value;
    if Assigned(OnChange) then
      OnChange(self);
  end;
end;

procedure TColoringLimit.SetStringLimitValue(const Value: string);
begin
  if FStringLimitValue <> Value then
  begin
    FStringLimitValue := Value;
    if Assigned(OnChange) then
      OnChange(self);
  end;
end;

procedure TColoringLimit.SetUseLimit(const Value: boolean);
begin
  if FUseLimit <> Value then
  begin
    FUseLimit := Value;
    if Assigned(OnChange) then
      OnChange(self);
  end;
end;
{ TColoringLimits }

procedure TColoringLimits.Assign(Source: TPersistent);
var
  Value: TColoringLimits;
begin
  if Source is TColoringLimits then
  begin
    Value := TColoringLimits(Source);
    LowerLimit := Value.LowerLimit;
    UpperLimit := Value.UpperLimit;
    ActiveOnly := Value.ActiveOnly;
    RealValuesToSkip := Value.RealValuesToSkip;
    IntegerValuesToSkip := Value.IntegerValuesToSkip;
    StringValuesToSkip := Value.StringValuesToSkip;
    LogTransform := Value.LogTransform;
    Update;
  end
  else
  begin
    inherited;
  end;
end;

constructor TColoringLimits.Create;
begin
  FLowerLimit := TColoringLimit.Create;
  FUpperLimit := TColoringLimit.Create;
  FUpperLimit.DefaultBooleanLimitValue := True;
  FRealValuesToSkip := TSkipRealCollection.Create;
  FIntegerValuesToSkip := TSkipIntegerCollection.Create;
  FStringValuesToSkip := TStringList.Create;
  TStringList(FStringValuesToSkip).Sorted := true;
  TStringList(FStringValuesToSkip).Duplicates := dupIgnore;
end;

destructor TColoringLimits.Destroy;
begin
  FStringValuesToSkip.Free;
  FIntegerValuesToSkip.Free;
  FRealValuesToSkip.Free;
  FLowerLimit.Free;
  FUpperLimit.Free;
  inherited;
end;

procedure TColoringLimits.SetActiveOnly(const Value: boolean);
begin
  FActiveOnly := Value;
end;

procedure TColoringLimits.SetIntegerValuesToSkip(
  const Value: TSkipIntegerCollection);
begin
  FIntegerValuesToSkip.Assign(Value);
end;

procedure TColoringLimits.SetLogTransform(const Value: boolean);
begin
  FLogTransform := Value;
end;

procedure TColoringLimits.SetLowerLimit(const Value: TColoringLimit);
begin
  FLowerLimit.Assign(Value);
end;

procedure TColoringLimits.SetRealValuesToSkip(const Value: TSkipRealCollection);
begin
  FRealValuesToSkip.Assign(Value);
end;

procedure TColoringLimits.SetStringValuesToSkip(const Value: TStrings);
begin
  FStringValuesToSkip.Assign(Value);
end;

procedure TColoringLimits.SetUpperLimit(const Value: TColoringLimit);
begin
  FUpperLimit.Assign(Value);
end;

function TColoringLimits.StoreIntegerSkipValues: boolean;
begin
  result := FIntegerValuesToSkip.Count > 0
end;

function TColoringLimits.StoreRealSkipValues: boolean;
begin
  result := FRealValuesToSkip.Count > 0
end;

procedure TColoringLimits.Update;
var
  TempReal: double;
  TempInteger: integer;
  TempBoolean: boolean;
  TempString: string;
begin
  if LowerLimit.UseLimit and UpperLimit.UseLimit then
  begin
    assert(LowerLimit.DataType = UpperLimit.DataType);
    case LowerLimit.DataType of
      rdtDouble:
        begin
          if LowerLimit.RealLimitValue > UpperLimit.RealLimitValue then
          begin
            TempReal := UpperLimit.RealLimitValue;
            UpperLimit.RealLimitValue := LowerLimit.RealLimitValue;
            LowerLimit.RealLimitValue := TempReal;
          end;
        end;
      rdtInteger:
        begin
          if LowerLimit.IntegerLimitValue > UpperLimit.IntegerLimitValue then
          begin
            TempInteger := UpperLimit.IntegerLimitValue;
            UpperLimit.IntegerLimitValue := LowerLimit.IntegerLimitValue;
            LowerLimit.IntegerLimitValue := TempInteger;
          end;
        end;
      rdtBoolean:
        begin
          if LowerLimit.BooleanLimitValue > UpperLimit.BooleanLimitValue then
          begin
            TempBoolean := UpperLimit.BooleanLimitValue;
            UpperLimit.BooleanLimitValue := LowerLimit.BooleanLimitValue;
            LowerLimit.BooleanLimitValue := TempBoolean;
          end;
        end;
      rdtString:
        begin
          if LowerLimit.StringLimitValue > UpperLimit.StringLimitValue then
          begin
            TempString := UpperLimit.StringLimitValue;
            UpperLimit.StringLimitValue := LowerLimit.StringLimitValue;
            LowerLimit.StringLimitValue := TempString;
          end;
        end;
    else
      Assert(False);
    end;
  end;
end;

function TColoringLimits.ValueOk(AValue: double): boolean;
var
  SkipIndex: Integer;
  SkipItem: TSkipReal;
  Epsilon: double;
begin
  result := True;
  if LogTransform and (AValue <= 0) then
  begin
    result := False;
    Exit;
  end;
  for SkipIndex := 0 to RealValuesToSkip.Count - 1 do
  begin
    SkipItem := RealValuesToSkip.Items[SkipIndex] as TSkipReal;
    if SkipItem.RealValue = AValue then
    begin
      result := False;
      Exit;
    end;
    if SkipItem.RealValue = 0 then
    begin
      Epsilon := 1E-6;
    end
    else
    begin
      Epsilon := Abs(SkipItem.RealValue)*1E-6;
    end;
    if ((SkipItem.RealValue - Epsilon) < AValue)
      and ((SkipItem.RealValue + Epsilon) > AValue) then
    begin
      result := False;
      Exit;
    end;
  end;
end;

function TColoringLimits.ValueOk(AValue: integer): boolean;
var
  SkipIndex: Integer;
  SkipItem: TSkipInteger;
begin
  result := True;
  for SkipIndex := 0 to IntegerValuesToSkip.Count - 1 do
  begin
    SkipItem := IntegerValuesToSkip.Items[SkipIndex] as TSkipInteger;
    if SkipItem.IntegerValue = AValue then
    begin
      result := False;
      Exit;
    end;
  end;
end;

function TColoringLimits.ValueOk(const AValue: String): boolean;
begin
  result := StringValuesToSkip.IndexOf(AValue) < 0;
end;

{ TCustomTimeList }

function TCustomTimeList.GetUpToDate: boolean;
var
  Index: Integer;
begin
  result := FUpToDate;
  if result then
  begin
    for Index := 0 to Count - 1 do
    begin
      result := Items[Index].UpToDate;
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TCustomTimeList.SetUpToDate(const Value: boolean);
begin
  FUpToDate := Value;
  if not FUpToDate then
  begin
    if frmGoPhast.PhastModel <> nil then
    begin

      if frmGoPhast.PhastModel.TopTimeList = self then
      begin
        frmGoPhast.PhastGrid.NeedToRecalculateTopCellColors := True;
        frmGoPhast.ModflowGrid.NeedToRecalculateTopCellColors := True;
      end;
      if frmGoPhast.PhastModel.FrontTimeList = self then
      begin
        frmGoPhast.PhastGrid.NeedToRecalculateFrontCellColors := True;
        frmGoPhast.ModflowGrid.NeedToRecalculateFrontCellColors := True;
      end;
      if frmGoPhast.PhastModel.SideTimeList = self then
      begin
        frmGoPhast.PhastGrid.NeedToRecalculateSideCellColors := True;
        frmGoPhast.ModflowGrid.NeedToRecalculateSideCellColors := True;
      end;
    end;
  end;
end;

procedure TCustomTimeList.Invalidate;
var
  Model: TPhastModel;
begin
  SetUpToDate(False);
  if FModel <> nil then
  begin
    Model := FModel as TPhastModel;
    if self = Model.ThreeDTimeList then
    begin
      if Model.Grid <> nil then
      begin
        Model.Grid.GridChanged;
      end;
    end;
  end;
end;

function TCustomTimeList.MaxValue(Time: double): string;
var
  TimeIndex: Integer;
  DataSet: TDataArray;
begin
  result := '?';
  if UpToDate then
  begin
    TimeIndex := FirstTimeGreaterThan(Time) - 1;
    if TimeIndex >= 0 then
    begin
      DataSet := Items[TimeIndex];
      result := DataSet.MaxValue;
    end;
  end;
end;

function TCustomTimeList.MinValue(Time: double): string;
var
  TimeIndex: Integer;
  DataSet: TDataArray;
begin
  result := '?';
  if UpToDate then
  begin
    TimeIndex := FirstTimeGreaterThan(Time) - 1;
    if TimeIndex >= 0 then
    begin
      DataSet := Items[TimeIndex];
      result := DataSet.MinValue;
    end;
  end;
end;

function TCustomTimeList.Add(const ATime: double;
  const Data: TDataArray): integer;
begin
  result := IndexOf(ATime);
  if result >= 0 then
  begin
    Assert(FData[result] = Data);
    Exit;
  end;

  result := FTimes.Add(ATime);
  if result >= FData.Count then
  begin
    FData.Add(Data);
  end
  else
  begin
    FData.Insert(result, Data);
  end;

  Data.Limits := Limits;
  Data.Max := Max;
  Data.Min := Min;
  Data.CheckMax := CheckMax;
  Data.CheckMin := CheckMin;

  Invalidate;
end;

function TCustomTimeList.GetTimes(const Index: integer): double;
begin
  result := FTimes[Index];
end;

function TCustomTimeList.IndexOf(const ATime: double): integer;
begin
  result := FTimes.IndexOf(ATime);
end;

procedure TCustomTimeList.SetName(const Value: string);
begin
  FName := Value;
  Invalidate;
end;

function TCustomTimeList.GetClassification: string;
begin
  result := FClassification;
  if result = '' then
  begin
    if (Name = StrSpecifiedHead)
      or (Name = StrSpecifiedHeadSolution)
      then
    begin
      result := 'PHAST Specified Head';
    end
    else if (Name = StrTopFluxBoundaryFlux)
      or (Name = StrFrontFluxBoundaryFlux)
      or (Name = StrSideFluxBoundaryFlux)
      or (Name = StrTopFluxBoundaryAssocSoln)
      or (Name = StrFrontFluxBoundaryAssocSoln)
      or (Name = StrSideFluxBoundaryAssocSoln)
      then
    begin
      Result := 'PHAST Flux';
    end
    else if (Name = StrTopLeakyBoundaryHead)
      or (Name = StrTopLeakyBoundaryAssocSoln)
      or (Name = StrFrontLeakyBoundaryHead)
      or (Name = StrFrontLeakyBoundaryAssocSoln)
      or (Name = StrSideLeakyBoundaryHead)
      or (Name = StrSideLeakyBoundaryAssocSoln)
      then
    begin
      Result := 'PHAST Leaky';
    end
    else if (Name = StrRiverHead)
      or (Name = StrRiverAssocSoln)
      then
    begin
      Result := 'PHAST River';
    end
    else if (Name = StrWellInjectionRate)
      or (Name = StrWellSolution)
      then
    begin
      Result := 'PHAST Well';
    end
    else if (Name = StrMODFLOWWellPumping)
      then
    begin
      Result := 'MODFLOW Well';
    end
    else if (Name = StrMODFLOWGhbConductance)
      or (Name = StrMODFLOWGhbHead)
      then
    begin
      Result := 'MODFLOW General Head Boundary';
    end
    else if (Name = StrMODFLOWDrainElevation)
      or (Name = StrMODFLOWDrainConductance)
      then
    begin
      Result := 'MODFLOW Drain';
    end
    else if (Name = StrMODFLOWDrainReturnConductance)
      or (Name = StrMODFLOWDrainReturnElevation)
      or (Name = StrMODFLOWDrainReturnFraction)
      then
    begin
      Result := 'MODFLOW Drain Return';
    end
    else if (Name = StrMODFLOWRiverConductance)
      or (Name = StrMODFLOWRiverStage)
      or (Name = StrMODFLOWRiverBottom)
      then
    begin
      Result := 'MODFLOW River';
    end
    else if (Name = StrMODFLOWCHDStartingHead)
      or (Name = StrMODFLOWCHDEndingHead)
      then
    begin
      Result := 'MODFLOW CHD';
    end
    else if (Pos(StrMODFLOWEtsRateFraction, Name) = 1)
      or (Pos(StrMODFLOWEtsDepthFraction, Name) = 1)
      or (Name = StrMODFLOWEtsRate)
      or (Name = StrMODFLOWEtsDepth)
      or (Name = StrMODFLOWEtsSurface)
      or (Name = StrMODFLOWEtsLayer)
      then
    begin
      Result := 'MODFLOW Evapotranspiration Segments';
    end
    else if (Name = StrMODFLOWEvtRate)
      or (Name = StrMODFLOWEvtDepth)
      or (Name = StrMODFLOWEvtSurface)
      or (Name = StrMODFLOWEvtLayer)
      then
    begin
      Result := 'MODFLOW Evapotranspiration';
    end
    else if (Name = StrMODFLOWRchRate)
      or (Name = StrMODFLOWRchLayer)
      then
    begin
      Result := 'MODFLOW Recharge';
    end
    else if (Name = StrModflowSfrSegment)
      or (Name = StrModflowSfrReach)
      or (Name = StrModflowSfrIcalc)
      or (Name = StrModflowSfrReachLength)
      or (Name = StrModflowSfrStreamTop)
      or (Name = StrModflowSfrStreamSlope)
      or (Name = StrModflowSfrStreamThickness)
      or (Name = StrModflowSfrStreamK)
      or (Name = StrModflowSfrSatWatCont)
      or (Name = StrModflowSfrInitWatCont)
      or (Name = StrModflowSfrBrooksCorey)
      or (Name = StrModflowSfrVertK)
      or (Name = StrModflowSfrDownstreamSegment)
      or (Name = StrModflowSfrDiversionSegment)
      or (Name = StrModflowSfrIprior)
      or (Name = StrModflowSfrFlow)
      or (Name = StrModflowSfrRunoff)
      or (Name = StrModflowSfrPrecipitation)
      or (Name = StrModflowSfrEvapotranspiration)
      or (Name = StrModflowSfrChannelRoughness)
      or (Name = StrModflowSfrBankRoughness)
      or (Name = StrModflowSfrDepthCoefficient)
      or (Name = StrModflowSfrDepthExponent)
      or (Name = StrModflowSfrWidthCoefficient)
      or (Name = StrModflowSfrWidthExponent)
      or (Name = StrModflowSfrUpstreamHydraulicConductivity)
      or (Name = StrModflowSfrDownstreamHydraulicConductivity)
      or (Name = StrModflowSfrUpstreamWidth)
      or (Name = StrModflowSfrDownstreamWidth)
      or (Name = StrModflowSfrUpstreamThickness)
      or (Name = StrModflowSfrDownstreamThickness)
      or (Name = StrModflowSfrUpstreamElevation)
      or (Name = StrModflowSfrDownstreamElevation)
      or (Name = StrModflowSfrUpstreamDepth)
      or (Name = StrModflowSfrDownstreamDepth)
      or (Name = StrModflowSfrUpstreamSaturatedWaterContent)
      or (Name = StrModflowSfrDownstreamSaturatedWaterContent)
      or (Name = StrModflowSfrUpstreamInitialUnsaturatedWaterContent)
      or (Name = StrModflowSfrDownstreamInitialUnsaturatedWaterContent)
      or (Name = StrModflowSfrUpstreamBrooksCoreyExponent)
      or (Name = StrModflowSfrDownstreamBrooksCoreyExponent)
      or (Name = StrModflowSfrUpstreamMaxUnsaturatedKz)
      or (Name = StrModflowSfrDownstreamMaxUnsaturatedKz)
      then
    begin
      Result := 'MODFLOW Streamflow Routing';
    end
    else if (Name = StrUzfInfiltrationRate)
      or (Name = StrUzfExtinctionDepth)
      or (Name = StrUzfWaterContent)
      or (Name = StrUzfEtDemand)
      then
    begin
      Result := 'MODFLOW Unsaturated Zone Flow';
    end
    else if (Name = StrMODFLOWHeadObservations)
      then
    begin
      Result := 'MODFLOW Observations';
    end
    else if (Name = StrWellRadius)
      or (Name = StrSkinRadius)
      or (Name = StrSkinK)
      or (Name = StrB)
      or (Name = StrC)
      or (Name = StrP)
      or (Name = StrCellToWellConductance)
      or (Name = StrPartialPenetration)
      then
    begin
      Result := 'MODFLOW Multinode Well';
    end
    else
    begin
      result := 'Undefined';
    end;
    if result <> 'Undefined' then
    begin
      FClassification := result;
    end;
  end;
end;

function TCustomTimeList.GetCount: integer;
begin
  result := FTimes.Count;
end;

function TCustomTimeList.FirstTimeGreaterThan(const ATime: double): integer;
var
  Top, Bottom, Middle: integer;
begin
  if Count = 0 then
  begin
    result := -1
  end
  else if Times[0] > ATime then
  begin
    result := 0;
  end
  else if Times[Count - 1] <= ATime then
  begin
    result := Count;
  end
  else
  begin
    Top := Count - 1;
    Bottom := 0;
    while Top - Bottom > 1 do
    begin
      Middle := (Top + Bottom) div 2;
      if Times[Middle] <= ATime then
      begin
        Bottom := Middle;
      end
      else
      begin
        Top := Middle;
      end;
    end; // While Top - Bottom > 1 do
    result := Top;
  end;
end;

procedure TCustomTimeList.FreeItem(Index: integer);
begin
  FData[Index] := nil;
end;

function TCustomTimeList.IndexOfDataSet(const Data: TDataArray): integer;
begin
  result := FData.IndexOf(Data);
end;

procedure TCustomTimeList.SetOrientation(const Value: TDataSetOrientation);
begin
  FOrientation := Value;
  Invalidate;
end;

procedure TCustomTimeList.Changed(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    Invalidate;
  end;
  frmGoPhast.ScreenObjectsChanged(Sender);
end;

procedure TCustomTimeList.Clear;
begin
  FTimes.Clear;
  FData.Clear;
  Invalidate;
end;

constructor TCustomTimeList.Create(Model: TObject);
begin
  inherited Create;
  Assert((Model = nil) or (Model is TPhastModel));
  FModel := Model;
  FLimits := TColoringLimits.Create;
  FTimes := TRealList.Create;
  FTimes.Sorted := True;
  FData := TObjectList.Create;
  Invalidate;
end;

destructor TCustomTimeList.Destroy;
var
  Model: TPhastModel;
begin
  if (FModel <> nil) and not (csDestroying in Application.ComponentState) then
  begin
    Model := FModel as TPhastModel;
    if Model.TopTimeList = self then
    begin
      Model.TopTimeList := nil;
    end;
    if Model.FrontTimeList = self then
    begin
      Model.FrontTimeList := nil;
    end;
    if Model.SideTimeList = self then
    begin
      Model.SideTimeList := nil;
    end;
    if Model.ThreeDTimeList = self then
    begin
      Model.ThreeDTimeList := nil;
    end;
  end;
  FLimits.Free;
  FTimes.Free;
  FData.Free;
  inherited;
end;

function TCustomTimeList.GetItems(const Index: integer): TDataArray;
begin
  result := FData[Index];
  result.ATimeList := self;
end;

procedure TCustomTimeList.SetCheckMax(const Value: boolean);
begin
  FCheckMax := Value;
end;

procedure TCustomTimeList.SetCheckMin(const Value: boolean);
begin
  FCheckMin := Value;
end;

procedure TCustomTimeList.SetClassification(const Value: string);
begin
  FClassification := Value;
end;

procedure TCustomTimeList.SetItems(const Index: integer;
  const Value: TDataArray);
begin
  if FData[Index] <> Value then
  begin
    FData[Index] := Value;
    Invalidate;
  end;
end;

procedure TCustomTimeList.SetLimits(const Value: TColoringLimits);
begin
  FLimits.Assign(Value);
  UpdateLimits;
end;

procedure TCustomTimeList.SetMax(const Value: double);
begin
  FMax := Value;
end;

procedure TCustomTimeList.SetMin(const Value: double);
begin
  FMin := Value;
end;

procedure TCustomTimeList.UpDateLimits;
var
  Index: integer;
  DataSet: TDataArray;
begin
  Limits.Update;
  for Index := 0 to Count - 1 do
  begin
    DataSet := self.Items[Index];
    DataSet.Limits := Limits;
  end;
end;

function TCustomTimeList.UsedByModel: boolean;
begin
  if Assigned(FOnTimeListUsed) then
  begin
    result := FOnTimeListUsed(self);
  end
  else
  begin
    result := True;
  end;
end;

function TCustomSparseDataSet.GetIsValue(const Layer, Row, Col: Integer): boolean;
begin
  result := True;
  CheckRestoreData;
  if (FPriorLayer = Layer)
    and (FPriorRow = Row)
    and (FPriorCol = Col) then
  begin
    result := FPriorResult;
    Exit;
  end;

  if (BoundaryTypeDataSet <> nil) and (BoundaryTypeDataSet <> self) then
  begin
    result := BoundaryTypeDataSet.IsValue[Layer, Row, Col];
  end;
  result := result and FAnnotation.IsValue[Layer, Row, Col];
  FPriorLayer := Layer;
  FPriorRow := Row;
  FPriorCol := Col;
  FPriorResult := result;

end;

procedure TCustomSparseDataSet.SetIsValue(const Layer, Row, Col: Integer;
      const Value: boolean);
begin
  if (BoundaryTypeDataSet <> nil) and (BoundaryTypeDataSet <> self) then
  begin
    BoundaryTypeDataSet.IsValue[Layer, Row, Col] := Value;
  end;
end;

procedure TDataArray.Assign(Source: TPersistent);
begin
  if (Source is TDataArray) then
  begin
//    DataType := TDataArray(Source).DataType;
//    Orientation := TDataArray(Source).Orientation;
//    TwoDInterpolator := TDataArray(Source).TwoDInterpolator;
  end
  else
  begin
    inherited;
  end;
end;

procedure TDataArray.CacheData;
var
  TempFile: TTempFileStream;
  Compressor: TCompressionStream;
begin
  if UpToDate then
  begin
    if not FCleared then
    begin
      FCachedLayerCount := FLayerCount;
      FCachedRowCount := FRowCount;
      FCachedColumnCount := FColumnCount;
    end;
    if not FDataCached and (IsUniform <> iuTrue) then
    begin
      if FTempFileName = '' then
      begin
        FTempFileName := TempFileName;
      end;
      TempFile := TTempFileStream.Create(FTempFileName, fmOpenReadWrite);
//      TempFile := TFileStream.Create(FTempFileName, fmCreate or fmShareDenyWrite,
//        ReadWritePermissions);
      Compressor := TCompressionStream.Create(clDefault, TempFile);
      try
        TempFile.Position := 0;
        StoreData(Compressor);
      finally
        Compressor.Free;
        TempFile.Free;
      end;
      FDataCached := True;
    end;
    Clear;
    FCleared := True;
  end;
end;

procedure TDataArray.RestoreArraySize;
var
  Grid: TCustomGrid;
begin
  if FCleared then
  begin
    Grid := frmGoPhast.Grid;
    UpDateDimensions(Grid.LayerCount, Grid.RowCount, Grid.ColumnCount);
    SetDimensions(False);
  end;
end;

procedure TDataArray.CheckIfUniform;
var
  FirstValueFound: boolean;
  ColLimit: Integer;
  RowLimit: Integer;
  LayerLimit: Integer;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  FIsUniform := iuUnknown;
  if not UpToDate or FCleared then
  begin
    Exit
  end;
  FUniformAnnotation := '';
  FUniformStringValue := '';
  FUniformIntegerValue := 0;
  FUniformBooleanValue := False;
  FUniformRealValue := 0;

  FirstValueFound := False;
  GetLimits(ColLimit, RowLimit, LayerLimit);

  for LayerIndex := 0 to LayerLimit do
  begin
    for RowIndex := 0 to RowLimit do
    begin
      for ColIndex := 0 to ColLimit do
      begin
        if IsValue[LayerIndex, RowIndex, ColIndex] then
        begin
          if FirstValueFound then
          begin
            case DataType of
              rdtDouble:
                begin
                  if FUniformRealValue <>
                    RealData[LayerIndex, RowIndex, ColIndex] then
                  begin
                    FIsUniform := iuFalse;
                    Exit;
                  end;
                end;
              rdtInteger:
                begin
                  if FUniformIntegerValue <>
                    IntegerData[LayerIndex, RowIndex, ColIndex] then
                  begin
                    FIsUniform := iuFalse;
                    Exit;
                  end;
                end;
              rdtBoolean:
                begin
                  if FUniformBooleanValue <>
                    BooleanData[LayerIndex, RowIndex, ColIndex] then
                  begin
                    FIsUniform := iuFalse;
                    Exit;
                  end;
                end;
              rdtString:
                begin
                  if FUniformStringValue <>
                    StringData[LayerIndex, RowIndex, ColIndex] then
                  begin
                    FIsUniform := iuFalse;
                    Exit;
                  end;
                end;
            end;
            if FUniformAnnotation <>
              Annotation[LayerIndex, RowIndex, ColIndex] then
            begin
              FIsUniform := iuFalse;
              Exit;
            end;
          end
          else
          begin
            FirstValueFound := True;
            case DataType of
              rdtDouble:
                begin
                  FUniformRealValue :=
                    RealData[LayerIndex, RowIndex, ColIndex];
                end;
              rdtInteger:
                begin
                  FUniformIntegerValue :=
                    IntegerData[LayerIndex, RowIndex, ColIndex];
                end;
              rdtBoolean:
                begin
                  FUniformBooleanValue :=
                    BooleanData[LayerIndex, RowIndex, ColIndex];
                end;
              rdtString:
                begin
                  FUniformStringValue :=
                    StringData[LayerIndex, RowIndex, ColIndex];
                end;
            end;
            FUniformAnnotation := Annotation[LayerIndex, RowIndex, ColIndex];
          end;
        end
        else
        begin
          FIsUniform := iuFalse;
          Exit;
        end;
      end;
    end;
  end;
  FIsUniform := iuTrue;
end;

procedure TDataArray.CheckRestoreData;
begin
  if FDataCached and FCleared and (IsUniform <> iuTrue) then
  begin
    RestoreArraySize;
    RestoreData;
  end;
end;

procedure TDataArray.RestoreData;
var
  TempFile: TTempFileStream;
  DecompressionStream: TDecompressionStream;
begin
  Assert(FileExists(FTempFileName));
  Assert(FDataCached);
  Assert(FCleared);
  TempFile := TTempFileStream.Create(FTempFileName, fmOpenRead);
//  TempFile := TFileStream.Create(FTempFileName,
//    fmOpenRead or fmShareDenyWrite, ReadWritePermissions);
  DecompressionStream := TDecompressionStream.Create(TempFile);
  try
    ReadData(DecompressionStream);
  finally
    DecompressionStream.Free;
    TempFile.Free;
  end;
  FCleared := False;
end;

procedure TDataArray.RestoreSubscription(Sender: TObject; const AName: string);
var
  Model: TPhastModel;
  ObservedItem: TObserver;
begin
  Model := FPhastModel as TPhastModel;
  ObservedItem := Model.GetObserverByName(AName);
  Assert(ObservedItem <> nil);
  ObservedItem.TalksTo(self);
  Invalidate;
end;

{ TContours }

procedure TContours.Assign(Source: TContours);
begin
  AutomaticColors := Source.AutomaticColors;
  SpecifyContours := Source.SpecifyContours;
  ContourValues := Copy(Source.ContourValues);
  LineThicknesses := Copy(Source.LineThicknesses);
  ContourColors := Copy(Source.ContourColors);
  LogTransform := Source.LogTransform;
end;

constructor TContours.Create;
begin
  FAutomaticColors := True;
end;

function TContours.GetCount: integer;
begin
  result := Length(FContourValues);
end;

procedure TContours.SetCount(const Value: integer);
begin
  if Value <> Count then
  begin
    SetLength(FContourValues, Value);
    SetLength(FContourColors, Value);
    SetLength(FLineThicknesses, Value);
  end;
end;

procedure TCustom2DInterpolater.EvaluateExpression(Compiler: TRbwParser;
  var Expression: TExpression; AScreenObject: TObject);
var
  DI: Integer;
  IsBoundary: Boolean;
  ScreenObject: TScreenObject;
begin
  try
    Expression.Evaluate;
  except
    on E: ERbwParserError do
    begin
      ScreenObject := AScreenObject as TScreenObject;
      DI := ScreenObject.IndexOfDataSet(DataSet);
      if DI >= 0 then
      begin
        IsBoundary := False;
      end
      else
      begin
        IsBoundary := True;
        DI := ScreenObject.IndexOfBoundaryDataSet(DataSet);
        Assert(DI >= 0);
      end;
      ResetScreenObjectFunction(DI, ScreenObject, Compiler, DataSet.DataType, E.Message, IsBoundary);
      Expression := Compiler.CurrentExpression;
      Expression.Evaluate;
    end;
  end;
end;

{ TSkipReal }

procedure TSkipReal.Assign(Source: TPersistent);
begin
  if Source is TSkipReal then
  begin
    RealValue := TSkipReal(Source).RealValue
  end
  else
  begin
    inherited;
  end;
end;

procedure TSkipReal.SetRealValue(const Value: double);
begin
  FRealValue := Value;
end;

{ TSkipInteger }

procedure TSkipInteger.Assign(Source: TPersistent);
begin
  if Source is TSkipInteger then
  begin
    IntegerValue := TSkipInteger(Source).IntegerValue
  end
  else
  begin
    inherited;
  end;
end;

procedure TSkipInteger.SetIntegerValue(const Value: integer);
begin
  FIntegerValue := Value;
end;

{ TSkipRealCollection }

constructor TSkipRealCollection.Create;
begin
  inherited Create(TSkipReal);
end;

{ TSkipIntegerCollection }

constructor TSkipIntegerCollection.Create;
begin
  inherited Create(TSkipInteger);
end;

initialization
  RegisterClasses([TDataArray, TCustom2DInterpolater, TRealSparseDataSet]);

end.