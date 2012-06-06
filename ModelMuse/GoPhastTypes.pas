{@abstract(@name is used to define types, constants, and small classes used
  in a variety of places in GoPhast.)}
unit GoPhastTypes;

interface

uses
  GR32, // defines TColor32.
  SysUtils, Types, Classes, FastGEO, Graphics;

type
  PReal = ^Real;

  {@abstract(@name is a one-dimensional array of doubles.)
  }
  TOneDRealArray = array of Double;

  {@abstract(@name is a one-dimensional array of integers.)
  }
  TOneDIntegerArray = array of integer;

  {@abstract(@name is a two-dimensional array of doubles.)
  }
  TTwoDRealArray = array of TOneDRealArray;

  {@abstract(@name is a three-dimensional array of doubles.)
  }
  TThreeDRealArray = array of TTwoDRealArray;

  {@abstract(@name is a pointer to a TPoint2D.)
  }
  P2DRealPoint = TPoint2DPtr;

  {@abstract(@name is a one-dimensional array of T2DRealPoints.)
  }
  TRealPointArray = TPolygon2D;//array of TPoint2D;

  {@abstract(@name is a two-dimensional array of T2DRealPoints.)
  }
  T2DRealPointArray = array of TRealPointArray;

  {@abstract(@name is a two-dimensional array of booleans.)
  }
  T2DBoolArray = array of array of boolean;

  {@abstract(@name represents a 3D point with real-number coordinates.)
  }
  T3DRealPoint = record
    X: real;
    Y: real;
    Z: real;
  end;
  P3DRealPoint = ^T3DRealPoint;

  TRealArray = array of Real;

  // @name is a 1D array of @link(T3DRealPoint)s.
  T3DRealPointArray1 = array of T3DRealPoint;

  // @name is a 2D array of @link(T3DRealPoint)s.
  T3DRealPointArray2 = array of T3DRealPointArray1;

  // @name is a 3D array of @link(T3DRealPoint)s.
  T3DRealPointArray3 = array of array of array of T3DRealPoint;

  // @name records the minimum and maximum values assigned to a data set.
  TMinMax = record
    LogRMin, LogRMax: Double;
    RMinPositive, RMin, RMax: Real;
    IMin, IMax: Integer;
    BMin, BMax: Boolean;
    SMin, SMax: string;
  end;


  // @name is used to indicate which view of the model the cursor
  // is over.
  TCursorGrid = (cgNone, cgTop, cgFront, cgSide);

  // @name is used to describe the direction
  // @link(ScreenObjectUnit.TScreenObject)s are
  // viewed from.
  TViewDirection = (vdTop, vdFront, vdSide);

  // @name is used to specify the columns in the table on
  // @link(frmDataSetsUnits.TfrmDataSets).
  //
  // @value(dcName = name of the @link(DataSetUnit.TDataArray).)
  // @value(dcType = the type of data @link(DataSetUnit.TDataArray.DataType)
  //   (boolean, integer, real number, or string)
  //   stored by the @link(DataSetUnit.TDataArray).)
  // @value(dcOrientation = the @link(DataSetUnit.TDataArray.Orientation) of the
  //   @link(DataSetUnit.TDataArray).)
  // @value(dcEvaluatedAt = the @link(DataSetUnit.TDataArray.EvaluatedAt)
  //    of the @link(DataSetUnit.TDataArray).)
  // @value(dcUnits = the @link(DataSetUnit.TDataArray.Units) of the
  //   @link(DataSetUnit.TDataArray).)
  // @value(dcFormula = the @link(DataSetUnit.TDataArray.Formula) of the
  //    @link(DataSetUnit.TDataArray).)
  // @value(dcInterpolation = the @link(DataSetUnit.TDataArray.TwoDInterpolator)
  //    of the @link(DataSetUnit.TDataArray).)
  TDataColumns = (dcName, dcType, dcOrientation, dcEvaluatedAt,
    dcUnits, dcFormula, dcInterpolation);
  // @name specifies which values in the table on
  // @link(frmDataSetsUnits.TfrmDataSets) the user can edit.
  // only columns not included in @name can be edited.
  TDataLock = set of TDataColumns;

  // @name is used in TDataArray.@link(TDataArray.Orientation) to
  // indicate whether the @link(TDataArray) is a 2D or 3D data set and,
  // if it is 2D, which face of the grid it is associated with.
  //
  // @value(dsoTop 2D top face)
  // @value(dsoFront 2D front face)
  // @value(dsoSide 2D side face)
  // @value(dso3D 3D)
  TDataSetOrientation = (dsoTop, dsoFront, dsoSide, dso3D);

  TDataSetOrientations = set of TDataSetOrientation;

  // @name is used in specifying the number of elevations associated with
  // a @link(TScreenObject).
  TElevationCount = (ecZero, ecOne, ecTwo);

  // @name is used to specify whether a data set is evaluated at
  // element centers or at nodes.
  TEvaluatedAt = (eaBlocks, eaNodes);

  TLgrCellTreatment = (lctUse, lctIgnore, lctZero);

  // @name represents the frequencies with which data can be printed
  // in PHAST.
  TFrequencyUnits = (fuDefault, fuSeconds, fuMinutes, fuHours, fuDays,
    fuYears, fuStep, fuEnd);

  // @name represents the time units recognized by PHAST.
  TTimeUnits = (tuSeconds, tuMinutes, tuHours, tuDays, tuYears);

  // @name represents the length units recognized by PHAST.
  TLengthUnits = (luInches, luFeet, luMiles, luMillimeters,
    luCentimeters, luMeters, luKilometers);

  // @name represents the 1/length units recognized by PHAST.
  TInverseLengthUnits = (iluInches, iluFeet, iluMiles, iluMillimeters,
    iluCentimeters, iluMeters, iluKilometers);

  // @name represents the volume units recognized by PHAST.
  TVolumeUnits = (vuGallons, vuInches3, vuFeet3, vuMiles3,
    vuLiters, vuMillimeters3, vuCentimeters3, vuMeters3, vuKilometers3);

  // @name represents the solvers used by PHAST.
  TPhastSolver = (psDirect, psIterative);

  //TInterpolationDirection determines whether "PHAST" style interpolation
  // is used or "PHAST" style mixtures.  If "PHAST" style interpolation
  // is used, it also determines the coordinate direction.
  // @value(pidX = Interpolate in the X direction.)
  // @value(pidY = Interpolate in the Y direction.)
  // @value(pidZ = Interpolate in the Z direction.)
  // @value(pidMix = Use "PHAST" style mixtures.)
  // See @LINK(TPhastInterpolationValues).
  TInterpolationDirection = (pidX, pidY, pidZ, pidMix);

  {@abstract(@name is a pointer to a @link(TInterpolationDirection).)
  @longcode(#
  PInterpolationDirection = ^TInterpolationDirection;
  #)
  }
  PInterpolationDirection = ^TInterpolationDirection;

  // @name represent the items whose print frequencies
  // can be specified in PHAST.
  TPrintFrequencyRows = (pfrName, pfrTime, pfrFlowRate, pfrComponents,
    pfrConductance, pfrFlowBalance, pfrChemPrint, pfrHDFChem, pfrHDFHeads,
    pfrHDFVelocity, pfrHeads, pfrProgress, pfrRestart, pfrVelocities, pfrWells, pfrXYZChem,
    pfrXYZComponents, pfrXYZHeads, pfrXYZVelocities, pfrXYZWells,
    pfrBoundaryConditions, pfrDefault);

  // @name represents the types of boundary
  // @value(btNone = no boundary condition)
  // @value(btSpecifiedHead = Specified head boundary condition)
  // @value(btFlux = Flux boundary condition)
  // @value(btLeaky = Leaky boundary condition)
  // @value(btRiver = River boundary condition)
  // @value(btWell = Well boundary condition)
  TBoundaryTypes = (btNone, btSpecifiedHead, btFlux, btLeaky, btRiver, btWell);
  TModflowBoundaryType = (mbtNone, mbtCHD);

  // @name specifies how elevations are specified in the well boundary
  // condition. See @link(TWellBoundary.WellElevationFormat).
  TWellElevationFormat = (wefElevation, wefDepth);

  // @name is used to indicate what type of model is active.
  // The type of model should never be set to msUndefined.
  TModelSelection = (msUndefined, msPhast, msModflow, msModflowLGR,
    msModflowNWT {$IFDEF SUTRA}, msSutra {$ENDIF});

  //  @name is used to indicate how the spacing of layers within a unit
  // is specified.
  // @value(gmUniform) The layers are spaced uniformly.
  // @value(gmUp) The layers increase in thickness upward.
  // @value(gmDown) The layers increase in thickness downward.
  // @value(gmMiddle) The layers increase in thickness toward the middle
  // of the unit from both the top and bottom.
  // @value(gmEdge) The layers increase in thickness toward both the top
  // and bottom of the unit from the middle.
  // @value(gmCustom) The thickness of each layer is specified individually.
  TGrowthMethod = (gmUniform, gmUp, gmDown, gmMiddle, gmEdge, gmCustom);

  // @name indicates the locations at which a @link(TScreenObject) should
  // assign values to cells in a @link(TDataArray)
  // @value(alAll Assign values to all locations.)
  // @value(alFirstVertex Assign values to cells at the location of the first
  //   vertex in the @link(TScreenObject).)
  // @value(alLastVertex Assign values to cells at the location of the last
  //   vertex in the @link(TScreenObject).)
  TAssignmentLocation = (alAll, alFirstVertex, alLastVertex);

  TByteSet = set of byte;

  TIface = (iIndeterminant, iHorizontal, iInternal,
    iLeft, iRight, iFront, iBack, iBottom, iTop);

  TStatFlag = (stVariance, stStandardDev,
    stCoefVar, stWeight, stSquaredWeight);

  TObservationPurpose = (ofObserved, ofPredicted, ofInacative);

  // @name is used in @link(TCustomTransientWriter.AssignTransient2DArray).
  // @value(umAssign Replace existing values with new values.)
  // @value(umAdd Add new values to existing values.)
  TUpdateMethod = (umAssign, umAdd);

  // In MT3DMS transport observations, observation results can be specified
  // either at a specific time or at a desired frequency using the same
  // variable. @name is used to indicate which form is to be used.
  TObservationType = (otTime, otFrequency);

  TBaseModel = class;

  // @abstract(@name invalidates the model when it is changed.)
  TPhastCollection = class(TCollection)
  protected
    FModel: TBaseModel;
  public
    procedure InvalidateModel;
    property Model: TBaseModel read FModel;
    // @name invalidates the model.
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification);
      override;
    constructor Create(ItemClass: TCollectionItemClass; Model: TBaseModel);
  end;

  TPhastCollectionItem = class(TCollectionItem)
  protected
    procedure SetRealProperty(var AField: double; const NewValue: double);
    procedure SetBooleanProperty(var AField: boolean; const NewValue: boolean);
    procedure SetIntegerProperty(var AField: integer; const NewValue: integer);
    procedure SetStringProperty(var AField: string; const NewValue: string);
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
  public
    procedure InvalidateModel;
    function Model: TBaseModel;
  end;

  TRealItem = class(TPhastCollectionItem)
  private
    FValue: double;
    FOnChange: TNotifyEvent;
    procedure SetValue(const Value: double);
  protected
    procedure ReadValue(Reader: TReader);
    procedure WriteValue(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
    function IsSame(Item: TRealItem): Boolean;
  published
    property Value: double read FValue write SetValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TRealCollection = class(TPhastCollection)
  private
    FInitialValue: Real;
    function GetItems(Index: Integer): TRealItem;
    procedure SetItems(Index: Integer; const Value: TRealItem);
    procedure SetInitialValue(const Value: Real);
  public
    constructor Create(Model: TBaseModel);
    function IsSame(RealCollection: TRealCollection): Boolean;
    property  Items[Index: Integer]: TRealItem read GetItems
      write SetItems; default;
    function Add: TRealItem;
    property InitialValue: Real read FInitialValue write SetInitialValue;
  end;

  TPointArray = array of TPoint;

  TRealStorage = class(TPersistent)
  private
    FValue: real;
    FOnChange: TNotifyEvent;
    procedure SetValue(const Value: real);
  protected
    procedure ReadValue(Reader: TReader);
    procedure WriteValue(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Value: real read FValue write SetValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TStringStorage = class(TPersistent)
  private
    FValue: string;
    FOnChange: TNotifyEvent;
    procedure SetValue(const Value: string);
  protected
    procedure ReadValue(Reader: TReader);
    procedure WriteValue(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Value: string read FValue write SetValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGoPhastPersistent = class(TPersistent)
  protected
    FModel: TBaseModel;
    procedure InvalidateModel; virtual;
    procedure SetBooleanProperty(var AField: boolean; const NewValue: boolean);
    procedure SetIntegerProperty(var AField: integer; const NewValue: integer);
    procedure SetRealProperty(var AField: double; const NewValue: double);
    procedure SetStringProperty(var AField: string; const NewValue: string);
    procedure SetPointProperty(var AField: TPoint; const NewValue: TPoint);
    procedure SetColorProperty(var AField: TColor; const NewValue: TColor);
    procedure SetDataTimeProperty(var AField: TDateTime; const NewValue: TDateTime);
  public
    property Model: TBaseModel read FModel;
    Constructor Create(Model: TBaseModel);
  end;

  TBaseModel = class abstract(TComponent)
  private
    // See @link(UpToDate).
    FUpToDate: boolean;
  protected
    // See @link(UpToDate).
    procedure SetUpToDate(const Value : boolean); virtual;
    function GetDisplayName: string; virtual; abstract;
    function GetModelSelection: TModelSelection; virtual; abstract;
    procedure SetModelSelection(const Value: TModelSelection); virtual; abstract;
  public
    // Call @name to indicate that the model has changed in some important
    // respect.  The user will be prompted to save the model when closing.
    procedure Invalidate; virtual;

    // @name indicates whether or not the model needs to be saved to file.
    // See @link(Invalidate).
    property UpToDate: boolean read FUpToDate write SetUpToDate;
    property DisplayName: string read GetDisplayName;
  published
    property ModelSelection: TModelSelection read GetModelSelection
      write SetModelSelection;

  end;

  TLayerSort = class(TObject)
    Layer: integer;
    ActiveCells: integer;
    Proportion: double;
  end;


  function EvalAtToString(const Eval: TEvaluatedAt;
    const Model: TModelSelection; const Plural, TitleCase: boolean): string;

  function ValidName(const OriginalName: string): string;
  function RightCopy(const Source: string; LengthToCopy: integer): string;

  function OrientationToViewDirection(Orientation: TDataSetOrientation): TViewDirection;

const
  StrModelTop = 'Model_Top';

  {@name is used when writing the PHAST input file to insert a consistent
    number of blank spaces.

  @longcode(#
  BlankSpaces = '      ';
  #)
  }
  BlankSpaces = '      ';

  // @name is the section name in the ini file that holds the
  // names of the most recently opened files.
  MRU_Section = 'MostRecentlyUsed';

resourcestring
  {@name is used as the default name for a new data set.

  @longcode(#
  rsNewDataSet = 'New data set';
  #)
  }
  rsNewDataSet = 'New data set';

  // @name is used to set captions for several radio buttons.
  // @Seealso(rsSetValueOfIntersected)
  // @Seealso(rsSetValueOf)
  // @Seealso(rsByInterpolation)
  // @Seealso(frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  //  frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  // @Seealso(frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions
  // frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions)
  // @Seealso(frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions
  // frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions)
  // @Seealso(frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions
  // frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions)
  rsSetValueOfEnclosed = 'Set values of enclosed ';
  // @name is used to set captions for several radio buttons.
  // @Seealso(rsSetValueOfEnclosed)
  // @Seealso(rsSetValueOf)
  // @Seealso(rsByInterpolation)
  // @Seealso(frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  //  frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  // @Seealso(frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions
  // frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions)
  // @Seealso(frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions
  // frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions)
  // @Seealso(frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions
  // frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions)
  rsSetValueOfIntersected = 'Set values of intersected ';
  // @name is used to set captions for several radio buttons.
  // @Seealso(rsSetValueOfEnclosed)
  // @Seealso(rsSetValueOfIntersected)
  // @Seealso(rsByInterpolation)
  // @Seealso(frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  //  frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  // @Seealso(frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions
  // frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions)
  // @Seealso(frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions
  // frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions)
  // @Seealso(frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions
  // frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions)
  rsSetValueOf = 'Set values of ';
  // @name is used to set captions for several radio buttons.
  // @Seealso(rsSetValueOfEnclosed)
  // @Seealso(rsSetValueOfIntersected)
  // @Seealso(rsSetValueOf)
  // @Seealso(frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  //  frmScreenObjectPropertiesUnit.TfrmScreenObjectProperties.SetCheckBoxCaptions)
  // @Seealso(frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions
  // frmCustomImportSimpleFileUnit.TfrmCustomImportSimpleFile.SetCheckBoxCaptions)
  // @Seealso(frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions
  // frmImportPointsUnits.TfrmImportPoints.SetCheckBoxCaptions)
  // @Seealso(frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions
  // frmImportShapefileUnit.TfrmImportShapefile.SetCheckBoxCaptions)
  rsByInterpolation = ' by interpolation';

  StrLowerLimit = 'Lower limit';
  StrUpperLimit = 'Upper limit';
  StrObjectIntersectLength = 'ObjectIntersectLength';
  StrObjectSectionIntersectLength = 'ObjectSectionIntersectLength';
  StrObjectIntersectArea = 'ObjectIntersectArea';
  StrObjectArea = 'ObjectArea';
  StrObjectLength = 'ObjectLength';
  StrStartingTime = 'Starting time';
  StrEndingTime = 'Ending time';
  StrPumpingRate = 'Pumping rate';
  StrConductance = 'Conductance';
  StrConductanceMultipl = ' conductance multiplier';
  StrRiverStage = 'River stage';
  StrRiverBottom = 'River bottom';
  StrBoundaryHead = 'Boundary head';
  StrDrainElevation = 'Drain elevation';
  StrStartingHead = 'Starting head';
  StrEndingHead = 'Ending head';
  StrElevation = 'Elevation';


  StrVariance = 'Variance (0)';
  StrStdDev = 'Standard dev. (1)';
  StrCoefVar = 'Coef. of var. (2)';
  StrWt = 'Weight (3)';
  StrSqRtWt = 'Sq. rt. of weight (4)';

  // @name represents the characters used to define the end of a line.
  EndOfLine = sLineBreak;
  StrStressPeriodLabel = 'Stress Period: ';
  StrTimeStepLabel = 'Time Step: ';
  StrElapsedTimeLabel = 'Elapsed Time: ';
  StrTransportStep = 'Transport Step: ';
  StrParentModel = 'Parent model';

const
  // On Linux, @name is used to control the access permissions of files.
  // @name has no effect in Windows.
{$IFDEF MSWINDOWS}
  ReadWritePermissions = 0;
{$ELSE}
  ReadWritePermissions = S_IREAD or S_IWRITE or S_IRGRP or S_IWGRP or S_IROTH;
{$ENDIF}

  clTransparent32 : TColor32 = 0;
  SelectEpsilon = 5;

var
  ObservationStatFlagLabels: TStringList = nil;
  PredictionStatFlagLabels: TStringList = nil;

function StrToStatFlag(Const AStatFlagLabel: string): TStatFlag;
function StatFlatToStr(AStatFlag: TStatFlag): string;
function SortLayerSorts(Item1, Item2: Pointer): Integer;

resourcestring
  StrNoBoundaryConditio = 'No boundary conditions assigned to the %s because' +
  ' the object does not set the values of either enclosed or intersected cel' +
  'ls.';
  StrErrorObjectDuplicateTimes = 'Error; Object = %0:s Duplicate Times = %1:s';
  StrErrorObjectEarlyTimes = 'Error; Object = %0:s Early Times = %1:s';
  StrErrorObjectLateTimes = 'Error; Object = %0:s Late Times = %1:s';
  StrObjectS = 'Object: %s';
  StrObjectSTimeG = 'Object: %0:s; Time: %1:g';
  StrAssignedBy0sWit = 'Assigned by %0:s with formula = "%1:s."';

implementation


{$IFNDEF Testing}
uses PhastModelUnit, Character;
{$ENDIF}

function SortLayerSorts(Item1, Item2: Pointer): Integer;
var
  LayerSort1: TLayerSort;
  LayerSort2: TLayerSort;
begin
  LayerSort1 := Item1;
  LayerSort2 := Item2;
  result := LayerSort1.ActiveCells - LayerSort2.ActiveCells;
  if result = 0 then
  begin
    result := LayerSort1.Layer - LayerSort2.Layer;
  end;
end;


function StrToStatFlag(Const AStatFlagLabel: string): TStatFlag;
var
  Position: integer;
begin
  Position := ObservationStatFlagLabels.IndexOf(AStatFlagLabel);
  Assert(Position >= 0);
  result := TStatFlag(Position);
end;

function StatFlatToStr(AStatFlag: TStatFlag): string;
begin
  result := ObservationStatFlagLabels[Ord(AStatFlag)];
end;

{ TPhastCollection }

constructor TPhastCollection.Create(ItemClass: TCollectionItemClass;
  Model: TBaseModel);
begin
  FModel := Model;
  inherited Create(ItemClass);
end;

procedure TPhastCollection.InvalidateModel;
begin
  if Model <> nil then
  begin
    Model.Invalidate;
  end;
end;

procedure TPhastCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited;
{$IFNDEF Testing}
  InvalidateModel;
{$ENDIF}
end;

{ TRealStorage }

{ TRealStorage }

procedure TRealStorage.Assign(Source: TPersistent);
begin
  if Source is TRealStorage then
  begin
    Value := TRealStorage(Source).Value;
  end
  else
  begin
    inherited;
  end;
end;

procedure TRealStorage.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Value', ReadValue, WriteValue, Value = 0)
end;

procedure TRealStorage.ReadValue(Reader: TReader);
begin
  Value := Reader.ReadFloat;
end;

procedure TRealStorage.SetValue(const Value: real);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    if Assigned(OnChange) then
    begin
      OnChange(self);
    end;
  end;
end;

procedure TRealStorage.WriteValue(Writer: TWriter);
begin
  Writer.WriteFloat(Value);
end;

{ TGoPhastPersistent }

constructor TGoPhastPersistent.Create(Model: TBaseModel);
begin
  inherited Create;
  Assert((Model = nil) or (Model is TCustomModel));
  FModel := Model;
end;

procedure TGoPhastPersistent.InvalidateModel;
begin
  if FModel <> nil then
  begin
    FModel.Invalidate;
  end;
end;

procedure TGoPhastPersistent.SetBooleanProperty(var AField: boolean;
  const NewValue: boolean);
begin
  if AField <> NewValue then
  begin
    AField := NewValue;
    InvalidateModel;
  end;
end;

procedure TGoPhastPersistent.SetColorProperty(var AField: TColor;
  const NewValue: TColor);
begin
  if AField <> NewValue then
  begin
    AField := NewValue;
    InvalidateModel;
  end;
end;

procedure TGoPhastPersistent.SetDataTimeProperty(var AField: TDateTime;
  const NewValue: TDateTime);
begin
  if AField <> NewValue then
  begin
    AField := NewValue;
    InvalidateModel;
  end;
end;

procedure TGoPhastPersistent.SetIntegerProperty(var AField: integer;
  const NewValue: integer);
begin
  if AField <> NewValue then
  begin
    AField := NewValue;
    InvalidateModel;
  end;
end;

procedure TGoPhastPersistent.SetPointProperty(var AField: TPoint;
  const NewValue: TPoint);
begin
  SetIntegerProperty(AField.X, NewValue.X);
  SetIntegerProperty(AField.Y, NewValue.Y);
end;

procedure TGoPhastPersistent.SetRealProperty(var AField: double;
  const NewValue: double);
begin
  if AField <> NewValue then
  begin
    AField := NewValue;
    InvalidateModel;
  end;
end;

procedure TGoPhastPersistent.SetStringProperty(var AField: string;
  const NewValue: string);
begin
  if AField <> NewValue then
  begin
    AField := NewValue;
    InvalidateModel;
  end;
end;

function EvalAtToString(const Eval: TEvaluatedAt; const Model: TModelSelection;
  const Plural, TitleCase: boolean): string;
begin
  result := '';
  case Model of
    msUndefined, msPhast {$IFDEF SUTRA}, msSutra {$ENDIF}:
      begin
        case Eval of
          eaBlocks:
            begin
              if TitleCase then
              begin
                result := 'Element';
              end
              else
              begin
                result := 'element';
              end;
            end;
          eaNodes:
            begin
              if TitleCase then
              begin
                result := 'Node';
              end
              else
              begin
                result := 'node';
              end;
            end;
          else
            Assert(False);
        end;
      end;
    msModflow, msModflowLGR, msModflowNWT:
      begin
        case Eval of
          eaBlocks:
            begin
              if TitleCase then
              begin
                result := 'Cell';
              end
              else
              begin
                result := 'cell';
              end;
            end;
          eaNodes:
            begin
              if TitleCase then
              begin
                result := 'Cell corner';
              end
              else
              begin
                result := 'cell corner';
              end;
            end;
          else
            Assert(False);
        end;
      end;
    else Assert(False);
  end;
  if Plural then
  begin
    result := result + 's';
  end;
end;

{ TPhastCollectionItem }

procedure TPhastCollectionItem.BeginUpdate;
begin
  // do nothing;
end;

procedure TPhastCollectionItem.EndUpdate;
begin
  // do nothing;
end;

procedure TPhastCollectionItem.InvalidateModel;
var
  PhastCollection: TPhastCollection;
begin
  PhastCollection := Collection as TPhastCollection;
  PhastCollection.Update(self);
  PhastCollection.InvalidateModel;
end;

function ValidName(const OriginalName: string): string;
  function Alpha(C: Char): Boolean; inline;
  begin
    Result := TCharacter.IsLetter(C) or (C = '_');
  end;

  function AlphaNumeric(C: Char): Boolean; inline;
  begin
    Result := TCharacter.IsLetterOrDigit(C) or (C = '_');
  end;
var
  Index: integer;
  AChar: Char;
begin
  result :=  Trim(OriginalName);
  try
    if IsValidIdent(Result, False) then
    begin
      Exit;
    end
    else
    begin
      for Index := 1 to Length(result) do
      begin
        AChar := result[Index];
        if Index = 1 then
        begin
          if not Alpha(AChar) then
          begin
            result[Index] := '_';
          end;
        end
        else
        begin
          if not AlphaNumeric(AChar) then
          begin
            result[Index] := '_';
          end;
        end;
      end;
    end;
  finally
    if result = '' then
    begin
      result := '_';
    end
  end;
end;

function RightCopy(const Source: string; LengthToCopy: integer): string;
var
  Start: Integer;
begin
  Start := Length(Source) - LengthToCopy + 1;
  if Start < 1 then
  begin
    Start := 1;
  end;
  result := Copy(Source, Start, LengthToCopy);
end;

function TPhastCollectionItem.Model: TBaseModel;
begin
  result := (Collection as TPhastCollection).Model;
end;

procedure TPhastCollectionItem.SetBooleanProperty(var AField: boolean;
  const NewValue: boolean);
begin
  if AField <> NewValue then
  begin
    BeginUpdate;
    try
      AField := NewValue;
      InvalidateModel;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TPhastCollectionItem.SetIntegerProperty(var AField: integer;
  const NewValue: integer);
begin
  if AField <> NewValue then
  begin
    BeginUpdate;
    try
      AField := NewValue;
      InvalidateModel;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TPhastCollectionItem.SetRealProperty(var AField: double;
  const NewValue: double);
begin
  if AField <> NewValue then
  begin
    BeginUpdate;
    try
      AField := NewValue;
      InvalidateModel;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TPhastCollectionItem.SetStringProperty(var AField: string;
  const NewValue: string);
begin
  if AField <> NewValue then
  begin
    BeginUpdate;
    try
      AField := NewValue;
      InvalidateModel;
    finally
      EndUpdate;
    end;
  end;
end;

procedure InitializeStatTypeLabels;
begin
  ObservationStatFlagLabels := TStringList.Create;
  PredictionStatFlagLabels := TStringList.Create;

  ObservationStatFlagLabels.Add(StrVariance);
  ObservationStatFlagLabels.Add(StrStdDev);
  ObservationStatFlagLabels.Add(StrCoefVar);
  ObservationStatFlagLabels.Add(StrWt);
  ObservationStatFlagLabels.Add(StrSqRtWt);

  PredictionStatFlagLabels.Add(StrVariance);
  PredictionStatFlagLabels.Add(StrStdDev);
end;

procedure TBaseModel.SetUpToDate(const Value : boolean);
begin
  FUpToDate := Value;
end;

procedure TBaseModel.Invalidate;
begin
  UpToDate := False;
end;

function OrientationToViewDirection(Orientation: TDataSetOrientation): TViewDirection;
begin
  result := vdTop;
  case Orientation of
    dsoTop: result := vdTop;
    dsoFront: result := vdFront;
    dsoSide: result := vdSide;
    else Assert(False);
  end;
end;


{ TStringStorage }

procedure TStringStorage.Assign(Source: TPersistent);
begin
  if Source is TStringStorage then
  begin
    Value := TStringStorage(Source).Value;
  end
  else
  begin
    inherited;
  end;
end;

procedure TStringStorage.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Value', ReadValue, WriteValue, Value = '')
end;

procedure TStringStorage.ReadValue(Reader: TReader);
begin
  Value := Reader.ReadString;
end;

procedure TStringStorage.SetValue(const Value: string);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    if Assigned(OnChange) then
    begin
      OnChange(self);
    end;
  end;
end;

procedure TStringStorage.WriteValue(Writer: TWriter);
begin
  Writer.WriteString(Value);
end;

{ TRealItem }

procedure TRealItem.Assign(Source: TPersistent);
begin
  if Source is TRealItem then
  begin
    Value := TRealItem(Source).Value;
  end
  else
  begin
    inherited;
  end;
  // if Assign is updated, update IsSame too.
end;

procedure TRealItem.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Value', ReadValue, WriteValue, Value = 0)
end;

function TRealItem.IsSame(Item: TRealItem): Boolean;
begin
  Result := Value = Item.Value;
end;

procedure TRealItem.ReadValue(Reader: TReader);
begin
  Value := Reader.ReadFloat;
end;

procedure TRealItem.SetValue(const Value: double);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    InvalidateModel;
    if Assigned(OnChange) then
    begin
      OnChange(self);
    end;
  end;
end;

procedure TRealItem.WriteValue(Writer: TWriter);
begin
  Writer.WriteFloat(Value);
end;

{ TRealCollection }

function TRealCollection.Add: TRealItem;
begin
  result := inherited Add as TRealItem;
  result.FValue := InitialValue;
end;

constructor TRealCollection.Create(Model: TBaseModel);
begin
  inherited Create(TRealItem, Model);
end;

function TRealCollection.GetItems(Index: Integer): TRealItem;
begin
  result := inherited Items[index] as TRealItem
end;

function TRealCollection.IsSame(RealCollection: TRealCollection): Boolean;
var
  index: Integer;
begin
  // if Assign is updated, update IsSame too.
  result := Count = RealCollection.Count;
  if result then
  begin
    for index := 0 to Count - 1 do
    begin
      result := Items[index].IsSame(RealCollection[index]);
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TRealCollection.SetInitialValue(const Value: Real);
begin
  FInitialValue := Value;
end;

procedure TRealCollection.SetItems(Index: Integer; const Value: TRealItem);
begin
  inherited Items[index] := Value;
end;

initialization
  InitializeStatTypeLabels;

finalization
  ObservationStatFlagLabels.Free;
  PredictionStatFlagLabels.Free;

end.
