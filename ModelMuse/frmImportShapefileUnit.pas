{@abstract(The primary purpose of @name is to define @link(TfrmImportShapefile)
  which is used to import Shapefiles.)
  @name also defines @link(TUndoImportShapefile) which is used to undo or
  redo the import of the Shapefile.
  @author(Richard B. Winston <rbwinst@usgs.gov>)}
unit frmImportShapefileUnit;

interface

{ TODO : When importing objects, move to first object if none are
visisble on screen. }
{ TODO :
There needs to be a way to import lots of Time-Series data
from a data base (Access or Excel) }
{ TODO :
There should be a method to convert coordinates from lat-long to X-Y
using Lambert and Albers projections (and UTM).  Get a table of  the
parameters for all the state-plane coordinate systems for use in the
conversion. }
uses Windows,
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, XBase1, Buttons, ExtCtrls,
  Grids, RbwDataGrid4, ShapefileUnit, IntListUnit, ScreenObjectUnit,
  RbwParser, ComCtrls, Spin, UndoItemsScreenObjects, JvExStdCtrls, JvCombobox,
  JvListComb, Mask, JvExMask, JvSpin, JvExControls, JvPageList, ArgusDataEntry,
  ModflowPackagesUnit, ModflowBoundaryUnit, DataSetUnit, UndoItems, RbwEdit,
  JvExComCtrls, JvComCtrls, ModflowPackageSelectionUnit,
  frameLocationMethodUnit, JvToolEdit, ModflowTransientListParameterUnit,
  OrderedCollectionUnit;

type
  TSfrColumns = (scStartTime, scEndTime, scIcalc,
    scOutflowSegment, scDiversionSegment, scIprior,
    scFlow, scPtsw, scEtsw,
    scRunoff, scRoughCh, scRoughBk, scCdpth, scFdpth, scAwdth, edBwdth,
    scHcond1, scThickM1, scElevUp, scWidth1, scDepth1, scHcond2, scThickM2,
    scElevDn, scWidth2, scDepth2);

  TLakeColumns = (lcStartTime, lcEndTime, lcMinStage, lcMaxStage, lcPrecip,
    lcEvap, lcRunoff, lcWithdrawl);

  TUzfColumns = (ucStartTime, ucEndTime, ucInfiltration, ucEvapRate,
    ucExtinctDepth, ucExtinctWaterContent);

  TRchColumns = (rcStartTime, rcEndTime, rcParameterName, rcFluxRate, rcLayer);

  TChdColumns = (ccStartingTime, ccEndingTime, ccParameterName,
    ccStartingHead, ccEndingHead);

  TDrnColumns = (dcStartingTime, dcEndingTime, dcParameterName,
    dcElevation, dcConductance);

  TDrtColumns = (dtcStartingTime, dtcEndingTime, dtcParameterName,
    dtcElevation, dtcConductance, dtcReturnFraction);

  TEtsColumns = (etscStartingTime, etscEndingTime, etscParameterName,
    etscRate, etscSurface, etscDepth);

  TEvtColumns = (evtcStartingTime, evtcEndingTime, evtcParameterName,
    evtRate, evtcSurface, evtcDepth, evtcLayer);

  TGhbColumns = (ghbcStartingTime, ghbcEndingTime, ghbcParameterName,
    ghbcHead, ghbcConductance);

  TRivColumns = (rivcStartingTime, rivcEndingTime, rivcParameterName,
    rivcBottom, rivcStage, rivcConductance);

  TWelColumns = (welcStartingTime, welcEndingTime, welcParameterName,
    welcPumpingRate);

  {@abstract(@name is used to undo or redo the import of a Shapefile)}
  TUndoImportShapefile = class(TCustomImportMultipleScreenObjects)
  protected
    FNewDataSets: TList;
    FOldProperties: TList;
    FNewProperties: TList;
    // @name describes what @classname does.
    function Description: string; override;
  public
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name makes sure the (TScreenObject.Deleted)
    // property @link(TScreenObject)s is @false.
    procedure DoCommand; override;
    // @name calls @link(DoCommand).
    procedure Redo; override;
    procedure StoreChangedDataSetProperties(var OldProperties, NewProperties: TList);
    procedure StoreNewDataSets(var NewDataSets: TList);
    // @name makes sure the (TScreenObject.Deleted)
    // property @link(TScreenObject)s is @true.
    procedure Undo; override;
    // @name notifies @link(frmGoPhast) that
    // frmGoPhast.@link(TfrmGoPhast.TopScreenObjectsChanged) is @True or
    // frmGoPhast.@link(TfrmGoPhast.FrontScreenObjectsChanged) is @True or
    // frmGoPhast.@link(TfrmGoPhast.SideScreenObjectsChanged) is @True
    // all three or true.
    // It then sets AScreenObject.@link(TObserver.UpToDate) to @True.
  end;

{ TODO : Check for similarities between TfrmImportShapefile and TfrmImportDXF. }

  { TODO : Allow there to be a persistent connection between the imported
  TScreenObjects and the original Shapefile.
  The user can update when desired or
  have updating done automatically.}

  {@abstract(@name is used to import Shapefiles.)}
  TfrmImportShapefile = class(TfrmCustomGoPhast)
    // @name: TButton;
    // See @link(btnSelectClick).
    btnAll: TButton;
    // @name: TBitBtn;
    // Clicking @name closes the @classname without changing anything.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // Clicking @name show help on the @classname.
    btnHelp: TBitBtn;
    // @name: TButton;
    // See @link(btnSelectClick).
    btnNone: TButton;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TButton;
    // See @link(btnToggleClick).
    btnToggle: TButton;
    // @name is used to turn on or off coordinate transformations from
    // decimal degrees to UTM coordinates.
    cbCoordinateConversion: TCheckBox;
    // @name is used to choose the ellipsoid for the coordinate conversion.
    comboEllipsoid: TComboBox;
    // @name is an image of the UTM zones.
    imageUtmZones: TImage;
    // @name tells that 10 points will be plotted on the map if coordinate
    // conversion is to be performed.
    lblCoordinateConversionInfo: TLabel;
    // @name gives the coordinates of the first point.
    lblCoordinates: TLabel;
    // @name is the label for @link(comboEllipsoid).
    lblEllipsoid: TLabel;
    // @name is the label for @link(seZoneNumber).
    lblUtmZoneNumber: TLabel;
    // @name: TOpenDialog;
    // @name is used to select the Shapefile.
    OpenDialogShape: TOpenDialog;
    // @name is TPageControl used to hold @link(tabData) and
    // @link(tabCoordinateConversion).
    pcImportShape: TPageControl;
    // @name: TPanel;
    // @name holds the buttons and other controls at the bottom of the
    // @classname.
    pnlButton: TPanel;
    // @name holds controls used to determine how the shapes in the
    // Shapefile will be imported.
    pnlData: TPanel;
    // @name shows the first point to be plotted on the map.
    Shape1: TShape;
    // @name shows the second point to be plotted on the map.
    Shape2: TShape;
    // @name shows the third point to be plotted on the map.
    Shape3: TShape;
    // @name shows the fourth point to be plotted on the map.
    Shape4: TShape;
    // @name shows the fifth point to be plotted on the map.
    Shape5: TShape;
    // @name shows the sixth point to be plotted on the map.
    Shape6: TShape;
    // @name shows the seventh point to be plotted on the map.
    Shape7: TShape;
    // @name shows the eighth point to be plotted on the map.
    Shape8: TShape;
    // @name shows the ninth point to be plotted on the map.
    Shape9: TShape;
    // @name shows the tenth point to be plotted on the map.
    Shape10: TShape;
    // @name holds the controls that determine how the shapes in the
    // Shapefile will be imported.
    tabData: TTabSheet;
    // @name holds the controls used to convert from decimal degrees to UTM
    // coordinates.
    tabCoordinateConversion: TTabSheet;
    // @name: TXBase;
    // @name is used to read the database file that is part of the
    // Shapefile.
    xbShapeDataBase: TXBase;
    seZoneNumber: TJvSpinEdit;
    pnlDataGrids: TPanel;
    dgFields: TRbwDataGrid4;
    tabFeatures: TTabSheet;
    pnlBoundaryCondition: TPanel;
    splitterBoundary: TSplitter;
    pnlBoundaryControls: TPanel;
    lblBoundaryTimeCount: TLabel;
    comboBoundaryChoice: TComboBox;
    seBoundaryTimeCount: TJvSpinEdit;
    rdgBoundaryConditions: TRbwDataGrid4;
    plBoundary: TJvPageList;
    jvspNone: TJvStandardPage;
    jvspPhastSpecifiedHead: TJvStandardPage;
    lblSolutionType: TLabel;
    comboSolutionType: TComboBox;
    jvspPhastLeaky: TJvStandardPage;
    jvspPhastRiver: TJvStandardPage;
    jvspPhastWell: TJvStandardPage;
    lblRiverDescripton: TLabel;
    lblRiverHydraulicConductivity: TLabel;
    lblRiverWidth: TLabel;
    lblRiverDepth: TLabel;
    lblRiverBedThickness: TLabel;
    lblLeakyHydraulicConductivity: TLabel;
    lblLeakyThickness: TLabel;
    comboLeakyHydraulicConductivity: TComboBox;
    comboLeakyThickness: TComboBox;
    comboRiverDescripton: TComboBox;
    comboRiverHydraulicConductivity: TComboBox;
    comboRiverWidth: TComboBox;
    comboRiverDepth: TComboBox;
    comboRiverBedThickness: TComboBox;
    pnlPhastWell: TPanel;
    lblWellDescription: TLabel;
    WellDescription: TComboBox;
    lblWellDiameter: TLabel;
    comboWellDiameter: TComboBox;
    lblWellLandSurfaceDatum: TLabel;
    comboWellLandSurfaceDatum: TComboBox;
    lblWellPumpAllocation: TLabel;
    comboWellPumpAllocation: TComboBox;
    comboWellIntervalStyle: TComboBox;
    lblWellIntervalStyle: TLabel;
    lblWellIntervals: TLabel;
    seWellIntervals: TJvSpinEdit;
    dgWellElevations: TRbwDataGrid4;
    jvspConductanceInterp: TJvStandardPage;
    lblConductanceInterpretation: TLabel;
    comboFormulaInterp: TComboBox;
    lblBoundaryChoice: TLabel;
    jvspModflowSFR: TJvStandardPage;
    pcSFR: TPageControl;
    tabSfrBasic: TTabSheet;
    tabSfrUnsaturated: TTabSheet;
    lblSfrSegmentNumber: TLabel;
    comboSfrSegmentNumber: TComboBox;
    lblReachLength: TLabel;
    lblStreamTop: TLabel;
    lblStreambedThickness: TLabel;
    lblSlope: TLabel;
    lblStreambedK: TLabel;
    lblSaturatedVolumetricWater: TLabel;
    lblInitialVolumetricWater: TLabel;
    lblBrooksCoreyExponent: TLabel;
    lblMaxUnsaturatedKz: TLabel;
    comboSfrReachLength: TComboBox;
    comboSfrStreambedTop: TComboBox;
    comboSfrStreamSlope: TComboBox;
    comboSfrStreambedThickness: TComboBox;
    comboSfrStreambedKv: TComboBox;
    comboSaturatedVolumetricWater: TComboBox;
    comboInitialVolumetricWater: TComboBox;
    comboBrooksCoreyExponent: TComboBox;
    comboaxUnsaturatedKz: TComboBox;
    jvspModflowLAK: TJvStandardPage;
    LblLakeID: TLabel;
    comboLakeID: TComboBox;
    lblInitialStage: TLabel;
    comboInitialStage: TComboBox;
    lblSill: TLabel;
    comboSill: TComboBox;
    lblCenterLake: TLabel;
    comboCenterLake: TComboBox;
    lblNumShapes: TLabel;
    tabOptions: TTabSheet;
    cbImportObjects: TCheckBox;
    cbEnclosedCells: TCheckBox;
    cbIntersectedCells: TCheckBox;
    cbInterpolation: TCheckBox;
    cbImportGrid: TCheckBox;
    lblImportCriterion: TLabel;
    lblCombineShapes: TLabel;
    lblVisibility: TLabel;
    edImportCriterion: TEdit;
    comboJoinObjects: TJvImageComboBox;
    comboVisibility: TJvImageComboBox;
    rgEvaluatedAt: TRadioGroup;
    btnImportCriterion: TButton;
    rgElevationCount: TRadioGroup;
    lblZ: TLabel;
    edZ: TRbwEdit;
    lblHighZ: TLabel;
    edHighZ: TRbwEdit;
    lblLowZ: TLabel;
    edLowZ: TRbwEdit;
    btnZ: TButton;
    btnHighZ: TButton;
    btnLowZ: TButton;
    jvspModflowDRT: TJvStandardPage;
    lblConductanceInterpretationDRT: TLabel;
    comboFormulaInterpDRT: TComboBox;
    lblDrainReturnLocationMethod: TLabel;
    comboDrainReturnLocationMethod: TComboBox;
    pcDrtReturnLChoice: TJvPageControl;
    tabDrtNone: TTabSheet;
    tabDrtLocation: TTabSheet;
    lblDrtX: TLabel;
    lblDrtY: TLabel;
    lblDrtZ: TLabel;
    rdeDrtX: TRbwDataEntry;
    rdeDrtY: TRbwDataEntry;
    rdeDrtZ: TRbwDataEntry;
    tabDrtCell: TTabSheet;
    lblDrtCol: TLabel;
    lblDrtRow: TLabel;
    lblDrtLay: TLabel;
    rdeDrtLay: TRbwDataEntry;
    rdeDrtRow: TRbwDataEntry;
    rdeDrtCol: TRbwDataEntry;
    jvspModflowHFB: TJvStandardPage;
    lblHydraulicConductivity: TLabel;
    lblBarrierThickness: TLabel;
    rgAngleAdjustment: TRadioGroup;
    comboHfbHydCond: TComboBox;
    comboHfbThickness: TComboBox;
    jvspModflowHOB: TJvStandardPage;
    lblHeadObservationNames: TLabel;
    comboHeadObservationNames: TComboBox;
    lblHeadObsType: TLabel;
    comboHeadObsType: TComboBox;
    rdeIgnoreValues: TRbwDataEntry;
    lblIgnoreValues: TLabel;
    comboITT: TComboBox;
    lblITT: TLabel;
    jvspModflowMNW2: TJvStandardPage;
    pcMnw2: TPageControl;
    tabBasic: TTabSheet;
    lblWellId: TLabel;
    lblLossType: TLabel;
    lblPartialPenetration: TLabel;
    lblZPump: TLabel;
    tabLossControls: TTabSheet;
    lblWellRadius: TLabel;
    lblSkinRadius: TLabel;
    lblBCoefficient: TLabel;
    lblCCoefficient: TLabel;
    lblPCoefficient: TLabel;
    lblCellToWellConductance: TLabel;
    lblKSkin: TLabel;
    tabDischargeAdjustment: TTabSheet;
    lblReferenceHead: TLabel;
    lblLiftQ0: TLabel;
    lblLiftQMax: TLabel;
    lblWellTolerance: TLabel;
    lblMnw2PumplocX: TLabel;
    lblMnw2PumplocY: TLabel;
    lblMnw2PumplocZ: TLabel;
    lblPumpLocation: TLabel;
    comboMnw2WellId: TComboBox;
    comboMnw2LossType: TComboBox;
    comboSpecifyPump: TComboBox;
    lblSpecifyPump: TLabel;
    comboZPump: TComboBox;
    comboMnw2PumplocX: TComboBox;
    comboMnw2PumplocY: TComboBox;
    comboMnw2PumplocZ: TComboBox;
    lblConstrainPumping: TLabel;
    comboConstrainPumping: TComboBox;
    lblPartialPenetrationFlag: TLabel;
    comboPartialPenetrationFlag: TComboBox;
    comboPartialPenetration: TComboBox;
    lblPumpCap: TLabel;
    comboPumpCap: TComboBox;
    comboWellRadius: TComboBox;
    comboSkinRadius: TComboBox;
    comboKSkin: TComboBox;
    comboBCoefficient: TComboBox;
    comboCCoefficient: TComboBox;
    comboPCoefficient: TComboBox;
    comboCellToWellConductance: TComboBox;
    comboReferenceHead: TComboBox;
    comboLiftQ0: TComboBox;
    comboLiftQMax: TComboBox;
    comboWellTolerance: TComboBox;
    rpShapeCompiler: TRbwParser;
    // @name edits the formula in @link(edImportCriterion).
    procedure btnImportCriterionClick(Sender: TObject);
    // @name sets all the checkboxes to checked
    // in column 1 of @link(dgFields) to
    // @true if Sender = btnAll.  Otherwise it sets them all to unchecked.
    // @name is the OnClick event-handler for @link(btnAll) and @link(btnNone).
    procedure btnSelectClick(Sender: TObject);
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name toggles the checkboxes in column 1 of @link(dgFields) from
    // Checked to Unchecked or the reverse.
    procedure btnToggleClick(Sender: TObject);
    // @name activates or deactivates controls related to coordinate conversion
    // and shows or hides points on the image of the UTM zones.
    procedure cbCoordinateConversionClick(Sender: TObject);
    // @name checks that at least one of @link(cbEnclosedCells),
    // @link(cbIntersectedCells), or @link(cbInterpolation) is checked
    // and emphasisizes them if not.  It also disables the OK button until
    // at least one of them is checked.
    procedure cbEnclosedCellsClick(Sender: TObject);
    // @name changes the ellipsoid used for the coordinate conversions.
    procedure comboEllipsoidChange(Sender: TObject);
    // @name draws some cells in a disabled state.
    procedure dgFieldsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    // @name assigns interpolators and @link(TDataArray)s to
    // the picklist for columns 2 and 3.
    // See @link(GetDataSets) and @link(GetInterpolators).
    procedure dgFieldsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    // @name displays the interpolator for the selected @link(TDataArray)
    // in column 3.
    procedure dgFieldsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    // @name checks the formula in edImportCriterion.
    // See @link(CheckImportCriterionFormula).
    procedure edImportCriterionExit(Sender: TObject);
    // @name initialized @classname.
    procedure FormCreate(Sender: TObject); override;
    // @name destroys @link(FGeometryFile).
    procedure FormDestroy(Sender: TObject);
    // @name changes the captions of @link(cbEnclosedCells),
    // @link(cbIntersectedCells), and @link(cbInterpolation).
    procedure rgEvaluatedAtClick(Sender: TObject);
    procedure cbImportObjectsClick(Sender: TObject);
    procedure cbImportGridClick(Sender: TObject);
    procedure comboBoundaryChoiceChange(Sender: TObject);
    procedure comboRealFieldChange(Sender: TObject);
    procedure comboBooleanFieldChange(Sender: TObject);
    procedure BoundaryGridBeforeDrawCell(Sender: TObject; ACol, ARow: Integer);
    procedure seWellIntervalsChange(Sender: TObject);
    procedure comboJoinObjectsChange(Sender: TObject);
    procedure seBoundaryTimeCountChange(Sender: TObject);
    procedure rdgBoundaryConditionsDistributeTextProgress(Sender: TObject;
      Position, Max: Integer);
    procedure btnElevFormulaEdit(Sender: TObject);
    procedure edZExit(Sender: TObject);
    procedure edHighZExit(Sender: TObject);
    procedure edLowZExit(Sender: TObject);
    procedure rgElevationCountClick(Sender: TObject);
    procedure comboDrainReturnLocationMethodChange(Sender: TObject);
  private
    FGeometryFileName: string;
    FIndexFileName: string;
    FDataBaseFileName: string;
    FAllowShapesToCombine: boolean;
    FShouldEnableImportGrid: Boolean;
    // @name stores the TRbwDataType of the fields in the Shapefile.
    FFieldTypes: array of TRbwDataType;
    // @name is the geometry file of the Shapefile.
    FGeometryFile: TShapefileGeometryReader;
    FRealFieldNames: TStringList;
    FIntegerFieldNames: TStringList;
    FBooleanFieldNames: TStringList;
    FStringFieldNames: TStringList;
    FRealFieldAndGlobalVariablesNames: TStringList;
    FRealFieldGlobalsAndDataSetsNames: TStringList;
    FShapeCount: integer;
    FFieldNumbers: TStringList;
    FNumPointsInCurrentShape: Integer;
    FInvalidParameterNames: TStringList;
    // @name checks for valid data in @link(dgFields).
    function CheckDataSets: boolean;
    // @name checks that AFormula is a valid formula.
    procedure CheckImportCriterionFormula(AFormula: string);
    // @name creates variables in Parser for each attribute in the shape file.
    procedure CreateVariables(Parser: TRbwParser);
    // @name stores in @link(dgFields).Columns[2].PickList the names
    // of the TDataSets that can be used with the parameter specified
    // in ARow of @link(dgFields).
    procedure GetDataSets(const ARow: integer);
    // @name stores in @link(dgFields).Columns[3].PickList the names
    // of the interpolators that can be used with th parameter in ARow.
    procedure GetInterpolators(const ARow: integer);
    // @name converts a latitude and longitude in degrees to a point
    // on @link(imageUtmZones).
    function LatLongToPoint(Long, Lat: double): TPoint;
    // @name converts a latitude and longitude in degrees to a UTM zone.
    // @name takes into account the non-regular UTM zones.
    function LatLongToUTM_Zone(const LongitudeDegrees, LatitudeDegrees: double):
      integer;
    // @name makes any required new data sets.
    procedure MakeNewDataSets(NewDataSets: TList);
    { Set the captions of @link(cbEnclosedCells), @link(cbIntersectedCells),
      and @link(cbInterpolation) based on @link(rgEvaluatedAt).ItemIndex.}
    procedure SetCheckBoxCaptions;
    // @name imports the Shapes into GoPhast.
    procedure SetData;
    // @name is used to display a progress indication when reeding the
    // Shapefile.
    procedure ShapefileProgress(Sender: TObject; FractionDone: double);
    // @name converts a latitude and longitude in degrees to a UTM zone
    // for the non-special UTM zones.  @seealso(LatLongToUTM_Zone).
    function SimpleLongToUTM_Zone(const LongitudeDegrees: double): integer;
    procedure ImportGrid(FieldNames: TStringList);
    procedure EnableOK;
    procedure InitializeBoundaryConditionControls;
    procedure AssignBoundary(AScreenObject: TScreenObject);
    procedure AssignAPhastSpecifiedHeadBoundary(AScreenObject: TScreenObject);
    procedure AssignAPhastBoundary(Boundary: TCustomInterpolatedBoundary);
    function GetRealFormulaFromText(const Text: string; DataSetsOK: boolean = True): string;
    function GetRealValueFromText(const FieldName: string): Extended;
    procedure AssignAPhastLeakyBoundary(AScreenObject: TScreenObject);
    procedure AssignAPhastRiverBoundary(AScreenObject: TScreenObject);
    function GetStringValueFromText(const FieldName: string): string;
    function GetBooleanValueFromText(FieldName: string): Boolean;
    procedure AssignAPhastWellBoundary(AScreenObject: TScreenObject);
    procedure EnableFeatureImport;
    procedure ImportModflowChdBoundary(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForCHD;
    procedure InitializeBoundaryControlsForGHB;
    procedure ImportModflowGhbBoundary(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForWEL;
    procedure ImportModflowWelBoundary(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForRIV;
    procedure ImportModflowRivPackage(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForDRN;
    procedure ImportModflowDrnPackage(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForRCH(Packages: TModflowPackages);
    function GetIntegerValueFromText(const FieldName: string): integer;
    procedure ImportModflowRchPackage(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForEVT(Packages: TModflowPackages);
    procedure ImportModflowEvtPackage(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForETS(Packages: TModflowPackages);
    procedure ImportModflowEtsPackage(AScreenObject: TScreenObject;
      Packages: TModflowPackages);
    procedure InitializeBoundaryControlsForHOB;
    procedure ImportModflowHobPackage(AScreenObject: TScreenObject);
    function GetFormulaInterpretation(combo: TComboBox): TFormulaInterpretation;
    procedure AssignColFeatureProperties;
    procedure EnableEvalAt;
    procedure AssignInterpolator(DataSet: TDataArray; Index: Integer;
      out NewProperties, OldProperties: TPhastDataSetStorage);
    procedure ChangeInterpolators(NewProperties, OldProperties: TList);
    procedure InitializeBoundaryControlsForSFR;
    procedure ImportModflowSfrPackage(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForLAK;
    procedure ImportModflowLakPackage(AScreenObject: TScreenObject);
    procedure CheckElevationFormula(Edit: TRbwEdit; AFormula: string);
    procedure InitializeBoundaryControlsForDRT;
    procedure ImportModflowDrtPackage(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForRES(Packages: TModflowPackages);
    procedure ImportModflowResPackage(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForHFB;
    procedure ImportModflowHfbPackage(AScreenObject: TScreenObject);
    procedure InitializeBoundaryControlsForUZF;
    procedure ImportModflowUzfPackage(AScreenObject: TScreenObject);
    function GetIntegerFormulaFromText(const text: string; DataSetsOK: boolean = True): string;
    procedure CreateDataSetVariables(Parser: TRbwParser);
    function DataArrayOrientationOK(DataArray: TDataArray): boolean;
    procedure AddModflowPackageToImportChoices(APackage: TModflowPackageSelection);
    procedure InitializeBoundaryControlsForMnw2;
    procedure ImportModflowMnw2Package(AScreenObject: TScreenObject);
    procedure GetTransientParameter(var Param: TModflowTransientListParameter;
      var ParameterName: string; ParameterColumn: Integer; Row: Integer);
    procedure GetNewBoundaryItem(var AnItem: TCustomModflowBoundaryItem;
      const ParameterName: string; var Param: TModflowTransientListParameter;
      var ParamItem: TModflowParamItem; Boundary: TModflowParamBoundary);
    procedure AddParameterNamesToPickList(ParameterType: TParameterType;
      ParameterColumn: Integer);
    { Private declarations }
  public
    // @name returns @true if the Shapefile is selected.
    function GetData: boolean;
    { Public declarations }
  end;

  TFieldNumStorage = class(TObject)
    FXBase: TXBase;
    FieldNumber: integer;
    Formula: string;
    RealValue: double;
    IntValue: integer;
    Cached: boolean;
    function GetRealValue: double;
    function GetIntValue: integer;
    Constructor Create(XBase: TXBase);
    function RealFormula: string;
    function IntFormula: string;
  end;

implementation

uses Math, Contnrs , frmGoPhastUnit, GoPhastTypes, frmProgressUnit,
  frmDataSetsUnits, ModelMuseUtilities, frmShowHideObjectsUnit, CoordinateConversionUnit,
  frmFormulaUnit, FastGEO, RealListUnit, ValueArrayStorageUnit, GIS_Functions,
  PhastModelUnit, TimeUnit,
  ModflowConstantHeadBoundaryUnit, ModflowGhbUnit, ModflowWellUnit, 
  ModflowRivUnit, ModflowDrnUnit, ModflowRchUnit, ModflowEvtUnit, 
  ModflowEtsUnit, ModflowHobUnit, ModflowSfrReachUnit,
  ModflowSfrParamIcalcUnit, ModflowSfrFlows, ModflowSfrChannelUnit,
  ModflowSfrEquationUnit, ModflowSfrSegment, ModflowSfrUnit, ModflowTimeUnit, 
  ModflowLakUnit, ModflowDrtUnit, ModflowResUnit, ModflowHfbUnit, 
  ModflowUzfUnit, GlobalVariablesUnit, frameScreenObjectMNW2Unit,
  ModflowMnw2Unit, frmErrorsAndWarningsUnit;

resourcestring
  StrParameterName = 'Parameter name';

{$R *.dfm}

type
  TFieldGridColumns = (fgcAttributes, fgcImport, fgcDataSet, fgcInterpolator);

  TValueRecord = record
    StringData: string;
    case DataType: TRbwDataType of
      rdtDouble: (RealData: double);
      rdtInteger: (IntData: integer);
      rdtBoolean: (BoolData: boolean);
  end;

  TValueObject = class(TObject)
    Data: TValueRecord;
  end;

  TValueBool = class(TBooleanVariable)
  public
    function Decompile: string; override;
  end;

  TValueInt = class(TIntegerVariable)
  public
    function Decompile: string; override;
  end;

  TValueReal = class(TRealVariable)
  public
    function Decompile: string; override;
  end;

  TValueStr = class(TStringVariable)
    function Decompile: string; override;
  end;

  TDecompileType = (dcNormal, dcValue);

var
  GlobalDecompileType: TDecompileType = dcNormal;

const
  StrAttribute = 'Attribute';
  StrAttributes = 'Attributes';

function ConvertPoint(const ShapePoint: TShapePoint): TPoint2D;
begin
  result.X := ShapePoint.X;
  result.Y := ShapePoint.Y;
end;

function DistanceOriginToLine(const StartingPoint, EndingPoint: TShapePoint;
  out DistanceToOrigin: double): boolean;
const
  Origin: TPoint2D = (X: 0; Y: 0);
var
  LineMag: double;
  U: double;
  Intersection: TPoint2D;
  StartPoint, EndPoint: TPoint2D;
begin
  StartPoint := ConvertPoint(StartingPoint);
  EndPoint := ConvertPoint(EndingPoint);
  LineMag := Distance( EndPoint, StartPoint );
  result := LineMag > 0;
  if not result then
  begin
    Exit;
  end;

  U := ( ( ( Origin.X - StartPoint.X ) * ( EndPoint.X - StartPoint.X ) ) +
      ( ( Origin.Y - StartPoint.Y ) * ( EndPoint.Y - StartPoint.Y ) )  ) /
      ( Sqr(LineMag) );

  Intersection.X := StartPoint.X + U * ( EndPoint.X - StartPoint.X );
  Intersection.Y := StartPoint.Y + U * ( EndPoint.Y - StartPoint.Y );

  DistanceToOrigin := Distance( Origin, Intersection );
end;

function FieldToVarName(AString: string): string;
var
  Index: Integer;
begin
  result := AString;
  if Length(result) > 0 then
  begin
    if not (result[1] in ['_', 'A'..'Z', 'a'..'z']) then
    result := '_' + result;
    for Index := 2 to Length(result) do
    begin
      if not (result[Index] in ['_', 'A'..'Z', 'a'..'z', '0'..'9']) then
      begin
        result[Index] := '_';
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.FormCreate(Sender: TObject);
begin
  inherited;
  FFieldNumbers := TStringList.Create;
  FFieldNumbers.CaseSensitive := False;
  FFieldNumbers.Sorted := True;

  FRealFieldNames := TStringList.Create;
  FIntegerFieldNames := TStringList.Create;
  FBooleanFieldNames := TStringList.Create;
  FStringFieldNames := TStringList.Create;
  FRealFieldAndGlobalVariablesNames := TStringList.Create;
  FRealFieldGlobalsAndDataSetsNames := TStringList.Create;

  cbEnclosedCellsClick(nil);
  SetCheckBoxCaptions;
  comboEllipsoid.ItemIndex := 2;
  comboEllipsoidChange(nil);

  pcImportShape.ActivePageIndex := 0;

  dgFields.DefaultRowHeight := dgFields.Canvas.TextHeight('Fields')+4;
  dgFields.Cells[Ord(fgcAttributes), 0] := StrAttribute;
  dgFields.Cells[Ord(fgcImport), 0] := 'Import';
  dgFields.Cells[Ord(fgcDataSet), 0] := 'Data Set';
  dgFields.Cells[Ord(fgcInterpolator), 0] := 'Interpolation';
  dgFields.ColWidths[Ord(fgcDataSet)] := 120;

  pcSFR.ActivePageIndex := 0;

  InitializeBoundaryConditionControls;
  EnableFeatureImport;
end;

procedure TfrmImportShapefile.FormDestroy(Sender: TObject);
var
  Index: Integer;
begin
  inherited;
  FRealFieldGlobalsAndDataSetsNames.Free;
  FRealFieldAndGlobalVariablesNames.Free;
  FRealFieldNames.Free;
  FIntegerFieldNames.Free;
  FBooleanFieldNames.Free;
  FStringFieldNames.Free;
  FGeometryFile.Free;
  for Index := 0 to FFieldNumbers.Count - 1 do
  begin
    FFieldNumbers.Objects[Index].Free;
  end;
  FFieldNumbers.Free;
end;



function TfrmImportShapefile.GetData: boolean;
var
  FilesOK: boolean;
  Index: integer;
  ValidFields: TStringList;
  ValidIndicies: TIntegerList;
  ShapeIndex: Integer;
  Shape: TShapeObject;
  VarIndex: Integer;
  Variable: TGlobalVariable;
  DSIndex: Integer;
  DataArray: TDataArray;
begin
  inherited;
  rgEvaluatedAt.Items[Ord(eaBlocks)] := EvalAtToString(eaBlocks,
    frmGoPhast.PhastModel.ModelSelection, True, True);
  rgEvaluatedAt.Items[Ord(eaNodes)] := EvalAtToString(eaNodes,
    frmGoPhast.PhastModel.ModelSelection, True, True);
  EnableEvalAt;

  FShouldEnableImportGrid := False;
  FAllowShapesToCombine := False;
  FilesOK := False;
  try
    result := OpenDialogShape.Execute;
    if result then
    begin
      FGeometryFileName := OpenDialogShape.FileName;
      Caption := Caption + ' - ' + FGeometryFileName;
      FIndexFileName := ChangeFileExt(FGeometryFileName, '.shx');
      FDataBaseFileName := ChangeFileExt(FGeometryFileName, '.dbf');
      if not FileExists(FGeometryFileName) then
      begin
        Beep;
        MessageDlg('The ".shp" file "' + FGeometryFileName
          + '" does not exist.', mtError, [mbOK], 0);
        Exit;
      end;
      if not FileExists(FDataBaseFileName) then
      begin
        Beep;
        if MessageDlg('The ".dbf" file "' + FDataBaseFileName
          + '" does not exist.  Do you want to just import the geometry '
          + 'of the shapes in the shape file',
          mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
        begin
          Exit;
        end;
        FDataBaseFileName := '';
      end;
      FGeometryFile := TShapefileGeometryReader.Create;
      frmProgress.pbProgress.Max := 1000;
      frmProgress.pbProgress.Position := 0;
      frmProgress.Caption := 'Reading Shape Geometry File';
      frmProgress.PopupParent := self;
      frmProgress.ProgressLabelCaption := 'Reading shape 1';
      FShapeCount := 0;
      frmProgress.Show;
      try
        FGeometryFile.OnProgress := ShapefileProgress;
        FGeometryFile.ReadFromFile(FGeometryFileName, FIndexFileName);
      finally
        frmProgress.Hide;
      end;
      lblNumShapes.Caption := 'Number of shapes = '
        + IntToStrFormatted(FGeometryFile.Count);

      cbImportObjectsClick(nil);

      if FDataBaseFileName <> '' then
      begin
        xbShapeDataBase.FileName := FDataBaseFileName;
        xbShapeDataBase.Active := True;
        xbShapeDataBase.GotoBOF;
        ValidFields := TStringList.Create;
        ValidIndicies := TIntegerList.Create;
        try
          for Index := 1 to xbShapeDataBase.FieldCount do
          begin
            if xbShapeDataBase.GetFieldType(Index)
              in [xbfChar, xbfNumber, xbfLogic] then
            begin
              ValidIndicies.Add(Index);
              ValidFields.Add(xbShapeDataBase.GetFieldName(Index))
            end;
          end;
          if (ValidFields.Count = 0) and (MessageDlg('None of the fields in '
            + FDataBaseFileName
            + '" can be imported.  Do you want to just import the geometry '
            + 'of the shapes in the shape file',
            mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
          begin
            Exit;
          end;

          SetLength(FFieldTypes, ValidIndicies.Count);
          for Index := 0 to ValidIndicies.Count - 1 do
          begin
            case xbShapeDataBase.GetFieldType(ValidIndicies[Index]) of
              xbfChar:
                begin
                  FStringFieldNames.Add(ValidFields[Index]);
                  FFieldTypes[Index] := rdtString;
                end;
              xbfNumber:
                begin
                  if xbShapeDataBase.GetFieldDecimals(
                    ValidIndicies[Index]) = 0 then
                  begin
                    FFieldTypes[Index] := rdtInteger;
                    FIntegerFieldNames.Add(ValidFields[Index]);
                    FRealFieldNames.Add(ValidFields[Index]);
                  end
                  else
                  begin
                    FFieldTypes[Index] := rdtDouble;
                    FRealFieldNames.Add(ValidFields[Index]);
                  end;
                end;
              xbfLogic:
                begin
                  FFieldTypes[Index] := rdtBoolean;
                  FBooleanFieldNames.Add(ValidFields[Index]);
                end;
            else
              Assert(False);
            end;
          end;
          FRealFieldAndGlobalVariablesNames.Assign(FRealFieldNames);
          for VarIndex := 0 to frmGoPhast.PhastModel.GlobalVariables.Count - 1 do
          begin
            Variable := frmGoPhast.PhastModel.GlobalVariables[VarIndex];
            if Variable.Format in [rdtDouble, rdtInteger] then
            begin
              FRealFieldAndGlobalVariablesNames.Add(Variable.Name)
            end;
          end;
          FRealFieldGlobalsAndDataSetsNames.Assign(FRealFieldAndGlobalVariablesNames);
          for DSIndex := 0 to frmGoPhast.PhastModel.DataSetCount - 1 do
          begin
            DataArray := frmGoPhast.PhastModel.DataSets[DSIndex];
            if DataArray.DataType in [rdtDouble, rdtInteger] then
            begin
              FRealFieldGlobalsAndDataSetsNames.Add(DataArray.Name)
            end;
          end;

          WellDescription.Items := FStringFieldNames;
          comboWellDiameter.Items := FRealFieldNames;
          comboWellLandSurfaceDatum.Items := FRealFieldNames;
          comboWellPumpAllocation.Items := FBooleanFieldNames;
          comboWellIntervalStyle.Items.AddStrings(FStringFieldNames);
          comboSolutionType.Items.AddStrings(FStringFieldNames);
          comboLeakyHydraulicConductivity.Items := FRealFieldNames;
          comboLeakyThickness.Items := FRealFieldNames;
          comboRiverDescripton.Items := FStringFieldNames;
          comboRiverHydraulicConductivity.Items := FRealFieldNames;
          comboRiverWidth.Items := FRealFieldNames;
          comboRiverDepth.Items := FRealFieldNames;
          comboRiverBedThickness.Items := FRealFieldNames;
          dgWellElevations.Columns[1].PickList := FRealFieldNames;
          dgWellElevations.Columns[2].PickList := FRealFieldNames;
          rdgBoundaryConditions.Columns[0].PickList := FRealFieldNames;
          rdgBoundaryConditions.Columns[1].PickList := FRealFieldNames;
          rdgBoundaryConditions.Columns[2].PickList := FIntegerFieldNames;

          dgFields.RowCount := ValidFields.Count + 1;
          for Index := 0 to ValidFields.Count - 1 do
          begin
            dgFields.Cells[Ord(fgcAttributes), Index + 1] := ValidFields[Index];
            dgFields.Cells[Ord(fgcDataSet), Index + 1] := rsNewDataSet;
            dgFields.Cells[Ord(fgcInterpolator), Index + 1] := 'None';
          end;
          FilesOK := True;
          FShouldEnableImportGrid := cbEnclosedCells.Enabled;
          if FShouldEnableImportGrid then
          begin
            FShouldEnableImportGrid := (ValidFields.IndexOf('X_INDEX') >= 0)
              and (ValidFields.IndexOf('Y_INDEX') >= 0);
            if FShouldEnableImportGrid then
            begin
              for ShapeIndex := 0 to FGeometryFile.Count - 1 do
              begin
                Shape := FGeometryFile[ShapeIndex];
                FShouldEnableImportGrid := (Shape.FNumPoints = 5)
                  and (Shape.FNumParts = 1);
                if not FShouldEnableImportGrid then
                begin
                  break;
                end;
              end;
            end;
          end;
          FAllowShapesToCombine := True;
          for ShapeIndex := 0 to FGeometryFile.Count - 1 do
          begin
            Shape := FGeometryFile[ShapeIndex];
            FAllowShapesToCombine := Shape.FNumParts <= 1;
            if not FAllowShapesToCombine then
            begin
              break;
            end;

          end;
        finally
          ValidFields.Free;
          ValidIndicies.Free;
        end;
      end;
      cbImportGrid.Enabled := FShouldEnableImportGrid;
      if not FShouldEnableImportGrid then
      begin
        cbImportGrid.Checked := False;
      end;
      comboJoinObjects.Enabled := FAllowShapesToCombine;
    end;
  finally
    if not FilesOK then
    begin
      Close;
    end;
  end;
end;

procedure TfrmImportShapefile.AddParameterNamesToPickList(ParameterType: TParameterType; ParameterColumn: Integer);
var
  ParamIndex: Integer;
  Param: TModflowTransientListParameter;
begin
  for ParamIndex := 0 to frmGoPhast.PhastModel.ModflowTransientParameters.Count - 1 do
  begin
    Param := frmGoPhast.PhastModel.ModflowTransientParameters[ParamIndex];
    if Param.ParameterType = ParameterType then
    begin
      rdgBoundaryConditions.Columns[ParameterColumn].PickList.Add(Param.ParameterName);
    end;
  end;
end;

procedure TfrmImportShapefile.GetNewBoundaryItem(
  var AnItem: TCustomModflowBoundaryItem; const ParameterName: string;
  var Param: TModflowTransientListParameter;
  var ParamItem: TModflowParamItem; Boundary: TModflowParamBoundary);

begin
  if Param = nil then
  begin
    AnItem := Boundary.Values.Add
      as TCustomModflowBoundaryItem;
  end
  else
  begin
    if ParamItem = nil then
    begin
      ParamItem := Boundary.Parameters.Add
        as TModflowParamItem;
      ParamItem.Param.ParamName := ParameterName;
    end;
    AnItem := ParamItem.Param.Add as TCustomModflowBoundaryItem;
  end;
end;

procedure TfrmImportShapefile.GetTransientParameter(
  var Param: TModflowTransientListParameter; var ParameterName: string;
  ParameterColumn: Integer; Row: Integer);
begin
  ParameterName := GetStringValueFromText(
    rdgBoundaryConditions.Cells[ParameterColumn, Row]);
  ParameterName := StringReplace(ParameterName, '"', '',
    [rfReplaceAll, rfIgnoreCase]);
  if ParameterName = '' then
  begin
    Param := nil;
  end
  else
  begin
    Param := frmGoPhast.PhastModel.
      ModflowTransientParameters.GetParamByName(ParameterName);
    if Param = nil then
    begin
      FInvalidParameterNames.Add(ParameterName);
    end;
  end;
end;

procedure TfrmImportShapefile.AddModflowPackageToImportChoices(APackage: TModflowPackageSelection);
begin
  if APackage.IsSelected then
  begin
    comboBoundaryChoice.Items.AddObject(APackage.PackageIdentifier, APackage);
  end;
end;

procedure TfrmImportShapefile.CreateDataSetVariables(Parser: TRbwParser);
var
  DataArray: TDataArray;
  Index: Integer;
begin
  for Index := 0 to frmGoPhast.PhastModel.DataSetCount - 1 do
  begin
    DataArray := frmGoPhast.PhastModel.DataSets[Index];
    if (DataArray.Orientation = dsoTop) and (Parser.IndexOfVariable(DataArray.Name) < 0) then
    begin
      case DataArray.DataType of
        rdtDouble:
          Parser.CreateVariable(DataArray.Name, 'Data Sets', 0, TRealVariable);
        rdtInteger:
          Parser.CreateVariable(DataArray.Name, 'Data Sets', 0, TIntegerVariable);
        rdtBoolean:
          Parser.CreateVariable(DataArray.Name, 'Data Sets', False, TBooleanVariable);
        rdtString:
          Parser.CreateVariable(DataArray.Name, 'Data Sets', '', TStringVariable);
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.ChangeInterpolators(NewProperties,
  OldProperties: TList);
var
  DataSet: TDataArray;
  DataSetIndex: Integer;
  Index: Integer;
  NewProp, OldProp: TPhastDataSetStorage;
begin
  for Index := 1 to dgFields.RowCount - 1 do
  begin
    if dgFields.Checked[Ord(fgcImport), Index]
      and (dgFields.Cells[Ord(fgcDataSet), Index] <> rsNewDataSet) then
    begin
      DataSetIndex := dgFields.Columns[Ord(fgcDataSet)].PickList.
        IndexOf(dgFields.Cells[Ord(fgcDataSet), Index]);
      if DataSetIndex >= 0 then
      begin
        DataSet := dgFields.Columns[Ord(fgcDataSet)].PickList.
          Objects[DataSetIndex] as TDataArray;
        AssignInterpolator(DataSet, Index, NewProp, OldProp);
        if NewProp = nil then
        begin
          Assert(OldProp = nil);
        end
        else
        begin
          Assert(OldProp <> nil);
          NewProperties.Add(NewProp);
          OldProperties.Add(OldProp);
        end;
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.AssignInterpolator(DataSet: TDataArray;
  Index: Integer; out NewProperties, OldProperties: TPhastDataSetStorage);
var
  Interpolator: TCustom2DInterpolater;
  AType: TInterpolatorType;
  InterpolatorIndex: Integer;
begin
  NewProperties := nil;
  OldProperties := nil;
  if dgFields.Cells[Ord(fgcInterpolator), Index] <> '' then
  begin
    InterpolatorIndex := dgFields.Columns[Ord(fgcInterpolator)].PickList.IndexOf(dgFields.Cells[Ord(fgcInterpolator), Index]);
    if InterpolatorIndex < 0 then
    begin
      if DataSet.TwoDInterpolator <> nil then
      begin
        OldProperties := TPhastDataSetStorage.Create;
        OldProperties.Assign(DataSet);
      end;
      DataSet.TwoDInterpolator := nil;
      if OldProperties <> nil then
      begin
        NewProperties := TPhastDataSetStorage.Create;
        NewProperties.Assign(DataSet);
      end;
    end
    else
    begin
      AType := TInterpolatorType(dgFields.Columns[Ord(fgcInterpolator)].PickList.Objects[InterpolatorIndex]);
      if AType = nil then
      begin
        if DataSet.TwoDInterpolator <> nil then
        begin
          OldProperties := TPhastDataSetStorage.Create;
          OldProperties.Assign(DataSet);
        end;
        DataSet.TwoDInterpolator := nil;
        if OldProperties <> nil then
        begin
          NewProperties := TPhastDataSetStorage.Create;
          NewProperties.Assign(DataSet);
        end;
      end
      else if (DataSet.TwoDInterpolator = nil)
        or not (DataSet.TwoDInterpolator is AType) then
      begin
        Interpolator := AType.Create(nil);
        try
          OldProperties := TPhastDataSetStorage.Create;
          OldProperties.Assign(DataSet);
          DataSet.TwoDInterpolator := Interpolator;
          NewProperties := TPhastDataSetStorage.Create;
          NewProperties.Assign(DataSet);
        finally
          Interpolator.Free;
        end;
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.EnableEvalAt;
begin
  rgEvaluatedAt.Enabled := cbImportObjects.Checked
    and (frmGoPhast.PhastModel.ModelSelection = msPhast);
end;

procedure TfrmImportShapefile.AssignColFeatureProperties;
var
  Index: Integer;
begin
  for Index := 0 to rdgBoundaryConditions.ColCount - 1 do
  begin
    rdgBoundaryConditions.Columns[Index].AutoAdjustColWidths := True;
    rdgBoundaryConditions.Columns[Index].AutoAdjustRowHeights := True;
    rdgBoundaryConditions.Columns[Index].WordWrapCaptions := True;
  end;
end;

function TfrmImportShapefile.GetFormulaInterpretation(combo: TComboBox): TFormulaInterpretation;
begin
  if combo.ItemIndex in [Ord(Low(TFormulaInterpretation))..
    Ord(High(TFormulaInterpretation))] then
  begin
    result := TFormulaInterpretation(combo.ItemIndex);
  end
  else
  begin
    Text := UpperCase(GetStringValueFromText(combo.Text));
    if Text = 'SPECIFIC' then
    begin
      result := fiSpecific;
    end
    else if Text = 'DIRECT' then
    begin
      result := fiDirect;
    end
    else if Text = 'TOTAL' then
    begin
      result := fiTotal;
    end
    else
    begin
      result := fiDirect;
    end;
  end;
end;

procedure TfrmImportShapefile.ImportModflowMnw2Package(
  AScreenObject: TScreenObject);
var
  Boundary: TMnw2Boundary;
  Index: Integer;
  UseRow: Boolean;
  StartTime: double;
  EndTime: double;
  TimeItem: TMnw2TimeItem;
  LossTypeString: string;
  SpatialItem: TMnw2SpatialItem;
  Row: Integer;
  RowIndex: Integer;
  TextToUse: string;
  QCUT: Integer;
begin
  AScreenObject.CreateMnw2Boundary;
  Boundary := AScreenObject.ModflowMnw2Boundary;
  SpatialItem := Boundary.Values.Add as TMnw2SpatialItem;

  // Basic tab
  Boundary.WellID := GetStringValueFromText(comboMnw2WellId.Text);

  LossTypeString := GetStringValueFromText(comboMnw2LossType.Text);
  LossTypeString := UpperCase(Trim(LossTypeString));
  if LossTypeString = 'NONE' then
  begin
    Boundary.LossType := mltNone;
  end
  else if LossTypeString = 'THIEM' then
  begin
    Boundary.LossType := mltThiem;
  end
  else if LossTypeString = 'SKIN' then
  begin
    Boundary.LossType := mltSkin;
  end
  else if LossTypeString = 'GENERAL' then
  begin
    Boundary.LossType := mltEquation;
  end
  else if LossTypeString = 'SPECIFYCWC' then
  begin
    Boundary.LossType := mtlSpecify;
  end;

  Boundary.SpecifyPump := GetIntegerValueFromText(comboSpecifyPump.Text) <> 0;

  if Boundary.SpecifyPump then
  begin
    if FNumPointsInCurrentShape = 1 then
    begin
      Boundary.PumpElevation := GetRealValueFromText(comboZPump.Text);
    end
    else
    begin
      Boundary.PumpCellTarget.TargetType := ttLocation;
      Boundary.PumpCellTarget.TargetLocation.X :=
        GetRealValueFromText(comboMnw2PumplocX.Text);
      Boundary.PumpCellTarget.TargetLocation.Y :=
        GetRealValueFromText(comboMnw2PumplocY.Text);
      Boundary.PumpCellTarget.TargetLocation.Z :=
        GetRealValueFromText(comboMnw2PumplocZ.Text);
    end;
  end;

  Boundary.ConstrainPumping :=
    GetIntegerValueFromText(comboConstrainPumping.Text) <> 0;
  Boundary.PartialPenetrationCorrection :=
    GetIntegerValueFromText(comboPartialPenetrationFlag.Text) <> 0;

  SpatialItem.PartialPenetration :=
    GetRealFormulaFromText(comboPartialPenetration.Text);

  Boundary.AdjustPumping :=
    GetIntegerValueFromText(comboPumpCap.Text) <> 0;

  // Loss Controls tab
  case Boundary.LossType of
    mltNone: ; // do nothing
    mltThiem:
      begin
        SpatialItem.WellRadius :=
          GetRealFormulaFromText(comboWellRadius.Text);
      end;
    mltSkin:
      begin
        SpatialItem.WellRadius :=
          GetRealFormulaFromText(comboWellRadius.Text);
        SpatialItem.SkinRadius :=
          GetRealFormulaFromText(comboSkinRadius.Text);
        SpatialItem.SkinK :=
          GetRealFormulaFromText(comboKSkin.Text);
      end;
    mltEquation:
      begin
        SpatialItem.WellRadius :=
          GetRealFormulaFromText(comboWellRadius.Text);
        SpatialItem.B :=
          GetRealFormulaFromText(comboBCoefficient.Text);
        SpatialItem.C :=
          GetRealFormulaFromText(comboCCoefficient.Text);
        SpatialItem.P :=
          GetRealFormulaFromText(comboPCoefficient.Text);
      end;
    mtlSpecify: 
      begin
        SpatialItem.CellToWellConductance :=
          GetRealFormulaFromText(comboCellToWellConductance.Text);
      end;
    else Assert(false);
  end;

  // Discharge Adjustment tab
  if Boundary.AdjustPumping then
  begin
    Boundary.ReferenceHead := GetRealValueFromText(comboReferenceHead.Text);
    Boundary.MaximumLift := GetRealValueFromText(comboLiftQ0.Text);
    Boundary.LiftAtMaxRate := GetRealValueFromText(comboLiftQMax.Text);
    Boundary.WellTolerance := GetRealValueFromText(comboWellTolerance.Text);
  end;

  // Time data
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    Row := Index + 1;
    UseRow := (rdgBoundaryConditions.Cells[Ord(mtcStartTime), Row] <> '')
      and (rdgBoundaryConditions.Cells[Ord(mtcEndTime), Row] <> '')
      and (rdgBoundaryConditions.Cells[Ord(mtcPumpingRate), Row] <> '');
    if UseRow then
    begin
      StartTime := GetRealValueFromText(
        rdgBoundaryConditions.Cells[Ord(mtcStartTime), Row]);
      EndTime := GetRealValueFromText(
        rdgBoundaryConditions.Cells[Ord(mtcEndTime), Row]);

      TimeItem := Boundary.TimeValues.Add as TMnw2TimeItem;
      TimeItem.StartTime := StartTime;
      TimeItem.EndTime := EndTime;
      TimeItem.PumpingRate := GetRealFormulaFromText(
        rdgBoundaryConditions.Cells[Ord(mtcPumpingRate), Row]);

      if Boundary.AdjustPumping then
      begin
        TextToUse := '';
        for RowIndex := Row downto 1 do
        begin
          if rdgBoundaryConditions.Cells[
            Ord(mtcMultiplier), RowIndex] <> '' then
          begin
            TextToUse := rdgBoundaryConditions.
              Cells[Ord(mtcMultiplier), RowIndex];
            break;
          end;
        end;
        if TextToUse <> '' then
        begin
          TimeItem.HeadCapacityMultiplier := GetRealFormulaFromText(TextToUse);
        end;
      end;

      if Boundary.ConstrainPumping then
      begin
        TextToUse := '';
        for RowIndex := Row downto 1 do
        begin
          if rdgBoundaryConditions.Cells[
            Ord(mtcLimitingWaterLevel), RowIndex] <> '' then
          begin
            TextToUse := rdgBoundaryConditions.
              Cells[Ord(mtcLimitingWaterLevel), RowIndex];
            break;
          end;
        end;
        if TextToUse <> '' then
        begin
          TimeItem.LimitingWaterLevel := GetRealFormulaFromText(TextToUse);
        end;

        TextToUse := '';
        for RowIndex := Row downto 1 do
        begin
          if rdgBoundaryConditions.Cells[
            Ord(mtcLimitMethod), RowIndex] <> '' then
          begin
            TextToUse := rdgBoundaryConditions.
              Cells[Ord(mtcLimitMethod), RowIndex];
            break;
          end;
        end;
        if TextToUse <> '' then
        begin
          QCUT := GetIntegerValueFromText(TextToUse);
          if QCUT = 0 then
          begin
            TimeItem.LimitMethod := mlmNoMinimum;
          end
          else if QCUT > 0 then
          begin
            TimeItem.LimitMethod := mlmRate;
          end
          else
          begin
            TimeItem.LimitMethod := mlmFraction;
          end;
        end;

        if TimeItem.LimitMethod <> mlmNoMinimum then
        begin
          TextToUse := '';
          for RowIndex := Row downto 1 do
          begin
            if rdgBoundaryConditions.Cells[
              Ord(mtcMinRate), RowIndex] <> '' then
            begin
              TextToUse := rdgBoundaryConditions.
                Cells[Ord(mtcMinRate), RowIndex];
              break;
            end;
          end;
          if TextToUse <> '' then
          begin
            TimeItem.InactivationPumpingRate :=
              GetRealFormulaFromText(TextToUse);
          end;

          TextToUse := '';
          for RowIndex := Row downto 1 do
          begin
            if rdgBoundaryConditions.Cells[
              Ord(mtcMaxRate), RowIndex] <> '' then
            begin
              TextToUse := rdgBoundaryConditions.
                Cells[Ord(mtcMaxRate), RowIndex];
              break;
            end;
          end;
          if TextToUse <> '' then
          begin
            TimeItem.ReactivationPumpingRate :=
              GetRealFormulaFromText(TextToUse);
          end;
        end;
      end;
    end;
  end;

end;

procedure TfrmImportShapefile.ImportModflowUzfPackage(
  AScreenObject: TScreenObject);
var
  Boundary: TUzfBoundary;
  Index: Integer;
  UseRow: Boolean;
  StartTime: Extended;
  EndTime: Extended;
  Item: TRchItem;
  EvtItem: TEvtItem;
  ExtinctItem: TUzfExtinctDepthItem;
  WaterContentItem: TUzfWaterContentItem;
begin
  AScreenObject.CreateUzfBoundary;
  Boundary := AScreenObject.ModflowUzfBoundary;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(ucStartTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(ucEndTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(ucInfiltration), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(ucEvapRate), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(ucExtinctDepth), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(ucEndTime), Index + 1] <> '');
    if UseRow then
    begin
      StartTime := GetRealValueFromText(
        rdgBoundaryConditions.Cells[Ord(scStartTime), Index + 1]);
      EndTime := GetRealValueFromText(
        rdgBoundaryConditions.Cells[Ord(scEndTime), Index + 1]);

      Item := Boundary.Values.Add as TRchItem;
      Item.StartTime := StartTime;
      Item.EndTime := EndTime;
      Item.RechargeRate := GetRealFormulaFromText(
        rdgBoundaryConditions.Cells[Ord(ucInfiltration), Index + 1]);

      EvtItem := Boundary.EvapotranspirationDemand.Add as TEvtItem;
      EvtItem.StartTime := StartTime;
      EvtItem.EndTime := EndTime;
      EvtItem.EvapotranspirationRate := GetRealFormulaFromText(
        rdgBoundaryConditions.Cells[Ord(ucEvapRate), Index + 1]);

      ExtinctItem := Boundary.ExtinctionDepth.Add as TUzfExtinctDepthItem;
      ExtinctItem.StartTime := StartTime;
      ExtinctItem.EndTime := EndTime;
      ExtinctItem.UzfExtinctDepth := GetRealFormulaFromText(
        rdgBoundaryConditions.Cells[Ord(ucExtinctDepth), Index + 1]);

      WaterContentItem := Boundary.WaterContent.Add as TUzfWaterContentItem;
      WaterContentItem.StartTime := StartTime;
      WaterContentItem.EndTime := EndTime;
      WaterContentItem.UzfWaterContent := GetRealFormulaFromText(
        rdgBoundaryConditions.Cells[Ord(ucExtinctWaterContent), Index + 1]);
    end;
  end;
end;

procedure TfrmImportShapefile.ImportModflowHfbPackage(
  AScreenObject: TScreenObject);
var
  Boundary: THfbBoundary;
begin
  AScreenObject.CreateHfbBoundary;
  Boundary := AScreenObject.ModflowHfbBoundary;
  Boundary.IsUsed := True;
  Boundary.HydraulicConductivityFormula :=
    GetRealFormulaFromText(comboHfbHydCond.Text);
  Boundary.ThicknessFormula :=
    GetRealFormulaFromText(comboHfbThickness.Text);
  Boundary.AdjustmentMethod := TAdjustmentMethod(rgAngleAdjustment.ItemIndex);
end;

procedure TfrmImportShapefile.ImportModflowLakPackage(AScreenObject: TScreenObject);
var
  Index: Integer;
  UseRow: Boolean;
  Item: TLakItem;
  StartTime: Extended;
  EndTime: Extended;
  Boundary: TLakBoundary;
  AValue: string;
begin
  AScreenObject.CreateLakBoundary;
  Boundary := AScreenObject.ModflowLakBoundary;

  Boundary.InitialStage :=
    GetRealValueFromText(comboInitialStage.Text);
  Boundary.CenterLake :=
    GetIntegerValueFromText(comboLakeID.Text);
  Boundary.Sill :=
    GetRealValueFromText(comboSill.Text);
  Boundary.LakeID :=
    GetIntegerValueFromText(comboLakeID.Text);

  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[0, Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[1, Index + 1] <> '');
    if UseRow then
    begin
      StartTime := GetRealValueFromText(
        rdgBoundaryConditions.Cells[Ord(scStartTime), Index + 1]);
      EndTime := GetRealValueFromText(
        rdgBoundaryConditions.Cells[Ord(scEndTime), Index + 1]);

      Item := Boundary.Values.Add as TLakItem;
      Item.StartTime := StartTime;
      Item.EndTime := EndTime;

      AValue := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
        Ord(lcMinStage), Index + 1]);
      Item.MinimumStage := AValue;

      AValue := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
        Ord(lcMaxStage), Index + 1]);
      Item.MaximumStage := AValue;

      AValue := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
        Ord(lcPrecip), Index + 1]);
      Item.Precipitation := AValue;

      AValue := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
        Ord(lcEvap), Index + 1]);
      Item.Evaporation := AValue;

      AValue := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
        Ord(lcRunoff), Index + 1]);
      Item.OverlandRunoff := AValue;

      AValue := GetRealFormulaFromText(rdgBoundaryConditions.Cells[
        Ord(lcWithdrawl), Index + 1]);
      Item.Withdrawal := AValue;
    end;
  end;
end;

procedure TfrmImportShapefile.ImportModflowSfrPackage(AScreenObject: TScreenObject);
var
  Index: Integer;
  UseRow: Boolean;
  Item: TSfrItem;
  IcalcItem: TSfrParamIcalcItem;
  StartTime: Extended;
  EndTime: Extended;
  SegmentFlowItem: TSfrSegmentFlowItem;
  ChannelItem: TSfrChannelItem;
  EqItem: TSfrEquationItem;
  SegItem: TSfrSegmentItem;
  First: boolean;
  Boundary: TSfrBoundary;
  IPrior: integer;
  RCHLEN: string;
  FieldNumber: Integer;
  Value: Extended;
  ICalc: Integer;
  InitializeGrid: boolean;
  ColIndex: Integer;
  CellText: string;
  CachedPosition: Integer;
  FieldStorage: TFieldNumStorage;
begin
  First := True;
  AScreenObject.CreateSfrBoundary;
  Boundary := AScreenObject.ModflowSfrBoundary;
  InitializeGrid := False;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[0, Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[1, Index + 1] <> '');
    if UseRow then
    begin
      InitializeGrid := rdgBoundaryConditions.Objects[0,Index+1] = nil;
      break;
    end;
  end;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[0, Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[1, Index + 1] <> '');
    if UseRow then
    begin
      for ColIndex := 0 to rdgBoundaryConditions.ColCount - 1 do
      begin
        if InitializeGrid then
        begin
          CellText := rdgBoundaryConditions.Cells[ColIndex, Index + 1];
          CachedPosition := FFieldNumbers.Indexof(CellText);
          if CachedPosition >= 0 then
          begin
            FieldStorage := FFieldNumbers.Objects[CachedPosition] as TFieldNumStorage;
          end
          else
          begin
            FieldNumber := xbShapeDataBase.GetFieldNumberFromName(CellText);
            FieldStorage := TFieldNumStorage.Create(xbShapeDataBase);
            FieldStorage.FieldNumber := FieldNumber;
            FieldStorage.Formula := CellText;
            FFieldNumbers.AddObject(CellText, FieldStorage);
            if FieldNumber = 0 then
            begin
              FieldStorage.RealValue := StrToFloatDef(CellText, 0);
              FieldStorage.IntValue := StrToIntDef(CellText, 0);
            end;
          end;
          rdgBoundaryConditions.Objects[ColIndex, Index+1] := FieldStorage;
        end
        else
        begin
          FieldStorage := TFieldNumStorage(rdgBoundaryConditions.Objects[ColIndex, Index+1]);
          FieldStorage.Cached := False;
        end;
      end;
    end;
  end;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[0, Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[1, Index + 1] <> '');
    if UseRow then
    begin
      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scStartTime), Index + 1]);
      StartTime := FieldStorage.GetRealValue;
      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scEndTime), Index + 1]);
      EndTime := FieldStorage.GetRealValue;
      if First then
      begin
        First := False;
        Boundary.SegementNumber :=
          GetIntegerValueFromText(comboSfrSegmentNumber.Text);

        Item := Boundary.Values.Add as TSfrItem;
        Item.StartTime := StartTime;
        Item.EndTime := EndTime;

        FieldNumber := xbShapeDataBase.GetFieldNumberFromName(comboSfrReachLength.Text);
        if FieldNumber = 0 then
        begin
          // not a field
          if TryStrToFloat(comboSfrReachLength.Text, Value) then
          begin
            RCHLEN := FloatToStr(Value);
          end
          else
          begin
            RCHLEN := comboSfrReachLength.Text;
          end;
        end
        else
        begin
          Value := xbShapeDataBase.GetFieldNum(FieldNumber);
          RCHLEN := FloatToStr(Value);
        end;
        Item.ReachLength := RCHLEN;
        Item.HydraulicConductivity :=
          GetRealFormulaFromText(comboSfrStreambedKv.Text, False);
        Item.StreamBedThickness :=
          GetRealFormulaFromText(comboSfrStreambedThickness.Text, False);
        Item.StreambedElevation :=
          GetRealFormulaFromText(comboSfrStreambedTop.Text, False);
        Item.StreamSlope :=
          GetRealFormulaFromText(comboSfrStreamSlope.Text, False);
        Item.SaturatedWaterContent :=
          GetRealFormulaFromText(comboSaturatedVolumetricWater.Text, False);
        Item.InitialWaterContent :=
          GetRealFormulaFromText(comboInitialVolumetricWater.Text, False);
        Item.BrooksCoreyExponent :=
          GetRealFormulaFromText(comboBrooksCoreyExponent.Text, False);
        Item.VerticalK :=
          GetRealFormulaFromText(comboaxUnsaturatedKz.Text, False);
      end;

      IcalcItem := Boundary.ParamIcalc.Add as TSfrParamIcalcItem;
      IcalcItem.StartTime := StartTime;
      IcalcItem.EndTime := EndTime;
      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scIcalc), Index + 1]);
      ICalc := FieldStorage.GetIntValue;
      if (ICalc < 0) or (ICalc > 4) then
      begin
        ICalc := 0;
      end;
      IcalcItem.ICalc := ICalc;
      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scOutflowSegment), Index + 1]);
      IcalcItem.OutflowSegment := FieldStorage.GetIntValue;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scDiversionSegment), Index + 1]);
      IcalcItem.DiversionSegment := FieldStorage.GetIntValue;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scIprior), Index + 1]);
      IPrior := FieldStorage.GetIntValue;

      if (IPrior > 0) or (IPrior < -3) then
      begin
        IPrior := 0;
      end;
      IcalcItem.IPRIOR := IPrior;

      SegmentFlowItem := Boundary.SegmentFlows.Add as TSfrSegmentFlowItem;
      SegmentFlowItem.StartTime := StartTime;
      SegmentFlowItem.EndTime := EndTime;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scFlow), Index + 1]);
      SegmentFlowItem.Flow := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scPtsw), Index + 1]);
      SegmentFlowItem.Precipitation := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scEtsw), Index + 1]);
      SegmentFlowItem.Evapotranspiration := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scRunoff), Index + 1]);
      SegmentFlowItem.Runnoff := FieldStorage.RealFormula;

      if ICalc in [1,2] then
      begin
        ChannelItem := Boundary.ChannelValues.Add as TSfrChannelItem;
        ChannelItem.StartTime := StartTime;
        ChannelItem.EndTime := EndTime;

        FieldStorage := TFieldNumStorage(
          rdgBoundaryConditions.Objects[Ord(scRoughCh), Index + 1]);
        ChannelItem.ChannelRoughness := FieldStorage.RealFormula;

        FieldStorage := TFieldNumStorage(
          rdgBoundaryConditions.Objects[Ord(scRoughBk), Index + 1]);
        ChannelItem.BankRoughness := FieldStorage.RealFormula;
      end;

      if ICalc = 3 then
      begin
        EqItem := Boundary.EquationValues.Add as TSfrEquationItem;
        EqItem.StartTime := StartTime;
        EqItem.EndTime := EndTime;

        FieldStorage := TFieldNumStorage(
          rdgBoundaryConditions.Objects[Ord(scCdpth), Index + 1]);
        EqItem.DepthCoefficient := FieldStorage.RealFormula;

        FieldStorage := TFieldNumStorage(
          rdgBoundaryConditions.Objects[Ord(scFdpth), Index + 1]);
        EqItem.DepthExponent := FieldStorage.RealFormula;

        FieldStorage := TFieldNumStorage(
          rdgBoundaryConditions.Objects[Ord(scAwdth), Index + 1]);
        EqItem.WidthCoefficient := FieldStorage.RealFormula;

        FieldStorage := TFieldNumStorage(
          rdgBoundaryConditions.Objects[Ord(edBwdth), Index + 1]);
        EqItem.WidthExponent := FieldStorage.RealFormula;
      end;

      SegItem := Boundary.UpstreamSegmentValues.Add as TSfrSegmentItem;
      SegItem.StartTime := StartTime;
      SegItem.EndTime := EndTime;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scHcond1), Index + 1]);
      SegItem.HydraulicConductivity := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scThickM1), Index + 1]);
      SegItem.StreamBedThickness := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scElevUp), Index + 1]);
      SegItem.StreambedElevation := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scWidth1), Index + 1]);
      SegItem.StreamWidth := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scDepth1), Index + 1]);
      SegItem.StreamDepth := FieldStorage.RealFormula;

      SegItem := Boundary.DownstreamSegmentValues.Add as TSfrSegmentItem;
      SegItem.StartTime := StartTime;
      SegItem.EndTime := EndTime;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scHcond2), Index + 1]);
      SegItem.HydraulicConductivity := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scThickM2), Index + 1]);
      SegItem.StreamBedThickness := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scElevDn), Index + 1]);
      SegItem.StreambedElevation := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scWidth2), Index + 1]);
      SegItem.StreamWidth := FieldStorage.RealFormula;

      FieldStorage := TFieldNumStorage(
        rdgBoundaryConditions.Objects[Ord(scDepth2), Index + 1]);
      SegItem.StreamDepth := FieldStorage.RealFormula;
    end;
  end;
end;

procedure TfrmImportShapefile.ImportModflowHobPackage(
  AScreenObject: TScreenObject);
var
  UseRow: Boolean;
  Item: THobItem;
  ATime: Extended;
  Index: Integer;
  ShouldIgnoreValues: boolean;
  IgnoreValue: double;
  AValue: Extended;
begin
  ShouldIgnoreValues := rdeIgnoreValues.Text <> '';
  if ShouldIgnoreValues then
  begin
    IgnoreValue := StrToFloat(rdeIgnoreValues.Text);
  end
  else
  begin
    IgnoreValue := 0;
  end;
  AScreenObject.CreateHeadObservations;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[0, Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[1, Index + 1] <> '');
    if UseRow then
    begin
      ATime := GetRealValueFromText(rdgBoundaryConditions.Cells[0, Index + 1]);
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[1, Index + 1]);
      if ShouldIgnoreValues and ((ATime = IgnoreValue) or (AValue = IgnoreValue)) then
      begin
        Continue;
      end;
      Item := AScreenObject.ModflowHeadObservations.Values.Add as THobItem;
      Item.Time := ATime;
      Item.Head := AValue;
    end;
  end;
  AScreenObject.ModflowHeadObservations.ObservationName :=
    GetStringValueFromText(comboHeadObservationNames.Text);
  AScreenObject.ModflowHeadObservations.Purpose :=
    TObservationPurpose(comboHeadObsType.ItemIndex);
  AScreenObject.ModflowHeadObservations.MultiObsMethod :=
    TMultiObsMethod(comboITT.ItemIndex);
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForMnw2;
var
  Index: Integer;
begin
  plBoundary.ActivePage := jvspModflowMNW2;
  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := 8;
  AssignColFeatureProperties;
  for Index := Ord(mtcStartTime) to Ord(mtcEndTime) do
  begin
    rdgBoundaryConditions.Columns[Index].ComboUsed := True;
    rdgBoundaryConditions.Columns[Index].Format := rcf4String;
    rdgBoundaryConditions.Columns[Index].PickList := FRealFieldNames;
  end;

  for Index := Ord(mtcPumpingRate) to Ord(mtcLimitingWaterLevel) do
  begin
    rdgBoundaryConditions.Columns[Index].ComboUsed := True;
    rdgBoundaryConditions.Columns[Index].Format := rcf4String;
    rdgBoundaryConditions.Columns[Index].PickList :=
      FRealFieldAndGlobalVariablesNames;
  end;

  rdgBoundaryConditions.Columns[Ord(mtcLimitMethod)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(mtcLimitMethod)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(mtcLimitMethod)].PickList :=
    FIntegerFieldNames;

  for Index := Ord(mtcMinRate) to Ord(mtcMaxRate) do
  begin
    rdgBoundaryConditions.Columns[Index].ComboUsed := True;
    rdgBoundaryConditions.Columns[Index].Format := rcf4String;
    rdgBoundaryConditions.Columns[Index].PickList :=
      FRealFieldAndGlobalVariablesNames;
  end;

  rdgBoundaryConditions.Cells[Ord(mtcStartTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[Ord(mtcEndTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[Ord(mtcPumpingRate), 0] := StrDesiredPumpingRate;
  rdgBoundaryConditions.Cells[Ord(mtcMultiplier), 0] := StrHeadCapacityMultip;
  rdgBoundaryConditions.Cells[Ord(mtcLimitingWaterLevel), 0] :=
    StrLimitingWaterLevel;
  rdgBoundaryConditions.Cells[Ord(mtcLimitMethod), 0] := StrPumpingLimitMethod;
  rdgBoundaryConditions.Cells[Ord(mtcMinRate), 0] := StrDeactivationPumping;
  rdgBoundaryConditions.Cells[Ord(mtcMaxRate), 0] := StrReactivationPumping;

  pcMnw2.ActivePageIndex := 0;

  // Basic tab
  comboMnw2WellId.Items := FStringFieldNames;
  comboMnw2LossType.Items := FStringFieldNames;
  comboSpecifyPump.Items := FIntegerFieldNames;
  comboZPump.Items := FRealFieldNames;
  comboMnw2PumplocX.Items := FRealFieldNames;
  comboMnw2PumplocY.Items := FRealFieldNames;
  comboMnw2PumplocZ.Items := FRealFieldNames;
  comboConstrainPumping.Items := FIntegerFieldNames;
  comboPartialPenetrationFlag.Items := FIntegerFieldNames;
  comboPartialPenetration.Items := FRealFieldGlobalsAndDataSetsNames;
  comboPumpCap.Items := FIntegerFieldNames;

  // Loss Controls tab
  comboWellRadius.Items := FRealFieldGlobalsAndDataSetsNames;
  comboSkinRadius.Items := FRealFieldGlobalsAndDataSetsNames;
  comboKSkin.Items := FRealFieldGlobalsAndDataSetsNames;
  comboBCoefficient.Items := FRealFieldGlobalsAndDataSetsNames;
  comboCCoefficient.Items := FRealFieldGlobalsAndDataSetsNames;
  comboPCoefficient.Items := FRealFieldGlobalsAndDataSetsNames;
  comboCellToWellConductance.Items := FRealFieldGlobalsAndDataSetsNames;

  // Discharge Adjustment tab
  comboReferenceHead.Items := FRealFieldNames;
  comboLiftQ0.Items := FRealFieldNames;
  comboLiftQMax.Items := FRealFieldNames;
  comboWellTolerance.Items := FRealFieldNames;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForUZF;
var
  Index: Integer;
begin
  plBoundary.ActivePage := jvspNone;
  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := 6;
  AssignColFeatureProperties;
  for Index := Ord(ucStartTime) to Ord(ucEndTime) do
  begin
    rdgBoundaryConditions.Columns[Index].ComboUsed := True;
    rdgBoundaryConditions.Columns[Index].Format := rcf4String;
    rdgBoundaryConditions.Columns[Index].PickList := FRealFieldNames;
  end;
  for Index := Ord(ucInfiltration) to Ord(ucExtinctWaterContent) do
  begin
    rdgBoundaryConditions.Columns[Index].ComboUsed := True;
    rdgBoundaryConditions.Columns[Index].Format := rcf4String;
    rdgBoundaryConditions.Columns[Index].PickList := FRealFieldGlobalsAndDataSetsNames;
  end;
  rdgBoundaryConditions.Cells[Ord(ucStartTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[Ord(ucEndTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[Ord(ucInfiltration), 0] := 'Infiltration rate';
  rdgBoundaryConditions.Cells[Ord(ucEvapRate), 0] := 'Evapo- transpiration demand';
  rdgBoundaryConditions.Cells[Ord(ucExtinctDepth), 0] := 'ET exctinction depth';
  rdgBoundaryConditions.Cells[Ord(ucExtinctWaterContent), 0] := 'ET exctinction water content';

end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForHFB;
begin
  plBoundary.ActivePage := jvspModflowHFB;
  rdgBoundaryConditions.ColCount := 0;
  rdgBoundaryConditions.Enabled := False;
  comboHfbHydCond.Items := FRealFieldAndGlobalVariablesNames;
  comboHfbThickness.Items := FRealFieldAndGlobalVariablesNames;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForLAK;
var
  TimeIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  Index: Integer;
const
  ColumnNames : array[2..7] of string = ('Minimum stage', 'Maximum stage',
    'Precipitation', 'Evaporation', 'Overland runoff', 'Withdrawal');
begin
  plBoundary.ActivePage := jvspModflowLAK;

  comboLakeID.Items := FIntegerFieldNames;
  comboCenterLake.Items := FIntegerFieldNames;

  comboInitialStage.Items := FRealFieldNames;
  comboSill.Items := FRealFieldNames;

  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := 8;
  AssignColFeatureProperties;

  rdgBoundaryConditions.Columns[Ord(scStartTime)].WordWrapCaptions := True;
  rdgBoundaryConditions.Cells[Ord(scStartTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Columns[Ord(scStartTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scStartTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scStartTime)].PickList.Clear;
  for TimeIndex := 0 to frmGoPhast.PhastModel.ModflowStressPeriods.Count - 1 do
  begin
    StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods.Items[TimeIndex];
    rdgBoundaryConditions.Columns[Ord(scStartTime)].PickList.Add(FloatToStr(StressPeriod.StartTime));
  end;
  rdgBoundaryConditions.Columns[Ord(scStartTime)].PickList.AddStrings(FRealFieldNames);

  rdgBoundaryConditions.Columns[Ord(scEndTime)].WordWrapCaptions := True;
  rdgBoundaryConditions.Cells[Ord(scEndTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Columns[Ord(scEndTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scEndTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scEndTime)].PickList.Clear;
  for TimeIndex := 0 to frmGoPhast.PhastModel.ModflowStressPeriods.Count - 1 do
  begin
    StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods.Items[TimeIndex];
    rdgBoundaryConditions.Columns[Ord(scEndTime)].PickList.Add(FloatToStr(StressPeriod.EndTime));
  end;
  rdgBoundaryConditions.Columns[Ord(scEndTime)].PickList.AddStrings(FRealFieldNames);

  for Index := 2 to 7 do
  begin
    rdgBoundaryConditions.Columns[Index].WordWrapCaptions := True;
    rdgBoundaryConditions.Cells[Index, 0] := ColumnNames[Index];
    rdgBoundaryConditions.Columns[Index].ComboUsed := True;
    rdgBoundaryConditions.Columns[Index].Format := rcf4String;
    rdgBoundaryConditions.Columns[Index].PickList := FRealFieldAndGlobalVariablesNames;
  end;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForSFR;
var
  TimeIndex: Integer;
  StressPeriod: TModflowStressPeriod;
begin
  plBoundary.ActivePage := jvspModflowSFR;

  comboSfrSegmentNumber.Items := FIntegerFieldNames;

  comboSfrReachLength.Items := FRealFieldGlobalsAndDataSetsNames;
  comboSfrStreambedTop.Items := FRealFieldAndGlobalVariablesNames;
  comboSfrStreamSlope.Items := FRealFieldAndGlobalVariablesNames;
  comboSfrStreambedThickness.Items := FRealFieldAndGlobalVariablesNames;
  comboSfrStreambedKv.Items := FRealFieldAndGlobalVariablesNames;
  comboSaturatedVolumetricWater.Items := FRealFieldAndGlobalVariablesNames;
  comboInitialVolumetricWater.Items := FRealFieldAndGlobalVariablesNames;
  comboBrooksCoreyExponent.Items := FRealFieldAndGlobalVariablesNames;
  comboaxUnsaturatedKz.Items := FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := Ord(High(TSfrColumns)) + 1;
  AssignColFeatureProperties;

  rdgBoundaryConditions.Cells[Ord(scStartTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Columns[Ord(scStartTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scStartTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scStartTime)].PickList.Clear;
  for TimeIndex := 0 to frmGoPhast.PhastModel.ModflowStressPeriods.Count - 1 do
  begin
    StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods.Items[TimeIndex];
    rdgBoundaryConditions.Columns[Ord(scStartTime)].PickList.Add(FloatToStr(StressPeriod.StartTime));
  end;
  rdgBoundaryConditions.Columns[Ord(scStartTime)].PickList.AddStrings(FRealFieldAndGlobalVariablesNames);

  rdgBoundaryConditions.Cells[Ord(scEndTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Columns[Ord(scEndTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scEndTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scEndTime)].PickList.Clear;
  for TimeIndex := 0 to frmGoPhast.PhastModel.ModflowStressPeriods.Count - 1 do
  begin
    StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods.Items[TimeIndex];
    rdgBoundaryConditions.Columns[Ord(scEndTime)].PickList.Add(FloatToStr(StressPeriod.EndTime));
  end;
  rdgBoundaryConditions.Columns[Ord(scEndTime)].PickList.AddStrings(FRealFieldAndGlobalVariablesNames);

  rdgBoundaryConditions.Cells[Ord(scIcalc), 0] := 'ICALC';
  rdgBoundaryConditions.Columns[Ord(scIcalc)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scIcalc)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scIcalc)].PickList := FIntegerFieldNames;

  rdgBoundaryConditions.Cells[Ord(scFlow), 0] := 'FLOW';
  rdgBoundaryConditions.Columns[Ord(scFlow)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scFlow)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scFlow)].PickList := FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(scOutflowSegment), 0] := 'Outflow Segments';
  rdgBoundaryConditions.Columns[Ord(scOutflowSegment)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scOutflowSegment)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scOutflowSegment)].PickList := FIntegerFieldNames;

  rdgBoundaryConditions.Cells[Ord(scDiversionSegment), 0] := 'Diversion Segments';
  rdgBoundaryConditions.Columns[Ord(scDiversionSegment)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scDiversionSegment)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scDiversionSegment)].PickList := FIntegerFieldNames;

  rdgBoundaryConditions.Cells[Ord(scIprior), 0] := 'IPRIOR';
  rdgBoundaryConditions.Columns[Ord(scIprior)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scIprior)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scIprior)].PickList := FIntegerFieldNames;

  rdgBoundaryConditions.Cells[Ord(scPtsw), 0] := 'PTSW';
  rdgBoundaryConditions.Columns[Ord(scPtsw)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scPtsw)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scPtsw)].PickList := FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(scEtsw), 0] := 'ETSW';
  rdgBoundaryConditions.Columns[Ord(scEtsw)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scEtsw)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scEtsw)].PickList := FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(scRunoff), 0] := 'RUNOFF';
  rdgBoundaryConditions.Columns[Ord(scRunoff)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scRunoff)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scRunoff)].PickList := FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(scRoughCh), 0] := 'ROUGHCH';
  rdgBoundaryConditions.Columns[Ord(scRoughCh)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scRoughCh)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scRoughCh)].PickList := FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(scRoughBk), 0] := 'ROUGHBK';
  rdgBoundaryConditions.Columns[Ord(scRoughBk)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scRoughBk)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scRoughBk)].PickList := FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(scCdpth), 0] := 'CDPTH';
  rdgBoundaryConditions.Columns[Ord(scCdpth)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scCdpth)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scCdpth)].PickList := FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(scFdpth), 0] := 'FDPTH';
  rdgBoundaryConditions.Columns[Ord(scFdpth)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scFdpth)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scFdpth)].PickList := FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(scAwdth), 0] := 'AWDTH';
  rdgBoundaryConditions.Columns[Ord(scAwdth)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scAwdth)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scAwdth)].PickList := FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(edBwdth), 0] := 'BWDTH';
  rdgBoundaryConditions.Columns[Ord(edBwdth)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(edBwdth)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(edBwdth)].PickList := FRealFieldAndGlobalVariablesNames;

  rdgBoundaryConditions.Cells[Ord(scHcond1), 0] := 'HCOND1';
  rdgBoundaryConditions.Columns[Ord(scHcond1)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scHcond1)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scHcond1)].PickList := FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scThickM1), 0] := 'THICKM1';
  rdgBoundaryConditions.Columns[Ord(scThickM1)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scThickM1)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scThickM1)].PickList := FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scElevUp), 0] := 'ELEVUP';
  rdgBoundaryConditions.Columns[Ord(scElevUp)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scElevUp)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scElevUp)].PickList := FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scWidth1), 0] := 'WIDTH1';
  rdgBoundaryConditions.Columns[Ord(scWidth1)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scWidth1)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scWidth1)].PickList := FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scDepth1), 0] := 'DEPTH1';
  rdgBoundaryConditions.Columns[Ord(scDepth1)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scDepth1)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scDepth1)].PickList := FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scHcond2), 0] := 'HCOND2';
  rdgBoundaryConditions.Columns[Ord(scHcond2)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scHcond2)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scHcond2)].PickList := FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scThickM2), 0] := 'THICKM2';
  rdgBoundaryConditions.Columns[Ord(scThickM2)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scThickM2)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scThickM2)].PickList := FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scElevDn), 0] := 'ELEVDN';
  rdgBoundaryConditions.Columns[Ord(scElevDn)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scElevDn)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scElevDn)].PickList := FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scWidth2), 0] := 'WIDTH2';
  rdgBoundaryConditions.Columns[Ord(scWidth2)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scWidth2)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scWidth2)].PickList := FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(scDepth2), 0] := 'DEPTH2';
  rdgBoundaryConditions.Columns[Ord(scDepth2)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(scDepth2)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(scDepth2)].PickList := FRealFieldGlobalsAndDataSetsNames;

end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForHOB;
begin
  plBoundary.ActivePage := jvspModflowHOB;
  comboHeadObservationNames.Items := FStringFieldNames;
  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := 2;
  AssignColFeatureProperties;
  rdgBoundaryConditions.Columns[0].ComboUsed := True;
  rdgBoundaryConditions.Columns[1].ComboUsed := True;
  rdgBoundaryConditions.Columns[0].Format := rcf4String;
  rdgBoundaryConditions.Columns[1].Format := rcf4String;
  rdgBoundaryConditions.Columns[0].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[1].PickList := FRealFieldNames;
  rdgBoundaryConditions.Cells[0, 0] := 'Time';
  rdgBoundaryConditions.Cells[1, 0] := 'Observed head';
end;

procedure TfrmImportShapefile.ImportModflowResPackage(
  AScreenObject: TScreenObject);
var
  Index: Integer;
  UseRow: Boolean;
  Item: TResItem;
  AValue: Extended;
  AFormula: string;
begin
  AScreenObject.CreateResBoundary;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[0, Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[1, Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[2, Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[3, Index + 1] <> '');
    if UseRow then
    begin
      Item := AScreenObject.ModflowResBoundary.Values.Add as TResItem;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[0, Index + 1]);
      Item.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[1, Index + 1]);
      Item.EndTime := AValue;
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[2, Index + 1]);
      Item.StartHead := AFormula;
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[3, Index + 1]);
      Item.EndHead := AFormula;
    end;
  end;
end;

procedure TfrmImportShapefile.ImportModflowEtsPackage(
  AScreenObject: TScreenObject; Packages: TModflowPackages);
var
  Index: Integer;
  UseRow: Boolean;
  Item: TEvtItem;
  AValue: Extended;
  FractionIndex: Integer;
  SurfDepthItem: TEtsSurfDepthItem;
  FractItem: TStringValueItem;
  LayerItem: TEvtLayerItem;
  AnIntValue: string;
  AFormula: string;
  Boundary: TModflowParamBoundary;
  ParamItem: TModflowParamItem;
  ParameterName: string;
  AnItem: TCustomModflowBoundaryItem;
  Param: TModflowTransientListParameter;
begin
  AScreenObject.CreateEtsBoundary;

  Boundary := AScreenObject.ModflowEtsBoundary;
  ParamItem := nil;

  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(etscStartingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(etscEndingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(etscRate), Index + 1] <> '');
    if UseRow then
    begin
      GetTransientParameter(Param, ParameterName, Ord(etscParameterName), Index+1);
      if (ParameterName <> '') and (Param = nil) then
      begin
        Continue;
      end;
      GetNewBoundaryItem(AnItem, ParameterName, Param, ParamItem, Boundary);
      Item := AnItem as TEvtItem;

//      Item := AScreenObject.ModflowEtsBoundary.Values.Add as TEvtItem;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[Ord(etscStartingTime), Index + 1]);
      Item.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[Ord(etscEndingTime), Index + 1]);
      Item.EndTime := AValue;
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[Ord(etscRate), Index + 1]);
      Item.EvapotranspirationRate := AFormula;
    end;
  end;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(etscStartingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(etscEndingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(etscSurface), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(etscDepth), Index + 1] <> '');
    for FractionIndex := 0 to Packages.EtsPackage.SegmentCount - 2 do
    begin
      UseRow := UseRow and (rdgBoundaryConditions.
        Cells[Ord(etscDepth)+1 + (FractionIndex * 2), Index + 1] <> '')
        and (rdgBoundaryConditions.
        Cells[Ord(etscDepth)+2 + (FractionIndex * 2), Index + 1] <> '');
    end;
    if UseRow then
    begin
      SurfDepthItem := AScreenObject.ModflowEtsBoundary.
        EtsSurfDepthCollection.Add as TEtsSurfDepthItem;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[Ord(etscStartingTime), Index + 1]);
      SurfDepthItem.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[Ord(etscEndingTime), Index + 1]);
      SurfDepthItem.EndTime := AValue;
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[Ord(etscSurface), Index + 1]);
      SurfDepthItem.EvapotranspirationSurface := AFormula;
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[Ord(etscDepth), Index + 1]);
      SurfDepthItem.EvapotranspirationDepth := AFormula;
      for FractionIndex := 0 to Packages.EtsPackage.SegmentCount - 2 do
      begin
        FractItem := SurfDepthItem.DepthFractions.Add as TStringValueItem;
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.
          Cells[Ord(etscDepth)+1 + (FractionIndex * 2), Index + 1]);
        FractItem.Value := AFormula;

        FractItem := SurfDepthItem.EtFractions.Add as TStringValueItem;
        AFormula := GetRealFormulaFromText(rdgBoundaryConditions.
          Cells[Ord(etscDepth)+2 + (FractionIndex * 2), Index + 1]);
        FractItem.Value := AFormula;
      end;
    end;
  end;
  if Packages.EtsPackage.TimeVaryingLayers then
  begin
    for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
    begin
      UseRow := (rdgBoundaryConditions.Cells[Ord(etscStartingTime), Index + 1] <> '')
        and (rdgBoundaryConditions.Cells[Ord(etscEndingTime), Index + 1] <> '')
        and (rdgBoundaryConditions.
        Cells[rdgBoundaryConditions.ColCount - 1, Index + 1] <> '');
      if UseRow then
      begin
        LayerItem := AScreenObject.ModflowEtsBoundary.
          EvapotranspirationLayers.Add as TEvtLayerItem;
        AValue := GetRealValueFromText(
          rdgBoundaryConditions.Cells[Ord(etscStartingTime), Index + 1]);
        LayerItem.StartTime := AValue;
        AValue := GetRealValueFromText(
          rdgBoundaryConditions.Cells[Ord(etscEndingTime), Index + 1]);
        LayerItem.EndTime := AValue;
        AnIntValue := GetIntegerFormulaFromText(rdgBoundaryConditions.
          Cells[rdgBoundaryConditions.ColCount - 1, Index + 1]);
        LayerItem.EvapotranspirationLayer := AnIntValue;
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForRES(
  Packages: TModflowPackages);
var
  Index: Integer;
begin
  plBoundary.ActivePage := jvspNone;
  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := 4;
  AssignColFeatureProperties;
  for Index := 0 to 1 do
  begin
    rdgBoundaryConditions.Columns[Index].ComboUsed := True;
    rdgBoundaryConditions.Columns[Index].Format := rcf4String;
    rdgBoundaryConditions.Columns[Index].PickList := FRealFieldNames;
  end;
  for Index := 2 to rdgBoundaryConditions.ColCount - 1 do
  begin
    rdgBoundaryConditions.Columns[Index].ComboUsed := True;
    rdgBoundaryConditions.Columns[Index].Format := rcf4String;
    rdgBoundaryConditions.Columns[Index].PickList := FRealFieldAndGlobalVariablesNames;
  end;
  rdgBoundaryConditions.Cells[0, 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[1, 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[2, 0] := 'Starting head';
  rdgBoundaryConditions.Cells[3, 0] := 'Ending head';
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForETS(
  Packages: TModflowPackages);
var
  Index: Integer;
begin
  plBoundary.ActivePage := jvspNone;
  rdgBoundaryConditions.Enabled := True;
  if Packages.EtsPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.ColCount :=
      (Packages.EtsPackage.SegmentCount - 1) * 2 + Ord(etscDepth) + 2;
  end
  else
  begin
    rdgBoundaryConditions.ColCount :=
      (Packages.EtsPackage.SegmentCount - 1) * 2 + Ord(etscDepth) + 1;
  end;
  AssignColFeatureProperties;
  for Index := Ord(etscStartingTime) to Ord(etscEndingTime) do
  begin
    rdgBoundaryConditions.Columns[Index].ComboUsed := True;
    rdgBoundaryConditions.Columns[Index].Format := rcf4String;
    rdgBoundaryConditions.Columns[Index].PickList := FRealFieldNames;
  end;

  rdgBoundaryConditions.Columns[Ord(etscParameterName)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(etscParameterName)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(etscParameterName)].PickList := FStringFieldNames;
  AddParameterNamesToPickList(ptETS, Ord(etscParameterName));

  for Index := Ord(etscRate) to rdgBoundaryConditions.ColCount - 1 do
  begin
    rdgBoundaryConditions.Columns[Index].ComboUsed := True;
    rdgBoundaryConditions.Columns[Index].Format := rcf4String;
    rdgBoundaryConditions.Columns[Index].PickList := FRealFieldGlobalsAndDataSetsNames;
  end;
  if Packages.EtsPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.Columns[rdgBoundaryConditions.ColCount - 1].PickList
      := FIntegerFieldNames;
  end;
  rdgBoundaryConditions.Cells[Ord(etscStartingTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[Ord(etscEndingTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[Ord(etscParameterName), 0] := StrParameterName;
  rdgBoundaryConditions.Cells[Ord(etscRate), 0] := 'Evapo- transpiration rate';
  rdgBoundaryConditions.Cells[Ord(etscSurface), 0] := 'Evapo- transpiration surface';
  rdgBoundaryConditions.Cells[Ord(etscDepth), 0] := 'Evapo- transpiration depth';
  for Index := 0 to (Packages.EtsPackage.SegmentCount - 2) do
  begin
    rdgBoundaryConditions.Cells[Ord(etscDepth)+1 + (Index * 2), 0] :=
      'Fractional depth ' + IntToStr(Index + 1);
    rdgBoundaryConditions.Cells[Ord(etscDepth)+2 + (Index * 2), 0] :=
      'Fractional rate ' + IntToStr(Index + 1);
  end;
  if Packages.EtsPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.Cells[rdgBoundaryConditions.ColCount - 1, 0] :=
      'Evapo- transpiration layer';
  end;
end;

procedure TfrmImportShapefile.ImportModflowEvtPackage(
  AScreenObject: TScreenObject);
var
  Index: Integer;
  UseRow: Boolean;
  Item: TEvtItem;
  AValue: Extended;
  SurfDepthItem: TEvtSurfDepthItem;
  LayerItem: TEvtLayerItem;
  AnIntValue: string;
  AFormula: string;
  ParamItem: TModflowParamItem;
  Boundary: TModflowParamBoundary;
  ParameterName: string;
  Param: TModflowTransientListParameter;
  AnItem: TCustomModflowBoundaryItem;
begin
  AScreenObject.CreateEvtBoundary;
  Boundary := AScreenObject.ModflowEvtBoundary;
  ParamItem := nil;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(evtcStartingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(evtcEndingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(evtRate), Index + 1] <> '');
    if UseRow then
    begin
      GetTransientParameter(Param, ParameterName, Ord(evtcParameterName), Index+1);
      if (ParameterName <> '') and (Param = nil) then
      begin
        Continue;
      end;
      GetNewBoundaryItem(AnItem, ParameterName, Param, ParamItem, Boundary);
      Item := AnItem as TEvtItem;

//      Item := AScreenObject.ModflowEvtBoundary.Values.Add as TEvtItem;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[Ord(evtcStartingTime), Index + 1]);
      Item.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[Ord(evtcEndingTime), Index + 1]);
      Item.EndTime := AValue;
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[Ord(evtRate), Index + 1]);
      Item.EvapotranspirationRate := AFormula;
    end;
  end;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(evtcStartingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(evtcEndingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(evtcSurface), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(evtcDepth), Index + 1] <> '');
    if UseRow then
    begin
      SurfDepthItem := AScreenObject.ModflowEvtBoundary.
        EvtSurfDepthCollection.Add as TEvtSurfDepthItem;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[Ord(evtcStartingTime), Index + 1]);
      SurfDepthItem.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[Ord(evtcEndingTime), Index + 1]);
      SurfDepthItem.EndTime := AValue;
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[Ord(evtcSurface), Index + 1]);
      SurfDepthItem.EvapotranspirationSurface := AFormula;
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[Ord(evtcDepth), Index + 1]);
      SurfDepthItem.EvapotranspirationDepth := AFormula;
    end;
  end;
  if rdgBoundaryConditions.ColCount = Ord(evtcLayer)+1 then
  begin
    for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
    begin
      UseRow := (rdgBoundaryConditions.Cells[Ord(evtcStartingTime), Index + 1] <> '')
        and (rdgBoundaryConditions.Cells[Ord(evtcEndingTime), Index + 1] <> '')
        and (rdgBoundaryConditions.Cells[Ord(evtcLayer), Index + 1] <> '');
      if UseRow then
      begin
        LayerItem := AScreenObject.ModflowEvtBoundary.
          EvapotranspirationLayers.Add as TEvtLayerItem;
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(evtcStartingTime), Index + 1]);
        LayerItem.StartTime := AValue;
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(evtcEndingTime), Index + 1]);
        LayerItem.EndTime := AValue;
        AnIntValue := GetIntegerFormulaFromText(rdgBoundaryConditions.
          Cells[Ord(evtcLayer), Index + 1]);
        LayerItem.EvapotranspirationLayer := AnIntValue;
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForEVT(
  Packages: TModflowPackages);
var
  Index: Integer;
begin
  plBoundary.ActivePage := jvspNone;
  rdgBoundaryConditions.Enabled := True;
  if Packages.EvtPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.ColCount := Ord(evtcLayer) + 1;
  end
  else
  begin
    rdgBoundaryConditions.ColCount := Ord(evtcDepth) + 1;
  end;
  AssignColFeatureProperties;
  rdgBoundaryConditions.Columns[Ord(evtcStartingTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(evtcEndingTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(evtcParameterName)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(evtRate)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(evtcSurface)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(evtcDepth)].ComboUsed := True;
  if Packages.EvtPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.Columns[Ord(evtcLayer)].ComboUsed := True;
  end;

  rdgBoundaryConditions.Columns[Ord(evtcStartingTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(evtcEndingTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(evtcParameterName)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(evtRate)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(evtcSurface)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(evtcDepth)].Format := rcf4String;
  if Packages.EvtPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.Columns[Ord(evtcLayer)].Format := rcf4String;
  end;
  for Index := Ord(evtcStartingTime) to Ord(evtcEndingTime) do
  begin
    rdgBoundaryConditions.Columns[Index].PickList := FRealFieldNames;
  end;
  rdgBoundaryConditions.Columns[Ord(evtcParameterName)].PickList := FStringFieldNames;
  AddParameterNamesToPickList(ptEVT, Ord(evtcParameterName));
  for Index := Ord(evtRate) to rdgBoundaryConditions.ColCount - 1 do
  begin
    rdgBoundaryConditions.Columns[Index].PickList := FRealFieldGlobalsAndDataSetsNames;
  end;
  if Packages.EvtPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.Columns[Ord(evtcLayer)].PickList := FIntegerFieldNames;
  end;
  rdgBoundaryConditions.Cells[Ord(evtcStartingTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[Ord(evtcEndingTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[Ord(evtcParameterName), 0] := StrParameterName;
  rdgBoundaryConditions.Cells[Ord(evtRate), 0] := 'Evapo- transpiration rate';
  rdgBoundaryConditions.Cells[Ord(evtcSurface), 0] := 'Evapo- transpiration surface';
  rdgBoundaryConditions.Cells[Ord(evtcDepth), 0] := 'Evapo- transpiration depth';
  if Packages.EvtPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.Cells[Ord(evtcLayer), 0] := 'Evapo- transpiration layer';
  end;
end;

procedure TfrmImportShapefile.ImportModflowRchPackage(
  AScreenObject: TScreenObject);
var
  Index: Integer;
  UseRow: Boolean;
  Item: TRchItem;
  AValue: Extended;
  LayerItem: TRchLayerItem;
  AnIntValue: string;
  AFormula: string;
  ParameterName: string;
  Param: TModflowTransientListParameter;
  AnItem: TCustomModflowBoundaryItem;
  ParamItem: TModflowParamItem;
  Boundary: TModflowParamBoundary;
begin
  AScreenObject.CreateRchBoundary;
  Boundary := AScreenObject.ModflowRchBoundary;
  ParamItem := nil;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(rcStartTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(rcEndTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(rcFluxRate), Index + 1] <> '');
    if UseRow then
    begin
      GetTransientParameter(Param, ParameterName, Ord(rcParameterName), Index+1);
      if (ParameterName <> '') and (Param = nil) then
      begin
        Continue;
      end;

      GetNewBoundaryItem(AnItem, ParameterName, Param, ParamItem, Boundary);
      Item := AnItem as TRchItem;

      AValue := GetRealValueFromText(rdgBoundaryConditions.
        Cells[Ord(rcStartTime), Index + 1]);
      Item.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.
        Cells[Ord(rcEndTime), Index + 1]);
      Item.EndTime := AValue;
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.
        Cells[Ord(rcFluxRate), Index + 1]);
      Item.RechargeRate := AFormula;
    end;
  end;
  if rdgBoundaryConditions.ColCount = Ord(rcLayer)+1 then
  begin
    for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
    begin
      UseRow := (rdgBoundaryConditions.Cells[Ord(rcStartTime), Index + 1] <> '')
        and (rdgBoundaryConditions.Cells[Ord(rcEndTime), Index + 1] <> '')
        and (rdgBoundaryConditions.Cells[Ord(rcLayer), Index + 1] <> '');
      if UseRow then
      begin
        LayerItem := AScreenObject.ModflowRchBoundary.
          RechargeLayers.Add as TRchLayerItem;
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(rcStartTime), Index + 1]);
        LayerItem.StartTime := AValue;
        AValue := GetRealValueFromText(rdgBoundaryConditions.
          Cells[Ord(rcEndTime), Index + 1]);
        LayerItem.EndTime := AValue;
        AnIntValue := GetIntegerFormulaFromText(rdgBoundaryConditions.
          Cells[Ord(rcLayer), Index + 1]);
        LayerItem.RechargeLayer := AnIntValue;
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForRCH(
  Packages: TModflowPackages);
begin
  plBoundary.ActivePage := jvspNone;
  rdgBoundaryConditions.Enabled := True;
  if Packages.RchPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.ColCount := Ord(rcLayer)+1;
  end
  else
  begin
    rdgBoundaryConditions.ColCount := Ord(rcFluxRate)+1;
  end;
  AssignColFeatureProperties;
  rdgBoundaryConditions.Columns[Ord(rcStartTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(rcEndTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(rcParameterName)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(rcFluxRate)].ComboUsed := True;
  if Packages.RchPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.Columns[Ord(rcLayer)].ComboUsed := True;
  end;
  rdgBoundaryConditions.Columns[Ord(rcStartTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(rcEndTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(rcParameterName)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(rcFluxRate)].Format := rcf4String;
  if Packages.RchPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.Columns[Ord(rcLayer)].Format := rcf4String;
  end;
  rdgBoundaryConditions.Columns[Ord(rcStartTime)].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(rcEndTime)].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(rcParameterName)].PickList := FStringFieldNames;
  AddParameterNamesToPickList(ptRCH, Ord(rcParameterName));
  rdgBoundaryConditions.Columns[Ord(rcFluxRate)].PickList := FRealFieldGlobalsAndDataSetsNames;
  if Packages.RchPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.Columns[Ord(rcLayer)].PickList := FIntegerFieldNames;
  end;
  rdgBoundaryConditions.Cells[Ord(rcStartTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[Ord(rcEndTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[Ord(rcParameterName), 0] := StrParameterName;
  rdgBoundaryConditions.Cells[Ord(rcFluxRate), 0] := 'Recharge rate';
  if Packages.RchPackage.TimeVaryingLayers then
  begin
    rdgBoundaryConditions.Cells[Ord(rcLayer), 0] := 'Recharge layer';
  end;
end;

procedure TfrmImportShapefile.ImportModflowDrtPackage(
  AScreenObject: TScreenObject);
var
  AValue: Extended;
  Index: Integer;
  UseRow: Boolean;
  Item: TDrtItem;
  AnInteger: Integer;
  AFormula: string;
  Boundary: TModflowParamBoundary;
  ParamItem: TModflowParamItem;
  Param: TModflowTransientListParameter;
  ParameterName: string;
  AnItem: TCustomModflowBoundaryItem;
begin
  AScreenObject.CreateDrtBoundary;
  AScreenObject.ModflowDrtBoundary.FormulaInterpretation :=
    GetFormulaInterpretation(comboFormulaInterpDRT);

  case comboDrainReturnLocationMethod.ItemIndex of
    0:
      begin
        AScreenObject.ModflowDrtBoundary.DrainReturn.ReturnChoice := rtNone;
      end;
    1:
      begin
        AScreenObject.ModflowDrtBoundary.DrainReturn.ReturnChoice := rtLocation;
      end;
    2:
      begin
        AScreenObject.ModflowDrtBoundary.DrainReturn.ReturnChoice := rtCell;
      end;
    else Assert(False);
  end;
  case AScreenObject.ModflowDrtBoundary.DrainReturn.ReturnChoice of
    rtNone: ; // dp nothing.
    rtObject: Assert(False);
    rtLocation:
      begin
        AValue := GetRealValueFromText(rdeDrtX.Text);
        AScreenObject.ModflowDrtBoundary.DrainReturn.ReturnLocation.X := AValue;
        AValue := GetRealValueFromText(rdeDrtY.Text);
        AScreenObject.ModflowDrtBoundary.DrainReturn.ReturnLocation.Y := AValue;
        AValue := GetRealValueFromText(rdeDrtZ.Text);
        AScreenObject.ModflowDrtBoundary.DrainReturn.ReturnLocation.Z := AValue;
      end;
    rtCell:
      begin
        AnInteger := GetIntegerValueFromText(rdeDrtCol.Text);
        AScreenObject.ModflowDrtBoundary.DrainReturn.ReturnCell.Col := AnInteger;
        AnInteger := GetIntegerValueFromText(rdeDrtRow.Text);
        AScreenObject.ModflowDrtBoundary.DrainReturn.ReturnCell.Row := AnInteger;
        AnInteger := GetIntegerValueFromText(rdeDrtLay.Text);
        AScreenObject.ModflowDrtBoundary.DrainReturn.ReturnCell.Lay := AnInteger;
      end;
    else Assert(False);
  end;

  Boundary := AScreenObject.ModflowDrtBoundary;
  ParamItem := nil;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(dtcStartingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(dtcEndingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(dtcElevation), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(dtcConductance), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(dtcReturnFraction), Index + 1] <> '');
    if UseRow then
    begin
      GetTransientParameter(Param, ParameterName, Ord(dtcParameterName), Index+1);
      if (ParameterName <> '') and (Param = nil) then
      begin
        Continue;
      end;

      GetNewBoundaryItem(AnItem, ParameterName, Param, ParamItem, Boundary);
      Item := AnItem as TDrtItem;

//      Item := AScreenObject.ModflowDrtBoundary.Values.Add as TDrtItem;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[Ord(dtcStartingTime), Index + 1]);
      Item.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[Ord(dtcEndingTime), Index + 1]);
      Item.EndTime := AValue;
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[Ord(dtcElevation), Index + 1]);
      Item.Elevation := AFormula;
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[Ord(dtcConductance), Index + 1]);
      Item.Conductance := AFormula;
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[Ord(dtcReturnFraction), Index + 1]);
      Item.ReturnFraction := AFormula;
    end;
  end;
end;

procedure TfrmImportShapefile.ImportModflowDrnPackage(
  AScreenObject: TScreenObject);
var
  AValue: Extended;
  Index: Integer;
  UseRow: Boolean;
  Item: TDrnItem;
  AFormula: string;
  Boundary: TModflowParamBoundary;
  ParamItem: TModflowParamItem;
  Param: TModflowTransientListParameter;
  ParameterName: string;
  AnItem: TCustomModflowBoundaryItem;
begin
  AScreenObject.CreateDrnBoundary;
  Boundary := AScreenObject.ModflowDrnBoundary;
  ParamItem := nil;
  AScreenObject.ModflowDrnBoundary.FormulaInterpretation :=
    GetFormulaInterpretation(comboFormulaInterp);

  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(dcStartingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(dcEndingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(dcElevation), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(dcConductance), Index + 1] <> '');
    if UseRow then
    begin
      GetTransientParameter(Param, ParameterName, Ord(dcParameterName), Index+1);
      if (ParameterName <> '') and (Param = nil) then
      begin
        Continue;
      end;
      GetNewBoundaryItem(AnItem, ParameterName, Param, ParamItem, Boundary);
      Item := AnItem as TDrnItem;
//      Item := AScreenObject.ModflowDrnBoundary.Values.Add as TDrnItem;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[Ord(dcStartingTime), Index + 1]);
      Item.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[Ord(dcEndingTime), Index + 1]);
      Item.EndTime := AValue;
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[Ord(dcElevation), Index + 1]);
      Item.Elevation := AFormula;
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[Ord(dcConductance), Index + 1]);
      Item.Conductance := AFormula;
    end;
  end;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForDRT;
begin
  plBoundary.ActivePage := jvspModflowDRT;
  lblConductanceInterpretationDRT.Caption := 'Conductance interpretation';
  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := Ord(dtcReturnFraction)+1;
  AssignColFeatureProperties;
  rdgBoundaryConditions.Columns[Ord(dtcStartingTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(dtcEndingTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(dtcParameterName)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(dtcElevation)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(dtcConductance)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(dtcReturnFraction)].ComboUsed := True;

  rdgBoundaryConditions.Columns[Ord(dtcStartingTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(dtcEndingTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(dtcParameterName)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(dtcElevation)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(dtcConductance)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(dtcReturnFraction)].Format := rcf4String;

  rdgBoundaryConditions.Columns[Ord(dtcStartingTime)].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(dtcEndingTime)].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(dtcParameterName)].PickList := FStringFieldNames;
  AddParameterNamesToPickList(ptDRT, Ord(dtcParameterName));
  rdgBoundaryConditions.Columns[Ord(dtcElevation)].PickList := FRealFieldGlobalsAndDataSetsNames;
  rdgBoundaryConditions.Columns[Ord(dtcConductance)].PickList := FRealFieldGlobalsAndDataSetsNames;
  rdgBoundaryConditions.Columns[Ord(dtcReturnFraction)].PickList := FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(dtcStartingTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[Ord(dtcEndingTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[Ord(dtcParameterName), 0] := StrParameterName;
  rdgBoundaryConditions.Cells[Ord(dtcElevation), 0] := 'Elevation';
  rdgBoundaryConditions.Cells[Ord(dtcConductance), 0] := 'Conductance';
  rdgBoundaryConditions.Cells[Ord(dtcReturnFraction), 0] := 'Return fraction';
  
  while comboFormulaInterpDRT.Items.Count > 3 do
  begin
    comboFormulaInterpDRT.Items.Delete(comboFormulaInterpDRT.Items.Count-1);
  end;
  comboFormulaInterpDRT.Items.AddStrings(FStringFieldNames);

  rdeDrtX.Items := FRealFieldNames;
  rdeDrtY.Items := FRealFieldNames;
  rdeDrtZ.Items := FRealFieldNames;

  rdeDrtCol.Items := FIntegerFieldNames;
  rdeDrtRow.Items := FIntegerFieldNames;
  rdeDrtLay.Items := FIntegerFieldNames;

  comboDrainReturnLocationMethodChange(nil);
end;
procedure TfrmImportShapefile.InitializeBoundaryControlsForDRN;
begin
  plBoundary.ActivePage := jvspConductanceInterp;
  lblConductanceInterpretation.Caption := 'Conductance interpretation';
  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := Ord(dcConductance) + 1;
  AssignColFeatureProperties;
  rdgBoundaryConditions.Columns[Ord(dcStartingTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(dcEndingTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(dcParameterName)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(dcElevation)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(dcConductance)].ComboUsed := True;

  rdgBoundaryConditions.Columns[Ord(dcStartingTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(dcEndingTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(dcParameterName)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(dcElevation)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(dcConductance)].Format := rcf4String;

  rdgBoundaryConditions.Columns[Ord(dcStartingTime)].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(dcEndingTime)].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(dcParameterName)].PickList := FStringFieldNames;
  AddParameterNamesToPickList(ptDRN, Ord(dcParameterName));
  rdgBoundaryConditions.Columns[Ord(dcElevation)].PickList := FRealFieldGlobalsAndDataSetsNames;
  rdgBoundaryConditions.Columns[Ord(dcConductance)].PickList := FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(dcStartingTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[Ord(dcEndingTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[Ord(dcParameterName), 0] := StrParameterName;
  rdgBoundaryConditions.Cells[Ord(dcElevation), 0] := 'Elevation';
  rdgBoundaryConditions.Cells[Ord(dcConductance), 0] := 'Conductance';

  while comboFormulaInterp.Items.Count > 3 do
  begin
    comboFormulaInterp.Items.Delete(comboFormulaInterp.Items.Count-1);
  end;
  comboFormulaInterp.Items.AddStrings(FStringFieldNames);
end;

procedure TfrmImportShapefile.ImportModflowRivPackage(
  AScreenObject: TScreenObject);
var
  AValue: Extended;
  Item: TRivItem;
  UseRow: Boolean;
  Index: Integer;
  AFormula: string;
  Param: TModflowTransientListParameter;
  ParameterName: string;
  AnItem: TCustomModflowBoundaryItem;
  ParamItem: TModflowParamItem;
  Boundary: TModflowParamBoundary;
begin
  AScreenObject.CreateRivBoundary;
  Boundary := AScreenObject.ModflowRivBoundary;
  ParamItem := nil;
  AScreenObject.ModflowRivBoundary.FormulaInterpretation :=
    GetFormulaInterpretation(comboFormulaInterp);

  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(rivcStartingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(rivcEndingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(rivcBottom), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(rivcStage), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(rivcConductance), Index + 1] <> '');
    if UseRow then
    begin
      GetTransientParameter(Param, ParameterName, Ord(rivcParameterName), Index+1);
      if (ParameterName <> '') and (Param = nil) then
      begin
        Continue;
      end;

      GetNewBoundaryItem(AnItem, ParameterName, Param, ParamItem, Boundary);
      Item := AnItem as TRivItem;

//      Item := AScreenObject.ModflowRivBoundary.Values.Add as TRivItem;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[Ord(rivcStartingTime), Index + 1]);
      Item.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[Ord(rivcEndingTime), Index + 1]);
      Item.EndTime := AValue;
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[Ord(rivcBottom), Index + 1]);
      Item.RiverBottom := AFormula;
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[Ord(rivcStage), Index + 1]);
      Item.RiverStage := AFormula;
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[Ord(rivcConductance), Index + 1]);
      Item.Conductance := AFormula;
    end;
  end;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForRIV;
begin
  plBoundary.ActivePage := jvspConductanceInterp;
  lblConductanceInterpretation.Caption := 'Conductance interpretation';
  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := Ord(rivcConductance)+1;
  AssignColFeatureProperties;

  rdgBoundaryConditions.Columns[Ord(rivcStartingTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(rivcEndingTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(rivcParameterName)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(rivcBottom)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(rivcStage)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(rivcConductance)].ComboUsed := True;

  rdgBoundaryConditions.Columns[Ord(rivcStartingTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(rivcEndingTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(rivcParameterName)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(rivcBottom)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(rivcStage)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(rivcConductance)].Format := rcf4String;

  rdgBoundaryConditions.Columns[Ord(rivcStartingTime)].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(rivcEndingTime)].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(rivcParameterName)].PickList := FStringFieldNames;
  AddParameterNamesToPickList(ptRIV, Ord(rivcParameterName));
  rdgBoundaryConditions.Columns[Ord(rivcBottom)].PickList := FRealFieldGlobalsAndDataSetsNames;
  rdgBoundaryConditions.Columns[Ord(rivcStage)].PickList := FRealFieldGlobalsAndDataSetsNames;
  rdgBoundaryConditions.Columns[Ord(rivcConductance)].PickList := FRealFieldGlobalsAndDataSetsNames;

  while comboFormulaInterp.Items.Count > 3 do
  begin
    comboFormulaInterp.Items.Delete(comboFormulaInterp.Items.Count-1);
  end;
  comboFormulaInterp.Items.AddStrings(FStringFieldNames);
  rdgBoundaryConditions.Cells[Ord(rivcStartingTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[Ord(rivcEndingTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[Ord(rivcParameterName), 0] := StrParameterName;
  rdgBoundaryConditions.Cells[Ord(rivcBottom), 0] := 'River bottom';
  rdgBoundaryConditions.Cells[Ord(rivcStage), 0] := 'River stage';
  rdgBoundaryConditions.Cells[Ord(rivcConductance), 0] := 'Conductance';
end;

procedure TfrmImportShapefile.ImportModflowWelBoundary(
  AScreenObject: TScreenObject);
var
  AValue: Extended;
  Item: TWellItem;
  UseRow: Boolean;
  Index: Integer;
  AFormula: string;
  Param: TModflowTransientListParameter;
  ParameterName: string;
  AnItem: TCustomModflowBoundaryItem;
  ParamItem: TModflowParamItem;
  Boundary: TModflowParamBoundary;
begin
  AScreenObject.CreateWelBoundary;
  Boundary := AScreenObject.ModflowWellBoundary;
  ParamItem := nil;
  AScreenObject.ModflowWellBoundary.FormulaInterpretation :=
    GetFormulaInterpretation(comboFormulaInterp);

  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(welcStartingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(welcEndingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(welcPumpingRate), Index + 1] <> '');
    if UseRow then
    begin
      GetTransientParameter(Param, ParameterName, Ord(welcParameterName), Index+1);
      if (ParameterName <> '') and (Param = nil) then
      begin
        Continue;
      end;

      GetNewBoundaryItem(AnItem, ParameterName, Param, ParamItem, Boundary);
      Item := AnItem as TWellItem;

      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[Ord(welcStartingTime), Index + 1]);
      Item.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[Ord(welcEndingTime), Index + 1]);
      Item.EndTime := AValue;
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[Ord(welcPumpingRate), Index + 1]);
      Item.PumpingRate := AFormula;
    end;
  end;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForWEL;
begin
  plBoundary.ActivePage := jvspConductanceInterp;
  lblConductanceInterpretation.Caption := 'Pumping rate interpretation';
  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := Ord(welcPumpingRate)+1;
  AssignColFeatureProperties;
  
  rdgBoundaryConditions.Columns[Ord(welcStartingTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(welcEndingTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(welcParameterName)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(welcPumpingRate)].ComboUsed := True;

  rdgBoundaryConditions.Columns[Ord(welcStartingTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(welcEndingTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(welcParameterName)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(welcPumpingRate)].Format := rcf4String;

  rdgBoundaryConditions.Columns[Ord(welcStartingTime)].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(welcEndingTime)].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(welcParameterName)].PickList := FStringFieldNames;
  rdgBoundaryConditions.Columns[Ord(welcPumpingRate)].PickList := FRealFieldGlobalsAndDataSetsNames;
  AddParameterNamesToPickList(ptQ, Ord(welcParameterName));

  rdgBoundaryConditions.Cells[Ord(welcStartingTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[Ord(welcEndingTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[Ord(welcParameterName), 0] := StrParameterName;
  rdgBoundaryConditions.Cells[Ord(welcPumpingRate), 0] := 'Pumping rate';

  while comboFormulaInterp.Items.Count > 3 do
  begin
    comboFormulaInterp.Items.Delete(comboFormulaInterp.Items.Count-1);
  end;
  comboFormulaInterp.Items.AddStrings(FStringFieldNames);
end;

procedure TfrmImportShapefile.ImportModflowGhbBoundary(
  AScreenObject: TScreenObject);
var
  Index: Integer;
  UseRow: Boolean;
  Item: TGhbItem;
  AValue: Extended;
  AFormula: string;
  Param: TModflowTransientListParameter;
  ParameterName: string;
  AnItem: TCustomModflowBoundaryItem;
  ParamItem: TModflowParamItem;
  Boundary: TModflowParamBoundary;
begin
  AScreenObject.CreateGhbBoundary;
  Boundary := AScreenObject.ModflowGhbBoundary;
  ParamItem := nil;
  AScreenObject.ModflowGhbBoundary.FormulaInterpretation :=
    GetFormulaInterpretation(comboFormulaInterp);

  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(ghbcStartingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(ghbcEndingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(ghbcHead), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(ghbcConductance), Index + 1] <> '');
    if UseRow then
    begin
      GetTransientParameter(Param, ParameterName, Ord(ghbcParameterName), Index+1);
      if (ParameterName <> '') and (Param = nil) then
      begin
        Continue;
      end;
      GetNewBoundaryItem(AnItem, ParameterName, Param, ParamItem, Boundary);
      Item := AnItem as TGhbItem;

//      Item := AScreenObject.ModflowGhbBoundary.Values.Add as TGhbItem;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[Ord(ghbcStartingTime), Index + 1]);
      Item.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.Cells[Ord(ghbcEndingTime), Index + 1]);
      Item.EndTime := AValue;
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[Ord(ghbcHead), Index + 1]);
      Item.BoundaryHead := AFormula;
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.Cells[Ord(ghbcConductance), Index + 1]);
      Item.Conductance := AFormula;
    end;
  end;
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForGHB;
begin
  plBoundary.ActivePage := jvspConductanceInterp;
  lblConductanceInterpretation.Caption := 'Conductance interpretation';
  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := Ord(ghbcConductance)+1;
  AssignColFeatureProperties;

  rdgBoundaryConditions.Columns[Ord(ghbcStartingTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(ghbcEndingTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(ghbcParameterName)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(ghbcHead)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(ghbcConductance)].ComboUsed := True;

  rdgBoundaryConditions.Columns[Ord(ghbcStartingTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(ghbcEndingTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(ghbcParameterName)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(ghbcHead)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(ghbcConductance)].Format := rcf4String;

  rdgBoundaryConditions.Columns[Ord(ghbcStartingTime)].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(ghbcEndingTime)].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(ghbcParameterName)].PickList := FStringFieldNames;
  AddParameterNamesToPickList(ptGHB, Ord(ghbcParameterName));
  rdgBoundaryConditions.Columns[Ord(ghbcHead)].PickList := FRealFieldGlobalsAndDataSetsNames;
  rdgBoundaryConditions.Columns[Ord(ghbcConductance)].PickList := FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(ghbcStartingTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[Ord(ghbcEndingTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[Ord(ghbcParameterName), 0] := StrParameterName;
  rdgBoundaryConditions.Cells[Ord(ghbcHead), 0] := 'Boundary head';
  rdgBoundaryConditions.Cells[Ord(ghbcConductance), 0] := 'Conductance';

  while comboFormulaInterp.Items.Count > 3 do
  begin
    comboFormulaInterp.Items.Delete(comboFormulaInterp.Items.Count-1);
  end;
  comboFormulaInterp.Items.AddStrings(FStringFieldNames);
end;

procedure TfrmImportShapefile.InitializeBoundaryControlsForCHD;
begin
  plBoundary.ActivePage := jvspNone;
  rdgBoundaryConditions.Enabled := True;
  rdgBoundaryConditions.ColCount := Ord(ccEndingHead) + 1;
  AssignColFeatureProperties;
  rdgBoundaryConditions.Columns[Ord(ccStartingTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(ccEndingTime)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(ccParameterName)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(ccStartingHead)].ComboUsed := True;
  rdgBoundaryConditions.Columns[Ord(ccEndingHead)].ComboUsed := True;

  rdgBoundaryConditions.Columns[Ord(ccStartingTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(ccEndingTime)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(ccParameterName)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(ccStartingHead)].Format := rcf4String;
  rdgBoundaryConditions.Columns[Ord(ccEndingHead)].Format := rcf4String;

  rdgBoundaryConditions.Columns[Ord(ccStartingTime)].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(ccEndingTime)].PickList := FRealFieldNames;
  rdgBoundaryConditions.Columns[Ord(ccParameterName)].PickList := FStringFieldNames;
  AddParameterNamesToPickList(ptCHD, Ord(ccParameterName));
  rdgBoundaryConditions.Columns[Ord(ccStartingHead)].PickList := FRealFieldGlobalsAndDataSetsNames;
  rdgBoundaryConditions.Columns[Ord(ccEndingHead)].PickList := FRealFieldGlobalsAndDataSetsNames;

  rdgBoundaryConditions.Cells[Ord(ccStartingTime), 0] := StrStartingTime;
  rdgBoundaryConditions.Cells[Ord(ccEndingTime), 0] := StrEndingTime;
  rdgBoundaryConditions.Cells[Ord(ccParameterName), 0] := StrParameterName;
  rdgBoundaryConditions.Cells[Ord(ccStartingHead), 0] := 'Starting head';
  rdgBoundaryConditions.Cells[Ord(ccEndingHead), 0] := 'Ending head';
end;

procedure TfrmImportShapefile.ImportModflowChdBoundary(
  AScreenObject: TScreenObject);
var
  AValue: Extended;
  Item: TChdItem;
  UseRow: Boolean;
  Index: Integer;
  AFormula: string;
  ParameterName: string;
  Param: TModflowTransientListParameter;
  AnItem: TCustomModflowBoundaryItem;
  ParamItem: TModflowParamItem;
  Boundary: TModflowParamBoundary;
begin
  AScreenObject.CreateChdBoundary;
  Boundary := AScreenObject.ModflowChdBoundary;
  ParamItem := nil;
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[Ord(ccStartingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(ccEndingTime), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(ccStartingHead), Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[Ord(ccEndingHead), Index + 1] <> '');
    if UseRow then
    begin
      GetTransientParameter(Param, ParameterName, Ord(ccParameterName), Index+1);
      if (ParameterName <> '') and (Param = nil) then
      begin
        Continue;
      end;
      GetNewBoundaryItem(AnItem, ParameterName, Param, ParamItem, Boundary);
      Item := AnItem as TChdItem;

      AValue := GetRealValueFromText(rdgBoundaryConditions.
        Cells[Ord(ccStartingTime), Index + 1]);
      Item.StartTime := AValue;
      AValue := GetRealValueFromText(rdgBoundaryConditions.
        Cells[Ord(ccEndingTime), Index + 1]);
      Item.EndTime := AValue;
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.
        Cells[Ord(ccStartingHead), Index + 1]);
      Item.StartHead := AFormula;
      AFormula := GetRealFormulaFromText(rdgBoundaryConditions.
        Cells[Ord(ccEndingHead), Index + 1]);
      Item.EndHead := AFormula;
    end;
  end;
end;

procedure TfrmImportShapefile.EnableFeatureImport;
var
  ShouldEnable: boolean;
begin
  ShouldEnable := (comboJoinObjects.ItemIndex = 0)
    and (comboBoundaryChoice.Items.Count > 1);
  if ShouldEnable then
  begin
    case frmGoPhast.ModelSelection of
      msUndefined:
        begin
          Assert(False);
        end;
      msPhast:
        begin
          ShouldEnable := (rgEvaluatedAt.ItemIndex = 1);
        end;
      msModflow:
        begin
          ShouldEnable := (rgEvaluatedAt.ItemIndex = 0);
        end;
    else
      begin
        Assert(False);
      end;
    end;
  end;
  tabFeatures.TabVisible := ShouldEnable;
end;

procedure TfrmImportShapefile.AssignAPhastWellBoundary(
  AScreenObject: TScreenObject);
var
  AnInterval: TWellInterval;
  UseRow: Boolean;
  Index: Integer;
  WellElevationFormat: string;
  FieldNumber: Integer;
  FieldName: string;
  BooleanVariable: Boolean;
  AValue: Extended;
  Description: string;
begin
  Description := GetStringValueFromText(WellDescription.Text);
  AScreenObject.WellBoundary.Description := Description;
  AValue := GetRealValueFromText(comboWellDiameter.Text);
  AScreenObject.WellBoundary.Diameter := AValue;
  AValue := GetRealValueFromText(comboWellLandSurfaceDatum.Text);
  AScreenObject.WellBoundary.LandSurfaceDatum := AValue;
  BooleanVariable := GetBooleanValueFromText(comboWellPumpAllocation.Text);
  AScreenObject.WellBoundary.AllocateByPressureAndMobility := BooleanVariable;

  FieldName := comboWellIntervalStyle.Text;
  FieldNumber := xbShapeDataBase.GetFieldNumberFromName(FieldName);
  if FieldNumber = 0 then
  begin
    // not a field.
    if comboWellIntervalStyle.ItemIndex in [0, 1] then
    begin
      AScreenObject.WellBoundary.WellElevationFormat :=
        TWellElevationFormat(comboWellIntervalStyle.ItemIndex);
    end;
  end
  else
  begin
    WellElevationFormat := UpperCase(xbShapeDataBase.GetFieldStr(FieldName));
    if Pos('ELEVATION', WellElevationFormat) > 0 then
    begin
      AScreenObject.WellBoundary.WellElevationFormat := wefElevation;
    end
    else if Pos('DEPTH', WellElevationFormat) > 0 then
    begin
      AScreenObject.WellBoundary.WellElevationFormat := wefDepth;
    end;
  end;

  for Index := 0 to seWellIntervals.AsInteger - 1 do
  begin
    UseRow := (dgWellElevations.Cells[1, Index + 1] <> '')
      and (dgWellElevations.Cells[2, Index + 1] <> '');
    if UseRow then
    begin
      AnInterval := AScreenObject.WellBoundary.Intervals.Add as TWellInterval;
      AValue := GetRealValueFromText(dgWellElevations.Cells[1, Index + 1]);
      AnInterval.FirstElevation := AValue;
      AValue := GetRealValueFromText(dgWellElevations.Cells[2, Index + 1]);
      AnInterval.SecondElevation := AValue;
    end;
  end;
  AssignAPhastBoundary(AScreenObject.WellBoundary);
end;

function TfrmImportShapefile.GetBooleanValueFromText(
  FieldName: string): boolean;
var
  Value: string;
  FieldNumber: Integer;
begin
  FieldNumber := xbShapeDataBase.GetFieldNumberFromName(FieldName);
  if FieldNumber = 0 then
  begin
    // not a field
    FieldName := UpperCase(FieldName);
    result := (Length(FieldName) > 0)
      and ((FieldName[1] = 'T') or (FieldName[1] = 'Y'));
  end
  else
  begin
    Value := xbShapeDataBase.GetFieldStr(FieldName);
    if (Value = 'Y') or (Value = 'y') or (Value = 'T') or (Value = 't') then
    begin
      result := True;
    end
    else
    begin
      result := False;
    end;
  end;
end;

function TfrmImportShapefile.GetStringValueFromText(
  const FieldName: string): string;
var
  FieldNumber: Integer;
  CachedPosition: Integer;
  FieldStorage: TFieldNumStorage;
begin
  CachedPosition := FFieldNumbers.Indexof(FieldName);
  if CachedPosition >= 0 then
  begin
    FieldStorage := FFieldNumbers.Objects[CachedPosition] as TFieldNumStorage;
    FieldNumber := FieldStorage.FieldNumber;
  end
  else
  begin
    FieldNumber := xbShapeDataBase.GetFieldNumberFromName(FieldName);
    FieldStorage := TFieldNumStorage.Create(xbShapeDataBase);
    FieldStorage.FieldNumber := FieldNumber;
    FFieldNumbers.AddObject(FieldName, FieldStorage);
  end;
//  FieldNumber := xbShapeDataBase.GetFieldNumberFromName(FieldName);
  if FieldNumber = 0 then
  begin
    if Trim(FieldName) = '' then
    begin
      result := '""';
    end
    else
    begin
      result := Trim(FieldName);
      if result[1] <> '"' then
      begin
        result := '"' + result;
      end;
      if result[Length(result)] <> '"' then
      begin
        result := result + '"';
      end;
    end;
  end
  else
  begin
    result := xbShapeDataBase.GetFieldbyNumber(FieldNumber);
  end;
end;

procedure TfrmImportShapefile.AssignAPhastRiverBoundary(
  AScreenObject: TScreenObject);
var
  AFormula: string;
  Description: string;
begin
  Description := GetStringValueFromText(comboRiverDescripton.Text);
  AScreenObject.RiverBoundary.Description := Description;
  AFormula := GetRealFormulaFromText(comboRiverHydraulicConductivity.Text);
  AScreenObject.RiverBoundary.BedHydraulicConductivity := AFormula;
  AFormula := GetRealFormulaFromText(comboRiverWidth.Text);
  AScreenObject.RiverBoundary.Width := AFormula;
  AFormula := GetRealFormulaFromText(comboRiverDepth.Text);
  AScreenObject.RiverBoundary.Depth := AFormula;
  AFormula := GetRealFormulaFromText(comboRiverBedThickness.Text);
  AScreenObject.RiverBoundary.BedThickness := AFormula;
  AssignAPhastBoundary(AScreenObject.RiverBoundary);
end;

procedure TfrmImportShapefile.AssignAPhastLeakyBoundary(
  AScreenObject: TScreenObject);
var
  AFormula: string;
begin
  AFormula := GetRealFormulaFromText(comboLeakyHydraulicConductivity.Text);
  AScreenObject.LeakyBoundary.HydraulicConductivity := AFormula;
  AFormula := GetRealFormulaFromText(comboLeakyThickness.Text);
  AScreenObject.LeakyBoundary.Thickness := AFormula;
  AssignAPhastBoundary(AScreenObject.LeakyBoundary);
end;

function TfrmImportShapefile.DataArrayOrientationOK(DataArray: TDataArray): boolean;
begin
  Assert(DataArray <> nil);
  if rgElevationCount.ItemIndex = 0 then
  begin
    result := DataArray.Orientation = dsoTop;
  end
  else
  begin
    result := DataArray.Orientation in [dsoTop, dso3D];
  end;
end;

function TfrmImportShapefile.GetRealFormulaFromText(
  const Text: string; DataSetsOK: boolean = True): string;
var
  FieldNumber: Integer;
  DataArray: TDataArray;
  Variable: TGlobalVariable;
  CachedPosition: Integer;
  FieldStorage: TFieldNumStorage;
  Value: double;
begin
  CachedPosition := FFieldNumbers.Indexof(Text);
  if CachedPosition >= 0 then
  begin
    FieldStorage := FFieldNumbers.Objects[CachedPosition] as TFieldNumStorage;
    FieldNumber := FieldStorage.FieldNumber;
  end
  else
  begin
    FieldNumber := xbShapeDataBase.GetFieldNumberFromName(Text);
    FieldStorage := TFieldNumStorage.Create(xbShapeDataBase);
    FieldStorage.FieldNumber := FieldNumber;
    FFieldNumbers.AddObject(Text, FieldStorage);
  end;
//  FieldNumber := xbShapeDataBase.GetFieldNumberFromName(Text);
  if FieldNumber <> 0 then
  begin
    if xbShapeDataBase.GetFieldType(FieldNumber) = xbfChar then
    begin
      result := GetRealFormulaFromText(Trim(
        GetStringValueFromText(Text)), DataSetsOK);
    end
    else
    begin
      result := FloatToStr(GetRealValueFromText(Text));
    end;
  end
  else
  begin
    if FieldStorage.Formula <> '' then
    begin
      result := FieldStorage.Formula;
      Exit;
    end;
    if TryStrToFloat(Text, Value) then
    begin
      result := Text;
      FieldStorage.Formula := result;
      exit;
    end;
    if DataSetsOK then
    begin
      DataArray := frmGoPhast.PhastModel.GetDataSetByName(Text);
      if (DataArray <> nil) and
        DataArrayOrientationOK(DataArray) and
        (DataArray.DataType in [rdtDouble, rdtInteger]) then
      begin
        result := Text;
        FieldStorage.Formula := result;
        Exit;
      end;
    end;

    Variable := frmGoPhast.PhastModel.GlobalVariables.GetVariableByName(Text);
    if (Variable <> nil) and
      (Variable.Format in [rdtDouble, rdtInteger]) then
    begin
      result := Text;
      FieldStorage.Formula := result;
      Exit;
    end;

    result := FloatToStr(GetRealValueFromText(Text));
    FieldStorage.Formula := result;
  end;
end;

function TfrmImportShapefile.GetRealValueFromText(
  const FieldName: string): Extended;
var
  FieldNumber: Integer;
  CachedPosition: Integer;
  FieldStorage: TFieldNumStorage;
begin
  CachedPosition := FFieldNumbers.Indexof(FieldName);
  if CachedPosition >= 0 then
  begin
    FieldStorage := FFieldNumbers.Objects[CachedPosition] as TFieldNumStorage;
    FieldNumber := FieldStorage.FieldNumber;
  end
  else
  begin
    FieldNumber := xbShapeDataBase.GetFieldNumberFromName(FieldName);
    FieldStorage := TFieldNumStorage.Create(xbShapeDataBase);
    FieldStorage.FieldNumber := FieldNumber;
    FFieldNumbers.AddObject(FieldName, FieldStorage);
  end;
  if FieldNumber = 0 then
  begin
    // not a field
    if not TryStrToFloat(FieldName, result) then
    begin
      result := 0;
    end;
  end
  else
  begin
    result := xbShapeDataBase.GetFieldNum(FieldNumber);
  end;
end;

function TfrmImportShapefile.GetIntegerFormulaFromText(
  const Text: string; DataSetsOK: boolean = True): string;
var
  FieldNumber: Integer;
  DataArray: TDataArray;
  Variable: TGlobalVariable;
  CachedPosition: Integer;
  FieldStorage: TFieldNumStorage;
  Value: integer;
begin
  CachedPosition := FFieldNumbers.Indexof(Text);
  if CachedPosition >= 0 then
  begin
    FieldStorage := FFieldNumbers.Objects[CachedPosition] as TFieldNumStorage;
    FieldNumber := FieldStorage.FieldNumber;
  end
  else
  begin
    FieldNumber := xbShapeDataBase.GetFieldNumberFromName(Text);
    FieldStorage := TFieldNumStorage.Create(xbShapeDataBase);
    FieldStorage.FieldNumber := FieldNumber;
    FFieldNumbers.AddObject(Text, FieldStorage);
  end;
//  FieldNumber := xbShapeDataBase.GetFieldNumberFromName(Text);
  if FieldNumber <> 0 then
  begin
    if xbShapeDataBase.GetFieldType(FieldNumber) = xbfChar then
    begin
      result := GetIntegerFormulaFromText(Trim(
        GetStringValueFromText(Text)), DataSetsOK);
    end
    else
    begin
      Result := IntToStr(GetIntegerValueFromText(Text));
    end;
  end
  else
  begin
    if FieldStorage.Formula <> '' then
    begin
      result := FieldStorage.Formula;
      Exit;
    end;
    if TryStrToInt(Text, Value) then
    begin
      result := Text;
      FieldStorage.Formula := result;
      exit;
    end;
    if DataSetsOK then
    begin
      DataArray := frmGoPhast.PhastModel.GetDataSetByName(Text);
      if (DataArray <> nil) and
        DataArrayOrientationOK(DataArray) and
        (DataArray.DataType = rdtInteger) then
      begin
        result := Text;
        FieldStorage.Formula := result;
        Exit;
      end;
    end;

    Variable := frmGoPhast.PhastModel.GlobalVariables.GetVariableByName(Text);
    if (Variable <> nil) and
      (Variable.Format = rdtInteger) then
    begin
      result := Text;
      FieldStorage.Formula := result;
      Exit;
    end;

    Result := IntToStr(GetIntegerValueFromText(Text));
    FieldStorage.Formula := result;
  end;
end;


function TfrmImportShapefile.GetIntegerValueFromText(
  const FieldName: string): integer;
var
  FieldNumber: Integer;
  CachedPosition: Integer;
  FieldStorage: TFieldNumStorage;
begin
  CachedPosition := FFieldNumbers.Indexof(FieldName);
  if CachedPosition >= 0 then
  begin
    FieldStorage := FFieldNumbers.Objects[CachedPosition] as TFieldNumStorage;
    FieldNumber := FieldStorage.FieldNumber;
  end
  else
  begin
    FieldNumber := xbShapeDataBase.GetFieldNumberFromName(FieldName);
    FieldStorage := TFieldNumStorage.Create(xbShapeDataBase);
    FieldStorage.FieldNumber := FieldNumber;
    FFieldNumbers.AddObject(FieldName, FieldStorage);
  end;
//  FieldNumber := xbShapeDataBase.GetFieldNumberFromName(FieldName);
  if FieldNumber = 0 then
  begin
    // not a field
    if not TryStrToInt(FieldName, result) then
    begin
      result := 0;
    end;
  end
  else
  begin
    result := xbShapeDataBase.GetFieldInt(FieldNumber);
  end;
end;

procedure TfrmImportShapefile.AssignAPhastBoundary(
  Boundary: TCustomInterpolatedBoundary);
var
  IntItem: TIntegerPhastBoundaryCondition;
  AnInt: Integer;
  RealItem: TRealPhastBoundaryCondition;
  AValue: Extended;
  FieldNumber: Integer;
  FieldName: string;
  ATime: Extended;
  UseRow: Boolean;
  Index: Integer;
begin
  for Index := 0 to seBoundaryTimeCount.AsInteger - 1 do
  begin
    UseRow := (rdgBoundaryConditions.Cells[0, Index + 1] <> '')
      and (rdgBoundaryConditions.Cells[1, Index + 1] <> '');
    if UseRow then
    begin
      ATime := 0;
      FieldName := rdgBoundaryConditions.Cells[0, Index + 1];
      FieldNumber := xbShapeDataBase.GetFieldNumberFromName(FieldName);
      if FieldNumber = 0 then
      begin
        // not a field
        if not TryStrToFloat(FieldName, ATime) then
        begin
          Continue;
        end;
      end
      else
      begin
        ATime := xbShapeDataBase.GetFieldNum(FieldName);
      end;
      AValue := 0;
      FieldName := rdgBoundaryConditions.Cells[1, Index + 1];
      FieldNumber := xbShapeDataBase.GetFieldNumberFromName(FieldName);
      if FieldNumber = 0 then
      begin
        // not a field
        if not TryStrToFloat(FieldName, AValue) then
        begin
          Continue;
        end;
      end
      else
      begin
        AValue := xbShapeDataBase.GetFieldNum(FieldName);
      end;
      if (ATime = 0) and (Index < Boundary.BoundaryValue.Count) then
      begin
        RealItem := Boundary.BoundaryValue.Items[Index]
          as TRealPhastBoundaryCondition;
      end
      else
      begin
        RealItem := Boundary.BoundaryValue.Add as TRealPhastBoundaryCondition;
      end;
      RealItem.Time := ATime;
      RealItem.FormulaExpression := FloatToStr(AValue);
      AnInt := 0;
      FieldName := rdgBoundaryConditions.Cells[2, Index + 1];
      if FieldName <> '' then
      begin
        FieldNumber := xbShapeDataBase.GetFieldNumberFromName(FieldName);
        if FieldNumber = 0 then
        begin
          // not a field
          if not TryStrToInt(FieldName, AnInt) then
          begin
            Continue;
          end;
        end
        else
        begin
          AnInt := xbShapeDataBase.GetFieldInt(FieldName);
        end;
        if (ATime = 0) and (Index < Boundary.Solution.Count) then
        begin
          IntItem := Boundary.Solution.Items[Index]
            as TIntegerPhastBoundaryCondition;
        end
        else
        begin
          IntItem := Boundary.Solution.Add as TIntegerPhastBoundaryCondition;
        end;
        IntItem.Time := ATime;
        IntItem.FormulaExpression := IntToStr(AnInt);
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.AssignAPhastSpecifiedHeadBoundary(
  AScreenObject: TScreenObject);
var
  SolutionType: string;
  FieldNumber: Integer;
  FieldName: string;
begin
  FieldName := comboSolutionType.Text;
  FieldNumber := xbShapeDataBase.GetFieldNumberFromName(FieldName);
  if FieldNumber = 0 then
  begin
    // not a field.
    if comboSolutionType.ItemIndex in [0, 1] then
    begin
      AScreenObject.SpecifiedHeadBoundary.SolutionType :=
        TSolutionType(comboSolutionType.ItemIndex);
    end;
  end
  else
  begin
    SolutionType := UpperCase(xbShapeDataBase.GetFieldStr(FieldName));
    if Pos('SPECIFIED', SolutionType) > 0 then
    begin
      AScreenObject.SpecifiedHeadBoundary.SolutionType := stSpecified;
    end
    else if Pos('ASSOCIATED', SolutionType) > 0 then
    begin
      AScreenObject.SpecifiedHeadBoundary.SolutionType := stAssociated;
    end;
  end;
  AssignAPhastBoundary(AScreenObject.SpecifiedHeadBoundary);
end;

procedure TfrmImportShapefile.InitializeBoundaryConditionControls;
var
  Model: TPhastModel;
  Packages: TModflowPackages;
  Index: Integer;
  Item: TTimeItem;
begin
  plBoundary.ActivePage := jvspNone;
  Model := frmGoPhast.PhastModel;
  case Model.ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast:
      begin
        comboBoundaryChoice.Items.Add('Specified head');
        comboBoundaryChoice.Items.Add('Flux boundary');
        comboBoundaryChoice.Items.Add('Leaky boundary');
        comboBoundaryChoice.Items.Add('River boundary');
        comboBoundaryChoice.Items.Add('Well boundary');

        rdgBoundaryConditions.Cells[0,0] := 'Time';

        rdgBoundaryConditions.Columns[0].Format := rcf4Real;
        rdgBoundaryConditions.Columns[0].ComboUsed := True;

        rdgBoundaryConditions.Columns[0].PickList.Add(
          FloatToStr(Model.Times.StartTime.Value));
        for Index := 0 to Model.Times.Count - 1 do
        begin
          Item := Model.Times.Items[Index] as TTimeItem;
          rdgBoundaryConditions.Columns[0].PickList.Add(
            FloatToStr(Item.EndingTime));
        end;
      end;
    msModflow:
      begin
        Packages := Model.ModflowPackages;
        AddModflowPackageToImportChoices(Packages.ChdBoundary);
        AddModflowPackageToImportChoices(Packages.DrnPackage);
        AddModflowPackageToImportChoices(Packages.DrtPackage);
        AddModflowPackageToImportChoices(Packages.EtsPackage);
        AddModflowPackageToImportChoices(Packages.EvtPackage);
        AddModflowPackageToImportChoices(Packages.GhbBoundary);
        AddModflowPackageToImportChoices(Packages.HfbPackage);
        AddModflowPackageToImportChoices(Packages.HobPackage);
        AddModflowPackageToImportChoices(Packages.LakPackage);
        AddModflowPackageToImportChoices(Packages.Mnw2Package);
        AddModflowPackageToImportChoices(Packages.RchPackage);
        AddModflowPackageToImportChoices(Packages.ResPackage);
        AddModflowPackageToImportChoices(Packages.RivPackage);
        AddModflowPackageToImportChoices(Packages.SfrPackage);
        AddModflowPackageToImportChoices(Packages.UzfPackage);
        AddModflowPackageToImportChoices(Packages.WelPackage);
      end;
  else
    begin
      Assert(False);
    end;
  end;
  if comboBoundaryChoice.Items.Count <= 1 then
  begin
    pnlBoundaryCondition.Visible := false;
  end;
end;

procedure TfrmImportShapefile.EnableOK;
var
  ShouldEnable: boolean;
begin
  ShouldEnable := cbImportObjects.Checked
  or (cbImportGrid.Enabled and  cbImportGrid.Checked);
  if cbImportGrid.Enabled then
  begin
    EmphasizeCheckBoxes([cbImportObjects, cbImportGrid]);
  end
  else
  begin
    EmphasizeCheckBoxes([cbImportObjects]);
  end;
  if cbImportObjects.Checked then
  begin
    if cbEnclosedCells.Enabled then
    begin
      EmphasizeCheckBoxes([cbEnclosedCells, cbIntersectedCells,
        cbInterpolation]);
      ShouldEnable := cbEnclosedCells.Checked
        or cbIntersectedCells.Checked
        or cbInterpolation.Checked
    end
    else
    begin
      EmphasizeCheckBoxes([cbIntersectedCells, cbInterpolation]);
      ShouldEnable := cbIntersectedCells.Checked
        or cbInterpolation.Checked
    end;
  end;
  btnOK.Enabled := ShouldEnable;
end;

procedure TfrmImportShapefile.ImportGrid(FieldNames: TStringList);
var
  UndoCreateGrid: TUndoCreateGrid;
  ZeroPosition: Integer;
  DistanceToOrigin: Double;
  DeltaAngle: Double;
  ColumnAngle: Double;
  RowAngle: Double;
  ShapeObject: TShapeObject;
  Index: Integer;
  ColumnPositions: TRealList;
  RowPositions: TRealList;
  YCount: TIntegerList;
  XCount: TIntegerList;
  YIndex: Integer;
  XIndex: Integer;
  TestRowAngle, TestColAngle: double;
  FoundAngles: boolean;
  procedure NoGrid;
  begin
    Beep;
    MessageDlg('The Shapefile appears not to contain grid information.',
      mtInformation, [mbOK], 0);
  end;
begin
  frmProgress.Caption := 'Creating Grid';
  frmProgress.pbProgress.Max := FGeometryFile.Count;
  frmProgress.pbProgress.Position := 0;
  frmProgress.ProgressLabelCaption := '0 out of '
    + IntToStr(frmProgress.pbProgress.Max) + '.';
  frmProgress.Prefix := 'Shape ';
  frmProgress.PopupParent := self;
  frmProgress.Show;

  FoundAngles := False;
  TestRowAngle := 0;
  TestColAngle := 0;
  RowAngle := 0;
  XIndex := FieldNames.IndexOf('X_INDEX');
  YIndex := FieldNames.IndexOf('Y_INDEX');
  Assert((XIndex >= 0) and (YIndex >= 0));
  XCount := TIntegerList.Create;
  YCount := TIntegerList.Create;
  RowPositions := TRealList.Create;
  ColumnPositions := TRealList.Create;
  try
    xbShapeDataBase.GotoBOF;
    for Index := 0 to FGeometryFile.Count - 1 do
    begin
      ShapeObject := FGeometryFile[Index];
      if (ShapeObject.FNumPoints <> 5) or (ShapeObject.FNumParts <> 1) then
      begin
        NoGrid;
        Exit;
      end;
      RowAngle := ArcTan2((ShapeObject.FPoints[0].Y - ShapeObject.FPoints[1].Y),
        (ShapeObject.FPoints[0].X - ShapeObject.FPoints[1].X));
      if RowAngle > Pi / 2 then
      begin
        RowAngle := RowAngle - Pi;
      end;
      if RowAngle <= -Pi / 2 then
      begin
        RowAngle := RowAngle + Pi;
      end;

      ColumnAngle := ArcTan2(
        (ShapeObject.FPoints[1].Y - ShapeObject.FPoints[2].Y),
        (ShapeObject.FPoints[1].X - ShapeObject.FPoints[2].X));
      if ColumnAngle > Pi / 2 then
      begin
        ColumnAngle := ColumnAngle - Pi;
      end;
      if ColumnAngle <= -Pi / 2 then
      begin
        ColumnAngle := ColumnAngle + Pi;
      end;

      if FoundAngles then
      begin
        if Abs(TestRowAngle - RowAngle) > 1E-08 then
        begin
          NoGrid;
          Exit;
        end;
        if Abs(TestColAngle - ColumnAngle) > 1E-08 then
        begin
          NoGrid;
          Exit;
        end;
      end
      else
      begin
        DeltaAngle := Abs(RowAngle - ColumnAngle) - Pi / 2;
        if DeltaAngle > 1E-08 then
        begin
          NoGrid;
          Exit;
        end;
        TestRowAngle := RowAngle;
        TestColAngle := ColumnAngle;
        FoundAngles := True;
      end;

      RowAngle := ArcTan2((ShapeObject.FPoints[3].Y - ShapeObject.FPoints[2].Y),
        (ShapeObject.FPoints[3].X - ShapeObject.FPoints[2].X));
      if RowAngle > Pi / 2 then
      begin
        RowAngle := RowAngle - Pi;
      end;
      if RowAngle <= -Pi / 2 then
      begin
        RowAngle := RowAngle + Pi;
      end;

      ColumnAngle := ArcTan2(
        (ShapeObject.FPoints[4].Y - ShapeObject.FPoints[3].Y),
        (ShapeObject.FPoints[4].X - ShapeObject.FPoints[3].X));
      if ColumnAngle > Pi / 2 then
      begin
        ColumnAngle := ColumnAngle - Pi;
      end;
      if ColumnAngle <= -Pi / 2 then
      begin
        ColumnAngle := ColumnAngle + Pi;
      end;

      if Abs(TestRowAngle - RowAngle) > 1E-08 then
      begin
        NoGrid;
        Exit;
      end;
      if Abs(TestColAngle - ColumnAngle) > 1E-08 then
      begin
        NoGrid;
        Exit;
      end;

      XIndex := xbShapeDataBase.GetFieldInt('X_INDEX');
      Assert(XIndex >= 1);
      YIndex := xbShapeDataBase.GetFieldInt('Y_INDEX');
      Assert(YIndex >= 1);
      while XCount.Count <= XIndex do
      begin
        XCount.Add(0);
      end;
      while ColumnPositions.Count <= XIndex do
      begin
        ColumnPositions.Add(0);
      end;
      if DistanceOriginToLine(ShapeObject.FPoints[1], ShapeObject.FPoints[2],
        DistanceToOrigin) then
      begin
        XCount[XIndex - 1] := XCount[XIndex - 1] + 1;
        ColumnPositions[XIndex - 1] :=
          ColumnPositions[XIndex - 1] + DistanceToOrigin;
      end;
      if DistanceOriginToLine(ShapeObject.FPoints[4], ShapeObject.FPoints[3],
        DistanceToOrigin) then
      begin
        XCount[XIndex] := XCount[XIndex] + 1;
        ColumnPositions[XIndex] := ColumnPositions[XIndex] + DistanceToOrigin;
      end;
      while YCount.Count <= YIndex do
      begin
        YCount.Add(0);
      end;
      while RowPositions.Count <= YIndex do
      begin
        RowPositions.Add(0);
      end;
      if DistanceOriginToLine(ShapeObject.FPoints[0], ShapeObject.FPoints[1],
        DistanceToOrigin) then
      begin
        YCount[YIndex] := YCount[YIndex] + 1;
        RowPositions[YIndex] := RowPositions[YIndex] + DistanceToOrigin;
      end;
      if DistanceOriginToLine(ShapeObject.FPoints[3], ShapeObject.FPoints[2],
        DistanceToOrigin) then
      begin
        YCount[YIndex - 1] := YCount[YIndex - 1] + 1;
        RowPositions[YIndex - 1] :=
          RowPositions[YIndex - 1] + DistanceToOrigin;
      end;
      xbShapeDataBase.GotoNext;
      frmProgress.StepIt;
      Application.ProcessMessages;
    end;
    ZeroPosition := XCount.IndexOf(0);
    while ZeroPosition >= 0 do
    begin
      XCount.Delete(ZeroPosition);
      ColumnPositions.Delete(ZeroPosition);
      ZeroPosition := XCount.IndexOf(0);
    end;
    ZeroPosition := YCount.IndexOf(0);
    while ZeroPosition >= 0 do
    begin
      YCount.Delete(ZeroPosition);
      RowPositions.Delete(ZeroPosition);
      ZeroPosition := YCount.IndexOf(0);
    end;
    if (XCount.Count = 0) or (YCount.Count = 0) then
    begin
      MessageDlg('Unable to import grid.', mtInformation, [mbOK], 0);
    end
    else
    begin
      UndoCreateGrid := TUndoCreateGrid.Create;
      try
        SetLength(UndoCreateGrid.FNewColumns, ColumnPositions.Count);
        for Index := 0 to ColumnPositions.Count - 1 do
        begin
          UndoCreateGrid.FNewColumns[Index] :=
            ColumnPositions[Index] / XCount[Index];
        end;
        SetLength(UndoCreateGrid.FNewRows, RowPositions.Count);
        for Index := 0 to RowPositions.Count - 1 do
        begin
          UndoCreateGrid.FNewRows[Index] :=
            RowPositions[Index] / YCount[Index];
        end;
        SetLength(UndoCreateGrid.FNewLayerElevations, 0);

        UndoCreateGrid.NewAngle := RowAngle;
      except
        UndoCreateGrid.Free;
        raise ;
      end;
      frmGoPhast.UndoStack.Submit(UndoCreateGrid);
    end;
  finally
    XCount.Free;
    YCount.Free;
    RowPositions.Free;
    ColumnPositions.Free;
  end;
end;

function TfrmImportShapefile.CheckDataSets: boolean;
var
  UsedDataSets: TStringList;
  Position: integer;
  DataSetName: string;
  Index: integer;
  DataSet: TDataArray;
  FieldIndex: integer;
begin
  result := False;
  UsedDataSets := TStringList.Create;
  try
    for Index := 1 to dgFields.RowCount - 1 do
    begin
      DataSetName := dgFields.Cells[Ord(fgcDataSet), Index];
      if dgFields.Checked[Ord(fgcImport), Index] and
        (DataSetName <> rsNewDataSet) then
      begin
        Position := UsedDataSets.IndexOf(DataSetName);
        if Position >= 0 then
        begin
          Beep;
          MessageDlg('"' + DataSetName + '" has been selected for two '
            + 'or more fields that are being imported.  You need to '
            + 'correct this before continuing.', mtWarning, [mbOK], 0);
          Exit;
        end;
        UsedDataSets.Add(DataSetName);
        DataSet := frmGoPhast.PhastModel.GetDataSetByName(DataSetName);
        Assert(DataSet <> nil);
//        DataSet := frmGoPhast.PhastModel.DataSets[Position];
        FieldIndex := xbShapeDataBase.GetFieldNumberFromName(dgFields.
          Cells[0, Index]);
        Assert(FieldIndex >= 1);
        case DataSet.DataType of
          rdtInteger:
            begin
              if (xbShapeDataBase.GetFieldType(FieldIndex) <> xbfNumber)
                or (xbShapeDataBase.GetFieldDecimals(FieldIndex) <> 0) then
              begin
                Beep;
                MessageDlg('"' + DataSetName + '" contains integers but '
                  + dgFields.Cells[Ord(fgcAttributes), Index]
                  + ' does not.  You need to '
                  + 'correct this before continuing.', mtWarning, [mbOK], 0);
                Exit;
              end;
            end;
          rdtDouble:
            begin
              if xbShapeDataBase.GetFieldType(FieldIndex) <> xbfNumber then
              begin
                Beep;
                MessageDlg('"' + DataSetName + '" contains real numbers but '
                  + dgFields.Cells[Ord(fgcAttributes), Index]
                  + ' does not.  You need to '
                  + 'correct this before continuing.', mtWarning, [mbOK], 0);
                Exit;
              end;
            end;
          rdtBoolean:
            begin
              if xbShapeDataBase.GetFieldType(FieldIndex) <> xbfLogic then
              begin
                Beep;
                MessageDlg('"' + DataSetName + '" contains booleans but '
                  + dgFields.Cells[Ord(fgcAttributes), Index]
                  + ' does not.  You need to '
                  + 'correct this before continuing.', mtWarning, [mbOK], 0);
                Exit;
              end;
            end;
          rdtString:
            begin
              if xbShapeDataBase.GetFieldType(FieldIndex) <> xbfChar then
              begin
                Beep;
                MessageDlg('"' + DataSetName + '" contains strings but '
                  + dgFields.Cells[Ord(fgcAttributes), Index]
                  + ' does not.  You need to '
                  + 'correct this before continuing.', mtWarning, [mbOK], 0);
                Exit;
              end;
            end;
        else
          Assert(False);
        end;
      end;
    end;
    result := True;
  finally
    UsedDataSets.Free;
  end;
end;

procedure TfrmImportShapefile.MakeNewDataSets(NewDataSets: TList);
var
  Index: integer;
  NewDataSetName: string;
  DataSet: TDataArray;
  FieldIndex: integer;
  NewFormula: string;
  NewDataType: TRbwDataType;
  NewProperties: TPhastDataSetStorage;
  OldProperties: TPhastDataSetStorage;
begin
  for Index := 1 to dgFields.RowCount - 1 do
  begin
    if dgFields.Checked[Ord(fgcImport), Index] and
      (dgFields.Cells[Ord(fgcDataSet), Index] = rsNewDataSet) then
    begin
      NewDataSetName := GenerateNewName(dgFields.
        Cells[Ord(fgcAttributes), Index]);

      FieldIndex := xbShapeDataBase.GetFieldNumberFromName(dgFields.
        Cells[Ord(fgcAttributes), Index]);
      Assert(FieldIndex >= 1);

      NewDataType := rdtDouble;
      case xbShapeDataBase.GetFieldType(FieldIndex) of
        xbfChar:
          begin
            NewDataType := rdtString;
            NewFormula := '""';
          end;
        xbfNumber:
          begin
            if xbShapeDataBase.GetFieldDecimals(FieldIndex) = 0 then
            begin
              NewDataType := rdtInteger;
              NewFormula := '0';
            end
            else
            begin
              NewDataType := rdtDouble;
              NewFormula := '0.';
            end;
          end;
        xbfLogic:
          begin
            NewDataType := rdtBoolean;
            NewFormula := 'False';
          end;
      else
        Assert(False);
      end;

      DataSet := frmGoPhast.PhastModel.CreateNewDataArray(TDataArray,
        NewDataSetName, NewFormula, [], NewDataType,
        TEvaluatedAt(rgEvaluatedAt.ItemIndex), dsoTop,
        strDefaultClassification + '|Created from Shapefile Attribute');

      NewDataSets.Add(DataSet);

      DataSet.Units := '';
      AssignInterpolator(DataSet, Index, NewProperties, OldProperties);
      NewProperties.Free;
      OldProperties.Free;

      DataSet.UpdateDimensions(frmGoPhast.Grid.LayerCount,
        frmGoPhast.Grid.RowCount, frmGoPhast.Grid.ColumnCount);

      dgFields.Cells[Ord(fgcDataSet), Index] := NewDataSetName;
    end;
  end;
end;

procedure TfrmImportShapefile.SetData;
var
  Index: integer;
  ShapeObject: TShapeObject;
  AScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  ScreenObjectList: TList;
  PointIndex: integer;
  DataSetIndex: integer;
  DataSetName: string;
  Position: integer;
  DataSet: TDataArray;
  Value: string;
  DataSets: TList;
  FieldNames: TStringList;
  Undo: TUndoImportShapefile;
  EvalAt: TEvaluatedAt;
  Root: string;
  CentralMeridian: double;
  CentralMeridianDegrees: double;
  X, Y: double;
  ShapePoint: TShapePoint;
  PointRecord: TPoint2D;
  Variables: TList;
  IntVariable: TIntegerVariable;
  RealVariable: TRealVariable;
  BooleanVariable: TBooleanVariable;
  StringVariable: TStringVariable;
  AFormula: string;
  StringFormula: string;
  ImportCriterionExpression: TExpression;
  ZExpression: TExpression;
  HighZExpression: TExpression;
  LowZExpression: TExpression;
  Variable: TCustomVariable;
  InvalidObjectNumbers: TIntegerList;
  InvalidFormulaNumbers: TIntegerList;
  ErrorString: string;
  ExistingObjectCount: integer;
  SectionIndex: Integer;
  NextStart: integer;
  NewSection: boolean;
  CombinedObjects: boolean;
  Formula: string;
  ValueIndex: Integer;
  OptionalExtensions: TStringList;
  OptionalFileName: string;
  Item: TValueArrayItem;
  MultiValueList: TList;
  ValueList: TValueArrayStorage;
  DeleteCount, AddCount: integer;
  NewDataSets: TList;
  NewProperties: TList;
  OldProperties: TList;
  FieldName: string;
  RealFieldNames: TStringList;
  InvalidParametersIndex: Integer;
const
  WarningRoot = 'No parameters with the following names exist. '
    + 'Import of the feature will be skipped for the shapes for which '
    + ' these names were specified.';
begin
  FInvalidParameterNames := TStringList.Create;
  try
    FInvalidParameterNames.Sorted := True;
    FInvalidParameterNames.Duplicates := dupIgnore;
    frmGoPhast.ChangingSelection := True;
    CombinedObjects := comboJoinObjects.Enabled
      and (comboJoinObjects.ItemIndex = 1);
    GlobalDecompileType := dcValue;
    InvalidObjectNumbers := TIntegerList.Create;
    InvalidFormulaNumbers := nil;
    MultiValueList := TList.Create;
    NewDataSets:= TList.Create;
    NewProperties := TObjectList.Create;
    OldProperties := TObjectList.Create;
    try
      CentralMeridian := 0;
      CentralMeridianDegrees := 0;
      if cbCoordinateConversion.Checked then
      begin
        CentralMeridianDegrees := seZoneNumber.Value * 6 - 183;
        CentralMeridian := CentralMeridianDegrees / 180 * Pi;
      end;

      EvalAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
      frmGoPhast.CanDraw := False;
      try
        if not CheckDataSets then
        begin
          ModalResult := mrNone;
          Exit;
        end;
        MakeNewDataSets(NewDataSets);
        ChangeInterpolators(NewProperties, OldProperties);

        Variables := TList.Create;
        DataSets := TList.Create;
        FieldNames := TStringList.Create;
        RealFieldNames := TStringList.Create;
        try
          rpShapeCompiler.ClearVariables;
          rpShapeCompiler.ClearExpressions;
          CreateVariables(rpShapeCompiler);
          CreateDataSetVariables(rpShapeCompiler);
          frmGoPhast.PhastModel.RegisterGlobalVariables(rpShapeCompiler);
          for DataSetIndex := 1 to dgFields.RowCount - 1 do
          begin
            DataSetName := dgFields.Cells[Ord(fgcDataSet), DataSetIndex];
            if dgFields.Checked[Ord(fgcImport), DataSetIndex] then
            begin
              DataSet := frmGoPhast.PhastModel.GetDataSetByName(DataSetName);
              Assert(DataSet <> nil);
  //            DataSet := frmGoPhast.PhastModel.DataSets[Position];
              DataSets.Add(DataSet);
            end
            else
            begin
              DataSets.Add(nil);
            end;
            FieldName := dgFields.Cells[Ord(fgcAttributes), DataSetIndex];
            if FieldName <> '' then
            begin
              RealFieldNames.Add(FieldName);
              FieldName := FieldToVarName(FieldName);
              FieldNames.Add(FieldName);
              Variable := rpShapeCompiler.Variables[rpShapeCompiler.
                IndexOfVariable(FieldName)] as TCustomVariable;
              Variables.Add(Variable);
            end;
          end;
          AFormula := edImportCriterion.Text;
          GlobalDecompileType := dcNormal;
          try
            rpShapeCompiler.Compile(AFormula);
          finally
            GlobalDecompileType := dcValue;
          end;
          ImportCriterionExpression := rpShapeCompiler.CurrentExpression;
          Assert(ImportCriterionExpression.ResultType = rdtBoolean);

          case rgElevationCount.ItemIndex of
            0:
              begin
                ZExpression := nil;
                HighZExpression := nil;
                LowZExpression := nil;
              end;
            1:
              begin
                AFormula := edZ.Text;
                GlobalDecompileType := dcNormal;
                try
                  rpShapeCompiler.Compile(AFormula);
                finally
                  GlobalDecompileType := dcValue;
                end;
                ZExpression := rpShapeCompiler.CurrentExpression;
                Assert(ZExpression.ResultType in [rdtDouble, rdtInteger]);

                HighZExpression := nil;
                LowZExpression := nil;
              end;
            2:
              begin
                ZExpression := nil;

                AFormula := edHighZ.Text;
                GlobalDecompileType := dcNormal;
                try
                  rpShapeCompiler.Compile(AFormula);
                finally
                  GlobalDecompileType := dcValue;
                end;
                HighZExpression := rpShapeCompiler.CurrentExpression;
                Assert(HighZExpression.ResultType in [rdtDouble, rdtInteger]);

                AFormula := edLowZ.Text;
                GlobalDecompileType := dcNormal;
                try
                  rpShapeCompiler.Compile(AFormula);
                finally
                  GlobalDecompileType := dcValue;
                end;
                LowZExpression := rpShapeCompiler.CurrentExpression;
                Assert(LowZExpression.ResultType in [rdtDouble, rdtInteger]);
              end;
            else
              begin
                ZExpression := nil;
                HighZExpression := nil;
                LowZExpression := nil;
                Assert(False);
              end;
          end;

          Root := TScreenObject.ValidName(
            ExtractFileRoot(OpenDialogShape.FileName)+ '_');

          if cbImportGrid.Enabled and cbImportGrid.Checked then
          begin
            ImportGrid(FieldNames);
          end;

          if cbImportObjects.Checked then
          begin
            frmProgress.Caption := 'Creating Objects';
            frmProgress.pbProgress.Max := FGeometryFile.Count;
            frmProgress.pbProgress.Position := 0;
            frmProgress.ProgressLabelCaption := '0 out of '
              + IntToStr(frmProgress.pbProgress.Max) + '.';
            frmProgress.Prefix := 'Shape ';
            frmProgress.PopupParent := self;
            frmProgress.Show;

            if FDataBaseFileName <> '' then
            begin
              xbShapeDataBase.GotoBOF;
            end;
            frmGoPhast.PhastModel.BeginScreenObjectUpdate;
            ScreenObjectList := TList.Create;
            try
              ExistingObjectCount :=
                frmGoPhast.PhastModel.
                NumberOfLargestScreenObjectsStartingWith(Root);

              Undo := TUndoImportShapefile.Create;
              try
                if CombinedObjects then
                begin
                  ScreenObjectList.Capacity := 1;
                end
                else
                begin
                  ScreenObjectList.Capacity := FGeometryFile.Count;
                end;
                AScreenObject := nil;
                DeleteCount := 0;
                AddCount := 0;
                for Index := 0 to FGeometryFile.Count - 1 do
                begin
                  ShapeObject := FGeometryFile[Index];
                  FNumPointsInCurrentShape := ShapeObject.FNumPoints;
                  if not CombinedObjects or (Index = 0) then
                  begin
                    AScreenObject := TScreenObject.CreateWithViewDirection(
                      frmGoPhast.PhastModel, vdTop,
                      UndoCreateScreenObject, False);
                    AScreenObject.ElevationCount :=
                      TElevationCount(rgElevationCount.ItemIndex);
                    if CombinedObjects then
                    begin
                      case AScreenObject.ElevationCount of
                        ecZero: ; // do nothing
                        ecOne:
                          begin
                            AScreenObject.ElevationFormula :=
                              rsObjectImportedValuesR
                              + '("' + StrImportedElevations + '")';
                          end;
                        ecTwo:
                          begin
                            AScreenObject.HigherElevationFormula :=
                              rsObjectImportedValuesR
                              + '("' + StrImportedHigherElev + '")';
                            AScreenObject.LowerElevationFormula :=
                              rsObjectImportedValuesR
                              + '("' + StrImportedLowerEleva + '")';
                          end;
                        else Assert(False);
                      end;
                    end;

                    Inc(ExistingObjectCount);
                    AScreenObject.Name := Root + IntToStr(ExistingObjectCount);
                    AScreenObject.SetValuesOfEnclosedCells :=
                      cbEnclosedCells.Enabled and cbEnclosedCells.Checked;
                    AScreenObject.SetValuesOfIntersectedCells :=
                      cbIntersectedCells.Checked;
                    AScreenObject.SetValuesByInterpolation :=
                      cbInterpolation.Checked;
                    AScreenObject.EvaluatedAt := EvalAt;
                    AScreenObject.Selected := comboVisibility.ItemIndex = 0;
                    AScreenObject.Visible := comboVisibility.ItemIndex <= 1;
                    if CombinedObjects then
                    begin
                      for DataSetIndex := 0 to DataSets.Count - 1 do
                      begin
                        DataSet := DataSets[DataSetIndex];
                        if DataSet = nil then
                        begin
                          MultiValueList.Add(nil);
                        end
                        else
                        begin
                          Item := AScreenObject.
                            ImportedValues.Add as TValueArrayItem;
                          Item.Name := DataSet.Name;
                          Item.Values.DataType := DataSet.DataType;
                          Item.Values.Count := FGeometryFile.Count;
                          MultiValueList.Add(Item.Values);
                        end;
                      end;
                    end;
                  end;
                  try
                    AScreenObject.Invalidate;

                    if FDataBaseFileName = '' then
                    begin
                      if CombinedObjects then
                      begin
                        if ScreenObjectList.Count = 0 then
                        begin
                          ScreenObjectList.Add(AScreenObject);
                        end;
                      end
                      else
                      begin
                        ScreenObjectList.Add(AScreenObject);
                      end;
                    end
                    else
                    begin
                      Assert(DataSets.Count = Variables.Count);
                      Assert(DataSets.Count = FieldNames.Count);

                      for DataSetIndex := 0 to DataSets.Count - 1 do
                      begin
                        DataSet := DataSets[DataSetIndex];
                        if not CombinedObjects or (Index = 0) then
                        begin
                          if DataSet <> nil then
                          begin
                            Position := AScreenObject.AddDataSet(DataSet);
                            Assert(Position >= 0);
                          end
                          else
                          begin
                            Position := -1;
                          end;
                        end
                        else
                        begin
                          Position := AScreenObject.IndexOfDataSet(DataSet);
                        end;

                        Variable := Variables[DataSetIndex];
                        ValueList := nil;
                        if CombinedObjects then
                        begin
                          ValueList := MultiValueList[DataSetIndex];
                        end;
                        case Variable.ResultType of
                          rdtDouble:
                            begin
                              RealVariable := Variables[DataSetIndex];
                              RealVariable.Value :=
                                xbShapeDataBase.GetFieldNum(
                                RealFieldNames[DataSetIndex]);
                              if DataSet <> nil then
                              begin
                                if CombinedObjects then
                                begin
                                  ValueList.RealValues[Index-DeleteCount+AddCount]
                                    := RealVariable.Value;
                                end
                                else
                                begin
                                  AScreenObject.DataSetFormulas[Position]
                                    := FloatToStr(RealVariable.Value);
                                end;
                              end;
                            end;
                          rdtInteger:
                            begin
                              IntVariable := Variables[DataSetIndex];
                              IntVariable.Value :=
                                xbShapeDataBase.GetFieldInt(
                                RealFieldNames[DataSetIndex]);
                              if DataSet <> nil then
                              begin
                                if CombinedObjects then
                                begin
                                  if DataSet.DataType = rdtDouble then
                                  begin
                                    ValueList.RealValues[Index-DeleteCount+AddCount]
                                      := IntVariable.Value;
                                  end
                                  else
                                  begin
                                    ValueList.IntValues[Index-DeleteCount+AddCount]
                                      := IntVariable.Value;
                                  end;
                                end
                                else
                                begin
                                  AScreenObject.DataSetFormulas[Position]
                                    := IntToStr(IntVariable.Value);
                                end;
                              end;
                            end;
                          rdtBoolean:
                            begin
                              BooleanVariable := Variables[DataSetIndex];
                              Value :=
                                xbShapeDataBase.GetFieldStr(
                                RealFieldNames[DataSetIndex]);
                              if (Value = 'Y') or (Value = 'y')
                                or (Value = 'T') or (Value = 't') then
                              begin
                                BooleanVariable.Value := True;
                                if DataSet <> nil then
                                begin
                                  if CombinedObjects then
                                  begin
                                    ValueList.BooleanValues[
                                      Index-DeleteCount+AddCount] := True;
                                  end
                                  else
                                  begin
                                    AScreenObject.DataSetFormulas[
                                      Position] := 'True';
                                  end;
                                end;
                              end
                              else
                              begin
                                BooleanVariable.Value := False;
                                if DataSet <> nil then
                                begin
                                  if CombinedObjects then
                                  begin
                                    ValueList.BooleanValues[
                                      Index-DeleteCount+AddCount] := False;
                                  end
                                  else
                                  begin
                                    AScreenObject.DataSetFormulas[Position]
                                      := 'False';
                                  end;
                                end;
                              end;
                            end;
                          rdtString:
                            begin
                              StringVariable := Variables[DataSetIndex];
                              StringFormula := xbShapeDataBase.GetFieldStr(
                                RealFieldNames[DataSetIndex]);
                              StringFormula := StringReplace(StringFormula, '"', '''', [rfReplaceAll]);
                              StringVariable.Value := StringFormula;
                              if DataSet <> nil then
                              begin
                                if CombinedObjects then
                                begin
                                  ValueList.StringValues[Index-DeleteCount+AddCount]
                                    := StringVariable.Value;
                                end
                                else
                                begin
                                  AScreenObject.DataSetFormulas[Position] := '"'
                                    + StringVariable.Value + '"';
                                end;
                              end;
                            end;
                        else
                          Assert(False);
                        end;
                      end;
                      ImportCriterionExpression.Evaluate;
                      if ImportCriterionExpression.BooleanResult then
                      begin
                        AssignBoundary(AScreenObject);
                        if CombinedObjects then
                        begin
                          if ScreenObjectList.Count = 0 then
                          begin
                            ScreenObjectList.Add(AScreenObject);
                          end;
                        end
                        else
                        begin
                          ScreenObjectList.Add(AScreenObject);
                        end;
                      end
                      else
                      begin
                        if CombinedObjects then
                        begin
                          Inc(DeleteCount);
                        end
                        else
                        begin
                          AScreenObject.Free;
                        end;
                        xbShapeDataBase.GotoNext;
                        frmProgress.StepIt;
                        Application.ProcessMessages;
                        Continue;
                      end;
                    end;

                    case AScreenObject.ElevationCount of
                      ecZero: ; // do nothing
                      ecOne:
                        begin
                          ZExpression.Evaluate;
                          if CombinedObjects then
                          begin
                            AScreenObject.ImportedSectionElevations.Add
                              (ZExpression.DoubleResult);
                          end
                          else
                          begin
                            AScreenObject.ElevationFormula
                              := ZExpression.Decompile;
  //                            := FloatToStr(ZExpression.DoubleResult);
                          end;
                        end;
                      ecTwo:
                        begin
                          HighZExpression.Evaluate;
                          LowZExpression.Evaluate;
                          if CombinedObjects then
                          begin
                            AScreenObject.ImportedHigherSectionElevations.Add
                              (HighZExpression.DoubleResult);
                            AScreenObject.ImportedLowerSectionElevations.Add
                              (LowZExpression.DoubleResult);
                          end
                          else
                          begin
                            AScreenObject.HigherElevationFormula
                              := HighZExpression.Decompile;
                            AScreenObject.LowerElevationFormula
                              := LowZExpression.Decompile;
                          end;
                        end;
                      else Assert(False)
                    end;


                    if CombinedObjects then
                    begin
                      if Index = 0 then
                      begin
                        AScreenObject.Capacity := ShapeObject.FNumPoints;
                      end
                      else
                      begin
                        AScreenObject.Capacity := AScreenObject.Capacity
                          + ShapeObject.FNumPoints;
                      end;
                    end
                    else
                    begin
                      AScreenObject.Capacity := ShapeObject.FNumPoints;
                    end;
                    SectionIndex := 0;
                    NextStart := -1;
                    if (SectionIndex < Length(ShapeObject.FParts)) then
                    begin
                      NextStart := ShapeObject.FParts[SectionIndex];
                    end;
                    AScreenObject.BeginUpdate;
                    try
                    for PointIndex := 0 to ShapeObject.FNumPoints - 1 do
                    begin
                      NewSection := (PointIndex = NextStart) or (PointIndex = 0);
                      if PointIndex = NextStart then
                      begin
                        Inc(SectionIndex);
                        if (SectionIndex < Length(ShapeObject.FParts)) then
                        begin
                          NextStart := ShapeObject.FParts[SectionIndex];
                        end;
                      end;
                      ShapePoint := ShapeObject.FPoints[PointIndex];
                      if cbCoordinateConversion.Checked then
                      begin
                        X := ShapePoint.X;
                        Y := ShapePoint.Y;
                        if X > CentralMeridianDegrees + 180 then
                        begin
                          X := X - 360;
                        end
                        else if X < CentralMeridianDegrees - 180 then
                        begin
                          X := X + 360;
                        end;

                        ConvertToUTM(Y / 180 * Pi, X / 180 * Pi,
                          CentralMeridian, X, Y);
                        PointRecord.X := X;
                        PointRecord.Y := Y;
                        AScreenObject.AddPoint(PointRecord, NewSection);
                      end
                      else
                      begin
                        AScreenObject.AddPoint(ConvertPoint(ShapePoint),
                          NewSection);
                      end;
                      if CombinedObjects and NewSection and (PointIndex > 0) then
                      begin
                        Inc(AddCount);
                        for ValueIndex := 0 to MultiValueList.Count - 1 do
                        begin
                          ValueList := MultiValueList[ValueIndex];
                          ValueList.Count := ValueList.Count + 1;
                          case ValueList.DataType of
                            rdtDouble: ValueList.
                              RealValues[Index-DeleteCount+AddCount]
                              := ValueList.
                              RealValues[Index-DeleteCount+AddCount-1];
                            rdtInteger:  ValueList.
                              IntValues[Index-DeleteCount+AddCount]
                              := ValueList.
                              IntValues[Index-DeleteCount+AddCount-1];
                            rdtBoolean:  ValueList.
                              BooleanValues[Index-DeleteCount+AddCount]
                              := ValueList.
                              BooleanValues[Index-DeleteCount+AddCount-1];
                            rdtString:  ValueList.
                              StringValues[Index-DeleteCount+AddCount]
                              := ValueList.
                              StringValues[Index-DeleteCount+AddCount-1];
                            else Assert(False);
                          end;
                        end;
                      end;
                      if (PointIndex mod 100) = 99 then
                      begin
                        frmProgress.ProgressLabelCaption :=
                          'Object ' + IntToStr(Index+1)
                          + ' out of ' + IntToStr(frmProgress.pbProgress.Max)
                          + '.  Point ' + IntToStr(PointIndex + 1) + ' out of '
                          + IntToStr(ShapeObject.FNumPoints) + '.';
                        Application.ProcessMessages;
                      end;
                    end;
                    finally
                      AScreenObject.EndUpdate;
                    end;
                  except
                    on E: EScreenObjectError do
                    begin
                      ScreenObjectList.Remove(AScreenObject);
                      AScreenObject.Free;
                      InvalidObjectNumbers.Add(Index+1);
                      if CombinedObjects then
                      begin
                        break
                      end;
                    end;
                    on E: ERbwParserError do
                    begin
                      ScreenObjectList.Remove(AScreenObject);
                      AScreenObject.Free;
                      if InvalidFormulaNumbers = nil then
                      begin
                        InvalidFormulaNumbers := TIntegerList.Create;
                      end;
                      InvalidFormulaNumbers.Add(Index+1);
                      if CombinedObjects then
                      begin
                        break
                      end;
                    end;
                  end;
                  if FDataBaseFileName <> '' then
                  begin
                    xbShapeDataBase.GotoNext;
                  end;
                  frmProgress.StepIt;
                  Application.ProcessMessages;

                end;
                if CombinedObjects then
                begin
                  Assert(ScreenObjectList.Count <= 1);
                  for ValueIndex := 0 to MultiValueList.Count - 1 do
                  begin
                    ValueList := MultiValueList[ValueIndex];
                    if ValueList <> nil then
                    begin
                      ValueList.Count := FGeometryFile.Count
                        - DeleteCount + AddCount;
                    end;
                  end;

                  frmProgress.Caption := 'Assigning Formulas';
                  frmProgress.pbProgress.Position := 0;

                  if ScreenObjectList.Count = 1 then
                  begin
                    AScreenObject := ScreenObjectList[0];

                    frmProgress.pbProgress.Max := DataSets.Count;
                    frmProgress.ProgressLabelCaption := '0 out of '
                      + IntToStr(frmProgress.pbProgress.Max) + '.';
                    frmProgress.Prefix := 'Formula ';
                    frmProgress.PopupParent := self;
                    frmProgress.Show;
                    Application.ProcessMessages;

                    for DataSetIndex := 0 to DataSets.Count - 1 do
                    begin
                      DataSet := DataSets[DataSetIndex];
                      if DataSet = nil then
                      begin
                        frmProgress.pbProgress.Max :=
                          frmProgress.pbProgress.Max -1;
                      end;
                    end;

                    for DataSetIndex := 0 to DataSets.Count - 1 do
                    begin
                      DataSet := DataSets[DataSetIndex];
                      if DataSet = nil then
                      begin
                        Continue;
                      end;
                      Position := AScreenObject.IndexOfDataSet(DataSet);
                      Assert(Position >= 0);
                      case DataSet.DataType of
                        rdtDouble: Formula := rsObjectImportedValuesR + '("' + DataSet.Name + '")';
                        rdtInteger: Formula := rsObjectImportedValuesI + '("' + DataSet.Name + '")';
                        rdtBoolean: Formula := rsObjectImportedValuesB + '("' + DataSet.Name + '")';
                        rdtString: Formula := rsObjectImportedValuesT + '("' + DataSet.Name + '")';
                        else Assert(False);
                      end;
                      AScreenObject.DataSetFormulas[Position] := Formula;
                      frmProgress.StepIt;
                      Application.ProcessMessages;
                    end;
                  end;
                end;
                if ScreenObjectList.Count > 0 then
                begin
                  Undo.StoreNewScreenObjects(ScreenObjectList);
                  Undo.StoreNewDataSets(NewDataSets);
                  Undo.StoreChangedDataSetProperties(OldProperties,NewProperties);
                  frmGoPhast.UndoStack.Submit(Undo);
                  frmGoPhast.PhastModel.AddFileToArchive(FGeometryFileName);
                  frmGoPhast.PhastModel.AddFileToArchive(FIndexFileName);
                  frmGoPhast.PhastModel.AddFileToArchive(FDataBaseFileName);
                  OptionalExtensions := TStringList.Create;
                  try
                    OptionalExtensions.Add('.apr');
                    OptionalExtensions.Add('.sbn');
                    OptionalExtensions.Add('.sbx');
                    OptionalExtensions.Add('.fbn');
                    OptionalExtensions.Add('.fbx');
                    OptionalExtensions.Add('.ain');
                    OptionalExtensions.Add('.aih');
                    OptionalExtensions.Add('.ixs');
                    OptionalExtensions.Add('.mxs');
                    OptionalExtensions.Add('.prj');
                    OptionalExtensions.Add('.atx');
                    OptionalExtensions.Add('.xml');
                    for Index := 0 to OptionalExtensions.Count - 1 do
                    begin
                      OptionalFileName := ChangeFileExt(FGeometryFileName,
                        OptionalExtensions[Index]);
                      if FileExists(OptionalFileName) then
                      begin
                        frmGoPhast.PhastModel.AddFileToArchive(OptionalFileName);
                      end;
                    end;
                  finally
                    OptionalExtensions.Free;
                  end;
                end
                else
                begin
                  Undo.Free;
                end;
              except on E: Exception do
                begin
                  MessageDlg(E.Message, mtError, [mbOK], 0);
                  Undo.Free;
                end;
              end;
            finally
              ScreenObjectList.Free;
              frmGoPhast.PhastModel.EndScreenObjectUpdate;
            end;
          end;
        finally
          Variables.Free;
          DataSets.Free;
          FieldNames.Free;
          RealFieldNames.Free;
          frmProgress.Hide;
        end;
      finally
        frmGoPhast.CanDraw := True;
        if InvalidObjectNumbers.Count > 0 then
        begin
          Beep;
          if MessageDlg(IntToStr(InvalidObjectNumbers.Count) +
            ' objects were invalid because they cross '
            + 'themselves and have been skipped.  Do you want to see the numbers '
            + 'of the ones that have been skipped?',
            mtWarning, [mbYes, mbNo], 0)= mrYes then
          begin
            ErrorString := '';
            for Index := 0 to InvalidObjectNumbers.Count -1 do
            begin
              ErrorString := ErrorString
                + IntToStr(InvalidObjectNumbers[Index]) + ', ';
            end;
            SetLength(ErrorString, Length(ErrorString) -2);

            MessageDlg(ErrorString, mtInformation, [mbOK], 0);
          end;
        end;
        if InvalidFormulaNumbers <> nil then
        begin
          Beep;
          if MessageDlg(IntToStr(InvalidFormulaNumbers.Count) +
            ' objects had attributes that could not be read into ModelMuse properly. '
            + 'They have been skipped.  Do you want to see the numbers '
            + 'of the ones that have been skipped?',
            mtWarning, [mbYes, mbNo], 0)= mrYes then
          begin
            ErrorString := '';
            for Index := 0 to InvalidFormulaNumbers.Count -1 do
            begin
              ErrorString := ErrorString
                + IntToStr(InvalidFormulaNumbers[Index]) + ', ';
            end;
            SetLength(ErrorString, Length(ErrorString) -2);

            MessageDlg(ErrorString, mtInformation, [mbOK], 0);
          end;
        end;
      end;
    finally
      InvalidObjectNumbers.Free;
      InvalidFormulaNumbers.Free;
      MultiValueList.Free;
      NewDataSets.Free;
      NewProperties.Free;
      OldProperties.Free;
      GlobalDecompileType := dcNormal;
      frmGoPhast.ChangingSelection := False;
    end;
    for InvalidParametersIndex := 0 to FInvalidParameterNames.Count - 1 do
    begin
      frmErrorsAndWarnings.AddWarning(WarningRoot,
        FInvalidParameterNames[InvalidParametersIndex]);
    end;
    if FInvalidParameterNames.Count > 0 then
    begin
      frmErrorsAndWarnings.ShowAfterDelay;
    end;
  finally
    FInvalidParameterNames.Free;
  end;
end;


procedure TfrmImportShapefile.seWellIntervalsChange(Sender: TObject);
begin
  inherited;
  dgWellElevations.RowCount := seWellIntervals.AsInteger + 1;
end;

procedure TfrmImportShapefile.btnOKClick(Sender: TObject);
begin
  inherited;
  edImportCriterionExit(nil);
  if not btnOk.Enabled then
  begin
    ModalResult := mrNone;
    Exit;
  end;
  case rgElevationCount.ItemIndex of
    0:
      begin

      end;
    1:
      begin
        edZExit(nil);
        if not btnOk.Enabled then
        begin
          ModalResult := mrNone;
          Exit;
        end;
      end;
    2:
      begin
        edHighZExit(nil);
        if not btnOk.Enabled then
        begin
          ModalResult := mrNone;
          Exit;
        end;
        edLowZExit(nil);
        if not btnOk.Enabled then
        begin
          ModalResult := mrNone;
          Exit;
        end;
      end;
    else Assert(False);
  end;

  Hide;
  SetData;
  // ModalResult can not be set before calling SetData because SetData
  // may display a dialog box.  If ModalResult is already set, the
  // application will hang.
  ModalResult := mrOK;
end;

procedure TfrmImportShapefile.GetInterpolators(const ARow: integer);
var
  List: TList;
  Index: integer;
  AType: TInterpolatorType;
  DataType: TRbwDataType;
  DataArray: TDataArray;
begin
  Assert(SizeOf(TObject) = SizeOf(TInterpolatorType));
  List := TList.Create;
  try
    AddInterpolatorsToList(List);
    dgFields.Columns[Ord(fgcInterpolator)].PickList.Clear;
    DataType := FFieldTypes[ARow - 1];
    if dgFields.Cells[Ord(fgcDataSet),ARow] <> rsNewDataSet then
    begin
      DataArray := frmGoPhast.PhastModel.GetDataSetByName(
        dgFields.Cells[Ord(fgcDataSet),ARow]);
      DataType := DataArray.DataType;
    end;
    dgFields.Columns[Ord(fgcInterpolator)].PickList.Add('None');
    for Index := 0 to List.Count - 1 do
    begin
      AType := List[Index];
      // We aren't checking the orientation here because Shapefiles
      // will always be imported to the top view.
      if DataType in AType.ValidReturnTypes then
      begin
        dgFields.Columns[Ord(fgcInterpolator)].PickList.AddObject(
          AType.InterpolatorName, TObject(AType));
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TfrmImportShapefile.GetDataSets(const ARow: integer);
var
  EvalAt: TEvaluatedAt;
  DataSet: TDataArray;
  Index: integer;
  PickList: TStrings;
begin
  EvalAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
  PickList := dgFields.Columns[Ord(fgcDataSet)].PickList;

  PickList.Clear;
  PickList.AddObject(rsNewDataSet, nil);
  for Index := 0 to frmGoPhast.PhastModel.DataSetCount - 1 do
  begin
    DataSet := frmGoPhast.PhastModel.DataSets[Index];
    if (DataSet.EvaluatedAt = EvalAt)
      and (DataSet.Orientation = dsoTop)
      and ((DataSet.DataType = FFieldTypes[ARow - 1])
      or ((DataSet.DataType = rdtDouble)
      and (FFieldTypes[ARow - 1] = rdtInteger))) then
    begin
      PickList.AddObject(DataSet.Name, DataSet);
    end;
  end;

  if PickList.IndexOf(
    dgFields.Cells[Ord(fgcDataSet), ARow]) < 0 then
  begin
    dgFields.Cells[Ord(fgcDataSet), ARow] := rsNewDataSet;
  end;
end;

procedure TfrmImportShapefile.SetCheckBoxCaptions;
var
  NodeElemString: string;
begin
  NodeElemString := EvalAtToString(TEvaluatedAt(rgEvaluatedAt.ItemIndex),
       frmGoPhast.ModelSelection, True, False);
  cbEnclosedCells.Caption := rsSetValueOfEnclosed + NodeElemString;
  cbIntersectedCells.Caption := rsSetValueOfIntersected + NodeElemString;
  cbInterpolation.Caption := rsSetValueOf + NodeElemString + rsByInterpolation;
//  case rgEvaluatedAt.ItemIndex of
//    0: // elements
//      begin
//        cbEnclosedCells.Caption := rsSetValueOfEnclosedElements;
//        cbIntersectedCells.Caption := rsSetValueOfIntersectedElements;
//        cbInterpolation.Caption := rsSetValueOfElementsByInterpolation;
//      end;
//    1: // cells
//      begin
//        cbEnclosedCells.Caption := rsSetValueOfEnclosedNodes;
//        cbIntersectedCells.Caption := rsSetValueOfIntersectedNodes;
//        cbInterpolation.Caption := rsSetValueOfNodesByInterpolation;
//      end;
//  else
//    Assert(False);
//  end;
end;

procedure TfrmImportShapefile.rdgBoundaryConditionsDistributeTextProgress(
  Sender: TObject; Position, Max: Integer);
begin
  inherited;
  seBoundaryTimeCount.AsInteger := rdgBoundaryConditions.RowCount -1;
end;

procedure TfrmImportShapefile.rgElevationCountClick(Sender: TObject);
begin
  inherited;
  edZ.Enabled := rgElevationCount.ItemIndex = 1;
  btnZ.Enabled := edZ.Enabled;

  edHighZ.Enabled := rgElevationCount.ItemIndex = 2;
  btnHighZ.Enabled := edHighZ.Enabled;

  edLowZ.Enabled := rgElevationCount.ItemIndex = 2;
  btnLowZ.Enabled := edLowZ.Enabled;
end;

procedure TfrmImportShapefile.rgEvaluatedAtClick(Sender: TObject);
var
  Index: integer;
begin
  inherited;
  for Index := 1 to dgFields.RowCount - 1 do
  begin
    GetDataSets(Index);
  end;
  SetCheckBoxCaptions;
  EnableFeatureImport;
end;

procedure TfrmImportShapefile.seBoundaryTimeCountChange(Sender: TObject);
begin
  inherited;
  rdgBoundaryConditions.RowCount := seBoundaryTimeCount.AsInteger + 1;
end;

procedure TfrmImportShapefile.dgFieldsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  DrawCell: boolean;
begin
  inherited;
  if (ARow >= dgFields.FixedRows)
    and (TFieldGridColumns(ACol) in [fgcDataSet, fgcInterpolator]) then
  begin
    DrawCell := false;
    if not dgFields.Checked[Ord(fgcImport), ARow] then
    begin
      DrawCell := True;
      dgFields.Canvas.Brush.Color := Color;
    end;
//    if ACol = Ord(fgcInterpolator) then
//    begin
//      if dgFields.Cells[Ord(fgcDataSet), ARow] <> rsNewDataSet then
//      begin
//        DrawCell := True;
//        dgFields.Canvas.Brush.Color := Color;
//      end;
//    end;
    if DrawCell then
    begin
      dgFields.Canvas.FillRect(Rect);
      dgFields.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 2,
        dgFields.Cells[ACol, ARow]);
    end;
  end;
end;

procedure TfrmImportShapefile.dgFieldsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if (ARow >= dgFields.FixedRows)
    and (TFieldGridColumns(ACol) in [fgcDataSet, fgcInterpolator]) then
  begin
    CanSelect := dgFields.Checked[Ord(fgcImport), ARow];
    if ACol = Ord(fgcInterpolator) then
    begin
      if CanSelect and not dgFields.Drawing then
      begin
        GetInterpolators(ARow);
      end;
    end;
    if (ACol = Ord(fgcDataSet)) and CanSelect and not dgFields.Drawing then
    begin
      GetDataSets(ARow);
    end;
  end;
end;

{ TUndoImportShapefile }
procedure TUndoImportShapefile.StoreChangedDataSetProperties(var OldProperties,
  NewProperties: TList);
begin
  FOldProperties.Free;
  FOldProperties := OldProperties;
  OldProperties := nil;
  FNewProperties.Free;
  FNewProperties := NewProperties;
  NewProperties := nil;
end;

procedure TUndoImportShapefile.StoreNewDataSets(var NewDataSets: TList);
begin
  FNewDataSets.Free;
  FNewDataSets := NewDataSets;
  NewDataSets := nil;
end;

constructor TUndoImportShapefile.Create;
begin
  inherited;
  FNewDataSets := TList.Create;
  FOldProperties := TList.Create;
  FNewProperties := TList.Create;
end;

function TUndoImportShapefile.Description: string;
begin
  result := 'import shape file';
end;

destructor TUndoImportShapefile.Destroy;
begin
  inherited;
  FNewDataSets.Free;
  FOldProperties.Free;
  FNewProperties.Free;
end;

procedure TUndoImportShapefile.DoCommand;
var
  Index: integer;
  DataSet: TDataArray;
  Prop: TPhastDataSetStorage;
begin
  frmGoPhast.CanDraw := False;
  try
    UnDeleteNewScreenObjects;
    for Index := 0 to FNewDataSets.Count -1 do
    begin
      DataSet := FNewDataSets[Index];
      if frmGoPhast.PhastModel.IndexOfDataSet(DataSet.Name) < 0 then
      begin
        frmGoPhast.PhastModel.AddDataSet(DataSet);
      end;
      frmGoPhast.DeletedDataSets.Extract(DataSet);
    end;
    for Index := 0 to FNewProperties.Count - 1 do
    begin
      Prop := FNewProperties[Index];
      Prop.AssignToDataSet;
    end;
  finally
    frmGoPhast.CanDraw := True;
  end;
  FShouldUpdateShowHideObjects := True;
  inherited;
  WarnSfrLengthProblems(FNewScreenObjects);
  frmGoPhast.PhastModel.FormulaManager.Pack;
end;

procedure TUndoImportShapefile.Redo;
begin
  DoCommand;
  FShouldUpdateShowHideObjects := True;
  inherited;
  WarnSfrLengthProblems(FNewScreenObjects);
end;

procedure TUndoImportShapefile.Undo;
var
  Index: integer;
  DataSet: TDataArray;
  Prop: TPhastDataSetStorage;
begin
  inherited;
  frmGoPhast.CanDraw := False;
  try
    DeleteNewScreenObjects;
    for Index := 0 to FNewDataSets.Count - 1 do
    begin
      DataSet := FNewDataSets[Index];
      frmGoPhast.PhastModel.ExtractDataSet(DataSet);
    end;
    for Index := 0 to FOldProperties.Count - 1 do
    begin
      Prop := FOldProperties[Index];
      Prop.AssignToDataSet;
    end;
    frmGoPhast.DeletedDataSets.Assign(FNewDataSets, laOr);

  finally
    frmGoPhast.CanDraw := True;
  end;
  FShouldUpdateShowHideObjects := True;
  inherited;
//  UpdateDisplay;
  WarnSfrLengthProblems(FNewScreenObjects);
  frmGoPhast.PhastModel.FormulaManager.Pack;
end;

procedure TfrmImportShapefile.dgFieldsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
var
  DataSet: TDataArray;
  Index: integer;
begin
  inherited;
  if (ARow >= 1) and (ACol = Ord(fgcDataSet)) then
  begin
    if Value <> rsNewDataSet then
    begin
      Index := dgFields.Columns[Ord(fgcDataSet)].PickList.IndexOf(Value);
      Assert(Index > 0);
      DataSet := dgFields.Columns[Ord(fgcDataSet)].
        PickList.Objects[Index] as TDataArray;
      if DataSet.TwoDInterpolator = nil then
      begin
        GetInterpolators(ARow);
        dgFields.Cells[Ord(fgcInterpolator), ARow] :=
          dgFields.Columns[Ord(fgcInterpolator)].PickList[0];
      end
      else
      begin
        dgFields.Cells[Ord(fgcInterpolator), ARow] :=
          DataSet.TwoDInterpolator.InterpolatorName;
      end;
    end;
    dgFields.Invalidate;
  end;
end;

procedure TfrmImportShapefile.btnSelectClick(Sender: TObject);
var
  RowIndex: integer;
  SelectAll: boolean;
begin
  inherited;
  SelectAll := Sender = btnAll;
  for RowIndex := 1 to dgFields.RowCount - 1 do
  begin
    dgFields.Checked[Ord(fgcImport), RowIndex] := SelectAll;
  end;
end;

procedure TfrmImportShapefile.btnToggleClick(Sender: TObject);
var
  RowIndex: integer;
begin
  inherited;
  for RowIndex := 1 to dgFields.RowCount - 1 do
  begin
    dgFields.Checked[Ord(fgcImport), RowIndex] :=
      not dgFields.Checked[Ord(fgcImport), RowIndex];
  end;
end;

procedure TfrmImportShapefile.btnElevFormulaEdit(Sender: TObject);
var
  AFormula: string;
  Edit: TRbwEdit;
begin
  inherited;
  if Sender = btnZ then
  begin
    Edit := edZ;
  end
  else if Sender = btnHighZ then
  begin
    Edit := edHighZ;
  end
  else if Sender = btnLowZ then
  begin
    Edit := edLowZ;
  end
  else
  begin
    Edit := nil;
    Assert(false);
  end;
  rpShapeCompiler.ClearVariables;
  rpShapeCompiler.ClearExpressions;

  with TfrmFormula.Create(self) do
  begin
    try
      IncludeGIS_Functions;
      PopupParent := self;
      DataSetGroupName := StrAttributes;
      // put the formula in the TfrmFormula.
      AFormula := Edit.Text;

      // register the appropriate variables with the
      // parser.
      CreateVariables(rbFormulaParser);
      CreateDataSetVariables(rbFormulaParser);
      frmGoPhast.PhastModel.RegisterGlobalVariables(rpShapeCompiler);

      RemoveGIS_Functions;
      // show the variables and functions
      UpdateTreeList;
      Formula := AFormula;

      // The user edits the formula.
      ShowModal;
      if ResultSet then
      begin
        CheckElevationFormula(Edit, Formula);
      end;

    finally
      Free;
    end;
  end
end;

procedure TfrmImportShapefile.ShapefileProgress(Sender: TObject;
  FractionDone: double);
begin
  frmProgress.pbProgress.Position
    := Round(frmProgress.pbProgress.Max * FractionDone);
  Inc(FShapeCount);
  frmProgress.ProgressLabelCaption := 'Reading shape ' + IntToStrFormatted(FShapeCount);
  if (FShapeCount mod 100 = 0) or (FractionDone = 1) then
  begin
    Application.ProcessMessages;
  end;

end;
procedure TfrmImportShapefile.cbCoordinateConversionClick(Sender: TObject);
var
  ShapeObject: TShapeObject;
  Index: integer;
  AName: string;
  Shape: TShape;
  Component: TComponent;
  ShapePoint: TShapePoint;
  Point: TPoint;
  Zone: integer;
  Error: boolean;
begin
  inherited;
  if FShouldEnableImportGrid then
  begin
    cbImportGrid.Enabled := FShouldEnableImportGrid
      and not cbCoordinateConversion.Checked;
    if not cbImportGrid.Enabled and cbImportGrid.Checked then
    begin
      cbImportGrid.Checked := False;
      Beep;
      MessageDlg('Sorry. Importing the grid while performing '
        + 'a coordinate conversion is not allowed.', mtInformation, [mbOK], 0);
    end;
  end;
  Error := False;
  seZoneNumber.Enabled := cbCoordinateConversion.Checked;
  comboEllipsoid.Enabled := cbCoordinateConversion.Checked;
  if comboEllipsoid.Enabled then
  begin
    comboEllipsoid.Color := clWindow;
  end
  else
  begin
    comboEllipsoid.Color := clBtnFace;
  end;

  if cbCoordinateConversion.Checked and (FGeometryFile.Count > 0) then
  begin
    ShapeObject := FGeometryFile[0];
    if ShapeObject.FNumPoints > 0 then
    begin
      lblCoordinates.Caption := 'Coordinates of first point = ('
        + FloatToStr(ShapeObject.FPoints[0].X) + ', '
        + FloatToStr(ShapeObject.FPoints[0].Y) + ').';
      Zone := LatLongToUTM_Zone(ShapeObject.FPoints[0].X,
        ShapeObject.FPoints[0].Y);
      seZoneNumber.Value := Zone;
    end;

    for Index := 1 to 10 do
    begin
      AName := 'Shape' + IntToStr(Index);
      Component := self.FindComponent(AName);
      Shape := Component as TShape;
      ShapeObject := FGeometryFile[Random(FGeometryFile.Count)];
      if ShapeObject.FNumPoints > 0 then
      begin
        ShapePoint := ShapeObject.FPoints[0];
        if (ShapePoint.X < -360) or (ShapePoint.X > 360)
          or (ShapePoint.Y > 84) or (ShapePoint.Y < -80) then
        begin
          Shape.Visible := False;
          Error := True;
        end
        else
        begin
          Point := LatLongToPoint(ShapePoint.X, ShapePoint.Y);
          // X is from 29 to 708.
          // Y is from 43 to 382.
          Error := Error or
            (Point.X < 29) or (Point.X > 708) or
            (Point.Y < 43) or (Point.Y > 382);

          Shape.Left := Point.x - (Shape.Width div 2) + imageUtmZones.Left;
          Shape.Top := Point.Y - (Shape.Height div 2) + imageUtmZones.Top;
          Shape.Visible := True;
        end;
      end
      else
      begin
        Shape.Visible := False;
      end;
    end;
    if Error then
    begin
      Beep;
      MessageDlg('One or more of your data points appear to has invalid '
        + 'coordinates. Coordinate conversion can not be performed on this '
        + 'shape file.', mtWarning, [mbOK], 0);
      cbCoordinateConversion.Checked := False;
    end;

  end
  else
  begin
    for Index := 1 to 10 do
    begin
      AName := 'Shape' + IntToStr(Index);
      Component := self.FindComponent(AName);
      Shape := Component as TShape;
      Shape.Visible := False;
    end;
  end;
end;

function TfrmImportShapefile.LatLongToUTM_Zone(const LongitudeDegrees,
  LatitudeDegrees: double): integer;
begin
  if (LatitudeDegrees >= 56) and (LatitudeDegrees <= 64)
    and (LongitudeDegrees >= 0) and (LongitudeDegrees <= 12) then
  begin
    if LongitudeDegrees >= 3 then
    begin
      result := 32
    end
    else
    begin
      result := 31
    end;
  end
  else if (LatitudeDegrees >= 72) and (LatitudeDegrees <= 84)
    and (LongitudeDegrees >= 0) and (LongitudeDegrees <= 42) then
  begin
    if LongitudeDegrees <= 9 then
    begin
      result := 31;
    end
    else if LongitudeDegrees <= 21 then
    begin
      result := 33;
    end
    else if LongitudeDegrees <= 33 then
    begin
      result := 35;
    end
    else
    begin
      result := 37;
    end;
  end
  else
  begin
    result := SimpleLongToUTM_Zone(LongitudeDegrees);
  end;
end;

function TfrmImportShapefile.LatLongToPoint(Long, Lat: double): TPoint;
begin
  if Long > 180 then
  begin
    Long := Long - 360;
  end
  else if Long < -180 then
  begin
    Long := Long + 360;
  end;
  // X is from 29 to 708.
  result.x := Round((Long + 180) / 360 * (679 - 29) + 29);
  // Y is from 43 to 382
  result.y := Round(((-Lat) + 84) / 164 * (339 - 43) + 43);
end;

function TfrmImportShapefile.SimpleLongToUTM_Zone(
  const LongitudeDegrees: double): integer;
begin
  result := Floor(LongitudeDegrees / 6) + 31;
  if result > 60 then
  begin
    result := result - 60
  end;
end;

procedure TfrmImportShapefile.comboBooleanFieldChange(Sender: TObject);
var
  Combo: TComboBox;
begin
  inherited;
  Combo := Sender as TComboBox;
  if Combo.ItemIndex < 0 then
  begin
    if (UpperCase(Combo.Text) <> 'TRUE')
      and (UpperCase(Combo.Text) <> 'FALSE') then
    begin
      Combo.Color := clRed;
    end
    else
    begin
      Combo.Color := clWindow;
    end;
  end
  else
  begin
    Combo.Color := clWindow;
  end;
end;

procedure TfrmImportShapefile.comboBoundaryChoiceChange(Sender: TObject);
var
  Model: TPhastModel;
  APackage: TModflowPackageSelection;
  Packages: TModflowPackages;
begin
  inherited;
  Model := frmGoPhast.PhastModel;
  rdgBoundaryConditions.Enabled := comboBoundaryChoice.ItemIndex <> 0;
  seBoundaryTimeCount.Enabled := comboBoundaryChoice.ItemIndex <> 0;
  case Model.ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast:
      begin
        rdgBoundaryConditions.Enabled := True;
        rdgBoundaryConditions.ColCount := 3;
        AssignColFeatureProperties;
        rdgBoundaryConditions.Columns[0].ComboUsed := True;
        rdgBoundaryConditions.Columns[1].ComboUsed := True;
        rdgBoundaryConditions.Columns[2].ComboUsed := True;
        rdgBoundaryConditions.Columns[0].Format := rcf4String;
        rdgBoundaryConditions.Columns[1].Format := rcf4String;
        rdgBoundaryConditions.Columns[2].Format := rcf4String;
        rdgBoundaryConditions.Cells[0,0] := StrStartingTime;
        case comboBoundaryChoice.ItemIndex of
          0: //none
            begin
              plBoundary.ActivePage := jvspNone;
            end;
          1: //specified head
            begin
              plBoundary.ActivePage := jvspPhastSpecifiedHead;
              rdgBoundaryConditions.Cells[1,0] := 'Head';
              rdgBoundaryConditions.Cells[2,0] := 'Solution';
            end;
          2: //specified flux
            begin
              plBoundary.ActivePage := jvspNone;
              rdgBoundaryConditions.Cells[1,0] := 'Flux';
              rdgBoundaryConditions.Cells[2,0] := 'Solution';
            end;
          3: //Leaky
            begin
              plBoundary.ActivePage := jvspPhastLeaky;
              rdgBoundaryConditions.Cells[1,0] := 'Head';
              rdgBoundaryConditions.Cells[2,0] := 'Solution';
            end;
          4: //River
            begin
              plBoundary.ActivePage := jvspPhastRiver;
              rdgBoundaryConditions.Cells[1,0] := 'Head';
              rdgBoundaryConditions.Cells[2,0] := 'Solution';
            end;
          5: //Well
            begin
              plBoundary.ActivePage := jvspPhastWell;
              rdgBoundaryConditions.Cells[1,0] := 'Pumping Range;';
              rdgBoundaryConditions.Cells[2,0] := 'Solution';
            end;
          else
            begin
              Assert(False);
            end;
        end;
      end;
    msModflow:
      begin
        Packages := Model.ModflowPackages;
        APackage := comboBoundaryChoice.Items.Objects[
          comboBoundaryChoice.ItemIndex] as TModflowPackageSelection;
        if APackage = Packages.ChdBoundary then
        begin
          InitializeBoundaryControlsForCHD;
        end
        else if APackage = Packages.GhbBoundary then
        begin
          InitializeBoundaryControlsForGHB;
        end
        else if APackage = Packages.WelPackage then
        begin
          InitializeBoundaryControlsForWEL;
        end
        else if APackage = Packages.RivPackage then
        begin
          InitializeBoundaryControlsForRIV;
        end
        else if APackage = Packages.DrnPackage then
        begin
          InitializeBoundaryControlsForDRN;
        end
        else if APackage = Packages.DrtPackage then
        begin
          InitializeBoundaryControlsForDRT;
        end
        else if APackage = Packages.RchPackage then
        begin
          InitializeBoundaryControlsForRCH(Packages);
        end
        else if APackage = Packages.EvtPackage then
        begin
          InitializeBoundaryControlsForEVT(Packages);
        end
        else if APackage = Packages.EtsPackage then
        begin
          InitializeBoundaryControlsForETS(Packages);
        end
        else if APackage = Packages.ResPackage then
        begin
          InitializeBoundaryControlsForRES(Packages);
        end
        else if APackage = Packages.HobPackage then
        begin
          InitializeBoundaryControlsForHOB;
        end
        else if APackage = Packages.SfrPackage then
        begin
          InitializeBoundaryControlsForSFR;
        end
        else if APackage = Packages.LakPackage then
        begin
          InitializeBoundaryControlsForLAK;
        end
        else if APackage = Packages.HfbPackage then
        begin
          InitializeBoundaryControlsForHFB
        end
        else if APackage = Packages.UzfPackage then
        begin
          InitializeBoundaryControlsForUZF
        end
        else if APackage = Packages.Mnw2Package then
        begin
          InitializeBoundaryControlsForMnw2
        end;
      end;
    else
      begin
        Assert(False);
      end;
  end;
end;

procedure TfrmImportShapefile.comboDrainReturnLocationMethodChange(
  Sender: TObject);
begin
  inherited;
  pcDrtReturnLChoice.ActivePageIndex :=
    comboDrainReturnLocationMethod.ItemIndex;
end;

procedure TfrmImportShapefile.comboEllipsoidChange(Sender: TObject);
begin
  inherited;
  case comboEllipsoid.ItemIndex of
    0:
      begin
        Ellipsoid := Airy1830;
      end;
    1:
      begin
        Ellipsoid := Bessel1841;
      end;
    2:
      begin
        Ellipsoid := Clarke1866;
      end;
    3:
      begin
        Ellipsoid := Clarke1880;
      end;
    4:
      begin
        Ellipsoid := Everest1830;
      end;
    5:
      begin
        Ellipsoid := Fischer1960;
      end;
    6:
      begin
        Ellipsoid := Fischer1968;
      end;
    7:
      begin
        Ellipsoid := GRS67_1967;
      end;
    8:
      begin
        Ellipsoid := GRS75_1975;
      end;
    9:
      begin
        Ellipsoid := GRS80_1980;
      end;
    10:
      begin
        Ellipsoid := Hough1956;
      end;
    11:
      begin
        Ellipsoid := International1924;
      end;
    12:
      begin
        Ellipsoid := Krassowsky1940;
      end;
    13:
      begin
        Ellipsoid := SouthAmerican1969;
      end;
    14:
      begin
        Ellipsoid := WGS60_1960;
      end;
    15:
      begin
        Ellipsoid := WGS66_1966;
      end;
    16:
      begin
        Ellipsoid := WGS72_1972;
      end;
    17:
      begin
        Ellipsoid := WGS84;
      end;
  else
    Assert(False);
  end;
end;


procedure TfrmImportShapefile.comboJoinObjectsChange(Sender: TObject);
begin
  inherited;
  EnableFeatureImport;
end;

procedure TfrmImportShapefile.comboRealFieldChange(Sender: TObject);
var
  Combo: TComboBox;
  Value: Extended;
begin
  inherited;
  Combo := Sender as TComboBox;
  if Combo.ItemIndex < 0 then
  begin
    if (Combo.Text <> '') and not TryStrToFloat(Combo.Text, Value) then
    begin
      Combo.Color := clRed;
    end
    else
    begin
      Combo.Color := clWindow;
    end;
  end
  else
  begin
    Combo.Color := clWindow;
  end;
end;

procedure TfrmImportShapefile.CreateVariables(Parser: TRbwParser);
var
  RowIndex: integer;
  VariableName: string;
begin
  for RowIndex := 1 to dgFields.RowCount - 1 do
  begin
    VariableName := FieldToVarName(dgFields.Cells[Ord(fgcAttributes),RowIndex]);
    if VariableName <> '' then
    begin
      case xbShapeDataBase.GetFieldType(xbShapeDataBase.
        GetFieldNumberFromName(dgFields.Cells[Ord(fgcAttributes),RowIndex])) of
        xbfChar:
          begin
            Parser.CreateVariable(VariableName, StrAttributes, '', TValueStr);
          end;
        xbfNumber:
          begin
            if xbShapeDataBase.GetFieldDecimals(xbShapeDataBase.
              GetFieldNumberFromName(dgFields.
              Cells[Ord(fgcAttributes),RowIndex])) = 0 then
            begin
              Parser.CreateVariable(VariableName, StrAttributes, 0, TValueInt);
            end
            else
            begin
              Parser.CreateVariable(VariableName, StrAttributes, 0.0, TValueReal);
            end;
          end;
        xbfLogic:
          begin
            Parser.CreateVariable(VariableName, StrAttributes, False, TValueBool);
          end;
      else
        Assert(False);
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.CheckImportCriterionFormula(AFormula: string);
var
  Expression: TExpression;
begin
  EnableOK;
  if cbImportObjects.Checked then
  begin
    rpShapeCompiler.ClearExpressions;
    rpShapeCompiler.ClearVariables;
    CreateVariables(rpShapeCompiler);
    try
      rpShapeCompiler.Compile(AFormula);
    Except
      on ERbwParserError do
      begin
        btnOK.Enabled := False;
        Exit;
      end;
    end;
    Expression := rpShapeCompiler.CurrentExpression;
    case Expression.ResultType of
      rdtDouble, rdtInteger:
        begin
          AFormula := '' + AFormula + '<>0';
        end;
      rdtBoolean:
        begin
          // do nothing.
        end;
      rdtString:
        begin
          AFormula := 'UpperCase(' + AFormula + ')="TRUE"';
        end;
    else
      Assert(False);
    end;

    try
      rpShapeCompiler.Compile(AFormula);
      Expression := rpShapeCompiler.CurrentExpression;

      edImportCriterion.Text := Expression.Decompile;
    Except
      on ERbwParserError do
      begin
        btnOK.Enabled := False;
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.CheckElevationFormula(Edit: TRbwEdit;
  AFormula: string);
var
  Expression: TExpression;
begin
  EnableOK;
  if cbImportObjects.Checked then
  begin
    rpShapeCompiler.ClearExpressions;
    rpShapeCompiler.ClearVariables;
    CreateVariables(rpShapeCompiler);
    CreateDataSetVariables(rpShapeCompiler);
    frmGoPhast.PhastModel.RegisterGlobalVariables(rpShapeCompiler);
    try
      rpShapeCompiler.Compile(AFormula);
    Except
      on ERbwParserError do
      begin
        btnOK.Enabled := False;
        Exit;
      end;
    end;
    Expression := rpShapeCompiler.CurrentExpression;
    case Expression.ResultType of
      rdtDouble, rdtInteger:
        begin
          // do nothing.
        end;
      rdtBoolean:
        begin
          AFormula := 'If(' + AFormula + ', 1, 0)';
        end;
      rdtString:
        begin
          AFormula := 'TextToFloatDef(' +  AFormula + ', 0)';
        end;
    else
      Assert(False);
    end;

    try
      rpShapeCompiler.Compile(AFormula);
      Expression := rpShapeCompiler.CurrentExpression;

      Edit.Text := Expression.Decompile;
    Except
      on ERbwParserError do
      begin
        btnOK.Enabled := False;
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.AssignBoundary(AScreenObject: TScreenObject);
var
  Model: TPhastModel;
  Packages: TModflowPackages;
  Package: TObject;
begin
  if tabFeatures.TabVisible and (comboBoundaryChoice.ItemIndex > 0) then
  begin
    case frmGoPhast.ModelSelection of
      msUndefined:
        begin
          Assert(False);
        end;
      msPhast:
        begin
          case comboBoundaryChoice.ItemIndex of
            0:
              begin
                Assert(False);
              end;
          1: //specified head
            begin
              AssignAPhastSpecifiedHeadBoundary(AScreenObject);
            end;
          2: //specified flux
            begin
              AssignAPhastBoundary(AScreenObject.FluxBoundary);
            end;
          3: //Leaky
            begin
              AssignAPhastLeakyBoundary(AScreenObject);
            end;
          4: //River
            begin
              AssignAPhastRiverBoundary(AScreenObject);
            end;
          5: //Well
            begin
              AssignAPhastWellBoundary(AScreenObject);
            end;
          else
            begin
              Assert(False);
            end;
          end;
        end;
      msModflow:
        begin
          Model := frmGoPhast.PhastModel;
          Packages := Model.ModflowPackages;
          Package := comboBoundaryChoice.Items.Objects[
            comboBoundaryChoice.ItemIndex];
          if Packages.ChdBoundary = Package then
          begin
            ImportModflowChdBoundary(AScreenObject);
          end
          else if Package = Packages.GhbBoundary then
          begin
            ImportModflowGhbBoundary(AScreenObject);
          end
          else if Package = Packages.WelPackage then
          begin
            ImportModflowWelBoundary(AScreenObject);
          end
          else if Package = Packages.RivPackage then
          begin
            ImportModflowRivPackage(AScreenObject);
          end
          else if Package = Packages.DrnPackage then
          begin
            ImportModflowDrnPackage(AScreenObject);
          end
          else if Package = Packages.DrtPackage then
          begin
            ImportModflowDrtPackage(AScreenObject);
          end
          else if Package = Packages.RchPackage then
          begin
            ImportModflowRchPackage(AScreenObject);
          end
          else if Package = Packages.EvtPackage then
          begin
            ImportModflowEvtPackage(AScreenObject);
          end
          else if Package = Packages.EtsPackage then
          begin
            ImportModflowEtsPackage(AScreenObject, Packages);
          end
          else if Package = Packages.ResPackage then
          begin
            ImportModflowResPackage(AScreenObject);
          end
          else if Package = Packages.HobPackage then
          begin
            ImportModflowHobPackage(AScreenObject);
          end
          else if Package = Packages.SfrPackage then
          begin
            ImportModflowSfrPackage(AScreenObject);
          end
          else if Package = Packages.LakPackage then
          begin
            ImportModflowLakPackage(AScreenObject);
          end
          else if Package = Packages.HfbPackage then
          begin
            ImportModflowHfbPackage(AScreenObject);
          end
          else if Package = Packages.UzfPackage then
          begin
            ImportModflowUzfPackage(AScreenObject);
          end
          else if Package = Packages.Mnw2Package then
          begin
            ImportModflowMnw2Package(AScreenObject);
          end
        end;
      else
        begin
          Assert(False);
        end;
    end;
  end;
end;

procedure TfrmImportShapefile.BoundaryGridBeforeDrawCell(Sender: TObject; ACol,
  ARow: Integer);
var
  Grid: TRbwDataGrid4;
  Value: String;
  Dummy: Extended;
begin
  inherited;
  Grid := Sender as TRbwDataGrid4;
  if (ACol >= Grid.FixedCols) and (ARow >= Grid.FixedRows) then
  begin
    Value := Grid.Cells[ACol, ARow];
    if Value <> '' then
    begin
      if Grid.Columns[ACol].PickList.IndexOf(Value) < 0 then
      begin
        if not TryStrToFloat(Value, Dummy) then
        begin
          Grid.Canvas.Brush.Color := clRed;
        end;
      end;
    end;
  end;
end;

procedure TfrmImportShapefile.btnImportCriterionClick(Sender: TObject);
var
  AFormula: string;
begin
  inherited;
  rpShapeCompiler.ClearVariables;
  rpShapeCompiler.ClearExpressions;

  with TfrmFormula.Create(self) do
  begin
    try
      IncludeGIS_Functions;
      PopupParent := self;
      DataSetGroupName := StrAttributes;
      // put the formula in the TfrmFormula.
      AFormula := edImportCriterion.Text;

      // register the appropriate variables with the
      // parser.
      frmGoPhast.PhastModel.RegisterGlobalVariables(rpShapeCompiler);
      CreateVariables(rbFormulaParser);

      RemoveGIS_Functions;
      // show the variables and functions
      UpdateTreeList;
      Formula := AFormula;

      // The user edits the formula.
      ShowModal;
      if ResultSet then
      begin
        CheckImportCriterionFormula(Formula);
      end;

    finally
      Free;
    end;
  end
end;

procedure TfrmImportShapefile.edHighZExit(Sender: TObject);
begin
  inherited;
  CheckElevationFormula(edHighZ, edHighZ.Text);
end;

procedure TfrmImportShapefile.edImportCriterionExit(Sender: TObject);
begin
  inherited;
  CheckImportCriterionFormula(edImportCriterion.Text);
end;

procedure TfrmImportShapefile.edLowZExit(Sender: TObject);
begin
  inherited;
  CheckElevationFormula(edLowZ, edLowZ.Text);
end;

procedure TfrmImportShapefile.edZExit(Sender: TObject);
begin
  inherited;
  CheckElevationFormula(edZ, edZ.Text);
end;

procedure TfrmImportShapefile.cbEnclosedCellsClick(Sender: TObject);
begin
  inherited;
  EnableOK;
end;

procedure TfrmImportShapefile.cbImportGridClick(Sender: TObject);
begin
  inherited;
  EnableOK;
end;

procedure TfrmImportShapefile.cbImportObjectsClick(Sender: TObject);
begin
  inherited;
  cbEnclosedCells.Enabled := cbImportObjects.Checked  and
    (FGeometryFile.FileHeader.ShapeType
    in [stPolygon, stPolygonZ, stPolygonM]);
  cbIntersectedCells.Enabled := cbImportObjects.Checked;
  cbInterpolation.Enabled := cbImportObjects.Checked;
  EnableEvalAt;
  edImportCriterion.Enabled := cbImportObjects.Checked;
  btnImportCriterion.Enabled := cbImportObjects.Checked;
  comboJoinObjects.Enabled := FAllowShapesToCombine
    and cbImportObjects.Checked;
  comboVisibility.Enabled := cbImportObjects.Checked;
  EnableOK;
end;

{ TValueBool }

function TValueBool.Decompile: string;
begin
  case GlobalDecompileType of
    dcNormal: result := inherited Decompile;
    dcValue:
      begin
        if BooleanResult then
        begin
          result := 'True';
        end
        else
        begin
          result := 'False';
        end;
      end;
    else Assert(False);
  end;
end;

{ TValueInt }

function TValueInt.Decompile: string;
begin
  case GlobalDecompileType of
    dcNormal: result := inherited Decompile;
    dcValue: result := IntToStr(IntegerResult);
    else Assert(False);
  end;
end;

{ TValueReal }

function TValueReal.Decompile: string;
begin
  case GlobalDecompileType of
    dcNormal: result := inherited Decompile;
    dcValue: result := FloatToStr(DoubleResult);
    else Assert(False);
  end;

end;

{ TValueStr }

function TValueStr.Decompile: string;
begin
  case GlobalDecompileType of
    dcNormal: result := inherited Decompile;
    dcValue: result := '"' + StringResult + '"';
    else Assert(False);
  end;

end;

{ TFieldNumStorage }

constructor TFieldNumStorage.Create(XBase: TXBase);
begin
  inherited Create;
  FXBase := XBase;
end;

function TFieldNumStorage.GetIntValue: integer;
begin
  if FieldNumber = 0 then
  begin
    result := IntValue;
  end
  else
  begin
    result := FXBase.GetFieldInt(FieldNumber);
  end;
end;

function TFieldNumStorage.GetRealValue: double;
begin
  if FieldNumber = 0 then
  begin
    result := RealValue;
  end
  else
  begin
    result := FXBase.GetFieldNum(FieldNumber);
  end;
end;

function TFieldNumStorage.IntFormula: string;
begin
  if (FieldNumber = 0) or Cached then
  begin
    result := Formula;
  end
  else
  begin
    if  (FXBase.GetFieldType(FieldNumber) <> xbfNumber) then
    begin
      result := Trim(FXBase.GetFieldByNumber( FieldNumber ));
    end
    else
    begin
      Result := IntToStr(GetIntValue);
    end;
    Cached := True;
    Formula := result;
  end;
end;

function TFieldNumStorage.RealFormula: string;
begin
  if (FieldNumber = 0) or Cached  then
  begin
    result := Formula;
  end
  else
  begin
    if  (FXBase.GetFieldType(FieldNumber) <> xbfNumber) then
    begin
      result := Trim(FXBase.GetFieldByNumber( FieldNumber ))
    end
    else
    begin
      Result := FloatToStr(GetRealValue);
    end;
    Cached := True;
    Formula := result;
  end;
end;

end.
