{@abstract(
The main purpose of @name is to provide classes used in saving a
PHAST model to a stream and reading a PHAST model from a stream.)
@Link(TPhastModel) does this by making the significant features of a model
published properties.  Because @Link(TPhastModel)
is derived from TPersistent, this
automatically provides it with methods to read and write its published
properties.  Most of the published properties of @Link(TPhastModel) correspond
directly to settings in PHAST.  However @Link(TPhastModel.DataSetList) and
@Link(TPhastModel.ObjectList) are are descendants of TCollection.  They store
instances of @Link(TDataArray) and @Link(TScreenObject) respectively.

@author(Richard B. Winston <rbwinst@usgs.gov>)}
unit PhastModelUnit;

interface

uses Windows, Types, GuiSettingsUnit, SysUtils, Classes, Contnrs, Controls,
  Forms, ZoomBox2, ScreenObjectUnit, DataSetUnit, FastGEO, IniFiles,
  ModflowPackageSelectionUnit, PhastGridUnit, RbwParser, GoPhastTypes,
  PrintFrequency, TimeUnit, PhastDataSets, RealListUnit, CompressedImageUnit,
  LayerStructureUnit, AbstractGridUnit, ModflowGridUnit, SubscriptionUnit,
  ModflowPackagesUnit, ModflowOptionsUnit, ModflowTimeUnit, FluxObservationUnit,
  ModflowOutputControlUnit, ModflowParameterUnit, Graphics, ColorSchemes,
  ModflowTransientListParameterUnit, GlobalVariablesUnit, OrderedCollectionUnit,
  ModflowBoundaryDisplayUnit, ModflowBoundaryUnit, ClassificationUnit,
  ModflowHfbDisplayUnit, EdgeDisplayUnit, ModflowUnitNumbers, HufDefinition,
  ModelMateClassesUnit, ModflowHobUnit, EZDSLHsh, FormulaManagerUnit,
  PathlineReader, LegendUnit, DisplaySettingsUnit;

const
  // @name is the name of the @link(TDataArray) that specifies whether an
  // element in PHAST is active or not.
  rsActive = 'Active';
  // @name is the name of the @link(TDataArray) that specifies
  // the hydraulic conductivity in the X direction.
  rsKx = 'Kx';
  // @name is the name of the @link(TDataArray) that specifies
  // the hydraulic conductivity in the Y direction.
  rsKy = 'Ky';
  // @name is the name of the @link(TDataArray) that specifies
  // the hydraulic conductivity in the Z direction.
  rsKz = 'Kz';
  // @name is the name of the @link(TDataArray) that specifies
  // the porosity.
  rsPorosity = 'Porosity';
  // @name is the name of the @link(TDataArray) that specifies
  // the specific storage.
  rsSpecific_Storage = 'Specific_Storage';
  // @name is the name of the @link(TDataArray) that specifies
  // the longitudinal dispersivity.
  rsLong_Dispersivity = 'Longitudinal_Dispersivity';
  // @name is the name of the @link(TDataArray) that specifies
  // the horizontal transverse dispersivity.
  rsHorizontal_Transv_Dispersivity = 'Horizontal_Transverse_Dispersivity';
  // @name is the name of the @link(TDataArray) that specifies
  // the vertical transverse dispersivity.
  rsVertical_Transv_Dispersivity = 'Vertical_Transverse_Dispersivity';
  // @name is the name of the @link(TDataArray) that specifies
  // the initial head.
  rsInitial_Head = 'Initial_Head';
  // @name is the name of the @link(TDataArray) that specifies
  // the initial water table.
  rsInitial_Water_Table = 'Initial_Water_Table';
  // @name is the name of the @link(TDataArray) that specifies
  // the initial solution.
  rsChemistry_Initial_Solution = 'Chemistry_Initial_Solution';
  // @name is the name of the @link(TDataArray) that specifies
  // the initial equilibrium phases.
  rsChemistry_Initial_Equilibrium_Phases =
    'Chemistry_Initial_Equilibrium_Phases';
  // @name is the name of the @link(TDataArray) that specifies
  // the initial surface properties.
  rsChemistry_Initial_Surface = 'Chemistry_Initial_Surface';
  // @name is the name of the @link(TDataArray) that specifies
  // the initial exchange properties.
  rsChemistry_Initial_Exchange = 'Chemistry_Initial_Exchange';
  // @name is the name of the @link(TDataArray) that specifies
  // the initial gas phase properties.
  rsChemistry_Initial_Gas_Phase = 'Chemistry_Initial_Gas_Phase';
  // @name is the name of the @link(TDataArray) that specifies
  // the initial solid-solution properties.
  rsChemistry_Initial_Solid_Solutions = 'Chemistry_Initial_Solid_Solutions';
  // @name is the name of the @link(TDataArray) that specifies
  // the initial kinetic properties.
  rsChemistry_Initial_Kinetics = 'Chemistry_Initial_Kinetics';
  // @name is the name of the @link(TDataArray) that specifies
  // the "Print Chemistry" distribution.
  rsPrint_Chemistry = 'Print_Chemistry';
  // @name is the name of the @link(TDataArray) that specifies
  // the "Print XYZ Chemistry" distribution.
  rsPrint_XYZ_Chemistry = 'Print_XYZ_Chemistry';

  // @name is the name of the @link(TDataArray) that specifies
  // the hydraulic conductivity for the leaky boundary condition on the top
  // view of the model.
  rsTopLeakyHydraulicConductivity = 'Top_Leaky_Hydraulic_Conductivity';
  // @name is the name of the @link(TDataArray) that specifies
  // the thickness associated with the leaky boundary condition on the top
  // view of the model.
  rsTopLeakyThickness = 'Top_Leaky_Thickness';
  // @name is the name of the @link(TDataArray) that specifies
  // the hydraulic conductivity for the leaky boundary condition on the front
  // view of the model.
  rsFrontLeakyHydraulicConductivity = 'Front_Leaky_Hydraulic_Conductivity';
  // @name is the name of the @link(TDataArray) that specifies
  // the thickness associated with the leaky boundary condition on the front
  // view of the model.
  rsFrontLeakyThickness = 'Front_Leaky_Thickness';
  // @name is the name of the @link(TDataArray) that specifies
  // the hydraulic conductivity for the leaky boundary condition on the side
  // view of the model.
  rsSideLeakyHydraulicConductivity = 'Side_Leaky_Hydraulic_Conductivity';
  // @name is the name of the @link(TDataArray) that specifies
  // the thickness associated with the leaky boundary condition on the side
  // view of the model.
  rsSideLeakyThickness = 'Side_Leaky_Thickness';
  // @name is the name of the @link(TDataArray) that specifies
  // the hydraulic conductivity for the river boundary condition.
  rsRiverHydraulicConductivity = 'River_Hydraulic_Conductivity';
  // @name is the name of the @link(TDataArray) that specifies
  // the width for the river boundary condition.
  rsRiverWidth = 'River_Width';
  // @name is the name of the @link(TDataArray) that specifies
  // the depth for the river boundary condition.
  rsRiverDepth = 'River_Depth';
  // @name is the name of the @link(TDataArray) that specifies
  // the bed thickness for the river boundary condition.
  rsRiverBedThickness = 'River_Bed_Thickness';
  // @name is the name of the @link(TDataArray) that specifies
  // what type of solution is occurs with a specified head boundary.
  // (specified solution or associated solution).
  rsSolutionType = 'Specified_Head_Solution_Type';


  StrSpecifiedHead = 'Specified_Head';
  StrSpecifiedHeadSolution = 'Specified_Head_Solution';
  StrTopFluxBoundaryFlux = 'Top_Flux_Boundary_Flux';
  StrFrontFluxBoundaryFlux = 'Front_Flux_Boundary_Flux';
  StrSideFluxBoundaryFlux = 'Side_Flux_Boundary_Flux';
  StrTopFluxBoundaryAssocSoln = 'Top_Flux_Boundary_Associated_Solution';
  StrFrontFluxBoundaryAssocSoln = 'Front_Flux_Boundary_Associated_Solution';
  StrSideFluxBoundaryAssocSoln = 'Side_Flux_Boundary_Associated_Solution';
  StrTopLeakyBoundaryHead = 'Top_Leaky_Boundary_Head';
  StrTopLeakyBoundaryAssocSoln = 'Top_Leaky_Boundary_Associated_Solution';
  StrFrontLeakyBoundaryHead = 'Front_Leaky_Boundary_Head';
  StrFrontLeakyBoundaryAssocSoln = 'Front_Leaky_Boundary_Associated_Solution';
  StrSideLeakyBoundaryHead = 'Side_Leaky_Boundary_Head';
  StrSideLeakyBoundaryAssocSoln = 'Side_Leaky_Boundary_Associated_Solution';
  StrRiverHead = 'River_Head';
  StrRiverAssocSoln = 'River_Associated_Solution';
  StrWellInjectionRate = 'Well_Injection_Rate';
  StrWellSolution = 'Well_Solution';


  rsModflow_Initial_Head = 'Modflow_Initial_Head';
  rsModflow_CBKz = 'Confining_Bed_Kz';
  rsSpecificYield = 'Specific_Yield';
  rsWetDryThreshold = 'Wet_Dry_Threshold';
  rsWetDryFlag = 'Wet_Dry_Flag';
  rsWetDry = 'WetDry';
  rsHorizontalAnisotropy = 'Horizontal_Anisotropy';
  rsVerticalAnisotropy = 'Vertical_Anisotropy';
  rsModflowSpecifiedHead = 'Modflow_Specified_Head';

  rsResLayer = 'Reservoir_Layer';
  rsResBottom = 'Reservoir_Elevation';
  rsResKv = 'Reservoir_Hydraulic_Conductivity';
  rsResBedThickness = 'Reservoir_Bed_Thickness';
  rsResClassificaton = 'Reservoir';

  rsLakeID = 'Lake_ID';
  rsLakeLeakance = 'Lakebed_Leakance';
  rsLakeClassificaton = 'Lake';

  // names of @link(TModflowBoundaryDisplayTimeList)s

  //
  StrMODFLOWWellPumping = 'WEL Pumping Rate';
  StrMODFLOWGhbConductance = 'GHB Conductance';
  StrMODFLOWGhbHead = 'GHB Boundary Head';
  StrMODFLOWDrainConductance = 'DRN Conductance';
  StrMODFLOWDrainElevation = 'DRN Elevation';
  StrMODFLOWDrainReturnConductance = 'DRT Return Conductance';
  StrMODFLOWDrainReturnElevation = 'DRT Return Elevation';
  StrMODFLOWDrainReturnFraction = 'DRT Return Fraction';
  StrMODFLOWRiverConductance = 'RIV Conductance';
  StrMODFLOWRiverStage = 'RIV Stage';
  StrMODFLOWRiverBottom = 'RIV Bottom';
  StrMODFLOWCHDStartingHead = 'CHD Starting Head';
  StrMODFLOWCHDEndingHead = 'CHD Ending Head';
  StrMODFLOWEtsRateFraction = 'ETS Evapotranspiration Rate Fraction';
  StrMODFLOWEtsDepthFraction = 'ETS Evapotranspiration Depth Fraction';
  StrMODFLOWEtsRate = 'ETS Evapotranspiration Rate';
  StrMODFLOWEtsDepth = 'ETS Evapotranspiration Depth';
  StrMODFLOWEtsSurface = 'ETS Evapotranspiration Surface';
  StrMODFLOWEtsLayer = 'ETS Evapotranspiration Layer';

  StrMODFLOWEvtRate = 'EVT Evapotranspiration Rate';
  StrMODFLOWEvtDepth = 'EVT Evapotranspiration Depth';
  StrMODFLOWEvtSurface = 'EVT Evapotranspiration Surface';
  StrMODFLOWEvtLayer = 'EVT Evapotranspiration Layer';
  StrMODFLOWRchRate = 'RCH Rate';
  StrMODFLOWRchLayer = 'RCH Layer';

  StrModflowSfrSegment = 'SFR Segment';
  StrModflowSfrReach = 'SFR Reach';
  StrModflowSfrIcalc = 'SFR ICALC';
  StrModflowSfrReachLength = 'SFR Reach Length';
  StrModflowSfrStreamTop = 'SFR Streambed Top';
  StrModflowSfrStreamSlope = 'SFR Stream Slope';
  StrModflowSfrStreamThickness = 'SFR Streambed Thickness';
  StrModflowSfrStreamK = 'SFR Streambed Kv';
  StrModflowSfrSatWatCont = 'SFR Saturated Volumetric Water Content';
  StrModflowSfrInitWatCont = 'SFR Initial Volumentric Water Content';
  StrModflowSfrBrooksCorey = 'SFR Brooks Corey Exponent';
  StrModflowSfrVertK = 'SFR Max Unsaturated Kz';
  StrModflowSfrDownstreamSegment = 'SFR Outflow Segment';
  StrModflowSfrDiversionSegment = 'SFR Diversion Segment';
  StrModflowSfrIprior = 'SFR Diversion Priority';
  StrModflowSfrFlow = 'SFR Flow';
  StrModflowSfrRunoff = 'SFR Runoff';
  StrModflowSfrPrecipitation = 'SFR Precipitation';
  StrModflowSfrEvapotranspiration = 'SFR Evapotranspiration';
  StrModflowSfrChannelRoughness = 'SFR Channel Roughness';
  StrModflowSfrBankRoughness = 'SFR Bank Roughness';
  StrModflowSfrDepthCoefficient = 'SFR Depth Coefficient';
  StrModflowSfrDepthExponent = 'SFR Depth Exponent';
  StrModflowSfrWidthCoefficient = 'SFR Width Coefficient';
  StrModflowSfrWidthExponent = 'SFR Width Exponent';
  StrModflowSfrUpstreamHydraulicConductivity = 'SFR Upstream Hydraulic Conductivity';
  StrModflowSfrDownstreamHydraulicConductivity = 'SFR Downstream Hydraulic Conductivity';
  StrModflowSfrUpstreamWidth = 'SFR Upstream Width';
  StrModflowSfrDownstreamWidth = 'SFR Downstream Width';
  StrModflowSfrUpstreamThickness = 'SFR Upstream Thickness';
  StrModflowSfrDownstreamThickness = 'SFR Downstream Thickness';
  StrModflowSfrUpstreamElevation = 'SFR Upstream Elevation';
  StrModflowSfrDownstreamElevation = 'SFR Downstream Elevation';
  StrModflowSfrUpstreamDepth = 'SFR Upstream Depth';
  StrModflowSfrDownstreamDepth = 'SFR Downstream Depth';
  StrModflowSfrUpstreamSaturatedWaterContent =
    'SFR Upstream Saturated Water Content';
  StrModflowSfrDownstreamSaturatedWaterContent =
    'SFR Downstream Saturated Water Content';
  StrModflowSfrUpstreamInitialUnsaturatedWaterContent =
    'SFR Upstream Initial Unsaturated Water Content';
  StrModflowSfrDownstreamInitialUnsaturatedWaterContent =
    'SFR Downstream Initial Unsaturated Water Content';
  StrModflowSfrUpstreamBrooksCoreyExponent =
    'SFR Upstream Brooks Corey Exponent';
  StrModflowSfrDownstreamBrooksCoreyExponent =
    'SFR Downstream Brooks Corey Exponent';
  StrModflowSfrUpstreamMaxUnsaturatedKz = 'SFR Upstream Max Unsaturated Kz';
  StrModflowSfrDownstreamMaxUnsaturatedKz = 'SFR Downstream Max Unsaturated Kz';
  StrUzfInfiltrationRate = 'UZF Infiltration Rate';
  StrUzfExtinctionDepth = 'UZF Extinction Depth';
  StrUzfWaterContent = 'UZF Water Content';
  StrUzfEtDemand = 'UZF ET Demand';
  StrMODFLOWHeadObservations = 'Head Observations';
  StrWellRadius = 'Well Radius';
  StrSkinRadius = 'Skin Radius';
  StrSkinK = 'Skin K';
  StrB = 'B';
  StrC = 'C';
  StrP = 'P';
  StrCellToWellConductance = 'Cell to Well Conductance';
  StrPartialPenetration = 'Partial Penetration Fraction';

  StrUzfLandSurface = 'Land_Surface';
  StrUzfLayer = 'UZF_Layer';
  StrUzfDischargeRouting = 'Discharge_Routing';
  StrUzfVerticalK = 'Maximum_Unsaturated_Vertical_K';
  strUzfClassification = 'UZF';
  StrUzfBrooksCoreyEpsilon = 'Brooks_Corey_Epsilon';
  StrUzfSaturatedWaterContent = 'Saturated_Water_Content';
  StrUzfInitialUnsaturatedWaterContent = 'Initial_Unsaturated_Water_Content';
  StrUzfGage_1_and_2 = 'UZF_Gage_1_and_2';
  StrUzfGage3 = 'UZF_Gage3';
  StrHydrology = 'Hydrology';
  StrChemistry = 'Chemistry';
  StrOutput = 'Output';

  StrModpathZone = 'Modpath_Zone';
  StrHufReferenceSurface = 'HUF_Reference_Surface';

const
  WetError = 'The wetting option is active but '
    + 'no layers of the proper type have been specified.';
  WettableLayers = [1,3];
  
type
  // @name represents how PHAST results are printed - XY orientation or
  // XZ orientation.
  TpgPrintOrientation = (pgXY, pgXZ);

  TModelMateOperation = (mmoImport, mmoExport);

  TGetZoomBoxEvent = procedure (Sender: TObject; VD: TViewDirection;
    var ZoomBox: TQrbwZoomBox2) of object;
  TGetCurrentScreenObjectEvent = procedure (Sender: TObject; VD: TViewDirection;
    var ScreenObject: TScreenObject) of object;
  TConvertPointEvent = procedure (Sender: TObject; VD: TViewDirection;
    const RealPoint: TPoint2D; var ScreenCoordinate: TPoint) of object;
  TCheckScreenObjectEvent = procedure(Sender: TObject;
    ScreenObject: TScreenObject; var IsACurrentScreenObject: boolean) of object;

  TDataSetCreationData = record
    DataSetType: TDataArrayType;
    Orientation: TDataSetOrientation;
    DataType: TRbwDataType;
    Name: string;
    Formula: string;
    Classification: String;
    DataSetNeeded: TObjectUsedEvent;
    DataSetShouldBeCreated: TObjectUsedEvent;
    Lock: TDataLock;
    CheckMax: boolean;
    CheckMin: boolean;
    Max: double;
    Min: double;
    EvaluatedAt: TEvaluatedAt;
    AssociatedDataSets: string;
  end;

  TDataSetClassification = class(TClassificationObject)
  private
    FDataArray: TDataArray;
  public
    function ClassificationName: string; Override;
    function FullClassification: string; Override;
    Constructor Create(ADataArray: TDataArray);
    property DataArray: TDataArray read FDataArray;
  end;

  TProgramLocations = class(TPersistent)
  private
    FTextEditorLocation: string;
    FModflowLocation: string;
    FModPathLocation: string;
    FModelMonitorLocation: string;
    FPhastLocation: string;
    FZoneBudgetLocation: string;
    FModelMateLocation: string;
    function GetTextEditorLocation: string;
    procedure SetModflowLocation(const Value: string);
    function RemoveQuotes(const Value: string): string;
    procedure SetModPathLocation(const Value: string);
    procedure SetModelMonitorLocation(const Value: string);
    procedure SetPhastLocation(const Value: string);
    procedure SetZoneBudgetLocation(const Value: string);
    procedure SetModelMateLocation(const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
    procedure WriteToIniFile(IniFile: TMemInifile);
    procedure ReadFromIniFile(IniFile: TMemInifile);
  published
    property ModflowLocation: string read FModflowLocation
      write SetModflowLocation;
    property TextEditorLocation: string read GetTextEditorLocation
      write FTextEditorLocation;
    property ModPathLocation: string read FModPathLocation
      write SetModPathLocation;
    property ModelMonitorLocation: string read FModelMonitorLocation
      write SetModelMonitorLocation;
    property PhastLocation: string read FPhastLocation write SetPhastLocation;
    property ZoneBudgetLocation: string read FZoneBudgetLocation
      write SetZoneBudgetLocation;
    property ModelMateLocation: string read FModelMateLocation
      write SetModelMateLocation;
  end;

  {
    @abstract(@name is used to save or load a @link(TDataArray) from a stream or
    save it to a stream.)  For the most part, writing to @name
    simply stores the value being written to @name in one of the
    private fields of @name whereas reading from @name
    reads data not from the private field but from the associated data set.
    The private procedure @Link(TDataSetItem.UpdateDataSet) is used to
    transfer the stored values to an actual data set.

    See @link(TDataSetCollection).
  }
  TDataSetItem = class(TCollectionItem)
  private
    // @name: @link(TDataArray);
    // @name is set to a subcomponent in @link(SetDataSetClass).  This is
    // required for the data set properties to be saved to file.
    // See @link(DataSet).
    FDataSet: TDataArray;
    // @name: string;
    // See @link(DataSetFormula).
    FDataSetFormula: string;
    // @name: string;
    // See @link(MixtureFormula).
    FMixtureFormula: string;
    FParameterFormula: string;
    // See @link(DataSetClass).
    function GetDataSetClass: string;
    // See @link(DataSetFormula).
    function GetDataSetFormula: string;
    // See @link(MixtureFormula).
    function GetMixtureFormula: string;
    // See @link(DataSetClass).
    procedure SetDataSetClass(const Value: string);
    // @name calls TDataArray.@link(TObserver.UpdateWithName).
    procedure UpdateDataSet;
    function GetParameterFormula: string;
  public
    // If Source is a @classname,
    // @name copies the published properties of Source.
    // It seems that @name is never called.
    procedure Assign(Source: TPersistent); override;
  published
    // @name is the name of the the class of @link(DataSet).
    // @name must be read before @link(DataSet) to ensure that @link(DataSet) is
    // created before being read.
    property DataSetClass: string read GetDataSetClass write SetDataSetClass;
    // @name is the formula used with @link(DataSet).
    property DataSetFormula: string read GetDataSetFormula write
      FDataSetFormula;
    // See @link(DataSetFormula)
    // @name is only for backwards compatibility.
    property DataSetFunction: string read GetDataSetFormula write
      FDataSetFormula stored False;
    // @name is the formula for mixtures used with @link(DataSet).
    property MixtureFormula: string read GetMixtureFormula write
      FMixtureFormula;
    // See @link(MixtureFormula)
    // @name is only for backwards compatibility.
    property MixtureFunction: string read GetMixtureFormula write
      FMixtureFormula stored False;
    property ParameterFormula: string read GetParameterFormula
      write FParameterFormula;
    // @name is the @link(TDataArray) whose values are being saved or read.
    // @name must be read after @link(DataSetClass) to ensure that it is
    // created before being read.
    property DataSet: TDataArray read FDataSet;
  end;

  {
    @abstract(@name is a collection of @Link(TDataSetItem)s.)
  }
  TDataSetCollection = class(TCollection)
  public
    // @name creates an instance of @classname that will store
    // @link(TDataSetItem)s in its Items property.
    constructor Create;
  end;

  {@abstract(@name stores the default units in PHAST.)}
  TUnits = class(TPersistent)
  private
    // @name: @link(TLengthUnits);
    // See @link(DefaultDispersivityUnits).
    FDefaultDispersivityUnits: TLengthUnits;
    // @name: @link(TLengthUnits);
    // See @link(DefaultFluxLengthUnits).
    FDefaultFluxLengthUnits: TLengthUnits;
    // @name: @link(TTimeUnits);
    // See @link(DefaultFluxTimeUnits).
    FDefaultFluxTimeUnits: TTimeUnits;
    // @name: @link(TLengthUnits);
    // See @link(DefaultHeadUnits).
    FDefaultHeadUnits: TLengthUnits;
    // @name: @link(TLengthUnits);
    // See @link(DefaultHorizontalGridUnits).
    FDefaultHorizontalGridUnits: TLengthUnits;
    // @name: @link(TLengthUnits);
    // See @link(DefaultHydraulicConductivityLengthUnits).
    FDefaultHydraulicConductivityLengthUnits: TLengthUnits;
    // @name: @link(TTimeUnits);
    // See @link(DefaultHydraulicConductivityTimeUnits).
    FDefaultHydraulicConductivityTimeUnits: TTimeUnits;
    // @name: @link(TLengthUnits);
    // See @link(DefaultLeakyHydraulicConductivityLengthUnits).
    FDefaultLeakyHydraulicConductivityLengthUnits: TLengthUnits;
    // @name: @link(TTimeUnits);
    // See @link(DefaultLeakyHydraulicConductivityTimeUnits).
    FDefaultLeakyHydraulicConductivityTimeUnits: TTimeUnits;
    // @name: @link(TLengthUnits);
    // See @link(DefaultLeakyThicknessUnits).
    FDefaultLeakyThicknessUnits: TLengthUnits;
    // @name: @link(TLengthUnits);
    // See @link(DefaultRiverBedHydraulicConductivityLengthUnits).
    FDefaultRiverBedHydraulicConductivityLengthUnits: TLengthUnits;
    // @name: @link(TTimeUnits);
    // See @link(DefaultRiverBedHydraulicConductivityTimeUnits).
    FDefaultRiverBedHydraulicConductivityTimeUnits: TTimeUnits;
    // @name: @link(TLengthUnits);
    // See @link(DefaultRiverBedThicknessUnits).
    FDefaultRiverBedThicknessUnits: TLengthUnits;
    // @name: @link(TInverseLengthUnits);
    // See @link(DefaultSpecificStorageUnits).
    FDefaultSpecificStorageUnits: TInverseLengthUnits;
    // @name: @link(TTimeUnits);
    // See @link(DefaultTimeUnits).
    FDefaultTimeUnits: TTimeUnits;
    // @name: @link(TLengthUnits);
    // See @link(DefaultVerticalGridUnits).
    FDefaultVerticalGridUnits: TLengthUnits;
    // @name: @link(TLengthUnits);
    // See @link(DefaultWellDiameterUnits).
    FDefaultWellDiameterUnits: TLengthUnits;
    // @name: @link(TTimeUnits);
    // See @link(DefaultWellFlowTimeUnits).
    FDefaultWellFlowTimeUnits: TTimeUnits;
    // @name: @link(TVolumeUnits);
    // See @link(DefaultWellFlowVolumnUnits).
    FDefaultWellFlowVolumnUnits: TVolumeUnits;
    // See @link(DefaultDispersivityUnits).
    procedure SetDefaultDispersivityUnits(const Value: TLengthUnits);
    // See @link(DefaultFluxLengthUnits).
    procedure SetDefaultFluxLengthUnits(const Value: TLengthUnits);
    // See @link(DefaultFluxTimeUnits).
    procedure SetDefaultFluxTimeUnits(const Value: TTimeUnits);
    // See @link(DefaultHeadUnits).
    procedure SetDefaultHeadUnits(const Value: TLengthUnits);
    // See @link(DefaultHorizontalGridUnits).
    procedure SetDefaultHorizontalGridUnits(const Value: TLengthUnits);
    // See @link(DefaultHydraulicConductivityLengthUnits).
    procedure SetDefaultHydraulicConductivityLengthUnits(
      const Value: TLengthUnits);
    // See @link(DefaultHydraulicConductivityTimeUnits).
    procedure SetDefaultHydraulicConductivityTimeUnits(
      const Value: TTimeUnits);
    // See @link(DefaultLeakyHydraulicConductivityLengthUnits).
    procedure SetDefaultLeakyHydraulicConductivityLengthUnits(
      const Value: TLengthUnits);
    // See @link(DefaultLeakyHydraulicConductivityTimeUnits).
    procedure SetDefaultLeakyHydraulicConductivityTimeUnits(
      const Value: TTimeUnits);
    // See @link(DefaultLeakyThicknessUnits).
    procedure SetDefaultLeakyThicknessUnits(const Value: TLengthUnits);
    // See @link(DefaultRiverBedHydraulicConductivityLengthUnits).
    procedure SetDefaultRiverBedHydraulicConductivityLengthUnits(
      const Value: TLengthUnits);
    // See @link(DefaultRiverBedHydraulicConductivityTimeUnits).
    procedure SetDefaultRiverBedHydraulicConductivityTimeUnits(
      const Value: TTimeUnits);
    // See @link(DefaultRiverBedThicknessUnits).
    procedure SetDefaultRiverBedThicknessUnits(const Value: TLengthUnits);
    // See @link(DefaultSpecificStorageUnits).
    procedure SetDefaultSpecificStorageUnits(
      const Value: TInverseLengthUnits);
    // See @link(DefaultTimeUnits).
    procedure SetDefaultTimeUnits(const Value: TTimeUnits);
    // See @link(DefaultVerticalGridUnits).
    procedure SetDefaultVerticalGridUnits(const Value: TLengthUnits);
    // See @link(DefaultWellDiameterUnits).
    procedure SetDefaultWellDiameterUnits(const Value: TLengthUnits);
    // See @link(DefaultWellFlowTimeUnits).
    procedure SetDefaultWellFlowTimeUnits(const Value: TTimeUnits);
    // See @link(DefaultWellFlowVolumnUnits).
    procedure SetDefaultWellFlowVolumnUnits(const Value: TVolumeUnits);
  public
    // If Source is a @classname, @name copies Source to the
    // object that called @name.
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname
    constructor Create;
  published
    // @name is the default units for dispersivity in PHAST.
    property DefaultDispersivityUnits: TLengthUnits read
      FDefaultDispersivityUnits write SetDefaultDispersivityUnits;
    // @name is the default length units for flux in PHAST.
    property DefaultFluxLengthUnits: TLengthUnits read FDefaultFluxLengthUnits
      write SetDefaultFluxLengthUnits;
    // @name is the default time units for flux in PHAST.
    property DefaultFluxTimeUnits: TTimeUnits read FDefaultFluxTimeUnits write
      SetDefaultFluxTimeUnits;
    // @name is the default units for head in PHAST.
    property DefaultHeadUnits: TLengthUnits read FDefaultHeadUnits write
      SetDefaultHeadUnits;
    // @name is the default units for the horizontal grid dimensions in PHAST.
    property DefaultHorizontalGridUnits: TLengthUnits read
      FDefaultHorizontalGridUnits write SetDefaultHorizontalGridUnits;
    // @name is the default length units for the
    // hydraulic conductivity in PHAST.
    property DefaultHydraulicConductivityLengthUnits: TLengthUnits read
      FDefaultHydraulicConductivityLengthUnits write
      SetDefaultHydraulicConductivityLengthUnits;
    // @name is the default time units for the hydraulic conductivity in PHAST.
    property DefaultHydraulicConductivityTimeUnits: TTimeUnits read
      FDefaultHydraulicConductivityTimeUnits write
      SetDefaultHydraulicConductivityTimeUnits;
    // @name is the default length units for the
    // hydraulic conductivity of the leaky boundary condition in PHAST.
    property DefaultLeakyHydraulicConductivityLengthUnits: TLengthUnits read
      FDefaultLeakyHydraulicConductivityLengthUnits write
      SetDefaultLeakyHydraulicConductivityLengthUnits;
    // @name is the default time units for the hydraulic 
    // conductivity of the leaky boundary condition in PHAST.
    property DefaultLeakyHydraulicConductivityTimeUnits: TTimeUnits read
      FDefaultLeakyHydraulicConductivityTimeUnits write
      SetDefaultLeakyHydraulicConductivityTimeUnits;
    // @name is the default units for the thickness of the 
    // leaky boundary condition in PHAST.
    property DefaultLeakyThicknessUnits: TLengthUnits read
      FDefaultLeakyThicknessUnits write SetDefaultLeakyThicknessUnits;
    // @name is the default length units for the hydraulic 
    // conductivity of the river boundary condition in PHAST.
    property DefaultRiverBedHydraulicConductivityLengthUnits: TLengthUnits read
      FDefaultRiverBedHydraulicConductivityLengthUnits write
      SetDefaultRiverBedHydraulicConductivityLengthUnits;
    // @name is the default time units for the hydraulic 
    // conductivity of the river boundary condition in PHAST.
    property DefaultRiverBedHydraulicConductivityTimeUnits: TTimeUnits read
      FDefaultRiverBedHydraulicConductivityTimeUnits write
      SetDefaultRiverBedHydraulicConductivityTimeUnits;
    // @name is the default units for the thickness 
    // of the river boundary condition in PHAST.
    property DefaultRiverBedThicknessUnits: TLengthUnits read
      FDefaultRiverBedThicknessUnits write SetDefaultRiverBedThicknessUnits;
    // @name is the default units for the specific storage in PHAST.
    property DefaultSpecificStorageUnits: TInverseLengthUnits read
      FDefaultSpecificStorageUnits write SetDefaultSpecificStorageUnits;
    // @name is the default units for the time in PHAST.
    property DefaultTimeUnits: TTimeUnits read FDefaultTimeUnits write
      SetDefaultTimeUnits;
    // @name is the default units for the vertical grid dimensions in PHAST.
    property DefaultVerticalGridUnits: TLengthUnits read
      FDefaultVerticalGridUnits write SetDefaultVerticalGridUnits;
    // @name is the default units for the well diameter 
    // in the well boundary condition in PHAST.
    property DefaultWellDiameterUnits: TLengthUnits read
      FDefaultWellDiameterUnits write SetDefaultWellDiameterUnits;
    // @name is the default time units for the pumping 
    // rate in the well boundary condition in PHAST.
    property DefaultWellFlowTimeUnits: TTimeUnits
      read FDefaultWellFlowTimeUnits write SetDefaultWellFlowTimeUnits;
    // @name is the default volume units for the pumping 
    // rate in the well boundary condition in PHAST.
    property DefaultWellFlowVolumnUnits: TVolumeUnits read
      FDefaultWellFlowVolumnUnits write SetDefaultWellFlowVolumnUnits;
  end;

  {@abstract(@name stores options related to the PRINT_INITIAL
    data block in PHAST.)}
  TPrintInitial = class(TPersistent)
  private
    // @name: boolean;
    // See @link(PrintInitialBoundaryConditions).
    FPrintInitialBoundaryConditions: boolean;
    // @name: boolean;
    // See @link(PrintInitialComponents).
    FPrintInitialComponents: boolean;
    // @name: boolean;
    // See @link(PrintInitialConductance).
    FPrintInitialConductance: boolean;
    // @name: boolean;
    // See @link(PrintInitialEchoInput).
    FPrintInitialEchoInput: boolean;
    // @name: boolean;
    // See @link(PrintInitialFluidProperties).
    FPrintInitialFluidProperties: boolean;
    // @name: boolean;
    // See @link(PrintInitialForceChemistryPrint).
    FPrintInitialForceChemistryPrint: boolean;
    // @name: boolean;
    // See @link(PrintInitialHDF_Chemistry).
    FPrintInitialHDF_Chemistry: boolean;
    // @name: boolean;
    // See @link(PrintInitialHDF_Heads).
    FPrintInitialHDF_Heads: boolean;
    // @name: boolean;
    // See @link(PrintInitialHDF_SteadyFlowVelocites).
    FPrintInitialHDF_SteadyFlowVelocites: boolean;
    // @name: boolean;
    // See @link(PrintInitialHeads).
    FPrintInitialHeads: boolean;
    // @name: boolean;
    // See @link(PrintInitialMediaProperties).
    FPrintInitialMediaProperties: boolean;
    // @name: boolean;
    // See @link(PrintInitialSolutionMethod).
    FPrintInitialSolutionMethod: boolean;
    // @name: boolean;
    // See @link(PrintInitialSteadyFlowVelocities).
    FPrintInitialSteadyFlowVelocities: boolean;
    // @name: boolean;
    // See @link(PrintInitialWells).
    FPrintInitialWells: boolean;
    // @name: boolean;
    // See @link(PrintInitialXYZ_Chemistry).
    FPrintInitialXYZ_Chemistry: boolean;
    // @name: boolean;
    // See @link(PrintInitialXYZ_Components).
    FPrintInitialXYZ_Components: boolean;
    // @name: boolean;
    // See @link(PrintInitialXYZ_Heads).
    FPrintInitialXYZ_Heads: boolean;
    // @name: boolean;
    // See @link(PrintInitialXYZ_SteadyFlowVelocities).
    FPrintInitialXYZ_SteadyFlowVelocities: boolean;
    // @name: boolean;
    // See @link(PrintInitialXYZ_Wells).
    FPrintInitialXYZ_Wells: boolean;
    // See @link(PrintInitialBoundaryConditions).
    procedure SetPrintInitialBoundaryConditions(const Value: boolean);
    // See @link(PrintInitialComponents).
    procedure SetPrintInitialComponents(const Value: boolean);
    // See @link(PrintInitialConductance).
    procedure SetPrintInitialConductance(const Value: boolean);
    // See @link(PrintInitialEchoInput).
    procedure SetPrintInitialEchoInput(const Value: boolean);
    // See @link(PrintInitialFluidProperties).
    procedure SetPrintInitialFluidProperties(const Value: boolean);
    // See @link(PrintInitialForceChemistryPrint).
    procedure SetPrintInitialForceChemistryPrint(const Value: boolean);
    // See @link(PrintInitialHDF_Chemistry).
    procedure SetPrintInitialHDF_Chemistry(const Value: boolean);
    // See @link(PrintInitialHDF_Heads).
    procedure SetPrintInitialHDF_Heads(const Value: boolean);
    // See @link(PrintInitialHDF_SteadyFlowVelocites).
    procedure SetPrintInitialHDF_SteadyFlowVelocites(const Value: boolean);
    // See @link(PrintInitialHeads).
    procedure SetPrintInitialHeads(const Value: boolean);
    // See @link(PrintInitialMediaProperties).
    procedure SetPrintInitialMediaProperties(const Value: boolean);
    // See @link(PrintInitialSolutionMethod).
    procedure SetPrintInitialSolutionMethod(const Value: boolean);
    // See @link(PrintInitialSteadyFlowVelocities).
    procedure SetPrintInitialSteadyFlowVelocities(const Value: boolean);
    // See @link(PrintInitialWells).
    procedure SetPrintInitialWells(const Value: boolean);
    // See @link(PrintInitialXYZ_Chemistry).
    procedure SetPrintInitialXYZ_Chemistry(const Value: boolean);
    // See @link(PrintInitialXYZ_Components).
    procedure SetPrintInitialXYZ_Components(const Value: boolean);
    // See @link(PrintInitialXYZ_Heads).
    procedure SetPrintInitialXYZ_Heads(const Value: boolean);
    // See @link(PrintInitialXYZ_SteadyFlowVelocities).
    procedure SetPrintInitialXYZ_SteadyFlowVelocities(
      const Value: boolean);
    // See @link(PrintInitialXYZ_Wells).
    procedure SetPrintInitialXYZ_Wells(const Value: boolean);
  public
    // If Source is a @classname, @name copies Source to the
    // object that called @name.
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname
    constructor Create;
  published
    // @name specifies whether PHAST will print the initial boundary conditions.
    property PrintInitialBoundaryConditions: boolean read
      FPrintInitialBoundaryConditions write SetPrintInitialBoundaryConditions;
    // @name specifies whether PHAST will print the initial components.
    property PrintInitialComponents: boolean read FPrintInitialComponents
      write SetPrintInitialComponents;
    // @name specifies whether PHAST will print the initial conductance.
    property PrintInitialConductance: boolean read FPrintInitialConductance
      write SetPrintInitialConductance;
    // @name specifies whether PHAST will echo the input.
    property PrintInitialEchoInput: boolean read FPrintInitialEchoInput write
      SetPrintInitialEchoInput;
    // @name specifies whether PHAST will print the initial fluid properties.
    property PrintInitialFluidProperties: boolean read
      FPrintInitialFluidProperties write SetPrintInitialFluidProperties;
    // @name specifies whether PHAST will force the chemistry to be printed.
    property PrintInitialForceChemistryPrint: boolean read
      FPrintInitialForceChemistryPrint write SetPrintInitialForceChemistryPrint;
    // @name specifies whether PHAST will print the 
    // initial chemistry in the HDF file.
    property PrintInitialHDF_Chemistry: boolean read FPrintInitialHDF_Chemistry
      write SetPrintInitialHDF_Chemistry;
    // @name specifies whether PHAST will print the 
    // 
    // initial heads in the HDF file.
    property PrintInitialHDF_Heads: boolean read FPrintInitialHDF_Heads write
      SetPrintInitialHDF_Heads;
    // @name specifies whether PHAST will print the 
    // steady flow velocities in the HDF file.
    property PrintInitialHDF_SteadyFlowVelocites: boolean read
      FPrintInitialHDF_SteadyFlowVelocites write
      SetPrintInitialHDF_SteadyFlowVelocites;
    // @name specifies whether PHAST will print the initial heads.
    property PrintInitialHeads: boolean read FPrintInitialHeads write
      SetPrintInitialHeads;
    // @name specifies whether PHAST will print the initial media properties.
    property PrintInitialMediaProperties: boolean read
      FPrintInitialMediaProperties write SetPrintInitialMediaProperties;
    // @name specifies whether PHAST will print the initial solution method.
    property PrintInitialSolutionMethod: boolean
      read FPrintInitialSolutionMethod write SetPrintInitialSolutionMethod;
    // @name specifies whether PHAST will print 
    // the initial steady flow velocities.
    property PrintInitialSteadyFlowVelocities: boolean read
      FPrintInitialSteadyFlowVelocities write
      SetPrintInitialSteadyFlowVelocities;
    // @name specifies whether PHAST will print the initial wells.
    property PrintInitialWells: boolean read FPrintInitialWells write
      SetPrintInitialWells;
    // @name specifies whether PHAST will print 
    // the initial chemistry at X,Y,Z locations.
    property PrintInitialXYZ_Chemistry: boolean read FPrintInitialXYZ_Chemistry
      write SetPrintInitialXYZ_Chemistry;
    // @name specifies whether PHAST will print 
    // the initial components at X,Y,Z locations.
    property PrintInitialXYZ_Components: boolean
      read FPrintInitialXYZ_Components write SetPrintInitialXYZ_Components;
    // @name specifies whether PHAST will print 
    // the initial heads at X,Y,Z locations.
    property PrintInitialXYZ_Heads: boolean read FPrintInitialXYZ_Heads
      write SetPrintInitialXYZ_Heads;
    // @name specifies whether PHAST will print 
    // the initial steady flow velocities at X,Y,Z locations.
    property PrintInitialXYZ_SteadyFlowVelocities: boolean
      read FPrintInitialXYZ_SteadyFlowVelocities
      write SetPrintInitialXYZ_SteadyFlowVelocities;
    // @name specifies whether PHAST will print 
    // the initial wells at X,Y,Z locations.
    property PrintInitialXYZ_Wells: boolean read FPrintInitialXYZ_Wells
      write SetPrintInitialXYZ_Wells;
  end;

  {@abstract(@name is used to store options related to the grid in PHAST.)}
  TGridOptions = class(TPersistent)
  private
    // @name: boolean;
    // See @link(ChemicalDimensionX).
    FChemicalDimensionX: boolean;
    // @name: boolean;
    // See @link(ChemicalDimensionY).
    FChemicalDimensionY: boolean;
    // @name: boolean;
    // See @link(ChemicalDimensionZ).
    FChemicalDimensionZ: boolean;
    // @name: boolean;
    // See @link(PrintOrientation).
    FPrintOrientation: TpgPrintOrientation;
    // See @link(ChemicalDimensionX).
    procedure SetChemicalDimensionX(const Value: boolean);
    // See @link(ChemicalDimensionY).
    procedure SetChemicalDimensionY(const Value: boolean);
    // See @link(ChemicalDimensionZ).
    procedure SetChemicalDimensionZ(const Value: boolean);
    // See @link(PrintOrientation).
    procedure SetPrintOrientation(const Value: TpgPrintOrientation);
  public
    // If Source is a @classname, @name copies Source to the
    // object that called @name.
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname
    constructor Create;
  published
    // @name determines whether chemical calculations in the X direction
    // are performed.
    property ChemicalDimensionX: boolean read FChemicalDimensionX
      write SetChemicalDimensionX;
    // @name determines whether chemical calculations in the Y direction
    // are performed.
    property ChemicalDimensionY: boolean read FChemicalDimensionY
      write SetChemicalDimensionY;
    // @name determines whether chemical calculations in the Z direction
    // are performed.
    property ChemicalDimensionZ: boolean read FChemicalDimensionZ
      write SetChemicalDimensionZ;
    // @name determines whether PHAST prints data in an X,Y or X,Z orientation.
    property PrintOrientation: TpgPrintOrientation read FPrintOrientation
      write SetPrintOrientation;
  end;

  {@abstract(@name is used to store fluid properties in PHAST.)}
  TFluidProperties = class(TComponent)
  private
    // @name: boolean;
    // Values of zero are never stored for real-number properties
    // in Delphi.  @name is used to get around this limitation so that
    // if a value of zero for @link(FluidCompressibility) is specified by
    // the user, it can be recovered correctly when reading the file.
    FCompressibilitySet: boolean;
    // @name: boolean;
    // Values of zero are never stored for real-number properties
    // in Delphi.  @name is used to get around this limitation so that
    // if a value of zero for @link(FluidDensity) is specified by
    // the user, it can be recovered correctly when reading the file.
    FDensitySet: boolean;
    // Values of zero are never stored for real-number properties
    // in Delphi.  @name is used to get around this limitation so that
    // if a value of zero for @link(FluidDiffusivity) is specified by
    // the user, it can be recovered correctly when reading the file.
    FDiffusivitySet: boolean;
    // @name: double;
    // See @link(FluidCompressibility).
    FFluidCompressibility: double;
    // @name: double;
    // See @link(FluidDensity).
    FFluidDensity: Double;
    // @name: double;
    // See @link(FluidDiffusivity).
    FFluidDiffusivity: double;
    // @name: double;
    // See @link(FluidViscosity).
    FFluidViscosity: double;
    // @name: boolean;
    // Values of zero are never stored for real-number properties
    // in Delphi.  @name is used to get around this limitation so that
    // if a value of zero for @link(FluidViscosity) is specified by
    // the user, it can be recovered correctly when reading the file.
    FViscositySet: boolean;
    // See @link(FluidCompressibility).
    procedure SetFluidCompressibility(const Value: double);
    // See @link(FluidDensity).
    procedure SetFluidDensity(const Value: Double);
    // See @link(FluidDiffusivity).
    procedure SetFluidDiffusivity(const Value: double);
    // See @link(FluidViscosity).
    procedure SetFluidViscosity(const Value: double);
  protected
    // @name checks @link(FCompressibilitySet), @link(FDensitySet),
    // and @link(FViscositySet). If any of them is false, the corresponding
    // property is set to zero.
    procedure Loaded; override;
    // @name initializes all published properties.
    procedure Initialize;
  public
    // If Source is a @classname, @name copies Source to the
    // object that called @name.
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname
    constructor Create(AOwner: TComponent); override;
  published
    // @name is the fluid compressibility.
    property FluidCompressibility: double read FFluidCompressibility
      write SetFluidCompressibility;
    // @name is the fluid density.
    property FluidDensity: Double read FFluidDensity write SetFluidDensity;
    // @name is the fluid diffusivity.
    property FluidDiffusivity: double read FFluidDiffusivity
      write SetFluidDiffusivity;
    // @name is the fluid viscosity..
    property FluidViscosity: double read FFluidViscosity
      write SetFluidViscosity;
  end;

  {@abstract(@name specifies options related to the solution method in PHAST.)}
  TSolutionOptions = class(TComponent)
  private
    // See @link(CrossDispersion).
    FCrossDispersion: boolean;
    // See @link(MaximumIterations).
    FMaximumIterations: integer;
    // See @link(SaveDirections).
    FSaveDirections: integer;
    // See @link(SolverType).
    FSolverType: TPhastSolver;
    // See @link(SpaceDifferencing).
    FSpaceDifferencing: double;
    // See @link(TimeDifferencing).
    FTimeDifferencing: double;
    // Values of zero are never stored for real-number properties
    // in Delphi.  @name is used to get around this limitation so that
    // if a value of zero for @link(TimeDifferencing) is specified by
    // the user, it can be recovered correctly when reading the file.
    FTimeDifferencingSet: boolean;
    // See @link(Tolerance).
    FTolerance: double;
    // Values of zero are never stored for real-number properties
    // in Delphi.  @name is used to get around this limitation so that
    // if a value of zero for @link(Tolerance) is specified by
    // the user, it can be recovered correctly when reading the file.
    FToleranceSet: boolean;
    FRebalanceByCell: boolean;
    FRebalanceFraction: TRealStorage;
    // See @link(CrossDispersion).
    procedure SetCrossDispersion(const Value: boolean);
    // See @link(MaximumIterations).
    procedure SetMaximumIterations(const Value: integer);
    // See @link(SaveDirections).
    procedure SetSaveDirections(const Value: integer);
    // See @link(SolverType).
    procedure SetSolverType(const Value: TPhastSolver);
    // See @link(SpaceDifferencing).
    procedure SetSpaceDifferencing(const Value: double);
    // See @link(TimeDifferencing).
    procedure SetTimeDifferencing(const Value: double);
    // See @link(Tolerance).
    procedure SetTolerance(const Value: double);
    procedure SetRebalanceByCell(const Value: boolean);
    procedure SetRebalanceFraction(const Value: TRealStorage);
  protected
    // @name initializes all published properties.
    procedure Initialize;
    // @name checks @link(FTimeDifferencingSet)
    // and @link(FToleranceSet). If one of them is false, the corresponding
    // property is set to zero.
    procedure Loaded; override;
  public
    // If Source is a @classname, @name copies Source to the
    // object that called @name.
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // @name specifies whether PHAST should use cross-dispersion.
    property CrossDispersion: boolean read FCrossDispersion write
      SetCrossDispersion stored True;
    // @name specifies the maximum number of iterations in PHAST.
    property MaximumIterations: integer read FMaximumIterations write
      SetMaximumIterations default 500;
    property RebalanceFraction: TRealStorage read FRebalanceFraction
      write SetRebalanceFraction stored True;
    property RebalanceByCell: boolean read FRebalanceByCell
      write SetRebalanceByCell;
    // @name specifies the number of save-directions in PHAST.
    property SaveDirections: integer read FSaveDirections
      write SetSaveDirections default 10;
    // @name specifies which solver to use in PHAST.
    property SolverType: TPhastSolver read FSolverType write SetSolverType
      default psIterative;
    // @name specifies the space-differencing in PHAST.
    property SpaceDifferencing: double read FSpaceDifferencing write
      SetSpaceDifferencing stored True;
    // @name specifies the time-differencing in PHAST.
    property TimeDifferencing: double read FTimeDifferencing write
      SetTimeDifferencing stored True;
    // @name specifies the tolerance in PHAST.
    property Tolerance: double read FTolerance write SetTolerance stored True;
  end;

  {@abstract(@name is used to store options related to the STEADY_FLOW
    data block in PHAST.)}
  TSteadyFlowOptions = class(TPersistent)
  private
    // @name: double;
    // See @link(FlowBalanceTolerance).
    FFlowBalanceTolerance: double;
    // @name: double;
    // See @link(HeadChangeLimit).
    FHeadChangeLimit: double;
    // @name: double;
    // See @link(HeadTolerance).
    FHeadTolerance: double;
    // @name: integer;
    // See @link(Iterations).
    FIterations: integer;
    // @name: double;
    // See @link(MaximumTimeStep).
    FMaximumTimeStep: double;
    // @name: double;
    // See @link(MinimumTimeStep).
    FMinimumTimeStep: double;
    // @name: boolean;
    // See @link(SteadyFlow).
    FSteadyFlow: boolean;
    // @name: boolean;
    // See @link(UseDefaultHeadChangeLimit).
    FUseDefaultHeadChangeLimit: boolean;
    // @name: boolean;
    // See @link(UseDefaultMaximumTimeStep).
    FUseDefaultMaximumTimeStep: boolean;
    // @name: boolean;
    // See @link(UseDefaultMinimumTimeStep).
    FUseDefaultMinimumTimeStep: boolean;
    // See @link(FlowBalanceTolerance).
    procedure SetFlowBalanceTolerance(const Value: double);
    // See @link(HeadChangeLimit).
    procedure SetHeadChangeLimit(const Value: double);
    // See @link(HeadTolerance).
    procedure SetHeadTolerance(const Value: double);
    // See @link(Iterations).
    procedure SetIterations(const Value: integer);
    // See @link(MaximumTimeStep).
    procedure SetMaximumTimeStep(const Value: double);
    // See @link(MinimumTimeStep).
    procedure SetMinimumTimeStep(const Value: double);
    // See @link(SteadyFlow).
    procedure SetSteadyFlow(const Value: boolean);
    // See @link(UseDefaultHeadChangeLimit).
    procedure SetUseDefaultHeadChangeLimit(const Value: boolean);
    // See @link(UseDefaultMaximumTimeStep).
    procedure SetUseDefaultMaximumTimeStep(const Value: boolean);
    // See @link(UseDefaultMinimumTimeStep).
    procedure SetUseDefaultMinimumTimeStep(const Value: boolean);
  public
    // If Source is a @classname, @name copies Source to the
    // object that called @name.
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname
    constructor Create;
  published
    // @name is the error tolerance for the flow balance in PHAST.
    property FlowBalanceTolerance: double read FFlowBalanceTolerance
      write SetFlowBalanceTolerance;
    // @name is the maximum head change in one time step in PHAST.
    property HeadChangeLimit: double read FHeadChangeLimit
      write SetHeadChangeLimit;
    // @name is the error tolerance for the head in PHAST.
    property HeadTolerance: double read FHeadTolerance write SetHeadTolerance;
    // @name is the maximum number of iterations in PHAST.
    property Iterations: integer read FIterations write SetIterations
      default 100;
    // @name is the maximum time-step size in PHAST.
    property MaximumTimeStep: double read FMaximumTimeStep
      write SetMaximumTimeStep;
    // @name is the minimum time-step size in PHAST.
    property MinimumTimeStep: double read FMinimumTimeStep
      write SetMinimumTimeStep;
    // @name species whether or not the PHAST is a steady-state simulation.
    property SteadyFlow: boolean read FSteadyFlow write SetSteadyFlow;
    // @name specifies whether PHAST should use the default head-change limit
    // or used @link(HeadChangeLimit).
    property UseDefaultHeadChangeLimit: boolean read FUseDefaultHeadChangeLimit
      write SetUseDefaultHeadChangeLimit default True;
    // @name specifies whether PHAST should use the default maximum time-step
    // size or used @link(MaximumTimeStep).
    property UseDefaultMaximumTimeStep: boolean read FUseDefaultMaximumTimeStep
      write SetUseDefaultMaximumTimeStep default True;
    // @name specifies whether PHAST should use the default minimum time-step
    // size or used @link(MinimumTimeStep).
    property UseDefaultMinimumTimeStep: boolean read FUseDefaultMinimumTimeStep
      write SetUseDefaultMinimumTimeStep default True;
  end;

  {@abstract(@name stores options related to chemistry in PHAST.)}
  TChemistryOptions = class(TPersistent)
  private
    // @name: boolean;
    // See @link(UseEquilibriumPhases).
    FUseEquilibriumPhases: boolean;
    // @name: boolean;
    // See @link(UseExchange).
    FUseExchange: boolean;
    // @name: boolean;
    // See @link(UseGasPhases).
    FUseGasPhases: boolean;
    // @name: boolean;
    // See @link(UseKineticReactants).
    FUseKineticReactants: boolean;
    // @name: boolean;
    // See @link(UseSolidSolution).
    FUseSolidSolution: boolean;
    // @name: boolean;
    // See @link(UseSurfaceAssemblages).
    FUseSurfaceAssemblages: boolean;
    // See @link(UseEquilibriumPhases).
    procedure SetUseEquilibriumPhases(const Value: boolean);
    // See @link(UseExchange).
    procedure SetUseExchange(const Value: boolean);
    // See @link(UseGasPhases).
    procedure SetUseGasPhases(const Value: boolean);
    // See @link(UseKineticReactants).
    procedure SetUseKineticReactants(const Value: boolean);
    // See @link(UseSolidSolution).
    procedure SetUseSolidSolution(const Value: boolean);
    // See @link(UseSurfaceAssemblages).
    procedure SetUseSurfaceAssemblages(const Value: boolean);
  public
    // If Source is a @classname, @name copies Source to the
    // object that called @name.
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname
    constructor Create;
  published
    // @name specifies whether or not PHAST will use equilibrium phases.
    property UseEquilibriumPhases: boolean read FUseEquilibriumPhases
      write SetUseEquilibriumPhases stored True;
    // @name specifies whether or not PHAST will use exchange.
    property UseExchange: boolean read FUseExchange write SetUseExchange
      stored True;
    // @name specifies whether or not PHAST will use gas phases.
    property UseGasPhases: boolean read FUseGasPhases write SetUseGasPhases
      stored True;
    // @name specifies whether or not PHAST will use kinetic reactions.
    property UseKineticReactants: boolean read FUseKineticReactants
      write SetUseKineticReactants stored True;
    // @name specifies whether or not PHAST will use solid solutions.
    property UseSolidSolution: boolean read FUseSolidSolution
      write SetUseSolidSolution stored True;
    // @name specifies whether or not PHAST will use surface assemblages.
    property UseSurfaceAssemblages: boolean read FUseSurfaceAssemblages
      write SetUseSurfaceAssemblages stored True;
  end;

  {@abstract(@name is used to group together a series of related
    @link(TPhastTimeList)s.)}
  TTimeListGroup = class(TObject)
  private
    // @name: TList;
    // @name stores the @link(TPhastTimeList)s.
    FList: TList;
    // @name: string;
    // See @link(Name).
    FName: string;
    // See @link(Items).
    function GetItems(const Index: integer): TPhastTimeList;
    // See @link(Items).
    procedure SetItems(const Index: integer; const Value: TPhastTimeList);
    // See @link(Name).
    procedure SetName(const Value: string);
  public
    // @name adds TimeList to the group stored in this @classname.
    // @returns(@name returns the position in Items at
    // which TimeList is stored.)
    function Add(const TimeList: TPhastTimeList): integer;
    // @name is the number of @link(TPhastTimeList)s in this @classname.
    function Count: integer;
    // @name creates an instance of @classname
    constructor Create;
    // @name destroys the current instance of @classname.
    // do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name returns the position of TimeList in @classname.
    // If TimeList is not in @classname, @name returns -1.
    function IndexOf(const TimeList: TPhastTimeList): integer;
    // @name provides access to the @link(TPhastTimeList)s in @classname.
    property Items[const Index: integer]: TPhastTimeList read GetItems
      write SetItems;
    // @name is a name for the group of @link(TPhastTimeList)s.
    property Name: string read FName write SetName;
  end;


  TLookUpList = class(TStringList)
  private
    FLastIndex: integer;
  public
    Constructor Create;
  end;

{ TODO : 
Make comments a pervasive feature of the model.  The project as whole
should show the project name, author, date, and coordinate system.
There should also be a general comment field for the project.  Each
DataArray and  ScreenObject should have a contour and there should
also be comments for each DataArray or boundary condition specified
by a ScreenObject.  Any special dialog box that has a preserved state
that affects the model output should also have a comment. }
  TCustomModel = class;

  TDataArrayManager = class(TObject)
  private
    FCustomModel: TCustomModel;
    // @name is used to store the @link(TDataArray)s in the model that
    // are defined throughout the grid.  An example is the data set for
    // the hydraulic conductivity in the X direction.
    FDataSets: TObjectList;
    FDataSetsToCache: TList;
    // @name is used to store @link(TDataArray)s that are related to
    // boundary conditions but which do not vary with time.
    FBoundaryDataSets: TObjectList;
    FDataSetLookUpList: THashTable;
    FRiverDataSets: TList;
    FStoreCachedData: boolean;
    // @name is used to store @link(TDataArray)s that have been deleted
    // so that they can be restored later.
    FDeletedDataSets: TList;
    // See @link(DataSetCount).
    function GetDataSetCount: integer;
    // See @link(DataSets).
    function GetDataSets(const Index: integer): TDataArray;
    // See @link(BoundaryDataSetCount).
    function GetBoundaryDataSetCount: integer;
    // See @link(BoundaryDataSets).
    function GetBoundaryDataSets(const Index: integer): TDataArray;
    // @name returns the position of the @link(TDataArray) whose name is
    // DataSetName in List.
    // @name is used in @link(IndexOfBoundaryDataSet)
    // and @link(IndexOfDataSet).
    function IndexOfDataSetInList(DataSetName: string;
      const List: TObjectList): integer;
    function GetDataSetsCapacity: integer;
    procedure SetDataSetsCapacity(const Value: integer);
    procedure Invalidate;
    procedure DefinePackageDataArrays;
    procedure InvalidateDataSetLookupList;
    function DataArrayHeld(DataArray: TDataArray): boolean;
    function GetChildDataArrayManager(Index: integer): TDataArrayManager;
    function GetChildDataArrayManagerCount: integer;
  public
    FDataArrayCreationRecords: array of TDataSetCreationData;
    procedure Assign(Source: TDataArrayManager);
    // @name indicates the number of @link(TDataArray)s in @link(DataSets).
    procedure AddDataSetToLookUpList(const DataSet: TDataArray);
    Constructor Create(Model: TCustomModel);
    Destructor Destroy; override;
    procedure ClearDataSetsToCache;
    procedure ClearAllDataSets;
    // @name holds all the @link(TDataArray)s that are related to the river
    // boundary but which are not transient.
    property RiverDataSets: TList read FRiverDataSets;
    property StoreCachedData: boolean read FStoreCachedData
      write FStoreCachedData;
    property DataSetCount: integer read GetDataSetCount;
    // @name is used to access the @link(TDataArray)s that are defined
    // throughout the grid.
    property DataSets[const Index: integer]: TDataArray read GetDataSets;
    // @name is used to determine the number of @link(TDataArray)s in
    // @link(BoundaryDataSets).  Only data sets that don't vary with
    // time yet are related to boundary conditions are in
    // @link(BoundaryDataSets).
    property BoundaryDataSetCount: integer read GetBoundaryDataSetCount;
    // @name is used to access @link(TDataArray)s in
    // @link(FBoundaryDataSets).  Only data sets that don't vary with
    // time yet are related to boundary conditions are in
    // @link(FBoundaryDataSets).
    property BoundaryDataSets[const Index: integer]: TDataArray
      read GetBoundaryDataSets;
    property DataSetsCapacity: integer read GetDataSetsCapacity
      write SetDataSetsCapacity;
    // @name adds DataSet to @link(FDataSets) and calls @link(AddDataSetToLookUpList);
    function AddDataSet(const DataSet: TDataArray): Integer; virtual;
    procedure AddDataSetToCache(DataArray: TDataArray);
    procedure DontCache(DataArray: TDataArray);
    procedure CacheDataArrays;
    // @name creates a new @link(TDataArray) and adds it to @link(DataSets).
    function CreateNewDataArray(const ClassType: TDataArrayType;
      const Name, Formula: string; Lock: TDataLock; DataType: TRbwDataType;
      EvaluatedAt: TEvaluatedAt; Orientation: TDataSetOrientation;
      const Classification: string): TDataArray;
    // @name retrieves a @link(TDataArray) from
    // @link(DataSets) based on its name.
    function GetDataSetByName(const DataSetName: string): TDataArray;
    procedure CreateInitialDataSets;
    procedure RemoveDataSetFromLookUpList(const DataSet: TDataArray);
    // @name creates non-transient @link(TDataArray)s for boundary conditions.
    // @name is used to store DataSet in @link(FBoundaryDataSets).
    function AddBoundaryDataSet(const DataSet: TDataArray): Integer;
    // @name creates non-transient @link(TDataArray)s for boundary conditions.
    procedure CreateInitialBoundaryDataSets;
    // @name removes DataSet from @link(DataSets) without freeing it.
    procedure ExtractDataSet(const DataSet: TDataArray);
    // @name returns the position of the @link(TDataArray) in
    // @link(DataSets) whose Name is DataSetName. If none has that
    // name, @name returns -1.
    function IndexOfDataSet(DataSetName: string): integer;
    // @name returns the position of the @link(TDataArray) in
    // @link(BoundaryDataSets) whose Name is DataSetName. If none has that
    // name, @name returns -1.
    function IndexOfBoundaryDataSet(DataSetName: string): integer;
    procedure UpdateDataSetDimensions;
    procedure InvalidateAllDataSets;
    procedure ClearDeletedDataSets;
    procedure UnlinkDeletedDataSets;
    procedure HandleAddedDataArrays(AddedDataSetList: TList);
    procedure HandleDeletedDataArrays(DeletedDataSetList: TList);
    property ChildDataArrayManagerCount: integer
      read GetChildDataArrayManagerCount;
    property ChildDataArrayManagers[Index: integer]: TDataArrayManager
      read GetChildDataArrayManager;
  end;

  TChildModelCollection = class;

  TCustomModel = class abstract (TComponent)
  private
    FClearing: Boolean;
    FDataArrayManager: TDataArrayManager;
    FAlternateFlowPackage: boolean;
    FAlternateSolver: boolean;
    FBatchFileAdditionsAfterModel: TStrings;
    FBatchFileAdditionsBeforeModel: TStrings;
    FModflowGrid: TModflowGrid;
    FModflowNameFileLines: TStrings;
    FModflowPackages: TModflowPackages;
    FDrainObservations: TFluxObservationGroups;
    FGhbObservations: TFluxObservationGroups;
    FHeadFluxObservations: TFluxObservationGroups;
    FRiverObservations: TFluxObservationGroups;
    FHydrogeologicUnits: THydrogeologicUnits;
    FFilesToArchive: TStrings;
    FModelInputFiles: TStrings;
    FFileName: string;
    FModflowWettingOptions: TWettingOptions;

    FrpFrontFormulaCompiler: TRbwParser;
    FrpFrontFormulaCompilerNodes: TRbwParser;
    FrpSideFormulaCompiler: TRbwParser;
    FrpSideFormulaCompilerNodes: TRbwParser;
    FrpThreeDFormulaCompiler: TRbwParser;
    FrpThreeDFormulaCompilerNodes: TRbwParser;
    FrpTopFormulaCompiler: TRbwParser;
    FrpTopFormulaCompilerNodes: TRbwParser;
    FParsers: TList;
    FGlobalVariables: TGlobalVariables;
    FThreeDGridObserver: TObserver;
    FTopGridObserver: TObserver;
    FEdgeDisplay: TCustomModflowGridEdgeDisplay;
    FHufKxNotifier: TObserver;
    FHufKyNotifier: TObserver;
    FHufKzNotifier: TObserver;
    FHufSsNotifier: TObserver;
    FHufSyNotifier: TObserver;
    // See @link(UpToDate).
    FUpToDate: boolean;

    function AlwaysUsed(Sender: TObject): boolean;
    function AquiferPropertiesUsed(Sender: TObject): boolean;
    function KyUsed(Sender: TObject): boolean;
    function KzUsed(Sender: TObject): boolean;
    function PorosityUsed(Sender: TObject): boolean;
    function SpecificStorageUsed(Sender: TObject): boolean;
    // @name returns true if the model uses solute transport.
    // @name is used an event handler for
    // TDataArray.@link(TDataArray.OnDataSetUsed)
    function ChemistryUsed(Sender: TObject): boolean;
    // @name returns true if the model uses initial heads.
    // @name is used an event handler for
    // TDataArray.@link(TDataArray.OnDataSetUsed)
    function InitialHeadUsed(Sender: TObject): boolean;
    // @name returns true if the model uses equilibrium phases.
    // @name is used an event handler for
    // TDataArray.@link(TDataArray.OnDataSetUsed)
    function EquilibriumPhasesUsed(Sender: TObject): boolean;
    // @name returns true if the model uses surface reactions.
    // @name is used an event handler for
    // TDataArray.@link(TDataArray.OnDataSetUsed)
    function SurfacesUsed(Sender: TObject): boolean;
    // @name returns true if the model uses exchange reactions.
    // @name is used an event handler for
    // TDataArray.@link(TDataArray.OnDataSetUsed)
    function ExchangeUsed(Sender: TObject): boolean;
    // @name returns true if the model uses gas phases.
    // @name is used an event handler for
    // TDataArray.@link(TDataArray.OnDataSetUsed)
    function GasPhaseUsed(Sender: TObject): boolean;
    // @name returns true if the model uses solid solutions.
    // @name is used an event handler for
    // TDataArray.@link(TDataArray.OnDataSetUsed)
    function SolidSolutionUsed(Sender: TObject): boolean;
    // @name returns true if the model uses kinetics.
    // @name is used an event handler for
    // TDataArray.@link(TDataArray.OnDataSetUsed)
    function KineticsUsed(Sender: TObject): boolean;
    function ModflowUsed(Sender: TObject): boolean;
    function RouteUzfDischarge(Sender: TObject): boolean;
    function ModflowInitialHeadUsed(Sender: TObject): boolean;
    function ConfiningBedKzUsed(Sender: TObject): boolean;
    function VerticalAnisotropyUsed(Sender: TObject): boolean;
    function HorizontalAnisotropyUsed(Sender: TObject): boolean;
    function SpecificYieldUsed(Sender: TObject): boolean;
    function WetDryUsed(Sender: TObject): boolean;
    function ModpathUsed(Sender: TObject): boolean;
    function HufReferenceSurfaceNeeded(Sender: TObject): boolean;
    function BcfUsed(Sender: TObject): boolean;
    function ConfinedStorageCoefUsed(Sender: TObject): boolean;
    function OptionalDataSet(Sender: TObject): boolean;
    function HufSelected(Sender: TObject): boolean;
    function HufStorageUsed(Sender: TObject): boolean;
    function ZoneBudgetSelected(Sender: TObject): boolean;
    function SwtSelected(Sender: TObject): boolean;
    function SwtOffsetsUsed(Sender: TObject): boolean;
    function SwtSpecifiedUsed(Sender: TObject): boolean;
    function IndenticalTransientArray(DataArray: TDataArray; DataArrays: TList;
      var CachedIndex: integer): TDataArray;
    // See @link(TimeLists).
    function GetTimeLists(Index: integer): TCustomTimeList;
    // See @link(TimeListCount).
    function GetTimeListCount: integer;
    procedure ClearAllTimeLists;
    procedure InitializeHobDisplay(Sender: TObject);
    function ModflowHobPackageUsed(Sender: TObject): boolean;
    procedure GetMfHobHeadsUseList(Sender: TObject; NewUseList: TStringList);
    procedure CreateModflowDisplayTimeLists;
    procedure SetUnitNumbers(const Value: TUnitNumbers);
    procedure UpdateHfb(Sender: TObject);
    // See @link(DataSetList).
    function GetDataSetCollection: TDataSetCollection;
    procedure FinalizePvalAndTemplate(FileName: string);
    procedure FinalizeDischargeRouting(Sender: TObject);
    procedure GetParameterUsedAndParameterFormulaForLPF(
      out ParameterUsed: Boolean; out ParameterFormula: string;
      ParameterType: TParameterType);
    procedure UpdateLpfDataArrayParameterUsed(const DataArrayName: string;
      ParameterType: TParameterType);

  private
    FGrid: TCustomGrid;
    procedure SetAlternateFlowPackage(const Value: boolean);
    procedure SetAlternateSolver(const Value: boolean);
    procedure SetBatchFileAdditionsAfterModel(const Value: TStrings);
    procedure SetBatchFileAdditionsBeforeModel(const Value: TStrings);
    procedure SetModflowGrid(const Value: TModflowGrid);
    procedure SetModflowNameFileLines(const Value: TStrings);
    procedure SetModflowPackages(const Value: TModflowPackages);
    // See @link(UpToDate).
    procedure SetUpToDate(const Value: boolean); virtual;
    procedure SetHeadFluxObservations(const Value: TFluxObservationGroups);
    procedure SetRiverObservations(const Value: TFluxObservationGroups);
    procedure SetDrainObservations(const Value: TFluxObservationGroups);
    procedure SetGhbObservations(const Value: TFluxObservationGroups);
    procedure SetHydrogeologicUnits(const Value: THydrogeologicUnits);
    procedure SetFilesToArchive(const Value: TStrings);
    procedure SetModelInputFiles(const Value: TStrings);
    procedure SetFileName(const Value: string);
    procedure SetModflowWettingOptions(const Value: TWettingOptions);


    procedure ClearParsers;
    function GetParsers(Index: integer): TRbwParser;
    procedure SetGlobalVariables(const Value: TGlobalVariables);
    procedure SetEdgeDisplay(const Value: TCustomModflowGridEdgeDisplay);
    procedure AllObserversStopTalking;
    procedure FreeHufNotifiers;
    procedure FreeGridNotifiers;
    function GetModflowFullStressPeriods: TModflowStressPeriods; virtual; abstract;

  var
    FTransientMultiplierArrays: TList;
    FCachedMultiplierArrayIndex: Integer;
    FTransientZoneArrays: TList;
    FCachedZoneArrayIndex: integer;
    FUpdatingFullStressPeriods: Boolean;
    // See @link(DataSetList).
    FDataSetCollection: TDataSetCollection;
    // See @link(FrontTimeList).
    FFrontTimeList: TCustomTimeList;
    // See @link(SideTimeList).
    FSideTimeList: TCustomTimeList;
    // See @link(TopTimeList).
    FTopTimeList: TCustomTimeList;
    // See @link(TopDisplayTime).
    FTopDisplayTime: double;
    // See @link(FrontDisplayTime).
    FFrontDisplayTime: double;
    // See @link(SideDisplayTime).
    FSideDisplayTime: double;
    // See @link(ThreeDDisplayTime).
    FThreeDDisplayTime: double;
    // See @link(ThreeDTimeList).
    FThreeDTimeList: TCustomTimeList;
    FPValFile: TStringList;
    FTemplate: TStringList;
  strict private
    FHfbWriter: TObject;
    FMfHobHeads: THobDisplayTimeList;
    FUnitNumbers: TUnitNumbers;
    FHfbDisplayer: THfbDisplayer;

    // @name holds the @link(TCustomTimeList)s in the model.
    // See @link(TimeLists).
    FTimeLists: TList;

  protected
    function GetScreenObjects(const Index: integer): TScreenObject;virtual;abstract;
    function GetScreenObjectCount: integer;virtual;abstract;
    function GetModflowSteadyParameters: TModflowSteadyParameters;virtual;abstract;
    procedure SetModflowSteadyParameters(const Value: TModflowSteadyParameters);virtual;abstract;
    function GetModelSelection: TModelSelection;virtual;abstract;
    procedure SetModelSelection(const Value: TModelSelection);virtual;abstract;
    // @name causes the grid to not be colored by any @link(TDataArray).
    function GetLayerStructure: TLayerStructure;virtual;abstract;
    procedure SetLayerStructure(const Value: TLayerStructure);virtual;abstract;
    procedure SetModflowOptions(const Value: TModflowOptions);virtual;abstract;
    function GetModflowOptions: TModflowOptions;virtual;abstract;
    function GetModflowStressPeriods: TModflowStressPeriods;virtual;abstract;
    procedure SetModflowStressPeriods(const Value: TModflowStressPeriods);virtual;abstract;
    procedure SetSoluteTransport(const Value: boolean);virtual;abstract;
    function GetSoluteTransport: boolean;virtual;abstract;
    procedure SetUseWaterTable(const Value: boolean);virtual;abstract;
    function GetUseWaterTable: boolean;virtual;abstract;
    function GetFreeSurface: boolean;virtual;abstract;
    procedure SetFreeSurface(const Value: boolean);virtual;abstract;
    procedure SetChemistryOptions(const Value: TChemistryOptions); virtual; abstract;
    function GetChemistryOptions: TChemistryOptions; virtual; abstract;
    procedure SetHufParameters(const Value: THufModflowParameters);virtual;abstract;
    function GetHufParameters: THufModflowParameters;virtual;abstract;
    function GetObservationPurpose: TObservationPurpose; virtual; abstract;
    procedure SetObservationPurpose(const Value: TObservationPurpose); virtual; abstract;
    function GetModflowTransientParameters: TModflowTransientListParameters; virtual;abstract;
    procedure SetModflowTransientParameters(
      const Value: TModflowTransientListParameters);virtual;abstract;
    function GetModflowOutputControl: TModflowOutputControl;virtual;abstract;
    procedure SetModflowOutputControl(const Value: TModflowOutputControl);virtual;abstract;
    function GetProgramLocations: TProgramLocations;virtual;abstract;
    procedure SetProgramLocations(const Value: TProgramLocations);virtual;abstract;
    procedure ClearViewedItems; virtual;
    procedure InternalClear; virtual;
  public
    procedure Assign(Source: TPersistent); override;
    property Clearing: Boolean read FClearing;
    property DataArrayManager: TDataArrayManager read FDataArrayManager;

    procedure OnActiveDataSetChanged(Sender: TObject);
    procedure Clear;
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    // Call @name to indicate that the model has changed in some important
    // respect.  The user will be prompted to save the model when closing.
    procedure Invalidate; virtual;
    // @name indicates whether or not the model needs to be saved to file.
    // See @link(Invalidate).
    property UpToDate: boolean read FUpToDate write SetUpToDate;
    procedure AddModelInputFile(const FileName: string);
    procedure AddFileToArchive(const FileName: string);

    property Grid: TCustomGrid read FGrid;

    // @name is the TRbwParser for formulas for data sets on the front
    // view of the model evaluated at elements.
    property rpFrontFormulaCompiler: TRbwParser read FrpFrontFormulaCompiler;
    // @name is the TRbwParser for formulas for data sets on the front
    // view of the model evaluated at nodes.
    property rpFrontFormulaCompilerNodes: TRbwParser
      read FrpFrontFormulaCompilerNodes;
    // @name is the TRbwParser for formulas for data sets on the side
    // view of the model evaluated at elements.
    property rpSideFormulaCompiler: TRbwParser read FrpSideFormulaCompiler;
    // @name is the TRbwParser for formulas for data sets on the side
    // view of the model evaluated at nodes.
    property rpSideFormulaCompilerNodes: TRbwParser
      read FrpSideFormulaCompilerNodes;
    // @name is the TRbwParser for formulas for 3D data sets
    // view of the model evaluated at elements.
    property rpThreeDFormulaCompiler: TRbwParser read FrpThreeDFormulaCompiler;
    // @name is the TRbwParser for formulas for 3D data sets
    // view of the model evaluated at nodes.
    property rpThreeDFormulaCompilerNodes: TRbwParser
      read FrpThreeDFormulaCompilerNodes;
    // @name is the TRbwParser for formulas for data sets on the top
    // view of the model evaluated at elements.
    property rpTopFormulaCompiler: TRbwParser read FrpTopFormulaCompiler;
    // @name is the TRbwParser for formulas for data sets on the top
    // view of the model evaluated at nodes.
    property rpTopFormulaCompilerNodes: TRbwParser
      read FrpTopFormulaCompilerNodes;
    // @name returns the TRbwParser that should be used for a particular
    // @link(TDataSetOrientation) and @link(TEvaluatedAt).
    function GetCompiler(const Orientation: TDataSetOrientation;
      const EvaluatedAt: TEvaluatedAt): TRbwParser;
    function ParserCount: integer;
    property Parsers[Index: integer]: TRbwParser read GetParsers;
    procedure ClearExpressionsAndVariables;
    procedure FillCompilerList(CompilerList: TList);
    procedure RefreshGlobalVariables(CompilerList: TList);
    procedure CreateGlobalVariables;
    // @name creates a TCustomVariable to represent DataSet in each TRbwParser
    // that should have one.
    // @seealso(RemoveVariables)
    procedure CreateVariables(const DataSet: TDataArray);
    // @name removes the variables that represent DataSet in any
    // expression.
    // @seealso(CreateVariables).
    procedure RemoveVariables(const DataSet: TDataArray);

    property ThreeDGridObserver: TObserver read FThreeDGridObserver;
    property TopGridObserver: TObserver read FTopGridObserver;
    property HufKxNotifier: TObserver read FHufKxNotifier;
    property HufKyNotifier: TObserver read FHufKyNotifier;
    property HufKzNotifier: TObserver read FHufKzNotifier;
    property HufSsNotifier: TObserver read FHufSsNotifier;
    property HufSyNotifier: TObserver read FHufSyNotifier;

    // If @name is the currently active @link(TCustomModflowGridEdgeDisplay).
    property EdgeDisplay: TCustomModflowGridEdgeDisplay read FEdgeDisplay
      write SetEdgeDisplay;

    // @name provides access to the @link(TScreenObject)s in the model.
    // However, the @link(TScreenObject)s are saved are read from files
    // via @link(ObjectList).
    property ScreenObjects[const Index: integer]: TScreenObject
      read GetScreenObjects;

    // @name returns the number of @link(TScreenObject)s
    // in @link(ScreenObjects).
    property ScreenObjectCount: integer read GetScreenObjectCount;

    // @name returns true if the model uses the water table
    // to set the initial condition.
    // @name is used an event handler for
    // TDataArray.@link(TDataArray.OnDataSetUsed).
    // @name is also used in WriteHeadIC which
    // is found in the implementation section of @link(WritePhastUnit).
    function InitialWaterTableUsed(Sender: TObject): boolean;
    function ReservoirLayerUsed(Sender: TObject): boolean;
    function ReservoirPackageUsed(Sender: TObject): boolean;
    function LakePackageUsed(Sender: TObject): boolean;
    function UzfPackageUsed(Sender: TObject): boolean;
    function UzfUnsatVertKUsed(Sender: TObject): boolean;
    function UzfInitialInfiltrationUsed(Sender: TObject): boolean;

    property TransientMultiplierArrays: TList read FTransientMultiplierArrays;
    property TransientZoneArrays: TList read FTransientZoneArrays;

    function IndenticalTransientMultiplierArray(DataArray: TDataArray): TDataArray;
    function IndenticalTransientZoneArray(DataArray: TDataArray): TDataArray;

    property ModflowFullStressPeriods: TModflowStressPeriods
      read GetModflowFullStressPeriods;

    procedure UpdateModflowFullStressPeriods;
    procedure AddTimeList(TimeList: TCustomTimeList);
    procedure RemoveTimeList(TimeList: TCustomTimeList);
    // @name provides access to all the @link(TCustomTimeList)s in the model.
    property TimeLists[Index: integer]: TCustomTimeList read GetTimeLists;
    // @name is the number of @link(TCustomTimeList)s in @link(TimeLists).
    property TimeListCount: integer read GetTimeListCount;

    property MfHobHeads: THobDisplayTimeList read FMfHobHeads;

    function ProgramName: string;

    property UnitNumbers: TUnitNumbers read FUnitNumbers
      write SetUnitNumbers stored False;

    function PackageGeneratedExternally(const PackageName: string): boolean;
    procedure WritePValAndTemplate(const ParameterName: string;
      const Value: double);

    property HfbDisplayer: THfbDisplayer read FHfbDisplayer;

    function GetObserverByName(const ObserverName: string): TObserver;
    function CheckWetting: boolean;

    property ProgramLocations: TProgramLocations read GetProgramLocations
      write SetProgramLocations;

    function FixFileName(AFileName: string): string;
    // @name exports the input files for MODFLOW and optionally runs MODFLOW.
    procedure ExportModflowModel(FileName: string; RunModel: boolean);
    procedure ExportModpathModel(FileName: string;
      RunModel, NewBudgetFile: boolean);
    procedure ExportZoneBudgetModel(FileName: string; RunModel: boolean);


    // @name is the @link(TCustomTimeList) for
    // the transient data set used to color
    // the front view of the grid.
    // If the data set used to color the grid is not transient, or the grid
    // is not colored, @name has no meaning.
    // See @link(FrontDisplayTime) and
    // TCustomGrid.@link(TCustomGrid.FrontDataSet).
    property FrontTimeList: TCustomTimeList read FFrontTimeList
      write FFrontTimeList;

    // @name is the TPhastTimeList for the transient data set used to color
    // the side view of the grid.
    // If the data set used to color the grid is not transient, or the grid
    // is not colored, @name has no meaning.
    // See @link(SideDisplayTime) and
    // TCustomGrid.@link(TCustomGrid.SideDataSet).
    property SideTimeList: TCustomTimeList read FSideTimeList
      write FSideTimeList;
    // @name is the @link(TCustomTimeList)
    // for the transient data set used to color
    // the top view of the grid.
    // If the data set used to color the grid is not transient, or the grid
    // is not colored, @name has no meaning.
    // See @link(TopDisplayTime) and
    // TCustomGrid.@link(TCustomGrid.TopDataSet).
    property TopTimeList: TCustomTimeList read FTopTimeList write FTopTimeList;
    // @name is the time for the transient data set used to color the
    // top view of the grid.
    // If the data set used to color the grid is not transient, or the grid
    // is not colored, @name has no meaning.
    // See @link(TopTimeList) and
    // TCustomGrid.@link(TCustomGrid.TopDataSet).
    property TopDisplayTime: double read FTopDisplayTime;
    // @name is the time for the transient data set used to color the
    // front view of the grid.
    // If the data set used to color the grid is not transient, or the grid
    // is not colored, @name has no meaning.
    // See @link(FrontTimeList) and
    // TCustomGrid.@link(TCustomGrid.FrontDataSet).
    property FrontDisplayTime: double read FFrontDisplayTime;
    // @name is the time for the transient data set used to color the
    // side view of the grid.
    // If the data set used to color the grid is not transient, or the grid
    // is not colored, @name has no meaning.
    // See @link(SideTimeList) and
    // TCustomGrid.@link(TCustomGrid.SideDataSet).
    property SideDisplayTime: double read FSideDisplayTime;
    // @name is the time for the transient data set used to color the
    // 3D view of the grid.
    // If the data set used to color the grid is not transient, or the grid
    // is not colored, @name has no meaning.
    // See @link(ThreeDTimeList) and
    // TCustomGrid.@link(TCustomGrid.ThreeDDataSet).
    property ThreeDDisplayTime: double read FThreeDDisplayTime;
    // @name is the @link(TCustomTimeList) for
    // the transient data set used to color
    // the 3D view of the grid.
    // If the data set used to color the grid is not transient, or the grid
    // is not colored, @name has no meaning.
    // See @link(ThreeDDisplayTime) and
    // TCustomGrid.@link(TCustomGrid.ThreeDDataSet).
    property ThreeDTimeList: TCustomTimeList read FThreeDTimeList
      write FThreeDTimeList;
    property ModflowSteadyParameters: TModflowSteadyParameters
      read GetModflowSteadyParameters write SetModflowSteadyParameters;

    property ModelSelection: TModelSelection read GetModelSelection
      write SetModelSelection;

    property LayerStructure: TLayerStructure read GetLayerStructure
      write SetLayerStructure;

    property ModflowOptions: TModflowOptions read GetModflowOptions
      write SetModflowOptions;

    property ModflowStressPeriods: TModflowStressPeriods
      read GetModflowStressPeriods write SetModflowStressPeriods;

    // @name stores SoluteTransport option in PHAST.
    property SoluteTransport: boolean read GetSoluteTransport
      write SetSoluteTransport;

    // @name stores whether PHAST is using the water table
    // to set the initial condition.
    property UseWaterTable: boolean read GetUseWaterTable write SetUseWaterTable;

    // @name stores FreeSurface option in PHAST.
    property FreeSurface: boolean read GetFreeSurface write SetFreeSurface;

    // @name stores options related to chemistry in PHAST.
    property ChemistryOptions: TChemistryOptions read GetChemistryOptions
      write SetChemistryOptions;

    property HufParameters: THufModflowParameters read GetHufParameters
      write SetHufParameters;

    property ObservationPurpose: TObservationPurpose read GetObservationPurpose
      write SetObservationPurpose;

    property ModflowTransientParameters: TModflowTransientListParameters
      read GetModflowTransientParameters write SetModflowTransientParameters;

    property ModflowOutputControl: TModflowOutputControl
      read GetModflowOutputControl write SetModflowOutputControl;

    // @name stores the @link(TDataArray)s in GoPhast.
    property DataSetList: TDataSetCollection read GetDataSetCollection
      write FDataSetCollection;

  published
    property AlternateFlowPackage: boolean read FAlternateFlowPackage
      write SetAlternateFlowPackage;
    property AlternateSolver: boolean read FAlternateSolver
      write SetAlternateSolver;
    property BatchFileAdditionsAfterModel: TStrings
      read FBatchFileAdditionsAfterModel write SetBatchFileAdditionsAfterModel;
    property BatchFileAdditionsBeforeModel: TStrings
      read FBatchFileAdditionsBeforeModel
      write SetBatchFileAdditionsBeforeModel;
    property ModflowGrid: TModflowGrid read FModflowGrid write SetModflowGrid;
    property ModflowNameFileLines: TStrings read FModflowNameFileLines
      write SetModflowNameFileLines;
    property ModflowPackages: TModflowPackages read FModflowPackages
      write SetModflowPackages;
    property HeadFluxObservations: TFluxObservationGroups
      read FHeadFluxObservations write SetHeadFluxObservations;
    property DrainObservations: TFluxObservationGroups
      read FDrainObservations write SetDrainObservations;
    property GhbObservations: TFluxObservationGroups
      read FGhbObservations write SetGhbObservations;
    property RiverObservations: TFluxObservationGroups
      read FRiverObservations write SetRiverObservations;
    property HydrogeologicUnits: THydrogeologicUnits read FHydrogeologicUnits
      write SetHydrogeologicUnits;
    property FilesToArchive: TStrings read FFilesToArchive
      write SetFilesToArchive;
    property ModelInputFiles: TStrings read FModelInputFiles
      write SetModelInputFiles;
    property ModelFileName: string read FFileName write SetFileName;
    property ModflowWettingOptions: TWettingOptions read FModflowWettingOptions
      write SetModflowWettingOptions;
    property GlobalVariables: TGlobalVariables read FGlobalVariables
      write SetGlobalVariables;

  end;

  {
  @abstract(@name is used to read model configuration data to and
  from a stream and to store @link(TDataArray)s and @link(TScreenObject)s.)

  @name allows model configuration data to be read from a stream
  by making the significant features of a model
  published properties.  Because TModel is derived from TPersistent, this
  automatically provides it with methods to read and write its published
  properties.  Most of the published properties of @Link(TPhastModel)
  correspond
  directly to settings for the simulation models it supports such as
  PHAST and MODFLOW.

  @Link(TPhastModel.DataSetList) and @Link(TPhastModel.ObjectList) are
  are descendants of TCollection.  They store and read
  instances of @Link(TDataArray) and @Link(TScreenObject) respectively.
  However, when not reading data from a stream or writing it to a stream.
  @Link(TDataArray)s and @Link(TScreenObject)s are accessed via
  @link(TPhastModel.DataSets) and @link(TPhastModel.ScreenObjects) respectively.
  The number @Link(TDataArray)s or @Link(TScreenObject)s can be determined
  using @Link(TPhastModel.DataSetCount) and
  @Link(TPhastModel.ScreenObjectCount).
  }
  TPhastModel = class(TCustomModel)
  private
    FObservationPurpose: TObservationPurpose;
    FCachedScreenObjectIndex: integer;
    // See @link(Bitmaps).
    FBitmaps: TCompressedBitmapCollection;
    // See @link(ChemistryOptions).
    FChemistryOptions: TChemistryOptions;
    // See @link(Diffusivity).
    FDiffusivity: double;
    // Values of zero are never stored for real-number properties
    // in Delphi.  @name is used to get around this limitation so that
    // if a value of zero for @link(Diffusivity) is specified by
    // the user, it can be recovered correctly when reading the file.
    FDiffusivitySet: boolean;
    // See @link(FileVersion).
    FFileVersion: string;
    // See @link(FluidProperties).
    FFluidProperties: TFluidProperties;
    // See @link(FluxBoundaryChemistryGroup).
    FFluxBoundaryChemistryGroup: TTimeListGroup;
    // See @link(FluxBoundaryChemistryGroup).
    FFluxBoundaryFluxGroup: TTimeListGroup;
    // See @link(FreeSurface).
    FFreeSurface: boolean;
    // See @link(FrontBoundaryType).
    FFrontBoundaryType: TDataArray;
    // See @link(FrontFluxBoundaryChemistry).
    FFrontFluxBoundaryChemistry: TPhastTimeList;
    // See @link(FrontFluxBoundaryFlux).
    FFrontFluxBoundaryFlux: TPhastTimeList;
    // See @link(FrontLeakyAssociatedSolution).
    FFrontLeakyAssociatedSolution: TPhastTimeList;
    // See @link(FrontLeakyHead).
    FFrontLeakyHead: TPhastTimeList;
    // See @link(GridOptions).
    FGridOptions: TGridOptions;
    // See @link(LeakyAssociatedSolutionGroup).
    FLeakyAssociatedSolutionGroup: TTimeListGroup;
    // See @link(LeakyHeadGroup).
    FLeakyHeadGroup: TTimeListGroup;
    // See @link(ModelTimes).
    FModelTimes: TRealList;
    // See @link(PhastGrid).
    FPhastGrid: TPhastGrid;
    // See @link(PrintFrequency).
    FPrintFrequency: TPrintFrequencyCollection;
    // See @link(PrintInitial).
    FPrintInitial: TPrintInitial;
    // See @link(RiverAssociatedSolution).
    FRiverAssociatedSolution: TPhastTimeList;
    // See @link(RiverAssociatedSolutionGroup).
    FRiverAssociatedSolutionGroup: TTimeListGroup;
    // See @link(RiverHead).
    FRiverHead: TPhastTimeList;
    // See @link(RiverHeadGroup).
    FRiverHeadGroup: TTimeListGroup;
    // @name is used to store or read @link(TScreenObject)s.
    // However, most handling of read @link(TScreenObject)s in the program
    // is via @link(FScreenObjectList) rather than @name.
    // See @link(FScreenObjectList).
    FScreenObjectCollection: TScreenObjectCollection;
    // @name holds the @link(TScreenObject)s in the model.
    // Nearly all access to @link(TScreenObject)s is through @name.
    // However, see @link(FScreenObjectCollection).
    // @name is actually as TObjectList.
    FScreenObjectList: TList;
    // See @link(SideBoundaryType).
    FSideBoundaryType: TDataArray;
    // See @link(SideFluxBoundaryChemistry).
    FSideFluxBoundaryChemistry: TPhastTimeList;
    // See @link(SideFluxBoundaryFlux).
    FSideFluxBoundaryFlux: TPhastTimeList;
    // See @link(SideLeakyAssociatedSolution).
    FSideLeakyAssociatedSolution: TPhastTimeList;
    // See @link(SideLeakyHead).
    FSideLeakyHead: TPhastTimeList;
    // See @link(SoluteTransport).
    FSoluteTransport: boolean;
    // See @link(SolutionOptions).
    FSolutionOptions: TSolutionOptions;
    // See @link(SomeSegmentsUpToDate).
    FSomeSegmentsUpToDate: boolean;
    // See @link(SpecifiedHeadAssociatedSolution).
    FSpecifiedHeadAssociatedSolution: TPhastTimeList;
    // See @link(SpecifiedHeadGroup).
    FSpecifiedHeadGroup: TTimeListGroup;
    // See @link(SpecifiedHeadHead).
    FSpecifiedHeadHead: TPhastTimeList;
    // See @link(SpecifiedHeadSolutionGroup).
    FSpecifiedHeadSolutionGroup: TTimeListGroup;
    // See @link(SpecifiedSolution).
    FSpecifiedSolution: TPhastTimeList;
    // See @link(SteadyFlowOptions).
    FSteadyFlowOptions: TSteadyFlowOptions;
    // See @link(Times).
    FTimes: TTimeCollection;
    // See @link(Title).
    FTitle: TStrings;
    // See @link(Top2DBoundaryType).
    FTop2DBoundaryType: TDataArray;
    // See @link(TopBoundaryType).
    FTopBoundaryType: TDataArray;
    // See @link(TopFluxBoundaryChemistry).
    FTopFluxBoundaryChemistry: TPhastTimeList;
    // See @link(TopFluxBoundaryFlux).
    FTopFluxBoundaryFlux: TPhastTimeList;
    // See @link(TopLeakyAssociatedSolution).
    FTopLeakyAssociatedSolution: TPhastTimeList;
    // See @link(TopLeakyHead).
    FTopLeakyHead: TPhastTimeList;
    // See @link(Units).
    FUnits: TUnits;
    // See @link(UseWaterTable).
    FUseWaterTable: boolean;
    // See @link(WellInjectionOrPumpingRate).
    FWellInjectionOrPumpingRate: TPhastTimeList;
    // See @link(WellPumpingRateGroup).
    FWellPumpingRateGroup: TTimeListGroup;
    // See @link(WellSolution).
    FWellSolution: TPhastTimeList;
    // See @link(WellSolutionGroup).
    FWellSolutionGroup: TTimeListGroup;
    FModelSelection: TModelSelection;
    FLayerStructure: TLayerStructure;
    FOnModelSelectionChange: TNotifyEvent;
    FGuiSettings: TGuiSettings;
    FModflowOptions: TModflowOptions;
    FModflowStressPeriods: TModflowStressPeriods;
    FModflowOutputControl: TModflowOutputControl;
    FModflowSteadyParameters: TModflowSteadyParameters;
    FModflowTransientParameters: TModflowTransientListParameters;
    FOnGetZoomBox: TGetZoomBoxEvent;
    FOnScreenObjectsChanged: TNotifyEvent;
    FOnGetCurrentScreenObject: TGetCurrentScreenObjectEvent;
    FOnConvertPoint: TConvertPointEvent;
    FOnScreenObjectSelected: TNotifyEvent;
    FOnCheckScreenObject: TCheckScreenObjectEvent;
    FOn3DViewChanged: TNotifyEvent;
    FOnRefreshScreenObjects: TNotifyEvent;
    FProgramLocations: TProgramLocations;
    FModflowFullStressPeriods: TModflowStressPeriods;
    FOnScreenObjectUnSelected: TNotifyEvent;
    // See @link(SelectedScreenObjectCount).
    FSelectedScreenObjectCount: Integer;
    FScreenObjectUpdateCount: Integer;
    FArchiveName: string;
    FSortedObjectList: TLookUpList;
    FGridColors: TColorParameters;
    FContourColors: TColorParameters;
    FModelMateProjectFileName: string;
    FModelMateProject: TProject;
    FFormulaManager: TFormulaManager;
    FHufParameters: THufModflowParameters;
    FPathLine: TPathLineReader;
    FEndPoints: TEndPointReader;
    FTimeSeries: TTimeSeriesReader;
    FColorLegend: TLegend;
    FContourLegend: TLegend;
    FDisplaySettings: TDisplaySettingsCollection;
    FChildModels: TChildModelCollection;
    FImportingModel: boolean;
    // See @link(Exaggeration).
    function GetExaggeration: double;
    // See @link(FrontHeight).
    function GetFrontHeight: integer;
    // See @link(FrontX).
    function GetFrontX: double;
    // See @link(FrontY).
    function GetFrontY: double;
    // See @link(Height).
    function GetHeight: integer;
    // See @link(Left).
    function GetLeft: integer;
    // See @link(MagnificationFront).
    function GetMagnificationFront: double;
    // See @link(MagnificationSide).
    function GetMagnificationSide: double;
    // See @link(MagnificationTop).
    function GetMagnificationTop: double;
    // See @link(OwnsScreenObjects).
    function GetOwnsScreenObjects: boolean;
    // See @link(ObjectList).
    function GetScreenObjectCollection: TScreenObjectCollection;
    // See @link(SideWidth).
    function GetSideWidth: integer;
    // See @link(SideX).
    function GetSideX: double;
    // See @link(SideY).
    function GetSideY: double;
    // See @link(Top).
    function GetTop: integer;
    // See @link(TopViewHeight).
    function GetTopViewHeight: integer;
    // See @link(TopViewWidth).
    function GetTopViewWidth: integer;
    // See @link(TopX).
    function GetTopX: double;
    // See @link(TopY).
    function GetTopY: double;
    // See @link(Version).
    function GetVersion: string;
    // See @link(Width).
    function GetWidth: integer;
    // See @link(WindowState).
    function GetWindowState: TWindowState;
    // @name initializes all the @link(TPhastTimeList)s in @link(TimeLists).
    procedure InitializePhastBoundaries;
    // @name adds all the TTimeItem.@link(TTimeItem.EndingTime)s
    // in @link(Times) to @link(ModelTimes).
    // @name is only used in PHAST models;
    procedure RecordTimeControl;
    // See @link(Bitmaps).
    procedure SetBitmaps(const Value: TCompressedBitmapCollection);
    // See @link(Diffusivity).
    procedure SetDiffusivity(const Value: double);
    // See @link(Exaggeration).
    procedure SetExaggeration(Value: double);
    // See @link(FlowOnly).
    procedure SetFlowOnly(const Value: boolean);
    // See @link(FrontHeight).
    procedure SetFrontHeight(Value : integer);
    // See @link(FrontX).
    procedure SetFrontX(const Value: double);
    // See @link(FrontY).
    procedure SetFrontY(const Value: double);
    // See @link(Height).
    procedure SetHeight(const Value: integer);
    // See @link(Left).
    procedure SetLeft(const Value: integer);
    // See @link(MagnificationFront).
    procedure SetMagnificationFront(Value: double);
    // See @link(MagnificationSide).
    procedure SetMagnificationSide(Value: double);
    // See @link(MagnificationTop).
    procedure SetMagnificationTop(Value: double);
    // See @link(OwnsScreenObjects).
    procedure SetOwnsScreenObjects(const Value: boolean);
    // See @link(PhastGrid).
    procedure SetPhastGrid(const Value: TPhastGrid);
    // See @link(PrintFrequency).
    procedure SetPrintFrequency(const Value: TPrintFrequencyCollection);
    // See @link(ObjectList).
    procedure SetScreenObjectCollection(
      const Value: TScreenObjectCollection);
    // See @link(MagnificationTop).
    procedure SetSideWidth(const Value: integer);
    // See @link(SideX).
    procedure SetSideX(const Value: double);
    // See @link(SideY).
    procedure SetSideY(const Value: double);
    // See @link(Times).
    procedure SetTimes(const Value: TTimeCollection);
    // See @link(Title).
    procedure SetTitle(const Value: TStrings);
    // See @link(Top).
    procedure SetTop(const Value: integer);
    // See @link(TopViewHeight).
    procedure SetTopViewHeight(const Value: integer);
    // See @link(TopViewWidth).
    procedure SetTopViewWidth(const Value: integer);
    // See @link(TopX).
    procedure SetTopX(const Value: double);
    // See @link(TopY).
    procedure SetTopY(const Value: double);
    // See @link(Version).
    procedure SetVersion(const Value: string);
    // See @link(Width).
    procedure SetWidth(const Value: integer);
    // See @link(WindowState).
    procedure SetWindowState(const Value: TWindowState);
    // See @link(Units);
    procedure SetUnits(const Value: TUnits);
    procedure UpdateDrainReturnObjects;
    procedure CreatePhastTimeLists;
    procedure CreatePhastTimeListGroups;
    procedure UpdateUseList(DataIndex: integer; NewUseList: TStringList;
      Item: TCustomModflowBoundaryItem);
    //    procedure DefinePackageDataArrays;
    procedure UpdateDischargeRouting(Sender: TObject);
    function DefaultArchiveName: string;
    function GetArchiveName: string;
    procedure SetArchiveName(const Value: string);
    procedure GetUnitID(var UnitID: Integer);
    procedure UpdateModPathZone(Sender: TObject);
    procedure NotifyGridColorsChanged(Sender: TObject);
    procedure SetModelMateProjectFileName(const Value: string);
    procedure SetModelMateProject(const Value: TProject);
    procedure UpdateModelMateParameter(ParameterList: TStringList;
      ModelMuseParam: TModflowParameter; Project: TProject;
      Operation: TModelMateOperation);
    procedure UpdateModelMateFluxObservation(ObservationList: TStringList;
      ModelMuseFluxObsGroup: TFluxObservationGroup; Project: TProject;
      Operation: TModelMateOperation);
    procedure UpdateModelMateHeadObservation(ObservationList: TStringList;
      const OBSNAM: string; ModelMuseHeadObs: THobItem; Project: TProject;
      Operation: TModelMateOperation; Method: TMultiObsMethod);
    procedure HandleModelMateParameters(Operation: TModelMateOperation;
      ParameterList: TStringList; Project: TProject);
    procedure HandleModelMateObservations(Operation: TModelMateOperation;
      ObservationList: TStringList; Project: TProject);
    procedure EnsureModelMateObsGroup(Project: TProject; GroupName: string;
      PlotSymbol: integer);
    function PhastUsed(Sender: TObject): boolean;
    procedure SetPathLine(const Value: TPathLineReader);
    function GetPathLine: TPathLineReader;
    function StorePathLine: Boolean;
    function GetEndPoints: TEndPointReader;
    procedure SetEndPoints(const Value: TEndPointReader);
    function StoreEndPoints: Boolean;
    procedure SetTimeSeries(const Value: TTimeSeriesReader);
    function StoreTimeSeries: Boolean;
    function GetTimeSeries: TTimeSeriesReader;
    procedure CreateInitialDataSetsForPhastTimeLists;
    procedure SetDisplaySettings(const Value: TDisplaySettingsCollection);
    procedure SetChildModels(const Value: TChildModelCollection);
    function StoreChildModels: Boolean;
  protected
    function GetLayerStructure: TLayerStructure;override;
    procedure SetLayerStructure(const Value: TLayerStructure);override;
    function GetModflowOptions: TModflowOptions;override;
    function GetModflowStressPeriods: TModflowStressPeriods;override;
    function GetSoluteTransport: boolean;override;
    function GetFreeSurface: boolean;override;
    function GetUseWaterTable: boolean;override;
    function GetChemistryOptions: TChemistryOptions;override;
    procedure SetChemistryOptions(const Value: TChemistryOptions);override;
    function GetHufParameters: THufModflowParameters;override;
    function GetModflowFullStressPeriods: TModflowStressPeriods; override;
    function GetModflowOutputControl: TModflowOutputControl;override;
    function GetProgramLocations: TProgramLocations;override;
    function GetObservationPurpose: TObservationPurpose; override;
    procedure SetObservationPurpose(const Value: TObservationPurpose); override;
    procedure SetHufParameters(const Value: THufModflowParameters);override;
    procedure SetModflowOptions(const Value: TModflowOptions);override;
    procedure SetModflowStressPeriods(const Value: TModflowStressPeriods);override;
    procedure SetModflowOutputControl(const Value: TModflowOutputControl);override;
    procedure SetModflowTransientParameters(
      const Value: TModflowTransientListParameters);override;
    procedure SetProgramLocations(const Value: TProgramLocations);override;
    // See @link(UseWaterTable).
    procedure SetUseWaterTable(const Value: boolean);override;
    // See @link(SoluteTransport).
    procedure SetSoluteTransport(const Value: boolean);override;
    // See @link(FreeSurface).
    procedure SetFreeSurface(const Value: boolean);override;
    function GetModflowTransientParameters: TModflowTransientListParameters; override;
    function GetModelSelection: TModelSelection;override;
    function GetModflowSteadyParameters: TModflowSteadyParameters;override;
    procedure SetModflowSteadyParameters(const Value: TModflowSteadyParameters);override;
    // Among other things, if @link(OnModelSelectionChange)
    // is assigned, @name calls @link(TfrmGoPhast.ModelSelectionChange
    // TfrmGoPhast.ModelSelectionChange).  @name also changes
    // the functions that are available. and sets @Link(TObserver)s
    // for the @link(Grid).
    procedure SetModelSelection(const Value: TModelSelection);override;
    // See @link(ScreenObjectCount).
    function GetScreenObjectCount: integer;override;
    // See @link(ScreenObjects).
    function GetScreenObjects(const Index: integer): TScreenObject;override;
    // @name causes the grid to not be colored by any @link(TDataArray).
    procedure ClearViewedItems; override;
    procedure SetUpToDate(const Value: boolean); override;
    // @name is used to fix up the model after @name is loaded from a file.
    procedure Loaded; override;
    // @name restores the model to its initial state. It gets rid of
    // all @link(TDataArray)s and @link(TScreenObject)s. It initializes
    // @link(Diffusivity) and @link(SolutionOptions).
    procedure InternalClear; override;
  public
    property ImportingModel: boolean read FImportingModel write FImportingModel;
    procedure Assign(Source: TPersistent); override;
    // @name updates and invalidates data sets that may have been calculated
    // incorrectly in previous versions of ModelMuse.
    procedure FixOldModel;
    // When a @link(TDataArray) or global variable is renamed, @name is
    // called to update all the formulas with the new names.
    procedure UpdateFormulas(OldNames, NewNames: TStringList);
    // MODFLOW can not use a file name containing a space character.
    // @name replaces the space character in a file name with an underscore.
    // @name is the event handler for @link(TDataArray.OnDataSetUsed
    // TDataArray.OnDataSetUsed) for @link(TDataArray)s related to the HUF
    // package.
    function HufDataArrayUsed(Sender: TObject): boolean;
    // @name is used when determining what data sets or global variables are
    // used when evaluating the formula for a MODFLOW boundary condition.
    // The names of all the @link(TDataArray)s and global variables are added
    // to NewUseList.
    procedure UpdateDisplayUseList(NewUseList: TStringList;
      ParamType: TParameterType; DataIndex: integer; const DisplayName: string);
    // @name invalidates all the @link(TModflowBoundaryDisplayTimeList)s.
    procedure InvalidateModflowBoundaries;
    // @name is the event handler for @link(TDataArray.OnDataSetUsed
    // TDataArray.OnDataSetUsed) for @link(TDataArray)s that have model results.
    function ModelResultsRequired(Sender: TObject): boolean;
    // @name is the event handler for @link(TDataArray.OnDataSetUsed
    // TDataArray.OnDataSetUsed) in MODFLOW models for @link(TDataArray)s
    // that define the top of the model and the bottom of @link(TLayerGroup)s.
    function ModelLayerDataArrayUsed(Sender: TObject): boolean;
    // @name is the event handler for @link(TDataArray.OnDataSetUsed
    // TDataArray.OnDataSetUsed) in MODFLOW models for @link(TDataArray)s
    // related to the SUB and SWT packages.
    function SubsidenceDataArrayUsed(Sender: TObject): boolean;
    // @name fills LayerGroupsDataSets with the @link(TDataArray)s used by
    // @link(LayerStructure).
    procedure GetLayerGroupDataSets(LayerGroupsDataSets: TList);
    // @name finds the @link(TScreenObject) that is closest to the last
    // point in TestScreenObject.
    procedure LocateNearestLakeOrStream(TestScreenObject: TScreenObject;
      var NearestLake, NearestStream: TScreenObject; Tolerance: double = 0);
    // @name sets the event handlers for the discharge routing array in
    // the UZF package.
    procedure DischargeRoutingUpdate;
    // @name increments @link(FScreenObjectUpdateCount).  While
    // @link(FScreenObjectUpdateCount) is greater than zero the
    // @link(OnScreenObjectsChanged) event is not called.
    procedure BeginScreenObjectUpdate;
    // @name decrements @link(FScreenObjectUpdateCount) and calls
    // @link(ScreenObjectsChanged).  While
    // @link(FScreenObjectUpdateCount) is greater than zero the
    // @link(OnScreenObjectsChanged) event is not called.
    procedure EndScreenObjectUpdate;
    // @name is the number of @link(TScreenObject)s that are selected.
    property SelectedScreenObjectCount: Integer read FSelectedScreenObjectCount;
    // @name is called by @link(TScreenObject)s when they become unselected.
    // @name decrements @link(FSelectedScreenObjectCount) and calls
    // @link(OnScreenObjectUnSelected).
    procedure ScreenObjectUnSelected;
    // See @link(TfrmGoPhast.ScreenObjectSelectionChange).
    property OnScreenObjectSelected: TNotifyEvent read FOnScreenObjectSelected
      write FOnScreenObjectSelected;
    // @name is called by @link(ScreenObjectUnSelected).
    // Then event handler for this is
    // @link(TfrmGoPhast.ScreenObjectSelectionChange)
    property OnScreenObjectUnSelected: TNotifyEvent
      read FOnScreenObjectUnSelected write FOnScreenObjectUnSelected;
    // @name increments @link(FSelectedScreenObjectCount) and then,
    // if assigned, @name calls @link(ScreenObjectSelected).
    procedure ScreenObjectSelected;
    // See @link(TfrmGoPhast.CheckScreenObject).
    property OnCheckScreenObject: TCheckScreenObjectEvent
      read FOnCheckScreenObject write FOnCheckScreenObject;
    // If assigned, @name calls @link(OnCheckScreenObject).
    function IsCurrentScreenObject(ScreenObject: TScreenObject): boolean;
    // @name updates @link(TDataArray.ParameterUsed TDataArray.ParameterUsed)
    // and @link(TDataArray.ParameterFormula TDataArray.ParameterFormula)
    // for the @link(TDataArray)s related to the LPF parameters.
    procedure UpdateDataArrayParameterUsed;
    // @name updates @link(TDataArray.OnPostInitialize
    // TDataArray.OnPostInitialize) for several @link(TDataArray)s.
    procedure UpdateOnPostInitialize;
    // @name adds AScreenObject to @link(FScreenObjectList).
    function AddScreenObject(const AScreenObject: TScreenObject): integer;
      virtual;
    // @name removes all @link(TScreenObject)s in @link(FScreenObjectList).
    // This will destroy them unless @link(OwnsScreenObjects) is
    // set to False first.
    procedure ClearScreenObjects;
    // @name are the parameters used in setting the colors of the grid cells.
    property GridColorParameters: TColorParameters read FGridColors;
    // @name are the parameters used in setting the colors of contours.
    property ContourColorParameters: TColorParameters read FContourColors;
    // @name creates an instance of @classname.
    constructor Create(AnOwner: TComponent); override;
    // @name returns the name of the most likely output file from which
    // model results will be imported.  If heads were saved, the name of the
    // file containing heads will be returned.
    // If heads were not saved by drawdowns were saved, the name of the
    // file containing drawdowns will be returned.
    // After that, it is the file containing the cell-by-cell flows.
    function DefaultModflowOutputFileName: string;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name draws the 3D views of the @link(TScreenObject)s.
    procedure DrawScreenObjects3D;
    // @name removes AScreenObject from @link(ScreenObjects) without freeing it.
    procedure ExtractScreenObject(const AScreenObject: TScreenObject);
    // @name indicates what version of GoPhast last saved the file that is
    // currently open.
    property FileVersion: string read FFileVersion;
    // @name is the group of @link(TPhastTimeList)s that are related
    // to the chemistry in specified flux boundaries.
    property FluxBoundaryChemistryGroup: TTimeListGroup read
      FFluxBoundaryChemistryGroup;
    // @name is the group of @link(TPhastTimeList)s that are related
    // to the flux in specified flux boundaries.
    property FluxBoundaryFluxGroup: TTimeListGroup read FFluxBoundaryFluxGroup;
    // @name indicates what type of boundary condition (if any)
    // is present on the front face of each grid cell.
    property FrontBoundaryType: TDataArray read FFrontBoundaryType;
    // @name is the @link(TPhastTimeList) that specifies the chemistry
    // for specified flux boundaries on the front view of model cells.
    property FrontFluxBoundaryChemistry: TPhastTimeList
      read FFrontFluxBoundaryChemistry;
    // @name is the @link(TPhastTimeList) that specifies the flux
    // for specified flux boundaries on the front view of model cells.
    property FrontFluxBoundaryFlux: TPhastTimeList read FFrontFluxBoundaryFlux;
    // @name is the @link(TPhastTimeList) that specifies the associated solution
    // for leaky boundaries on the front view of model cells.
    property FrontLeakyAssociatedSolution: TPhastTimeList
      read FFrontLeakyAssociatedSolution;
    // @name is the @link(TPhastTimeList) that specifies the head
    // for leaky boundaries on the front view of model cells.
    property FrontLeakyHead: TPhastTimeList read FFrontLeakyHead;
    // @name increases the number of @link(TScreenObject)s that can be
    // held in @link(ScreenObjects) by the amount Delta.
    // If Delta is less than or equal to zero, @name does nothing.
    procedure IncreaseScreenObjectCapacity(const Delta: integer);
    // @name returns the position of AScreenObject
    // in @link(ScreenObjects).  If AScreenObject is not in
    /// @link(ScreenObjects), @name returns -1.
    function IndexOfScreenObject(const AScreenObject: TScreenObject): integer;
    // @name sets all the times in the model in @link(ModelTimes).
    // @name calls @link(InitializePhastBoundaries) and @link(RecordTimeControl)
    // to do this.
    procedure InitializeTimes;
    // @name inserts AScreenObject into @link(ScreenObjects) at the position
    // specified by Index.
    procedure InsertScreenObject(const Index: integer;
      const AScreenObject: TScreenObject);
    // @name calls TScreenObject.@link(TScreenObject.Invalidate)
    // for every @link(TScreenObject) in @link(ScreenObjects).
    procedure InvalidateScreenObjects;
    // Calling @name ensures that TScreenObject.Segments.@link(
    // TCellElementSegmentList.UpToDate) is @False for every
    // @link(TScreenObject).
    // @name only does something if @link(SomeSegmentsUpToDate) is @true.
    procedure InvalidateSegments;
    // @name is the group of @link(TPhastTimeList)s that are related
    // to the associated solution in leaky boundaries.
    property LeakyAssociatedSolutionGroup: TTimeListGroup
      read FLeakyAssociatedSolutionGroup;
    // @name is the group of @link(TPhastTimeList)s that are related
    // to the head in leaky boundaries.
    property LeakyHeadGroup: TTimeListGroup read FLeakyHeadGroup;
    // @name is used to store all the times used in the model.
    // Its contents are set in @link(InitializeTimes).
    property ModelTimes: TRealList read FModelTimes;
    // @name checks all @link(TScreenObject TScreenObject) starting with Root
    // followed by a number and returns the largest number so detected.
    function NumberOfLargestScreenObjectsStartingWith(
      const Root: string): integer;
    // @name is used to get or set @link(FScreenObjectList).OwnsObjects
    property OwnsScreenObjects: boolean read GetOwnsScreenObjects
      write SetOwnsScreenObjects;
    // @name removes AScreenObject from @link(ScreenObjects).
    // AScreenObject is freed unless @link(OwnsScreenObjects) is set to
    // @false first.  At the time this was written @name always freed
    // AScreenObject.
    procedure RemoveScreenObject(const AScreenObject: TScreenObject);
    // @name restores coloring the grid by time-varying data sets.
    // The data sets must already be up to date.
    procedure RestoreColoredDataSets;
    // @name is the @link(TPhastTimeList) that specifies the associated solution
    // for river boundaries.
    property RiverAssociatedSolution: TPhastTimeList
      read FRiverAssociatedSolution;
    // @name is the group of @link(TPhastTimeList)s that are related
    // to the associated solution in river boundaries.
    property RiverAssociatedSolutionGroup: TTimeListGroup
      read FRiverAssociatedSolutionGroup;

    // @name is the @link(TPhastTimeList) that specifies the head
    // for river boundaries.
    property RiverHead: TPhastTimeList read FRiverHead;
    // @name is the group of @link(TPhastTimeList)s that are related
    // to the head in river boundaries.
    property RiverHeadGroup: TTimeListGroup read FRiverHeadGroup;
    // @name returns the class of @link(TScreenObject) that is normally
    // used with @classname.  @name returns @link(TScreenObject).
    function ScreenObjectClass: TScreenObjectClass;
    // @name indicates what type of boundary condition (if any)
    // is present on the side face of each grid cell.
    property SideBoundaryType: TDataArray read FSideBoundaryType;
    // @name is the @link(TPhastTimeList) that specifies the chemistry
    // for specified flux boundaries on the side view of model cells.
    property SideFluxBoundaryChemistry: TPhastTimeList
      read FSideFluxBoundaryChemistry;
    // @name is the @link(TPhastTimeList) that specifies the flux
    // for specified flux boundaries on the side view of model cells.
    property SideFluxBoundaryFlux: TPhastTimeList read FSideFluxBoundaryFlux;
    // @name is the @link(TPhastTimeList)
    // that specifies the associated solution
    // for leaky boundaries on the front view of model cells.
    property SideLeakyAssociatedSolution: TPhastTimeList
      read FSideLeakyAssociatedSolution;
    // @name is the @link(TPhastTimeList) that specifies the head
    // for leaky boundaries on the side view of model cells.
    property SideLeakyHead: TPhastTimeList read FSideLeakyHead;

    // If @name is @true, there is at least one @link(TScreenObject)
    // in which TScreenObject.Segments.@link(
    // TCellElementSegmentList.UpToDate) is @True
    // @name is used in @link(InvalidateSegments)
    property SomeSegmentsUpToDate: boolean read FSomeSegmentsUpToDate
      write FSomeSegmentsUpToDate;
    // @name is the @link(TPhastTimeList) that specifies the associated solution
    // for specified head boundaries.
    property SpecifiedHeadAssociatedSolution: TPhastTimeList
      read FSpecifiedHeadAssociatedSolution;
    // @name is the group of @link(TPhastTimeList)s that are related
    // to the head in specified head boundaries.
    property SpecifiedHeadGroup: TTimeListGroup read FSpecifiedHeadGroup;
    // @name is the @link(TPhastTimeList) that specifies the specified head
    // for specified head boundaries.
    property SpecifiedHeadHead: TPhastTimeList read FSpecifiedHeadHead;
    // @name is the group of @link(TPhastTimeList)s that are related
    // to the solution in specified head boundaries.
    property SpecifiedHeadSolutionGroup: TTimeListGroup
      read FSpecifiedHeadSolutionGroup;
    // @name is the @link(TPhastTimeList) that specifies the specified solution
    // for specified head boundaries.
    property SpecifiedSolution: TPhastTimeList read FSpecifiedSolution;
    function GetTimeListByName(const AName: string): TCustomTimeList;
    // @name indicates what type of 2D boundary condition (if any)
    // is present on the top face of each grid cell.
    property Top2DBoundaryType: TDataArray read FTop2DBoundaryType;
    // @name indicates what type of boundary condition (if any)
    // is present on the top face of each grid cell.
    property TopBoundaryType: TDataArray read FTopBoundaryType;
    // @name is the @link(TPhastTimeList) that specifies the chemistry
    // for specified flux boundaries on the top view of model cells.
    property TopFluxBoundaryChemistry: TPhastTimeList
      read FTopFluxBoundaryChemistry;
    // @name is the @link(TPhastTimeList) that specifies the flux
    // for specified flux boundaries on the top view of model cells.
    property TopFluxBoundaryFlux: TPhastTimeList read FTopFluxBoundaryFlux;
    // @name is the @link(TPhastTimeList)
    // that specifies the associated solution
    // for leaky boundaries on the top view of model cells.
    property TopLeakyAssociatedSolution: TPhastTimeList
      read FTopLeakyAssociatedSolution;
    // @name is the @link(TPhastTimeList) that specifies the head
    // for leaky boundaries on the top view of model cells.
    property TopLeakyHead: TPhastTimeList read FTopLeakyHead;
    // @name is called after reading a @classname to transfer the information
    // about @link(TDataArray)s from @link(DataSetList) to @link(DataSets).
    procedure UpdateDataSets;
    // @name assigns frmGoPhast.Grid.@link(TCustomGrid.FrontDataSet)
    // to be the @link(TDataArray) in TimeList at Time.
    procedure UpdateFrontTimeDataSet(const TimeList: TCustomTimeList;
      const Time: double);
    // @name is called after reading a @classname to transfer the information
    // about @link(TScreenObject)s from @link(ObjectList) to
    // @link(ScreenObjects).
    procedure UpdateScreenObjects;
    // @name assigns frmGoPhast.Grid.@link(TCustomGrid.SideDataSet)
    // to be the @link(TDataArray) in TimeList at Time.
    procedure UpdateSideTimeDataSet(const TimeList: TCustomTimeList;
      const Time: double);
    // @name assigns frmGoPhast.Grid.@link(TCustomGrid.ThreeDDataSet)
    // to be the @link(TDataArray) in TimeList at Time.
    procedure UpdateThreeDTimeDataSet(const TimeList: TCustomTimeList;
      const Time: double);
    // @name calls TPhastTimeList.@link(TPhastTimeList.Loaded)
    // for each @link(TPhastTimeList) in @link(TimeLists).
    procedure UpdateTimeLists;
    // @name assigns frmGoPhast.Grid.@link(TCustomGrid.TopDataSet)
    // to be the @link(TDataArray) in TimeList at Time.
    procedure UpdateTopTimeDataSet(const TimeList: TCustomTimeList;
      const Time: double);

    // @name is the @link(TPhastTimeList) that specifies
    // the injection or pumping rate for well boundaries.
    // Positive means flow out of the porous medium.
    property WellInjectionOrPumpingRate: TPhastTimeList
      read FWellInjectionOrPumpingRate;
    // @name is the group of @link(TPhastTimeList)s that are related
    // to the injection or pumping rate in well boundaries.
    property WellPumpingRateGroup: TTimeListGroup read FWellPumpingRateGroup;
    // @name is the @link(TPhastTimeList) that specifies
    // the solution for well boundaries.
    // Positive means flow out of the porous medium.
    property WellSolution: TPhastTimeList read FWellSolution;
    // @name is the group of @link(TPhastTimeList)s that are related
    // to the solution in well boundaries.
    property WellSolutionGroup: TTimeListGroup read FWellSolutionGroup;

    // name is called in @link(SetModelSelection).
    // The event handler for this event is
    // @link(TfrmGoPhast.ModelSelectionChange).
    property OnModelSelectionChange: TNotifyEvent read FOnModelSelectionChange
      write FOnModelSelectionChange;

    // See @link(TfrmGoPhast.GetZoomBox).
    property OnGetZoomBox: TGetZoomBoxEvent read FOnGetZoomBox
      write FOnGetZoomBox;
    // If assigned, @name calls @link(OnGetZoomBox). Otherwise, it returns nil.
    function ZoomBox(VD: TViewDirection): TQrbwZoomBox2;
    // See @link(TfrmGoPhast.ScreenObjectsChanged
    // TfrmGoPhast.ScreenObjectsChanged).
    property OnScreenObjectsChanged: TNotifyEvent read FOnScreenObjectsChanged
      write FOnScreenObjectsChanged;
    // The event handler for this event is @link(TfrmGoPhast.UpdateDisplay
    // TfrmGoPhast.UpdateDisplay).
    // @name is called in @link(RefreshScreenObjects).
    property OnRefreshScreenObjects: TNotifyEvent read FOnRefreshScreenObjects
      write FOnRefreshScreenObjects;
    // If assigned, @name calls @link(OnScreenObjectsChanged).
    // Typically, @link(OnScreenObjectsChanged) is assigned to
    // @link(TfrmGoPhast.ScreenObjectsChanged TfrmGoPhast.ScreenObjectsChanged).
    procedure ScreenObjectsChanged(Sender: TObject);
    // @name calls @link(OnRefreshScreenObjects) if assigned.
    // @name is called by @link(TScreenObject.RefreshGui
    // TScreenObject.RefreshGui).
    procedure RefreshScreenObjects(Sender: TObject);
    // see @link(TfrmGoPhast.GetCurrentScreenObject).
    property OnGetCurrentScreenObject :TGetCurrentScreenObjectEvent
      read FOnGetCurrentScreenObject write FOnGetCurrentScreenObject;
    // If assigned, @name calls @link(OnGetCurrentScreenObject).
    function GetCurrentScreenObject(VD: TViewDirection): TScreenObject;
    // See @link(TfrmGoPhast.Invalidate3DView).
    property On3DViewChanged: TNotifyEvent read FOn3DViewChanged
      write FOn3DViewChanged;
    // If assigned, @name calls @link(On3DViewChanged).
    procedure Notify3DViewChanged;
    // @name sets the @link(TScreenObject.Selected) property of
    // all @link(TScreenObject)s to false.  @name returns true if any
    // of them were selected.
    function ResetSelectedScreenObjects: boolean;
    // see @link(TfrmGoPhast.ConvertPoint).
    property OnConvertPoint: TConvertPointEvent read FOnConvertPoint
      write FOnConvertPoint;
    // @name calls @link(OnConvertPoint).
    function ConvertPoint(VD: TViewDirection;
      const RealPoint: TPoint2D): TPoint;
    procedure UpdateActive(Sender: TObject);
    procedure UpdateWetDry(Sender: TObject);
    procedure UpdateLakeId(Sender: TObject);
    procedure FinalizeActive(Sender: TObject);
    procedure FinalizeWetDry(Sender: TObject);
    procedure FinalizeLakeId(Sender: TObject);
    procedure InvalidateMfWellPumpage(Sender: TObject);
    procedure InvalidateMfGhbConductance(Sender: TObject);
    procedure InvalidateMfGhbBoundaryHead(Sender: TObject);
    procedure InvalidateMfDrnConductance(Sender: TObject);
    procedure InvalidateMfDrnElevation(Sender: TObject);
    procedure InvalidateMfDrtConductance(Sender: TObject);
    procedure InvalidateMfDrtElevation(Sender: TObject);
    procedure InvalidateMfDrtReturnFraction(Sender: TObject);
    procedure InvalidateMfRivConductance(Sender: TObject);
    procedure InvalidateMfRivStage(Sender: TObject);
    procedure InvalidateMfRivBottom(Sender: TObject);
    procedure InvalidateMfChdStartingHead(Sender: TObject);
    procedure InvalidateMfChdEndingHead(Sender: TObject);
    procedure InvalidateMfEtsEvapRate(Sender: TObject);
    procedure InvalidateMfEtsEvapSurface(Sender: TObject);
    procedure InvalidateMfEtsEvapDepth(Sender: TObject);
    procedure InvalidateMfEtsEvapLayer(Sender: TObject);
    procedure InvalidateEtsDepthFractions(Sender: TObject);
    procedure InvalidateEtsRateFractions(Sender: TObject);
    procedure InvalidateMfEvtEvapRate(Sender: TObject);
    procedure InvalidateMfEvtEvapSurface(Sender: TObject);
    procedure InvalidateMfEvtEvapDepth(Sender: TObject);
    procedure InvalidateMfEvtEvapLayer(Sender: TObject);
    procedure InvalidateMfRchRate(Sender: TObject);
    procedure InvalidateMfRchLayer(Sender: TObject);
    procedure InvalidateMfSfrData(Sender: TObject);
    procedure InvalidateMfUzfInfiltration(Sender: TObject);
    procedure InvalidateMfUzfEtDemand(Sender: TObject);
    procedure InvalidateMfUzfExtinctionDepth(Sender: TObject);
    procedure InvalidateMfUzfWaterContent(Sender: TObject);
    procedure InvalidateMfSfrSegmentReachAndIcalc(Sender: TObject);
    procedure InvalidateMfSfrReachLength(Sender: TObject);
    procedure InvalidateMfSfrStreamTop(Sender: TObject);
    procedure InvalidateMfSfrStreamSlope(Sender: TObject);
    procedure InvalidateMfSfrStreamThickness(Sender: TObject);
    procedure InvalidateMfSfrStreamK(Sender: TObject);
    procedure InvalidateMfSfrSaturatedWaterContent(Sender: TObject);
    procedure InvalidateMfSfrInitialWaterContent(Sender: TObject);
    procedure InvalidateMfSfrBrooksCorey(Sender: TObject);
    procedure InvalidateMfSfrVerticalUnsatK(Sender: TObject);
    procedure InvalidateMfSfrIprior(Sender: TObject);
    procedure InvalidateMfSfrFlow(Sender: TObject);
    procedure InvalidateMfSfrRunoff(Sender: TObject);
    procedure InvalidateMfSfrPrecipitation(Sender: TObject);
    procedure InvalidateMfSfrEvapotranspiration(Sender: TObject);
    procedure InvalidateMfSfrChannelRoughness(Sender: TObject);
    procedure InvalidateMfSfrBankRoughness(Sender: TObject);
    procedure InvalidateMfSfrDepthCoefficient(Sender: TObject);
    procedure InvalidateMfSfrDepthExponent(Sender: TObject);
    procedure InvalidateMfSfrWidthCoefficient(Sender: TObject);
    procedure InvalidateMfSfrWidthExponent(Sender: TObject);
    procedure InvalidateMfSfrUpstreamHydraulicConductivity(Sender: TObject);
    procedure InvalidateMfSfrDownstreamHydraulicConductivity(Sender: TObject);
    procedure InvalidateMfSfrUpstreamWidth(Sender: TObject);
    procedure InvalidateMfSfrDownstreamWidth(Sender: TObject);
    procedure InvalidateMfSfrUpstreamThickness(Sender: TObject);
    procedure InvalidateMfSfrDownstreamThickness(Sender: TObject);
    procedure InvalidateMfSfrUpstreamElevation(Sender: TObject);
    procedure InvalidateMfSfrDownstreamElevation(Sender: TObject);
    procedure InvalidateMfSfrUpstreamDepth(Sender: TObject);
    procedure InvalidateMfSfrDownstreamDepth(Sender: TObject);
    procedure InvalidateMfSfrUpstreamUnsaturatedWaterContent(Sender: TObject);
    procedure InvalidateMfSfrDownstreamUnsaturatedWaterContent(Sender: TObject);
    procedure InvalidateMfSfrUpstreamUnsatInitialWaterContent(Sender: TObject);
    procedure InvalidateMfSfrDownstreamUnsatInitialWaterContent(
      Sender: TObject);
    procedure InvalidateMfSfrUpstreamBrooksCorey(Sender: TObject);
    procedure InvalidateMfSfrDownstreamBrooksCorey(Sender: TObject);
    procedure InvalidateMfSfrUpstreamUnsatKz(Sender: TObject);
    procedure InvalidateMfSfrDownstreamUnsatKz(Sender: TObject);

    property ArchiveName: string read GetArchiveName write SetArchiveName;
    procedure CreateArchive(const FileName: string);
    procedure InvalidateMfHobHeads(Sender: TObject);
    function DefaultHigherElevationFormula(ViewDirection: TViewDirection): string;
    function DefaultLowerElevationFormula(ViewDirection: TViewDirection): string;
    function DefaultElevationFormula(ViewDirection: TViewDirection; EvalAt: TEvaluatedAt): string;
    function ParameterDataSetUsed(Sender: TObject): boolean;
    function GetScreenObjectByName(AName: string): TScreenObject;
    procedure CopyScreenObjectsToClipboard;
    procedure PasteObjectsFromClipboard(List: TList);
    property ModelMateProject: TProject read FModelMateProject
      write SetModelMateProject;
    procedure UpdateModelMateProject;
    procedure ImportFromModelMateProject(Project: TProject);
    procedure RegisterGlobalVariables(Parser: TRbwParser);

    property FormulaManager: TFormulaManager read FFormulaManager;
    procedure ClearScreenObjectCollection;

    property ColorLegend: TLegend read FColorLegend;
    property ContourLegend: TLegend read FContourLegend;

    function FileVersionEqualOrEarlier(TestVersion: string): boolean;

  published
    // The following properties are obsolete.

    // @name stores FlowOnly option in PHAST.
    property FlowOnly: boolean write SetFlowOnly stored False;
    { @name is used to store fluid properties in PHAST.}
    { @name is only for backwards compatibility.  It is not used.}
    property FluidProperties: TFluidProperties read FFluidProperties
      write FFluidProperties stored False;
    // @name stores the height in pixels of the front view of the model.
    property FrontHeight: integer read GetFrontHeight
      write SetFrontHeight stored False;
    // @name stores the reference X-coordinate for the front view of the model.
    property FrontX: double read GetFrontX write SetFrontX stored False;
    // @name stores the reference Y-coordinate for the front view of the model.
    property FrontY: double read GetFrontY write SetFrontY stored False;
    // @name is the height of the main form in pixels.
    property Height: integer read GetHeight write SetHeight stored False;
    // @name is the X-coordinate of the main form in pixels.
    property Left: integer read GetLeft write SetLeft stored False;
    // @name is the magnification of the front view of the model.
    property MagnificationFront: double read GetMagnificationFront
      write SetMagnificationFront stored False;
    // @name is the magnification of the side view of the model.
    property MagnificationSide: double read GetMagnificationSide
      write SetMagnificationSide stored False;
    // @name is the magnification of the top view of the model.
    property MagnificationTop: double read GetMagnificationTop
      write SetMagnificationTop stored False;
    // @name is the width in pixels of the side view of the model.
    property SideWidth: integer read GetSideWidth
      write SetSideWidth stored False;
    // @name stores the reference X-coordinate for the side view of the model.
    property SideX: double read GetSideX write SetSideX stored False;
    // @name stores the reference Y-coordinate for the front view of the model.
    property SideY: double read GetSideY write SetSideY stored False;
    // @name is the Y-coordinate of the main form in pixels.
    property Top: integer read GetTop write SetTop stored False;
    // @name is the height of the top view of the model in pixels
    property TopViewHeight: integer read GetTopViewHeight
      write SetTopViewHeight stored False;
    // @name is the width of the top view of the model in pixels
    property TopViewWidth: integer read GetTopViewWidth write SetTopViewWidth stored False;
    // @name stores the reference X-coordinate for the top view of the model.
    property TopX: double read GetTopX write SetTopX stored False;
    // @name stores the reference Y-coordinate for the top view of the model.
    property TopY: double read GetTopY write SetTopY stored False;
    // @name is the width of the main form in GoPhast in pixels.
    property Width: integer read GetWidth write SetWidth stored False;
    // @name stores whether the model is maximized, minimized, or normal.
    property WindowState: TWindowState read GetWindowState
      write SetWindowState stored False;

    // The following properties are used only in PHAST models.

    // @name is the diffusivity in PHAST.
    property Diffusivity: double read FDiffusivity write SetDiffusivity;
    // @name is used to store options related to the grid in PHAST.
    property GridOptions: TGridOptions read FGridOptions write FGridOptions;
    // @name defines the grid used with PHAST.
    property PhastGrid: TPhastGrid read FPhastGrid write SetPhastGrid;
    // @name represents the @link(TPrintFrequencyItem)s in PHAST.
    property PrintFrequency: TPrintFrequencyCollection read FPrintFrequency
      write SetPrintFrequency;
    // @name stores options related to the PRINT_INITIAL data block in PHAST.
    property PrintInitial: TPrintInitial read FPrintInitial write FPrintInitial;
    // @name specifies options related to the solution method in PHAST.
    property SolutionOptions: TSolutionOptions read FSolutionOptions
      write FSolutionOptions;
    // @name is used to store options related to the STEADY_FLOW
    // data block in PHAST.
    property SteadyFlowOptions: TSteadyFlowOptions read FSteadyFlowOptions
      write FSteadyFlowOptions;
    // @name represents a collection of stress periods in PHAST.
    property Times: TTimeCollection read FTimes write SetTimes;
    // @name represents the title associated with the PHAST model.
    property Title: TStrings read FTitle write SetTitle stored True;
    // @name stores the default units in PHAST.
    property Units: TUnits read FUnits write SetUnits;

    // The following properties are used only in MODFLOW models.

    // @name stores MODPATH pathline data.
    property PathLine: TPathLineReader read GetPathLine write SetPathLine
      stored StorePathLine;
    // @name stores MODPATH endpoint data.
    property EndPoints: TEndPointReader read GetEndPoints Write SetEndPoints
      stored StoreEndPoints;
    // @name stores MODPATH times series data.
    property TimeSeries: TTimeSeriesReader read GetTimeSeries
      write SetTimeSeries stored StoreTimeSeries;

    // The following properties are used in both PHAST and MODFLOW models.

    // @name represents a series of bitmaps that can be displayed on
    // the top, front, or side view of the model.
    property Bitmaps: TCompressedBitmapCollection read FBitmaps write
      SetBitmaps;
    // @name is the vertical exaggeration of the front, side, and 3D views
    // of the model in GoPhast.
    property Exaggeration: double read GetExaggeration write SetExaggeration;
    // @name is used to read or write @link(TScreenObject)s to or from files.
    property ObjectList: TScreenObjectCollection
      read GetScreenObjectCollection write SetScreenObjectCollection;
    // @name is the version of GoPhast that last saved the model that is being
    // edited.
    property Version: string read GetVersion write SetVersion;
    // @name specifies the size and appearance of various portions of the
    // ModelMuse main window.
    property GuiSettings: TGuiSettings read FGuiSettings write FGuiSettings;
    // @name is the ModelMate file associated with this model.
    property ModelMateProjectFileName: string read FModelMateProjectFileName
      write SetModelMateProjectFileName;
    //  see @link(TDisplaySettingsCollection).
    property DisplaySettings: TDisplaySettingsCollection read FDisplaySettings
      write SetDisplaySettings;
    property ChildModels: TChildModelCollection read FChildModels
       write SetChildModels stored StoreChildModels;


    property ModflowSteadyParameters;
    property ModelSelection;
    property LayerStructure;
    property ModflowOptions;
    property ModflowStressPeriods;
    property SoluteTransport;
    property UseWaterTable;
    property FreeSurface;
    property ChemistryOptions;
    property HufParameters;
    property ObservationPurpose;
    property ModflowTransientParameters;
    property ModflowOutputControl;
    property DataSetList;
  end;

  TChildModel = class(TCustomModel)
  private
    FParentModel: TCustomModel;
    FModelName: string;
    procedure SetModelName(const Value: string);
  protected
    function GetScreenObjects(const Index: integer): TScreenObject; override;
    function GetScreenObjectCount: integer; override;
    function GetModflowSteadyParameters: TModflowSteadyParameters; override;
    function GetLayerStructure: TLayerStructure; override;
    procedure SetLayerStructure(const Value: TLayerStructure); override;
    procedure SetModflowOptions(const Value: TModflowOptions); override;
    function GetModflowOptions: TModflowOptions; override;
    function GetModflowStressPeriods: TModflowStressPeriods; override;
    procedure SetModflowStressPeriods(const Value: TModflowStressPeriods); override;
    procedure SetSoluteTransport(const Value: boolean); override;
    function GetSoluteTransport: boolean; override;
    procedure SetUseWaterTable(const Value: boolean); override;
    function GetUseWaterTable: boolean; override;
    function GetFreeSurface: boolean; override;
    procedure SetFreeSurface(const Value: boolean); override;
    procedure SetChemistryOptions(const Value: TChemistryOptions); override;
    function GetChemistryOptions: TChemistryOptions; override;
    function GetObservationPurpose: TObservationPurpose; override;
    procedure SetObservationPurpose(const Value: TObservationPurpose); override;
    procedure SetHufParameters(const Value: THufModflowParameters); override;
    function GetHufParameters: THufModflowParameters; override;
    function GetModflowTransientParameters: TModflowTransientListParameters; override;
    procedure SetModflowTransientParameters(
      const Value: TModflowTransientListParameters); override;
    function GetModflowOutputControl: TModflowOutputControl; override;
    procedure SetModflowOutputControl(const Value: TModflowOutputControl); override;
    function GetProgramLocations: TProgramLocations; override;
    procedure SetProgramLocations(const Value: TProgramLocations); override;
    function GetModflowFullStressPeriods: TModflowStressPeriods; override;
    procedure SetModflowSteadyParameters(const Value: TModflowSteadyParameters); override;
    function GetModelSelection: TModelSelection; override;
    procedure SetModelSelection(const Value: TModelSelection); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Invalidate; override;
    property ParentModel: TCustomModel read FParentModel;
  published
    property ModelName: string read FModelName write SetModelName;
  end;

  TChildModelItem = class(TPhastCollectionItem)
  private
    FChildModel: TChildModel;
    procedure SetChildModel(const Value: TChildModel);
  public
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
  published
    property ChildModel : TChildModel read FChildModel write SetChildModel;
  end;

  TChildModelCollection = class(TPhastCollection)
  private
    function GetItem(Index: integer): TChildModelItem;
    procedure SetItem(Index: integer; const Value: TChildModelItem);
  public
    property Items[Index: integer]: TChildModelItem read GetItem write SetItem; default;
  published
    constructor Create(Model: TComponent);
  end;

  procedure EnableLighting;

  // @name generates a name for a data set that is valid
  // and does not conflict with the names of any existing data sets.
  function GenerateNewName( Root: string = 'NewDataSet';
    InvalidNames: TStringList = nil; Connector: string = ''): string;

  {@name is used to generate a valid name from one that may be invalid.
  Valid names must begin with a letter or underscore.  The remaining
  characters must be letters, digits or the underscore character.}
  function GenerateNewRoot(const Root: string): string;


const
  StrGlobalVariables = 'Global Variables';
  StrNotepadexe = 'Notepad.exe';

  StrModelName = 'ModelMuse';

  // '0.5.0.0' is release candidate 1.
  // '0.6.0.0' is release candidate 2.
  // '1.0.0.0' is initial release version.
  // '1.0.1.0' fixed bug that caused access violations if a formula was
  //    invalid such as '1 2'.
  //    Fixed bug that caused access violations if all the vertices in a
  //    contour were deleted when it was first created.
  //    Changed the hint in the Working area depending on the selected tool.
  // '1.0.2.0' Fixed bug that caused access violations if the user changed
  //    an object so that it no longer affected a data set and then tried
  //    to edit the object again.
  //    Fixed bug that would cause access violations if a new model was started
  //    while frmShowHideObjects was visible.
  //    Fixed bug that allowed the undo and redo menu items and buttons to
  //    remain enabled when starting a new model.
  // '1'0.3.0' Fixed the "ObjectIntersectArea" and "ObjectIntersectLength"
  //    functions.
  // '1'0.4.0' Fixed bug that caused access violations if the user edited
  //    the data sets that an object affected and then tried to undo
  //    those changes.
  //    Fixed bug in reading character fields from Shapefiles.
  // '1.0.5.0' Fixed bug that caused object intersections to be evaluated
  //    incorrectly if the object was evaluated at nodes.  Fixed bug that
  //    could cause access violations when deleting objects.
  //    Updated memory manager.
  // '1.0.6.0' Fixed bug that caused access violations when closing GoPhast
  //    if a dataset had been deleted and then restored. Fixed bug that caused
  //    certain formulas to be 'decompiled' incorrectly.
  // '1.0.7.0' Fixed bug that caused access violations when editing objects
  //    that specified boundary conditions via PHAST-style interpolation.
  // '1.0.8.0' Fixed "ObjectIntersectArea" function.
  //    Fixed bug in interpolation.
  // '1.0.9.0' Fixed bug that prevented adding new wells.
  // '1.0.10.0' Final beta version of ModelMuse.
  // '2.0.0.0' First release of ModelMuse: Added support for MODFLOW-2005
  //    and MODPATH.
  // '2.0.0.1' Fixed problem with backwards particle tracking in MODPATH.
  //    Automatically updates formulas when the names of data sets and
  //    global variables are changed.
  // '2.0.0.2' Fixed bug in exporting "Factor" in flow observations.
  // '2.0.0.3' Fixed bug that allowed a global variable to have the
  //    same name as a TDataArray.
  //    Added ability to specify additions to the batch files that run
  //    MODFLOW or PHAST.
  // '2.0.0.4' Fixed bug exporting LAYWET. Added export of HUF package.
  // '2.0.0.5' Fixed bug (introduced in 2.0.0.1) in freeing TScreenObject.   
  // '2.0.0.6' Added support for a new way of selecting objects for editing.
  //    Added support for importing calculated heads for HUF units.
  // '2.0.0.7' Fixed bug importing multiplier and zone arrays when "none" or
  //    'all" are not in upper case letters.
  //    Added support for importing flow files from HUF.
  //  '2.0.0.8' Fixed bug: when pasting an TScreenObject from the clipboard,
  //    imported values were not pasted too.
  //    Fixed bug in Nearest Point interpolation method.
  //    Fixed bugs in importing DRN, DRT, and UZF packages.
  //    In Show/Hide Objects dialog box, the selected objects are displayed in
  //    a bold font.
  //    New Data Set Values dialog box.
  //    Improved speed of Nearest Point method when point objects are used.
  //    Imported flow observations
  //  '2.0.0.9' Fixed bug importing elevations formulas when importing
  //    Shapefiles.
  //  '2.0.0.10' Fixed bug importing gridded data with rotated grid.
  //    Improved speed of some interpolation methods. Modified the Rearrange
  //    Objects dialog box to allow it to show only the visible or only the
  //    selected objects.
  //  '2.0.0.11' worked on speeding up importing Shapefiles.
  //  '2.0.0.12' Fixed bugs in managing subscriptions.
  //  '2.0.0.13' Reduced memory usage.
  //  '2.0.0.14' Fixed bug importing Shapefile.
  //    Improved display of frmShowHideObjects.
  //  '2.0.0.15' Fixed bug reading existing files.
  //  '2.0.0.16' Fixed bug determining stress periods.
  //  '2.0.0.17' Added capability to export objects as Shapefiles.
  //    Added the ability to delete images that have been imported.
  //    Deleting all the points in an object in the
  //    Object Properties dialog box now results in the object being deleted.
  //    When exporting boundary conditions, there is now a warning
  //    if no cells are defined.
  //    Grid Values dialog box updated to show grid cell dimensions.
  //    Transient data after the end of the
  //    last defined stress period is now ignored.
  //  '2.0.0.18' Fixed problems with exporting Shapefiles.
  //    Observations outside the defined model times are no longer exported.
  //  '2.0.0.19' Improved export of objects to Shapefiles.
  //    Holding down the Shift key when releasing the mouse while creating
  //    objects of moving nodes of objects now causes the new or moved vertex
  //    to "snap" to the location of a nearby node of another object if it
  //    is within 3 pixels of the cursor location.
  //  '2.0.0.20' Fixed bug in contouring data.
  //    Fixed bug in selecting CHD package
  //    (couldn't specify comments for the CHD package even if it was selected.)
  //  '2.0.0.21' Added shortcut for displaying the Show/Hide objects dialog
  //    box.
  //    Fixed bug in reducing the number of items for a MODFLOW boundary
  //    condition.
  //  '2.0.0.22' Better names for objects when importing model results.
  //  '2.0.0.23' When snapping to points is activated, the cursor changes
  //    when it is in the vicinity of a point on a different ob
  //  '2.0.0.24' Changed the way MODFLOW wells are exported so that if an
  //    object has multiple sections in the same cell,
  //    the exported pumping rate is the sum of the pumping rate for each
  //    section.
  //  '2.0.0.25' Fixed bug coloring the grid with parameters
  //    after modifying the grid.
  //    Fixed bugs in export of MODPATH files.
  //  '2.0.0.26' Fixed bug importing points; a blank file name
  //    is no longer added to the files to archive.
  //  '2.1.0.0' Fixed bug importing EVT, ETS, and RCH packages
  //    when parameters were used.
  //    Added Save and Clear buttons to Errors and Warnings dialog box.
  //    Fixed bug in export of WETDRY in HUF.
  //    Fixed bug in export of FACTOR in imported flow observations.
  //  '2.1.1.0' Fixed bug importing WETDRY in LPF.
  //    Fixed bug setting elevations of objects when importing
  //    EVT and ETS packages.
  //    Fixed bug importing SFR.
  //    Fixed bug importing DIS.
  //  '2.1.1.1' Fixed bug in which models that used parameters but not zones
  //    were not imported correctly.
  //  '2.1.1.2' Changed the way that CHD, DRN, DRT, GHB, and WEL are exported.
  //    Now each section of an object will represent a different boundary cell
  //    if two or more sections are in the same cell.
  //    Fixed bug in TframeSfrParamInstances that prevented some valid
  //    parameter instance names from being accepted.
  //    Fixed bug in TframeSfrParamInstances that prevented the Delete
  //    and insert buttons from being visible.
  //  '2.1.1.3' Improved handling of pasting vertex data in the
  //    Object Properties form.
  //    Fixed bug that sometimes prevented data set comments from being
  //    edited properly.
  //    Added support for new options in MODFLOW-2005 version 1.7.
  //      Added support for Gage type 4 for Lakes in the Gage package.
  //      Added support for Gage type 8 for SFR streams in the Gage package.
  //      Added support for SURFDEPTH in Lake package.
  //    New functionality in the MODFLOW Name File dialog box.  If the input
  //    for a package is listed in the Name File dialog box, the input file
  //    for that package will not be generated by ModelMuse.  See the help
  //    for the MODFLOW Name File dialog box for full details.
  //  '2.1.1.4' Fixed saving data for HUF package. This bug was not present in
  //    version '2.1.1.0'.
  //    Reduced memory usage.
  //  '2.1.1.5' Fixed bug that caused non parameter data to appear in the
  //    Object Properties dialog box for objects that did not have
  //    non-parameter data.  The bug occurred if a previous object which
  //    did have non-parameter data was edited first.
  //    Fixed bug that caused ModelMuse to hang if there was an error
  //    in a data set formula.
  //  '2.1.1.6' Added support for MNW2 and MNWI packages.
  //    Added new filters for coloring and contouring "Active only" and
  //    "Values to Ignore".
  //    Added import of ITT when importing head observations from Shapefiles.
  //    Fixed bug importing UZF gages.
  //    Fixed bug in setting head observation names.  Spaces and quotes are removed.
  //  '2.1.1.7' Worked on reading pathlines from MODPATH.
  //    Fixed bug that caused COFF to be exported incorrectly if an observation
  //    was exactly on a column boundary.
  //    Fixed bug in which the display of head observations was not changed
  //    when a TScreenObject was moved.
  //    Fixed bug that caused head observations imported from an existing
  //    MODFLOW model to be imported with the elevation from the wrong cell if
  //    ROFF = -0.5 or COFF = 0.5.
  //    Fixed bug that could cause access violations when contouring 2D data
  //    sets if the selected layer (or row or column) was not the first layer
  //    (or row or column).
  //    Added import of parameter names when importing RCH from a Shapefile.
  //  '2.1.1.8' Added import of parameter names when importing CHD, DRN, DRT,
  //    ETS, EVT, GHB, RIV, or WEL from a Shapefile.
  //    Fixed bug exporting flow observations when the formula for Factor
  //    is complex.
  //  '2.1.1.9' Added support for displaying pathlines from MODPATH.
  //  '2.1.1.10' Fixed bug that caused too much memory to be used when
  //    exporting ETS package.
  //  '2.1.1.11' Fixed bug that caused problems editing ETS data for
  //    objects.
  //    Added support for displaying endpoints from MODPATH.
  //  '2.2.0.0' Added support for displaying time-series data from MODPATH.
  //    Added support for multiple vertical screens in MNW2.
  //    Multiplier and Zone arrays for transient data are tested to see if
  //    they are identical to a previous Multiplier or Zone array
  //    for transient data. If so, the previous array is reused.  This
  //    saves memory in MODFLOW.
  //    Fixed bug exporting gridded data to Shapefile.  Duplicate fieldnames
  //    are no longer created.
  //    Fixed access violation bug in closing TfrmImportGriddedData.
  //    When importing points, empty lines
  //    and lines starting with "#" are skipped.
  //    Ruler settings are now saved with the file.
  //    Changed "Select Objects of Editing" dialog box to
  //    "Select Objects of Editing or Deletion" dialog box
  //    along with a change in functionality to allow objects to be
  //    deleted with this dialog box.
  //  '2.2.1.0' Fixed bug that prevented RunPhast.bat from executing
  //    correctly when it was executed by ModelMuse.
  //  '2.2.1.1' When importing model results, heads and drawdowns equal to
  //      HDRY and HNOFLO are not used in assigning the minimum and maximum
  //      values for the array.
  //    In Data Set Values dialog box, the layers are now in a list on the left.
  //    Fixed bug that caused the HFB and UZF packages to become
  //      deselected whenever the selected flow package was switched.
  //    Fixed bug that caused the number of wetting iterations could be saved
  //      as zero incorrectly if it should have been a 1.
  //      However, MODFLOW would change it back to 1 so it would not affect
  //      model computations.
  //    Added support for BCF package.
  //    When the HUF package is selected, optional data sets will be created
  //      that display hydaulic properties that result from applying the
  //      HUF parameters to the model.
  //    Added support of importing the MNWI package from an existing model.
  //    Fixed bug exporting HUF files in which storage parameters were exported
  //      in steady-state models.
  //  '2.2.1.2' Fixed bug in LPF and HUF package export in which there could
  //      be no space between a multiplier array name and a zone array name.
  //    Fixed bug in HUF package in which zone and multiplier arrays were not
  //      written.
  //    Fixed bugs that could cause access violation when opening a new model
  //      after having edited objects in the currently opened model.
  //  '2.2.1.3' Fixed bug that kept the videos from being played automatically.
  //    Added a check of the fractional depth and fractional rates in the ETS
  //      package.
  //    Fixed bug that caused an error when certain functions were used to
  //      assign values to data sets.
  //    Improved speed.
  //    Fixed bug that caused errors creating temporary files if more than
  //      one model was being run at one time.
  //    Fixed bug getting temp file names.
  //  '2.2.1.4' Added warning when an input file specified in the
  //      MODFLOW Name File dialog box does not exist.
  //    Files specified in the MODFLOW Name File dialog box are now
  //      included in the archive.
  //    Fixed export of Shapefiles.
  //  '2.2.1.5' The mouse wheel can now be used to zoom in or out in the
  //      top, front, or side views of the model.
  //    Fixed bug importing ModelMate file.
  //    Added support for the SUB package.
  //    Fixed bug deleting the last Layer group in a model.
  //    Added support for importing binary files generated by the SUB package.
  //    Added support for importing the SUB package
  //      from existing MODFLOW models.
  //    Fixed bug in getting the formula from the Formula Editor.
  //  '2.2.1.6' Fixed bug importing models that contain the HUF package when a
  //      zone array was not used with a parameter.
  //    Fixed bug that allowed invalid formulas to be set for
  //      the Z elevation formulas for an object.
  //    Fixed bug with access violations when checking the internet.
  //    Disabled MODPATH export for PHAST models.
  //  '2.2.1.7' Assigned default names to new systems of delay and no-delay beds.
  //    Fixed bug in evaluating formulas that contain "ActiveOnLayer" for
  //      Z-coordinates
  //  '2.2.1.8' When updating ModelMate files, if an observation
  //      or prediction group has no associated observations or predictions,
  //      and the user has elected to delete unused observations or predictions,
  //      the unused observation or prediction groups are deleted too.
  //    When importing model results and updating existing data for the results
  //      duplicate copies of HDRY and HNOFLO are no longer added to the
  //      values to skip.
  //    Improved error messages for functions that retrieve imported data.
  //    Fixed bug getting names of new temporary files.
  //    Fixed bugs in exporting ModelMate files.
  //  '2.2.1.9' Added info on associated model input data sets for SUB package.
  //    Added support for importing Surfer Grid files.
  //  '2.2.1.10' Attempted to fix problem with access violations when
  //      accessing the file menu.
  //  '2.2.1.11' Added support for Surfer ASCII grid files.
  //  '2.2.1.12' Fixed bug with not conserving memory properly in EVT.
  //  '2.2.1.13' Worked on conserving memory.
  //  '2.2.1.14' Worked on conserving memory. Fixed bug that caused
  //      access violations when undoing the setting of a boundary condition.
  //  '2.3.0.0' Fixed bug that could cause layer elevations to be set
  //      incorrectly after changing the layer structure.
  //    Fixed bug that could cause access violation when deleting a data set.
  //  '2.3.0.1' Bug fix: Corrected name of the
  //      "Select Objects for Editing or Deletion."
  //    Enhancement: Added warning messages in export of the
  //      MODPATH input files if the required MODFLOW input or
  //      output files are missing.
  //    Bug fix: when deleting data sets, check that all data set formulas
  //      are still OK.
  //    Bug fix: the text on the status panel is no longer truncated
  //      when it is over 110 characters in length.
  //    Bug fix: fixed access violation when closing with the Color Grid
  //      or Contour data dialog boxes open.
  //    Bug fix: fixed import of rate data in the UZF package when some
  //      rates are reused and others are not reused.
  //    Change: Boundary condition times prior to the beginning of the first
  //      defined boundary condition are now ignored.
  //  '2.3.0.2' Enhancement: Added support for sampling DEM files.
  //    Bug fix: Fixed access violations that occurred when closing
  //      ModelMuse after deleting a data set.
  //    Change: When importing character attributes of Shapefiles,
  //      blank characters at the beginning or end will be removed.
  //    Bug fix: The row width is now displayed correctly on the Grid Values
  //      dialog box.
  //    Bug fix: When the first time defined in the MODFLOW Time dialog box
  //      is after the first time defined in an object, values were not
  //      assigned properly. (Bug introduced in version 2.3.0.1.)
  //  '2.3.0.3' Change: added pumping rate comment for wells. This can
  //      be helpful when using PEST.
  //  '2.3.0.4' Bug fix: If attempting to draw a bitmap results in an
  //       out-of-resources error, drawing the bitmap will be skipped.
  //     Change: menu item caption switches between "Show 2-D Grid"
  //       and "Hide 2-D Grid" depending on whether or not the grid lines
  //       are visible.
  //     Bug fix: Various controls on the Color Grid and
  //       Contour Grid dialog boxes now changes position appropriately
  //       if the dialog box is changed in size.
  //     Bug fix: The beginning and ending times for MODPATH can now be
  //       set to non-integer values.
  //     Bug fix: The name of a multiplier array would not be exported
  //       correctly if it was used in one of the HUF package input files
  //       but that input file was not being created by ModelMuse.
  //  '2.3.0.5' Change: if a bitmap can not be displayed, it will be hidden
  //       and then must be shown again manually after decreasing the
  //       magnification.
  //     Bug fix: fixed "List index out of bounds" error when deleting
  //       a data set.
  //     Bug fix: If every cell has gone dry, there is no longer an
  //       "List index out of bounds" error when attempting to import
  //       the model results.
  //     Enhancement: It is now possible to paste multiple data set names in
  //       the table on the Data tab of the Import Shapefile dialog box.
  //  '2.3.0.6' Bug fix: The labels for the various Z-coordinate formulas
  //       in the Import Shapefile dialog box are now enabled when appropriate.
  //  '2.3.0.7' - '2.3.0.9' Bug fix: Fixed bug that prevented
  //       the Packages and Programs dialog box from being displayed
  //       on some computers.
  //     Bug fix: Fixed bug that caused an Assertion error when
  //       attempting to show the "Select Objects By Name" dialog box
  //       in a model that had no objects.
  //  '2.3.0.10' Enhancement: The Grid Value dialog box has been changed to
  //       display the name of the selected object and to show its 3'rd
  //       dimension coordinates for those cells intersected or enclosed
  //       by the object.
  //  '2.3.0.11' Enhancement: Improved speed of importing data.
  //  '2.3.0.12' Bug fix: Saving the ModelMuse file with a new file name now
  //       results in the default name for the model input files being
  //       changed as well.
  //     Bug fix: When importing Shapefiles, if the feature that is being
  //       imported must be imported as separate objects, the Import Shapefile
  //       dialog box no longer allows them to be combined.
  //     Bug fix: When flow observations were used the GHB, DRN, RIV, and CHD
  //       packages were not always exported correctly.
  //   '2.3.0.13' - '2.3.0.17' Reduced disk usage.
  //   '2.3.0.18' Enhancement: Improved speed.
  //   '2.3.0.19' Bug fix: fixed bug introduced in '2.3.0.18'.
  //   '2.3.0.20' Bug fix: Fixed display of the recharge, EVT, and ETS layers
  //       when no parameters were defined.
  //     Bug fix: The SFR package input could be generated incorrectly if an
  //       object that was supposed to define a stream segment did not
  //       intersect the grid.
  //   '2.3.0.21' Bug fix: Fixed display of RCH, EVT, and ETS rates when
  //       parameters are first defined.
  //     Bug fix: Fixed when certain controls related to MODPATH become
  //       enabled.
  //     Bug fix: There no longer is an access violation if you attempt to
  //       create a new model while in the midst of creating a new object.
  //     Bug fix: Fixed calculation of HUF_Kx.
  //     Bug fix: Fixed export of Reference time in the MODPATH response file.
  //   '2.3.0.22' Enhancement: When the MODFLOW Hydrogeologic Units dialog box
  //       is closed, it now checks that at least one parameter
  //       has been associated with each hydrogeologic unit and that
  //       each parameter is associated with at least one hydrogeologic unit.
  //     Enhancement: The thickness of HUF units is now constrained to be
  //       greater than or equal to zero.
  //     Bug fix: Access violations no longer occur when creating a new
  //       object after having closed a previous model and starting
  //       a new model.
  //     Bug fix: fixed export of BeginPeriod and BeginStep.  They must both
  //       be set to 1 if any stress period in the model is a steady-state
  //       stress period.
  //   '2.3.0.23' Bug fix: Fixed Assertion error in evaluation of MNW2 input.
  //     Enhancement: Added error message if a MODPATH particle start time was
  //       invalid.
  //     Bug fix: Fixed import of the RES package from existing models.
  //     Enhancement: Added Epsilon when specifying data values to skip.
  //     Bug fix: Fixed display of UZF transient data sets.
  //     Bug fix: Fixed deletion of verticies in an object when all but one
  //       vertices are deleted.
  //     Bug fix: Fixed default file names for the MODFLOW name file
  //       and for the default output files when the file name has a space
  //       in it.
  //     Bug fix: Fixed display of MODPATH particle release times.
  //     Bug fix: Fixed assignment of color to MODPATH pathlines.
  //     Bug fix: Fixed crash when entering multiple times for flow
  //       observations.
  //     Change: Statistic and Stat-Flag are no longer visible for head
  //       observations unless ModelMate interface has been activated.
  //     Bug fix: When attempting to import an incomplete Shapefile,
  //       ModelMuse now exits the procedure properly.
  //     Enhancement: When importing head observations, if the ModelMate
  //       interface is active, Statistic and StatFlag are imported too.
  //   '2.3.0.24' Bug fix.  Entering a real number for the recharge layer
  //       no longer causes ModelMuse to hang. Instead an error message is
  //       displayed.
  //   '2.3.0.25' Bug fix: If wetting is active but there are no
  //       convertible layers a warning is displayed.
  //     Bug fix: If wetting is active but there are no
  //       convertible layers data set 7 is no longer created in the LPF
  //       package and data set 4 is no longer created in the HUF package.
  //     Bug fix: When the ET Surface and the ET Depth are undefined
  //       in the EVT or ETS packages, an error message is displayed instead of
  //       the export failing.
  //     Bug fix: Editing an object containing SFR data no longer results in
  //       an access violation if the SFR package is not selected.
  //     Bug fix: Editing an object that defines data for the SFR, MNW2, HOB
  //       or HFB packages no longer results in deletion of data for those
  //       packages when those packages are not selected.
  //     Bug fix: In the Object Properties dialog box, clicking the
  //       "Edit F()..." button on the Data Sets tab would cause
  //       an Assertion error when no data set was selected.  The button
  //       is now disabled when no data set is selected.
  //   '2.3.0.26' Bug fix: When performing coordinate conversions on Shapefiles,
  //       you can no longer select a UTM zone until you have chosen to
  //       perform the conversion.
  //     Enhancement: In the Show or Hide Objects dialog box, any collapsed
  //       branch that contains a selected object is shown with a bold font.
  //     Bug fix: Fixed access violations when opening a model when a model is
  //       already open.
  //     Enhancement: If additional vertices are added to an object that
  //       defines head observations, an error message is displayed when
  //       attempting to export the HOB input file or plot HOB data on the
  //       grid.
  //     Bug fix: If the flow package corresponding to a flow observation
  //       is not selected in the object properties dialog box, attempting
  //       to select the flow observation will no longer cause toggling
  //       of the selected node.
  //     Bug fix: After selecting the item with with to color the grid in
  //       the Color Grid dialog box, you can click directly on the Apply
  //       button without clicking elsewhere first.
  //     Enhancement: When defining a new global variable, default values for
  //       the type and value are displayed in the Global Variables dialog box.
  //     Enhancement: Bitmaps that were hidden because the magnification was
  //       too high, are now displayed again automatically when the
  //       magnification is sufficiently reduced.
  //     Enhancement: Added new function for use in formulas - PositionInList.
  //     Enhancement: Objects now have a comment field that can be used to
  //       document what the object does.
  //     Enhancement: The Grid Spacing dialog box now allows negative numbers
  //       for the default spacing for columns, rows, and layers.  In MODFLOW
  //       models, the default spacing for rows is set to -100 instead of 100.
  //     Enhancement: When tips are turned off, ModelMuse will tell you when it
  //       detects new videos on the ModelMuse web site.
  //     Bug Fix: Editing an object that defines an SFR stream with unsaturated
  //       properties defined no longer causes an assertion failure if the
  //       object is edited again.
  //     Bug fix: Importing a model no longer causes an access violation
  //       when the previous model had objects that defined an SFR stream
  //       with unsaturated properties.
  //   '2.4.0.0' No additional changes.
  //   '2.4.0.1' Enhancement: It is now posssible to copy and paste multiple
  //       cells in the parameter grids in the Packages and Programs dialog box.
  //     Enhancement: The helpfile has increased functionality.
  //     Enhancement: Support for ZONEBUDGET added.
  //   '2.4.0.2' Bug fix: The number of observations in the Flow observations
  //       packages was calculated incorrectly when both observation and
  //       predictions were used.
  //   '2.4.0.3' Bug fix: FractionOfObjectLength was calculated incorrectly
  //       if an object had more than one section.
  //     Enhancement: Added new function InterpolatedVertexValues that allows
  //       numeric values associated with individual vertices to be interpolated
  //       along the length of the object.
  //   '2.4.0.4' Bug fix: ObjectLength was calculated incorrectly if an
  //       object had more than one section.
  //     Bug fix: ObjectVertexDistance was calculated incorrectly if an
  //       object had more than one section.
  //     Enhancement: Antialiasing used to improve appearance.
  //     Bug fix: Importing a MODFLOW-2005 model failed if LAYVKA in the
  //       LPF package was anything besidea a zero or one.
  //     Bug fix: If there VKA is less than or equal to zero
  //       but there is only one layer, ModelMuse no longer reports
  //       and error while exporting the LPF package.
  //     Bug fix: Unchecking the MODPATH initial particle placement checkbox
  //       didn't work.
  //     Bug fix: Editing more than one object at a time
  //       could result in an assertion failure.
  //     Enhancement: When coloring grid only on the active areas, inactive
  //       areas are shaded.
  //     Enhancement: Additional options for displaying grid lines have been
  //       added. It is now possible to show just the outline of the active
  //       area or just the gridlines inside the active avea.
  //   '2.4.0.5' Bug fix; Sample DEM imported data at the wrong
  //       locations if the grid angle was not zero.
  //   '2.4.0.6' Enhancement: Legends have been added to the Color Grid and
  //       Contour Data dialog boxes.
  //     Enhancement: Added three new functions ColumnCenter, RowCenter,
  //       and LayerCenter.  See help for details.
  //   '2.4.0.7' Bug fix: When deleting time-varying data from objects
  //       for the RCH, EVT, or ETS packages, sometimes not all the data
  //       would be deleted properly.
  //     Bug fix: In the LPF package, LAYTYP was not set to a negative
  //       value properly when the THICKSTRT option was used.
  //   '2.4.0.8' No change
  //   '2.4.0.9' Fixed bug editing LPF parameters that could cause an
  //       assertion failure.
  //   '2.4.0.10' Enhancement: The Export Image dialog box has been added.
  //       It allows the user to export an image of the top, front, or
  //       side view of the model as an Enhanced Windows Metafile.
  //     Enhancement: When a formula results in a value that is infinite or
  //       not a number, the maximum double-precision real number is
  //       substituted for it.
  //   '2.4.0.11' Bug fix for Export Image dialog box.
  //   '2.4.0.12' Bug fix: Under certain circumstances extra LPF parameters
  //       would be added incorrectly.
  //   '2.4.0.13' Bug fix: Attempting to open a model containing a UZF gage
  //       caused an access violation.
  //   '2.4.0.14' Bug fix: Transient UZF data were not displayed properly.
  //       This was not a bug in the released version.
  //     Bug fix: If the user edited something in the MODFLOW Packages
  //       and Programs dialog box that caused the grid to be recolored,
  //       the MODFLOW Packages and Programs dialog box might not close
  //       properly. This was not a bug in the released version.
  //   '2.4.0.15' Bug fix: closing the model while the Color Grid or
  //       Contour Data dialog boxes were visible could result in an
  //       access violation.
  //       This was not a bug in the released version.
  //   '2.4.0.16' Change: Changed default options for ZONEBUDGET.
  //   '2.4.0.17' Bug fix: ModelMuse could not always read the budget file
  //       if there was only one column in the model.
  //   '2.4.0.18' Enhancement: Enabled MadExcept.
  //   '2.4.0.19' Second attempt at enabling MadExcept.
  //   '2.4.0.20' Bug fix: If a parameter or layer group was renamed, formulas
  //       that used data sets related to those parameters were not updated
  //       properly.
  //   '2.4.0.21' Bug fix: Using the up-down controls on the Color Grid or
  //       Contour Data dialog boxes would cause selected data set
  //       to change when clicking on another control until the
  //       dialog box was closed.
  //     Bug fix: When importing MODFLOW models that contained the lake package
  //       the Lake_ID data set was not being set properly.
  //     Bug fix: Fixed bug that caused access violations when closing a model.
  //     Enhancement: Improved speed of opening Object Properties dialog box.
  //   '2.4.0.22' Bug fix: Failure to define unsaturated flow properties
  //       in the SFR package when they are required now results
  //       in an error message instead of an assertion failure.
  //     Bug fix: Added support for contour legends with
  //       boolean and string data.
  //   '2.4.0.23' - '2.4.0.24' Bug fix: Attempted to work around problem
  //       that causes range check errors when closing ModelMuse
  //       on some machines.
  //   '2.4.0.25' Change: Data set values are now saved to file
  //       with the data set if the data set values are up to date.
  //       This improves the speed of opening some large models.
  //   '2.4.0.26' Bug fix: Fixed bug that could cause an access violation
  //       when showing the MODFLOW Packages and Programs dialog
  //       box if one of the packages in the dialog box had been deactivated
  //       and then the cancel button was pressed.
  //     Bug fix: Fixed bug that could cause extra parameters to be added
  //       when a parameter value was edited.
  //     Enhancement: When selecting a MODPATH output file, the most likely
  //       name of the output file is selected automatically.
  //   '2.5.0.0' No additional changes.
  //   '2.5.0.1'  Change: The "Object|Edit|Merge Objects" command now operates when
  //       very small differences exist between the endpoints of the objects
  //       being merged. Previously, the match had to be exact.
  //     Enhancement: In the Object Properties dialog box, if you paste
  //       a group of vertices, the table will expand to accommodate the
  //       new vertices.
  //   '2.5.0.2' Bug fix: Editing an object in a new model after having
  //       previously closed a previous model without restarting ModelMuse
  //       caused an Assertion failure.
  //     Bug fix: Eliminated an assertion failure that occurred under certain
  //       circumstances when attempting to draw an object.
  //   '2.5.0.3' Bug fix: Eliminated Range Check Error that sometimes
  //       occurred when a menu item was selected.
  //   '2.5.0.4' Bug fix: Fixed bugs in deleting all print/save choices in the
  //       Subsidence package.
  //     Bug fix: Fixed layout of Subsidence package controls in
  //       MODFLOW Packags and Programs dialog box.
  //     Bug fix: The Formula Editor was displaying the ActiveOnLayer function
  //       in cases where it wouldn't work.
  //     Bug fix: The ActiveOnLayer function sometimes returned
  //       an incorrect value.
  //   '2.5.0.5' Bug fix: When closing a model, a stack overflow could occur
  //       under unusual circumstances.
  //   '2.5.0.6' Bug fix: If "Boundary Conditions, Observations,
  //       and Other Features" was selected in the "Color Grid" dialog box,
  //       an access violation could occur.
  //     Bug fix: Fixed problem that sometimes prevented the discretization
  //       from begin specified.
  //     Enhancement: Added support for SWT package.
  //     Bug fix: Export of format codes in SUB package was incorrect.
  //     Bug fix: Format codes in SUB package imported incorrectly.
  //     Enhancement: Added support for HYDMOD package.
  //     Bug fix: When more than one object was being edited in the
  //       Object Properties dialog box, switching to the SFR|Network tab and
  //       clicking the button under OUTSET and IUPSEG would cause an
  //       Assertion failure. Now it gives a more meaningful error message.
  //   '2.5.0.7' Bug fix: Changing the discretization when the grid was
  //        colored with transient data caused an access violation.
  //   '2.5.0.8' Bug fix: Attempting to export the MODPATH input files before
  //       MODPATH has been activated now results in a warning message instead
  //       of an assertion failure.
  //     Bug fix: It is now possible to select the "Edit vertex values" button
  //       when the "Select vertices" button is pressed.
  //     Bug fix: When exporting the BCF package, sometimes an attempt was made
  //       to export data set 8 when it shouldn't have been exported.
  //   '2.5.0.9' Bug fix: If a HUF SYTP parameter was defined, attempting to
  //       open the MODFLOW packages and programs dialog box would result in an
  //       assertion failure.
  //     Enhancement: The Manage Parameters dialog box has been added.
  //     Enhancement: The Global Variables are now alphabetized.
  //     Enhancement: Initial heads can now be read from a binary head file
  //       generated by another MODFLOW model.
  //   '2.5.0.10' Bug fix: Fixed sorting of global variables
  //     (bug was not in released version.)
  //   '2.5.0.11' Enhancement: The selection cube now responds
  //       to the mouse wheel.
  //     Change: When importing model results, the formulas used for the
  //       3D data sets have been changed to keep the formulas valid if
  //       the number of layers is increased.
  //     Bug fix: Attempting to contour a data set that is uniform no longer
  //       results in a range-check error.
  //     Bug fix: When attempting to export the MODFLOW input files, if a file
  //       can't be created because it is in use, an error message will be
  //       displayed.
  //   '2.5.0.12' Enhancement: The Manage Head Observations dialog box has
  //       been added.
  //     Change: If an invalid formula is encountered when exporting the MODFLOW
  //       input files the Formula Errors dialog box is not displayed until the
  //       export is complete.
  //   '2.5.0.13' Change: Items in "Search for Objects" dialog box
  //       are now listed in a tree component.
  //     Bug fix: When parameters are used in the RCH, EVT, and ETS packages,
  //       the print codes for the parameters are set correctly.
  //   '2.5.0.14' Enhancement: When saving a ModelMate file, the user can
  //       choose to have the ModelMate file opened with ModelMate.
  //     Bug fix: fixed problem that could make it opening the Object
  //       Properties dialog box slow.
  //     Bug fix: Attempting to open a Shapefile that is already open
  //       now generates an error message but not a bug report.
  //     Enhancement: A warning is now issued if an observation name is not
  //       valid when used in UCODE.
  //     Change: ModelMate program locations are now saved to an ini file.
  //     Bug fix: In MODFLOW models, if a data set is evaluated at nodes,
  //       the user can no longer attempt to color the grid with the data set
  //       values or contour the data set values.
  //     Bug fix: MODFLOW models in which the same cells are defined as
  //       constant head cells through both the BAS and CHD packages are
  //       now imported correctly.
  //   '2.5.0.15' Enhancement: ModelMate interaction improved.
  //   '2.5.0.16' Enhancement: When the nonparameter data in the CHD, DRN, DRT,
  //       EVT, ETS, GHB, RCH, RIV, or WEL packages for one stress period
  //       repeat the data from a previous stress period, the package instructs
  //       MODFLOW to reuse the data from the previous stress period rather
  //       than exporting another copy of the same data.
  //   '2.5.0.17' Enhancement: Reduced memory usage when opening files.
  //     Enhancement: Attempting to read an invalid DEM now results in an
  //       error message instead of generating a bug report.
  //   '2.6.0.0' No further changes.
  //   '2.6.0.1' Enhancement: When coloring the grid causes a data set to be
  //       recalculated, there will be form displaying the steps involved
  //       in calculating the values.
  //     Bug fix: Postprocessing for Geostatic Stress and changes in Geostatic
  //       stress has been fixed.
  //     Bug fix: Opening an object in the Object Properties dialog box no
  //       longer causes causes transient data to be recalculated.
  //     Enhancement: Channel cross sections in the SFR package can now be
  //       imported from Shapefiles.
  //   '2.6.0.2' Enhancement: When importing SFR data from Shapefiles, formulas
  //       can now be used for SLOPE, STRTOP, STRTHICK, STRHC1, THTS, THTI,
  //       EPS, and UHC.
  //     Enhancement: Improved error handling when importing an image.
  //     Bug fix: Attempting to write a locked file or attempting to open a
  //       file that doesn't exist now results in an error message instead of
  //       a bug report.
  //     Bug fix: Exporting locations of MODPATH and ZONEBUDGET sometimes
  //       were not enclosed in quotes when quotes were required.
  //     Enhancement: Reduced flicker when drawing new objects. This may also
  //       have fixed a intermittent bug that caused access violations when
  //       coloring the grid.
  //     Bug fix: Fixed coloring the grid when integer values were being used
  //       to color the grid and limits were used to filter what cells
  //       would be colored.
  //   '2.6.0.3' -----
  //   '2.6.0.4' Bug fix: VertexInterpolate didn't work properly
  //       if the grid was rotated.
  //   '2.6.0.5' Enhancement: Images can now be exported as .bmp files as
  //       well as .emf files.
  //     Bug fix: Pasting objects didn't work properly if the object
  //       had associated vertex values.
  //     Bug fix: Sometimes changing the number of Z formulas
  //       could lead to an assertion failure.
  //     Enhancement: Improved speed of opening the Global Variables
  //       dialog box.
  //     Bug fix: Opening the Object Properties dialog box would sometimes
  //       cause an access violation if another model had been opened
  //       previously.
  //     Enhancement: In the Select Objects by Name dialog box, the objects
  //       are sorted alphabetically.
  //     Bug fix: In the Select Objects by Name dialog box, the objects are
  //       now placed on the correct tab instead of always being put on the
  //       tab for the top view if the model.
  //   '2.6.0.6' Enhancement: In the Color Grid and Contour Data dialog boxes,
  //       when the user changes the data set used to color or contour the grid,
  //       the same limits as in the data set that is currently displayed
  //       can be used with a newly selected data set.
  //   '2.6.0.7' Bug fix: in the SFR package, NSTRAIL, ISUZN, and NSFRSETS
  //       were written when ISFROPT > 0 instead of when ISFROPT > 1.
  //   '2.6.0.8' Bug fix: Deleting the last vertex of an object
  //       no longer causes an error.
  //     Bug fix: When the grid was colored with horizontal flow barriers,
  //       changing an object that defines a horizontal flow barrier
  //       now causes the displayed barriers to be updated correctly.
  //     Editing multiple objects in a new model no longer causes an error.
  //     Bug fix: Inactive cells are no longer included in head observations.
  //   '2.6.0.9' Bug fix: on the Import Image dialog box, it is no longer
  //       possible to set the number of rows in the table to less than 1.
  //     Bug fix: Previously, the MODPATH zone was incorrectly limited to
  //       values greater than or equal to zero even for specified head cells.
  //   '2.6.0.10' Bug fix: Attempting to import model head or drawdown results
  //       from a file in which the number of layers is greater than the number
  //       of simulated layers now results in an error message instead of
  //       generating an error report.
  //   '2.6.0.11' Enhancement: The macros "%SP", "%TS", and "%ET" can now be
  //       used in text on the Export Image dialog box.  They will be replaced
  //       by the stress period number, time step number, and elapsed time
  //       respectively if those data are in the comment for the data set.
  //     Change: The elapased time is now included in the data set comment
  //       when importing MODFLOW results.
  //   '2.6.0.12' Bug fix: MODPATH results were not cleared when opening
  //       a new model.
  //   '2.6.0.13' Bug fix: Fixed macros in Import Image dialog box so that
  //       they work with the title too. (Bug not in released version.)
  //   '2.6.0.14' Enhancement: Contours can now be exported to a Shapefile.
  //     Change: All the commands for exporting Shapefiles have been moved to
  //       a submenu.
  //     Enhancement: Pathlines can now be exported to a Shapefile.
  //     Enhancement: Endpoints can now be exported to a Shapefile.
  //     Bug fix: Previously, the display of MODPATH times series points was
  //       incorrect when there was more than one release time.
  //     Enhancement: TimeSeries can now be exported to a Shapefile.
  //   '2.6.0.15' Bug fix: when opening a new model, bitmaps from the previous
  //       model are now removed.
  //     Enhancement: ModelMuse now can create a series of bitmaps that
  //       can be used to create a video.
  //   '2.6.0.16' Enhancement: ModelMuse now will warn the user if a CHD, DRN,
  //       DRT, GHB, RIV, SFR, or WEL cell is in an inactive cell.
  //   '2.6.0.17' Enhancement: When coloring the grid with transient data,
  //       only the data for the time being used to color the grid is evaluated.
  //     Bug fix: Sometimes moving an object did not cause the data sets
  //       that depend on it to be updated.
  //     Bug fix: When importing a shape file, the interpretation algorithm
  //       is now set correctly.
  //     Bug fix: When exporting .emf files, sometimes the image size was
  //       set to an incorrect value.
  //   '2.7.0.0' Change: Updated memory manager to latest version.
  //   '2.7.0.1' Bug fix: In the Set Widths of Columns, Rows, and Layers,
  //       dialog box, it is no longer possible to specify an invalid
  //       column, row, or layer.
  //     Bug fix: when exporting a PHAST model multiple times, the name of the
  //       file wasn't set appropriately.
  //     Bug fix: When exporting a MODFLOW model after previously having
  //       exported a different MODFLOW model, the default name for the model
  //       is now set correctly rather than being the same name used previously.
  //     Bug fix: Fixed access violations when importing model results
  //       into a model and contouring those results.
  //     Bug fix: When computing the size of the MODPATH composite budget file,
  //       file sizes larger than 2 GB can now be computed without causing an
  //       error.
  //   '2.7.0.2' Enhancement: Data set values can now be exported to a
  //       comma-separated value file along with X, Y, Z coordinates.
  //   '2.7.0.3' Bug fix: Renaming a data set and then attempting to use that
  //       data set in the formula for another data set no longer causes an
  //       error.
  //   '2.7.0.4' Bug fix: when exporting .emf images, the dimensions of the
  //       image were set incorrectly.
  //   '2.7.0.5' Enhancement: When data set values can are exported to a
  //       comma-separated value file, column, row, and layer numbers are
  //       exported too.
  //   '2.7.0.6' Bug fix: When a data set is first created, it was treated
  //       as being a real number data set in the Formula Editor even if its
  //       type had been changed.
  //     Bug fix: Pasting data into several of the tables could sometimes cause
  //       errors if the data that was being pasted was larger than the table
  //       could hold.
  //     Bug fix: The ActiveOnLayer function can now only be applied in a
  //       context where it will be evaluated on blocks.
  //     Enhancement: When importing heads, the water table is imported too.
  //   '2.7.0.7' Enhancement: When exporting MODPATH input files, ModelMuse
  //        now warns the user if not all time steps have been exported.
  //     Enhancement: When a new version of ModelMuse is available,
  //        the dialog box that informs the user of the new version has a
  //        button that the user can click to go to the ModelMuse web site.
  //     Enhancement: The "About" dialog box has a
  //        button that the user can click to go to the ModelMuse web site.
  //     Bug fix: Eliminated a range check error that could sometimes occur
  //        if user moved the mouse while importing model results.
  //     Enhancement: If the user specifies a head observation with a blank
  //        observation name, an error message is generated during export of
  //        the head observations file.
  //     Enhancement: The Grid Value dialog box now allows the user to see
  //       the value of any data set instead of just the one that is being
  //       used to color the grid or whose values have been contoured.
  //   '2.7.0.8' Bug fix: When importing model results, the legend on the
  //       Color Grid or Contour Data dialog box is updated.
  //     Bug fix: When exporting an image of the side view of the model,
  //        the horizontal scale now shows the correct values.
  //   '2.7.0.9' Bug fix: fixed bug that could cause an access violation when
  //       deleting parameter in the Manage Parameters dialog box.
  //     Bug fix: If the model runs out of memory when attempting to create
  //       a new model, an error message is displayed to the user instead of
  //       sending a bug report.
  //     Bug fix: In the Start-up dialog box, if the user specifies a layer
  //       group but does not give it a name, it is skipped instead of causing
  //       and assertion failure.
  //     Bug fix: Fixed access violation in Export Object as Shapefile dialog
  //       that could occur when unchecking a check box.
  //   '2.7.0.10' Bug fix: Interpolated Vertex Value gave incorrect results
  //       if the grid was rotated.
  //   '2.7.0.11' Bug fix: The main window no longer goes behind the windows
  //       of other programs when coloring the grid.
  //     Enhancement: The Grid Value dialog box displays the vertex number
  //       and section of the selected object. at the cursor location.
  //     Bug fix: ModelMuse now displays an error message when exporting or
  //       displaying the UZF data if some data has not been defined.
  //   '2.7.0.12' Enhancement: The function Get_HufSytp has been added.
  //       It evaluates SYTP parameters in HUF.
  //   '2.7.0.13' Enhancement: The location of the grid in
  //       real world coordinates is written as a comment in the
  //       discretization file.
  //     Bug fix: Fixed reading shape files from which some shapes have been
  //       deleted.
  //     Bug fix: Fixed position of Insert and Delete buttons for the table
  //       of times for the SFR package in the Object Properties dialog box.
  //     Bug fix: Clicking the Insert buttons on the Object Properties dialog
  //       box or the MODFLOW Time dialog box could result in errors if no
  //       row in the related grid was selected.
  //     Enhancement: Reduced memory usage while reading ModelMuse file.
  //     Bug fix: Fixed evaluation of the GetHufSytp function.
  //   '2.7.0.14' Bug fix: fixed evaluation of GetHufKx when KDEP parameters
  //       are used.
  //     Bug fix: fixed importing models that use SYTP parameters in the HUF
  //       package.
  //     Bug fix: fixed evaluation of GetHuf_Interlayer_Kz when KDEP parameters
  //       are used.
  //     Bug fix: If an object has too many vertices, the Object Properties
  //       dialog box now does not display them because attempting to display
  //       them caused an access violation.
  //     Enhancement: When importing data, less memory may be used in some
  //       cases.
  //   '2.7.0.15' Bug fix: Fixed editing the head observation purpose.
  //     Enhancement: Added Natural Neighbor interpolation.
  //   '2.7.0.16' Bug fix: Fixed export of PHAST specified flux
  //     associated solution on the X face.
  //   '2.7.0.17' Bug fix: A problem with duplicate parameter instance names
  //       in MODFLOW models has been fixed.
  //   '2.8.0.0' no additional changes.

  ModelVersion = '2.8.0.0';
  StrPvalExt = '.pval';
  StrJtf = '.jtf';
  StandardLock : TDataLock = [dcName, dcType, dcOrientation, dcEvaluatedAt];
  StrHUF = 'HUF2';
  StrTop = '_Top';
  StrThickness = '_Thickness';
  StrConfinedStorageCoe = 'Confined_Storage_Coefficient';
  StrVerticalConductance = 'Vertical_Conductance';
  StrTransmissivity = 'Transmissivity';
  StrZonebudget = 'ZoneBudget';
  StrZones = 'Zones';
  StrGeostaticStress = 'Geostatic_Stress';
  StrSpecificGravitySat = 'Specific_Gravity_Saturated';
  StrSpecificGravityUns = 'Specific_Gravity_Unsaturated';
  StrInitialPreOffsets = 'Initial_Preconsolidation_Stress_Offset';
  StrInitialPreconsolida = 'Initial_Preconsolidation_Stress';
  StrFhd = '.fhd';
  StrBhd = '.bhd';
  StrFdn = '.fdn';
  StrBdn = '.bdn';
  StrCbcExt = '.cbc';
  StrHuffhd = '.huf_fhd';
  StrHufbhd = '.huf_bhd';
  StrHufflow = '.huf_flow';
  StrSubOut = '.Sub_Out';
  StrSwtOut = '.Swt_Out';
  StrSubSubOut = '.SubSubOut';
  StrSubComMlOut = '.SubComMlOut';
  StrSubComIsOut = '.SubComIsOut';
  StrSubVdOut = '.SubVdOut';
  StrSubNdCritHeadOut = '.SubNdCritHeadOut';
  StrSubDCritHeadOut = '.SubDCritHeadOut';
  StrSwtSubOut = '.SwtSubOut';
  StrSwtComMLOut = '.SwtComMLOut';
  StrSwtComIsOut = '.SwtComIsOut';
  StrSwtVDOut = '.SwtVDOut';
  StrSwtPreConStrOut = '.SwtPreConStrOut';
  StrSwtDeltaPreConStrOu = '.SwtDeltaPreConStrOut';
  StrSwtGeoStatOut = '.SwtGeoStatOut';
  StrSwtDeltaGeoStatOut = '.SwtDeltaGeoStatOut';
  StrSwtEffStressOut = '.SwtEffStressOut';
  StrSwtDeltaEffStressOu = '.SwtDeltaEffStressOut';
  StrSwtVoidRatioOut = '.SwtVoidRatioOut';
  StrSwtThickCompSedOut = '.SwtThickCompSedOut';
  StrSwtLayerCentElevOut = '.SwtLayerCentElevOut';

implementation

uses StrUtils, Dialogs, OpenGL12x, Math, frmGoPhastUnit, UndoItems,
  frmColorsUnit, GIS_Functions, ModflowDrtUnit, CustomModflowWriterUnit,
  ModflowDiscretizationWriterUnit, ModflowBasicWriterUnit,
  ModflowMultiplierZoneWriterUnit, ModflowCHD_WriterUnit, ModflowPCG_WriterUnit,
  ModflowGHB_WriterUnit, ModflowWellWriterUnit, ModflowRiverWriterUnit,
  ModflowDRN_WriterUnit, ModflowDRT_WriterUnit, ModflowRCH_WriterUnit,
  ModflowEVT_WriterUnit, ModflowETS_WriterUnit, ModflowRES_WriterUnit,
  IntListUnit, ModflowLAK_Writer, ModflowWellUnit, ModflowLPF_WriterUnit,
  ModelMuseUtilities, frmFormulaErrorsUnit, ModflowGhbUnit, ModflowEtsUnit,
  ModflowEvtUnit, ModflowRchUnit, ModflowSfrWriterUnit, ModflowSfrUnit,
  ModflowSfrReachUnit, ModflowSfrFlows, ModflowSfrChannelUnit,
  ModflowSfrEquationUnit, ModflowSfrSegment, ModflowSfrUnsatSegment,
  frmModflowPackagesUnit, ModflowUzfWriterUnit, ModflowGMG_WriterUnit,
  ModflowSIP_WriterUnit, ModflowDE4_WriterUnit, ModflowOC_Writer,
  ModflowGAG_WriterUnit, ModflowHOB_WriterUnit, ModflowUzfUnit,
  ModflowHFB_WriterUnit, frmProgressUnit, ModpathStartingLocationsWriter,
  ModpathMainFileWriterUnit, ModpathTimeFileWriterUnit,
  ModpathResponseFileWriterUnit, ModpathNameFileWriterUnit, Clipbrd,
  GlobalTypesUnit, DependentsUnit, ModelMateUtilities, GlobalBasicData,
  AbZipper, AbArcTyp, PriorInfoUnit, frmErrorsAndWarningsUnit,
  ModflowHUF_WriterUnit, ModflowKDEP_WriterUnit, ModflowLVDA_WriterUnit,
  ModflowMNW2_WriterUnit, ModflowBCF_WriterUnit, ModflowSubsidenceDefUnit,
  ModflowSUB_Writer, ZoneBudgetWriterUnit, MODFLOW_SwtWriterUnit,
  ModflowHydmodWriterUnit, IniFileUtilities, TempFiles;

resourcestring
  StrProgramLocations = 'Program Locations';
  StrMODFLOW2005 = 'MODFLOW-2005';
  StrTextEditor = 'Text Editor';
  StrMODPATH = 'MODPATH';
  StrModelMonitor = 'ModelMonitor';
  StrMpathDefaultPath = 'C:\WRDAPP\Mpath.5_0\setup\Mpathr5_0.exe';
  StrModflowDefaultPath = 'C:\WRDAPP\MF2005.1_8\Bin\mf2005.exe';
  StrModelMonitorDefaultPath = 'ModelMonitor.exe';
  StrPHAST = 'PHAST';
  StrPhastDefaultPath = 'C:\Program Files\USGS\phast-1.5.1\bin\phast.bat';
  StrZoneBudgetDefaultPath = 'C:\WRDAPP\Zonbud.3_01\Bin\zonbud.exe';
  StrModelMateDefaultPath = 'C:\WRDAPP\ModelMate_0_23_1\Bin\ModelMate.exe';
  StrModelMate = 'ModelMate';
  StrAnyTimesAfterThe = 'Any times after the end of the last defined stress ' +
  'period will be ignored.';
  StrAnyTimesBeforeThe = 'Any times before the beginning of the first define' +
  'd stress period will be ignored.';
const
  StrAndNegatedAtCons = ' and negated at constant head cell';
  StrAndMadePositiveA = ' and made positive at constant head cell';
  StrValueOfZeroConver = 'Value of zero converted to 1 at active cell.';
  StrNonzeroValueOfZe = 'Non-zero value converted to 0 at inactive cell.';

const
  StatFlagStrings : array[Low(TStatFlag)..High(TStatFlag)] of string
    = ('VAR', 'SD', 'CV', 'WT', 'SQRWT');



const
  UcodeDelimiter = '@';

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

function GenerateNewName(Root: string = 'NewDataSet';
  InvalidNames: TStringList = nil; Connector: string = ''): string;
var
  Names: TStringList;
  Index: integer;
  DataSet: TDataArray;
  DataArrayManager: TDataArrayManager;
  GlobalVariable: TGlobalVariable;
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
    if InvalidNames <> nil then
    begin
      Names.AddStrings(InvalidNames);
    end;

    DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
    for Index := 0 to DataArrayManager.DataSetCount - 1 do
    begin
      DataSet := DataArrayManager.DataSets[Index];
      Names.Add(DataSet.Name);
    end;

    // Don't allow the name to be the same as a deleted data set.
    for Index := 0 to DataArrayManager.FDeletedDataSets.Count - 1 do
    begin
      DataSet := DataArrayManager.FDeletedDataSets[Index];
      Names.Add(DataSet.Name);
    end;

    // Names now includes the names of all the data sets.

    // don't allow the name to be the same as a global variable.
    for Index := 0 to frmGoPhast.PhastModel.GlobalVariables.Count -1 do
    begin
      GlobalVariable := frmGoPhast.PhastModel.GlobalVariables[Index];
      Names.Add(GlobalVariable.Name);
    end;

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

  

function StrToStatFlag(const StatFlagString: string): TStatFlag;
var
  Index: TStatFlag;
begin
  if StatFlagString = '' then
  begin
    result := Low(TStatFlag);
  end
  else
  begin
    result := Low(TStatFlag);
    for Index := Low(TStatFlag) to High(TStatFlag) do
    begin
      if StatFlagString = StatFlagStrings[Index] then
      begin
        result := Index;
        Exit;
      end;
    end;
    Assert(False);
  end;
end;

  { TPhastModel }

function TPhastModel.AddScreenObject(const AScreenObject: TScreenObject):
  integer;
begin
  result := FScreenObjectList.Add(AScreenObject);
  Invalidate;
end;

procedure TPhastModel.Assign(Source: TPersistent);
var
  SourceModel: TPhastModel;
begin
  if Source is TPhastModel then
  begin
    SourceModel := TPhastModel(Source);
    Diffusivity := SourceModel.Diffusivity;
    GridOptions := SourceModel.GridOptions;
    PhastGrid := SourceModel.PhastGrid;
    PrintFrequency := SourceModel.PrintFrequency;
    PrintInitial := SourceModel.PrintInitial;
    SolutionOptions := SourceModel.SolutionOptions;
    SteadyFlowOptions := SourceModel.SteadyFlowOptions;
    Times := SourceModel.Times;
    Title := SourceModel.Title;
    Units := SourceModel.Units;
    PathLine := SourceModel.PathLine;
    EndPoints := SourceModel.EndPoints;
    TimeSeries := SourceModel.TimeSeries;
    Bitmaps := SourceModel.Bitmaps;
    Exaggeration := SourceModel.Exaggeration;
    ObjectList := SourceModel.ObjectList;
    Version := SourceModel.Version;
    GuiSettings := SourceModel.GuiSettings;
    ModelMateProjectFileName := SourceModel.ModelMateProjectFileName;
    DisplaySettings := SourceModel.DisplaySettings;
    ModflowSteadyParameters := SourceModel.ModflowSteadyParameters;
    ModelSelection := SourceModel.ModelSelection;
    LayerStructure := SourceModel.LayerStructure;
    ModflowOptions := SourceModel.ModflowOptions;
    ModflowStressPeriods := SourceModel.ModflowStressPeriods;
    SoluteTransport := SourceModel.SoluteTransport;
    UseWaterTable := SourceModel.UseWaterTable;
    FreeSurface := SourceModel.FreeSurface;
    ChemistryOptions := SourceModel.ChemistryOptions;
    HufParameters := SourceModel.HufParameters;
    ObservationPurpose := SourceModel.ObservationPurpose;
    ModflowTransientParameters := SourceModel.ModflowTransientParameters;
    ModflowOutputControl := SourceModel.ModflowOutputControl;
    DataSetList := SourceModel.DataSetList;
  end;
  inherited;

end;

procedure TPhastModel.BeginScreenObjectUpdate;
begin
  Inc(FScreenObjectUpdateCount);
end;

constructor TPhastModel.Create(AnOwner: TComponent);
begin
  inherited;
  FChildModels := TChildModelCollection.Create(self);
  FDisplaySettings := TDisplaySettingsCollection.Create(self);
  FColorLegend := TLegend.Create(nil);
  FColorLegend.ValueAssignmentMethod := vamAutomatic;
  FContourLegend := TLegend.Create(nil);
  FContourLegend.ValueAssignmentMethod := vamAutomatic;


  FHufParameters := THufModflowParameters.Create(self);
  FFormulaManager:= TFormulaManager.Create;
  FUpdatingFullStressPeriods := False;

  FGridColors := TColorParameters.Create;
  FGridColors.OnChange := NotifyGridColorsChanged;

  FContourColors := TColorParameters.Create;

  FDataArrayManager.DefinePackageDataArrays;

  FSelectedScreenObjectCount := 0;
  FClearing := False;


  FProgramLocations := TProgramLocations.Create;

  FCachedScreenObjectIndex := -1;


  FModflowOptions := TModflowOptions.Create(self);

  FLayerStructure := TLayerStructure.Create(self);
  FBitmaps := TCompressedBitmapCollection.Create;
  ThreeDGridObserver.OnUpToDateSet := ModflowGrid.NotifyGridChanged;

  FDiffusivity := 1E-9;
  FModelTimes := TRealList.Create;
  FFileVersion := ModelVersion;

  FTopBoundaryType := TIntegerSparseDataSet.Create(self);
  FTopBoundaryType.Name := 'FTopBoundaryType';
  FTopBoundaryType.Orientation := dso3D;
  FTopBoundaryType.EvaluatedAt := eaNodes;
  (FTopBoundaryType as TIntegerSparseDataSet).BoundaryTypeDataSet :=
    FTopBoundaryType;

  FFrontBoundaryType := TIntegerSparseDataSet.Create(self);
  FFrontBoundaryType.Name := 'FFrontBoundaryType';
  FFrontBoundaryType.Orientation := dso3D;
  FFrontBoundaryType.EvaluatedAt := eaNodes;
  (FFrontBoundaryType as TIntegerSparseDataSet).BoundaryTypeDataSet :=
    FFrontBoundaryType;

  FSideBoundaryType := TIntegerSparseDataSet.Create(self);
  FSideBoundaryType.Name := 'FSideBoundaryType';
  FSideBoundaryType.Orientation := dso3D;
  FSideBoundaryType.EvaluatedAt := eaNodes;
  (FSideBoundaryType as TIntegerSparseDataSet).BoundaryTypeDataSet :=
    FSideBoundaryType;

  FTop2DBoundaryType := TIntegerSparseDataSet.Create(self);
  FTop2DBoundaryType.Name := 'FTop2DBoundaryType';
  FTop2DBoundaryType.Orientation := dsoTop;
  FTop2DBoundaryType.EvaluatedAt := eaNodes;
  (FTop2DBoundaryType as TIntegerSparseDataSet).BoundaryTypeDataSet :=
    FTop2DBoundaryType;

  FScreenObjectList := TObjectList.Create;
  FPhastGrid := TPhastGrid.Create;
  FDataSetCollection := TDataSetCollection.Create;
  FScreenObjectCollection := TScreenObjectCollection.Create(self);
  FFluidProperties := TFluidProperties.Create(self);
  FFluidProperties.SetSubComponent(True);
  FSolutionOptions := TSolutionOptions.Create(self);
  FSolutionOptions.SetSubComponent(True);
  FPrintFrequency := TPrintFrequencyCollection.Create(self);
  FPrintInitial := TPrintInitial.Create;
  FGridOptions := TGridOptions.Create;
  FSteadyFlowOptions := TSteadyFlowOptions.Create;
  FChemistryOptions := TChemistryOptions.Create;

  FPrintFrequency.Add;
  FTitle := TStringList.Create;
  FTitle.Add('PHAST input generated by ' + StrModelName + '.');
  FUnits := TUnits.Create;
  FTimes := TTimeCollection.Create(self);
  FTimes.Add;

  FModflowStressPeriods := TModflowStressPeriods.Create(self);
  FModflowFullStressPeriods := TModflowStressPeriods.Create(nil);
  with FModflowStressPeriods.Add as TModflowStressPeriod do
  begin
    StartTime := -1;
    PeriodLength := 1;
    TimeStepMultiplier := 1;
    MaxLengthOfFirstTimeStep := 1;
    DrawDownReference := False;
  end;

  FModflowOutputControl := TModflowOutputControl.Create(Self);

  FModflowSteadyParameters:= TModflowSteadyParameters.Create(Self);
  FModflowTransientParameters := TModflowTransientListParameters.Create(self);

  CreateModflowDisplayTimeLists;

  FDataArrayManager.CreateInitialDataSets;
  FDataArrayManager.CreateInitialBoundaryDataSets;

  CreatePhastTimeLists;
  CreateInitialDataSetsForPhastTimeLists;
  CreatePhastTimeListGroups;

//  ModelSelection := msPhast;
  FDataArrayManager.CreateInitialDataSets;

end;

procedure TPhastModel.CreateArchive(const FileName: string);
var
  ArchiveFiles: TStringList;
  Index: Integer;
  Zipper: TAbZipper;
  FileIndex: Integer;
  DeletedFiles: TStringList;
  ErrorMessage: string;
  RelativePath: string;
begin
  ArchiveName := FileName;
  ArchiveFiles := TStringList.Create;
  DeletedFiles := TStringList.Create;
  try
    ArchiveFiles.Add(ModelFileName);
    ArchiveFiles.AddStrings(ModelInputFiles);
    ArchiveFiles.AddStrings(FilesToArchive);
    for Index := ArchiveFiles.Count - 1 downto 0 do
    begin
      if not FileExists(ArchiveFiles[Index]) then
      begin
        DeletedFiles.Add(ArchiveFiles[Index]);
        ArchiveFiles.Delete(Index);
      end
      else
      begin
        RelativePath := ExtractRelativePath(ArchiveName,
          ArchiveFiles[Index]);
        if Copy(RelativePath, 1,2) <> '..' then
        begin
          ArchiveFiles[Index] := RelativePath;
        end;
      end;
    end;

    if ArchiveFiles.Count = 0 then
    begin
      Beep;
      MessageDlg('Sorry, no files to archive.', mtInformation, [mbOK], 0);
    end
    else
    begin

      if DeletedFiles.Count > 0 then
      begin
        if DeletedFiles.Count > 10 then
        begin
          ErrorMessage := IntToStr(DeletedFiles.Count)
            + ' of the files that were to be archived can not be found.'
            + #13#10#13#10'The remaining files will be archived.';
        end
        else
        begin
          ErrorMessage := 'The following files that '
            + 'were to be archived can not be found: '#13#10#13#10
            + DeletedFiles.Text
            + #13#10'The remaining files will be archived.';
        end;
        MessageDlg(ErrorMessage, mtWarning, [mbOK], 0);
      end;
      Zipper := TAbZipper.Create(nil);
      try
        Zipper.FileName := ArchiveName;
        Zipper.BaseDirectory := ExtractFileDir(ArchiveName);
        Zipper.StoreOptions := Zipper.StoreOptions + [soReplace];
        for FileIndex := 0 to ArchiveFiles.Count - 1 do
        begin
          Zipper.AddFiles(ArchiveFiles[FileIndex], 0);
        end;
        try
        Zipper.Save;
        except
          on E: EFOpenError do
          begin
            Beep;
            MessageDlg(E.Message, mtError, [mbOK], 0);
          end;
          on E: EInvalidCast do
          begin
            // If the size of the archive exceeds the maximum size of a
            // long integer, TAbZipper doesn't handle it properly in
            // TAbZipArchive.SaveArchive and raises an EInvalidCast.
            Beep;
            MessageDlg('Error saving archive.  The archive may be too big.',
              mtError, [mbOK], 0);
          end;
        end;
      finally
        Zipper.Free;
      end;
    end;
  finally
    ArchiveFiles.Free;
    DeletedFiles.Free;
  end;
end;

procedure TPhastModel.GetLayerGroupDataSets(LayerGroupsDataSets: TList);
var
  DataSet: TDataArray;
  LayerGroup: TLayerGroup;
  Index: Integer;
begin
  if (LayerStructure <> nil) and (LayerStructure.Count > 0) then
  begin
    for Index := 0 to LayerStructure.Count - 1 do
    begin
      LayerGroup := LayerStructure.LayerGroups[Index];
      DataSet := FDataArrayManager.GetDataSetByName(LayerGroup.DataArrayName);
      Assert(DataSet <> nil);
      LayerGroupsDataSets.Add(DataSet);
    end;
  end;
end;

function TPhastModel.GetLayerStructure: TLayerStructure;
begin
  result := FLayerStructure;
end;

type
  TScreenObjectCrack = class(TScreenObject);

procedure TPhastModel.UpdateScreenObjects;
var
  Index: integer;
  AScreenObject: TScreenObjectCrack;
begin
  FScreenObjectCollection.UpdateScreenObjects;
  FScreenObjectCollection.Clear;
  for Index := 0 to ScreenObjectCount - 1 do
  begin
    AScreenObject := TScreenObjectCrack(ScreenObjects[Index]);
    AScreenObject.Loaded;
  end;

  UpdateDrainReturnObjects;
end;

destructor TPhastModel.Destroy;
var
  Index: Integer;
  DataArray: TDataArray;
  Variable: TGlobalVariable;
  ScreenObject: TScreenObject;
begin
  frmFileProgress:= TfrmProgressMM.Create(nil);
  try
    DataArrayManager.UnlinkDeletedDataSets;
    FClearing := True;
    try
      InternalClear;
    finally
      FClearing := False;
    end;

    FChildModels.Free;
    FTimeSeries.Free;
    FEndPoints.Free;
    FPathLine.Free;
    FHufParameters.Free;

    FModelMateProject.Free;

    FreeAndNil(FSortedObjectList);

    FModflowTransientParameters.Free;
    FModflowSteadyParameters.Free;
    FModflowOutputControl.Free;
    FModflowFullStressPeriods.Free;
    FModflowStressPeriods.Free;


    frmFileProgress.pbProgress.Position := 0;
    frmFileProgress.pbProgress.Max := FDataArrayManager.DataSetCount + GlobalVariables.Count
      + ScreenObjectCount;
    frmFileProgress.Show;

    for Index := 0 to FDataArrayManager.DataSetCount - 1 do
    begin
      DataArray := FDataArrayManager.DataSets[Index];
      DataArray.StopTalkingToAnyone;
      frmFileProgress.pbProgress.StepIt;
//      Application.ProcessMessages;
    end;



    FTopBoundaryType.StopTalkingToAnyone;
    FFrontBoundaryType.StopTalkingToAnyone;
    FSideBoundaryType.StopTalkingToAnyone;
    FTop2DBoundaryType.StopTalkingToAnyone;

    FModflowOptions.Free;

    for Index := 0 to GlobalVariables.Count - 1 do
    begin
      Variable := GlobalVariables[Index];
      Variable.StopTalkingToAnyone;
      frmFileProgress.pbProgress.StepIt;
      // calling Application.ProcessMessages during
      // destuction of TPhastModel can cause access violations
      // because the program attempts to draw the model
      // while it is partially destroyed.
//      Application.ProcessMessages;
    end;

    for Index := 0 to ScreenObjectCount - 1 do
    begin
      ScreenObject := ScreenObjects[Index];
      ScreenObject.StopTalkingToAnyone;
      frmFileProgress.pbProgress.StepIt;
//      Application.ProcessMessages;
    end;
    AllObserversStopTalking;
    FreeHufNotifiers;


    FLayerStructure.Free;
    FScreenObjectList.Free;
    FreeGridNotifiers;

    PhastGrid.Free;
    FDataSetCollection.Free;

    FScreenObjectCollection.Free;

    FPrintFrequency.Free;
    FPrintInitial.Free;
    FGridOptions.Free;
    FFluidProperties.Free;
    FSolutionOptions.Free;
    FSteadyFlowOptions.Free;
    FChemistryOptions.Free;
    FTitle.Free;
    FUnits.Free;
    FTimes.Free;

    FTopFluxBoundaryFlux.Free;
    FFrontFluxBoundaryFlux.Free;
    FSideFluxBoundaryFlux.Free;
    FTopFluxBoundaryChemistry.Free;
    FFrontFluxBoundaryChemistry.Free;
    FSideFluxBoundaryChemistry.Free;

    FTopLeakyHead.Free;
    FTopLeakyAssociatedSolution.Free;

    FFrontLeakyHead.Free;
    FFrontLeakyAssociatedSolution.Free;

    FSideLeakyHead.Free;
    FSideLeakyAssociatedSolution.Free;

    FRiverAssociatedSolution.Free;
    FRiverHead.Free;

    FSpecifiedHeadHead.Free;
    FSpecifiedHeadAssociatedSolution.Free;

    FSpecifiedSolution.Free;
    FWellInjectionOrPumpingRate.Free;
    FWellSolution.Free;
    FModelTimes.Free;

    FTopBoundaryType.Free;
    FFrontBoundaryType.Free;
    FSideBoundaryType.Free;
    FTop2DBoundaryType.Free;

    FWellSolutionGroup.Free;
    FSpecifiedHeadGroup.Free;
    FSpecifiedHeadSolutionGroup.Free;
    FRiverHeadGroup.Free;
    FLeakyAssociatedSolutionGroup.Free;
    FRiverAssociatedSolutionGroup.Free;
    FWellPumpingRateGroup.Free;
    FFluxBoundaryChemistryGroup.Free;
    FLeakyHeadGroup.Free;
    FFluxBoundaryFluxGroup.Free;

    FBitmaps.Free;

    FGuiSettings.Free;

    FProgramLocations.Free;



    FContourColors.Free;
    FGridColors.Free;


    FFormulaManager.Free;
    FColorLegend.Free;
    FContourLegend.Free;
    FDisplaySettings.Free;
  finally
    FreeAndNil(frmFileProgress);
  end;

  inherited;
end;

procedure TPhastModel.FinalizeActive(Sender: TObject);
var
  LakeIdArray: TDataArray;
  ActiveArray: TDataArray;
begin
  LakeIdArray := FDataArrayManager.GetDataSetByName(rsLakeID);
  ActiveArray := FDataArrayManager.GetDataSetByName(rsActive);
  if (LakeIdArray <> nil) and (ActiveArray <> nil) then
  begin
    LakeIdArray.StopsTalkingTo(ActiveArray);
  end;
end;

procedure TPhastModel.FinalizeLakeId(Sender: TObject);
begin
  FinalizeActive(Sender);
  FinalizeWetDry(Sender);
end;

function TPhastModel.GetPathLine: TPathLineReader;
begin
  if (FPathLine = nil) then
  begin
    FPathLine := TPathLineReader.Create;
  end;
  result := FPathLine;
end;

function TPhastModel.GetProgramLocations: TProgramLocations;
begin
  result := FProgramLocations;
end;

procedure TPhastModel.FinalizeWetDry(Sender: TObject);
var
  LakeIdArray: TDataArray;
  WetDryArray: TDataArray;
begin
  LakeIdArray := FDataArrayManager.GetDataSetByName(rsLakeID);
  WetDryArray := FDataArrayManager.GetDataSetByName(rsWetDryFlag);
  if (LakeIdArray <> nil) and (WetDryArray <> nil) then
  begin
    LakeIdArray.StopsTalkingTo(WetDryArray);
  end;
end;

function TPhastModel.GetScreenObjectCount: integer;
begin
  if FScreenObjectList = nil then
  begin
    result := 0
  end
  else
  begin
    result := FScreenObjectList.Count;
  end;

end;

function TPhastModel.GetScreenObjects(const Index: integer): TScreenObject;
begin
  result := FScreenObjectList[Index];
end;

function TPhastModel.HufDataArrayUsed(Sender: TObject): boolean;
var
  DataArray: TDataArray;
  DataArrayNames: TStringList;
begin
  if not ModflowPackages.HufPackage.IsSelected then
  begin
    result := False;
    Exit;
  end;
  DataArray := Sender as TDataArray;
  DataArrayNames := TStringList.Create;
  try
    HydrogeologicUnits.FillDataArrayNames(DataArrayNames);
    DataArrayNames.CaseSensitive := False;
    result := DataArrayNames.IndexOf(DataArray.Name) >= 0;
  finally
    DataArrayNames.Free;
  end;
end;

function TPhastModel.ModelLayerDataArrayUsed(Sender: TObject): boolean;
var
  Index: Integer;
  Group: TLayerGroup;
  DataArray: TDataArray;
begin
  result := (ModelSelection in [msModflow, msModflowLGR]);
  if result then
  begin
    DataArray := Sender as TDataArray;
    for Index := 0 to LayerStructure.Count - 1 do
    begin
      Group := LayerStructure[Index];
      result := (Group.DataArrayName = DataArray.Name);
      if result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TPhastModel.LocateNearestLakeOrStream(TestScreenObject: TScreenObject;
  var NearestLake, NearestStream: TScreenObject; Tolerance: double = 0);
var
  TestDist: Double;
  TestLocation: TPoint2D;
  AScreenObject: TScreenObject;
  Index: Integer;
  Dist: Double;
  OutFlowLocation: TPoint2D;
begin
  OutFlowLocation := TestScreenObject.Points[TestScreenObject.Count - 1];
  NearestStream := nil;
  Dist := 0;
  for Index := 0 to ScreenObjectCount - 1 do
  begin
    AScreenObject := ScreenObjects[Index];
    if not AScreenObject.Deleted and (AScreenObject <> TestScreenObject)
      and (AScreenObject.ModflowSfrBoundary <> nil)
      and AScreenObject.ModflowSfrBoundary.Used then
    begin
      TestLocation := AScreenObject.Points[0];
      if NearestStream = nil then
      begin
        TestDist := Distance(TestLocation, OutFlowLocation);
        if Tolerance > 0 then
        begin
          if TestDist < Tolerance then
          begin
            Dist := TestDist;
            NearestStream := AScreenObject;
          end;
        end
        else
        begin
          Dist := TestDist;
          NearestStream := AScreenObject;
        end;
      end
      else
      begin
        TestDist := Distance(TestLocation, OutFlowLocation);
        if TestDist < Dist then
        begin
          if (Tolerance > 0) then
          begin
            if TestDist < Tolerance then
            begin
              Dist := TestDist;
              NearestStream := AScreenObject;
            end;
          end
          else
          begin
            Dist := TestDist;
            NearestStream := AScreenObject;
          end;
        end;
      end;
    end;
  end;
  NearestLake := nil;
  if ModflowPackages.LakPackage.IsSelected then
  begin
    for Index := 0 to ScreenObjectCount - 1 do
    begin
      AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
      if not AScreenObject.Deleted
        and (AScreenObject <> TestScreenObject)
        and (AScreenObject.ModflowLakBoundary <> nil)
        and AScreenObject.ModflowLakBoundary.Used then
      begin
        TestDist := AScreenObject.DistanceToScreenObject(
          OutFlowLocation, TestLocation, 1);
        if (NearestStream = nil) or (TestDist < Dist) then
        begin
          if Tolerance > 0 then
          begin
            if TestDist < Tolerance then
            begin
              Dist := TestDist;
              NearestLake := AScreenObject;
            end;
          end
          else
          begin
            Dist := TestDist;
            NearestLake := AScreenObject;
          end;
        end;
      end;
    end;
  end;
  if NearestLake <> nil then
  begin
    NearestStream := nil;
  end;
end;

procedure TPhastModel.InternalClear;
var
  Index: Integer;
  DataSet: TDataArray;
  ScreenObject: TScreenObject;
  Variable: TGlobalVariable;
begin
    Bitmaps.Clear;
    ClearViewedItems;

    if PhastGrid <> nil then
    begin
      if PhastGrid.TopGridObserver <> nil then
      begin
        PhastGrid.TopGridObserver.StopTalkingToAnyone;
      end;
      if PhastGrid.ThreeDGridObserver <> nil then
      begin
        PhastGrid.ThreeDGridObserver.StopTalkingToAnyone;
      end;
    end;
    if ModflowGrid <> nil then
    begin
      if ModflowGrid.TopGridObserver <> nil then
      begin
        ModflowGrid.TopGridObserver.StopTalkingToAnyone;
      end;
      if ModflowGrid.ThreeDGridObserver <> nil then
      begin
        ModflowGrid.ThreeDGridObserver.StopTalkingToAnyone;
      end;
    end;

    FLayerStructure.StopTalkingToAnyone;
    AllObserversStopTalking;

    FCachedZoneArrayIndex := -1;
    FCachedMultiplierArrayIndex := -1;
    ModflowNameFileLines.Clear;
    Title.Clear;
    FrontX := 0;
    FrontY := 0;
    TopX := 0;
    TopY := 0;
    SideX := 0;
    SideY := 0;
    MagnificationFront := 0;
    MagnificationSide := 0;
    MagnificationTop := 0;
    GridColorParameters.Clear;
    ContourColorParameters.Clear;
    FDiffusivity := 1E-9;
    FFreeSurface := False;
    FDiffusivitySet := False;
    for Index := 0 to GlobalVariables.Count - 1 do
    begin
      Variable := GlobalVariables[Index];
      Variable.StopTalkingToAnyone;
    end;
    for Index := 0 to ScreenObjectCount - 1 do
    begin
      ScreenObject := ScreenObjects[Index];
      ScreenObject.StopTalkingToAnyone;
    end;
    for Index := 0 to FDataArrayManager.DataSetCount - 1 do
    begin
      DataSet := FDataArrayManager.DataSets[Index];
      DataSet.StopTalkingToAnyone;
    end;
    for Index := 0 to FDataArrayManager.BoundaryDataSetCount - 1 do
    begin
      DataSet := FDataArrayManager.BoundaryDataSets[Index];
      DataSet.StopTalkingToAnyone;
    end;

    ModelSelection := msUndefined;

    FScreenObjectList.Clear;
    FDataArrayManager.ClearAllDataSets;
    ClearParsers;
//    FDataSetFunctions.Clear;

    FModflowOptions.Clear;

    //  FluidProperties.Initialize;
    SolutionOptions.Initialize;
    PhastGrid.GridAngle := 0;
    ModflowGrid.GridAngle := 0;
    FLayerStructure.Clear;
    Times.Initialize;

    ModflowPackages.Reset;
    ModflowSteadyParameters.Clear;
    ModflowTransientParameters.Clear;
    GlobalVariables.Clear;
    ModflowOutputControl.Initialize;

    HydrogeologicUnits.Clear;
    FHufParameters.Clear;

    HeadFluxObservations.Clear;
    DrainObservations.Clear;
    GhbObservations.Clear;
    RiverObservations.Clear;
    FilesToArchive.Clear;
    ModelFileName := '';
    ModelInputFiles.Clear;
    FDataArrayManager.InvalidateDataSetLookupList;

    BatchFileAdditionsBeforeModel.Clear;
    BatchFileAdditionsAfterModel.Clear;
    AlternateFlowPackage := False;
    AlternateSolver := False;

    FormulaManager.Clear;
    FDisplaySettings.Clear;

    FreeAndNil(FPathline);
    FreeAndNil(FEndPoints);
    FreeAndNil(FTimeSeries);

    Invalidate;
  inherited;
end;

procedure TPhastModel.ExtractScreenObject(const AScreenObject: TScreenObject);
begin
  FScreenObjectList.Extract(AScreenObject);
  Invalidate;
end;

procedure TPhastModel.InsertScreenObject(const Index: integer;
  const AScreenObject: TScreenObject);
begin
  FScreenObjectList.Insert(Index, AScreenObject);
  Invalidate;
end;

function TPhastModel.GetObservationPurpose: TObservationPurpose;
begin
  result := FObservationPurpose;
end;

function TPhastModel.GetOwnsScreenObjects: boolean;
begin
  Result := (FScreenObjectList as TObjectList).OwnsObjects;
end;

procedure TPhastModel.SetOwnsScreenObjects(const Value: boolean);
begin
  (FScreenObjectList as TObjectList).OwnsObjects := Value;
end;

procedure TPhastModel.ClearScreenObjectCollection;
begin
  FScreenObjectCollection.Clear;
end;

procedure TPhastModel.ClearScreenObjects;
begin
  FScreenObjectList.Clear;
  Invalidate;
end;

procedure TPhastModel.RemoveScreenObject(const AScreenObject: TScreenObject);
begin
  FScreenObjectList.Remove(AScreenObject);
  Invalidate;
end;

function TPhastModel.IndexOfScreenObject(const AScreenObject: TScreenObject):
  integer;
begin
  if (FCachedScreenObjectIndex >= 0) and
    (FCachedScreenObjectIndex < FScreenObjectList.Count) then
  begin
    if FScreenObjectList[FCachedScreenObjectIndex] = AScreenObject then
    begin
      result := FCachedScreenObjectIndex;
      Exit;
    end;
  end;
  Inc(FCachedScreenObjectIndex);
  if (FCachedScreenObjectIndex >= 0) and
    (FCachedScreenObjectIndex < FScreenObjectList.Count) then
  begin
    if FScreenObjectList[FCachedScreenObjectIndex] = AScreenObject then
    begin
      result := FCachedScreenObjectIndex;
      Exit;
    end;
  end;
  Dec(FCachedScreenObjectIndex, 2);
  if (FCachedScreenObjectIndex >= 0) and
    (FCachedScreenObjectIndex < FScreenObjectList.Count) then
  begin
    if FScreenObjectList[FCachedScreenObjectIndex] = AScreenObject then
    begin
      result := FCachedScreenObjectIndex;
      Exit;
    end;
  end;
  result := FScreenObjectList.IndexOf(AScreenObject);
  FCachedScreenObjectIndex := result;
end;

procedure TPhastModel.SetPathLine(const Value: TPathLineReader);
begin
  if FPathLine = nil then
  begin
    FPathLine :=  TPathLineReader.Create;
  end;
  FPathLine.Assign(Value);
end;

procedure TPhastModel.SetPhastGrid(const Value: TPhastGrid);
begin
  FPhastGrid.Assign(Value);
  Invalidate;
end;

function TPhastModel.GetHeight: integer;
begin
  if GuiSettings = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GuiSettings.Height;
  end;
end;

function TPhastModel.GetHufParameters: THufModflowParameters;
begin
  result := FHufParameters;
end;

function TPhastModel.GetLeft: integer;
begin
  if GuiSettings = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GuiSettings.Left;
  end;
end;

function TPhastModel.GetTop: integer;
begin
  if GuiSettings = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GuiSettings.Top;
  end;
end;

function TPhastModel.GetWidth: integer;
begin
  if GuiSettings = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GuiSettings.Width;
  end;
end;

procedure TPhastModel.SetHeight(const Value: integer);
begin
  if GuiSettings <> nil then
  begin
    GuiSettings.Height := Value
  end;
end;

procedure TPhastModel.SetHufParameters(const Value: THufModflowParameters);
begin
  FHufParameters.Assign(Value);
end;

procedure TPhastModel.SetLayerStructure(const Value: TLayerStructure);
begin
  FLayerStructure.Assign(Value);
end;

procedure TPhastModel.SetLeft(const Value: integer);
begin
  if GuiSettings <> nil then
  begin
    GuiSettings.Left := Value
  end;
end;

procedure TPhastModel.SetTop(const Value: integer);
begin
  if GuiSettings <> nil then
  begin
    GuiSettings.Top := Value
  end;
end;

procedure TPhastModel.SetWidth(const Value: integer);
begin
  if GuiSettings <> nil then
  begin
    GuiSettings.Width := Value
  end;
end;

function TPhastModel.GetMagnificationFront: double;
begin
  if GuiSettings = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GuiSettings.MagnificationFront;
  end;
end;

function TPhastModel.GetMagnificationSide: double;
begin
  if GuiSettings = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GuiSettings.MagnificationSide;
  end;
end;

function TPhastModel.GetMagnificationTop: double;
begin
  if GuiSettings = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GuiSettings.MagnificationTop;
  end;
end;

function TPhastModel.PhastUsed(Sender: TObject): boolean;
begin
  result := ModelSelection = msPhast;
end;

function TPhastModel.StoreChildModels: Boolean;
begin
  result := FChildModels.Count > 0;  
end;

function TPhastModel.StoreEndPoints: Boolean;
begin
  result := (FEndPoints <> nil) and (FEndPoints.FileName <> '');
end;

function TPhastModel.StorePathLine: Boolean;
begin
  result := (FPathLine <> nil) and (FPathLine.FileName <> '');
end;

function TPhastModel.StoreTimeSeries: Boolean;
begin
  result := (FTimeSeries <> nil) and (FTimeSeries.FileName <> '');
end;

function TPhastModel.GetModelSelection: TModelSelection;
begin
  result := FModelSelection;
end;

function TPhastModel.GetModflowFullStressPeriods: TModflowStressPeriods;
begin
  result := FModflowFullStressPeriods;
end;

function TPhastModel.GetModflowOptions: TModflowOptions;
begin
  result := FModflowOptions;
end;

function TPhastModel.GetModflowOutputControl: TModflowOutputControl;
begin
  result := FModflowOutputControl;
end;

function TPhastModel.GetModflowSteadyParameters: TModflowSteadyParameters;
begin
  result := FModflowSteadyParameters;
end;

function TPhastModel.GetModflowStressPeriods: TModflowStressPeriods;
begin
  result := FModflowStressPeriods;
end;

function TPhastModel.GetModflowTransientParameters: TModflowTransientListParameters;
begin
  result := FModflowTransientParameters;
end;

procedure TPhastModel.UpdateUseList(DataIndex: integer;
  NewUseList: TStringList; Item: TCustomModflowBoundaryItem);
var
  Formula: string;
  TempUseList: TStringList;
  VariableIndex: Integer;
  ScreenObject: TScreenObject;
begin
  Formula := Item.BoundaryFormula[DataIndex];
  try
    rpThreeDFormulaCompiler.Compile(Formula);
  except on E: ErbwParserError do
    begin
      ScreenObject := Item.ScreenObject as TScreenObject;
      frmFormulaErrors.AddError(ScreenObject.Name, StrModflowSfrReachLength,
        Formula, E.Message);
      Formula := '0';
      rpThreeDFormulaCompiler.Compile(Formula);
    end;
  end;
  TempUseList := rpThreeDFormulaCompiler.CurrentExpression.VariablesUsed;
  for VariableIndex := 0 to TempUseList.Count - 1 do
  begin
    if NewUseList.IndexOf(TempUseList[VariableIndex]) < 0 then
    begin
      NewUseList.Add(TempUseList[VariableIndex]);
    end;
  end;
end;

procedure TPhastModel.UpdateDisplayUseList(NewUseList: TStringList;
  ParamType: TParameterType; DataIndex: integer; const DisplayName: string);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Item: TCustomModflowBoundaryItem;
  ValueIndex: Integer;
  ParamIndex: Integer;
  ParamItem: TModflowParamItem;
  Boundary: TModflowParamBoundary;
begin
  for ScreenObjectIndex := 0 to ScreenObjectCount - 1 do
  begin
    ScreenObject := ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.GetMfBoundary(ParamType);
    if (Boundary <> nil) and Boundary.Used then
    begin
      for ValueIndex := 0 to Boundary.Values.Count -1 do
      begin
        Item := Boundary.Values[ValueIndex];
        UpdateUseList(DataIndex, NewUseList, Item);
      end;
      for ParamIndex := 0 to Boundary. Parameters.Count - 1 do
      begin
        ParamItem := Boundary.Parameters[ParamIndex];
        for ValueIndex := 0 to ParamItem.Param.Count - 1 do
        begin
          Item := ParamItem.Param.Items[ValueIndex]
            as TCustomModflowBoundaryItem;
          UpdateUseList(DataIndex, NewUseList, Item);
        end;
      end;
    end;
  end;
end;

procedure TPhastModel.SetMagnificationFront(Value: double);
begin
  if GuiSettings <> nil then
  begin
    GuiSettings.MagnificationFront := Value
  end;
end;

procedure TPhastModel.SetMagnificationSide(Value: double);
begin
  if GuiSettings <> nil then
  begin
    GuiSettings.MagnificationSide := Value
  end;
end;

procedure TPhastModel.SetMagnificationTop(Value: double);
begin
  if GuiSettings <> nil then
  begin
    GuiSettings.MagnificationTop := Value
  end;
end;

procedure TPhastModel.SetModelMateProject(const Value: TProject);
begin
  if FModelMateProject <> Value then
  begin
    if Value = nil then
    begin
      FreeAndNil(FModelMateProject)
    end
    else
    begin
      if FModelMateProject = nil then
      begin
          FModelMateProject := TProject.Create(nil);
      end;
      FModelMateProject.Assign(Value);
    end;
  end;

end;

procedure TPhastModel.SetModelMateProjectFileName(const Value: string);
begin
  if FModelMateProjectFileName <> Value then
  begin
    FModelMateProjectFileName := Value;
    Invalidate;
  end;
end;

procedure TPhastModel.SetModelSelection(const Value: TModelSelection);
var
  Index: Integer;
  DataSet: TDataArray;
begin
  if FModelSelection <> Value then
  begin
    EdgeDisplay := nil;
    case FModelSelection of
      msUndefined:
        begin
          // ignore
        end;
      msPhast:
        begin
          PhastGrid.TopGridObserver := nil;
          PhastGrid.ThreeDGridObserver := nil;
        end;
      msModflow, msModflowLGR:
        begin
          ModflowGrid.TopGridObserver := nil;
          ModflowGrid.ThreeDGridObserver := nil;
        end;
      else Assert(False);
    end;
    FModelSelection := Value;
    case FModelSelection of
      msUndefined:
        begin
          PhastGrid.TopGridObserver := nil;
          PhastGrid.ThreeDGridObserver := nil;
          ModflowGrid.TopGridObserver := nil;
          ModflowGrid.ThreeDGridObserver := nil;
          FGrid := nil;
        end;
      msPhast:
        begin
          FGrid := PhastGrid;
        end;
      msModflow, msModflowLGR:
        begin
          FGrid := ModflowGrid;
        end;
      else Assert(False);
    end;
    if Grid <> nil then
    begin
      Grid.TopGridObserver := TopGridObserver;
      Grid.ThreeDGridObserver := ThreeDGridObserver;
    end;
    if Assigned(OnModelSelectionChange) then
    begin
      OnModelSelectionChange(self);
    end;
    AddGIS_Functions(rpTopFormulaCompiler, FModelSelection, eaBlocks);
    AddGIS_Functions(rpFrontFormulaCompiler, FModelSelection, eaBlocks);
    AddGIS_Functions(rpSideFormulaCompiler, FModelSelection, eaBlocks);
    AddGIS_Functions(rpThreeDFormulaCompiler, FModelSelection, eaBlocks);
    AddGIS_Functions(rpTopFormulaCompilerNodes, FModelSelection, eaNodes);
    AddGIS_Functions(rpFrontFormulaCompilerNodes, FModelSelection, eaNodes);
    AddGIS_Functions(rpSideFormulaCompilerNodes, FModelSelection, eaNodes);
    AddGIS_Functions(rpThreeDFormulaCompilerNodes, FModelSelection, eaNodes);

    UpdateDataArrayParameterUsed;

    if Grid <> nil then
    begin
      Grid.GridChanged;
    end;
    if not (csReading in ComponentState) and not (csDestroying in ComponentState) then
    begin
      FDataArrayManager.CreateInitialDataSets;
    end;


    for Index := 0 to FDataArrayManager.DataSetCount - 1 do
    begin
      DataSet := FDataArrayManager.DataSets[Index] as TDataArray;
      if Grid <> nil then
      begin
        DataSet.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
          Grid.ColumnCount);
      end
      else
      begin
        DataSet.UpdateDimensions(-1, -1, -1);
      end;
    end;
    InvalidateScreenObjects;
    Invalidate;
  end;
end;

procedure TPhastModel.SetModflowOptions(const Value: TModflowOptions);
begin
  FModflowOptions.Assign(Value);
end;

procedure TPhastModel.SetModflowOutputControl(
  const Value: TModflowOutputControl);
begin
  FModflowOutputControl.Assign(Value);
end;

procedure TPhastModel.SetModflowSteadyParameters(
  const Value: TModflowSteadyParameters);
begin
  FModflowSteadyParameters.Assign(Value);
  UpdateDataArrayParameterUsed;
end;

procedure TPhastModel.SetModflowStressPeriods(
  const Value: TModflowStressPeriods);
var
  InvalidateTransients: boolean;
  Index: Integer;
  OldStressPeriod: TModflowStressPeriod;
  NewStressPeriod: TModflowStressPeriod;
begin
  InvalidateTransients := FModflowStressPeriods.Count <> Value.Count;
  if not InvalidateTransients then
  begin
    for Index := 0 to FModflowStressPeriods.Count - 1 do
    begin
      OldStressPeriod := FModflowStressPeriods[Index];
      NewStressPeriod := Value[Index];
      InvalidateTransients :=
        (OldStressPeriod.StartTime <> NewStressPeriod.StartTime)
        or (OldStressPeriod.EndTime <> NewStressPeriod.EndTime);
      if InvalidateTransients then
      begin
        break;
      end;
    end;
  end;
  FModflowStressPeriods.Assign(Value);
  if InvalidateTransients then
  begin
    InvalidateModflowBoundaries;
  end;
end;

procedure TPhastModel.SetModflowTransientParameters(
  const Value: TModflowTransientListParameters);
begin
  FModflowTransientParameters.Assign(Value);
end;

function TPhastModel.GetEndPoints: TEndPointReader;
begin
  if (FEndPoints = nil) then
  begin
    FEndPoints := TEndPointReader.Create;
  end;
  result := FEndPoints;
end;

function TPhastModel.GetExaggeration: double;
begin
  result := 1;
  if frmGoPhast.frameFrontView <> nil then
  begin
    result := frmGoPhast.frameFrontView.ZoomBox.Exaggeration;
  end
  else if frmGoPhast.frameSideView <> nil then
  begin
    result := frmGoPhast.frameSideView.ZoomBox.Exaggeration;
  end;
end;

procedure TPhastModel.SetEndPoints(const Value: TEndPointReader);
begin
  if FEndPoints = nil then
  begin
    FEndPoints :=  TEndPointReader.Create;
  end;
  FEndPoints.Assign(Value);
end;

procedure TPhastModel.SetExaggeration(Value: double);
begin
  if Value <= 0 then
  begin
    Value := 1;
  end;
  if Exaggeration <> Value then
  begin
    if frmGoPhast.frameFrontView <> nil then
    begin
      frmGoPhast.frameFrontView.ZoomBox.Exaggeration := Value;
    end;
    if frmGoPhast.frameSideView <> nil then
    begin
      frmGoPhast.frameSideView.ZoomBox.Exaggeration := Value;
    end;
    frmGoPhast.PhastGrid.GridChanged;
    frmGoPhast.ModflowGrid.GridChanged;
  end;
end;

function TPhastModel.GetFreeSurface: boolean;
begin
  result := FFreeSurface;
end;

function TPhastModel.GetFrontHeight: integer;
begin
  if GuiSettings = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GuiSettings.FrontHeight;
  end;
end;

function TPhastModel.GetFrontX: double;
begin
  if GuiSettings = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GuiSettings.FrontX;
  end;
end;

function TPhastModel.GetFrontY: double;
begin
  if GuiSettings = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GuiSettings.FrontY;
  end;
end;

function TPhastModel.GetSideX: double;
begin
  if GuiSettings = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GuiSettings.SideX;
  end;
end;

function TPhastModel.GetSideY: double;
begin
  if GuiSettings = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GuiSettings.SideY;
  end;
end;

function TPhastModel.GetSoluteTransport: boolean;
begin
  result := FSoluteTransport;
end;

function TPhastModel.GetTopX: double;
begin
  if GuiSettings = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GuiSettings.TopX;
  end;
end;

function TPhastModel.GetTopY: double;
begin
  if GuiSettings = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GuiSettings.TopY;
  end;
end;

procedure TPhastModel.SetFrontHeight(Value: integer);
begin
  if GuiSettings <> nil then
  begin
    GuiSettings.FrontHeight := Value;
  end;
end;

procedure TPhastModel.SetFrontX(const Value: double);
begin
  if GuiSettings <> nil then
  begin
    GuiSettings.FrontX := Value;
  end;
end;

procedure TPhastModel.SetFrontY(const Value: double);
begin
  if GuiSettings <> nil then
  begin
    GuiSettings.FrontY := Value;
  end;
end;

procedure TPhastModel.SetSideX(const Value: double);
begin
  if GuiSettings <> nil then
  begin
    GuiSettings.SideX := Value
  end;
end;

procedure TPhastModel.SetSideY(const Value: double);
begin
  if GuiSettings <> nil then
  begin
    GuiSettings.SideY := Value
  end;
end;

procedure TPhastModel.SetTopX(const Value: double);
begin
  if GuiSettings <> nil then
  begin
    GuiSettings.TopX := Value
  end;
end;

procedure TPhastModel.SetTopY(const Value: double);
begin
  if GuiSettings <> nil then
  begin
    GuiSettings.TopY := Value
  end;
end;

function TPhastModel.GetTopViewHeight: integer;
begin
  if GuiSettings = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GuiSettings.TopViewHeight;
  end;
end;

function TPhastModel.GetTopViewWidth: integer;
begin
  if GuiSettings = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GuiSettings.TopViewWidth;
  end;
end;

procedure TPhastModel.SetTopViewHeight(const Value: integer);
begin
  if GuiSettings <> nil then
  begin
    GuiSettings.TopViewHeight := Value
  end;
end;

procedure TPhastModel.SetTopViewWidth(const Value: integer);
begin
  if GuiSettings <> nil then
  begin
    GuiSettings.TopViewWidth := Value
  end;
end;

function TPhastModel.GetVersion: string;
begin
  result := ModelVersion;
end;

procedure TPhastModel.SetVersion(const Value: string);
begin
  FFileVersion := Value;
end;

procedure TPhastModel.UpdateActive(Sender: TObject);
const
  SpecifiedHeadComment = 'All specified head cells are active';
var
  LakeIdArray: TDataArray;
  ActiveArray: TDataArray;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  IsLake: boolean;
  SpecifiedHeadArray: TDataArray;
  LakeComment: string;
begin
  if (ModelSelection in [msMODFLOW, msModflowLGR])
    and ModflowPackages.ChdBoundary.IsSelected then
  begin
    SpecifiedHeadArray := FDataArrayManager.GetDataSetByName(rsModflowSpecifiedHead);
    if SpecifiedHeadArray <> nil then
    begin
      SpecifiedHeadArray.Initialize;
      ActiveArray := FDataArrayManager.GetDataSetByName(rsActive);
      Assert(ActiveArray <> nil);

      for ColIndex := 0 to ModflowGrid.ColumnCount - 1 do
      begin
        for RowIndex := 0 to ModflowGrid.RowCount - 1 do
        begin
          for LayerIndex := 0 to ModflowGrid.LayerCount - 1 do
          begin
            if SpecifiedHeadArray.BooleanData[LayerIndex,RowIndex,ColIndex]
              and not ActiveArray.BooleanData[LayerIndex,RowIndex,ColIndex] then
            begin
              ActiveArray.BooleanData[LayerIndex,RowIndex,ColIndex] := True;
              ActiveArray.Annotation[LayerIndex,RowIndex,ColIndex] := SpecifiedHeadComment;
            end;
          end;
        end;
      end;
    end;
  end;
  if (ModelSelection in [msMODFLOW, msModflowLGR])
    and ModflowPackages.LakPackage.IsSelected  then
  begin
    LakeIdArray := FDataArrayManager.GetDataSetByName(rsLakeID);
    ActiveArray := FDataArrayManager.GetDataSetByName(rsActive);
    Assert(LakeIdArray <> nil);
    Assert(ActiveArray <> nil);
    LakeIdArray.Initialize;
    LakeComment := 'All Lake cells are inactive: (' + rsLakeID + ' <> 0)';
    for ColIndex := 0 to ModflowGrid.ColumnCount - 1 do
    begin
      for RowIndex := 0 to ModflowGrid.RowCount - 1 do
      begin
        IsLake := False;
        for LayerIndex := ModflowGrid.LayerCount -1 downto 0 do
        begin
          if LakeIdArray.IntegerData[LayerIndex,RowIndex,ColIndex] <> 0 then
          begin
            IsLake := True;
          end;
          if IsLake then
          begin
            ActiveArray.BooleanData[LayerIndex,RowIndex,ColIndex] := False;
            ActiveArray.Annotation[LayerIndex,RowIndex,ColIndex] := LakeComment;
          end;
        end;
      end;
    end;
    FDataArrayManager.AddDataSetToCache(LakeIdArray);
  end;
end;

procedure TPhastModel.UpdateDataArrayParameterUsed;
begin
  if not (csLoading in ComponentState) and not FClearing then
  begin
    UpdateLpfDataArrayParameterUsed(rsKx, ptLPF_HK);
    UpdateLpfDataArrayParameterUsed(rsHorizontalAnisotropy, ptLPF_HANI);
    UpdateLpfDataArrayParameterUsed(rsSpecific_Storage, ptLPF_SS);
    UpdateLpfDataArrayParameterUsed(rsSpecificYield, ptLPF_SY);
    // ideally, the ParameterFormula for the Kz data set should
    // incorporate the vertical anisotropy and the confining bed formulas
    // However, doing so is difficult and has not been done.
    //
    // The user may not have defined any ptLPF_VK parameters but
    // may have defined some ptLPF_VKCB or ptLPF_VANI parameters.
    // In such cases, the user should be able to set Kz using objects
    // for some layers but not others. TDataArray.Initialize
    // won't allow that.
    UpdateLpfDataArrayParameterUsed(rsKz, ptLPF_VK);
    UpdateLpfDataArrayParameterUsed(rsModflow_CBKz, ptLPF_VKCB);
    UpdateLpfDataArrayParameterUsed(rsVerticalAnisotropy, ptLPF_VANI);
  end;
end;

procedure TPhastModel.UpdateDataSets;
const
  OldLongDispersivityName = 'Long_Dispersivity';
  OldHorizontal_Transv_Dispersivity = 'Horizontal_Transv_Dispersivity';
  OldVertical_Transv_Dispersivity = 'Vertical_Transv_Dispersivity';
var
  Index: integer;
  ADataSet: TDataArray;
  Item: TDataSetItem;
  DataArray: TDataArray;
  ExistingDataSet: TDataArray;
  TempCompiler: TRbwParser;
  ParamItem: TModflowSteadyParameter;
  SearchName: string;
  Compiler: TRbwParser;
begin
  FDataArrayManager.DataSetsCapacity := FDataSetCollection.Count;
  for Index := 0 to FDataSetCollection.Count - 1 do
  begin
    Item := FDataSetCollection.Items[Index] as TDataSetItem;
    ADataSet := Item.FDataSet;
    Item.UpdateDataSet;

    SearchName := ADataSet.Name;
    if SearchName = OldLongDispersivityName then
    begin
      SearchName := rsLong_Dispersivity;
    end
    else if SearchName = OldHorizontal_Transv_Dispersivity then
    begin
      SearchName := rsHorizontal_Transv_Dispersivity;
    end
    else if SearchName = OldVertical_Transv_Dispersivity then
    begin
      SearchName := rsVertical_Transv_Dispersivity;
    end;

    ExistingDataSet := FDataArrayManager.GetDataSetByName(SearchName);

    if ExistingDataSet = nil then
    begin
      if ADataSet.Name <> SearchName then
      begin
        ADataSet.Name := SearchName;
      end;
      FDataArrayManager.AddDataSet(ADataSet);
      CreateVariables(ADataSet);
      ADataSet.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
        Grid.ColumnCount);
    end
    else
    begin
      ExistingDataSet.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
        Grid.ColumnCount);

      TempCompiler := GetCompiler(ExistingDataSet.Orientation,
        ExistingDataSet.EvaluatedAt);
      if TempCompiler.IndexOfVariable(ExistingDataSet.Name) < 0 then
      begin
        CreateVariables(ExistingDataSet);
      end;
    end;
  end;
  // The formulas can not be specified until all the data sets have
  // been updated.
  for Index := 0 to FDataSetCollection.Count - 1 do
  begin
    Item := FDataSetCollection.Items[Index] as TDataSetItem;
    ADataSet := Item.FDataSet;
    SearchName := ADataSet.Name;
    if SearchName = OldLongDispersivityName then
    begin
      SearchName := rsLong_Dispersivity;
    end
    else if SearchName = OldHorizontal_Transv_Dispersivity then
    begin
      SearchName := rsHorizontal_Transv_Dispersivity;
    end
    else if SearchName = OldVertical_Transv_Dispersivity then
    begin
      SearchName := rsVertical_Transv_Dispersivity;
    end;
    ExistingDataSet := FDataArrayManager.GetDataSetByName(SearchName);
    Assert(ExistingDataSet <> nil);
    ExistingDataSet.Assign(ADataSet);
    Compiler := GetCompiler(ExistingDataSet.Orientation, ExistingDataSet.EvaluatedAt);
    Compiler.Compile(Item.FDataSetFormula);
    ExistingDataSet.Formula := Item.FDataSetFormula;
    if ExistingDataSet is TCustomPhastDataSet then
    begin
      TCustomPhastDataSet(ExistingDataSet).MixtureFormula := Item.FMixtureFormula;
    end;
    if ExistingDataSet.Classification = StrLayerDefinition then
    begin
      ExistingDataSet.OnDataSetUsed := ModelLayerDataArrayUsed;
    end
    else if ExistingDataSet.Classification = StrHUF then
    begin
      ExistingDataSet.OnDataSetUsed := HufDataArrayUsed;
    end;
    if (ADataSet.TwoDInterpolator <> nil)
      and (ExistingDataSet.TwoDInterpolator = nil) then
    begin
      ExistingDataSet.TwoDInterpolator := ADataSet.TwoDInterpolator;
    end;
  end;
  for Index := 0 to FDataSetCollection.Count - 1 do
  begin
    Item := FDataSetCollection.Items[Index] as TDataSetItem;
    ADataSet := Item.FDataSet;
    if not DataArrayManager.DataArrayHeld(ADataSet) then
    begin
      ADataSet.Free;
    end;
  end;

  FDataSetCollection.Clear;

  FDataArrayManager.CreateInitialDataSets;

  for Index := 0 to ModflowSteadyParameters.Count - 1 do
  begin
    ParamItem := ModflowSteadyParameters.Items[Index];
    if ParamItem.UseMultiplier then
    begin
      DataArray := FDataArrayManager.GetDataSetByName(ParamItem.MultiplierName);
      if DataArray <> nil then
      begin
        DataArray.OnDataSetUsed := ParameterDataSetUsed;
      end;
    end;
    if ParamItem.UseZone then
    begin
      DataArray := FDataArrayManager.GetDataSetByName(ParamItem.ZoneName);
      if DataArray <> nil then
      begin
        DataArray.OnDataSetUsed := ParameterDataSetUsed;
      end;
    end;
  end;
end;

function TPhastModel.GetScreenObjectByName(AName: string): TScreenObject;
var
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  result := nil;
  if FSortedObjectList = nil then
  begin
    FSortedObjectList := TLookUpList.Create;
    for Index := 0 to ScreenObjectCount - 1 do
    begin
      ScreenObject := ScreenObjects[Index];
      if not ScreenObject.Deleted then
      begin
        FSortedObjectList.AddObject(ScreenObject.Name, ScreenObject)
      end;
    end;
    FSortedObjectList.CaseSensitive := False;
    FSortedObjectList.Sorted := True;
  end;
  if (FSortedObjectList.FLastIndex >= 0)
    and (FSortedObjectList.FLastIndex < FSortedObjectList.Count) then
  begin
    if AnsiCompareText(FSortedObjectList[FSortedObjectList.FLastIndex],
      AName) = 0 then
    begin
      result := FSortedObjectList.Objects[FSortedObjectList.FLastIndex] as TScreenObject;
      Exit;
    end;
  end;
  FSortedObjectList.FLastIndex := FSortedObjectList.IndexOf(AName);
  if FSortedObjectList.FLastIndex >= 0 then
  begin
    result := FSortedObjectList.Objects[FSortedObjectList.FLastIndex] as TScreenObject;
  end;
end;

function TPhastModel.GetScreenObjectCollection: TScreenObjectCollection;
var
  Index: integer;
  Item: TScreenObjectItem;
  AScreenObject: TScreenObject;
begin
  FScreenObjectCollection.Clear;

  for Index := 0 to ScreenObjectCount - 1 do
  begin
    AScreenObject := ScreenObjects[Index];
    if not AScreenObject.Deleted then
    begin
      Item := FScreenObjectCollection.Add as TScreenObjectItem;
      Item.SetScreenObject(AScreenObject);
    end;
  end;

  result := FScreenObjectCollection;
end;

procedure TPhastModel.SetFreeSurface(const Value: boolean);
begin
  if FFreeSurface <> Value then
  begin
    FFreeSurface := Value;
    Invalidate;
  end;
end;

procedure TPhastModel.SetPrintFrequency(const Value: TPrintFrequencyCollection);
begin
  FPrintFrequency.Assign(Value);
end;

procedure TPhastModel.SetProgramLocations(const Value: TProgramLocations);
begin
  FProgramLocations.Assign(Value);
end;

procedure TPhastModel.SetTitle(const Value: TStrings);
begin
  FTitle.Assign(Value);
  Invalidate;
end;

procedure TPhastModel.SetTimes(const Value: TTimeCollection);
begin
  FTimes.Assign(Value);
  Invalidate;
end;

procedure TPhastModel.SetTimeSeries(const Value: TTimeSeriesReader);
begin
  if FTimeSeries = nil then
  begin
    FTimeSeries := TTimeSeriesReader.Create
  end;
  FTimeSeries.Assign(Value);
end;

function TPhastModel.ScreenObjectClass: TScreenObjectClass;
begin
  result := TScreenObject;
end;

procedure TPhastModel.ScreenObjectsChanged(Sender: TObject);
begin
  if FScreenObjectUpdateCount > 0 then Exit;
  
  if Assigned(OnScreenObjectsChanged) then
  begin
    OnScreenObjectsChanged(Sender);
  end;
end;

procedure TPhastModel.RefreshScreenObjects(Sender: TObject);
begin
  if Assigned(OnRefreshScreenObjects) then
  begin
    OnRefreshScreenObjects(Sender);
  end;
end;

procedure TPhastModel.RegisterGlobalVariables(Parser: TRbwParser);
var
  Variable: TGlobalVariable;
  VariableIndex: Integer;
begin
  for VariableIndex := 0 to GlobalVariables.Count - 1 do
  begin
    Variable := GlobalVariables[VariableIndex];
    case Variable.Format of
      rdtDouble:
        begin
          Parser.CreateVariable(Variable.Name, StrGlobalVariables, Variable.RealValue);
        end;
      rdtInteger:
        begin
          Parser.CreateVariable(Variable.Name, StrGlobalVariables, Variable.IntegerValue);
        end;
      rdtBoolean:
        begin
          Parser.CreateVariable(Variable.Name, StrGlobalVariables, Variable.BooleanValue);
        end;
      rdtString:
        begin
          Parser.CreateVariable(Variable.Name, StrGlobalVariables, Variable.StringValue);
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TPhastModel.ScreenObjectSelected;
begin
  Inc(FSelectedScreenObjectCount);
  if Assigned(OnScreenObjectSelected) then
  begin
    OnScreenObjectSelected(self);
  end;
end;

procedure TPhastModel.ScreenObjectUnSelected;
begin
  Dec(FSelectedScreenObjectCount);
  if Assigned(OnScreenObjectUnSelected) then
  begin
    OnScreenObjectUnSelected(self);
  end;
end;

function TPhastModel.GetSideWidth: integer;
begin
  if GuiSettings = nil then
  begin
    result := 0;
  end
  else
  begin
    result := GuiSettings.SideWidth;
  end;
end;

procedure TPhastModel.SetSideWidth(const Value: integer);
begin
  if GuiSettings <> nil then
  begin
    GuiSettings.SideWidth := Value
  end;
end;

function TPhastModel.GetArchiveName: string;
begin
  if FArchiveName = '' then
  begin
    FArchiveName := DefaultArchiveName;
  end;
  result := FArchiveName;
end;

function TPhastModel.GetChemistryOptions: TChemistryOptions;
begin
  Result := FChemistryOptions;
end;

function TPhastModel.GetCurrentScreenObject(VD: TViewDirection): TScreenObject;
begin
  if Assigned(OnGetCurrentScreenObject) then
  begin
    OnGetCurrentScreenObject(self, VD, result);
  end;
end;

function TPhastModel.GetTimeSeries: TTimeSeriesReader;
begin
  if (FTimeSeries = nil) then
  begin
    FTimeSeries := TTimeSeriesReader.Create;
  end;
  result := FTimeSeries;
end;

function TPhastModel.GetTimeListByName(const AName: string): TCustomTimeList;
var
  Index: Integer;
  AList: TCustomTimeList;
begin
  result := nil;
  for Index := 0 to TimeListCount - 1 do
  begin
    AList := TimeLists[Index];
    if AList.Name = AName then
    begin
      result := AList;
      Exit;
    end;
  end;
end;

procedure TPhastModel.UpdateFormulas(OldNames, NewNames: TStringList);
var
  CompilerList: TList;
  CompilerIndex: Integer;
  Compiler: TRbwParser;
  VariableIndex: Integer;
  VarIndex: Integer;
begin
  FormulaManager.RemoveSubscriptions(OldNames, NewNames);
  CompilerList := TList.Create;
  try
    FillCompilerList(CompilerList);
    for CompilerIndex := 0 to CompilerList.Count - 1 do
    begin
      Compiler := CompilerList[CompilerIndex];
      for VariableIndex := 0 to OldNames.Count - 1 do
      begin
        VarIndex := Compiler.IndexOfVariable(OldNames[VariableIndex]);
        if VarIndex >= 0 then
        begin
          Compiler.RenameVariable(VarIndex, NewNames[VariableIndex]);
        end;
      end;
    end;
    FormulaManager.ResetFormulas;
    for CompilerIndex := 0 to CompilerList.Count - 1 do
    begin
      Compiler := CompilerList[CompilerIndex];
      for VariableIndex := 0 to OldNames.Count - 1 do
      begin
        VarIndex := Compiler.IndexOfVariable(NewNames[VariableIndex]);
        if VarIndex >= 0 then
        begin
          Compiler.RenameVariable(VarIndex, OldNames[VariableIndex]);
        end;
      end;
    end;
  finally
    CompilerList.Free;
  end;
end;

procedure TPhastModel.UpdateFrontTimeDataSet(const TimeList: TCustomTimeList;
  const Time: double);
var
  TimeIndex: integer;
  SelectedLayer, SelectedRow, SelectedColumn: integer;
begin
  SelectedLayer := Grid.SelectedLayer;
  SelectedRow := Grid.SelectedRow;
  SelectedColumn := Grid.SelectedColumn;
  try
    if not TimeList.UpToDate then
    begin
      TimeList.Initialize;
    end;
    TimeIndex := TimeList.FirstTimeGreaterThan(Time) - 1;
    if TimeIndex < 0 then
    begin
      Grid.FrontDataSet := nil;
    end
    else
    begin
      Grid.FrontDataSet := TimeList.Items[TimeIndex];
      Grid.FrontDataSet.UpdateMinMaxValues;
    end;
    FFrontTimeList := TimeList;
    FFrontDisplayTime := Time;
  finally
    Grid.SelectedLayer := SelectedLayer;
    Grid.SelectedRow := SelectedRow;
    Grid.SelectedColumn := SelectedColumn;
  end;
end;

procedure TPhastModel.UpdateLakeId(Sender: TObject);
var
  LakeIdArray: TDataArray;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  LakeList: TIntegerList;
  LakeId, NewLakeID: integer;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
begin
  if (ModelSelection in [msMODFLOW, msModflowLGR])
    and ModflowPackages.LakPackage.IsSelected  then
  begin
    LakeList := TIntegerList.Create;
    try
      LakeList.Sorted := True;

      for ScreenObjectIndex := 0 to ScreenObjectCount - 1 do
      begin
        ScreenObject := ScreenObjects[ScreenObjectIndex];
        if ScreenObject.Deleted then
        begin
          Continue;
        end;
        if (ScreenObject.ModflowLakBoundary <> nil)
          and ScreenObject.ModflowLakBoundary.Used then
        begin
          if ScreenObject.ModflowLakBoundary.LakeID > 0 then
          begin
            LakeList.AddUnique(ScreenObject.ModflowLakBoundary.LakeID);
          end;
        end;
      end;

      LakeIdArray := FDataArrayManager.GetDataSetByName(rsLakeID);
      Assert(LakeIdArray <> nil);
      for ColIndex := 0 to ModflowGrid.ColumnCount - 1 do
      begin
        for RowIndex := 0 to ModflowGrid.RowCount - 1 do
        begin
          for LayerIndex := 0 to ModflowGrid.LayerCount -1 do
          begin
            LakeId := LakeIdArray.IntegerData[LayerIndex,RowIndex,ColIndex];
            if LakeId <> 0 then
            begin
              NewLakeID := LakeList.IndexOf(LakeId) + 1;
              if LakeId <> NewLakeID then
              begin
                LakeIdArray.IntegerData[LayerIndex,RowIndex,ColIndex]
                  := NewLakeID;
                LakeIdArray.Annotation[LayerIndex,RowIndex,ColIndex]
                  := LakeIdArray.Annotation[LayerIndex,RowIndex,ColIndex]
                  + ' and then renumbered';
              end;
            end;
          end;
        end;
      end;
    finally
      LakeList.Free;
    end;
  end;
end;

procedure TPhastModel.UpdateDischargeRouting(Sender: TObject);
var
  DischargeRoutingArray: TDataArray;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  LakeList: TIntegerList;
  DischargeId, NewLakeID, NewStreamID: integer;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  SfrList: TIntegerList;
begin
  if (ModelSelection in [msMODFLOW, msModflowLGR])
    and (ModflowPackages.LakPackage.IsSelected
    or ModflowPackages.SfrPackage.IsSelected)  then
  begin
    LakeList := TIntegerList.Create;
    SfrList := TIntegerList.Create;
    try
      LakeList.Sorted := True;
      SfrList.Sorted := True;

      for ScreenObjectIndex := 0 to ScreenObjectCount - 1 do
      begin
        ScreenObject := ScreenObjects[ScreenObjectIndex];
        if ScreenObject.Deleted then
        begin
          Continue;
        end;
        if ModflowPackages.LakPackage.IsSelected
          and (ScreenObject.ModflowLakBoundary <> nil)
          and ScreenObject.ModflowLakBoundary.Used then
        begin
          if ScreenObject.ModflowLakBoundary.LakeID > 0 then
          begin
            LakeList.AddUnique(ScreenObject.ModflowLakBoundary.LakeID);
          end;
        end;
        if ModflowPackages.SfrPackage.IsSelected
          and (ScreenObject.ModflowSfrBoundary <> nil)
          and ScreenObject.ModflowSfrBoundary.Used then
        begin
          SfrList.AddUnique(ScreenObject.ModflowSfrBoundary.SegementNumber);
        end;
      end;

      DischargeRoutingArray := FDataArrayManager.GetDataSetByName(StrUzfDischargeRouting);
      Assert(DischargeRoutingArray <> nil);
      for ColIndex := 0 to ModflowGrid.ColumnCount - 1 do
      begin
        for RowIndex := 0 to ModflowGrid.RowCount - 1 do
        begin
          LayerIndex := 0;
          DischargeId := DischargeRoutingArray.IntegerData[
            LayerIndex,RowIndex,ColIndex];
          if DischargeId < 0 then
          begin
            NewLakeID := -(LakeList.IndexOf(-DischargeId) + 1);
            if DischargeId <> NewLakeID then
            begin
              DischargeRoutingArray.IntegerData[LayerIndex,RowIndex,ColIndex]
                := NewLakeID;
              DischargeRoutingArray.Annotation[LayerIndex,RowIndex,ColIndex]
                := DischargeRoutingArray.Annotation[
                LayerIndex,RowIndex,ColIndex]
                + ' and then renumbered';
            end;
          end
          else if DischargeId > 0 then
          begin
            NewStreamID := (SfrList.IndexOf(DischargeId) + 1);
            if DischargeId <> NewStreamID then
            begin
              DischargeRoutingArray.IntegerData[LayerIndex,RowIndex,ColIndex]
                := NewStreamID;
              DischargeRoutingArray.Annotation[LayerIndex,RowIndex,ColIndex]
                := DischargeRoutingArray.Annotation[
                LayerIndex,RowIndex,ColIndex]
                + ' and then renumbered';
            end;
          end;
        end;
      end;
    finally
      LakeList.Free;
      SfrList.Free;
    end;
  end;
end;

procedure TPhastModel.UpdateOnPostInitialize;
var
  ActiveArray: TDataArray;
  WetDryArray: TDataArray;
  LakeIdArray: TDataArray;
  SpecifiedHeadArray: TDataArray;
  ModPathZoneArray: TDataArray;
begin
  LakeIdArray := FDataArrayManager.GetDataSetByName(rsLakeID);
  ActiveArray := FDataArrayManager.GetDataSetByName(rsActive);
  WetDryArray := FDataArrayManager.GetDataSetByName(rsWetDryFlag);
  SpecifiedHeadArray := FDataArrayManager.GetDataSetByName(rsModflowSpecifiedHead);
  ModPathZoneArray := FDataArrayManager.GetDataSetByName(StrModpathZone);

  Assert(ActiveArray <> nil);
  if ModflowPackages.LakPackage.IsSelected then
  begin
    if LakeIdArray <> nil then
    begin
      LakeIdArray.OnPostInitialize := UpdateLakeId;
      LakeIdArray.OnDestroy := FinalizeLakeId;
      LakeIdArray.UpToDate := False;
      if ActiveArray <> nil then
      begin
        ActiveArray.OnPostInitialize := UpdateActive;
        ActiveArray.OnDestroy := FinalizeActive;
        LakeIdArray.TalksTo(ActiveArray);
        if ModflowPackages.ChdBoundary.IsSelected then
        begin
          if SpecifiedHeadArray <> nil then
          begin
            SpecifiedHeadArray.TalksTo(ActiveArray);
          end;
        end;
        ActiveArray.UpToDate := False;
      end;
      if WetDryArray <> nil then
      begin
        WetDryArray.OnPostInitialize := UpdateWetDry;
        WetDryArray.OnDestroy := FinalizeWetDry;
        LakeIdArray.TalksTo(WetDryArray);
        WetDryArray.UpToDate := False;
      end;
    end;
  end
  else if ModflowPackages.ChdBoundary.IsSelected then
  begin
    if SpecifiedHeadArray <> nil then
    begin
      if ActiveArray <> nil then
      begin
        ActiveArray.OnPostInitialize := UpdateActive;
        ActiveArray.OnDestroy := FinalizeActive;
        SpecifiedHeadArray.TalksTo(ActiveArray);
        if LakeIdArray <> nil then
        begin
          LakeIdArray.StopsTalkingTo(ActiveArray);
        end;
        ActiveArray.UpToDate := False;
      end;
    end;
    if WetDryArray <> nil then
    begin
      WetDryArray.OnPostInitialize := nil;
      WetDryArray.OnDestroy := nil;
      if LakeIdArray <> nil then
      begin
        LakeIdArray.StopsTalkingTo(WetDryArray);
        WetDryArray.UpToDate := False;
      end;
    end;
    if LakeIdArray <> nil then
    begin
      LakeIdArray.OnPostInitialize := nil;
      LakeIdArray.OnDestroy := nil;
      LakeIdArray.UpToDate := False;
    end;
  end
  else
  begin
    if ActiveArray <> nil then
    begin
      ActiveArray.OnPostInitialize := nil;
      ActiveArray.OnDestroy := nil;
      if LakeIdArray <> nil then
      begin
        LakeIdArray.StopsTalkingTo(ActiveArray);
        ActiveArray.UpToDate := False;
      end;
    end;
    if WetDryArray <> nil then
    begin
      WetDryArray.OnPostInitialize := nil;
      WetDryArray.OnDestroy := nil;
      if LakeIdArray <> nil then
      begin
        LakeIdArray.StopsTalkingTo(WetDryArray);
        WetDryArray.UpToDate := False;
      end;
    end;
    if LakeIdArray <> nil then
    begin
      LakeIdArray.OnPostInitialize := nil;
      LakeIdArray.OnDestroy := nil;
      LakeIdArray.UpToDate := False;
    end;
  end;
  DischargeRoutingUpdate;

  if ModPathZoneArray <> nil then
  begin
    ModPathZoneArray.OnPostInitialize := UpdateModPathZone;
    Assert(ActiveArray <> nil);
    ActiveArray.TalksTo(ModPathZoneArray);
    if SpecifiedHeadArray <> nil then
    begin
      SpecifiedHeadArray.TalksTo(ModPathZoneArray);
    end;
  end;
end;

procedure TPhastModel.UpdateSideTimeDataSet(const TimeList: TCustomTimeList;
  const Time: double);
var
  TimeIndex: integer;
  SelectedLayer, SelectedRow, SelectedColumn: integer;
begin
  SelectedLayer := Grid.SelectedLayer;
  SelectedRow := Grid.SelectedRow;
  SelectedColumn := Grid.SelectedColumn;
  try
    if not TimeList.UpToDate then
    begin
      TimeList.Initialize;
    end;
    TimeIndex := TimeList.FirstTimeGreaterThan(Time) - 1;
    if TimeIndex < 0 then
    begin
      Grid.SideDataSet := nil;
    end
    else
    begin
      Grid.SideDataSet := TimeList.Items[TimeIndex];
      Grid.SideDataSet.UpdateMinMaxValues;
    end;
    FSideTimeList := TimeList;
    FSideDisplayTime := Time;
  finally
    Grid.SelectedLayer := SelectedLayer;
    Grid.SelectedRow := SelectedRow;
    Grid.SelectedColumn := SelectedColumn;
  end;
end;

procedure TPhastModel.UpdateTopTimeDataSet(const TimeList: TCustomTimeList;
  const Time: double);
var
  TimeIndex: integer;
  SelectedLayer, SelectedRow, SelectedColumn: integer;
begin
  SelectedLayer := Grid.SelectedLayer;
  SelectedRow := Grid.SelectedRow;
  SelectedColumn := Grid.SelectedColumn;
  try
    if not TimeList.UpToDate then
    begin
      TimeList.Initialize;
    end;
    TimeIndex := TimeList.FirstTimeGreaterThan(Time) - 1;
    if TimeIndex < 0 then
    begin
      Grid.TopDataSet := nil;
    end
    else
    begin
      Grid.TopDataSet := TimeList.Items[TimeIndex];
      Grid.TopDataSet.UpdateMinMaxValues;
    end;
    FTopTimeList := TimeList;
    FTopDisplayTime := Time;
  finally
    Grid.SelectedLayer := SelectedLayer;
    Grid.SelectedRow := SelectedRow;
    Grid.SelectedColumn := SelectedColumn;
  end;
end;

procedure TPhastModel.UpdateModPathZone(Sender: TObject);
var
  ModPathZoneArray: TDataArray;
  ActiveArray: TDataArray;
  SpecifiedHeadArray: TDataArray;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  NegatedString: string;
  MadePositiveString: string;
  PriorNegatedString: string;
  PriorMadePositiveString: string;
begin
  PriorNegatedString := '';
  PriorMadePositiveString := '';
  ModPathZoneArray := FDataArrayManager.GetDataSetByName(StrModpathZone);
  ActiveArray := FDataArrayManager.GetDataSetByName(rsActive);
  Assert(ActiveArray <> nil);
  ActiveArray.Initialize;
  SpecifiedHeadArray := FDataArrayManager.GetDataSetByName(rsModflowSpecifiedHead);
  if SpecifiedHeadArray <> nil then
  begin
    SpecifiedHeadArray.Initialize;
  end;
  for ColIndex := 0 to ModflowGrid.ColumnCount - 1 do
  begin
    for RowIndex := 0 to ModflowGrid.RowCount - 1 do
    begin
      for LayerIndex := ModflowGrid.LayerCount -1 downto 0 do
      begin
        if ActiveArray.BooleanData[LayerIndex,RowIndex,ColIndex] then
        begin
          if ModPathZoneArray.IntegerData[LayerIndex,RowIndex,ColIndex] = 0 then
          begin
            ModPathZoneArray.IntegerData[LayerIndex,RowIndex,ColIndex] := 1;
            ModPathZoneArray.Annotation[LayerIndex,RowIndex,ColIndex] :=
              StrValueOfZeroConver;
          end;
        end
        else if ModPathZoneArray.IntegerData[LayerIndex,RowIndex,ColIndex] <> 0
          then
        begin
          ModPathZoneArray.IntegerData[LayerIndex,RowIndex,ColIndex] := 0;
          ModPathZoneArray.Annotation[LayerIndex,RowIndex,ColIndex] :=
            StrNonzeroValueOfZe;
        end;

        if SpecifiedHeadArray <> nil then
        begin
          if SpecifiedHeadArray.BooleanData[LayerIndex,RowIndex,ColIndex] then
          begin
            if ModPathZoneArray.IntegerData[LayerIndex,RowIndex,ColIndex] > 0
              then
            begin
              ModPathZoneArray.IntegerData[LayerIndex,RowIndex,ColIndex]
                := -ModPathZoneArray.IntegerData[LayerIndex,RowIndex,ColIndex];
              NegatedString := ModPathZoneArray.
                Annotation[LayerIndex,RowIndex,ColIndex]
                + StrAndNegatedAtCons;
              if PriorNegatedString <> NegatedString then
              begin
                PriorNegatedString := NegatedString
              end;
              ModPathZoneArray.Annotation[LayerIndex,RowIndex,ColIndex] :=
                PriorNegatedString

            end;
          end
          else
          begin
            if ModPathZoneArray.IntegerData[LayerIndex,RowIndex,ColIndex] < 0
              then
            begin
              ModPathZoneArray.IntegerData[LayerIndex,RowIndex,ColIndex]
                := -ModPathZoneArray.IntegerData[LayerIndex,RowIndex,ColIndex];
              MadePositiveString := ModPathZoneArray.
                Annotation[LayerIndex,RowIndex,ColIndex]
                + StrAndMadePositiveA;
              if PriorMadePositiveString <> MadePositiveString then
              begin
                PriorMadePositiveString := MadePositiveString
              end;
              ModPathZoneArray.Annotation[LayerIndex,RowIndex,ColIndex] :=
                PriorMadePositiveString
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TPhastModel.UpdateWetDry(Sender: TObject);
var
  LakeIdArray: TDataArray;
  WetDryArray: TDataArray;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  IsLake: boolean;
begin
  if (ModelSelection in [msMODFLOW, msModflowLGR])
    and ModflowPackages.LakPackage.IsSelected  then
  begin
    LakeIdArray := FDataArrayManager.GetDataSetByName(rsLakeID);
    WetDryArray := FDataArrayManager.GetDataSetByName(rsWetDryFlag);
    Assert(LakeIdArray <> nil);
    Assert(WetDryArray <> nil);
    LakeIdArray.Initialize;
    for ColIndex := 0 to ModflowGrid.ColumnCount - 1 do
    begin
      for RowIndex := 0 to ModflowGrid.RowCount - 1 do
      begin
        IsLake := False;
        for LayerIndex := ModflowGrid.LayerCount -1 downto 0 do
        begin
          if LakeIdArray.IntegerData[LayerIndex,RowIndex,ColIndex] <> 0 then
          begin
            IsLake := True;
          end;
          if IsLake then
          begin
            WetDryArray.IntegerData[LayerIndex,RowIndex,ColIndex] := 0;
            WetDryArray.Annotation[LayerIndex,RowIndex,ColIndex] :=
              'All Lake cells are inactive: (' + rsLakeID + ' <> 0)';
          end;
        end;
      end;
    end;
    FDataArrayManager.AddDataSetToCache(LakeIdArray);
  end;
end;

function TPhastModel.ZoomBox(VD: TViewDirection): TQrbwZoomBox2;
begin
  if Assigned(OnGetZoomBox) then
  begin
    OnGetZoomBox(self, VD, result);
  end
  else
  begin
    result := nil;
  end;
end;

function TPhastModel.ResetSelectedScreenObjects: boolean;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  // Deselect all objects.
  result := False;
  for Index := 0 to ScreenObjectCount - 1 do
  begin
    AScreenObject := ScreenObjects[Index];
    if AScreenObject.Selected then
    begin
      result := True;
      AScreenObject.Selected := False;
    end;
  end;
end;

procedure TPhastModel.RestoreColoredDataSets;
var
  TimeIndex: integer;
begin
  if FTopTimeList <> nil then
  begin
    TimeIndex := FTopTimeList.FirstTimeGreaterThan(FTopDisplayTime) - 1;
    if TimeIndex < 0 then
    begin
      Grid.TopDataSet := nil;
    end
    else
    begin
      Grid.TopDataSet := FTopTimeList.Items[TimeIndex];
    end;
  end;

  if FFrontTimeList <> nil then
  begin
    TimeIndex := FFrontTimeList.FirstTimeGreaterThan(FFrontDisplayTime) - 1;
    if TimeIndex < 0 then
    begin
      Grid.FrontDataSet := nil;
    end
    else
    begin
      Grid.FrontDataSet := FFrontTimeList.Items[TimeIndex];
    end;
  end;

  if FSideTimeList <> nil then
  begin
    TimeIndex := FSideTimeList.FirstTimeGreaterThan(FSideDisplayTime) - 1;
    if TimeIndex < 0 then
    begin
      Grid.SideDataSet := nil;
    end
    else
    begin
      Grid.SideDataSet := FSideTimeList.Items[TimeIndex];
    end;
  end;

  if FThreeDTimeList <> nil then
  begin
    TimeIndex := FThreeDTimeList.FirstTimeGreaterThan(FThreeDDisplayTime) - 1;
    if TimeIndex < 0 then
    begin
      Grid.ThreeDDataSet := nil;
    end
    else
    begin
      Grid.ThreeDDataSet := FThreeDTimeList.Items[TimeIndex];
    end;
  end;
end;

procedure TPhastModel.ClearViewedItems;
begin
  inherited;
  PhastGrid.TopDataSet := nil;
  PhastGrid.FrontDataSet := nil;
  PhastGrid.SideDataSet := nil;
  PhastGrid.ThreeDDataSet := nil;
  
  PhastGrid.TopContourDataSet := nil;
  PhastGrid.FrontContourDataSet := nil;
  PhastGrid.SideContourDataSet := nil;
  PhastGrid.ThreeDContourDataSet := nil;
end;

function TPhastModel.ConvertPoint(VD: TViewDirection;
  const RealPoint: TPoint2D): TPoint;
begin
  Assert(Assigned(OnConvertPoint));
  OnConvertPoint(self, VD, RealPoint, result);
end;

procedure TPhastModel.CopyScreenObjectsToClipboard;
var
  Objects: TScreenObjectClipboard;
  Index: Integer;
  AScreenObject: TScreenObject;
  Item: TScreenObjectItem;
  MemStream: TMemoryStream;
  ClipStream: TStringStream;
begin
  inherited;
  Objects := TScreenObjectClipboard.Create(nil);
  try
    for Index := 0 to ScreenObjectCount - 1 do
    begin
      AScreenObject := ScreenObjects[Index];
      if AScreenObject.Selected then
      begin
        Item := Objects.ScreenObjects.Add as TScreenObjectItem;
        Item.SetScreenObject(AScreenObject);
      end;
    end;
    if Objects.ScreenObjects.Count > 0 then
    begin
      MemStream := TMemoryStream.Create;
      ClipStream := TStringStream.Create('');
      try
        MemStream.WriteComponent(Objects);
        MemStream.Position := 0;
        ClipStream.Position := 0;
        ObjectBinaryToText(MemStream, ClipStream);
        ClipStream.Position := 0;
        Clipboard.AsText := ClipStream.ReadString(ClipStream.Size);
      finally
        ClipStream.Free;
        MemStream.Free;
      end;
    end;
  finally
    Objects.Free;
  end;
end;

procedure TPhastModel.InitializeTimes;
begin
  ModelTimes.Clear;
  ModelTimes.Sorted := True;
  ModelTimes.Add(0);
  InitializePhastBoundaries;
  RecordTimeControl;
  Invalidate;
end;

procedure TPhastModel.RecordTimeControl;
var
  Index: integer;
  TimeItem: TTimeItem;
begin
  for Index := 0 to Times.Count - 1 do
  begin
    TimeItem := Times.Items[Index] as TTimeItem;
    ModelTimes.AddUnique(TimeItem.EndingTime);
  end;
end;

procedure TPhastModel.InitializePhastBoundaries;
var
  Index: integer;
  TimeList: TCustomTimeList;
  TimeIndex: integer;
begin
  for Index := 0 to TimeListCount - 1 do
  begin
    TimeList := TimeLists[Index];
    if not (TimeList is TPhastTimeList) then
    begin
      Continue;
    end;
    TimeList.Initialize;
    for TimeIndex := 0 to TimeList.Count - 1 do
    begin
      ModelTimes.AddUnique(TimeList.Times[TimeIndex]);
    end;
  end;
end;

type
  TComponentCrack = class(TComponent);

procedure TPhastModel.Loaded;
var
  Index: integer;
  Component: TComponentCrack;
begin
  inherited;
  for Index := 0 to ComponentCount - 1 do
  begin
    Component := TComponentCrack(Components[Index]);
    Component.Loaded;
  end;
  if not FDiffusivitySet then
  begin
    FDiffusivity := 0;
  end;

  UpdateTimeLists;

  if FrontHeight <= 0 then
  begin
    FrontHeight := 1;
  end;
  if SideWidth <= 0 then
  begin
    SideWidth := 1;
  end;
//  for Index := 0 to DataSetCount - 1 do
//  begin
//    DataSets[Index].OnNameChange := DataArrayNameChange;
//  end;
//  ClearNameChangeWarnings;
end;

procedure TPhastModel.InvalidateSegments;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  if SomeSegmentsUpToDate then
  begin
    for Index := 0 to ScreenObjectCount - 1 do
    begin
      AScreenObject := ScreenObjects[Index];
      AScreenObject.InvalidateSegments;
    end;
    SomeSegmentsUpToDate := False;
  end;
end;

function TPhastModel.IsCurrentScreenObject(
  ScreenObject: TScreenObject): boolean;
begin
  if Assigned(OnCheckScreenObject) then
  begin
    OnCheckScreenObject(self, ScreenObject, result);
  end
  else
  begin
    result := False;
  end;
end;

procedure TPhastModel.UpdateDrainReturnObjects;
var
  Index: Integer;
  SortedScreenObjectList: TStringList;
  ObjectIndex: Integer;
  ObjectName: string;
  ScreenObject: TScreenObject;
  DrainReturn: TDrainReturn;
begin
  SortedScreenObjectList := TStringList.Create;
  try
    for Index := 0 to ScreenObjectCount - 1 do
    begin
      ScreenObject := ScreenObjects[Index];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      SortedScreenObjectList.AddObject(ScreenObject.Name, ScreenObject);
    end;
    SortedScreenObjectList.Sort;
    for Index := 0 to ScreenObjectCount - 1 do
    begin
      ScreenObject := ScreenObjects[Index];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      if ScreenObject.ModflowDrtBoundary <> nil then
      begin
        DrainReturn := ScreenObject.ModflowDrtBoundary.DrainReturn;
        if DrainReturn.ReturnChoice = rtObject then
        begin
          ObjectName := DrainReturn.ReturnObject.ObjectName;
          if ObjectName <> '' then
          begin
            ObjectIndex := SortedScreenObjectList.IndexOf(ObjectName);
            if ObjectIndex >= 0 then
            begin
              DrainReturn.ReturnObject.ScreenObject :=
                SortedScreenObjectList.Objects[ObjectIndex];
            end;
          end;
        end;
      end;
    end;
  finally
    SortedScreenObjectList.Free;
  end;
end;

procedure TPhastModel.SetFlowOnly(const Value: boolean);
begin
  SoluteTransport := not Value;
end;

procedure TPhastModel.SetDiffusivity(const Value: double);
begin
  if FDiffusivity <> Value then
  begin
    FDiffusivity := Value;
    Invalidate;
  end;
  FDiffusivitySet := True;
end;

procedure TPhastModel.SetDisplaySettings(
  const Value: TDisplaySettingsCollection);
begin
  FDisplaySettings.Assign(Value);
end;

function TPhastModel.ModelResultsRequired(Sender: TObject): boolean;
begin
  result := False;
end;

procedure TPhastModel.EndScreenObjectUpdate;
begin
  Dec(FScreenObjectUpdateCount);
  ScreenObjectsChanged(nil);
end;

function TPhastModel.SubsidenceDataArrayUsed(Sender: TObject): boolean;
var
  Index: Integer;
  Group: TLayerGroup;
  DataArray: TDataArray;
  SubDataSetIndex: Integer;
  NoDelayItem: TSubNoDelayBedLayerItem;
  DelayItem: TSubDelayBedLayerItem;
  WT_Item: TSwtWaterTableItem;
begin
  result := (ModelSelection in [msMODFLOW, msModflowLGR])
    and ModflowPackages.SubPackage.IsSelected;
  if result then
  begin
    result := False;
    DataArray := Sender as TDataArray;
    for Index := 0 to LayerStructure.Count - 1 do
    begin
      Group := LayerStructure[Index];
      for SubDataSetIndex := 0 to Group.SubNoDelayBedLayers.Count - 1 do
      begin
        NoDelayItem := Group.SubNoDelayBedLayers[SubDataSetIndex];
        if (NoDelayItem.PreconsolidationHeadDataArrayName = DataArray.Name)
          or (NoDelayItem.ElasticSkeletalStorageCoefficientDataArrayName = DataArray.Name)
          or (NoDelayItem.InelasticSkeletalStorageCoefficientDataArrayName = DataArray.Name)
          or (NoDelayItem.InitialCompactionDataArrayName = DataArray.Name)
          then
        begin
          result := True;
          Exit;
        end;
      end;
      for SubDataSetIndex := 0 to Group.SubDelayBedLayers.Count - 1 do
      begin
        DelayItem := Group.SubDelayBedLayers[SubDataSetIndex];
        if (DelayItem.EquivNumberDataArrayName = DataArray.Name)
          or (DelayItem.VerticalHydraulicConductivityDataArrayName = DataArray.Name)
          or (DelayItem.ElasticSpecificStorageDataArrayName = DataArray.Name)
          or (DelayItem.InelasticSpecificStorageDataArrayName = DataArray.Name)
          or (DelayItem.InterbedStartingCompactionDataArrayName = DataArray.Name)
          or (DelayItem.InterbedEquivalentThicknessDataArrayName = DataArray.Name)
          then
        begin
          result := True;
          Exit;
        end
        else if (DelayItem.InterbedStartingHeadDataArrayName = DataArray.Name)
          or (DelayItem.InterbedPreconsolidationHeadDataArrayName = DataArray.Name)
          then
        begin
          result := ModflowPackages.SubPackage.ReadDelayRestartFileName = '';
          Exit;
        end;
      end;
    end;
  end;
  result := (ModelSelection in [msMODFLOW, msModflowLGR])
    and ModflowPackages.SwtPackage.IsSelected;
  if result then
  begin
    result := False;
    DataArray := Sender as TDataArray;
    for Index := 0 to LayerStructure.Count - 1 do
    begin
      Group := LayerStructure[Index];
      for SubDataSetIndex := 0 to Group.WaterTableLayers.Count - 1 do
      begin
        WT_Item := Group.WaterTableLayers[SubDataSetIndex];
        if (WT_Item.WaterTableCompressibleThicknessDataArrayName = DataArray.Name)
          or (WT_Item.WaterTableInitialVoidRatioDataArrayName = DataArray.Name)
          or (WT_Item.WaterTableInitialCompactionDataArrayName = DataArray.Name)
          then
        begin
          result := True;
          Exit;
        end;
        if (WT_Item.WaterTableInitialElasticSkeletalSpecificStorageDataArrayName = DataArray.Name)
          or (WT_Item.WaterTableInitialInelasticSkeletalSpecificStorageDataArrayName = DataArray.Name)
          then
        begin
          result := ModflowPackages.SwtPackage.CompressionSource = csSpecificStorage;
          Exit;
        end;
        if (WT_Item.WaterTableRecompressionIndexDataArrayName = DataArray.Name)
          or (WT_Item.WaterTableCompressionIndexDataArrayName = DataArray.Name)
          then
        begin
          result := ModflowPackages.SwtPackage.CompressionSource = csCompressionReComp;
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TPhastModel.FixOldModel;
var
  ModpathZone: TDataArray;
begin
  if (Grid.GridAngle <> 0)
    and FileVersionEqualOrEarlier('2.6.0.3')
    and (FormulaManager.FunctionUsed(StrVertexInterpolate)
    or FormulaManager.FunctionUsed(StrNodeInterpolate)) then
  begin
    // The VertexInterpolate function gave incorrect results in
    // versions '2.6.0.3' and earlier if the grid was rotated.
    // NodeInterpolate is a synonym for VertexInterpolate.
    FDataArrayManager.InvalidateAllDataSets;
  end;
  if FileVersionEqualOrEarlier('2.6.0.8') then
  begin
    // Modpath zone incorrectly limited to values >= 0 in
    // version 2.6.0.8 and earlier.
    ModpathZone := FDataArrayManager.GetDataSetByName(StrModpathZone);
    if ModpathZone <> nil then
    begin
      ModpathZone.CheckMin := False;
      ModpathZone.Invalidate;
    end;
  end;
  if (Grid.GridAngle <> 0)
    and FileVersionEqualOrEarlier('2.7.0.9')
    and FormulaManager.FunctionUsed(StrInterpolatedVertexValues) then
  begin
    // The InterpolatedVertexValue function gave incorrect results in
    // versions '2.7.0.9' and earlier if the grid was rotated.
    FDataArrayManager.InvalidateAllDataSets;
  end;
  if FileVersionEqualOrEarlier('2.7.0.13')
  and (FormulaManager.FunctionUsed(StrHufKx)
  or FormulaManager.FunctionUsed(StrHufKy)
  or FormulaManager.FunctionUsed(StrHufKz)
  or FormulaManager.FunctionUsed(StrHufSytp)) then
  begin
    FDataArrayManager.InvalidateAllDataSets;
  end;
end;

procedure TPhastModel.UpdateModelMateParameter(ParameterList: TStringList;
  ModelMuseParam: TModflowParameter; Project: TProject;
  Operation: TModelMateOperation);
const
  GroupNames : array[Low(TParameterType)..High(TParameterType)] of string
    = ('Undefined', 'HK', 'HANI', 'VK',
    'VANI', 'SS', 'SY', 'VKCB', 'RCH', 'EVT', 'ETS',
    'CHD', 'GHB', 'Q',
    'RIV', 'DRN', 'DRT', 'SFR', 'HFB',
    'HK', 'HANI', 'VK', 'VANI', 'SS', 'SY', 'SYTP', 'KDEP', 'LVDA');
var
  PIndex: integer;
  ValAttribute: TParameterAttribute;
  ModelMateParam: TParam;
  PARNAM: string;
  GroupAttribute: TParameterAttribute;
  GroupIndex: Integer;
  Group: TParam;
  FoundGroup: Boolean;
begin
  case ModelMuseParam.ParameterType of
    ptUndefined: Assert(False);
    ptLPF_HK, ptLPF_HANI, ptLPF_VK, ptLPF_VANI, ptLPF_VKCB:
      begin
        if not ModflowPackages.LpfPackage.IsSelected then
        begin
          Exit;
        end;
      end;
    ptLPF_SS,ptLPF_SY:
      begin
        if not ModflowPackages.LpfPackage.IsSelected
          or not ModflowStressPeriods.TransientModel then
        begin
          Exit;
        end;
      end;
    ptHUF_HK, ptHUF_HANI, ptHUF_VK, ptHUF_VANI, ptHUF_KDEP, ptHUF_LVDA:
      begin
        if not ModflowPackages.HufPackage.IsSelected then
        begin
          Exit;
        end;
      end;
    ptHUF_SS, ptHUF_SY, ptHUF_SYTP:
      begin
        if not ModflowPackages.HufPackage.IsSelected
          or not ModflowStressPeriods.TransientModel then
        begin
          Exit;
        end;
      end;
    ptRCH:
      begin
        if not ModflowPackages.RchPackage.IsSelected then
        begin
          Exit;
        end;
      end;
    ptEVT:
      begin
        if not ModflowPackages.EvtPackage.IsSelected then
        begin
          Exit;
        end;
      end;
    ptETS:
      begin
        if not ModflowPackages.EtsPackage.IsSelected then
        begin
          Exit;
        end;
      end;
    ptCHD:
      begin
        if not ModflowPackages.ChdBoundary.IsSelected then
        begin
          Exit;
        end;
      end;
    ptGHB:
      begin
        if not ModflowPackages.GhbBoundary.IsSelected then
        begin
          Exit;
        end;
      end;
    ptQ:
      begin
        if not ModflowPackages.WelPackage.IsSelected then
        begin
          Exit;
        end;
      end;
    ptRIV:
      begin
        if not ModflowPackages.RivPackage.IsSelected then
        begin
          Exit;
        end;
      end;
    ptDRN:
      begin
        if not ModflowPackages.DrnPackage.IsSelected then
        begin
          Exit;
        end;
      end;
    ptDRT:
      begin
        if not ModflowPackages.DrtPackage.IsSelected then
        begin
          Exit;
        end;
      end;
    ptSFR:
      begin
        if not ModflowPackages.SfrPackage.IsSelected then
        begin
          Exit;
        end;
      end;
    ptHFB:
      begin
        if not ModflowPackages.HfbPackage.IsSelected then
        begin
          Exit;
        end;
      end;
    else Assert(False);
  end;

  PARNAM := ModelMuseParam.ParameterName;
  if Length(PARNAM) > 10 then
  begin
    SetLength(PARNAM, 10);
  end;
  PIndex := ParameterList.IndexOf(PARNAM);

  ModelMateParam := nil;
  if (PIndex < 0) then
  begin
    if Operation = mmoExport then
    begin
      ModelMateParam := Project.ParamSet.Add;
      ModelMateParam.Initialize('ParamDefault','ParamDefault');
      ModelMateParam.Name := PARNAM;
      GroupAttribute := ModelMateParam.AllAtts.Items[ParAttPos(patGroupName)];
      GroupAttribute.Text := GroupNames[ModelMuseParam.ParameterType];
      FoundGroup := False;
      for GroupIndex := 0 to Project.ParGpSet.Count - 1 do
      begin
        Group := Project.ParGpSet.Items[GroupIndex];
        if SameText(Group.Name, GroupAttribute.Text) then
        begin
          FoundGroup := True;
          Break;
        end;
      end;
      if not FoundGroup then
      begin
        Group := Project.ParGpSet.Add;
        Group.Initialize(GroupAttribute.Text, GroupAttribute.Text);
      end;
    end;
  end
  else
  begin
    ModelMateParam := ParameterList.Objects[PIndex] as TParam;
  end;
  if ModelMateParam = nil then
  begin
    Exit;
  end;

  try

    ValAttribute := ModelMateParam.AllAtts.Items[ParAttPos(patStartValue)];
    case Operation of
      mmoImport:
        begin
          ModelMuseParam.Value := StrToFloatDef( ValAttribute.Text, 0);
        end;
      mmoExport:
        begin
          ValAttribute.Text := FortranFloatToStr(ModelMuseParam.Value);
        end
      else
        Assert(False);
    end;
  finally
    if PIndex >= 0 then
    begin
      ParameterList.Delete(PIndex);
    end;
  end;
end;

procedure TPhastModel.UpdateModelMateFluxObservation(
  ObservationList: TStringList; ModelMuseFluxObsGroup: TFluxObservationGroup;
  Project: TProject; Operation: TModelMateOperation);
Const
  ObservationGroupNames : array[Low(TFluxObsType)..High(TFluxObsType)] of string
    = ('CHOB_flows', 'RIV_flows', 'DRN_flows', 'GHB_flows');
var
  ModelMuseFluxObs: TFluxObservation;
  TimeIndex: Integer;
  OBSNAM: string;
  ObsIndex: Integer;
  ValAttribute: TDependentAttribute;
  ModelMateObs: TDep;
  GroupName: string;
  StatisticAttribute: TDependentAttribute;
  StatFlagAttribute: TDependentAttribute;
  DepSet: TDepSet;
begin
  DepSet := nil;
  case ObservationPurpose of
    ofObserved: DepSet :=  Project.ObsSet;
    ofPredicted: DepSet :=  Project.PredSet;
    else Assert(False);
  end;
  for TimeIndex := 0 to ModelMuseFluxObsGroup.ObservationTimes.Count - 1 do
  begin
    ModelMuseFluxObs := ModelMuseFluxObsGroup.ObservationTimes[TimeIndex];
    OBSNAM := ModelMuseFluxObsGroup.ObservationName
      + '_' + IntToStr(TimeIndex + 1);
    ObsIndex := ObservationList.IndexOf(OBSNAM);
    ModelMateObs := nil;
    if ObsIndex < 0 then
    begin
      if Operation = mmoExport then
      begin
        ModelMateObs := DepSet.Add;
        ModelMateObs.Name := OBSNAM;
        GroupName := ObservationGroupNames[ModelMuseFluxObsGroup.FluxObsType];
        ModelMateObs.AllAtts[DepAttPos(datGroupName)].Text := GroupName;

        EnsureModelMateObsGroup(Project, GroupName,
          Ord(ModelMuseFluxObsGroup.FluxObsType)+3);
      end;
    end
    else
    begin
      ModelMateObs := ObservationList.Objects[ObsIndex] as TDep;
    end;
    if ModelMateObs = nil then
    begin
      Exit;
    end;

    try
      ValAttribute := nil;
      StatisticAttribute := nil;
      StatFlagAttribute := nil;
      case ObservationPurpose of
        ofObserved:
          begin
            ValAttribute := ModelMateObs.AllAtts.Items[DepAttPos(datObsValue)];
            StatisticAttribute := ModelMateObs.AllAtts.Items[DepAttPos(datStatistic)];
            StatFlagAttribute := ModelMateObs.AllAtts.Items[DepAttPos(datStatFlag)];
          end;
        ofPredicted:
          begin
            ValAttribute := ModelMateObs.AllAtts.Items[DepAttPos(datRefValue)];
            StatisticAttribute := ModelMateObs.AllAtts.Items[DepAttPos(datMeasStatistic)];
            StatFlagAttribute := ModelMateObs.AllAtts.Items[DepAttPos(datMeasStatFlag)];
          end;
        else Assert(False);
      end;
      case Operation of
        mmoImport:
          begin
            ModelMuseFluxObs.ObservedValue := StrToFloatDef(ValAttribute.Text, 0);
            ModelMuseFluxObs.Statistic := StrToFloatDef(StatisticAttribute.Text, 0);
            ModelMuseFluxObs.StatFlag := StrToStatFlag(StatFlagAttribute.Text);
          end;
        mmoExport:
          begin
            ValAttribute.Text := FortranFloatToStr(ModelMuseFluxObs.ObservedValue);
            StatisticAttribute.Text := FortranFloatToStr(ModelMuseFluxObs.Statistic);
            StatFlagAttribute.Text := StatFlagStrings[ModelMuseFluxObs.StatFlag];
          end;
        else
          Assert(False);
      end;
    finally
      if ObsIndex >= 0 then
      begin
        ObservationList.Delete(ObsIndex);
      end;
    end;
  end;
end;

procedure TPhastModel.UpdateModelMateHeadObservation(
  ObservationList: TStringList; const OBSNAM: string;
  ModelMuseHeadObs: THobItem; Project: TProject;
  Operation: TModelMateOperation; Method: TMultiObsMethod);
const GroupNames: array[Low(TMultiObsMethod)..High(TMultiObsMethod)] of string
  = ('Heads', 'Head_Changes');
var
  ObsIndex: Integer;
  ValAttribute: TDependentAttribute;
  ModelMateObs: TDep;
  GroupName: string;
  StatAttribute: TDependentAttribute;
  StatFlagAttribute: TDependentAttribute;
  DepSet: TDepSet;
begin
  DepSet := nil;
  case ObservationPurpose of
    ofObserved: DepSet :=  Project.ObsSet;
    ofPredicted: DepSet :=  Project.PredSet;
    else Assert(False);
  end;
  ObsIndex := ObservationList.IndexOf(OBSNAM);
  ModelMateObs := nil;
  if ObsIndex < 0 then
  begin
    if Operation = mmoExport then
    begin
        ModelMateObs := DepSet.Add;
        ModelMateObs.Name := OBSNAM;

        GroupName := GroupNames[Method];
        ModelMateObs.AllAtts[DepAttPos(datGroupName)].Text := GroupName;
        EnsureModelMateObsGroup(Project, GroupName, Ord(Method)+1);
    end;
  end
  else
  begin
    ModelMateObs := ObservationList.Objects[ObsIndex] as TDep;
  end;
  if ModelMateObs = nil then
  begin
    Exit;
  end;

  try
      ValAttribute := nil;
      StatAttribute := nil;
      StatFlagAttribute := nil;
      case ObservationPurpose of
        ofObserved:
          begin
            ValAttribute := ModelMateObs.AllAtts.Items[DepAttPos(datObsValue)];
            StatAttribute := ModelMateObs.AllAtts.Items[DepAttPos(datStatistic)];
            StatFlagAttribute := ModelMateObs.AllAtts.Items[DepAttPos(datStatFlag)];
          end;
        ofPredicted:
          begin
            ValAttribute := ModelMateObs.AllAtts.Items[DepAttPos(datRefValue)];
            StatAttribute := ModelMateObs.AllAtts.Items[DepAttPos(datMeasStatistic)];
            StatFlagAttribute := ModelMateObs.AllAtts.Items[DepAttPos(datMeasStatFlag)];
          end;
        else Assert(False);
      end;
    case Operation of
      mmoImport:
        begin
          ModelMuseHeadObs.Head := StrToFloatDef(ValAttribute.Text, 0);
          ModelMuseHeadObs.Statistic := StrToFloatDef(StatAttribute.Text, 0);
          ModelMuseHeadObs.StatFlag := StrToStatFlag(StatFlagAttribute.Text);
        end;
      mmoExport:
        begin
          ValAttribute.Text := FortranFloatToStr(ModelMuseHeadObs.Head);
          StatAttribute.Text := FortranFloatToStr(ModelMuseHeadObs.Statistic);
          StatFlagAttribute.Text := StatFlagStrings[ModelMuseHeadObs.StatFlag];
        end;
      else
        Assert(False);
    end;
  finally
    if ObsIndex >= 0 then
    begin
      ObservationList.Delete(ObsIndex);
    end;
  end;
end;


procedure TPhastModel.UpdateModelMateProject;
var
  ParameterList: TStringList;
  Index: Integer;
  ModelMateParam: TParam;
  ObservationList: TStringList;
  PIndex: Integer;
  ObsIndex: Integer;
  AMessage: string;
  ModelMateObs: TDep;
  ModMatObs: TDepSet;
  DepType: string;
  ObsGroup: TDepSet;
  GroupIndex: Integer;
  Group: TDep;
  GroupUsed: Boolean;
  IniFName: string;
  IniFile: TMemIniFile;
begin
  if FileExists(ProgramLocations.ModflowLocation) then
  begin
    IniFName := IniFileName(frmGoPhast.Handle, 'ModelMate.exe');
    IniFile:= TMemInifile.Create(IniFName);
    try
      GlobalProgramLocations.ReadFromIniFile(IniFile);
      GlobalProgramLocations.Modflow2005Location
        := ProgramLocations.ModflowLocation;
      GlobalProgramLocations.WriteToIniFile(IniFile);
      IniFile.UpdateFile;
    finally
      IniFile.Free;
    end;
  end;
  case ModflowOptions.TimeUnit of
    0: ;// do nothing
    1:
      begin
        ModelMateProject.UcProject.ModelTimeUnits := 's';
      end;
    2:
      begin
        ModelMateProject.UcProject.ModelTimeUnits := 'min';
      end;
    3:
      begin
        ModelMateProject.UcProject.ModelTimeUnits := 'hr';
      end;
    4:
      begin
        ModelMateProject.UcProject.ModelTimeUnits := 'day';
      end;
    5:
      begin
        ModelMateProject.UcProject.ModelTimeUnits := 'yr';
      end;
    else Assert(False);
  end;

  case ModflowOptions.LengthUnit of
    0: ;// do nothing
    1:
      begin
        ModelMateProject.UcProject.ModelLengthUnits := 'ft';
      end;
    2:
      begin
        ModelMateProject.UcProject.ModelLengthUnits := 'm';
      end;
    3:
      begin
        ModelMateProject.UcProject.ModelLengthUnits := 'cm';
      end;
    else Assert(False);
  end;

  if ModflowOptions.ProjectName <> '' then
  begin
    ModelMateProject.UcProject.ModelName :=
      ModflowOptions.ProjectName;
  end
  else if ModelFileName <> '' then
  begin
    ModelMateProject.UcProject.ModelName :=
      ChangeFileExt(ExtractFileName(ModelFileName),'');
  end;

  ParameterList := TStringList.Create;
  try
    // parameters
    ModelMateProject.ParamSet.BeginUpdate;
    try
      HandleModelMateParameters(mmoExport, ParameterList, ModelMateProject);
      if (ParameterList.Count > 0) then
      begin
        if ParameterList.Count <= 10 then
        begin
          AMessage := 'Your ModelMate file contains the following unused parameters. '
            + 'Do you want to delete them?'#13#10#13#10
            + ParameterList.Text
        end
        else
        begin
          AMessage := 'Your ModelMate file contains '
            + IntToStr(ParameterList.Count)
            + ' unused parameters. '
            + 'Do you want to delete them?'
        end;
        if (MessageDlg(AMessage,
          mtInformation, [mbYes, mbNo], 0) = mrYes) then
        begin
          for Index := 0 to ParameterList.Count - 1 do
          begin
            ModelMateParam := ParameterList.Objects[Index] as TParam;
            for PIndex := 0 to ModelMateProject.ParamSet.Count - 1 do
            begin
              if ModelMateProject.ParamSet.Items[PIndex]
                = ModelMateParam then
              begin
                ModelMateProject.ParamSet.Delete(PIndex);
                break;
              end;
            end;
          end;
        end;
      end;
    finally
      ModelMateProject.ParamSet.EndUpdate;
    end;
  finally
    ParameterList.Free;
  end;

  DepType := '';
  ModMatObs := nil;
  ObsGroup := nil;
  ObservationList := TStringList.Create;
  try
    case ObservationPurpose of
      ofObserved:
        begin
          ModMatObs := ModelMateProject.ObsSet;
          DepType := 'observations';
          ObsGroup := ModelMateProject.ObsGpSet;
        end;
      ofPredicted:
        begin
          ModMatObs := ModelMateProject.PredSet;
          DepType := 'predictions';
          ObsGroup := ModelMateProject.PredGpSet;
        end
      else Assert(False);
    end;

    ModMatObs.BeginUpdate;
    try
      HandleModelMateObservations(mmoExport, ObservationList, ModelMateProject);

      if (ObservationList.Count > 0) then
      begin
        if ObservationList.Count <= 10 then
        begin
          AMessage := 'Your ModelMate file contains the following unused '
            + DepType + '. '
            + 'Do you want to delete them?'#13#10#13#10
            + ObservationList.Text;
        end
        else
        begin
          AMessage := 'Your ModelMate file contains '
            + IntToStr(ObservationList.Count)
            + ' unused '+ DepType + '. '
            + 'Do you want to delete them?'
        end;
        if (MessageDlg(AMessage,
          mtInformation, [mbYes, mbNo], 0) = mrYes) then
        begin
          for Index := 0 to ObservationList.Count - 1 do
          begin
            ModelMateObs := ObservationList.Objects[Index] as TDep;
            for ObsIndex := 0 to ModMatObs.Count - 1 do
            begin
              if ModMatObs.Items[ObsIndex]
                = ModelMateObs then
              begin
                ModMatObs.Delete(ObsIndex);
                break;
              end;
            end;
          end;
          for GroupIndex := ObsGroup.Count - 1 downto 0 do
          begin
            Group := ObsGroup.Items[GroupIndex];
            GroupUsed := False;
            for Index := 0 to ModMatObs.Count - 1 do
            begin
              ModelMateObs := ModMatObs.Items[Index];
              if ModelMateObs.AllAtts[DepAttPos(datGroupName)].Text = Group.Name then
              begin
                GroupUsed := True;
                break;
              end;
            end;
            if not GroupUsed then
            begin
              ObsGroup.Delete(GroupIndex);
            end;
          end;
        end;
      end;
    finally
      ModMatObs.EndUpdate;
    end;
  finally
    ObservationList.Free;
  end;
end;

function TPhastModel.FileVersionEqualOrEarlier(TestVersion: string): boolean;
var
  ProgramNum: Integer;
  SavedNum: Integer;
  VIndex: Integer;
  ProgramVersionList: TStringList;
  SavedVersionList: TStringList;
begin
  result := True;
  SavedVersionList := TStringList.Create;
  ProgramVersionList := TStringList.Create;
  try
    SavedVersionList.Delimiter := '.';
    SavedVersionList.DelimitedText := FileVersion;
    ProgramVersionList.Delimiter := '.';
    ProgramVersionList.DelimitedText := TestVersion;
    Assert(SavedVersionList.Count = 4);
    Assert(ProgramVersionList.Count = 4);
    for VIndex := 0 to SavedVersionList.Count - 1 do
    begin
      SavedNum := StrToInt(SavedVersionList[VIndex]);
      ProgramNum := StrToInt(ProgramVersionList[VIndex]);
      result := SavedNum <= ProgramNum;
      if (not result) or (SavedNum <> ProgramNum) then
      begin
        break;
      end;
    end;
  finally
    ProgramVersionList.Free;
    SavedVersionList.Free;
  end;
end;

procedure TPhastModel.SetUseWaterTable(const Value: boolean);
begin
  if FUseWaterTable <> Value then
  begin
    FUseWaterTable := Value;
    Invalidate;
  end;
end;

procedure TPhastModel.ImportFromModelMateProject(Project: TProject);
var
  ParameterList: TStringList;
  ObservationList: TStringList;
  IniFName: string;
  IniFile: TMemIniFile;
begin
  if Project.UcProject.ModelName <> '' then
  begin
    ModflowOptions.ProjectName := Project.UcProject.ModelName;
  end;

  ParameterList := TStringList.Create;
  try
    HandleModelMateParameters(mmoImport, ParameterList, Project);
  finally
    ParameterList.Free;
  end;

  ObservationList := TStringList.Create;
  try
    HandleModelMateObservations(mmoImport, ObservationList, Project);
  finally
    ObservationList.Free;
  end;

  IniFName := IniFileName(frmGoPhast.Handle, 'ModelMate.exe');
  IniFile:= TMemInifile.Create(IniFName);
  try
    GlobalProgramLocations.ReadFromIniFile(IniFile);
  finally
    IniFile.Free;
  end;

  if FileExists(GlobalProgramLocations.Modflow2005Location) then
  begin
    ProgramLocations.ModflowLocation := ExpandFileName(
      GlobalProgramLocations.Modflow2005Location);
  end;
end;

procedure TPhastModel.EnsureModelMateObsGroup(Project: TProject; GroupName: string;
  PlotSymbol: integer);
var
  FoundGroup: Boolean;
  GroupIndex: Integer;
  Group: TDep;
  IAtt: Integer;
begin
  FoundGroup := False;
  for GroupIndex := 0 to Project.ObsGpSet.Count - 1 do
  begin
    Group := Project.ObsGpSet.Items[GroupIndex];
    if SameText(Group.Name, GroupName) then
    begin
      FoundGroup := True;
      break;
    end;
  end;
  if not FoundGroup then
  begin
    Group := Project.ObsGpSet.Add;
    Group.Initialize(GroupName, GroupName, dcObs);
    Group.AllAtts[DepAttPos(datPlotSymbol)].Text := IntToStr(PlotSymbol);

    IAtt := PosDepCap('Statistic');
    Project.ObservationSetup.ObsAttributes.Items[IAtt].ControlMethod := cmByItem;
    IAtt := PosDepCap('StatFlag');
    Project.ObservationSetup.ObsAttributes.Items[IAtt].ControlMethod := cmByItem;
  end;
end;

procedure TPhastModel.HandleModelMateObservations(
  Operation: TModelMateOperation;
  ObservationList: TStringList; Project: TProject);
var
  Observations: THobBoundary;
  ScreenObject: TScreenObject;
  ScreenObjectIndex: Integer;
  ModelMuseFluxObsGroup: TFluxObservationGroup;
  ModelMateObs: TDep;
  ValueIndex: Integer;
  OBSNAM: string;
  ModelMuseHeadObs: THobItem;
  Index: Integer;
  DepSet: TDepSet;
  Method: TMultiObsMethod;
  ObsVersion: string;
begin
  // flux observations
  ObservationList.CaseSensitive := False;

  DepSet := nil;
  case ObservationPurpose of
    ofObserved: DepSet := Project.ObsSet;
    ofPredicted: DepSet := Project.PredSet;
    else Assert(False);
  end;
  for Index := 0 to DepSet.Count - 1 do
  begin
    ModelMateObs := DepSet.Items[Index];
    ObservationList.AddObject(ModelMateObs.Name, ModelMateObs);
  end;
  if ModflowPackages.ChobPackage.IsSelected then
  begin
    for Index := 0 to HeadFluxObservations.Count - 1 do
    begin
      ModelMuseFluxObsGroup := HeadFluxObservations[Index];
      if ModelMuseFluxObsGroup.Purpose = ObservationPurpose then
      begin
        UpdateModelMateFluxObservation(ObservationList, ModelMuseFluxObsGroup, Project, Operation);
      end;
    end;
  end;
  if ModflowPackages.DrobPackage.IsSelected then
  begin
    for Index := 0 to DrainObservations.Count - 1 do
    begin
      ModelMuseFluxObsGroup := DrainObservations[Index];
      if ModelMuseFluxObsGroup.Purpose = ObservationPurpose then
      begin
        UpdateModelMateFluxObservation(ObservationList, ModelMuseFluxObsGroup, Project, Operation);
      end;
    end;
  end;
  if ModflowPackages.GbobPackage.IsSelected then
  begin
    for Index := 0 to GhbObservations.Count - 1 do
    begin
      ModelMuseFluxObsGroup := GhbObservations[Index];
      if ModelMuseFluxObsGroup.Purpose = ObservationPurpose then
      begin
        UpdateModelMateFluxObservation(ObservationList, ModelMuseFluxObsGroup, Project, Operation);
      end;
    end;
  end;
  if ModflowPackages.RvobPackage.IsSelected then
  begin
    for Index := 0 to RiverObservations.Count - 1 do
    begin
      ModelMuseFluxObsGroup := RiverObservations[Index];
      if ModelMuseFluxObsGroup.Purpose = ObservationPurpose then
      begin
        UpdateModelMateFluxObservation(ObservationList, ModelMuseFluxObsGroup, Project, Operation);
      end;
    end;
  end;
  // Head observations
  for ScreenObjectIndex := 0 to ScreenObjectCount - 1 do
  begin
    ScreenObject := ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Observations := ScreenObject.ModflowHeadObservations;
    if (Observations <> nil) and Observations.Used
      and (Observations.Purpose = ObservationPurpose) then
    begin
      Method := Observations.MultiObsMethod;
      if Observations.Values.Count = 1 then
      begin
        ModelMuseHeadObs := Observations.Values.HobItems[0];
        OBSNAM := Observations.ObservationName;
        UpdateModelMateHeadObservation(ObservationList, OBSNAM,
          ModelMuseHeadObs, Project, Operation, momAllHeads);
      end
      else
      begin
        for ValueIndex := 0 to Observations.Values.Count - 1 do
        begin
          ModelMuseHeadObs := Observations.Values.HobItems[ValueIndex];
          OBSNAM := Observations.ObservationName + '_' + IntToStr(ValueIndex + 1);
          if Length(OBSNAM) > 12 then
          begin
            OBSNAM := Observations.ObservationName + IntToStr(ValueIndex + 1);
          end;
          if Length(OBSNAM) > 12 then
          begin
            // The GUI is designed to prevent this from ever being required.
            SetLength(OBSNAM, 12);
          end;
          if ValueIndex = 0 then
          begin
            UpdateModelMateHeadObservation(ObservationList, OBSNAM,
              ModelMuseHeadObs, Project, Operation, momAllHeads);
          end
          else
          begin
            UpdateModelMateHeadObservation(ObservationList, OBSNAM,
              ModelMuseHeadObs, Project, Operation, Method);
          end;
        end;
      end;
    end;
  end;
  case Operation of
    mmoImport:
      begin
        if ObservationList.Count > 0 then
        begin
          case ObservationPurpose of
            ofObserved: ObsVersion := 'observation';
            ofPredicted: ObsVersion := 'prediction';
            else Assert(False);
          end;
          Beep;
          MessageDlg('One or more ' + ObsVersion + 's from the '
            + 'ModelMate file are not present in the ModelMuse '
            + 'file and have been ignored. If an ' + ObsVersion + ' has been '
            + 'renamed, change the ' + ObsVersion + ' name so that it matches '
            + 'in ModelMuse and ModelMate and then try importing again.'#13#10#13#10
            + ObservationList.Text, mtWarning, [mbOK], 0);
        end;
      end;
    mmoExport: ;
    else Assert(False);
  end;
end;

procedure TPhastModel.HandleModelMateParameters(Operation: TModelMateOperation;
  ParameterList: TStringList; Project: TProject);
var
  ModelMuseParam: TModflowParameter;
  ModelMateParam: TParam;
  Index: Integer;
begin
  for Index := 0 to Project.ParamSet.Count - 1 do
  begin
    ModelMateParam := Project.ParamSet.Items[Index];
    ParameterList.AddObject(ModelMateParam.Name, ModelMateParam);
  end;
  ParameterList.CaseSensitive := False;
  for Index := 0 to ModflowSteadyParameters.Count - 1 do
  begin
    ModelMuseParam := ModflowSteadyParameters[Index];
    UpdateModelMateParameter(ParameterList, ModelMuseParam, Project, Operation);
  end;
  for Index := 0 to ModflowTransientParameters.Count - 1 do
  begin
    ModelMuseParam := ModflowTransientParameters[Index];
    UpdateModelMateParameter(ParameterList, ModelMuseParam, Project, Operation);
  end;
  for Index := 0 to HufParameters.Count - 1 do
  begin
    ModelMuseParam := HufParameters[Index];
    UpdateModelMateParameter(ParameterList, ModelMuseParam, Project, Operation);
  end;

  case Operation of
    mmoImport:
      begin
        if ParameterList.Count > 0 then
        begin
          Beep;
          MessageDlg('One or more parameters from the '
            + 'ModelMate file are not present in the ModelMuse '
            + 'file and have been ignored. If a parameter has been '
            + 'renamed, change the parameter name so that it matches '
            + 'in ModelMuse and ModelMate and then try importing again.'#13#10#13#10
            + ParameterList.Text, mtWarning, [mbOK], 0);
        end;
      end;
    mmoExport: ;
    else Assert(False);
  end;
end;

procedure TPhastModel.IncreaseScreenObjectCapacity(const Delta: integer);
begin
  if Delta <= 0 then Exit;
  FScreenObjectList.Capacity := FScreenObjectList.Capacity + Delta;
end;

function TPhastModel.GetWindowState: TWindowState;
begin
  if GuiSettings = nil then
  begin
    result := wsNormal;
  end
  else
  begin
    result := GuiSettings.WindowState;
  end;
end;

procedure TPhastModel.SetWindowState(const Value: TWindowState);
begin
  if GuiSettings <> nil then
  begin
    GuiSettings.WindowState := Value
  end;
//  frmGoPhast.WindowState := Value;
end;

procedure TPhastModel.SetSoluteTransport(const Value: boolean);
begin
  if FSoluteTransport <> Value then
  begin
    FSoluteTransport := Value;
    Invalidate;
  end;
end;

procedure TPhastModel.SetScreenObjectCollection(
  const Value: TScreenObjectCollection);
begin
  FScreenObjectCollection.Assign(Value);
end;

procedure TPhastModel.UpdateTimeLists;
var
  Index: integer;
  TimeList: TCustomTimeList;
begin
  for Index := 0 to TimeListCount - 1 do
  begin
    TimeList := TimeLists[Index];
    if TimeList is TPhastTimeList then
    begin
      TPhastTimeList(TimeList).Loaded;
    end;
  end;
  ModflowPackages.EtsPackage.UpdateEtsSegmentCount;
end;

procedure TPhastModel.UpdateThreeDTimeDataSet(const TimeList: TCustomTimeList;
  const Time: double);
var
  TimeIndex: integer;
  SelectedLayer, SelectedRow, SelectedColumn: integer;
begin
  SelectedLayer := Grid.SelectedLayer;
  SelectedRow := Grid.SelectedRow;
  SelectedColumn := Grid.SelectedColumn;
  try
    FThreeDDisplayTime := Time;
    if not TimeList.UpToDate then
    begin
      TimeList.Initialize;
    end;
    TimeIndex := TimeList.FirstTimeGreaterThan(Time) - 1;
    if TimeIndex < 0 then
    begin
      Grid.ThreeDDataSet := nil;
    end
    else
    begin
      Grid.ThreeDDataSet := TimeList.Items[TimeIndex];
      Grid.ThreeDDataSet.UpdateMinMaxValues
    end;
    FThreeDTimeList := TimeList;
  finally
    Grid.SelectedLayer := SelectedLayer;
    Grid.SelectedRow := SelectedRow;
    Grid.SelectedColumn := SelectedColumn;
  end;
end;

procedure TPhastModel.DrawScreenObjects3D;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  EnableLighting;

  glMatrixMode(GL_MODELVIEW);
  // do in reverse order so that object that are on top but at the same
  // location appear on top.
  for Index := ScreenObjectCount - 1 downto 0 do
  begin
    AScreenObject := ScreenObjects[Index];
    AScreenObject.Draw3D;
  end;
end;

procedure TPhastModel.InvalidateEtsDepthFractions(Sender: TObject);
begin
  ModflowPackages.EtsPackage.InvalidateEtsDepthFractions(Sender);
end;

procedure TPhastModel.InvalidateEtsRateFractions(Sender: TObject);
begin
  ModflowPackages.EtsPackage.InvalidateEtsRateFractions(Sender);
end;

procedure TPhastModel.InvalidateMfChdEndingHead(Sender: TObject);
begin
  ModflowPackages.ChdBoundary.MfChdEndingHead.Invalidate;
end;

procedure TPhastModel.InvalidateMfChdStartingHead(Sender: TObject);
begin
  ModflowPackages.ChdBoundary.MfChdStartingHead.Invalidate;
end;

procedure TPhastModel.InvalidateMfDrnConductance(Sender: TObject);
begin
  ModflowPackages.DrnPackage.MfDrnConductance.Invalidate;
end;

procedure TPhastModel.InvalidateMfDrnElevation(Sender: TObject);
begin
  ModflowPackages.DrnPackage.MfDrnElevation.Invalidate;
end;

procedure TPhastModel.InvalidateMfDrtConductance(Sender: TObject);
begin
  ModflowPackages.DrtPackage.MfDrtConductance.Invalidate;
end;

procedure TPhastModel.InvalidateMfDrtElevation(Sender: TObject);
begin
  ModflowPackages.DrtPackage.MfDrtElevation.Invalidate;
end;

procedure TPhastModel.InvalidateMfDrtReturnFraction(Sender: TObject);
begin
  ModflowPackages.DrtPackage.MfDrtReturnFraction.Invalidate;
end;

procedure TPhastModel.InvalidateMfEtsEvapDepth(Sender: TObject);
begin
  ModflowPackages.EtsPackage.MfEtsEvapDepth.Invalidate;
end;

procedure TPhastModel.InvalidateMfEtsEvapLayer(Sender: TObject);
begin
  ModflowPackages.EtsPackage.MfEtsEvapLayer.Invalidate;
end;

procedure TPhastModel.InvalidateMfEtsEvapRate(Sender: TObject);
begin
  ModflowPackages.EtsPackage.MfEtsEvapRate.Invalidate;
end;

procedure TPhastModel.InvalidateMfEtsEvapSurface(Sender: TObject);
begin
  ModflowPackages.EtsPackage.MfEtsEvapSurface.Invalidate;
end;

procedure TPhastModel.InvalidateMfEvtEvapDepth(Sender: TObject);
begin
  ModflowPackages.EvtPackage.MfEvtEvapDepth.Invalidate;
end;

procedure TPhastModel.InvalidateMfEvtEvapLayer(Sender: TObject);
begin
  ModflowPackages.EvtPackage.InvalidateMfEvtEvapLayer(Sender);
end;

procedure TPhastModel.InvalidateMfEvtEvapRate(Sender: TObject);
begin
  ModflowPackages.EvtPackage.MfEvtEvapRate.Invalidate;
end;

procedure TPhastModel.InvalidateMfEvtEvapSurface(Sender: TObject);
begin
  ModflowPackages.EvtPackage.MfEvtEvapSurface.Invalidate;
end;

procedure TPhastModel.InvalidateMfGhbBoundaryHead(Sender: TObject);
begin
  ModflowPackages.GhbBoundary.MfGhbBoundaryHead.Invalidate;
end;

procedure TPhastModel.InvalidateMfGhbConductance(Sender: TObject);
begin
  ModflowPackages.GhbBoundary.MfGhbConductance.Invalidate;
end;

procedure TPhastModel.InvalidateMfHobHeads(Sender: TObject);
begin
  MfHobHeads.Invalidate;
end;

procedure TPhastModel.InvalidateMfRchLayer(Sender: TObject);
begin
  ModflowPackages.RchPackage.InvalidateMfRchLayer(Sender);
end;

procedure TPhastModel.InvalidateMfRchRate(Sender: TObject);
begin
  ModflowPackages.RchPackage.MfRchRate.Invalidate;
end;

procedure TPhastModel.InvalidateMfRivBottom(Sender: TObject);
begin
  ModflowPackages.RivPackage.MfRivBottom.Invalidate;
end;

procedure TPhastModel.InvalidateMfRivConductance(Sender: TObject);
begin
  ModflowPackages.RivPackage.MfRivConductance.Invalidate;
end;

procedure TPhastModel.InvalidateMfRivStage(Sender: TObject);
begin
  ModflowPackages.RivPackage.MfRivStage.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrBankRoughness(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrBankRoughness.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrBrooksCorey(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrBrooksCorey.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrChannelRoughness(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrChannelRoughness.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrData(Sender: TObject);
begin
  InvalidateMfSfrUpstreamHydraulicConductivity(Sender);
  InvalidateMfSfrDownstreamHydraulicConductivity(Sender);
end;

procedure TPhastModel.InvalidateMfSfrDepthCoefficient(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrDepthCoefficient.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrDepthExponent(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrDepthExponent.Invalidate
end;

procedure TPhastModel.InvalidateMfSfrDownstreamBrooksCorey(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrDownstreamBrooksCorey.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrDownstreamDepth(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrDownstreamDepth.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrDownstreamElevation(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrDownstreamElevation.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrDownstreamHydraulicConductivity(
  Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrDownstreamHydraulicConductivity.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrDownstreamThickness(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrDownstreamThickness.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrDownstreamUnsatInitialWaterContent(
  Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrDownstreamUnsatInitialWaterContent.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrDownstreamUnsatKz(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrDownstreamUnsatKz.Invalidate;
end;

procedure TPhastModel.DischargeRoutingUpdate;
var
  LakeIdArray: TDataArray;
  DischargeRoutingArray: TDataArray;
begin
  LakeIdArray := FDataArrayManager.GetDataSetByName(rsLakeID);
  DischargeRoutingArray := FDataArrayManager.GetDataSetByName(StrUzfDischargeRouting);

  if ModflowPackages.SfrPackage.IsSelected
    or ModflowPackages.LakPackage.IsSelected then
  begin
    if DischargeRoutingArray <> nil then
    begin
      DischargeRoutingArray.OnPostInitialize := UpdateDischargeRouting;
      DischargeRoutingArray.OnDestroy := FinalizeDischargeRouting;
      DischargeRoutingArray.UpToDate := False;
      if LakeIdArray <> nil then
      begin
        DischargeRoutingArray.TalksTo(LakeIdArray);
      end;
    end;
  end
  else
  begin
    if DischargeRoutingArray <> nil then
    begin
      DischargeRoutingArray.OnPostInitialize := nil;
      DischargeRoutingArray.OnDestroy := nil;
      DischargeRoutingArray.UpToDate := False;
      if LakeIdArray <> nil then
      begin
        DischargeRoutingArray.StopsTalkingTo(LakeIdArray);
      end;
    end;
  end;
end;

function TPhastModel.DefaultArchiveName: string;
var
  ArchiveRoot: string;
begin
  if ModelFileName = '' then
  begin
    ArchiveRoot := GetCurrentDir + '\Archive'
  end
  else
  begin
    ArchiveRoot := ChangeFileExt(ModelFileName, '');
  end;
  result := ArchiveRoot + ' '
    + FormatDateTime('yyyy_mm_dd', Now) + '.zip';
end;

function TPhastModel.DefaultElevationFormula(
  ViewDirection: TViewDirection; EvalAt: TEvaluatedAt): string;
var
  UnitID: Integer;
  LayerGroup: TLayerGroup;
  Row: Integer;
  Column: Integer;
  Orientation: TDataSetOrientation;
  Compiler: TRbwParser;
begin
  if Grid = nil then
  begin
    result := '0';
    Exit;
  end;
  Orientation :=  dsoTop;
  case ViewDirection of
    vdTop:
      begin
        Orientation :=  dsoTop;
        if Grid.LayerCount > 0 then
        begin
          case ModelSelection of
            msUndefined: Assert(False);
            msPhast:
              begin
                result := FloatToStr((Grid.HighestElevation + Grid.LowestElevation)/2);
              end;
            msModflow, msModflowLGR:
              begin
                GetUnitID(UnitID);
                if UnitID > 0 then
                begin
                  LayerGroup := frmGoPhast.PhastModel.LayerStructure.
                    LayerGroups[UnitID-1];
                  result :=
                    LayerGroup.DataArrayName;
                  LayerGroup := frmGoPhast.PhastModel.LayerStructure.
                    LayerGroups[UnitID];
                  result :=
                    '(' + result + '+'
                    + LayerGroup.DataArrayName + ')/2';
                end
                else
                begin
                  result :=
                    FloatToStr((Grid.HighestElevation + Grid.LowestElevation)/2);
                end;
              end;
            else Assert(False);
          end;
        end
        else
        begin
          result := '0'
        end;
      end;
    vdFront:
      begin
        Orientation :=  dsoFront;
        if Grid.RowCount > 0 then
        begin
          Row := Grid.SelectedRow;
          if Row >= Grid.RowCount then
          begin
            Row := Grid.RowCount-1;
          end;
            result :=
              FloatToStr((Grid.RowPositions[Row]+ Grid.RowPositions[Row+1])/2);
        end
        else
        begin
          result := '0';
        end;
      end;
    vdSide:
      begin
        Orientation :=  dsoSide;
        if Grid.ColumnCount > 0 then
        begin
          Column := Grid.SelectedColumn;
          if Column >= Grid.ColumnCount then
          begin
            Column := Grid.ColumnCount -1;
          end;
          result :=
            FloatToStr((Grid.ColumnPositions[Column]
            + Grid.ColumnPositions[Column+1])/2);
        end
        else
        begin
          result := '0';
        end;
      end;
    else Assert(False);
  end;
  Compiler := GetCompiler(Orientation, EvalAt);
  Compiler.Compile(result);
end;

procedure TPhastModel.GetUnitID(var UnitID: Integer);
var
  LayerGroup: TLayerGroup;
  UnitIndex: Integer;
  LayersInUnitCount: Integer;
  Layer: Integer;
begin
  Layer := Grid.SelectedLayer;
  if Layer >= Grid.LayerCount then
  begin
    Layer := Grid.LayerCount - 1;
  end;
  LayersInUnitCount := 0;
  UnitID := -1;
  for UnitIndex := 1 to LayerStructure.Count - 1 do
  begin
    LayerGroup := LayerStructure.LayerGroups[UnitIndex];
    LayersInUnitCount := LayersInUnitCount + LayerGroup.LayerCount;
    if LayersInUnitCount >= Layer + 1 then
    begin
      UnitID := UnitIndex;
      break;
    end;
  end;
end;

function TPhastModel.GetUseWaterTable: boolean;
begin
  result := FUseWaterTable;
end;

function TPhastModel.DefaultHigherElevationFormula(
  ViewDirection: TViewDirection): string;
var
  UnitID: Integer;
  LayerGroup: TLayerGroup;
  Row: Integer;
  Column: Integer;
begin
  if Grid = nil then
  begin
    result := '0';
    Exit;
  end;
  case ViewDirection of
    vdTop:
      begin
        if Grid.LayerCount > 0 then
        begin
          case ModelSelection of
            msUndefined: Assert(False);
            msPhast:
              begin
                result := FloatToStr(Grid.HighestElevation);
              end;
            msModflow, msModflowLGR:
              begin
                GetUnitID(UnitID);
                if UnitID > 0 then
                begin
                  LayerGroup := frmGoPhast.PhastModel.LayerStructure.
                    LayerGroups[UnitID-1];
                  result :=
                    LayerGroup.DataArrayName;
                end
                else
                begin
                  result :=
                    FloatToStr(Grid.HighestElevation);
                end;
              end;
            else Assert(False);
          end;
        end
        else
        begin
          result := '0'
        end;
      end;
    vdFront:
      begin
        if Grid.RowCount > 0 then
        begin
          Row := Grid.SelectedRow;
          if Row >= Grid.RowCount then
          begin
            Row := Grid.RowCount-1;
          end;
          case Grid.RowDirection of
            rdSouthToNorth:
              begin
                result :=
                  FloatToStr(Grid.RowPositions[Row+1]);
              end;
            rdNorthToSouth:
              begin
                result :=
                  FloatToStr(Grid.RowPositions[Row]);
              end;
            else Assert(False);
          end;
        end
        else
        begin
          result := '0';
        end;
      end;
    vdSide:
      begin
        if Grid.ColumnCount > 0 then
        begin
          Column := Grid.SelectedColumn;
          if Column >= Grid.ColumnCount then
          begin
            Column := Grid.ColumnCount -1;
          end;
          case Grid.ColumnDirection of
            cdWestToEast:
              begin
                result :=
                  FloatToStr(Grid.ColumnPositions[Column+1]);
              end;
            cdEastToWest:
              begin
                result :=
                  FloatToStr(Grid.ColumnPositions[Column]);
              end;
          end;
        end
        else
        begin
          result := '0';
        end;
      end;
    else Assert(False);
  end;
end;

function TPhastModel.DefaultLowerElevationFormula(
  ViewDirection: TViewDirection): string;
var
  UnitID: Integer;
  LayerGroup: TLayerGroup;
  Row: Integer;
  Column: Integer;
begin
  if Grid = nil then
  begin
    result := '0';
    Exit;
  end;
  case ViewDirection of
    vdTop:
      begin
        if Grid.LayerCount > 0 then
        begin
          case ModelSelection of
            msUndefined: Assert(False);
            msPhast:
              begin
                result := FloatToStr(Grid.LowestElevation);
              end;
            msModflow, msModflowLGR:
              begin
                GetUnitID(UnitID);
                if UnitID > 0 then
                begin
                  LayerGroup := frmGoPhast.PhastModel.LayerStructure.
                    LayerGroups[UnitID];
                  result :=
                    LayerGroup.DataArrayName;
                end
                else
                begin
                  result :=
                    FloatToStr(Grid.LowestElevation);
                end;
              end;
            else Assert(False);
          end;
        end
        else
        begin
          result := '0'
        end;
      end;
    vdFront:
      begin
        if Grid.RowCount > 0 then
        begin
          Row := Grid.SelectedRow;
          if Row >= Grid.RowCount then
          begin
            Row := Grid.RowCount-1;
          end;
          case Grid.RowDirection of
            rdSouthToNorth:
              begin
                result :=
                  FloatToStr(Grid.RowPositions[Row]);
              end;
            rdNorthToSouth:
              begin
                result :=
                  FloatToStr(Grid.RowPositions[Row+1]);
              end;
            else Assert(False);
          end;
        end
        else
        begin
          result := '0';
        end;
      end;
    vdSide:
      begin
        if Grid.ColumnCount > 0 then
        begin
          Column := Grid.SelectedColumn;
          if Column >= Grid.ColumnCount then
          begin
            Column := Grid.ColumnCount -1;
          end;
          case Grid.ColumnDirection of
            cdWestToEast:
              begin
                result :=
                  FloatToStr(Grid.ColumnPositions[Column]);
              end;
            cdEastToWest:
              begin
                result :=
                  FloatToStr(Grid.ColumnPositions[Column+1]);
              end;
          end;
        end
        else
        begin
          result := '0';
        end;
      end;
    else Assert(False);
  end;
end;

function TPhastModel.DefaultModflowOutputFileName: string;
var
  Extension: string;
begin
  if ModflowOutputControl.HeadOC.SaveInExternalFile then
  begin
    case ModflowOutputControl.HeadOC.OutputFileType of
      oftText:
        begin
          Extension := StrFhd;
        end;
      oftBinary:
        begin
          Extension := StrBhd;
        end;
      else Assert(False);
    end;
  end
  else if ModflowOutputControl.DrawdownOC.SaveInExternalFile then
  begin
    case ModflowOutputControl.DrawdownOC.OutputFileType of
      oftText:
        begin
          Extension := StrFdn;
        end;
      oftBinary:
        begin
          Extension := StrBdn;
        end;
      else Assert(False);
    end;
  end
  else if ModflowOutputControl.SaveCellFlows = csfBinary then
  begin
    Extension := StrCbcExt;
  end
  else
  begin
    result := '';
    Exit;
  end;
  result := ChangeFileExt(ModelFileName, Extension);
  result := FixFileName(result);
end;

procedure TPhastModel.InvalidateMfSfrDownstreamUnsaturatedWaterContent(
  Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrDownstreamUnsaturatedWaterContent.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrDownstreamWidth(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrDownstreamWidth.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrEvapotranspiration(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrEvapotranspiration.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrFlow(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrFlow.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrInitialWaterContent(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrInitialWaterContent.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrIprior(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrIprior.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrPrecipitation(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrPrecipitation.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrReachLength(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrReachLength.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrRunoff(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrRunoff.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrStreamK(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrStreamK.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrSaturatedWaterContent(
  Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrSaturatedWaterContent.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrStreamSlope(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrStreamSlope.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrStreamThickness(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrStreamThickness.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrStreamTop(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrStreamTop.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrUpstreamBrooksCorey(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrUpstreamBrooksCorey.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrUpstreamDepth(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrUpstreamDepth.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrUpstreamElevation(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrUpstreamElevation.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrUpstreamHydraulicConductivity(
  Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrUpstreamHydraulicConductivity.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrUpstreamThickness(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrUpstreamThickness.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrUpstreamUnsatInitialWaterContent(
  Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrUpstreamUnsatInitialWaterContent.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrUpstreamUnsatKz(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrUpstreamUnsatKz.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrUpstreamUnsaturatedWaterContent(
  Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrUpstreamUnsaturatedWaterContent.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrUpstreamWidth(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrUpstreamWidth.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrVerticalUnsatK(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrVerticalUnsatK.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrWidthCoefficient(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrWidthCoefficient.Invalidate
end;

procedure TPhastModel.InvalidateMfSfrWidthExponent(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrWidthExponent.Invalidate;
end;

procedure TPhastModel.InvalidateMfUzfEtDemand(Sender: TObject);
begin
  ModflowPackages.UzfPackage.MfUzfEtDemand.Invalidate;
end;

procedure TPhastModel.InvalidateMfUzfExtinctionDepth(Sender: TObject);
begin
  ModflowPackages.UzfPackage.MfUzfExtinctionDepth.Invalidate;
end;

procedure TPhastModel.InvalidateMfUzfInfiltration(Sender: TObject);
begin
  ModflowPackages.UzfPackage.MfUzfInfiltration.Invalidate;
end;

procedure TPhastModel.InvalidateMfUzfWaterContent(Sender: TObject);
begin
  ModflowPackages.UzfPackage.MfUzfWaterContent.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrSegmentReachAndIcalc(Sender: TObject);
begin
  ModflowPackages.SfrPackage.
    InvalidateMfSfrSegmentReachAndIcalc(Sender);
end;

procedure TPhastModel.InvalidateMfWellPumpage(Sender: TObject);
begin
  ModflowPackages.WelPackage.MfWellPumpage.Invalidate;
end;

procedure TPhastModel.InvalidateModflowBoundaries;
begin
  ModflowPackages.EtsPackage.InvalidateAllTimeLists;
  ModflowPackages.ChdBoundary.InvalidateAllTimeLists;
  ModflowPackages.DrnPackage.InvalidateAllTimeLists;
  ModflowPackages.DrtPackage.InvalidateAllTimeLists;
  ModflowPackages.EvtPackage.InvalidateAllTimeLists;
  ModflowPackages.GhbBoundary.InvalidateAllTimeLists;
  if ModflowHobPackageUsed(nil) then
  begin
    InvalidateMfHobHeads(nil);
  end;
  ModflowPackages.RchPackage.InvalidateAllTimeLists;
  ModflowPackages.RivPackage.InvalidateAllTimeLists;
  ModflowPackages.SfrPackage.InvalidateAllTimeLists;
  ModflowPackages.UzfPackage.InvalidateAllTimeLists;
  ModflowPackages.WelPackage.InvalidateAllTimeLists;
  ModflowPackages.Mnw2Package.InvalidateAllTimeLists;
end;

procedure TPhastModel.CreatePhastTimeListGroups;
begin
  FSpecifiedHeadGroup := TTimeListGroup.Create;
  FSpecifiedHeadGroup.Add(FSpecifiedHeadHead);
  FSpecifiedHeadGroup.Name := 'Specified_Head';
  FSpecifiedHeadSolutionGroup := TTimeListGroup.Create;
  FSpecifiedHeadSolutionGroup.Add(FSpecifiedHeadAssociatedSolution);
  FSpecifiedHeadSolutionGroup.Name := 'Specified_Head_Solution';
  FFluxBoundaryFluxGroup := TTimeListGroup.Create;
  FFluxBoundaryFluxGroup.Add(FTopFluxBoundaryFlux);
  FFluxBoundaryFluxGroup.Add(FFrontFluxBoundaryFlux);
  FFluxBoundaryFluxGroup.Add(FSideFluxBoundaryFlux);
  FFluxBoundaryFluxGroup.Name := 'Flux_Boundary_Flux';
  FFluxBoundaryChemistryGroup := TTimeListGroup.Create;
  FFluxBoundaryChemistryGroup.Add(FTopFluxBoundaryChemistry);
  FFluxBoundaryChemistryGroup.Add(FFrontFluxBoundaryChemistry);
  FFluxBoundaryChemistryGroup.Add(FSideFluxBoundaryChemistry);
  FFluxBoundaryChemistryGroup.Name := 'Flux_Boundary_Chemistry';
  FLeakyHeadGroup := TTimeListGroup.Create;
  FLeakyHeadGroup.Add(FTopLeakyHead);
  FLeakyHeadGroup.Add(FFrontLeakyHead);
  FLeakyHeadGroup.Add(FSideLeakyHead);
  FLeakyHeadGroup.Name := 'Leaky_Head';
  FLeakyAssociatedSolutionGroup := TTimeListGroup.Create;
  FLeakyAssociatedSolutionGroup.Add(FTopLeakyAssociatedSolution);
  FLeakyAssociatedSolutionGroup.Add(FFrontLeakyAssociatedSolution);
  FLeakyAssociatedSolutionGroup.Add(FSideLeakyAssociatedSolution);
  FLeakyAssociatedSolutionGroup.Name := 'Leaky_Associated_Solution';
  FRiverHeadGroup := TTimeListGroup.Create;
  FRiverHeadGroup.Add(FRiverHead);
  FRiverHeadGroup.Name := 'River_Head';
  FRiverAssociatedSolutionGroup := TTimeListGroup.Create;
  FRiverAssociatedSolutionGroup.Add(FRiverAssociatedSolution);
  FRiverAssociatedSolutionGroup.Name := 'River_Associated_Solution';
  FWellPumpingRateGroup := TTimeListGroup.Create;
  FWellPumpingRateGroup.Add(FWellInjectionOrPumpingRate);
  FWellPumpingRateGroup.Name := 'Well_Pumping_Rate';
  FWellSolutionGroup := TTimeListGroup.Create;
  FWellSolutionGroup.Add(FWellSolution);
  FWellSolutionGroup.Name := 'Well_Solution';
end;

procedure TPhastModel.CreateInitialDataSetsForPhastTimeLists;
var
  PhastDataSet: TSparseArrayPhastInterpolationDataSet;
begin
  PhastDataSet := TSparseRealPhastDataSet.Create(self);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName('Z_Flux_Boundary_Flux0');
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dso3D;
  PhastDataSet.Formula := '0.';
  PhastDataSet.BoundaryDataType := TopBoundaryType;
  FTopFluxBoundaryFlux.Add(0, (PhastDataSet
    as TSparseArrayPhastInterpolationDataSet));

  PhastDataSet := TSparseRealPhastDataSet.Create(self);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName('Y_Flux_Boundary_Flux0');
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dso3D;
  PhastDataSet.Formula := '0.';
  PhastDataSet.BoundaryDataType := FrontBoundaryType;
  FFrontFluxBoundaryFlux.Add(0, PhastDataSet
    as TSparseArrayPhastInterpolationDataSet);

  PhastDataSet := TSparseRealPhastDataSet.Create(self);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName('X_Flux_Boundary_Flux0');
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dso3D;
  PhastDataSet.Formula := '0.';
  PhastDataSet.BoundaryDataType := SideBoundaryType;
  FSideFluxBoundaryFlux.Add(0, PhastDataSet
    as TSparseArrayPhastInterpolationDataSet);

  PhastDataSet := TSparseIntegerPhastDataSet.Create(self);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName('Z_Flux_Boundary_Chemistry0');
  PhastDataSet.DataType := rdtInteger;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dso3D;
  PhastDataSet.Formula := '0';
  PhastDataSet.BoundaryDataType := TopBoundaryType;
  FTopFluxBoundaryChemistry.Add(0, (PhastDataSet
    as TSparseArrayPhastInterpolationDataSet));

  PhastDataSet := TSparseIntegerPhastDataSet.Create(self);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName('Y_Flux_Boundary_Chemistry0');
  PhastDataSet.DataType := rdtInteger;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dso3D;
  PhastDataSet.Formula := '0';
  PhastDataSet.BoundaryDataType := FrontBoundaryType;
  FFrontFluxBoundaryChemistry.Add(0, PhastDataSet
    as TSparseArrayPhastInterpolationDataSet);

  PhastDataSet := TSparseIntegerPhastDataSet.Create(self);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName('X_Flux_Boundary_Chemistry0');
  PhastDataSet.DataType := rdtInteger;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dso3D;
  PhastDataSet.Formula := '0';
  PhastDataSet.BoundaryDataType := SideBoundaryType;
  FSideFluxBoundaryChemistry.Add(0, PhastDataSet
    as TSparseArrayPhastInterpolationDataSet);
end;

procedure TPhastModel.CreatePhastTimeLists;
begin
  FSpecifiedHeadHead := TPhastTimeList.Create(self);
  FSpecifiedHeadHead.Name := StrSpecifiedHead;
  FSpecifiedHeadHead.Orientation := dso3D;
  FSpecifiedHeadHead.Direction := dso3D;
  FSpecifiedHeadHead.BoundaryType := btSpecifiedHead;
  FSpecifiedHeadHead.BoundaryTypeDataSets.Add(FTopBoundaryType 
    as TIntegerSparseDataSet);
  FSpecifiedHeadHead.BoundaryTypeDataSets.Add(FFrontBoundaryType 
    as TIntegerSparseDataSet);
  FSpecifiedHeadHead.BoundaryTypeDataSets.Add(FSideBoundaryType 
    as TIntegerSparseDataSet);
  FSpecifiedHeadHead.DataType := rdtDouble;
  FSpecifiedHeadHead.OnTimeListUsed := PhastUsed;
  AddTimeList(FSpecifiedHeadHead);

  FSpecifiedHeadAssociatedSolution := TPhastTimeList.Create(self);
  FSpecifiedHeadAssociatedSolution.Name := StrSpecifiedHeadSolution;
  FSpecifiedHeadAssociatedSolution.Orientation := dso3D;
  FSpecifiedHeadAssociatedSolution.Direction := dso3D;
  FSpecifiedHeadAssociatedSolution.BoundaryType := btSpecifiedHead;
  FSpecifiedHeadAssociatedSolution.BoundaryTypeDataSets.Add(FTopBoundaryType
    as TIntegerSparseDataSet);
  FSpecifiedHeadAssociatedSolution.BoundaryTypeDataSets.Add(FFrontBoundaryType
    as TIntegerSparseDataSet);
  FSpecifiedHeadAssociatedSolution.BoundaryTypeDataSets.Add(FSideBoundaryType
    as TIntegerSparseDataSet);
  FSpecifiedHeadAssociatedSolution.DataType := rdtInteger;
  FSpecifiedHeadAssociatedSolution.OnTimeListUsed := PhastUsed;
  AddTimeList(FSpecifiedHeadAssociatedSolution);

  FTopFluxBoundaryFlux := TPhastTimeList.Create(self);
  FTopFluxBoundaryFlux.Name := StrTopFluxBoundaryFlux;
  FTopFluxBoundaryFlux.Orientation := dso3D;
  FTopFluxBoundaryFlux.Direction := dsoTop;
  FTopFluxBoundaryFlux.BoundaryType := btFlux;
  FTopFluxBoundaryFlux.BoundaryTypeDataSets.Add(FTopBoundaryType
    as TIntegerSparseDataSet);
  FTopFluxBoundaryFlux.DataType := rdtDouble;
  FTopFluxBoundaryFlux.OnTimeListUsed := PhastUsed;
  AddTimeList(FTopFluxBoundaryFlux);

  FFrontFluxBoundaryFlux := TPhastTimeList.Create(self);
  FFrontFluxBoundaryFlux.Name := StrFrontFluxBoundaryFlux;
  FFrontFluxBoundaryFlux.Orientation := dso3D;
  FFrontFluxBoundaryFlux.Direction := dsoFront;
  FFrontFluxBoundaryFlux.BoundaryType := btFlux;
  FFrontFluxBoundaryFlux.BoundaryTypeDataSets.Add(FFrontBoundaryType
    as TIntegerSparseDataSet);
  FFrontFluxBoundaryFlux.DataType := rdtDouble;
  FFrontFluxBoundaryFlux.OnTimeListUsed := PhastUsed;
  AddTimeList(FFrontFluxBoundaryFlux);

  FSideFluxBoundaryFlux := TPhastTimeList.Create(self);
  FSideFluxBoundaryFlux.Name := StrSideFluxBoundaryFlux;
  FSideFluxBoundaryFlux.Orientation := dso3D;
  FSideFluxBoundaryFlux.Direction := dsoSide;
  FSideFluxBoundaryFlux.BoundaryType := btFlux;
  FSideFluxBoundaryFlux.BoundaryTypeDataSets.Add(FSideBoundaryType
    as TIntegerSparseDataSet);
  FSideFluxBoundaryFlux.DataType := rdtDouble;
  FSideFluxBoundaryFlux.OnTimeListUsed := PhastUsed;
  AddTimeList(FSideFluxBoundaryFlux);

  FTopFluxBoundaryChemistry := TPhastTimeList.Create(self);
  FTopFluxBoundaryChemistry.Name := StrTopFluxBoundaryAssocSoln;
  FTopFluxBoundaryChemistry.Orientation := dso3D;
  FTopFluxBoundaryChemistry.Direction := dsoTop;
  FTopFluxBoundaryChemistry.BoundaryType := btFlux;
  FTopFluxBoundaryChemistry.BoundaryTypeDataSets.Add(FTopBoundaryType
    as TIntegerSparseDataSet);
  FTopFluxBoundaryChemistry.DataType := rdtInteger;
  FTopFluxBoundaryChemistry.OnTimeListUsed := PhastUsed;
  AddTimeList(FTopFluxBoundaryChemistry);

  FFrontFluxBoundaryChemistry := TPhastTimeList.Create(self);
  FFrontFluxBoundaryChemistry.Name := StrFrontFluxBoundaryAssocSoln;
  FFrontFluxBoundaryChemistry.Orientation := dso3D;
  FFrontFluxBoundaryChemistry.Direction := dsoFront;
  FFrontFluxBoundaryChemistry.BoundaryType := btFlux;
  FFrontFluxBoundaryChemistry.BoundaryTypeDataSets.Add(FFrontBoundaryType
    as TIntegerSparseDataSet);
  FFrontFluxBoundaryChemistry.DataType := rdtInteger;
  FFrontFluxBoundaryChemistry.OnTimeListUsed := PhastUsed;
  AddTimeList(FFrontFluxBoundaryChemistry);

  FSideFluxBoundaryChemistry := TPhastTimeList.Create(self);
  FSideFluxBoundaryChemistry.Name := StrSideFluxBoundaryAssocSoln;
  FSideFluxBoundaryChemistry.Orientation := dso3D;
  FSideFluxBoundaryChemistry.Direction := dsoSide;
  FSideFluxBoundaryChemistry.BoundaryType := btFlux;
  FSideFluxBoundaryChemistry.BoundaryTypeDataSets.Add(FSideBoundaryType
    as TIntegerSparseDataSet);
  FSideFluxBoundaryChemistry.DataType := rdtInteger;
  FSideFluxBoundaryChemistry.OnTimeListUsed := PhastUsed;
  AddTimeList(FSideFluxBoundaryChemistry);

  FTopLeakyHead := TPhastTimeList.Create(self);
  FTopLeakyHead.Name := StrTopLeakyBoundaryHead;
  FTopLeakyHead.Orientation := dso3D;
  FTopLeakyHead.Direction := dsoTop;
  FTopLeakyHead.BoundaryType := btLeaky;
  FTopLeakyHead.BoundaryTypeDataSets.Add(FTopBoundaryType
    as TIntegerSparseDataSet);
  FTopLeakyHead.DataType := rdtDouble;
  FTopLeakyHead.OnTimeListUsed := PhastUsed;
  AddTimeList(FTopLeakyHead);

  FTopLeakyAssociatedSolution := TPhastTimeList.Create(self);
  FTopLeakyAssociatedSolution.Name := StrTopLeakyBoundaryAssocSoln;
  FTopLeakyAssociatedSolution.Orientation := dso3D;
  FTopLeakyAssociatedSolution.Direction := dsoTop;
  FTopLeakyAssociatedSolution.BoundaryType := btLeaky;
  FTopLeakyAssociatedSolution.BoundaryTypeDataSets.Add(FTopBoundaryType
    as TIntegerSparseDataSet);
  FTopLeakyAssociatedSolution.DataType := rdtInteger;
  FTopLeakyAssociatedSolution.OnTimeListUsed := PhastUsed;
  AddTimeList(FTopLeakyAssociatedSolution);

  FFrontLeakyHead := TPhastTimeList.Create(self);
  FFrontLeakyHead.Name := StrFrontLeakyBoundaryHead;
  FFrontLeakyHead.Orientation := dso3D;
  FFrontLeakyHead.Direction := dsoFront;
  FFrontLeakyHead.BoundaryType := btLeaky;
  FFrontLeakyHead.BoundaryTypeDataSets.Add(FFrontBoundaryType
    as TIntegerSparseDataSet);
  FFrontLeakyHead.DataType := rdtDouble;
  FFrontLeakyHead.OnTimeListUsed := PhastUsed;
  AddTimeList(FFrontLeakyHead);

  FFrontLeakyAssociatedSolution := TPhastTimeList.Create(self);
  FFrontLeakyAssociatedSolution.Name := StrFrontLeakyBoundaryAssocSoln;
  FFrontLeakyAssociatedSolution.Orientation := dso3D;
  FFrontLeakyAssociatedSolution.Direction := dsoFront;
  FFrontLeakyAssociatedSolution.BoundaryType := btLeaky;
  FFrontLeakyAssociatedSolution.BoundaryTypeDataSets.Add(FFrontBoundaryType
    as TIntegerSparseDataSet);
  FFrontLeakyAssociatedSolution.DataType := rdtInteger;
  FFrontLeakyAssociatedSolution.OnTimeListUsed := PhastUsed;
  AddTimeList(FFrontLeakyAssociatedSolution);

  FSideLeakyHead := TPhastTimeList.Create(self);
  FSideLeakyHead.Name := StrSideLeakyBoundaryHead;
  FSideLeakyHead.Orientation := dso3D;
  FSideLeakyHead.Direction := dsoSide;
  FSideLeakyHead.BoundaryType := btLeaky;
  FSideLeakyHead.BoundaryTypeDataSets.Add(FSideBoundaryType
    as TIntegerSparseDataSet);
  FSideLeakyHead.DataType := rdtDouble;
  FSideLeakyHead.OnTimeListUsed := PhastUsed;
  AddTimeList(FSideLeakyHead);

  FSideLeakyAssociatedSolution := TPhastTimeList.Create(self);
  FSideLeakyAssociatedSolution.Name := StrSideLeakyBoundaryAssocSoln;
  FSideLeakyAssociatedSolution.Orientation := dso3D;
  FSideLeakyAssociatedSolution.Direction := dsoSide;
  FSideLeakyAssociatedSolution.BoundaryType := btLeaky;
  FSideLeakyAssociatedSolution.BoundaryTypeDataSets.Add(FSideBoundaryType
    as TIntegerSparseDataSet);
  FSideLeakyAssociatedSolution.DataType := rdtInteger;
  FSideLeakyAssociatedSolution.OnTimeListUsed := PhastUsed;
  AddTimeList(FSideLeakyAssociatedSolution);

  FRiverHead := TPhastTimeList.Create(self);
  FRiverHead.Name := StrRiverHead;
  FRiverHead.Orientation := dsoTop;
  FRiverHead.Direction := dsoTop;
  FRiverHead.BoundaryType := btRiver;
  FRiverHead.BoundaryTypeDataSets.Add(FTop2DBoundaryType
    as TIntegerSparseDataSet);
  FRiverHead.DataType := rdtDouble;
  FRiverHead.OnTimeListUsed := PhastUsed;
  AddTimeList(FRiverHead);

  FRiverAssociatedSolution := TPhastTimeList.Create(self);
  FRiverAssociatedSolution.Name := StrRiverAssocSoln;
  FRiverAssociatedSolution.Orientation := dsoTop;
  FRiverAssociatedSolution.Direction := dsoTop;
  FRiverAssociatedSolution.BoundaryType := btRiver;
  FRiverAssociatedSolution.BoundaryTypeDataSets.Add(FTop2DBoundaryType
    as TIntegerSparseDataSet);
  FRiverAssociatedSolution.DataType := rdtInteger;
  FRiverAssociatedSolution.OnTimeListUsed := PhastUsed;
  AddTimeList(FRiverAssociatedSolution);

  FWellInjectionOrPumpingRate := TPhastTimeList.Create(self);
  FWellInjectionOrPumpingRate.Name := StrWellInjectionRate;
  FWellInjectionOrPumpingRate.Orientation := dsoTop;
  FWellInjectionOrPumpingRate.Direction := dsoTop;
  FWellInjectionOrPumpingRate.BoundaryType := btWell;
  FWellInjectionOrPumpingRate.BoundaryTypeDataSets.Add(FTop2DBoundaryType
    as TIntegerSparseDataSet);
  FWellInjectionOrPumpingRate.DataType := rdtDouble;
  FWellInjectionOrPumpingRate.OnTimeListUsed := PhastUsed;
  AddTimeList(FWellInjectionOrPumpingRate);

  FWellSolution := TPhastTimeList.Create(self);
  FWellSolution.Name := StrWellSolution;
  FWellSolution.Orientation := dsoTop;
  FWellSolution.Direction := dsoTop;
  FWellSolution.BoundaryType := btWell;
  FWellSolution.BoundaryTypeDataSets.Add(FTop2DBoundaryType
    as TIntegerSparseDataSet);
  FWellSolution.DataType := rdtInteger;
  FWellSolution.OnTimeListUsed := PhastUsed;
  AddTimeList(FWellSolution);
end;

procedure TPhastModel.InvalidateScreenObjects;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  if Grid <> nil then
  begin
    Grid.BeginGridChange;
  end;
  try
    for Index := 0 to ScreenObjectCount - 1 do
    begin
      AScreenObject := ScreenObjects[Index];
      AScreenObject.Invalidate;
    end;
  finally
    if Grid <> nil then
    begin
      Grid.EndGridChange;
    end;
  end;
end;

procedure TPhastModel.SetArchiveName(const Value: string);
begin
  FArchiveName := Value;
end;

procedure TPhastModel.SetBitmaps(const Value: TCompressedBitmapCollection);
begin
  FBitmaps.Assign(Value);
end;

procedure TPhastModel.SetChemistryOptions(const Value: TChemistryOptions);
begin
  FChemistryOptions.Assign(Value);
end;

procedure TPhastModel.SetChildModels(const Value: TChildModelCollection);
begin
  FChildModels.Assign(Value);
end;

procedure TPhastModel.SetUnits(const Value: TUnits);
begin
  FUnits.Assign(Value);
end;

procedure TPhastModel.SetUpToDate(const Value: boolean);
begin
  inherited;
  if not UpToDate then
  begin
    FreeAndNil(FSortedObjectList);
  end;

end;

procedure TPhastModel.Notify3DViewChanged;
begin
  if Assigned(On3DViewChanged) then
  begin
    On3DViewChanged(self);
  end;
end;

function TPhastModel.NumberOfLargestScreenObjectsStartingWith(
  const Root: string): integer;
var
  Index: integer;
  AScreenObject: TScreenObject;
  AName: String;
  Value: integer;
begin
  result := 0;
  for Index := 0 to ScreenObjectCount -1 do
  begin
    AScreenObject := ScreenObjects[Index];
    if not AScreenObject.Deleted and (Pos(Root, AScreenObject.Name) = 1) then
    begin
      AName := Copy(AScreenObject.Name, Length(Root)+1, MAXINT);

      try
        if AName = '' then
        begin
          Value := 1;
        end
        else
        begin
          Value := StrToInt(AName);
        end;
        if Value > result then
        begin
          result := value;
        end;

      except on EConvertError do
        begin
          // Ignore
        end;
      end;
    end;
  end;
end;

procedure EnableLighting;
var
  light_specular: array[0..3] of Extended;
  light_diffuse: array[0..3] of Extended;
  light_ambient: array[0..3] of Extended;
begin
  light_ambient[0] := ColorValues.Ambient;
  light_ambient[1] := light_ambient[0];
  light_ambient[2] := light_ambient[0];
  light_ambient[3] := 1;

  light_diffuse[0] := ColorValues.Diffuse;
  light_diffuse[1] := light_diffuse[0];
  light_diffuse[2] := light_diffuse[0];
  light_diffuse[3] := 1;

  light_specular[0] := ColorValues.Specular;
  light_specular[1] := light_specular[0];
  light_specular[2] := light_specular[0];
  light_specular[3] := 1;

  glLightfv(GL_LIGHT0, GL_AMBIENT, @light_ambient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @light_diffuse);
  glLightfv(GL_LIGHT0, GL_SPECULAR, @light_specular);

  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, 1);

  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
end;

function TPhastModel.ParameterDataSetUsed(Sender: TObject): boolean;
begin
  result := ModflowSteadyParameters.IsDataSetUsed(Sender);
end;

procedure TPhastModel.PasteObjectsFromClipboard(List: TList);
var
  ClipboardText: string;
  ClipStream: TStringStream;
  MemStream: TMemoryStream;
  Objects: TScreenObjectClipboard;
  Index: Integer;
  Item: TScreenObjectItem;
  NewScreenObject: TScreenObject;
  OldScreenObject: TScreenObject;
  ScreenObj: TScreenObjectCrack;
  ScreenObjectNames: TStringList;
  ScreenObject: TScreenObject;
  NewIndex: Integer;
  NewName: string;
begin
  ClipboardText := ClipBoard.AsText;
  if Pos('object TScreenObjectClipboard', ClipboardText) = 1 then
  begin
    ClipStream := TStringStream.Create(ClipboardText);
    try
      ClipStream.Position := 0;
      MemStream := TMemoryStream.Create;
      try
        ObjectTextToBinary(ClipStream, MemStream);
        Objects := TScreenObjectClipboard.Create(nil);
        try
          MemStream.Position := 0;
          MemStream.ReadComponent(Objects);
          Objects.UpdateModel(self);

          ScreenObjectNames := TStringList.Create;
          try
            ScreenObjectNames.Sorted := True;
            ScreenObjectNames.Duplicates := dupIgnore;
            ScreenObjectNames.CaseSensitive := False;
            for Index := 0 to ScreenObjectCount - 1 do
            begin
              ScreenObject := ScreenObjects[Index];
              if not ScreenObject.Deleted then
              begin
                ScreenObjectNames.Add(ScreenObject.Name);
              end;
            end;

            for Index := 0 to Objects.ScreenObjects.Count - 1 do
            begin
              Item := Objects.ScreenObjects.Items[Index] as TScreenObjectItem;
              NewScreenObject := TScreenObjectClass(
                Item.ScreenObject.ClassType).Create(self);
              List.Add(NewScreenObject);
              OldScreenObject := Item.ScreenObject;
              Item.SetScreenObject(NewScreenObject);
              NewScreenObject.Assign(OldScreenObject);
              if ScreenObjectNames.IndexOf(NewScreenObject.Name) >= 0 then
              begin
                NewIndex := 0;
                NewName := ObjectPrefix + IntToStr(NewIndex);
                While ScreenObjectNames.IndexOf(NewName) >= 0 do
                begin
                  Inc(NewIndex);
                  NewName := ObjectPrefix + IntToStr(NewIndex);
                end;
                NewScreenObject.Name := NewName;
              end;
              OldScreenObject.Free;
            end;
            Objects.ScreenObjects.UpdateScreenObjects;
            for Index := 0 to Objects.ScreenObjects.Count - 1 do
            begin
              Item := Objects.ScreenObjects.Items[Index] as TScreenObjectItem;
              ScreenObj := TScreenObjectCrack(Item.ScreenObject);
              ScreenObj.Loaded;
            end;
            UpdateDrainReturnObjects;
          finally
            ScreenObjectNames.Free;
          end;
        finally
          Objects.Free;
        end;
      finally
        MemStream.Free;
      end;
    finally
      ClipStream.Free;
    end;
  end;
end;

procedure TPhastModel.NotifyGridColorsChanged(Sender: TObject);
begin
  if Grid.TopDataSet <> nil then
  begin
    Grid.NeedToRecalculateTopCellColors := True;
  end;
  if Grid.FrontDataSet <> nil then
  begin
    Grid.NeedToRecalculateFrontCellColors := True;
  end;
  if Grid.SideDataSet <> nil then
  begin
    Grid.NeedToRecalculateSideCellColors := True;
  end;
end;

{ TDataSetItem }
procedure TDataSetItem.Assign(Source: TPersistent);
var
  SourceItem: TDataSetItem;
  ClassType: TDataArrayType;
begin
  if Source is TDataSetItem then
  begin
    Beep;
    SourceItem := TDataSetItem(Source);
    FDataSet.Free;
    ClassType := TDataArrayType(SourceItem.FDataSet.ClassType);
    FDataSet := ClassType.Create(nil);
    FDataSet.Assign(SourceItem.FDataSet);
    FDataSetFormula := SourceItem.FDataSetFormula;
  end
  else
  begin
    inherited;
  end;
end;

function TDataSetItem.GetDataSetClass: string;
begin
  Result := FDataSet.ClassName;
end;

function TDataSetItem.GetDataSetFormula: string;
begin
  Result := FDataSet.Formula;
end;

function TDataSetItem.GetMixtureFormula: string;
begin
  if FDataSet is TCustomPhastDataSet then
  begin
    result := TCustomPhastDataSet(FDataSet).MixtureFormula;
  end
  else
  begin
    result := '';
  end;
end;

function TDataSetItem.GetParameterFormula: string;
begin
  Result := FParameterFormula;
end;

procedure TDataSetItem.SetDataSetClass(const Value: string);
begin
  if Value = 'TDataSet' then
  begin
    { TODO : This is for backwards compatibility. Remove when no longer needed. }
    FDataSet := TDataArrayType(TDataArray).Create
      (frmGoPhast.PhastModel);
  end
  else
  begin
    FDataSet := TDataArrayType(GetClass(Value)).Create
      (frmGoPhast.PhastModel);
  end;

  FDataSet.SetSubComponent(True);
end;

procedure TDataSetItem.UpdateDataSet;
begin
  FDataSet.UpdateWithName(FDataSet.Name);
end;

{ TDataSetCollection }

constructor TDataSetCollection.Create;
begin
  inherited Create(TDataSetItem);
end;

{ TUnits }

procedure TUnits.Assign(Source: TPersistent);
begin
  if Source is TUnits then
  begin
    with TUnits(Source) do
    begin
      self.DefaultTimeUnits := DefaultTimeUnits;
      self.DefaultHorizontalGridUnits := DefaultHorizontalGridUnits;
      self.DefaultVerticalGridUnits := DefaultVerticalGridUnits;
      self.DefaultHeadUnits := DefaultHeadUnits;
      self.DefaultHydraulicConductivityLengthUnits :=
        DefaultHydraulicConductivityLengthUnits;
      self.DefaultHydraulicConductivityTimeUnits :=
        DefaultHydraulicConductivityTimeUnits;
      self.DefaultSpecificStorageUnits := DefaultSpecificStorageUnits;
      self.DefaultDispersivityUnits := DefaultDispersivityUnits;
      self.DefaultFluxLengthUnits := DefaultFluxLengthUnits;
      self.DefaultFluxTimeUnits := DefaultFluxTimeUnits;
      self.DefaultLeakyHydraulicConductivityLengthUnits :=
        DefaultLeakyHydraulicConductivityLengthUnits;
      self.DefaultLeakyHydraulicConductivityTimeUnits :=
        DefaultLeakyHydraulicConductivityTimeUnits;
      self.DefaultLeakyThicknessUnits := DefaultLeakyThicknessUnits;
      self.DefaultWellDiameterUnits := DefaultWellDiameterUnits;
      self.DefaultWellFlowVolumnUnits := DefaultWellFlowVolumnUnits;
      self.DefaultWellFlowTimeUnits := DefaultWellFlowTimeUnits;
      self.DefaultRiverBedHydraulicConductivityLengthUnits :=
        DefaultRiverBedHydraulicConductivityLengthUnits;
      self.DefaultRiverBedHydraulicConductivityTimeUnits :=
        DefaultRiverBedHydraulicConductivityTimeUnits;
      self.DefaultRiverBedThicknessUnits := DefaultRiverBedThicknessUnits;
    end;
  end
  else
  begin
    inherited;
  end;
end;

constructor TUnits.Create;
begin
  DefaultDispersivityUnits := luMeters;
  DefaultFluxLengthUnits := luMeters;
  DefaultHeadUnits := luMeters;
  DefaultHorizontalGridUnits := luMeters;
  DefaultHydraulicConductivityLengthUnits := luMeters;
  DefaultLeakyHydraulicConductivityLengthUnits := luMeters;
  DefaultLeakyThicknessUnits := luMeters;
  DefaultRiverBedHydraulicConductivityLengthUnits := luMeters;
  DefaultRiverBedThicknessUnits := luMeters;
  DefaultVerticalGridUnits := luMeters;
  DefaultWellDiameterUnits := luCentimeters;
  DefaultWellFlowVolumnUnits := vuMeters3;
  DefaultSpecificStorageUnits := iluMeters;
end;

procedure TUnits.SetDefaultDispersivityUnits(const Value: TLengthUnits);
begin
  if FDefaultDispersivityUnits <> Value then
  begin
    FDefaultDispersivityUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TUnits.SetDefaultFluxLengthUnits(const Value: TLengthUnits);
begin
  if FDefaultFluxLengthUnits <> Value then
  begin
    FDefaultFluxLengthUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TUnits.SetDefaultFluxTimeUnits(const Value: TTimeUnits);
begin
  if FDefaultFluxTimeUnits <> Value then
  begin
    FDefaultFluxTimeUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TUnits.SetDefaultHeadUnits(const Value: TLengthUnits);
begin
  if FDefaultHeadUnits <> Value then
  begin
    FDefaultHeadUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TUnits.SetDefaultHorizontalGridUnits(const Value: TLengthUnits);
begin
  if FDefaultHorizontalGridUnits <> Value then
  begin
    FDefaultHorizontalGridUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TUnits.SetDefaultHydraulicConductivityLengthUnits(
  const Value: TLengthUnits);
begin
  if FDefaultHydraulicConductivityLengthUnits <> Value then
  begin
    FDefaultHydraulicConductivityLengthUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TUnits.SetDefaultLeakyHydraulicConductivityLengthUnits(
  const Value: TLengthUnits);
begin
  if FDefaultLeakyHydraulicConductivityLengthUnits <> Value then
  begin
    FDefaultLeakyHydraulicConductivityLengthUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TUnits.SetDefaultLeakyHydraulicConductivityTimeUnits(
  const Value: TTimeUnits);
begin
  if FDefaultLeakyHydraulicConductivityTimeUnits <> Value then
  begin
    FDefaultLeakyHydraulicConductivityTimeUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TUnits.SetDefaultLeakyThicknessUnits(const Value: TLengthUnits);
begin
  if FDefaultLeakyThicknessUnits <> Value then
  begin
    FDefaultLeakyThicknessUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TUnits.SetDefaultRiverBedHydraulicConductivityLengthUnits(
  const Value: TLengthUnits);
begin
  if FDefaultRiverBedHydraulicConductivityLengthUnits <> Value then
  begin
    FDefaultRiverBedHydraulicConductivityLengthUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TUnits.SetDefaultRiverBedHydraulicConductivityTimeUnits(
  const Value: TTimeUnits);
begin
  if FDefaultRiverBedHydraulicConductivityTimeUnits <> Value then
  begin
    FDefaultRiverBedHydraulicConductivityTimeUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TUnits.SetDefaultRiverBedThicknessUnits(
  const Value: TLengthUnits);
begin
  if FDefaultRiverBedThicknessUnits <> Value then
  begin
    FDefaultRiverBedThicknessUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TUnits.SetDefaultSpecificStorageUnits(
  const Value: TInverseLengthUnits);
begin
  if FDefaultSpecificStorageUnits <> Value then
  begin
    FDefaultSpecificStorageUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TUnits.SetDefaultTimeUnits(const Value: TTimeUnits);
begin
  if FDefaultTimeUnits <> Value then
  begin
    FDefaultTimeUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TUnits.SetDefaultVerticalGridUnits(const Value: TLengthUnits);
begin
  if FDefaultVerticalGridUnits <> Value then
  begin
    FDefaultVerticalGridUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TUnits.SetDefaultWellDiameterUnits(const Value: TLengthUnits);
begin
  if FDefaultWellDiameterUnits <> Value then
  begin
    FDefaultWellDiameterUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TUnits.SetDefaultWellFlowTimeUnits(const Value: TTimeUnits);
begin
  if FDefaultWellFlowTimeUnits <> Value then
  begin
    FDefaultWellFlowTimeUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TUnits.SetDefaultWellFlowVolumnUnits(const Value: TVolumeUnits);
begin
  if FDefaultWellFlowVolumnUnits <> Value then
  begin
    FDefaultWellFlowVolumnUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TUnits.SetDefaultHydraulicConductivityTimeUnits(
  const Value: TTimeUnits);
begin
  if FDefaultHydraulicConductivityTimeUnits <> Value then
  begin
    FDefaultHydraulicConductivityTimeUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

{ TPrintInitial }

procedure TPrintInitial.Assign(Source: TPersistent);
begin
  if Source is TPrintInitial then
  begin
    with TPrintInitial(Source) do
    begin
      self.PrintInitialBoundaryConditions := PrintInitialBoundaryConditions;
      self.PrintInitialComponents := PrintInitialComponents;
      self.PrintInitialConductance := PrintInitialConductance;
      self.PrintInitialEchoInput := PrintInitialEchoInput;
      self.PrintInitialFluidProperties := PrintInitialFluidProperties;
      self.PrintInitialForceChemistryPrint := PrintInitialForceChemistryPrint;
      self.PrintInitialHDF_Chemistry := PrintInitialHDF_Chemistry;
      self.PrintInitialHDF_Heads := PrintInitialHDF_Heads;
      self.PrintInitialHDF_SteadyFlowVelocites :=
        PrintInitialHDF_SteadyFlowVelocites;
      self.PrintInitialHeads := PrintInitialHeads;
      self.PrintInitialMediaProperties := PrintInitialMediaProperties;
      self.PrintInitialSolutionMethod := PrintInitialSolutionMethod;
      self.PrintInitialSteadyFlowVelocities :=
        PrintInitialSteadyFlowVelocities;
      self.PrintInitialWells := PrintInitialWells;
      self.PrintInitialXYZ_Chemistry := PrintInitialXYZ_Chemistry;
      self.PrintInitialXYZ_Components := PrintInitialXYZ_Components;
      self.PrintInitialXYZ_Heads := PrintInitialXYZ_Heads;
      self.PrintInitialXYZ_SteadyFlowVelocities :=
        PrintInitialXYZ_SteadyFlowVelocities;
      self.PrintInitialXYZ_Wells := PrintInitialXYZ_Wells;
    end;
  end
  else
  begin
    inherited;
  end;
end;

constructor TPrintInitial.Create;
begin
  PrintInitialEchoInput := True;
  PrintInitialFluidProperties := True;
  PrintInitialHDF_Chemistry := True;
  PrintInitialHDF_Heads := True;
  PrintInitialHDF_SteadyFlowVelocites := True;
  PrintInitialHeads := True;
  PrintInitialSolutionMethod := True;
  PrintInitialWells := True;
end;

procedure TPrintInitial.SetPrintInitialBoundaryConditions(
  const Value: boolean);
begin
  if FPrintInitialBoundaryConditions <> Value then
  begin
    FPrintInitialBoundaryConditions := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintInitial.SetPrintInitialComponents(const Value: boolean);
begin
  if FPrintInitialComponents <> Value then
  begin
    FPrintInitialComponents := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintInitial.SetPrintInitialConductance(const Value: boolean);
begin
  if FPrintInitialConductance <> Value then
  begin
    FPrintInitialConductance := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintInitial.SetPrintInitialEchoInput(const Value: boolean);
begin
  if FPrintInitialEchoInput <> Value then
  begin
    FPrintInitialEchoInput := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintInitial.SetPrintInitialFluidProperties(
  const Value: boolean);
begin
  if FPrintInitialFluidProperties <> Value then
  begin
    FPrintInitialFluidProperties := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintInitial.SetPrintInitialForceChemistryPrint(
  const Value: boolean);
begin
  if FPrintInitialForceChemistryPrint <> Value then
  begin
    FPrintInitialForceChemistryPrint := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintInitial.SetPrintInitialHDF_Chemistry(const Value: boolean);
begin
  if FPrintInitialHDF_Chemistry <> Value then
  begin
    FPrintInitialHDF_Chemistry := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintInitial.SetPrintInitialHDF_Heads(const Value: boolean);
begin
  if FPrintInitialHDF_Heads <> Value then
  begin
    FPrintInitialHDF_Heads := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintInitial.SetPrintInitialHDF_SteadyFlowVelocites(
  const Value: boolean);
begin
  if FPrintInitialHDF_SteadyFlowVelocites <> Value then
  begin
    FPrintInitialHDF_SteadyFlowVelocites := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintInitial.SetPrintInitialHeads(const Value: boolean);
begin
  if FPrintInitialHeads <> Value then
  begin
    FPrintInitialHeads := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintInitial.SetPrintInitialMediaProperties(
  const Value: boolean);
begin
  if FPrintInitialMediaProperties <> Value then
  begin
    FPrintInitialMediaProperties := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintInitial.SetPrintInitialSolutionMethod(
  const Value: boolean);
begin
  if FPrintInitialSolutionMethod <> Value then
  begin
    FPrintInitialSolutionMethod := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintInitial.SetPrintInitialSteadyFlowVelocities(
  const Value: boolean);
begin
  if FPrintInitialSteadyFlowVelocities <> Value then
  begin
    FPrintInitialSteadyFlowVelocities := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintInitial.SetPrintInitialWells(const Value: boolean);
begin
  if FPrintInitialWells <> Value then
  begin
    FPrintInitialWells := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintInitial.SetPrintInitialXYZ_Chemistry(const Value: boolean);
begin
  if FPrintInitialXYZ_Chemistry <> Value then
  begin
    FPrintInitialXYZ_Chemistry := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintInitial.SetPrintInitialXYZ_Components(
  const Value: boolean);
begin
  if FPrintInitialXYZ_Components <> Value then
  begin
    FPrintInitialXYZ_Components := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintInitial.SetPrintInitialXYZ_Heads(const Value: boolean);
begin
  if FPrintInitialXYZ_Heads <> Value then
  begin
    FPrintInitialXYZ_Heads := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintInitial.SetPrintInitialXYZ_SteadyFlowVelocities(
  const Value: boolean);
begin
  if FPrintInitialXYZ_SteadyFlowVelocities <> Value then
  begin
    FPrintInitialXYZ_SteadyFlowVelocities := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintInitial.SetPrintInitialXYZ_Wells(const Value: boolean);
begin
  if FPrintInitialXYZ_Wells <> Value then
  begin
    FPrintInitialXYZ_Wells := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

{ TGridOptions }

procedure TGridOptions.Assign(Source: TPersistent);
begin
  if Source is TGridOptions then
  begin
    with TGridOptions(Source) do
    begin
      self.ChemicalDimensionX := ChemicalDimensionX;
      self.ChemicalDimensionY := ChemicalDimensionY;
      self.ChemicalDimensionZ := ChemicalDimensionZ;
      self.PrintOrientation := PrintOrientation;
    end;
  end
  else
  begin
    inherited;
  end;
end;

constructor TGridOptions.Create;
begin
  ChemicalDimensionX := True;
  ChemicalDimensionY := True;
  ChemicalDimensionZ := True;
end;

procedure TGridOptions.SetChemicalDimensionX(const Value: boolean);
begin
  if FChemicalDimensionX <> Value then
  begin
    FChemicalDimensionX := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TGridOptions.SetChemicalDimensionY(const Value: boolean);
begin
  if FChemicalDimensionY <> Value then
  begin
    FChemicalDimensionY := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TGridOptions.SetChemicalDimensionZ(const Value: boolean);
begin
  if FChemicalDimensionZ <> Value then
  begin
    FChemicalDimensionZ := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TGridOptions.SetPrintOrientation(
  const Value: TpgPrintOrientation);
begin
  if FPrintOrientation <> Value then
  begin
    FPrintOrientation := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

{ TFluidProperties }

procedure TFluidProperties.Assign(Source: TPersistent);
begin
  if Source is TFluidProperties then
  begin
    with TFluidProperties(Source) do
    begin
      self.FluidCompressibility := FluidCompressibility;
      self.FluidDensity := FluidDensity;
      self.FluidDiffusivity := FluidDiffusivity;
      self.FluidViscosity := FluidViscosity;
    end;
  end
  else
  begin
    inherited;
  end;
end;

constructor TFluidProperties.Create(AOwner: TComponent);
begin
  inherited;
  Initialize;
end;

procedure TFluidProperties.Initialize;
begin
  FFluidCompressibility := 4.7E-10;
  FFluidDensity := 1000;
  FFluidDiffusivity := 1E-9;
  FFluidViscosity := 0.00115;
  FCompressibilitySet := False;
  FDensitySet := False;
  FDiffusivitySet := False;
  FViscositySet := False;
end;

procedure TFluidProperties.Loaded;
begin
  inherited;
  if not FCompressibilitySet then
  begin
    FFluidCompressibility := 0;
  end;
  if not FDensitySet then
  begin
    FFluidDensity := 0;
  end;
  if not FDiffusivitySet then
  begin
    FFluidDiffusivity := 0;
  end;
  if not FViscositySet then
  begin
    FFluidViscosity := 0;
  end;
end;

procedure TFluidProperties.SetFluidCompressibility(const Value: double);
begin
  if FFluidCompressibility <> Value then
  begin
    FFluidCompressibility := Value;
    frmGoPhast.InvalidateModel;
  end;
  FCompressibilitySet := True;
end;

procedure TFluidProperties.SetFluidDensity(const Value: Double);
begin
  if FFluidDensity <> Value then
  begin
    FFluidDensity := Value;
    frmGoPhast.InvalidateModel;
  end;
  FDensitySet := True;
end;

procedure TFluidProperties.SetFluidDiffusivity(const Value: double);
begin
  if FFluidDiffusivity <> Value then
  begin
    FFluidDiffusivity := Value;
    frmGoPhast.InvalidateModel;
  end;
  FDiffusivitySet := True;
end;

procedure TFluidProperties.SetFluidViscosity(const Value: double);
begin
  if FFluidViscosity <> Value then
  begin
    FFluidViscosity := Value;
    frmGoPhast.InvalidateModel;
  end;
  FViscositySet := True;
end;

{ TSolutionOptions }

procedure TSolutionOptions.Assign(Source: TPersistent);
begin
  if Source is TSolutionOptions then
  begin
    with TSolutionOptions(Source) do
    begin
      self.SolverType := SolverType;
      self.SpaceDifferencing := SpaceDifferencing;
      self.TimeDifferencing := TimeDifferencing;
      self.CrossDispersion := CrossDispersion;
      self.Tolerance := Tolerance;
      self.SaveDirections := SaveDirections;
      self.MaximumIterations := MaximumIterations;
      self.RebalanceByCell := RebalanceByCell;
      self.RebalanceFraction := RebalanceFraction;
    end;
  end
  else
  begin
    inherited;
  end;
end;

constructor TSolutionOptions.Create(AOwner: TComponent);
begin
  inherited;
  FRebalanceFraction := TRealStorage.Create;
  Initialize;
end;

destructor TSolutionOptions.Destroy;
begin
  FRebalanceFraction.Free;
  inherited;
end;

procedure TSolutionOptions.Initialize;
begin
  SolverType := psIterative;
  FSpaceDifferencing := 0;
  FTimeDifferencing := 1;
  FCrossDispersion := False;
  FTolerance := 1E-10;
  SaveDirections := 10;
  MaximumIterations := 500;
  FTimeDifferencingSet := False;
  FToleranceSet := False;
  RebalanceByCell := False;
  FRebalanceFraction.Value := 0.5;
end;

procedure TSolutionOptions.Loaded;
begin
  inherited;
  if not FTimeDifferencingSet then
  begin
    FTimeDifferencing := 0;
  end;
  if not FToleranceSet then
  begin
    FTolerance := 0;
  end;
end;

procedure TSolutionOptions.SetCrossDispersion(const Value: boolean);
begin
  if FCrossDispersion <> Value then
  begin
    FCrossDispersion := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TSolutionOptions.SetMaximumIterations(const Value: integer);
begin
  if FMaximumIterations <> Value then
  begin
    FMaximumIterations := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TSolutionOptions.SetRebalanceByCell(const Value: boolean);
begin
  if FRebalanceByCell <> Value then
  begin
    FRebalanceByCell := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TSolutionOptions.SetRebalanceFraction(const Value: TRealStorage);
begin
  if FRebalanceFraction.Value <> Value.Value then
  begin
    FRebalanceFraction.Assign(Value);
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TSolutionOptions.SetSaveDirections(const Value: integer);
begin
  if FSaveDirections <> Value then
  begin
    FSaveDirections := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TSolutionOptions.SetSolverType(const Value: TPhastSolver);
begin
  if FSolverType <> Value then
  begin
    FSolverType := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TSolutionOptions.SetSpaceDifferencing(const Value: double);
begin
  Assert((Value >= 0) and (Value <= 0.5));
  if FSpaceDifferencing <> Value then
  begin
    FSpaceDifferencing := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TSolutionOptions.SetTimeDifferencing(const Value: double);
begin
  Assert((Value >= 0.5) and (Value <= 1));
  if FTimeDifferencing <> Value then
  begin
    FTimeDifferencing := Value;
    frmGoPhast.InvalidateModel;
  end;
  FTimeDifferencingSet := True;
end;

procedure TSolutionOptions.SetTolerance(const Value: double);
begin
  if FTolerance <> Value then
  begin
    FTolerance := Value;
    frmGoPhast.InvalidateModel;
  end;
  FToleranceSet := True;
end;

{ TSteadyFlowOptions }

procedure TSteadyFlowOptions.Assign(Source: TPersistent);
begin
  if Source is TSteadyFlowOptions then
  begin
    with TSteadyFlowOptions(Source) do
    begin
      self.SteadyFlow := SteadyFlow;
      self.HeadTolerance := HeadTolerance;
      self.FlowBalanceTolerance := FlowBalanceTolerance;
      self.MinimumTimeStep := MinimumTimeStep;
      self.MaximumTimeStep := MaximumTimeStep;
      self.HeadChangeLimit := HeadChangeLimit;
      self.UseDefaultMinimumTimeStep := UseDefaultMinimumTimeStep;
      self.UseDefaultMaximumTimeStep := UseDefaultMaximumTimeStep;
      self.UseDefaultHeadChangeLimit := UseDefaultHeadChangeLimit;
      self.Iterations := Iterations;
    end;
  end
  else
  begin
    inherited;
  end;
end;

constructor TSteadyFlowOptions.Create;
begin
  HeadTolerance := 1E-5;
  FlowBalanceTolerance := 0.001;
  MinimumTimeStep := 1;
  MaximumTimeStep := 1000;
  HeadChangeLimit := 1;
  FUseDefaultHeadChangeLimit := True;
  FUseDefaultMaximumTimeStep := True;
  FUseDefaultMinimumTimeStep := True;
  FIterations := 100;
end;

procedure TSteadyFlowOptions.SetFlowBalanceTolerance(const Value: double);
begin
  if FFlowBalanceTolerance <> Value then
  begin
    FFlowBalanceTolerance := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TSteadyFlowOptions.SetHeadChangeLimit(const Value: double);
begin
  if FHeadChangeLimit <> Value then
  begin
    FHeadChangeLimit := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TSteadyFlowOptions.SetHeadTolerance(const Value: double);
begin
  if FHeadTolerance <> Value then
  begin
    FHeadTolerance := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TSteadyFlowOptions.SetIterations(const Value: integer);
begin
  if FIterations <> Value then
  begin
    FIterations := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TSteadyFlowOptions.SetMaximumTimeStep(const Value: double);
begin
  if FMaximumTimeStep <> Value then
  begin
    FMaximumTimeStep := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TSteadyFlowOptions.SetMinimumTimeStep(const Value: double);
begin
  if FMinimumTimeStep <> Value then
  begin
    FMinimumTimeStep := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TSteadyFlowOptions.SetSteadyFlow(const Value: boolean);
begin
  if FSteadyFlow <> Value then
  begin
    FSteadyFlow := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TSteadyFlowOptions.SetUseDefaultHeadChangeLimit(
  const Value: boolean);
begin
  if FUseDefaultHeadChangeLimit <> Value then
  begin
    FUseDefaultHeadChangeLimit := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TSteadyFlowOptions.SetUseDefaultMaximumTimeStep(
  const Value: boolean);
begin
  if FUseDefaultMaximumTimeStep <> Value then
  begin
    FUseDefaultMaximumTimeStep := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TSteadyFlowOptions.SetUseDefaultMinimumTimeStep(
  const Value: boolean);
begin
  if FUseDefaultMinimumTimeStep <> Value then
  begin
    FUseDefaultMinimumTimeStep := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

{ TChemistryOptions }

procedure TChemistryOptions.Assign(Source: TPersistent);
begin
  if Source is TChemistryOptions then
  begin
    with TChemistryOptions(Source) do
    begin
      self.UseKineticReactants := UseKineticReactants;
      self.UseGasPhases := UseGasPhases;
      self.UseEquilibriumPhases := UseEquilibriumPhases;
      self.UseExchange := UseExchange;
      self.UseSolidSolution := UseSolidSolution;
      self.UseSurfaceAssemblages := UseSurfaceAssemblages;
    end;
  end
  else
  begin
    inherited;
  end;
end;

constructor TChemistryOptions.Create;
begin
  FUseSolidSolution := True;
  FUseKineticReactants := True;
  FUseGasPhases := True;
  FUseEquilibriumPhases := True;
  FUseExchange := True;
  FUseSurfaceAssemblages := True;
end;

procedure TChemistryOptions.SetUseEquilibriumPhases(const Value: boolean);
begin
  if FUseEquilibriumPhases <> Value then
  begin
    FUseEquilibriumPhases := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TChemistryOptions.SetUseExchange(const Value: boolean);
begin
  if FUseExchange <> Value then
  begin
    FUseExchange := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TChemistryOptions.SetUseGasPhases(const Value: boolean);
begin
  if FUseGasPhases <> Value then
  begin
    FUseGasPhases := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TChemistryOptions.SetUseKineticReactants(const Value: boolean);
begin
  if FUseKineticReactants <> Value then
  begin
    FUseKineticReactants := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TChemistryOptions.SetUseSolidSolution(const Value: boolean);
begin
  if FUseSolidSolution <> Value then
  begin
    FUseSolidSolution := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TChemistryOptions.SetUseSurfaceAssemblages(const Value: boolean);
begin
  if FUseSurfaceAssemblages <> Value then
  begin
    FUseSurfaceAssemblages := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

{ TTimeListGroup }

function TTimeListGroup.Add(const TimeList: TPhastTimeList): integer;
begin
  result := FList.Add(TimeList);
end;

function TTimeListGroup.Count: integer;
begin
  result := FList.Count;
end;

constructor TTimeListGroup.Create;
begin
  FList := TList.Create;
end;

destructor TTimeListGroup.Destroy;
begin
  FList.Free;
  inherited;
end;

function TTimeListGroup.GetItems(const Index: integer): TPhastTimeList;
begin
  result := FList[Index];
end;

function TTimeListGroup.IndexOf(const TimeList: TPhastTimeList): integer;
begin
  result := FList.IndexOf(TimeList);
end;

procedure TTimeListGroup.SetItems(const Index: integer;
  const Value: TPhastTimeList);
begin
  FList[Index] := Value;
end;

procedure TTimeListGroup.SetName(const Value: string);
begin
  FName := Value;
end;

{ TProgramLocations }

procedure TProgramLocations.Assign(Source: TPersistent);
var
  SourceLocations: TProgramLocations;
begin
  if Source is TProgramLocations then
  begin
    SourceLocations := TProgramLocations(Source);
    ModflowLocation := SourceLocations.ModflowLocation;
    TextEditorLocation := SourceLocations.TextEditorLocation;
    ModPathLocation := SourceLocations.ModPathLocation;
    ModelMonitorLocation := SourceLocations.ModelMonitorLocation;
    PhastLocation := SourceLocations.PhastLocation;
    ZoneBudgetLocation := SourceLocations.ZoneBudgetLocation;
    ModelMateLocation := SourceLocations.ModelMateLocation;
  end
  else
  begin
    inherited;
  end;
end;

constructor TProgramLocations.Create;
var
  ADirectory: string;
begin
  FModflowLocation := StrModflowDefaultPath;
  FModPathLocation := StrMpathDefaultPath;
  PhastLocation := StrPhastDefaultPath;
  ZoneBudgetLocation := StrZoneBudgetDefaultPath;
  ADirectory := GetCurrentDir;
  try
    SetCurrentDir(ExtractFileDir(ParamStr(0)));
    FModelMonitorLocation :=
      ExpandFileName(StrModelMonitorDefaultPath);
  finally
    SetCurrentDir(ADirectory);
  end;
end;

procedure TProgramLocations.ReadFromIniFile(IniFile: TMemInifile);
var
  ADirectory: string;
  DefaultLocation: string;
begin
  ModflowLocation := IniFile.ReadString(StrProgramLocations, StrMODFLOW2005,
    StrModflowDefaultPath);
  TextEditorLocation := IniFile.ReadString(StrProgramLocations,
    StrTextEditor, '');
  ModPathLocation := IniFile.ReadString(StrProgramLocations, StrMODPATH,
    StrMpathDefaultPath);
  PhastLocation := IniFile.ReadString(StrProgramLocations, StrPHAST,
    StrPhastDefaultPath);
  ZoneBudgetLocation := IniFile.ReadString(StrProgramLocations, StrZonebudget,
    StrZoneBudgetDefaultPath);
  ModelMateLocation := IniFile.ReadString(StrProgramLocations, StrModelMate,
    StrModelMateDefaultPath);

  ADirectory := GetCurrentDir;
  try
    SetCurrentDir(ExtractFileDir(ParamStr(0)));
    DefaultLocation :=
      ExpandFileName(StrModelMonitorDefaultPath);
    ModelMonitorLocation := IniFile.ReadString(StrProgramLocations,
      StrModelMonitor, DefaultLocation);
  finally
    SetCurrentDir(ADirectory);
  end;
end;

function TProgramLocations.RemoveQuotes(const Value: string): string;
begin
  result := Trim(Value);
  if Length(result) > 0 then
  begin
    if result[1] = '"' then
    begin
      result := Copy(result, 2, MAXINT);
    end;
    if Length(result) > 0 then
    begin
      if result[Length(result)] = '"' then
      begin
        result := Copy(result, 1, Length(result) - 1);
      end;
    end;
  end;
end;

function TProgramLocations.GetTextEditorLocation: string;
begin
  if FTextEditorLocation = '' then
  begin
    result := StrNotepadexe;
  end
  else
  begin
    result := FTextEditorLocation
  end;
end;

procedure TProgramLocations.SetModelMateLocation(const Value: string);
begin
  FModelMateLocation := RemoveQuotes(Value);
end;

procedure TProgramLocations.SetModelMonitorLocation(const Value: string);
begin
  FModelMonitorLocation := RemoveQuotes(Value);
end;

procedure TProgramLocations.SetModflowLocation(const Value: string);
begin
  FModflowLocation := RemoveQuotes(Value);
end;

procedure TProgramLocations.SetModPathLocation(const Value: string);
begin
  FModPathLocation := RemoveQuotes(Value);
end;

procedure TProgramLocations.SetPhastLocation(const Value: string);
begin
  FPhastLocation := RemoveQuotes(Value);
end;

procedure TProgramLocations.SetZoneBudgetLocation(const Value: string);
begin
  FZoneBudgetLocation := RemoveQuotes(Value);;
end;

procedure TProgramLocations.WriteToIniFile(IniFile: TMemInifile);
begin
  IniFile.WriteString(StrProgramLocations, StrMODFLOW2005, ModflowLocation);
  IniFile.WriteString(StrProgramLocations, StrTextEditor, TextEditorLocation);
  IniFile.WriteString(StrProgramLocations, StrMODPATH, ModPathLocation);
  IniFile.WriteString(StrProgramLocations, StrModelMonitor, ModelMonitorLocation);
  IniFile.WriteString(StrProgramLocations, StrPHAST, PhastLocation);
  IniFile.WriteString(StrProgramLocations, StrZonebudget, ZoneBudgetLocation);
  IniFile.WriteString(StrProgramLocations, StrModelMate, ModelMateLocation);
end;

{ TDataSetClassification }

constructor TDataSetClassification.Create(ADataArray: TDataArray);
begin
  inherited Create;
  FDataArray := ADataArray;
end;

function TDataSetClassification.FullClassification: string;
begin
  result := FDataArray.FullClassification;
end;

function TDataSetClassification.ClassificationName: string;
begin
  result := FDataArray.Name;
end;

{ TLookUpList }

constructor TLookUpList.Create;
begin
  inherited;
  FLastIndex := -1;
end;

procedure TCustomModel.SetUpToDate(const Value: boolean);
begin
  FUpToDate := Value;
//  if not FUpToDate then
//  begin
//    FreeAndNil(FSortedObjectList);
//  end;
end;

constructor TCustomModel.Create(AnOwner: TComponent);
begin
  inherited;
  FPValFile := TStringList.Create;
  FTemplate := TStringList.Create;
  FTimeLists := TList.Create;
  FTransientZoneArrays := TObjectList.Create;
  FTransientMultiplierArrays := TObjectList.Create;
  FDataArrayManager:= TDataArrayManager.Create(self);
  FParsers := TList.Create;
  FrpTopFormulaCompiler := TRbwParser.Create(self);
  FParsers.Add(FrpTopFormulaCompiler);
  FrpFrontFormulaCompiler := TRbwParser.Create(self);
  FParsers.Add(FrpFrontFormulaCompiler);
  FrpSideFormulaCompiler := TRbwParser.Create(self);
  FParsers.Add(FrpSideFormulaCompiler);
  FrpThreeDFormulaCompiler := TRbwParser.Create(self);
  FParsers.Add(FrpThreeDFormulaCompiler);
  FrpTopFormulaCompilerNodes := TRbwParser.Create(self);
  FParsers.Add(FrpTopFormulaCompilerNodes);
  FrpFrontFormulaCompilerNodes := TRbwParser.Create(self);
  FParsers.Add(FrpFrontFormulaCompilerNodes);
  FrpSideFormulaCompilerNodes := TRbwParser.Create(self);
  FParsers.Add(FrpSideFormulaCompilerNodes);
  FrpThreeDFormulaCompilerNodes := TRbwParser.Create(self);
  FParsers.Add(FrpThreeDFormulaCompilerNodes);

//  FDataSetsToCache:= TList.Create;
//  FDataSets := TObjectList.Create;
//  FBoundaryDataSets := TObjectList.Create;
//  FDataSetFunctions := TStringList.Create;
  FModflowNameFileLines := TStringList.Create;
  FBatchFileAdditionsBeforeModel := TStringList.Create;
  FBatchFileAdditionsAfterModel := TStringList.Create;
  FModflowPackages := TModflowPackages.Create(self);
  FModflowPackages.LpfPackage.IsSelected := True;
  FModflowGrid := TModflowGrid.Create;

  FHeadFluxObservations := TFluxObservationGroups.Create(self);
  FRiverObservations := TFluxObservationGroups.Create(self);
  FDrainObservations := TFluxObservationGroups.Create(self);
  FGhbObservations := TFluxObservationGroups.Create(self);

  FHeadFluxObservations.FluxObservationType := fotHead;
  FRiverObservations.FluxObservationType := fotRiver;
  FDrainObservations.FluxObservationType := fotDrain;
  FGhbObservations.FluxObservationType := fotGHB;

  FHydrogeologicUnits := THydrogeologicUnits.Create(self);
  FFilesToArchive := TStringList.Create;
  FModelInputFiles := TStringList.Create;
  FModflowWettingOptions := TWettingOptions.Create(Self);
  FGlobalVariables := TGlobalVariables.Create(self);
//  FChangedDataArrayNames := TStringList.Create;

  FTopGridObserver:= TObserver.Create(nil);
  FThreeDGridObserver:= TObserver.Create(nil);
  FTopGridObserver.Name := 'TopGridObserver';
  FThreeDGridObserver.Name := 'ThreeDGridObserver';

  FTopGridObserver.TalksTo(FThreeDGridObserver);

  FHufKxNotifier := TObserver.Create(nil);
  FHufKxNotifier.Name := 'HufKxNotifier';
  FHufKyNotifier := TObserver.Create(nil);
  FHufKyNotifier.Name := 'HufKyNotifier';
  FHufKzNotifier := TObserver.Create(nil);
  FHufKzNotifier.Name := 'HufKzNotifier';
  FHufSsNotifier := TObserver.Create(nil);
  FHufSsNotifier.Name := 'HufSsNotifier';
  FHufSyNotifier := TObserver.Create(nil);
  FHufSyNotifier.Name := 'HufSyNotifier';

  FUnitNumbers := TUnitNumbers.Create(self);

  FHfbDisplayer:= THfbDisplayer.Create(nil);
  FHfbDisplayer.OnNeedToUpdate := UpdateHfb;
end;

destructor TCustomModel.Destroy;
begin
//  FChangedDataArrayNames.Free;
//  FBoundaryDataSets.Free;
//  FDataSets.Free;
//  FDataSetsToCache.Free;
  FModflowWettingOptions.Free;
  FFilesToArchive.Free;
  FModelInputFiles.Free;
  FModflowGrid.Free;
  FModflowPackages.Free;
  FBatchFileAdditionsAfterModel.Free;
  FBatchFileAdditionsBeforeModel.Free;
  FModflowNameFileLines.Free;
  FHeadFluxObservations.Free;
  FRiverObservations.Free;
  FDrainObservations.Free;
  FGhbObservations.Free;
  FHydrogeologicUnits.Free;
  FParsers.Free;
  FGlobalVariables.Free;
  FDataArrayManager.Free;
  FTransientMultiplierArrays.Free;
  FTransientZoneArrays.Free;
  FTimeLists.Free;
  FMfHobHeads.Free;
  FHfbDisplayer.Free;
  FHfbWriter.Free;
  FUnitNumbers.Free;
  FPValFile.Free;
  FTemplate.Free;
  inherited;
end;

function TCustomModel.EquilibriumPhasesUsed(Sender: TObject): boolean;
begin
  result := ChemistryUsed(Sender) and ChemistryOptions.UseEquilibriumPhases;
end;

procedure TCustomModel.Invalidate;
begin
  UpToDate := False;
end;

procedure TCustomModel.SetModflowGrid(const Value: TModflowGrid);
begin
  FModflowGrid.Assign(Value);
  Invalidate;
end;

procedure TCustomModel.SetModflowPackages(const Value: TModflowPackages);
begin
  FModflowPackages.Assign(Value);
end;

procedure TCustomModel.SetAlternateSolver(const Value: boolean);
begin
  if FAlternateSolver <> Value then
  begin
    FAlternateSolver := Value;
    Invalidate;
  end;
end;

procedure TCustomModel.SetAlternateFlowPackage(const Value: boolean);
begin
  if FAlternateFlowPackage <> Value then
  begin
    FAlternateFlowPackage := Value;
    Invalidate;
  end;
end;

procedure TCustomModel.SetBatchFileAdditionsBeforeModel(const Value: TStrings);
begin
  if not FBatchFileAdditionsBeforeModel.Equals(Value) then
  begin
    Invalidate;
  end;
  FBatchFileAdditionsBeforeModel.Assign(Value);
end;

procedure TCustomModel.SetBatchFileAdditionsAfterModel(const Value: TStrings);
begin
  if not FBatchFileAdditionsAfterModel.Equals(Value) then
  begin
    Invalidate;
  end;
  FBatchFileAdditionsAfterModel.Assign(Value);
end;

procedure TCustomModel.SetModflowNameFileLines(const Value: TStrings);
begin
  FModflowNameFileLines.Assign(Value);
  Invalidate;
end;

procedure TCustomModel.SetHeadFluxObservations(const Value: TFluxObservationGroups);
begin
  FHeadFluxObservations.Assign(Value);
end;

procedure TCustomModel.SetRiverObservations(const Value: TFluxObservationGroups);
begin
  FRiverObservations.Assign(Value);
end;

procedure TCustomModel.SetDrainObservations(const Value: TFluxObservationGroups);
begin
  FDrainObservations.Assign(Value);
end;

procedure TCustomModel.SetGhbObservations(const Value: TFluxObservationGroups);
begin
  FGhbObservations.Assign(Value);
end;

procedure TCustomModel.SetHydrogeologicUnits(const Value: THydrogeologicUnits);
begin
  FHydrogeologicUnits.Assign(Value);
end;

procedure TCustomModel.SetFilesToArchive(const Value: TStrings);
begin
  FFilesToArchive.Assign(Value);
  Invalidate;
end;

procedure TCustomModel.SetModelInputFiles(const Value: TStrings);
begin
  FModelInputFiles.Assign(Value);
  Invalidate;
end;

procedure TCustomModel.SetFileName(const Value: string);
begin
  if FFileName <> Value then
  begin
    FFileName := Value;
    Invalidate;
  end;
end;

procedure TCustomModel.SetModflowWettingOptions(const Value: TWettingOptions);
begin
  FModflowWettingOptions.Assign(Value);
end;

procedure TCustomModel.AddModelInputFile(const FileName: string);
begin
  if ModelInputFiles.IndexOf(FileName) < 0 then
  begin
    ModelInputFiles.Add(FileName);
    Invalidate;
  end;
end;

procedure TCustomModel.AddFileToArchive(const FileName: string);
begin
  if FilesToArchive.IndexOf(FileName) < 0 then
  begin
    FilesToArchive.Add(FileName);
    Invalidate;
  end;
end;

procedure TCustomModel.InternalClear;
begin
  FDataArrayManager.ClearDataSetsToCache;
  ClearAllTimeLists;
end;

function TCustomModel.GetCompiler(const Orientation: TDataSetOrientation;
      const EvaluatedAt: TEvaluatedAt): TRbwParser;
begin
  result := nil;

  case EvaluatedAt of
    eaBlocks:
      begin
        case Orientation of
          dsoTop:
            begin
              result := rpTopFormulaCompiler;
            end;
          dsoFront:
            begin
              result := rpFrontFormulaCompiler;
            end;
          dsoSide:
            begin
              result := rpSideFormulaCompiler;
            end;
          dso3D:
            begin
              result := rpThreeDFormulaCompiler;
            end;
        else
          Assert(False);
        end;
      end;
    eaNodes:
      begin
        case Orientation of
          dsoTop:
            begin
              result := rpTopFormulaCompilerNodes;
            end;
          dsoFront:
            begin
              result := rpFrontFormulaCompilerNodes;
            end;
          dsoSide:
            begin
              result := rpSideFormulaCompilerNodes;
            end;
          dso3D:
            begin
              result := rpThreeDFormulaCompilerNodes;
            end;
        else
          Assert(False);
        end;
      end;
  else
    Assert(False);
  end;
end;

function TCustomModel.ParserCount: integer;
begin
  result := FParsers.Count;
end;

procedure TCustomModel.ClearParsers;
var
  Parser: TRbwParser;
  Index: Integer;
begin
  for Index := 0 to ParserCount - 1 do
  begin
    Parser := Parsers[Index];
    Parser.ClearExpressions;
    Parser.ClearVariables;
  end;
end;

procedure TCustomModel.ClearViewedItems;
begin
  FrontTimeList := nil;
  SideTimeList := nil;
  TopTimeList := nil;
  ThreeDTimeList := nil;
  FTopDisplayTime := 0;
  FFrontDisplayTime := 0;
  FSideDisplayTime := 0;
  FThreeDDisplayTime := 0;

  ModflowGrid.TopDataSet := nil;
  ModflowGrid.FrontDataSet := nil;
  ModflowGrid.SideDataSet := nil;
  ModflowGrid.ThreeDDataSet := nil;

  ModflowGrid.TopContourDataSet := nil;
  ModflowGrid.FrontContourDataSet := nil;
  ModflowGrid.SideContourDataSet := nil;
  ModflowGrid.ThreeDContourDataSet := nil;
end;

function TCustomModel.GetParsers(Index: integer): TRbwParser;
begin
  result := FParsers[Index];
end;

procedure TCustomModel.Clear;
begin
  FClearing := True;
  try
    InternalClear;
  finally
    FClearing := False;
  end;
end;

procedure TCustomModel.ClearExpressionsAndVariables;
begin
  rpTopFormulaCompiler.ClearExpressions;
  rpTopFormulaCompiler.ClearVariables;
  rpFrontFormulaCompiler.ClearExpressions;
  rpFrontFormulaCompiler.ClearVariables;
  rpSideFormulaCompiler.ClearExpressions;
  rpSideFormulaCompiler.ClearVariables;
  rpThreeDFormulaCompiler.ClearExpressions;
  rpThreeDFormulaCompiler.ClearVariables;
  rpTopFormulaCompilerNodes.ClearExpressions;
  rpTopFormulaCompilerNodes.ClearVariables;
  rpFrontFormulaCompilerNodes.ClearExpressions;
  rpFrontFormulaCompilerNodes.ClearVariables;
  rpSideFormulaCompilerNodes.ClearExpressions;
  rpSideFormulaCompilerNodes.ClearVariables;
  rpThreeDFormulaCompilerNodes.ClearExpressions;
  rpThreeDFormulaCompilerNodes.ClearVariables;
end;

procedure TCustomModel.FillCompilerList(CompilerList: TList);
begin
  CompilerList.Add(rpFrontFormulaCompiler);
  CompilerList.Add(rpFrontFormulaCompilerNodes);
  CompilerList.Add(rpSideFormulaCompiler);
  CompilerList.Add(rpSideFormulaCompilerNodes);
  CompilerList.Add(rpThreeDFormulaCompiler);
  CompilerList.Add(rpThreeDFormulaCompilerNodes);
  CompilerList.Add(rpTopFormulaCompiler);
  CompilerList.Add(rpTopFormulaCompilerNodes);
end;

procedure TCustomModel.FinalizePvalAndTemplate(FileName: string);
var
  TemplateFileName: string;
  PValFileName: string;
  Comment: string;
begin
  if FPValFile.Count > 0 then
  begin
    FPValFile.Insert(0, IntToStr(FPValFile.Count));
    FTemplate.Insert(0, IntToStr(FTemplate.Count));

    Comment := '# PVAL file created on ' + DateToStr(Now) + ' by '
      + ProgramName + ' version ' + ModelVersion + '.';
    FPValFile.Insert(0, Comment);
    FTemplate.Insert(0, Comment);

    FTemplate.Insert(0, 'jtf ' + UcodeDelimiter);

    PValFileName := ChangeFileExt(FileName, StrPvalExt);
    TemplateFileName := ChangeFileExt(FileName, StrJtf);

    FPValFile.SaveToFile(PValFileName);
    FTemplate.SaveToFile(TemplateFileName);

    TCustomModflowWriter.WriteToNameFile('PVAL',
      UnitNumbers.UnitNumber(StrPval), PValFileName, foInput);
  end;
end;

procedure TCustomModel.SetGlobalVariables(const Value: TGlobalVariables);
var
  OldVariables: TStringList;
  NewVariables: TStringList;
  CompilerList: TList;
  Variable: TGlobalVariable;
  OldVariable: TGlobalVariable;
  NewVariable: TGlobalVariable;
  Index: integer;
  NewIndex: integer;
  procedure RemoveVariable(Variable: TGlobalVariable);
  var
    Index: Integer;
    Compiler: TRbwParser;
    VariableIndex: integer;
    CompilerVariable: TCustomVariable;
  begin
    for Index := 0 to CompilerList.Count - 1 do
    begin
      Compiler := CompilerList[Index];
      VariableIndex := Compiler.IndexOfVariable(Variable.Name);
      Assert(VariableIndex >= 0);
      CompilerVariable := Compiler.Variables[VariableIndex] as TCustomVariable;
      Compiler.RemoveVariable(CompilerVariable);
    end;
    Variable.UpToDate := False;
  end;
  procedure UpdateVariable(Variable: TGlobalVariable);
  var
    CompilerIndex: Integer;
    Compiler: TRbwParser;
    VariableIndex: integer;
    CompilerVariable: TCustomVariable;
    ValueChanged: boolean;
    RealVariable: TRealVariable;
    IntegerVariable: TIntegerVariable;
    BooleanVariable: TBooleanVariable;
    StringVariable: TStringVariable;
  begin
    ValueChanged := False;
    for CompilerIndex := 0 to CompilerList.Count - 1 do
    begin
      Compiler := CompilerList[CompilerIndex];
      VariableIndex := Compiler.IndexOfVariable(Variable.Name);
      if VariableIndex < 0 then
      begin
        case Variable.Format of
          rdtDouble:
            begin
              Compiler.CreateVariable(Variable.Name, StrGlobalVariables,
                Variable.RealValue);
            end;
          rdtInteger:
            begin
              Compiler.CreateVariable(Variable.Name, StrGlobalVariables,
                Variable.IntegerValue);
            end;
          rdtBoolean:
            begin
              Compiler.CreateVariable(Variable.Name, StrGlobalVariables,
                Variable.BooleanValue);
            end;
          rdtString:
            begin
              Compiler.CreateVariable(Variable.Name, StrGlobalVariables,
                Variable.StringValue);
            end;
          else Assert(False);
        end;
      end
      else
      begin
        CompilerVariable := Compiler.Variables[VariableIndex]
          as TCustomVariable;
        case Variable.Format of
          rdtDouble:
            begin
              RealVariable := CompilerVariable as TRealVariable;
              if (CompilerIndex = 0)
                and (RealVariable.Value <> Variable.RealValue) then
              begin
                ValueChanged := True;
              end;
              RealVariable.Value := Variable.RealValue;
            end;
          rdtInteger:
            begin
              IntegerVariable := CompilerVariable as TIntegerVariable;
              if (CompilerIndex = 0)
                and (IntegerVariable.Value <> Variable.IntegerValue) then
              begin
                ValueChanged := True;
              end;
              IntegerVariable.Value := Variable.IntegerValue;
            end;
          rdtBoolean:
            begin
              BooleanVariable := CompilerVariable as TBooleanVariable;
              if (CompilerIndex = 0)
                and (BooleanVariable.Value <> Variable.BooleanValue) then
              begin
                ValueChanged := True;
              end;
              BooleanVariable.Value := Variable.BooleanValue;
            end;
          rdtString:
            begin
              StringVariable := CompilerVariable as TStringVariable;
              if (CompilerIndex = 0)
                and (StringVariable.Value <> Variable.StringValue) then
              begin
                ValueChanged := True;
              end;
              StringVariable.Value := Variable.StringValue;
            end;
          else Assert(False);
        end;
      end;
    end;
    if ValueChanged then
    begin
      Variable.UpToDate := False;
      Variable.UpToDate := True;
    end;
  end;
begin
  OldVariables := TStringList.Create;
  NewVariables := TStringList.Create;
  CompilerList := TList.Create;
  try
    FillCompilerList(CompilerList);

    for Index := 0 to FGlobalVariables.Count - 1 do
    begin
      Variable := FGlobalVariables[Index];
      OldVariables.AddObject(UpperCase(Variable.Name), Variable);
    end;
    for Index := 0 to Value.Count - 1 do
    begin
      Variable := Value[Index];
      NewVariables.AddObject(UpperCase(Variable.Name), Variable);
    end;
    OldVariables.Sort;
    NewVariables.Sort;
    for Index := 0 to OldVariables.Count - 1 do
    begin
      NewIndex := NewVariables.IndexOf(OldVariables[Index]);
      if NewIndex < 0 then
      begin
        Variable := OldVariables.Objects[Index] as TGlobalVariable;
        RemoveVariable(Variable);
      end
      else
      begin
        OldVariable := OldVariables.Objects[Index] as TGlobalVariable;
        NewVariable := NewVariables.Objects[NewIndex] as TGlobalVariable;
        if OldVariable.Format <> NewVariable.Format then
        begin
          RemoveVariable(OldVariable);
        end;
      end;
    end;

    for Index := 0 to NewVariables.Count - 1 do
    begin
      Variable := NewVariables.Objects[Index] as TGlobalVariable;
      UpdateVariable(Variable);
    end;

  finally
    CompilerList.Free;
    NewVariables.Free;
    OldVariables.Free;
  end;

  FGlobalVariables.Assign(Value);
end;

procedure TCustomModel.RefreshGlobalVariables(CompilerList: TList);
var
  Compiler: TRbwParser;
  Variable: TGlobalVariable;
  CompilerIndex: Integer;
  VariableIndex: Integer;
begin
  for CompilerIndex := 0 to CompilerList.Count - 1 do
  begin
    Compiler := CompilerList[CompilerIndex];
    for VariableIndex := 0 to GlobalVariables.Count - 1 do
    begin
      Variable := GlobalVariables[VariableIndex];
      case Variable.Format of
        rdtDouble:
          begin
            Compiler.CreateVariable(Variable.Name, StrGlobalVariables,
              Variable.RealValue);
          end;
        rdtInteger:
          begin
            Compiler.CreateVariable(Variable.Name, StrGlobalVariables,
              Variable.IntegerValue);
          end;
        rdtBoolean:
          begin
            Compiler.CreateVariable(Variable.Name, StrGlobalVariables,
              Variable.BooleanValue);
          end;
        rdtString:
          begin
            Compiler.CreateVariable(Variable.Name, StrGlobalVariables,
              Variable.StringValue);
          end;
      else
        Assert(False);
      end;
    end;
  end;
end;

procedure TCustomModel.CreateGlobalVariables;
var
  CompilerList: TList;
begin
  CompilerList := TList.Create;
  try
    FillCompilerList(CompilerList);
    RefreshGlobalVariables(CompilerList);
  finally
    CompilerList.Free;
  end;
end;

procedure TCustomModel.CreateVariables(const DataSet: TDataArray);
var
  TempCompiler: TRbwParser;
  Local3DCompiler: TRbwParser;
  VarIndex: integer;
  Variable: TCustomValue;
begin
  TempCompiler := GetCompiler(DataSet.Orientation,
    DataSet.EvaluatedAt);

  Local3DCompiler := nil;
  case DataSet.EvaluatedAt of
    eaBlocks:
      begin
        Local3DCompiler := rpThreeDFormulaCompiler;
      end;
    eaNodes:
      begin
        Local3DCompiler := rpThreeDFormulaCompilerNodes;
      end;
  else
    Assert(False);
  end;

  VarIndex := TempCompiler.IndexOfVariable(DataSet.Name);
  if VarIndex < 0 then
  begin
    case DataSet.Datatype of
      rdtDouble:
        begin
          TempCompiler.CreateVariable(DataSet.Name,
            DataSet.FullClassification, 0.0);
          if TempCompiler <> Local3DCompiler then
          begin
            Local3DCompiler.CreateVariable(DataSet.Name,
              DataSet.FullClassification, 0.0);
          end;
        end;
      rdtInteger:
        begin
          TempCompiler.CreateVariable(DataSet.Name,
            DataSet.FullClassification, 0);
          if TempCompiler <> Local3DCompiler then
          begin
            Local3DCompiler.CreateVariable(DataSet.Name,
              DataSet.FullClassification, 0);
          end;
        end;
      rdtBoolean:
        begin
          TempCompiler.CreateVariable(DataSet.Name,
            DataSet.FullClassification, False);
          if TempCompiler <> Local3DCompiler then
          begin
            Local3DCompiler.CreateVariable(DataSet.Name,
              DataSet.FullClassification, False);
          end;
        end;
      rdtString:
        begin
          TempCompiler.CreateVariable(DataSet.Name,
            DataSet.FullClassification, '');
          if TempCompiler <> Local3DCompiler then
          begin
            Local3DCompiler.CreateVariable(DataSet.Name,
              DataSet.FullClassification, '');
          end;
        end;
    else
      Assert(False);
    end;
  end
  else
  begin
    Variable := TempCompiler.Variables[VarIndex];
    Assert(Variable.Name = UpperCase(DataSet.Name));
    Assert(Variable.ResultType = DataSet.DataType);
    Variable.Classification := DataSet.FullClassification;
    if TempCompiler <> Local3DCompiler then
    begin
      VarIndex := Local3DCompiler.IndexOfVariable(DataSet.Name);
      Variable := Local3DCompiler.Variables[VarIndex];
      Assert(Variable.Name = UpperCase(DataSet.Name));
      Assert(Variable.ResultType = DataSet.DataType);
      Variable.Classification := DataSet.FullClassification;
    end;
  end;
end;

procedure TCustomModel.RemoveVariables(const DataSet: TDataArray);
var
  TempCompiler: TRbwParser;
  Local3DCompiler: TRbwParser;
  VarIndex: integer;
  Variable: TCustomVariable;
begin
  TempCompiler := GetCompiler(DataSet.Orientation,
    DataSet.EvaluatedAt);

  Local3DCompiler := nil;
  case DataSet.EvaluatedAt of
    eaBlocks:
      begin
        Local3DCompiler := rpThreeDFormulaCompiler;
      end;
    eaNodes:
      begin
        Local3DCompiler := rpThreeDFormulaCompilerNodes;
      end;
  else
    Assert(False);
  end;
  VarIndex := TempCompiler.IndexOfVariable(DataSet.Name);
  if VarIndex >= 0 then
  begin
    Variable := TempCompiler.Variables[VarIndex] as TCustomVariable;
    TempCompiler.RemoveVariable(Variable);
  end;
  if TempCompiler <> Local3DCompiler then
  begin
    VarIndex := Local3DCompiler.IndexOfVariable(DataSet.Name);
    if VarIndex >= 0 then
    begin
      Variable := Local3DCompiler.Variables[VarIndex] as TCustomVariable;
      Local3DCompiler.RemoveVariable(Variable);
    end;
  end;
end;

procedure TCustomModel.SetEdgeDisplay(const Value: TCustomModflowGridEdgeDisplay);
begin
  FEdgeDisplay := Value;
end;

procedure TCustomModel.OnActiveDataSetChanged(Sender: TObject);
var
  ActiveDataArray: TDataArray;
  DataArray: TDataArray;
begin
  ActiveDataArray := Sender as TDataArray;
  Assert(ActiveDataArray <> nil);
  Assert(ActiveDataArray.Name = rsActive);
  if Grid <> nil then
  begin
    DataArray := Grid.ThreeDDataSet;
    if (DataArray <> nil) and DataArray.Limits.ActiveOnly then
    begin
      if not ActiveDataArray.UpToDate then
      begin
        Grid.NeedToRecalculate3DCellColors := True;
        ThreeDGridObserver.UpToDate := False;
        ThreeDGridObserver.UpToDate := True;
      end;
    end
    else if (EdgeDisplay <> nil)
      and EdgeDisplay.Limits[EdgeDisplay.DataToPlot].ActiveOnly then
    begin
      if not ActiveDataArray.UpToDate then
      begin
        Grid.NeedToRecalculate3DCellColors := True;
        ThreeDGridObserver.UpToDate := False;
        ThreeDGridObserver.UpToDate := True;
      end;
    end;
  end;
end;

function TCustomModel.OptionalDataSet(Sender: TObject): boolean;
begin
  result := False;
end;

procedure TCustomModel.AllObserversStopTalking;
begin
  FTopGridObserver.StopTalkingToAnyone;
  FThreeDGridObserver.StopTalkingToAnyone;
  FHufSyNotifier.StopTalkingToAnyone;
  FHufSsNotifier.StopTalkingToAnyone;
  FHufKzNotifier.StopTalkingToAnyone;
  FHufKyNotifier.StopTalkingToAnyone;
  FHufKxNotifier.StopTalkingToAnyone;
end;

procedure TCustomModel.FreeHufNotifiers;
begin
  FHufSyNotifier.Free;
  FHufSsNotifier.Free;
  FHufKzNotifier.Free;
  FHufKyNotifier.Free;
  FHufKxNotifier.Free;
end;

procedure TCustomModel.FreeGridNotifiers;
begin
  FTopGridObserver.Free;
  FTopGridObserver := nil;
  FThreeDGridObserver.Free;
  FThreeDGridObserver := nil;
end;

{ TDataArrayManager }

function TDataArrayManager.AddBoundaryDataSet(
  const DataSet: TDataArray): Integer;
begin
  result := FBoundaryDataSets.Add(DataSet);
  Invalidate;
end;

function TDataArrayManager.AddDataSet(const DataSet: TDataArray): Integer;
begin
  result := FDataSets.Add(DataSet);
  Invalidate;
  AddDataSetToLookUpList(DataSet);
end;

procedure TDataArrayManager.AddDataSetToCache(DataArray: TDataArray);
begin
  if FDataSetsToCache.IndexOf(DataArray) < 0 then
  begin
    if (DataArray.Name <> '')
      and not (DataArray is TSparseArrayPhastInterpolationDataSet)
      and not (DataArray is TCustomSparseDataSet)
      then
    begin
      FDataSetsToCache.Add(DataArray)
    end;
  end;
end;

procedure TDataArrayManager.AddDataSetToLookUpList(const DataSet: TDataArray);
var
  Dummy: pointer;
begin
  if (FDataSetLookUpList <> nil) and (DataSet.Name <> '') then
  begin
    if not FDataSetLookUpList.Search(DataSet.Name, Dummy) then
    begin
      FDataSetLookUpList.Insert(DataSet.Name, DataSet);
    end;
  end;
end;

procedure TDataArrayManager.Assign(Source: TDataArrayManager);
var
  Index: Integer;
  DataArray: TDataArray;
  SourceDataArray: TDataArray;
  DA_Index: Integer;
begin
  Assert(Source <> self);
  for Index := DataSetCount - 1 downto 0 do
  begin
    DataArray := DataSets[Index];
    if Source.IndexOfDataSet(DataArray.Name) < 0 then
    begin
      FDataSets.Delete(Index);
    end;
  end;
  for Index := BoundaryDataSetCount - 1 downto 0 do
  begin
    DataArray := BoundaryDataSets[Index];
    if Source.IndexOfBoundaryDataSet(DataArray.Name) < 0 then
    begin
      FBoundaryDataSets.Delete(Index);
    end;
  end;
  for Index := 0 to Source.DataSetCount - 1 do
  begin
    SourceDataArray := Source.DataSets[Index];
    DataArray := GetDataSetByName(SourceDataArray.Name);
    if DataArray = nil then
    begin
      DataArray := TDataArrayType(SourceDataArray.ClassType).Create(FCustomModel);
      AddDataSet(DataArray);
    end;
    DataArray.AssignProperties(SourceDataArray);
  end;
  for Index := 0 to Source.BoundaryDataSetCount - 1 do
  begin
    SourceDataArray := Source.BoundaryDataSets[Index];
    DA_Index := IndexOfBoundaryDataSet(SourceDataArray.Name);
    if DA_Index < 0 then
    begin
      DataArray := TDataArrayType(SourceDataArray.ClassType).Create(FCustomModel);
      AddBoundaryDataSet(DataArray);
    end
    else
    begin
      DataArray := BoundaryDataSets[DA_Index];
    end;
    DataArray.AssignProperties(SourceDataArray);
  end;
end;

procedure TDataArrayManager.CacheDataArrays;
var
  Index: Integer;
  DataArray: TDataArray;
begin
  for Index := 0 to FDataSetsToCache.Count - 1 do
  begin
    DataArray := FDataSetsToCache[Index];
    if (DataArray <> FCustomModel.Grid.TopDataSet)
      and (DataArray <> FCustomModel.Grid.FrontDataSet)
      and (DataArray <> FCustomModel.Grid.SideDataSet)
      and (DataArray <> FCustomModel.Grid.ThreeDDataSet)
      and (DataArray <> FCustomModel.Grid.TopContourDataSet)
      and (DataArray <> FCustomModel.Grid.FrontContourDataSet)
      and (DataArray <> FCustomModel.Grid.SideContourDataSet)
      and (DataArray <> FCustomModel.Grid.ThreeDContourDataSet) then
    begin
      DataArray.CacheData;
    end;
  end;
  FDataSetsToCache.Clear;
end;

procedure TDataArrayManager.ClearAllDataSets;
begin
  FDataSetsToCache.Clear;
  FDataSets.Clear;
  FBoundaryDataSets.Clear;
end;

procedure TDataArrayManager.ClearDataSetsToCache;
begin
  FDataSetsToCache.Clear;
end;

procedure TDataArrayManager.ClearDeletedDataSets;
var
  Index: Integer;
begin
  ClearingDeletedDataSets := True;
  try
    FDeletedDataSets.Clear;
  finally
    ClearingDeletedDataSets := False;
  end;
  for Index := 0 to ChildDataArrayManagerCount - 1 do
  begin
    ChildDataArrayManagers[Index].ClearDeletedDataSets;
  end;
end;

constructor TDataArrayManager.Create(Model: TCustomModel);
begin
  FCustomModel := Model;
  FRiverDataSets := TList.Create;
  FDataSetsToCache:= TList.Create;
  FDataSets := TObjectList.Create;
  FBoundaryDataSets := TObjectList.Create;
  FDeletedDataSets := TObjectList.Create;
end;

procedure TDataArrayManager.CreateInitialBoundaryDataSets;
var
  PhastDataSet: TDataArray;
begin
  PhastDataSet := TRealSparseDataSet.Create(FCustomModel);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName(rsTopLeakyHydraulicConductivity);
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dso3D;
  PhastDataSet.Formula := '100.';
  PhastDataSet.CheckMin := True;
  PhastDataSet.Min := 0;
  AddBoundaryDataSet(PhastDataSet);

  PhastDataSet := TRealSparseDataSet.Create(FCustomModel);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName(rsTopLeakyThickness);
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dso3D;
  PhastDataSet.Formula := '1.';
  PhastDataSet.CheckMin := True;
  PhastDataSet.Min := 0;
  AddBoundaryDataSet(PhastDataSet);

  PhastDataSet := TRealSparseDataSet.Create(FCustomModel);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName(rsFrontLeakyHydraulicConductivity);
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dso3D;
  PhastDataSet.Formula := '100.';
  PhastDataSet.CheckMin := True;
  PhastDataSet.Min := 0;
  AddBoundaryDataSet(PhastDataSet);

  PhastDataSet := TRealSparseDataSet.Create(FCustomModel);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName(rsFrontLeakyThickness);
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dso3D;
  PhastDataSet.Formula := '1.';
  PhastDataSet.CheckMin := True;
  PhastDataSet.Min := 0;
  AddBoundaryDataSet(PhastDataSet);

  PhastDataSet := TRealSparseDataSet.Create(FCustomModel);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName(rsSideLeakyHydraulicConductivity);
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dso3D;
  PhastDataSet.Formula := '100.';
  PhastDataSet.CheckMin := True;
  PhastDataSet.Min := 0;
  AddBoundaryDataSet(PhastDataSet);

  PhastDataSet := TRealSparseDataSet.Create(FCustomModel);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName(rsSideLeakyThickness);
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dso3D;
  PhastDataSet.Formula := '1.';
  PhastDataSet.CheckMin := True;
  PhastDataSet.Min := 0;
  AddBoundaryDataSet(PhastDataSet);

  PhastDataSet := TRealSparseDataSet.Create(FCustomModel);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName(rsRiverHydraulicConductivity);
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dsoTop;
  PhastDataSet.Formula := '100.';
  PhastDataSet.CheckMin := True;
  PhastDataSet.Min := 0;
  AddBoundaryDataSet(PhastDataSet);
  RiverDataSets.Add(PhastDataSet);

  PhastDataSet := TRealSparseDataSet.Create(FCustomModel);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName(rsRiverWidth);
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dsoTop;
  PhastDataSet.Formula := '10.';
  PhastDataSet.CheckMin := True;
  PhastDataSet.Min := 0;
  AddBoundaryDataSet(PhastDataSet);
  RiverDataSets.Add(PhastDataSet);

  PhastDataSet := TRealSparseDataSet.Create(FCustomModel);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName(rsRiverDepth);
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dsoTop;
  PhastDataSet.Formula := '10.';
  PhastDataSet.CheckMin := True;
  PhastDataSet.Min := 0;
  AddBoundaryDataSet(PhastDataSet);
  RiverDataSets.Add(PhastDataSet);

  PhastDataSet := TRealSparseDataSet.Create(FCustomModel);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName(rsRiverBedThickness);
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dsoTop;
  PhastDataSet.Formula := '1.';
  PhastDataSet.CheckMin := True;
  PhastDataSet.Min := 0;
  AddBoundaryDataSet(PhastDataSet);
  RiverDataSets.Add(PhastDataSet);

  PhastDataSet := TIntegerSparseDataSet.Create(FCustomModel);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName(rsSolutionType);
  PhastDataSet.DataType := rdtInteger;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dso3D;
  PhastDataSet.Formula := '0';
  PhastDataSet.CheckMin := True;
  PhastDataSet.Min := 0;
  AddBoundaryDataSet(PhastDataSet);

end;

procedure TDataArrayManager.CreateInitialDataSets;
var
  DataArray: TDataArray;
  Index: Integer;
  DataSetName: string;
  Orientation: TDataSetOrientation;
  DataType: TRbwDataType;
  ArrayNeeded: TObjectUsedEvent;
  ArrayArrayShouldBeCreated: TObjectUsedEvent;
  NewFormula, Classification: string;
  Lock: TDataLock;
begin
  { TODO : Find a way to extract common code from
TPhastModel.CreateModflowDataSets and
TCustomCreateRequiredDataSetsUndo.UpdateDataArray}

  // See DefinePackageDataArrays for the definition of the
  // contents of DataArrayCreationRecords.
  for Index := 0 to Length(FDataArrayCreationRecords) - 1 do
  begin
    DataSetName := FDataArrayCreationRecords[Index].Name;
    Orientation := FDataArrayCreationRecords[Index].Orientation;
    DataType := FDataArrayCreationRecords[Index].DataType;
    ArrayNeeded := FDataArrayCreationRecords[Index].DataSetNeeded;
    ArrayArrayShouldBeCreated := FDataArrayCreationRecords[Index].DataSetShouldBeCreated;
    NewFormula := FDataArrayCreationRecords[Index].Formula;
    Classification := FDataArrayCreationRecords[Index].Classification;
    Lock := FDataArrayCreationRecords[Index].Lock;

    DataArray := GetDataSetByName(DataSetName);
    Assert(Assigned(ArrayNeeded));
    if DataArray <> nil then
    begin
      DataArray.Name := DataSetName;
      DataArray.Lock := Lock;
      DataArray.OnDataSetUsed := ArrayNeeded;
      FCustomModel.CreateVariables(DataArray);
    end
    else if ArrayNeeded(self)
      or (Assigned(ArrayArrayShouldBeCreated)
      and ArrayArrayShouldBeCreated(self)) then
    begin
      DataArray := CreateNewDataArray(
        FDataArrayCreationRecords[Index].DataSetType, DataSetName, NewFormula,
        Lock, DataType, FDataArrayCreationRecords[Index].EvaluatedAt,
        Orientation, Classification);
      DataArray.OnDataSetUsed := ArrayNeeded;
      DataArray.Lock := Lock;
      DataArray.CheckMax := FDataArrayCreationRecords[Index].CheckMax;
      DataArray.CheckMin := FDataArrayCreationRecords[Index].CheckMin;
      DataArray.Max := FDataArrayCreationRecords[Index].Max;
      DataArray.Min := FDataArrayCreationRecords[Index].Min;
    end;
    if DataArray <> nil then
    begin
      if FCustomModel.Grid <> nil then
      begin
        DataArray.UpdateDimensions(FCustomModel.Grid.LayerCount, FCustomModel.Grid.RowCount,
          FCustomModel.Grid.ColumnCount);
      end;
      DataArray.AssociatedDataSets := FDataArrayCreationRecords[
        Index].AssociatedDataSets;
    end;
  end;

  DataArray := GetDataSetByName(rsActive);
  if not Assigned(DataArray.OnUpToDateSet) then
  begin
    DataArray.OnUpToDateSet := FCustomModel.OnActiveDataSetChanged;
  end;
end;

function TDataArrayManager.CreateNewDataArray(const ClassType: TDataArrayType;
  const Name, Formula: string; Lock: TDataLock; DataType: TRbwDataType;
  EvaluatedAt: TEvaluatedAt; Orientation: TDataSetOrientation;
  const Classification: string): TDataArray;
begin
  result := ClassType.Create(FCustomModel);
  result.Lock := Lock;
  result.UpdateWithName(Name);
  result.DataType := DataType;
  result.EvaluatedAt := EvaluatedAt;
  result.Orientation := Orientation;
  result.Formula := Formula;
  result.Classification := Classification;
  AddDataSet(result);
  FCustomModel.CreateVariables(result);
end;

function TDataArrayManager.DataArrayHeld(DataArray: TDataArray): boolean;
begin
  result := FDataSets.IndexOf(DataArray) >= 0
end;

procedure TDataArrayManager.DefinePackageDataArrays;
  procedure NoCheck(var ARecord: TDataSetCreationData);
  begin
    ARecord.CheckMax := False;
    ARecord.CheckMin := False;
    ARecord.Max := 1;
    ARecord.Min := 0;
  end;
var
  Index: integer;
begin
  SetLength(FDataArrayCreationRecords, 62);
  Index := 0;

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtBoolean;
  FDataArrayCreationRecords[Index].Name := rsActive;
  FDataArrayCreationRecords[Index].Formula := 'True';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.AlwaysUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: MEDIA-active'#13#10'MODFLOW BAS: IBOUND';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsKx;
  FDataArrayCreationRecords[Index].Formula := '0.0001';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded :=
    FCustomModel.AquiferPropertiesUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: MEDIA-Kx'#13#10'MODFLOW LPF: HK'#13#10'MODFLOW BCF: TRAN,HY';
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsKy;
  FDataArrayCreationRecords[Index].Formula := rsKx;
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.KyUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: MEDIA-Ky'#13#10'MODFLOW LPF: HANI'#13#10'MODFLOW HUF and BCF: (not used)';
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsKz;
  FDataArrayCreationRecords[Index].Formula := rsKx + ' / 10';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.KzUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: MEDIA-Kz'#13#10'MODFLOW LPF: VKA'#13#10'MODFLOW BCF: Vcont';
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsPorosity;
  FDataArrayCreationRecords[Index].Formula := '0.25';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.PorosityUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: MEDIA-porosity'#13#10'MODPATH: POR';
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsSpecific_Storage;
  FDataArrayCreationRecords[Index].Formula := '1e-5';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded :=
    FCustomModel.SpecificStorageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: MEDIA-specific_storage'#13#10'MODFLOW LPF: Ss'#13#10'MODFLOW BCF: Sf1';
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsLong_Dispersivity;
  FDataArrayCreationRecords[Index].Formula := '10.';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ChemistryUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: MEDIA-longitudinal_dispersivity';
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsHorizontal_Transv_Dispersivity;
  FDataArrayCreationRecords[Index].Formula := '1.';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ChemistryUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: MEDIA-horizontal_dispersivity';
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsVertical_Transv_Dispersivity;
  FDataArrayCreationRecords[Index].Formula := '1.';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ChemistryUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: MEDIA-vertical_dispersivity';
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsInitial_Head;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.InitialHeadUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: HEAD_IC-head';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsInitial_Water_Table;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded :=
    FCustomModel.InitialWaterTableUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: HEAD_IC-water_table';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TIntegerPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsChemistry_Initial_Solution;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrChemistry;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ChemistryUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: CHEMISTRY_IC-solution';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TIntegerPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsChemistry_Initial_Equilibrium_Phases;
  FDataArrayCreationRecords[Index].Formula := '-1';
  FDataArrayCreationRecords[Index].Classification := StrChemistry;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.EquilibriumPhasesUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: CHEMISTRY_IC-equilibrium_phases';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TIntegerPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsChemistry_Initial_Surface;
  FDataArrayCreationRecords[Index].Formula := '-1';
  FDataArrayCreationRecords[Index].Classification := StrChemistry;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SurfacesUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: CHEMISTRY_IC-surface';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TIntegerPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsChemistry_Initial_Exchange;
  FDataArrayCreationRecords[Index].Formula := '-1';
  FDataArrayCreationRecords[Index].Classification := StrChemistry;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ExchangeUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: CHEMISTRY_IC-exchange';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TIntegerPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsChemistry_Initial_Gas_Phase;
  FDataArrayCreationRecords[Index].Formula := '-1';
  FDataArrayCreationRecords[Index].Classification := StrChemistry;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.GasPhaseUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: CHEMISTRY_IC-gas_phase';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TIntegerPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsChemistry_Initial_Solid_Solutions;
  FDataArrayCreationRecords[Index].Formula := '-1';
  FDataArrayCreationRecords[Index].Classification := StrChemistry;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SolidSolutionUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: CHEMISTRY_IC-solid_solutions';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TIntegerPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsChemistry_Initial_Kinetics;
  FDataArrayCreationRecords[Index].Formula := '-1';
  FDataArrayCreationRecords[Index].Classification := StrChemistry;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.KineticsUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: CHEMISTRY_IC-kinetics';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsPrint_Chemistry;
  FDataArrayCreationRecords[Index].Formula := '1';
  FDataArrayCreationRecords[Index].Classification := StrOutput;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ChemistryUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: PRINT_LOCATIONS-chemistry';
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsPrint_XYZ_Chemistry;
  FDataArrayCreationRecords[Index].Formula := '1';
  FDataArrayCreationRecords[Index].Classification := StrOutput;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ChemistryUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: PRINT_LOCATIONS-xyz_chemistry';
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  // Reservoir layers
  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsResLayer;
  FDataArrayCreationRecords[Index].Formula := '1';
  FDataArrayCreationRecords[Index].Classification := rsResClassificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ReservoirLayerUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'RES: ISRESL';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsResBottom;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := rsResClassificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ReservoirPackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'RES: BRES';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsResKv;
  FDataArrayCreationRecords[Index].Formula := '100.';
  FDataArrayCreationRecords[Index].Classification := rsResClassificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ReservoirPackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'RES: HCres';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsResBedThickness;
  FDataArrayCreationRecords[Index].Formula := '1.';
  FDataArrayCreationRecords[Index].Classification := rsResClassificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ReservoirPackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'RES: Rbthck';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  // Lake Layers
  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsLakeID;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := rsLakeClassificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.LakePackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'LAK: LKARR';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsLakeLeakance;
  FDataArrayCreationRecords[Index].Formula := '100.';
  FDataArrayCreationRecords[Index].Classification := rsLakeClassificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.LakePackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'LAK: BDLKNC';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  // UZF layers
  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrUzfLandSurface;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfPackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'UZF: IUZFBND (via the formula for ' + StrUzfLayer + ')';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtBoolean;
  FDataArrayCreationRecords[Index].Name := rsModflowSpecifiedHead;
  FDataArrayCreationRecords[Index].Formula := 'False';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ModflowUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'BAS: IBOUND';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := StrUzfLayer;
  FDataArrayCreationRecords[Index].Formula :=
    'If((ActiveOnLayer(ElevationToLayer('
      + StrUzfLandSurface + ')) AND NOT SpecifiedHeadOnLayer(ElevationToLayer('
      + StrUzfLandSurface + '))), ElevationToModelLayer('
      + StrUzfLandSurface + '), 0)';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfPackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'UZF: IUZFBND';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := StrUzfDischargeRouting;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.RouteUzfDischarge;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'UZF: IRUNBND';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrUzfVerticalK;
  FDataArrayCreationRecords[Index].Formula := '1.';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfUnsatVertKUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'UZF: VKS';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrUzfBrooksCoreyEpsilon;
  FDataArrayCreationRecords[Index].Formula := '3.5';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfPackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'UZF: EPS';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrUzfSaturatedWaterContent;
  FDataArrayCreationRecords[Index].Formula := '0.3';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfPackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'UZF: THTS';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrUzfInitialUnsaturatedWaterContent;
  FDataArrayCreationRecords[Index].Formula := '0.3';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfInitialInfiltrationUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'UZF: THTI';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := StrUzfGage_1_and_2;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfPackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'UZF: IUZOPT';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := StrUzfGage3;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfPackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'UZF: IUZOPT';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsModflow_Initial_Head;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ModflowInitialHeadUsed;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.ModflowUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'BAS: STRT';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsModflow_CBKz;
  FDataArrayCreationRecords[Index].Formula := rsKz;
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ConfiningBedKzUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'LPF: VKCB; BCF: VCONT';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsVerticalAnisotropy;
  FDataArrayCreationRecords[Index].Formula :=
    'If((' + rsKz + ' = 0.), 0, (' + rsKx + ' / ' + rsKz + '))';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded :=
    FCustomModel.VerticalAnisotropyUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'LPF: VKA';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsHorizontalAnisotropy;
  FDataArrayCreationRecords[Index].Formula :=
    'If((' + rsKx + ' = 0.), 1., (' + rsKy + ' / ' + rsKx + '))';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.HorizontalAnisotropyUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'LPF: HANI';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsSpecificYield;
  FDataArrayCreationRecords[Index].Formula := '0.2';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SpecificYieldUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'LPF: Sy; BCF: Sf1';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsWetDryThreshold;
  FDataArrayCreationRecords[Index].Formula := StrLayerHeight + ' * 0.1';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.WetDryUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'BCF, LPF, HUF: WETDRY';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsWetDryFlag;
  FDataArrayCreationRecords[Index].Formula := 'IfI(' + rsActive + ', -1, 0)';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.WetDryUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].CheckMax := True;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Max := 1;
  FDataArrayCreationRecords[Index].Min := -1;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'BCF, LPF, HUF: WETDRY'
    + #13#10#13#10'A value < 0 indicates that only the cell below the '
      + 'dry cell can cause the dry cell to become active again.'
    + #13#10#13#10'A value > 0 indicates that the cell below the dry cell '
      + 'or the cells next to the dry cell can cause the dry cell to become '
      + 'active again.'
    + #13#10#13#10'A value = 0 indicates that the dry cell can not become '
      +'active again.';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsWetDry;
  FDataArrayCreationRecords[Index].Formula := rsWetDryThreshold
    + ' * ' + rsWetDryFlag;
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.WetDryUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'BCF, LPF, HUF: WETDRY';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := StrModpathZone;
  FDataArrayCreationRecords[Index].Formula := '1';
  FDataArrayCreationRecords[Index].Classification := 'MODPATH';
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ModpathUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := False;
//  FDataArrayCreationRecords[Index].Max := 1;
//  FDataArrayCreationRecords[Index].Min := 0;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'MODPATH: IBOUND';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrHufReferenceSurface;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrHUF;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.HufReferenceSurfaceNeeded;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := False;
  FDataArrayCreationRecords[Index].Max := 1;
  FDataArrayCreationRecords[Index].Min := 0;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'MODFLOW, KDEP package: RS';
  Inc(Index);

  // BCF data sets.
  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrTransmissivity;
  FDataArrayCreationRecords[Index].Formula := rsKx + ' * ' + StrLayerHeight;
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.BcfUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Max := 1;
  FDataArrayCreationRecords[Index].Min := 0;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'BCF: Tran';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrVerticalConductance;
  FDataArrayCreationRecords[Index].Formula := StrBcfVCONT;
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.BcfUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Max := 1;
  FDataArrayCreationRecords[Index].Min := 0;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'BCF: VCONT';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrConfinedStorageCoe;
  FDataArrayCreationRecords[Index].Formula := rsSpecific_Storage + ' * ' + StrLayerHeight;
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ConfinedStorageCoefUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Max := 1;
  FDataArrayCreationRecords[Index].Min := 0;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'BCF: Sf1';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := 'HUF_Kx';
  FDataArrayCreationRecords[Index].Formula := StrHufKx + '(' + rsModflow_Initial_Head + ')';
  FDataArrayCreationRecords[Index].Classification := StrHUF;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.OptionalDataSet;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.HufSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'displays HUF Kx values';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := 'HUF_Ky';
  FDataArrayCreationRecords[Index].Formula := StrHufKy + '(' + rsModflow_Initial_Head + ')';
  FDataArrayCreationRecords[Index].Classification := StrHUF;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.OptionalDataSet;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.HufSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'displays HUF Ky values';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := 'HUF_Interlayer_Kz';
  FDataArrayCreationRecords[Index].Formula := StrHufKz + '(' + rsModflow_Initial_Head + ')';
  FDataArrayCreationRecords[Index].Classification := StrHUF;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.OptionalDataSet;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.HufSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'displays HUF Kz values between one layer and the layer beneath';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := 'HUF_SS';
  FDataArrayCreationRecords[Index].Formula := StrHufSs + '(' + rsModflow_Initial_Head + ')';
  FDataArrayCreationRecords[Index].Classification := StrHUF;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.OptionalDataSet;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.HufStorageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'displays HUF SS values';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := 'HUF_Average_SY';
  FDataArrayCreationRecords[Index].Formula := StrHufAverageSy + '(' + rsModflow_Initial_Head + ')';
  FDataArrayCreationRecords[Index].Classification := StrHUF;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.OptionalDataSet;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.HufStorageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'displays average HUF Sy values for a cell';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := 'HUF_SY';
  FDataArrayCreationRecords[Index].Formula := StrHufSy + '(' + rsModflow_Initial_Head + ')';
  FDataArrayCreationRecords[Index].Classification := StrHUF;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.OptionalDataSet;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.HufStorageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'displays HUF Sy values for a cell';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := 'HUF_SYTP';
  FDataArrayCreationRecords[Index].Formula := StrHufSytp;
  FDataArrayCreationRecords[Index].Classification := StrHUF;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.OptionalDataSet;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.HufStorageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'displays HUF SYTP values for the top active cell';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := StrZones;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrZonebudget;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ZoneBudgetSelected;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.ZoneBudgetSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'ZONEBUDGET Zone array (IZONE)';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrGeostaticStress;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrSubsidence;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SwtSelected;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.SwtSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'SWT: GL0 (data set 4)';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrSpecificGravityUns;
  FDataArrayCreationRecords[Index].Formula := '1.7';
  FDataArrayCreationRecords[Index].Classification := StrSubsidence;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SwtSelected;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.SwtSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'SWT: SGM (data set 5)';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrSpecificGravitySat;
  FDataArrayCreationRecords[Index].Formula := '2.';
  FDataArrayCreationRecords[Index].Classification := StrSubsidence;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SwtSelected;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.SwtSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'SWT: SGS (data set 6)';
  Inc(Index);




  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrInitialPreOffsets;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrSubsidence;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SwtOffsetsUsed;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.SwtOffsetsUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'SWT: PCSOFF (data set 14)';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrInitialPreconsolida;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrSubsidence;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SwtSpecifiedUsed;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.SwtSpecifiedUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'SWT: PCS (data set 15)';
  Inc(Index);



  Assert(Length(FDataArrayCreationRecords) = Index);
end;

destructor TDataArrayManager.Destroy;
var
  Index: Integer;
  DataSet: TDataArray;
begin
  for Index := 0 to FDeletedDataSets.Count - 1 do
  begin
    DataSet := FDeletedDataSets[Index];
    DataSet.StopTalkingToAnyone;
    DataSet.SetModelToNil;
  end;
  FDeletedDataSets.Free;
  FDataSetLookUpList.Free;
  FBoundaryDataSets.Free;
  FDataSets.Free;
  FDataSetsToCache.Free;
  FRiverDataSets.Free;
  inherited;
end;

procedure TDataArrayManager.DontCache(DataArray: TDataArray);
begin
  FDataSetsToCache.Remove(DataArray);
end;

procedure TDataArrayManager.ExtractDataSet(const DataSet: TDataArray);
begin
  FDataSets.Extract(DataSet);
  Invalidate;
  RemoveDataSetFromLookUpList(DataSet);
end;

function TDataArrayManager.GetBoundaryDataSetCount: integer;
begin
  result := FBoundaryDataSets.Count;
end;

function TDataArrayManager.GetBoundaryDataSets(
  const Index: integer): TDataArray;
begin
  result := FBoundaryDataSets[Index] as TDataArray;
end;

function TDataArrayManager.GetChildDataArrayManager(
  Index: integer): TDataArrayManager;
var
  PhastModel: TPhastModel;
begin
  PhastModel := FCustomModel as TPhastModel;
  result := PhastModel.ChildModels[Index].ChildModel.DataArrayManager;
end;

function TDataArrayManager.GetChildDataArrayManagerCount: integer;
var
  PhastModel: TPhastModel;
begin
  if FCustomModel is TPhastModel then
  begin
    PhastModel := TPhastModel(FCustomModel);
    result := PhastModel.ChildModels.Count;
  end
  else
  begin
    result := 0;
  end;
end;

function TDataArrayManager.GetDataSetByName(
  const DataSetName: string): TDataArray;
var
  Index: Integer;
  APointer: pointer;
  DataArray: TDataArray;
begin
  if DataSetCount = 0 then
  begin
    result := nil;
    Exit;
  end;
  if FDataSetLookUpList = nil then
  begin
    FDataSetLookUpList := THashTable.Create(false);
    FDataSetLookUpList.IgnoreCase := True;
    FDataSetLookUpList.TableSize := Max(211, DataSetCount*2-1);
    for Index := 0 to DataSetCount - 1 do
    begin
      DataArray := DataSets[Index];
      FDataSetLookUpList.Insert(DataArray.Name, DataArray)
    end;
  end;
  FDataSetLookUpList.Search(DataSetName, APointer);
  result := APointer;
end;

function TDataArrayManager.GetDataSetCount: integer;
begin
  if FDataSets = nil then
  begin
    result := 0;
  end
  else
  begin
    result := FDataSets.Count;
  end;
end;

function TDataArrayManager.GetDataSets(const Index: integer): TDataArray;
begin
  result := FDataSets[Index] as TDataArray;
end;

function TDataArrayManager.GetDataSetsCapacity: integer;
begin
  if FDataSets = nil then
  begin
    result := 0;
  end
  else
  begin
    result := FDataSets.Capacity;
  end;
end;

procedure TDataArrayManager.HandleAddedDataArrays(AddedDataSetList: TList);
var
  Index: Integer;
  DataArray: TDataArray;
  ChildIndex: Integer;
  ChildManager: TDataArrayManager;
  NewAddedList: TList;
  DataArrayIndex: Integer;
  ChildDataArray: TDataArray;
  DeletedIndex: Integer;
  TestDataArray: TDataArray;
begin
  for Index := 0 to AddedDataSetList.Count - 1 do
  begin
    DataArray := AddedDataSetList[Index];
    if GetDataSetByName(DataArray.Name) = nil then
    begin
      AddDataSet(DataArray);
      FCustomModel.CreateVariables(DataArray);
    end;
    FDeletedDataSets.Extract(DataArray);
  end;
  for ChildIndex := 0 to ChildDataArrayManagerCount - 1 do
  begin
    ChildManager := ChildDataArrayManagers[ChildIndex];
    NewAddedList := TList.Create;
    try
      NewAddedList.Capacity := AddedDataSetList.Count;
      for DataArrayIndex := 0 to AddedDataSetList.Count - 1 do
      begin
        DataArray := AddedDataSetList[DataArrayIndex];
        ChildDataArray := ChildManager.GetDataSetByName(DataArray.Name);
        if ChildDataArray = nil then
        begin
          for DeletedIndex := 0 to ChildManager.FDeletedDataSets.Count - 1 do
          begin
            TestDataArray := ChildManager.FDeletedDataSets[DeletedIndex];
            if DataArray.Name = TestDataArray.Name then
            begin
              ChildDataArray := TestDataArray;
              break;
            end;
          end;
        end;
        if ChildDataArray = nil then
        begin
          ChildDataArray := CreateNewDataArray(TDataArrayType(DataArray.ClassType),
            DataArray.Name, DataArray.Formula, DataArray.Lock,
            DataArray.DataType, DataArray.EvaluatedAt, DataArray.Orientation,
            DataArray.Classification);
          ChildDataArray.AssignProperties(DataArray);
        end;
        DataArray.TalksTo(ChildDataArray);
        NewAddedList.Add(ChildDataArray);
      end;
      ChildManager.HandleAddedDataArrays(NewAddedList);
    finally
      NewAddedList.Free;
    end;
  end;
end;

procedure TDataArrayManager.HandleDeletedDataArrays(DeletedDataSetList: TList);
var
  Index: Integer;
  DataArray: TDataArray;
  ChildIndex: Integer;
  ChildManager: TDataArrayManager;
  NewDeletedList: TList;
  DataArrayIndex: integer;
  ChildDataArray: TDataArray;
begin
  for Index := DeletedDataSetList.Count - 1 downto 0 do
  begin
    DataArray := DeletedDataSetList[Index];
    FCustomModel.RemoveVariables(DataArray);
    ExtractDataSet(DataArray);
  end;
  FDeletedDataSets.Assign(DeletedDataSetList, laOr);
  for ChildIndex := 0 to ChildDataArrayManagerCount - 1 do
  begin
    ChildManager := ChildDataArrayManagers[ChildIndex];
    NewDeletedList := TList.Create;
    try
      NewDeletedList.Capacity := DeletedDataSetList.Count;
      for DataArrayIndex := 0 to DeletedDataSetList.Count - 1 do
      begin
        DataArray := DeletedDataSetList[DataArrayIndex];
        ChildDataArray := ChildManager.GetDataSetByName(DataArray.Name);
        NewDeletedList.Add(ChildDataArray);
      end;
      ChildManager.HandleDeletedDataArrays(NewDeletedList);
    finally
      NewDeletedList.Free;
    end;
  end;
end;

function TDataArrayManager.IndexOfBoundaryDataSet(DataSetName: string): integer;
begin
  result := IndexOfDataSetInList(DataSetName, FBoundaryDataSets);
end;

function TDataArrayManager.IndexOfDataSet(DataSetName: string): integer;
begin
  result := IndexOfDataSetInList(DataSetName, FDataSets);
end;

function TDataArrayManager.IndexOfDataSetInList(DataSetName: string;
  const List: TObjectList): integer;
var
  Index: integer;
  DataSet: TDataArray;
begin
  result := -1;
  DataSetName := UpperCase(DataSetName);
  for Index := 0 to List.Count - 1 do
  begin
    DataSet := List[Index] as TDataArray;
    if UpperCase(DataSet.Name) = DataSetName then
    begin
      result := Index;
      Exit;
    end;
  end;
end;

procedure TDataArrayManager.Invalidate;
begin
  FCustomModel.Invalidate;
end;

procedure TDataArrayManager.InvalidateAllDataSets;
var
  Index: Integer;
  DS: TDataArray;
begin
  for Index := 0 to DataSetCount - 1 do
  begin
    DS := DataSets[Index];
    DS.Invalidate;
  end;
  for Index := 0 to BoundaryDataSetCount - 1 do
  begin
    DS := BoundaryDataSets[Index];
    DS.Invalidate;
  end;
  for Index := 0 to ChildDataArrayManagerCount - 1 do
  begin
    ChildDataArrayManagers[Index].InvalidateAllDataSets;
  end;
end;

procedure TDataArrayManager.InvalidateDataSetLookupList;
begin
  FreeAndNil(FDataSetLookUpList);
end;

procedure TDataArrayManager.RemoveDataSetFromLookUpList(
  const DataSet: TDataArray);
var
  Dummy: pointer;
begin
  if (FDataSetLookUpList <> nil) and (DataSet.Name <> '') then
  begin
    if FDataSetLookUpList.Search(DataSet.Name, Dummy) then
    begin
      FDataSetLookUpList.Delete(DataSet.Name);
    end;
  end;
end;

procedure TDataArrayManager.SetDataSetsCapacity(const Value: integer);
begin
  if FDataSets <> nil then
  begin
    FDataSets.Capacity := Value;
  end;
end;

procedure TDataArrayManager.UnlinkDeletedDataSets;
var
  Index: Integer;
  DataSet: TDataArray;
begin
  for Index := 0 to FDeletedDataSets.Count - 1 do
  begin
    DataSet := FDeletedDataSets[Index];
    DataSet.StopTalkingToAnyone;
    DataSet.SetModelToNil;
  end;
  for Index := 0 to ChildDataArrayManagerCount - 1 do
  begin
    ChildDataArrayManagers[Index].UnlinkDeletedDataSets;
  end;
end;

procedure TDataArrayManager.UpdateDataSetDimensions;
var
  Index: integer;
  DataSet: TDataArray;
  Grid: TCustomGrid;
begin
  Grid := FCustomModel.Grid;
  if Grid = nil then
  begin
    for Index := 0 to DataSetCount - 1 do
    begin
      DataSet := DataSets[Index];
      DataSet.UpdateDimensions(-1, -1, -1);
    end;
    for Index := 0 to BoundaryDataSetCount - 1 do
    begin
      DataSet := BoundaryDataSets[Index];
      DataSet.UpdateDimensions(-1, -1, -1);
    end;
    for Index := 0 to FDeletedDataSets.Count - 1 do
    begin
      DataSet := FDeletedDataSets[Index];
      DataSet.UpdateDimensions(-1, -1, -1);
    end;
  end
  else
  begin
    for Index := 0 to DataSetCount - 1 do
    begin
      DataSet := DataSets[Index];
      DataSet.UpdateDimensions(Grid.LayerCount,
        Grid.RowCount, Grid.ColumnCount);
    end;
    for Index := 0 to BoundaryDataSetCount - 1 do
    begin
      DataSet := BoundaryDataSets[Index];
      DataSet.UpdateDimensions(Grid.LayerCount,
        Grid.RowCount, Grid.ColumnCount);
    end;
    for Index := 0 to FDeletedDataSets.Count - 1 do
    begin
      DataSet := FDeletedDataSets[Index];
      DataSet.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
        Grid.ColumnCount);
    end;
    Grid.NeedToRecalculateTopCellColors := True;
    Grid.NeedToRecalculateFrontCellColors := True;
    Grid.NeedToRecalculateSideCellColors := True;
  end;
  for Index := 0 to ChildDataArrayManagerCount - 1 do
  begin
    ChildDataArrayManagers[Index].UpdateDataSetDimensions;
  end;
end;

function TCustomModel.AlwaysUsed(Sender: TObject): boolean;
begin
  result := True;
end;

function TCustomModel.AquiferPropertiesUsed(Sender: TObject): boolean;
begin
  result := (ModelSelection = msPhast)
    or not ModflowPackages.HufPackage.IsSelected;
end;

procedure TCustomModel.Assign(Source: TPersistent);
var
  SourceModel: TCustomModel;
begin
  if Source is TCustomModel then
  begin
    SourceModel := TCustomModel(Source);
    AlternateFlowPackage := SourceModel.AlternateFlowPackage;
    AlternateSolver := SourceModel.AlternateSolver;
    BatchFileAdditionsAfterModel := SourceModel.BatchFileAdditionsAfterModel;
    BatchFileAdditionsBeforeModel := SourceModel.BatchFileAdditionsBeforeModel;
    ModflowGrid := SourceModel.ModflowGrid;
    ModflowNameFileLines := SourceModel.ModflowNameFileLines;
    ModflowPackages := SourceModel.ModflowPackages;
    HeadFluxObservations := SourceModel.HeadFluxObservations;
    DrainObservations := SourceModel.DrainObservations;
    GhbObservations := SourceModel.GhbObservations;
    RiverObservations := SourceModel.RiverObservations;
    HydrogeologicUnits := SourceModel.HydrogeologicUnits;
    FilesToArchive := SourceModel.FilesToArchive;
    ModelInputFiles := SourceModel.ModelInputFiles;
    ModelFileName := SourceModel.ModelFileName;
    ModflowWettingOptions := SourceModel.ModflowWettingOptions;
    GlobalVariables := SourceModel.GlobalVariables;
  end
  else
  begin
    inherited;
  end;
end;

function TCustomModel.KineticsUsed(Sender: TObject): boolean;
begin
  result := ChemistryUsed(Sender) and ChemistryOptions.UseKineticReactants;
end;

function TCustomModel.KyUsed(Sender: TObject): boolean;
begin
  result := (ModelSelection = msPhast)
    or ModflowPackages.LpfPackage.IsSelected;
end;

function TCustomModel.KzUsed(Sender: TObject): boolean;
var
  LayerGroupIndex: Integer;
  LayerGroup: TLayerGroup;
  ParamIndex: Integer;
  Param: TModflowSteadyParameter;
  VK_Used: Boolean;
  VaniUsed: Boolean;
  VkcbUsed: Boolean;
begin
  result := True;
  case ModelSelection of
    msUndefined: result := True;
    msPhast: result := True;
    msModflow, msModflowLGR:
      begin
        if ModflowPackages.HufPackage.IsSelected then
        begin
          result := False;
          Exit;
        end;
        if ModflowPackages.LpfPackage.IsSelected then
        begin
          VK_Used := False;
          VaniUsed := False;
          VkcbUsed := False;
          for ParamIndex := 0 to ModflowSteadyParameters.Count - 1 do
          begin
            Param := ModflowSteadyParameters[ParamIndex];
            if Param.ParameterType = ptLPF_VK then
            begin
              VK_Used := True;
            end
            else if Param.ParameterType = ptLPF_VANI then
            begin
              VaniUsed := True;
            end
            else if Param.ParameterType = ptLPF_VKCB then
            begin
              VkcbUsed := True;
            end
          end;

          result := False;
          for LayerGroupIndex := 1 to LayerStructure.Count - 1 do
          begin
            LayerGroup := LayerStructure[LayerGroupIndex];
            if LayerGroup.Simulated then
            begin
              case LayerGroup.VerticalHydraulicConductivityMethod of
                0: result := not VK_Used; // 0 means use vertical hydraulic conductivity
                1: result := not VaniUsed; // 1 means user vertical anisotropy
                else Assert(False);
              end;
              if result then
              begin
                break;
              end;
            end
            else
            begin
              if not VkcbUsed then
              begin
                result := True;
                break;
              end;
            end;
          end;
        end;
      end;
    else Assert(False);
  end;
end;

function TCustomModel.PorosityUsed(Sender: TObject): boolean;
begin
  result := (ModelSelection = msPhast)
    or (ModelSelection in [msModflow, msModflowLGR])
    and ModflowPackages.ModPath.IsSelected;
end;

function TCustomModel.SpecificStorageUsed(Sender: TObject): boolean;
begin
  result := False;
  case ModelSelection of
    msUndefined: result := False;
    msPhast: result := True;
    msModflow, msModflowLGR:
      begin
        if ModflowPackages.LpfPackage.IsSelected
          or ModflowPackages.BcfPackage.IsSelected then
        begin
          result := ModflowStressPeriods.TransientModel;
        end
        else
        begin
          result := False;
        end;
      end
    else Assert(False);
  end;
end;

function TCustomModel.ChemistryUsed(Sender: TObject): boolean;
begin
  result := (ModelSelection = msPhast) and SoluteTransport;
end;

function TCustomModel.InitialWaterTableUsed(Sender: TObject): boolean;
begin
  result := (ModelSelection = msPhast) and FreeSurface and UseWaterTable;
end;

function TCustomModel.InitialHeadUsed(Sender: TObject): boolean;
begin
  result := (ModelSelection = msPhast) and not InitialWaterTableUsed(Sender);
end;

function TCustomModel.SurfacesUsed(Sender: TObject): boolean;
begin
  result := ChemistryUsed(Sender) and ChemistryOptions.UseSurfaceAssemblages;
end;

function TCustomModel.ExchangeUsed(Sender: TObject): boolean;
begin
  result := ChemistryUsed(Sender) and ChemistryOptions.UseExchange;
end;

function TCustomModel.GasPhaseUsed(Sender: TObject): boolean;
begin
  result := ChemistryUsed(Sender) and ChemistryOptions.UseGasPhases;
end;

function TCustomModel.SolidSolutionUsed(Sender: TObject): boolean;
begin
  result := ChemistryUsed(Sender) and ChemistryOptions.UseSolidSolution;
end;

function TCustomModel.ReservoirLayerUsed(Sender: TObject): boolean;
begin
  result := ModflowPackages.ResPackage.IsSelected
    and (ModflowPackages.ResPackage.LayerOption = loSpecified);
end;

function TCustomModel.ReservoirPackageUsed(Sender: TObject): boolean;
begin
  result := ModflowPackages.ResPackage.IsSelected;
end;

function TCustomModel.LakePackageUsed(Sender: TObject): boolean;
begin
  result := ModflowPackages.LakPackage.IsSelected;
end;

function TCustomModel.UzfPackageUsed(Sender: TObject): boolean;
begin
  result := ModflowPackages.UzfPackage.IsSelected;
end;

function TCustomModel.UzfUnsatVertKUsed(Sender: TObject): boolean;
begin
  result := UzfPackageUsed(Sender) and
    (ModflowPackages.UzfPackage.VerticalKSource = 1);
end;

function TCustomModel.ModflowUsed(Sender: TObject): boolean;
begin
  result := ModelSelection in [msModflow, msModflowLGR];
end;

function TCustomModel.RouteUzfDischarge(Sender: TObject): boolean;
begin
  result := UzfPackageUsed(Sender)
    and ModflowPackages.UzfPackage.RouteDischargeToStreams
    and (ModflowPackages.SfrPackage.IsSelected
    or ModflowPackages.LakPackage.IsSelected);
end;

function TCustomModel.UzfInitialInfiltrationUsed(Sender: TObject): boolean;
begin
  result := UzfPackageUsed(Sender);
  if result then
  begin
    if (Sender <> nil) and (Sender is TUndoChangePackageSelection) then
    begin
      result := True;
    end
    else
    begin
      result := ModflowStressPeriods.CompletelyTransient;
    end;
  end;
end;

function TCustomModel.ModflowInitialHeadUsed(Sender: TObject): boolean;
begin
  result := ModflowUsed(Sender) and (ModflowOptions.InitialHeadFileName = '');
end;

function TCustomModel.ConfiningBedKzUsed(Sender: TObject): boolean;
var
  UnitIndex: Integer;
  LayerGroup: TLayerGroup;
begin
  result := (ModflowPackages.LpfPackage.IsSelected and
    (ModflowSteadyParameters.CountParameters([ptLPF_VKCB]) = 0))
    or ModflowPackages.BcfPackage.IsSelected;
  if result then
  begin
    result := False;
    for UnitIndex := 1 to LayerStructure.Count - 1 do
    begin
      LayerGroup := LayerStructure[UnitIndex];
      result := not LayerGroup.Simulated;
      if result then break;
    end;
  end;
end;

function TCustomModel.VerticalAnisotropyUsed(Sender: TObject): boolean;
var
  UnitIndex: Integer;
  LayerGroup: TLayerGroup;
begin
  result := False;
  if ModflowPackages.LpfPackage.IsSelected then
  begin
    for UnitIndex := 1 to LayerStructure.Count - 1 do
    begin
      LayerGroup := LayerStructure[UnitIndex];
      if LayerGroup.Simulated then
      begin
        result := LayerGroup.VerticalHydraulicConductivityMethod <> 0;
        if result then break;
      end;
    end;
  end;
end;

function TCustomModel.HorizontalAnisotropyUsed(Sender: TObject): boolean;
begin
  result := (ModelSelection in [msModflow, msModflowLGR])
    and ModflowPackages.LpfPackage.IsSelected;
end;

function TCustomModel.SpecificYieldUsed(Sender: TObject): boolean;
var
  UnitIndex: Integer;
  LayerGroup: TLayerGroup;
begin
  result := (ModflowPackages.LpfPackage.IsSelected and
    (ModflowSteadyParameters.CountParameters([ptLPF_SY]) = 0))
    or ModflowPackages.BcfPackage.IsSelected;
  if result then
  begin
    result := ModflowStressPeriods.TransientModel
  end;
  if result then
  begin
    result := False;
    for UnitIndex := 1 to LayerStructure.Count - 1 do
    begin
      LayerGroup := LayerStructure[UnitIndex];
      if LayerGroup.Simulated then
      begin
        result := LayerGroup.AquiferType in [1,3];
        if result then break;
      end;
    end;
  end;
end;

function TCustomModel.WetDryUsed(Sender: TObject): boolean;
begin
  result := ModflowWettingOptions.WettingActive;
end;

function TCustomModel.ModpathUsed(Sender: TObject): boolean;
begin
  result := (ModelSelection in [msModflow, msModflowLGR])
    and ModflowPackages.ModPath.IsSelected;
end;

function TCustomModel.HufReferenceSurfaceNeeded(Sender: TObject): boolean;
begin
  { TODO : In LGR, this function may need to updated. }
  result := (ModelSelection in [msModflow, msModflowLGR])
    and ModflowPackages.HufPackage.IsSelected
    and (ModflowPackages.HufPackage.ReferenceChoice = hrcReferenceLayer)
    and (HufParameters.CountParameters([ptHUF_KDEP]) > 0);
end;

function TCustomModel.BcfUsed(Sender: TObject): boolean;
begin
  result := (ModelSelection in [msModflow, msModflowLGR])
    and ModflowPackages.BcfPackage.IsSelected
end;

function TCustomModel.ConfinedStorageCoefUsed(Sender: TObject): boolean;
begin
  result := False;
  case ModelSelection of
    msUndefined: result := False;
    msPhast: result := False;
    msModflow, msModflowLGR:
      begin
        if ModflowPackages.BcfPackage.IsSelected then
        begin
          result := ModflowStressPeriods.TransientModel;
        end
        else
        begin
          result := False;
        end;
      end
    else Assert(False);
  end;
end;

function TCustomModel.HufSelected(Sender: TObject): boolean;
begin
  result := (ModelSelection in [msModflow, msModflowLGR])
    and ModflowPackages.HufPackage.IsSelected
end;

function TCustomModel.HufStorageUsed(Sender: TObject): boolean;
begin
  result := HufSelected(Sender)
    and ModflowStressPeriods.TransientModel;
end;

function TCustomModel.ZoneBudgetSelected(Sender: TObject): boolean;
begin
  result := (ModelSelection in [msModflow, msModflowLGR])
    and ModflowPackages.ZoneBudget.IsSelected
end;

function TCustomModel.SwtSelected(Sender: TObject): boolean;
begin
  result := (ModelSelection in [msModflow, msModflowLGR])
    and ModflowPackages.SwtPackage.IsSelected
end;

function TCustomModel.SwtOffsetsUsed(Sender: TObject): boolean;
begin
  result := SwtSelected(Sender)
    and (ModflowPackages.SwtPackage.PreconsolidationSource = pcOffsets);
end;

function TCustomModel.SwtSpecifiedUsed(Sender: TObject): boolean;
begin
  result := SwtSelected(Sender)
    and (ModflowPackages.SwtPackage.PreconsolidationSource = pcSpecified);
end;

function TCustomModel.IndenticalTransientArray(DataArray: TDataArray; DataArrays: TList;
      var CachedIndex: integer): TDataArray;
var
  Index: Integer;
  ADataArray: TDataArray;
  CachedDataArray: TDataArray;
begin
  // On entry DataArrays is a list of temporary TDataArray's
  // that have already had their Hash's computed.
  // CachedIndex is the location in DataArrays of the last
  // TDataArray that was identified in DataArrays via this procedure.
  Assert(DataArray <> nil);
  result := nil;
  CachedDataArray := nil;
  try
  DataArray.ComputeHash;
  // First check the most likely result.
  if (CachedIndex > 0)
    and (CachedIndex < DataArrays.Count) then
  begin
    CachedDataArray := DataArrays[CachedIndex];
    if CachedDataArray.Hash = DataArray.Hash then
    begin
      // If the Hash's are identical it is extremely likely
      // but not certain that the data are identical too.
      // Compare the two arrays to make absolutely sure they are
      // the same.
      if DataArray.IdenticalDataArrayContents(CachedDataArray) then
      begin
        result := CachedDataArray;
        CachedDataArray := nil;
        Exit;
      end;
    end;
  end;
  // No identical array was found so check all the arrays in the list.
  for Index := 0 to DataArrays.Count - 1 do
  begin
    if Index = CachedIndex then
    begin
      // This one has already been checked so skip it.
      Continue;
    end;
    ADataArray := DataArrays[Index];
    if ADataArray.Hash = DataArray.Hash then
    begin
      if DataArray.IdenticalDataArrayContents(ADataArray) then
      begin
        result := ADataArray;
        CachedIndex := Index;
        Exit;
      end;
      // ADataArray had the same Hash as DataArray.
      // The data had to be restored to to compare it's contents
      // so Cache it again.
      ADataArray.CacheData;
    end;
  end;

  finally
    if (CachedDataArray <> nil) and (result <> nil) then
    begin
      // A different DataArray was found so cache the data
      // from the previous one.
      CachedDataArray.CacheData;
    end;
  end;
end;

function TCustomModel.IndenticalTransientMultiplierArray(DataArray: TDataArray): TDataArray;
begin
  result := IndenticalTransientArray(DataArray,
    TransientMultiplierArrays, FCachedMultiplierArrayIndex);
end;

function TCustomModel.IndenticalTransientZoneArray(DataArray: TDataArray): TDataArray;
begin
  result := IndenticalTransientArray(DataArray,
    TransientZoneArrays, FCachedZoneArrayIndex);
end;

procedure TCustomModel.UpdateModflowFullStressPeriods;
var
  TimeList: TRealList;
  TimeIndex: integer;
  StressPeriod, NewStressPeriod: TModflowStressPeriod;
  ScreenObjectIndex, StressPeriodIndex: Integer;
  ScreenObject: TScreenObject;
  StartTime, EndTime: double;
  FirstTime: Double;
  DeletedTimes: Boolean;
//  StartTimeIndex: integer;
begin
  if FUpdatingFullStressPeriods then
  begin
    Exit;
  end;
  frmErrorsAndWarnings.RemoveWarningGroup(StrAnyTimesAfterThe);
  frmErrorsAndWarnings.RemoveWarningGroup(StrAnyTimesBeforeThe);

  ModflowFullStressPeriods.Clear;
  TimeList := TRealList.Create;
  try
    FUpdatingFullStressPeriods := True;
    TimeList.Sort;
    for TimeIndex := 0 to ModflowStressPeriods.Count - 1 do
    begin
      StressPeriod := ModflowStressPeriods[TimeIndex];
      TimeList.AddUnique(StressPeriod.StartTime);
      TimeList.AddUnique(StressPeriod.EndTime);
    end;
    for ScreenObjectIndex := 0 to ScreenObjectCount - 1 do
    begin
      ScreenObject := ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      ScreenObject.UpdateModflowTimes(TimeList);
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
    end;

    DeletedTimes := False;
    StressPeriod := ModflowStressPeriods[0];
    FirstTime := 0;
    While TimeList.Count > 0 do
    begin
      if TimeList[0] < StressPeriod.StartTime then
      begin
        if not DeletedTimes then
        begin
          FirstTime := TimeList[0]
        end;
        TimeList.Delete(0);
        DeletedTimes := True;
      end
      else
      begin
        break;
      end;
    end;

    if DeletedTimes then
    begin
      frmErrorsAndWarnings.AddWarning(
        StrAnyTimesBeforeThe,
        'The beginning of the first stress period is '
        + FloatToStr(StressPeriod.StartTime)
        + '. The first defined time is '
        + FloatToStr(FirstTime) + '.');
    end;

    TimeIndex := 0;
    for StressPeriodIndex := 0 to ModflowStressPeriods.Count - 1 do
    begin
      if TimeIndex+1 >= TimeList.Count then
      begin
        break;
      end;
      StressPeriod := ModflowStressPeriods[StressPeriodIndex];
      StartTime := TimeList[TimeIndex];
      EndTime := TimeList[TimeIndex+1];
      While (EndTime <= StressPeriod.EndTime) do
      begin
        NewStressPeriod :=
          ModflowFullStressPeriods.Add as TModflowStressPeriod;
        NewStressPeriod.Assign(StressPeriod);
        NewStressPeriod.StartTime := StartTime;
        NewStressPeriod.EndTime := EndTime;
        NewStressPeriod.PeriodLength :=
          NewStressPeriod.EndTime - NewStressPeriod.StartTime;
        Inc(TimeIndex);
        if TimeIndex+1 >= TimeList.Count then
        begin
          break;
        end;
        StartTime := TimeList[TimeIndex];
        EndTime := TimeList[TimeIndex+1];
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
      end;
    end;

    StressPeriod := ModflowStressPeriods[ModflowStressPeriods.Count - 1];
    EndTime := TimeList[TimeList.Count -1];
    if EndTime > StressPeriod.EndTime then
    begin
      frmErrorsAndWarnings.AddWarning(
        StrAnyTimesAfterThe,
        'The end of the last stress period is '
        + FloatToStr(StressPeriod.EndTime)
        + '. The last defined time is '
        + FloatToStr(EndTime) + '.');
//      StartTimeIndex := -1;
//      for TimeIndex := TimeList.Count -1 downto 0 do
//      begin
//        EndTime := TimeList[TimeIndex];
//        if EndTime = StressPeriod.EndTime then
//        begin
//          StartTimeIndex := TimeIndex;
//          break;
//        end;
//        Assert(EndTime > StressPeriod.EndTime);
//        if not frmProgress.ShouldContinue then
//        begin
//          Exit;
//        end;
//      end;
//      for TimeIndex := StartTimeIndex to TimeList.Count - 2 do
//      begin
//        StartTime := TimeList[TimeIndex];
//        EndTime := TimeList[TimeIndex+1];
//        NewStressPeriod :=
//          ModflowFullStressPeriods.Add as TModflowStressPeriod;
//        NewStressPeriod.Assign(StressPeriod);
//        NewStressPeriod.StartTime := StartTime;
//        NewStressPeriod.EndTime := EndTime;
//        NewStressPeriod.PeriodLength :=
//          NewStressPeriod.EndTime - NewStressPeriod.StartTime;
//        if not frmProgress.ShouldContinue then
//        begin
//          Exit;
//        end;
//      end;
    end;

  finally
    TimeList.Free;
    FUpdatingFullStressPeriods := False;
  end;
end;

procedure TCustomModel.AddTimeList(TimeList: TCustomTimeList);
begin
  FTimeLists.Add(TimeList);
end;

procedure TCustomModel.RemoveTimeList(TimeList: TCustomTimeList);
begin
  FTimeLists.Remove(TimeList);
end;

function TCustomModel.GetTimeLists(Index: integer): TCustomTimeList;
begin
  result := FTimeLists[Index];
end;

function TCustomModel.GetTimeListCount: integer;
begin
  result := FTimeLists.Count;
end;

procedure TCustomModel.ClearAllTimeLists;
var
  Index: Integer;
  TimeList: TModflowBoundaryDisplayTimeList;
begin
  for Index := 0 to FTimeLists.Count - 1 do
  begin
    TimeList := FTimeLists[Index];
    TimeList.Invalidate;
    TimeList.Clear;
  end;
end;

procedure TPhastModel.SetObservationPurpose(const Value: TObservationPurpose);
begin
  if FObservationPurpose <> Value then
  begin
    FObservationPurpose := Value;
    Invalidate;
  end;
end;

procedure TCustomModel.InitializeHobDisplay(Sender: TObject);
var
  HobWriter: TModflowHobWriter;
  List: TModflowBoundListOfTimeLists;
begin
  MfHobHeads.CreateDataSets;

  List := TModflowBoundListOfTimeLists.Create;
  HobWriter := TModflowHobWriter.Create(Self);
  try
    List.Add(MfHobHeads);
    HobWriter.UpdateDisplay(List, [0], ObservationPurpose);
  finally
    HobWriter.Free;
    List.Free;
  end;
  MfHobHeads.ComputeAverage;
  if frmErrorsAndWarnings.HasMessages then
  begin
    frmErrorsAndWarnings.Show;
  end;
end;

function TCustomModel.ModflowHobPackageUsed(Sender: TObject): boolean;
begin
  result := ModflowPackages.HobPackage.IsSelected;
end;

procedure TCustomModel.GetMfHobHeadsUseList(Sender: TObject; NewUseList: TStringList);
begin
  NewUseList.Add(rsActive);
  // do nothing
end;

function TCustomModel.GetObserverByName(const ObserverName: string): TObserver;
begin
  result := FDataArrayManager.GetDataSetByName(ObserverName);
  if result = nil then
  begin
    result := GlobalVariables.GetVariableByName(ObserverName);
  end;
end;

procedure TCustomModel.CreateModflowDisplayTimeLists;
begin
  FMfHobHeads := THobDisplayTimeList.Create(self);
  FMfHobHeads.OnInitialize := InitializeHobDisplay;
  FMfHobHeads.OnGetUseList := GetMfHobHeadsUseList;
  FMfHobHeads.OnTimeListUsed := ModflowHobPackageUsed;
  FMfHobHeads.Name := StrMODFLOWHeadObservations;
  AddTimeList(FMfHobHeads);
end;

function TCustomModel.ProgramName: string;
begin
  result := StrModelName;
end;

procedure TCustomModel.SetUnitNumbers(const Value: TUnitNumbers);
begin
  FUnitNumbers.Assign(Value);
end;

function TCustomModel.PackageGeneratedExternally(const PackageName: string): boolean;
var
  Index: Integer;
  ALine: string;
  FirstChar: Integer;
  CharIndex: Integer;
  AChar: Char;
  LastChar: Integer;
  APackageName: string;
begin
  result := false;
  for Index := 0 to ModflowNameFileLines.Count - 1 do
  begin
    ALine := ModflowNameFileLines[Index];
    // skip comment lines
    if (Length(ALine) = 0) or (ALine[1] = '#') then
    begin
      Continue;
    end;
    ALine := Trim(ALine);
    FirstChar := Length(ALine)+1;
    for CharIndex := 1 to Length(ALine) do
    begin
      AChar := ALine[CharIndex];
      if not (AChar in [',', ' ']) then
      begin
        FirstChar := CharIndex;
        break;
      end;
    end;
    LastChar := Length(ALine);
    for CharIndex := FirstChar + 1 to Length(ALine) do
    begin
      AChar := ALine[CharIndex];
      if (AChar in [',', ' ']) then
      begin
        LastChar := CharIndex-1;
        break;
      end;
    end;
    APackageName := Copy(ALine, FirstChar, LastChar-FirstChar+1);
    if SameText(APackageName, PackageName) then
    begin
      result := True;
      Exit;
    end;
  end;
end;

procedure TCustomModel.WritePValAndTemplate(const ParameterName: string;
      const Value: double);
begin
  FPValFile.Add(ParameterName + ' ' + FortranFloatToStr(Value));
  FTemplate.Add(ParameterName + ' ' + UcodeDelimiter + ParameterName
    + '                  ' + UcodeDelimiter);
end;

procedure TCustomModel.UpdateHfb(Sender: TObject);
begin
  frmProgressMM.Caption := 'Progress';
  frmProgressMM.Show;
  if FHfbWriter = nil then
  begin
    FHfbWriter := TModflowHfb_Writer.Create(Self);
  end;
  (FHfbWriter as TModflowHfb_Writer).UpdateDisplay;
  frmProgressMM.Hide;
  if frmErrorsAndWarnings.HasMessages then
  begin
    frmErrorsAndWarnings.Show;
  end;
end;

function TCustomModel.GetDataSetCollection: TDataSetCollection;
var
  Index: integer;
  ADataSet: TDataArray;
  Item: TDataSetItem;
begin
  FDataSetCollection.Clear;
  for Index := 0 to FDataArrayManager.DataSetCount - 1 do
  begin
    ADataSet := FDataArrayManager.DataSets[Index];
    Item := FDataSetCollection.Add as TDataSetItem;
    Item.FDataSet := ADataSet;
    ADataSet.SetSubComponent(True);
  end;
  result := FDataSetCollection;
end;

function TCustomModel.CheckWetting: boolean;
var
  Group: TLayerGroup;
  LayerGroupIndex: Integer;
var
  WetErrorMessage: string;
begin
  result := False;
  frmErrorsAndWarnings.RemoveErrorGroup(WetError);
  if ModflowWettingOptions.WettingActive then
  begin
    result := True;
    for LayerGroupIndex := 1 to LayerStructure.Count - 1 do
    begin
      Group := LayerStructure[LayerGroupIndex];
      if Group.AquiferType in WettableLayers then
      begin
        result := False;
        break;
      end;
    end;
  end;
  if result then
  begin
    if frmGoPhast.PhastModel.ModflowPackages.LpfPackage.IsSelected
      or frmGoPhast.PhastModel.ModflowPackages.HufPackage.IsSelected then
    begin
      WetErrorMessage := 'At least one layer must be convertible.';
    end
    else if frmGoPhast.PhastModel.ModflowPackages.BcfPackage.IsSelected then
    begin
      WetErrorMessage :=
        'At least one layer must be unconfined or fully convertible.';
    end
    else
    begin
      Assert(False);
    end;
    frmErrorsAndWarnings.AddError(WetError, WetErrorMessage);
  end;
end;

function TCustomModel.FixFileName(AFileName: string): string;
var
  FileName: string;
  FileDir: string;
begin
  result := AFileName;
  FileName := ExtractFileName(AFileName);
  if Pos(' ', FileName) > 0 then
  begin
    FileName := StringReplace(FileName, ' ', '_', [rfReplaceAll]);
    FileDir := ExtractFileDir(AFileName);
    result := IncludeTrailingPathDelimiter(FileDir) + FileName;
  end;
end;

procedure TCustomModel.ExportModflowModel(FileName: string; RunModel: boolean);
var
  NameWriter: TNameFileWriter;
  DisWriter: TModflowDiscretizationWriter;
  BasicWriter: TModflowBasicWriter;
  PcgWriter: TPcgWriter;
  LPF_Writer: TModflowLPF_Writer;
  HUF_Writer: TModflowHUF_Writer;
  BCF_Writer: TModflowBCF_Writer;
  KDEP_Writer : TModflowKDEP_Writer;
  LVDA_Writer : TModflowLVDA_Writer;
  ChdWriter: TModflowCHD_Writer;
  GhbWriter: TModflowGHB_Writer;
  WellWriter: TModflowWEL_Writer;
  RivWriter: TModflowRIV_Writer;
  DrnWriter: TModflowDRN_Writer;
  DrtWriter: TModflowDRT_Writer;
  RchWriter: TModflowRCH_Writer;
  EvtWriter: TModflowEVT_Writer;
  EtsWriter: TModflowETS_Writer;
  ResWriter: TModflowRES_Writer;
  LakWriter: TModflowLAK_Writer;
  SfrWriter: TModflowSFR_Writer;
  Mnw2Writer: TModflowMNW2_Writer;
  ZoneWriter: TModflowZoneWriter;
  MultiplierWriter: TModflowMultiplierWriter;
  BatchFileLocation: string;
  UzfWriter: TModflowUzfWriter;
  GmgWriter: TGmgWriter;
  SipWriter: TSipWriter;
  De4Writer: TDe4Writer;
  OCWriter: TOutputControlWriter;
  GageUnitNumber: integer;
  Gages: TStringList;
  GagWriter: TModflowGAG_Writer;
  HobWriter: TModflowHobWriter;
  HfbWriter: TModflowHfb_Writer;
  ListFileName: string;
  Index: Integer;
  StepCount: Integer;
  StressPeriod: TModflowStressPeriod;
  NumberOfSteps: Integer;
  SubWriter: TModflowSUB_Writer;
  SwtWriter: TModflowSWT_Writer;
  HydModWriter: TModflowHydmodWriter;
  PIndex: Integer;
  SteadyParam: TModflowSteadyParameter;
  MultipliersUsed: Boolean;
  ZoneUsed: Boolean;
  HufUnitIndex: integer;
  HGU: THydrogeologicUnit;
  HufParam: THufUsedParameter;
begin
  CheckWetting;

  FPValFile.Clear;
  FTemplate.Clear;

  GageUnitNumber:= UnitNumbers.UnitNumber(StrUNIT);
  SetCurrentDir(ExtractFileDir(FileName));
  FileName := FixFileName(FileName);
  TransientMultiplierArrays.Clear;
  TransientZoneArrays.Clear;

  if frmProgressMM = nil then
  begin
    frmProgressMM := TfrmProgressMM.Create(nil);
  end;
  try
    frmFormulaErrors.DelayShowing := True;
    try
      frmProgressMM.Prefix := 'File ';
      frmProgressMM.Caption := 'Exporting MODFLOW input files';
      frmProgressMM.btnAbort.Visible := True;
      frmProgressMM.ShouldContinue := True;
      frmProgressMM.Show;

      // The following tasks are always required.
      // 1. Full Stress periods,
      // 2. Discretization,
      // 3. Basic,
      // 4. Output Control,
      // 5. Zone Arrays,
      // 6. Multiplier Arrays
      NumberOfSteps := 3;

      NumberOfSteps := NumberOfSteps + ModflowPackages.SelectedPackageCount;
      if ModflowPackages.SfrPackage.IsSelected
        or ModflowPackages.LakPackage.IsSelected then
      begin
        // gages
        Inc(NumberOfSteps)
      end;

      MultipliersUsed := False;
      ZoneUsed := False;
      if ModflowTransientParameters.Count > 0 then
      begin
        MultipliersUsed := True;
        ZoneUsed := True;
      end;

      if not MultipliersUsed then
      begin
        if ModflowPackages.LpfPackage.IsSelected then
        begin
          for PIndex := 0 to ModflowSteadyParameters.Count - 1 do
          begin
            SteadyParam := ModflowSteadyParameters[PIndex];
            if SteadyParam.UseMultiplier then
            begin
              MultipliersUsed := True;
            end;
            if SteadyParam.UseZone then
            begin
              ZoneUsed := True;
            end;
          end;
        end
        else if ModflowPackages.HufPackage.IsSelected then
        begin
          for HufUnitIndex := 0 to HydrogeologicUnits.Count - 1 do
          begin
            HGU := HydrogeologicUnits[HufUnitIndex];
            for PIndex := 0 to HGU.HufUsedParameters.Count - 1 do
            begin
              HufParam := HGU.HufUsedParameters[PIndex];
              if HufParam.UseMultiplier then
              begin
                MultipliersUsed := True;
              end;
              if HufParam.UseZone then
              begin
                ZoneUsed := True;
              end;
            end;
          end;
        end;

      end;
      if MultipliersUsed then
      begin
        Inc(NumberOfSteps)
      end;
      if ZoneUsed then
      begin
        Inc(NumberOfSteps)
      end;

      frmProgressMM.pbProgress.Max := NumberOfSteps;
      frmProgressMM.pbProgress.Position := 0;

      UpdateModflowFullStressPeriods;

      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      frmProgressMM.StepIt;

      StepCount := 0;
      for Index := 0 to ModflowFullStressPeriods.Count - 1 do
      begin
        StressPeriod := ModflowFullStressPeriods.Items[Index];
        StepCount := StepCount + StressPeriod.NumberOfSteps;
      end;
      if StepCount > 1000 then
      begin
        if MessageDlg('Your model has ' + IntToStr(StepCount)
          + ' time steps. Do you want to continue?', mtWarning,
          [mbYes, mbNo], 0) <> mrYes then
        begin
          Exit;
        end;
      end;

      UsedMultiplierArrayNames.Clear;
      UsedZoneArrayNames.Clear;
      UsedMultiplierArrayNames.Sorted := True;
      UsedZoneArrayNames.Sorted := True;
      Gages := TStringList.Create;
      NameWriter := TNameFileWriter.Create(self);
      try
        NameWriter.InitilizeNameFile(FileName, ListFileName);
        DisWriter := TModflowDiscretizationWriter.Create(self);
        try
          DisWriter.WriteFile(FileName);
        finally
          DisWriter.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        frmProgressMM.StepIt;

        BasicWriter := TModflowBasicWriter.Create(self);
        try
          BasicWriter.WriteFile(FileName);
        finally
          BasicWriter.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        frmProgressMM.StepIt;

        OCWriter := TOutputControlWriter.Create(self);
        try
          OCWriter.WriteFile(FileName);
        finally
          OCWriter.Free;
        end;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        frmProgressMM.StepIt;

        PcgWriter := TPcgWriter.Create(self);
        try
          PcgWriter.WriteFile(FileName);
        finally
          PcgWriter.Free;
        end;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.PcgPackage.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        GmgWriter := TGmgWriter.Create(self);
        try
          GmgWriter.WriteFile(FileName);
        finally
          GmgWriter.Free;
        end;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.GmgPackage.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        SipWriter := TSipWriter.Create(self);
        try
          SipWriter.WriteFile(FileName);
        finally
          SipWriter.Free;
        end;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.SipPackage.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        De4Writer := TDe4Writer.Create(self);
        try
          De4Writer.WriteFile(FileName);
        finally
          De4Writer.Free;
        end;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.De4Package.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        LPF_Writer := TModflowLPF_Writer.Create(self);
        try
          LPF_Writer.WriteFile(FileName);
        finally
          LPF_Writer.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.LpfPackage.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        BCF_Writer := TModflowBCF_Writer.Create(self);
        try
          BCF_Writer.WriteFile(FileName);
        finally
          BCF_Writer.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.BcfPackage.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        HUF_Writer := TModflowHUF_Writer.Create(self);
        try
          HUF_Writer.WriteFile(FileName);
        finally
          HUF_Writer.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.HufPackage.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        KDEP_Writer := TModflowKDEP_Writer.Create(self);
        try
          KDEP_Writer.WriteFile(FileName);
        finally
          KDEP_Writer.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.HufPackage.IsSelected
          and (HufParameters.CountParameters([ptHUF_KDEP]) > 0) then
        begin
          frmProgressMM.StepIt;
        end;

        LVDA_Writer := TModflowLVDA_Writer.Create(self);
        try
          LVDA_Writer.WriteFile(FileName);
        finally
          LVDA_Writer.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.HufPackage.IsSelected
          and (ModflowSteadyParameters.CountParameters([ptHUF_LVDA]) > 0) then
        begin
          frmProgressMM.StepIt;
        end;

        ChdWriter := TModflowCHD_Writer.Create(self);
        try
          ChdWriter.WriteFile(FileName);
          ChdWriter.WriteFluxObservationFile(FileName, ObservationPurpose);
        finally
          ChdWriter.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.ChdBoundary.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        GhbWriter := TModflowGHB_Writer.Create(self);
        try
          GhbWriter.WriteFile(FileName);
          GhbWriter.WriteFluxObservationFile(FileName, ObservationPurpose);
        finally
          GhbWriter.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.GhbBoundary.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        WellWriter := TModflowWEL_Writer.Create(self);
        try
          WellWriter.WriteFile(FileName);
        finally
          WellWriter.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.WelPackage.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        RivWriter := TModflowRIV_Writer.Create(self);
        try
          RivWriter.WriteFile(FileName);
          RivWriter.WriteFluxObservationFile(FileName, ObservationPurpose);
        finally
          RivWriter.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.RivPackage.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        DrnWriter := TModflowDRN_Writer.Create(self);
        try
          DrnWriter.WriteFile(FileName);
          DrnWriter.WriteFluxObservationFile(FileName, ObservationPurpose);
        finally
          DrnWriter.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.DrnPackage.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        DrtWriter := TModflowDRT_Writer.Create(self);
        try
          DrtWriter.WriteFile(FileName);
        finally
          DrtWriter.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.DrtPackage.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        RchWriter := TModflowRCH_Writer.Create(self);
        try
          RchWriter.WriteFile(FileName);
        finally
          RchWriter.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.RchPackage.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        EvtWriter := TModflowEVT_Writer.Create(self);
        try
          EvtWriter.WriteFile(FileName);
        finally
          EvtWriter.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.EvtPackage.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        EtsWriter := TModflowETS_Writer.Create(self);
        try
          EtsWriter.WriteFile(FileName);
        finally
          EtsWriter.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.EtsPackage.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        ResWriter := TModflowRES_Writer.Create(self);
        try
          ResWriter.WriteFile(FileName);
        finally
          ResWriter.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.ResPackage.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        Mnw2Writer := TModflowMNW2_Writer.Create(self);
        try
          Mnw2Writer.WriteFile(FileName);
          Mnw2Writer.WriteMnwiFile(FileName, GageUnitNumber);
        finally
          Mnw2Writer.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.Mnw2Package.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        LakWriter := TModflowLAK_Writer.Create(self);
        try
          LakWriter.WriteFile(FileName, GageUnitNumber, Gages);
        finally
          LakWriter.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.LakPackage.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        // SfrWriter requires that LakWriter be completed first
        // so that TScreenObject.ModflowLakBoundary.TrueLakeID
        // is set properly.
        SfrWriter := TModflowSFR_Writer.Create(self);
        try
          SfrWriter.WriteFile(FileName, GageUnitNumber, Gages);
          FDataArrayManager.CacheDataArrays;
          Application.ProcessMessages;
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
          if ModflowPackages.SfrPackage.IsSelected then
          begin
            frmProgressMM.StepIt;
          end;

          HydModWriter := TModflowHydmodWriter.Create(self);
          try
            HydModWriter.WriteFile(FileName, SfrWriter);
          finally
            HydModWriter.Free;
          end;
          FDataArrayManager.CacheDataArrays;
          Application.ProcessMessages;
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
          if ModflowPackages.HydmodPackage.IsSelected then
          begin
            frmProgressMM.StepIt;
          end;


          // GagWriter requires that LakWriter and SfrWriter be completed first
          // so that the data in Gages is set.
          GagWriter := TModflowGAG_Writer.Create(self);
          try
            GagWriter.WriteFile(FileName, Gages, SfrWriter, GageUnitNumber);
          finally
            GagWriter.Free;
          end;
          FDataArrayManager.CacheDataArrays;
          Application.ProcessMessages;
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
          if ModflowPackages.SfrPackage.IsSelected
            or ModflowPackages.LakPackage.IsSelected then
          begin
            frmProgressMM.StepIt;
          end;

        finally
          SfrWriter.Free;
        end;


        HfbWriter := TModflowHfb_Writer.Create(Self);
        try
          HfbWriter.WriteFile(FileName);
        finally
          HfbWriter.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.HfbPackage.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        UzfWriter := TModflowUzfWriter.Create(Self);
        try
          UzfWriter.WriteFile(FileName, GageUnitNumber);
        finally
          UzfWriter.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.UzfPackage.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        SubWriter := TModflowSUB_Writer.Create(Self);
        try
          SubWriter.WriteFile(FileName);
        finally
          SubWriter.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.SubPackage.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        SwtWriter := TModflowSWT_Writer.Create(Self);
        try
          SwtWriter.WriteFile(FileName);
        finally
          SwtWriter.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.SwtPackage.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;
        // It is important that the TModflowZoneWriter not be used
        // until after all the zones have been identified through the
        // export of TModflowLPF_Writer and any other packages that use
        // zone arrays.
        ZoneWriter := TModflowZoneWriter.Create(self);
        try
          ZoneWriter.WriteFile(FileName);
        finally
          ZoneWriter.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ZoneUsed then
        begin
          frmProgressMM.StepIt;
        end;

        // It is important that the TModflowMultiplierWriter not be used
        // until after all the multiplier arrays have been identified through the
        // export of TModflowLPF_Writer and any other packages that use
        // multiplier arrays.
        MultiplierWriter := TModflowMultiplierWriter.Create(self);
        try
          MultiplierWriter.WriteFile(FileName);
        finally
          MultiplierWriter.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if MultipliersUsed then
        begin
          frmProgressMM.StepIt;
        end;

        HobWriter := TModflowHobWriter.Create(Self);
        try
          HobWriter.WriteFile(FileName, ObservationPurpose);
        finally
          HobWriter.Free;
        end;
        FDataArrayManager.CacheDataArrays;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ModflowPackages.HobPackage.IsSelected then
        begin
          frmProgressMM.StepIt;
        end;

        FinalizePvalAndTemplate(FileName);

        NameWriter.SaveNameFile(FileName);
      finally
        NameWriter.Free;
        Gages.Free;
      end;

      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      BatchFileLocation := WriteModflowBatchFile(
        ProgramLocations,
        ChangeFileExt(FileName, '.nam'), ListFileName,
        ModflowOptions.OpenInTextEditor, BatchFileAdditionsBeforeModel,
        BatchFileAdditionsAfterModel);

      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      if RunModel then
      begin
        WinExec(PChar('"' + BatchFileLocation + '"'), SW_SHOW);
      end;
    finally
      frmProgressMM.btnAbort.Visible := False;
      frmProgressMM.Hide;
      if frmProgressMM.Owner = nil then
      begin
        FreeAndNil(frmProgressMM);
      end;
      frmFormulaErrors.DelayShowing := False;
//      ReclaimMemory;
    end;
  except on E: EFCreateError do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TCustomModel.ExportModpathModel(FileName: string;
      RunModel, NewBudgetFile: boolean);
var
  StartLocations: TModpathStartingLocationsWriter;
  MainFileWriter: TModpathMainFileWriter;
  TimeFileWriter: TModpathTimeFileWriter;
  Responses: TModpathResponseFileWriter;
  NameFileWriter: TModpathNameFileWriter;
  BatchFileLocation: string;
  LargeBudgetFileResponse: string;
begin
  if (ModflowOutputControl.SaveCellFlows = csfNone)
    or (ModflowOutputControl.BudgetFrequency <> 1)
    or (ModflowOutputControl.BudgetFrequencyChoice <> fcTimeSteps)
    or (not ModflowOutputControl.HeadOC.SaveInExternalFile)
    or (ModflowOutputControl.HeadOC.Frequency <> 1)
    or (ModflowOutputControl.HeadOC.FrequencyChoice <> fcTimeSteps)
    then
  begin
    Beep;
    if (MessageDlg('MODPATH requires that the heads and flows be saved for '
      + 'every time step of the model. That isn''t the case for this model. '
      + 'Do you want to export the MODPATH input anyway?',
      mtWarning, [mbYes, mbNo], 0, mbNo) <> mrYes) then
    begin
      frmGoPhast.miOutputControlClick(nil);
      Exit;
    end;
  end;
  if frmProgressMM = nil then
  begin
    frmProgressMM := TfrmProgressMM.Create(nil);
  end;
  try
    try
      frmProgressMM.ShouldContinue := True;
      UpdateModflowFullStressPeriods;
      SetCurrentDir(ExtractFileDir(FileName));
      FileName := FixFileName(FileName);

      NameFileWriter := TModpathNameFileWriter.Create;
      try
        NameFileWriter.WriteFile(FileName, self);
      finally
        NameFileWriter.Free;
      end;

      MainFileWriter := TModpathMainFileWriter.Create(Self);
      try
        MainFileWriter.WriteFile(FileName);
      finally
        MainFileWriter.Free;
      end;
      StartLocations := TModpathStartingLocationsWriter.Create(Self);
      try
        StartLocations.WriteFile(FileName);
      finally
        StartLocations.Free;
      end;
      if ModflowPackages.ModPath.ShouldCreateTimeFile then
      begin
        TimeFileWriter := TModpathTimeFileWriter.Create(Self);
        try
          TimeFileWriter.WriteFile(FileName);
        finally
          TimeFileWriter.Free;
        end;
      end;
      Responses := TModpathResponseFileWriter.Create(Self);
      try
        Responses.WriteFile(FileName, NewBudgetFile);
        LargeBudgetFileResponse := Responses.FLargeBudgetFileResponse;
      finally
        Responses.Free;
      end;

      BatchFileLocation := WriteModpathBatchFile(ProgramLocations, FileName,
        ChangeFileExt(FileName,'.mplst'), RunModel, LargeBudgetFileResponse);

      if RunModel then
      begin
        WinExec(PChar('"' + BatchFileLocation + '"'), SW_SHOW);
      end;
    except on E: EFCreateError do
      begin
        Beep;
        MessageDlg(E.Message, mtError, [mbOK], 0);
      end;
    end;
  finally
    if frmProgressMM.Owner = nil then
    begin
      FreeAndNil(frmProgressMM);
    end;
//    ReclaimMemory;
  end;
end;

procedure TCustomModel.ExportZoneBudgetModel(FileName: string; RunModel: boolean);
var
  NumberOfSteps: Integer;
  ZoneFileWriter: TZoneBudgetZoneFileWriter;
  ResponseFileWriter: TZoneBudgetResponseFileWriter;
  BatchFileLocation: string;
begin
  SetCurrentDir(ExtractFileDir(FileName));
  FileName := FixFileName(FileName);
  if frmProgressMM = nil then
  begin
    frmProgressMM := TfrmProgressMM.Create(nil);
  end;
  try
    try
      frmProgressMM.Prefix := 'File ';
      frmProgressMM.Caption := 'Exporting ZONEBUDGET input files';
      frmProgressMM.btnAbort.Visible := True;
      frmProgressMM.ShouldContinue := True;
      frmProgressMM.Show;

      // Export ZoneFile;
      // Export Response File;
      // Export Batch File;
      NumberOfSteps := 3;

      frmProgressMM.pbProgress.Max := NumberOfSteps;
      frmProgressMM.pbProgress.Position := 0;

      ZoneFileWriter := TZoneBudgetZoneFileWriter.Create(self);
      try
        ZoneFileWriter.WriteFile(FileName);
      finally
        ZoneFileWriter.Free;
      end;
      FDataArrayManager.CacheDataArrays;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      frmProgressMM.StepIt;

      ResponseFileWriter := TZoneBudgetResponseFileWriter.Create(self);
      try
        ResponseFileWriter.WriteFile(FileName);
      finally
        ResponseFileWriter.Free;
      end;
      FDataArrayManager.CacheDataArrays;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      frmProgressMM.StepIt;

      BatchFileLocation := WriteZoneBudgetBatchFile(self, FileName, RunModel);

      if RunModel then
      begin
        WinExec(PChar('"' + BatchFileLocation + '"'), SW_SHOW);
      end;
    except on E: EFCreateError do
      begin
        Beep;
        MessageDlg(E.Message, mtError, [mbOK], 0);
      end;
    end;
  finally
    frmProgressMM.btnAbort.Visible := False;
    frmProgressMM.Hide;
    if frmProgressMM.Owner = nil then
    begin
      FreeAndNil(frmProgressMM);
    end;
//    ReclaimMemory;
  end;
end;

procedure TCustomModel.FinalizeDischargeRouting(Sender: TObject);
var
  LakeIdArray: TDataArray;
  DischargeRoutingArray: TDataArray;
begin
  DischargeRoutingArray := FDataArrayManager.GetDataSetByName(StrUzfDischargeRouting);
  LakeIdArray := FDataArrayManager.GetDataSetByName(rsLakeID);
  if (LakeIdArray <> nil) and (DischargeRoutingArray <> nil) then
  begin
    DischargeRoutingArray.StopsTalkingTo(LakeIdArray);
  end;
end;

procedure TCustomModel.GetParameterUsedAndParameterFormulaForLPF(
      out ParameterUsed: Boolean; out ParameterFormula: string;
      ParameterType: TParameterType);
var
  Index: Integer;
  Item: TModflowSteadyParameter;
begin
  ParameterFormula := '';
  ParameterUsed := (ModelSelection in [msModflow, msModflowLGR])
    and ModflowPackages.LpfPackage.IsSelected;
  if ParameterUsed then
  begin
    ParameterUsed := False;
    for Index := 0 to ModflowSteadyParameters.Count - 1 do
    begin
      Item := ModflowSteadyParameters[Index];
      if Item.ParameterType = ParameterType then
      begin
        ParameterUsed := True;
        if ParameterFormula <> '' then
        begin
          ParameterFormula := ParameterFormula + ' + ';
        end;
        if Item.UseZone then
        begin
          ParameterFormula := ParameterFormula + '(If(' + Item.ZoneName + ', ';
        end;
        if Item.UseMultiplier then
        begin
          ParameterFormula := ParameterFormula +
            '(' + Item.MultiplierName + ' * ';
        end;
        ParameterFormula := ParameterFormula + FortranFloatToStr(Item.Value);
        if Item.UseMultiplier then
        begin
          ParameterFormula := ParameterFormula + ')';
        end;
        if Item.UseZone then
        begin
          ParameterFormula := ParameterFormula + ', 0))';
        end;
      end;
    end;
  end;
end;

procedure TCustomModel.UpdateLpfDataArrayParameterUsed(const DataArrayName: string;
      ParameterType: TParameterType);
var
  DataArray: TDataArray;
  ParameterUsed: Boolean;
  ParameterFormula: string;
begin
  GetParameterUsedAndParameterFormulaForLPF(ParameterUsed, ParameterFormula,
    ParameterType);
  DataArray := FDataArrayManager.GetDataSetByName(DataArrayName);
  if (DataArray <> nil) then
  begin
    DataArray.ParameterUsed := ParameterUsed;
    if ParameterUsed then
    begin
      DataArray.ParameterFormula := ParameterFormula;
      DataArray.Lock := DataArray.Lock + [dcFormula];
    end
    else
    begin
      DataArray.ParameterFormula := '';
      DataArray.Lock := DataArray.Lock - [dcFormula];
    end;
  end;
end;

{ TChildModelCollection }

constructor TChildModelCollection.Create(Model: TComponent);
begin
  inherited Create(TChildModelItem, Model);
end;

function TChildModelCollection.GetItem(Index: integer): TChildModelItem;
begin
  result := inherited Items[Index] as TChildModelItem;
end;

procedure TChildModelCollection.SetItem(Index: integer;
  const Value: TChildModelItem);
begin
  inherited Items[Index] := Value;
end;

{ TChildModelItem }

constructor TChildModelItem.Create(Collection: TCollection);
begin
  inherited;
  FChildModel := TChildModel.Create(nil);
  FChildModel.FParentModel := Model as TCustomModel;
  FChildModel.Assign(FChildModel.FParentModel);
end;

destructor TChildModelItem.Destroy;
begin
  FChildModel.Free;
  inherited;
end;

procedure TChildModelItem.SetChildModel(const Value: TChildModel);
begin
  FChildModel.Assign(Value);
end;

{ TChildModel }

procedure TChildModel.Assign(Source: TPersistent);
begin
  if Source is TChildModel then
  begin
    ModelName := TChildModel(Source).ModelName;
  end;
  inherited;
end;

function TChildModel.GetChemistryOptions: TChemistryOptions;
begin
  result := ParentModel.GetChemistryOptions;
end;

function TChildModel.GetFreeSurface: boolean;
begin
  result := ParentModel.GetFreeSurface;
end;

function TChildModel.GetHufParameters: THufModflowParameters;
begin
  result := ParentModel.GetHufParameters;
end;

function TChildModel.GetLayerStructure: TLayerStructure;
begin
  result := ParentModel.GetLayerStructure;
end;

function TChildModel.GetModelSelection: TModelSelection;
begin
  result := ParentModel.GetModelSelection;
end;

function TChildModel.GetModflowFullStressPeriods: TModflowStressPeriods;
begin
  result := ParentModel.GetModflowFullStressPeriods;
end;

function TChildModel.GetModflowOptions: TModflowOptions;
begin
  result := ParentModel.GetModflowOptions;
end;

function TChildModel.GetModflowOutputControl: TModflowOutputControl;
begin
  result := ParentModel.GetModflowOutputControl;
end;

function TChildModel.GetModflowSteadyParameters: TModflowSteadyParameters;
begin
  result := ParentModel.GetModflowSteadyParameters;
end;

function TChildModel.GetModflowStressPeriods: TModflowStressPeriods;
begin
  result := ParentModel.GetModflowStressPeriods;
end;

function TChildModel.GetModflowTransientParameters: TModflowTransientListParameters;
begin
  result := ParentModel.GetModflowTransientParameters;
end;

function TChildModel.GetObservationPurpose: TObservationPurpose;
begin
  result := ParentModel.GetObservationPurpose;
end;

function TChildModel.GetProgramLocations: TProgramLocations;
begin
  result := ParentModel.GetProgramLocations;
end;

function TChildModel.GetScreenObjectCount: integer;
begin
  result := ParentModel.GetScreenObjectCount;
end;

function TChildModel.GetScreenObjects(const Index: integer): TScreenObject;
begin
  result := ParentModel.GetScreenObjects(Index);
end;

function TChildModel.GetSoluteTransport: boolean;
begin
  result := ParentModel.GetSoluteTransport
end;

function TChildModel.GetUseWaterTable: boolean;
begin
  result := ParentModel.GetUseWaterTable
end;

procedure TChildModel.Invalidate;
begin
  inherited;
  ParentModel.Invalidate;
end;

procedure TChildModel.SetChemistryOptions(const Value: TChemistryOptions);
begin
  ParentModel.SetChemistryOptions(Value);
end;

procedure TChildModel.SetFreeSurface(const Value: boolean);
begin
  ParentModel.SetFreeSurface(Value);
end;

procedure TChildModel.SetHufParameters(const Value: THufModflowParameters);
begin
  ParentModel.SetHufParameters(Value);
end;

procedure TChildModel.SetLayerStructure(const Value: TLayerStructure);
begin
  ParentModel.SetLayerStructure(Value);
end;

procedure TChildModel.SetModelName(const Value: string);
begin
  if FModelName <> Value then
  begin
    FModelName := Value;
    Invalidate;
  end;
end;

procedure TChildModel.SetModelSelection(const Value: TModelSelection);
begin
  ParentModel.SetModelSelection(Value);
end;

procedure TChildModel.SetModflowOptions(const Value: TModflowOptions);
begin
  ParentModel.SetModflowOptions(Value);
end;

procedure TChildModel.SetModflowOutputControl(
  const Value: TModflowOutputControl);
begin
  ParentModel.SetModflowOutputControl(Value);
end;

procedure TChildModel.SetModflowSteadyParameters(
  const Value: TModflowSteadyParameters);
begin
  ParentModel.SetModflowSteadyParameters(Value);
end;

procedure TChildModel.SetModflowStressPeriods(
  const Value: TModflowStressPeriods);
begin
  ParentModel.SetModflowStressPeriods(Value);
end;

procedure TChildModel.SetModflowTransientParameters(
  const Value: TModflowTransientListParameters);
begin
  ParentModel.SetModflowTransientParameters(Value);
end;

procedure TChildModel.SetObservationPurpose(const Value: TObservationPurpose);
begin
  ParentModel.SetObservationPurpose(Value);
end;

procedure TChildModel.SetProgramLocations(const Value: TProgramLocations);
begin
  ParentModel.SetProgramLocations(Value);
end;

procedure TChildModel.SetSoluteTransport(const Value: boolean);
begin
  ParentModel.SetSoluteTransport(Value);
end;

procedure TChildModel.SetUseWaterTable(const Value: boolean);
begin
  ParentModel.SetUseWaterTable(Value);
end;

initialization
  RegisterClass(TPhastModel);
  RegisterClass(TChildModel);

end.
