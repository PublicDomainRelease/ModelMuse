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
  ModelMateClassesUnit, ModflowHobUnit, FormulaManagerUnit,
  PathlineReader, LegendUnit, DisplaySettingsUnit, ModflowCellUnit,
  ModflowGageUnit, ModflowHeadObsResults, GR32, AxCtrls, Generics.Collections,
  Generics.Defaults, Mt3dmsTimesUnit, Mt3dmsChemSpeciesUnit,
  Mt3dmsFluxObservationsUnit, SutraMeshUnit, HashTableFacadeUnit,
  SutraOptionsUnit;

const
  kHufThickness = '_Thickness';
resourcestring
  StrHufThickness = kHufThickness;

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

  // @name indicates the layer number for a MODFLOW reservoir.
  rsResLayer = 'Reservoir_Layer';
  // @name is the elevation of a MODFLOW reservoir.
  rsResBottom = 'Reservoir_Elevation';
  // @name is the hydraulic conductivity of a MODFLOW reservoir.
  rsResKv = 'Reservoir_Hydraulic_Conductivity';
  // @name is the bed thickness of a MODFLOW reservoir.
  rsResBedThickness = 'Reservoir_Bed_Thickness';

  // @name is the lake number of a MODFLOW lake.
  rsLakeID = 'Lake_ID';
  // @name is the leakance of a MODFLOW lake.
  rsLakeLeakance = 'Lakebed_Leakance';

  rsModflowSpecifiedHead = 'Modflow_Specified_Head';

  // @name is the land surface in UZF. It is used to set IUZFBND.
  StrUzfLandSurface = 'Land_Surface';
  // @name set IUZFBND.
  StrUzfLayer = 'UZF_Layer';
  StrUzfDischargeRouting = 'Discharge_Routing';
  StrUzfVerticalK = 'Maximum_Unsaturated_Vertical_K';
  StrUzfBrooksCoreyEpsilon = 'Brooks_Corey_Epsilon';
  StrUzfSaturatedWaterContent = 'Saturated_Water_Content';
  StrUzfInitialUnsaturatedWaterContent = 'Initial_Unsaturated_Water_Content';
  StrUzfReisidualWaterContent = 'Residual_Water_Content';
  StrUzfGage_1_and_2 = 'UZF_Gage_1_and_2';
  StrUzfGage3 = 'UZF_Gage3';

  rsModflow_Initial_Head = 'Modflow_Initial_Head';
  rsModflow_CBKz = 'Confining_Bed_Kz';
  rsVerticalAnisotropy = 'Vertical_Anisotropy';
  rsHorizontalAnisotropy = 'Horizontal_Anisotropy';
  rsSpecificYield = 'Specific_Yield';
  rsWetDryThreshold = 'Wet_Dry_Threshold';
  rsWetDryFlag = 'Wet_Dry_Flag';
  rsWetDry = 'WetDry';

  StrModpathZone = 'Modpath_Zone';
  StrHufReferenceSurface = 'HUF_Reference_Surface';
  StrTransmissivity = 'Transmissivity';
  StrVerticalConductance = 'Vertical_Leakance';
  StrConfinedStorageCoe = 'Confined_Storage_Coefficient';

  StrHUFKxName = 'HUF_Kx';
  StrHUFKyName = 'HUF_Ky';
  StrHUFInterlayerKz = 'HUF_Interlayer_Kz';
  StrHUFSSName = 'HUF_SS';
  StrHUFAverageSYName = 'HUF_Average_SY';
  StrHUFSYName = 'HUF_SY';
  StrHUFSYTPName = 'HUF_SYTP';

  StrZones = 'Zones';
  StrGeostaticStress = 'Geostatic_Stress';
  StrSpecificGravityUns = 'Specific_Gravity_Unsaturated';
  StrSpecificGravitySat = 'Specific_Gravity_Saturated';
  StrInitialPreOffsets = 'Initial_Preconsolidation_Stress_Offset';
  StrInitialPreconsolida = 'Initial_Preconsolidation_Stress';

  STR_MT3DMS_Observation_Locations = 'MT3DMS_Observation_Locations';
  StrMT3DMSActive = 'MT3DMS_Active';
  rsBulkDensity = 'Bulk_Density';
  rsImmobPorosity = 'Immobile_Domain_Porosity';
  rsMT3DMS_Layer_Thickness = 'MT3DMS_Layer_Thickness';

  KModpathBudget = 'Modpath_Budget';
  KModpathRetardation = 'Modpath_Retardation';


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
  StrMT3DMSSSMConcentra = 'MT3DMS SSM Concentration';


  StrMT3DMS = 'MT3DMS';

resourcestring
  strUzfClassification = 'UZF';
  StrHydrology = 'Hydrology';
  StrChemistry = 'Chemistry';
  StrOutput = 'Output';
  WetError = 'The wetting option is active but '
    + 'no layers of the proper type have been specified.';
  StrNewDataSet = 'NewDataSet';
  rsResClassificaton = 'Reservoir';
  rsLakeClassificaton = 'Lake';
  StrMT3DMS_Classificaton = 'MT3DMS';
const
  WettableLayers = [1,3];


resourcestring
  // @name is the name of the @link(TDataArray) that specifies whether an
  // element in PHAST is active or not.
  rsActiveDisplayName = rsActive;
  // @name is the name of the @link(TDataArray) that specifies
  // the hydraulic conductivity in the X direction.
  rsKxDisplayName = rsKx;
  // @name is the name of the @link(TDataArray) that specifies
  // the hydraulic conductivity in the Y direction.
  rsKyDisplayName = rsKy;
  // @name is the name of the @link(TDataArray) that specifies
  // the hydraulic conductivity in the Z direction.
  rsKzDisplayName = rsKz;
  // @name is the name of the @link(TDataArray) that specifies
  // the porosity.
  rsPorosityDisplayName = rsPorosity;
  // @name is the name of the @link(TDataArray) that specifies
  // the specific storage.
  rsSpecific_StorageDisplayName = rsSpecific_Storage;
  // @name is the name of the @link(TDataArray) that specifies
  // the longitudinal dispersivity.
  rsLong_DispersivityDisplayName = rsLong_Dispersivity;
  // @name is the name of the @link(TDataArray) that specifies
  // the horizontal transverse dispersivity.
  rsHorizontal_Transv_DispersivityDisplayName = rsHorizontal_Transv_Dispersivity;
  // @name is the name of the @link(TDataArray) that specifies
  // the vertical transverse dispersivity.
  rsVertical_Transv_DispersivityDisplayName = rsVertical_Transv_Dispersivity;
  // @name is the name of the @link(TDataArray) that specifies
  // the initial head.
  rsInitial_HeadDisplayName = rsInitial_Head;
  // @name is the name of the @link(TDataArray) that specifies
  // the initial water table.
  rsInitial_Water_TableDisplayName = rsInitial_Water_Table;
  // @name is the name of the @link(TDataArray) that specifies
  // the initial solution.
  rsChemistry_Initial_SolutionDisplayName = rsChemistry_Initial_Solution;
  // @name is the name of the @link(TDataArray) that specifies
  // the initial equilibrium phases.
  rsChemistry_Initial_Equilibrium_PhasesDisplayName =
    rsChemistry_Initial_Equilibrium_Phases;
  // @name is the name of the @link(TDataArray) that specifies
  // the initial surface properties.
  rsChemistry_Initial_SurfaceDisplayName = rsChemistry_Initial_Surface;
  // @name is the name of the @link(TDataArray) that specifies
  // the initial exchange properties.
  rsChemistry_Initial_ExchangeDisplayName = rsChemistry_Initial_Exchange;
  // @name is the name of the @link(TDataArray) that specifies
  // the initial gas phase properties.
  rsChemistry_Initial_Gas_PhaseDisplayName = rsChemistry_Initial_Gas_Phase;
  // @name is the name of the @link(TDataArray) that specifies
  // the initial solid-solution properties.
  rsChemistry_Initial_Solid_SolutionsDisplayName = rsChemistry_Initial_Solid_Solutions;
  // @name is the name of the @link(TDataArray) that specifies
  // the initial kinetic properties.
  rsChemistry_Initial_KineticsDisplayName = rsChemistry_Initial_Kinetics;
  // @name is the name of the @link(TDataArray) that specifies
  // the "Print Chemistry" distribution.
  rsPrint_ChemistryDisplayName = rsPrint_Chemistry;
  // @name is the name of the @link(TDataArray) that specifies
  // the "Print XYZ Chemistry" distribution.
  rsPrint_XYZ_ChemistryDisplayName = rsPrint_XYZ_Chemistry;

  // @name indicates the layer number for a MODFLOW reservoir.
  rsResLayerDisplayName = rsResLayer;
  // @name is the elevation of a MODFLOW reservoir.
  rsResBottomDisplayName = rsResBottom;
  // @name is the hydraulic conductivity of a MODFLOW reservoir.
  rsResKvDisplayName = rsResKv;
  // @name is the bed thickness of a MODFLOW reservoir.
  rsResBedThicknessDisplayName = rsResBedThickness;

  // @name is the lake number of a MODFLOW lake.
  rsLakeIDDisplayName = rsLakeID;
  // @name is the leakance of a MODFLOW lake.
  rsLakeLeakanceDisplayName = rsLakeLeakance;

  rsModflowSpecifiedHeadDisplayName = rsModflowSpecifiedHead;

  // @name is the land surface in UZF. It is used to set IUZFBND.
  StrUzfLandSurfaceDisplayName = StrUzfLandSurface;
  // @name set IUZFBND.
  StrUzfLayerDisplayName = StrUzfLayer;
  StrUzfDischargeRoutingDisplayName = StrUzfDischargeRouting;
  StrUzfVerticalKDisplayName = StrUzfVerticalK;
  StrUzfBrooksCoreyEpsilonDisplayName = StrUzfBrooksCoreyEpsilon;
  StrUzfSaturatedWaterContentDisplayName = StrUzfSaturatedWaterContent;
  StrUzfInitialUnsaturatedWaterContentDisplayName = StrUzfInitialUnsaturatedWaterContent;
  StrUzfReisidualWaterContentDisplayName = StrUzfReisidualWaterContent;
  StrUzfGage_1_and_2DisplayName = StrUzfGage_1_and_2;
  StrUzfGage3DisplayName = StrUzfGage3;

  rsModflow_Initial_HeadDisplayName = rsModflow_Initial_Head;
  rsModflow_CBKzDisplayName = rsModflow_CBKz;
  rsVerticalAnisotropyDisplayName = rsVerticalAnisotropy;
  rsHorizontalAnisotropyDisplayName = rsHorizontalAnisotropy;
  rsSpecificYieldDisplayName = rsSpecificYield;
  rsWetDryThresholdDisplayName = rsWetDryThreshold;
  rsWetDryFlagDisplayName = rsWetDryFlag;
  rsWetDryDisplayName = rsWetDry;

  StrModpathZoneDisplayName = StrModpathZone;
  StrHufReferenceSurfaceDisplayName = StrHufReferenceSurface;
  StrTransmissivityDisplayName = StrTransmissivity;
  StrVerticalConductanceDisplayName = StrVerticalConductance;
  StrConfinedStorageCoeDisplayName = StrConfinedStorageCoe;

  StrHUFKxNameDisplayName = StrHUFKxName;
  StrHUFKyNameDisplayName = StrHUFKyName;
  StrHUFInterlayerKzDisplayName = StrHUFInterlayerKz;
  StrHUFSSNameDisplayName = StrHUFSSName;
  StrHUFAverageSYNameDisplayName = StrHUFAverageSYName;
  StrHUFSYNameDisplayName = StrHUFSYName;
  StrHUFSYTPNameDisplayName = StrHUFSYTPName;

  StrZonesDisplayName = StrZones;
  StrGeostaticStressDisplayName = StrGeostaticStress;
  StrSpecificGravityUnsDisplayName = StrSpecificGravityUns;
  StrSpecificGravitySatDisplayName = StrSpecificGravitySat;
  StrInitialPreOffsetsDisplayName = StrInitialPreOffsets;
  StrInitialPreconsolidaDisplayName = StrInitialPreconsolida;

  STR_MT3DMS_Observation_LocationsDisplayName = STR_MT3DMS_Observation_Locations;
  StrMT3DMSActiveDisplayName = StrMT3DMSActive;
  rsBulkDensityDisplayName = rsBulkDensity;
  rsImmobPorosityDisplayName = rsImmobPorosity;
  rsMT3DMS_Layer_ThicknessDisplayName = rsMT3DMS_Layer_Thickness;

  KModpathBudgetDisplayName = KModpathBudget;
  KModpathRetardationDisplayName = KModpathRetardation;

type
  TEvaluationType = (etExport, etDisplay);

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
    DisplayName: string;
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
    FModflowLgrLocation: string;
    FModflowNwtLocation: string;
    FMt3dmsLocation: string;
    FModPathLocationV6: string;
    function GetTextEditorLocation: string;
    procedure SetModflowLocation(const Value: string);
    function RemoveQuotes(const Value: string): string;
    procedure SetModPathLocation(const Value: string);
    procedure SetModelMonitorLocation(const Value: string);
    procedure SetPhastLocation(const Value: string);
    procedure SetZoneBudgetLocation(const Value: string);
    procedure SetModelMateLocation(const Value: string);
    procedure SetModflowLgrLocation(const Value: string);
    procedure SetModflowNwtLocation(const Value: string);
    procedure SetMt3dmsLocation(const Value: string);
    procedure SetModPathLocationV6(const Value: string);
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
    property ModPathLocationVersion6: string read FModPathLocationV6
      write SetModPathLocationV6;
    property ModelMonitorLocation: string read FModelMonitorLocation
      write SetModelMonitorLocation;
    property PhastLocation: string read FPhastLocation write SetPhastLocation;
    property ZoneBudgetLocation: string read FZoneBudgetLocation
      write SetZoneBudgetLocation;
    property ModelMateLocation: string read FModelMateLocation
      write SetModelMateLocation;
    property ModflowLgrLocation: string read FModflowLgrLocation
      write SetModflowLgrLocation;
    property ModflowNwtLocation: string read FModflowNwtLocation
      write SetModflowNwtLocation;
    property Mt3dmsLocation: string read FMt3dmsLocation
      write SetMt3dmsLocation;
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
    FDataSetLookUpList: THashTableFacade;
    FRiverDataSets: TList;
    FStoreCachedData: boolean;
    // @name is used to store @link(TDataArray)s that have been deleted
    // so that they can be restored later.
    FDeletedDataSets: TList;
    FDispersivityIndex: Integer;
    FPorosityIndex: Integer;
    // See @link(DataSetCount).
    function GetDataSetCount: integer;
    // See @link(DataSets).
    function GetDataSet(const Index: integer): TDataArray;
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
    function LocalCount: integer;
    // @name adds DataSet to @link(FDataSets) and calls @link(AddDataSetToLookUpList);
    function AddDataSet(const DataSet: TDataArray): Integer;
    procedure UpdateDataSetDimensions;
  public
    FDataArrayCreationRecords: array of TDataSetCreationData;
    procedure Assign(Source: TDataArrayManager);
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
    // @name indicates the number of @link(TDataArray)s in @name.
    property DataSetCount: integer read GetDataSetCount;
    // @name is used to access the @link(TDataArray)s that are defined
    // throughout the grid.
    property DataSets[const Index: integer]: TDataArray read GetDataSet; default;
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
    procedure AddDataSetToCache(DataArray: TDataArray);
    procedure DontCache(DataArray: TDataArray);
    procedure CacheDataArrays;
    // @name creates a new @link(TDataArray) and adds it to @link(DataSets).
    function CreateNewDataArray(const ClassType: TDataArrayType;
      const Name, Formula, DisplayName: string; Lock: TDataLock; DataType: TRbwDataType;
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
    procedure InvalidateAllDataSets;
    procedure InvalidateAll3DDataSets;
    procedure ClearDeletedDataSets;
    procedure UnlinkDeletedDataSets;
    procedure HandleAddedDataArrays(AddedDataSetList: TList);
    procedure HandleDeletedDataArrays(DeletedDataSetList: TList);
    property ChildDataArrayManagerCount: integer
      read GetChildDataArrayManagerCount;
    property ChildDataArrayManagers[Index: integer]: TDataArrayManager
      read GetChildDataArrayManager;
    procedure UpdateClassifications;
  end;

  TChildModelCollection = class;

  TCustomModel = class abstract (TBaseModel)
  private
    FOnModelSelectionChange: TNotifyEvent;
    // See @link(PhastGrid).
    FPhastGrid: TPhastGrid;
    FModelSelection: TModelSelection;
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

    function GetSomeSegmentsUpToDate: boolean; virtual; abstract;
    procedure SetSomeSegmentsUpToDate(const Value: boolean); virtual; abstract;
    // See @link(PhastGrid).
    procedure SetPhastGrid(const Value: TPhastGrid);
    procedure UpdateDischargeRouting(Sender: TObject);
    function AquiferPropertiesUsed(Sender: TObject): boolean; virtual;
    function KyUsed(Sender: TObject): boolean; virtual;
    function KzUsed(Sender: TObject): boolean; virtual;
    function PorosityUsed(Sender: TObject): boolean; virtual;
    function SpecificStorageUsed(Sender: TObject): boolean; virtual;
    // @name returns true if the model uses solute transport.
    // @name is used an event handler for
    // TDataArray.@link(TDataArray.OnDataSetUsed)
    function ChemistryUsed(Sender: TObject): boolean; virtual;
    // @name returns true if the model uses initial heads.
    // @name is used an event handler for
    // TDataArray.@link(TDataArray.OnDataSetUsed)
    function InitialHeadUsed(Sender: TObject): boolean; virtual;
    // @name returns true if the model uses equilibrium phases.
    // @name is used an event handler for
    // TDataArray.@link(TDataArray.OnDataSetUsed)
    function EquilibriumPhasesUsed(Sender: TObject): boolean; virtual;
    // @name returns true if the model uses surface reactions.
    // @name is used an event handler for
    // TDataArray.@link(TDataArray.OnDataSetUsed)
    function SurfacesUsed(Sender: TObject): boolean; virtual;
    // @name returns true if the model uses exchange reactions.
    // @name is used an event handler for
    // TDataArray.@link(TDataArray.OnDataSetUsed)
    function ExchangeUsed(Sender: TObject): boolean; virtual;
    // @name returns true if the model uses gas phases.
    // @name is used an event handler for
    // TDataArray.@link(TDataArray.OnDataSetUsed)
    function GasPhaseUsed(Sender: TObject): boolean; virtual;
    // @name returns true if the model uses solid solutions.
    // @name is used an event handler for
    // TDataArray.@link(TDataArray.OnDataSetUsed)
    function SolidSolutionUsed(Sender: TObject): boolean; virtual;
    // @name returns true if the model uses kinetics.
    // @name is used an event handler for
    // TDataArray.@link(TDataArray.OnDataSetUsed)
    function KineticsUsed(Sender: TObject): boolean; virtual;
    function ModflowUsed(Sender: TObject): boolean; virtual;
    function RouteUzfDischarge(Sender: TObject): boolean; virtual;
    function ModflowInitialHeadUsed(Sender: TObject): boolean; virtual;
    function ConfiningBedKzUsed(Sender: TObject): boolean; virtual;
    function VerticalAnisotropyUsed(Sender: TObject): boolean; virtual;
    function HorizontalAnisotropyUsed(Sender: TObject): boolean; virtual;
    function SpecificYieldUsed(Sender: TObject): boolean; virtual;
    function WetDryUsed(Sender: TObject): boolean; virtual;
    function ModpathUsed(Sender: TObject): boolean; virtual;
    function HufReferenceSurfaceNeeded(Sender: TObject): boolean; virtual;
    function BcfUsed(Sender: TObject): boolean; virtual;
    function ConfinedStorageCoefUsed(Sender: TObject): boolean; virtual;
    function OptionalDataSet(Sender: TObject): boolean;
    function HufSelected(Sender: TObject): boolean;
    function HufStorageUsed(Sender: TObject): boolean;
    function ZoneBudgetSelected(Sender: TObject): boolean; virtual;
    function SwtSelected(Sender: TObject): boolean; virtual;
    function SwtOffsetsUsed(Sender: TObject): boolean; virtual;
    function SwtSpecifiedUsed(Sender: TObject): boolean; virtual;
    function SutraUsed(Sender: TObject): boolean; virtual;
    function ModflowOrPhastUsed(Sender: TObject): boolean; virtual;
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
    function PrepareModflowFullStressPeriods(ShowWarning: boolean): Boolean;
    function CountStepsInExport: Integer;
    procedure GetDefaultOutputFileExtension(var Extension: string);
    function CheckMt3dTimes(ShowWarning: Boolean): boolean;

  private
    FGrid: TCustomModelGrid;
    FModflowOptions: TModflowOptions;
    FNameFileWriter: TObject;
    FHeadObsResults: THeadObsCollection;
    FMt3dmsHeadMassFluxObservations: TMt3dmsFluxObservationGroups;
    FMt3dmsMassLoadingMassFluxObservations: TMt3dmsFluxObservationGroups;
    FMt3dmsGhbMassFluxObservations: TMt3dmsFluxObservationGroups;
    FMt3dmsRivMassFluxObservations: TMt3dmsFluxObservationGroups;
    FMt3dmsWellMassFluxObservations: TMt3dmsFluxObservationGroups;
    FMt3dmsResMassFluxObservations: TMt3dmsFluxObservationGroups;
    FMt3dmsRchMassFluxObservations: TMt3dmsFluxObservationGroups;
    FMt3dmsDrtMassFluxObservations: TMt3dmsFluxObservationGroups;
    FMt3dmsEtsMassFluxObservations: TMt3dmsFluxObservationGroups;
    FMt3dmsEvtMassFluxObservations: TMt3dmsFluxObservationGroups;
    FMt3dmsDrnMassFluxObservations: TMt3dmsFluxObservationGroups;
    FMt3dmsLakMassFluxObservations: TMt3dmsFluxObservationGroups;
    FSutraMesh: TSutraMesh3D;
    FSutraOptions: TSutraOptions;
    procedure SetAlternateFlowPackage(const Value: boolean);
    procedure SetAlternateSolver(const Value: boolean);
    procedure SetBatchFileAdditionsAfterModel(const Value: TStrings);
    procedure SetBatchFileAdditionsBeforeModel(const Value: TStrings);
    procedure SetModflowGrid(const Value: TModflowGrid);
    procedure SetModflowNameFileLines(const Value: TStrings);
    procedure SetModflowPackages(const Value: TModflowPackages);
    procedure SetHeadFluxObservations(const Value: TFluxObservationGroups);
    procedure SetRiverObservations(const Value: TFluxObservationGroups);
    procedure SetDrainObservations(const Value: TFluxObservationGroups);
    procedure SetGhbObservations(const Value: TFluxObservationGroups);
    procedure SetHydrogeologicUnits(const Value: THydrogeologicUnits);
    procedure SetFilesToArchive(const Value: TStrings);
    procedure SetModelInputFiles(const Value: TStrings);
    procedure SetModflowWettingOptions(const Value: TWettingOptions);


    procedure ClearParsers;
    function GetParsers(Index: integer): TRbwParser;
    procedure SetGlobalVariables(const Value: TGlobalVariables);
    procedure SetEdgeDisplay(const Value: TCustomModflowGridEdgeDisplay);
    procedure AllObserversStopTalking;
    procedure FreeHufNotifiers;
    procedure FreeGridNotifiers;
    function GetModflowFullStressPeriods: TModflowStressPeriods; virtual; abstract;
    function StoreDrainObservations: Boolean;
    function StoreGhbObservations: Boolean;
    function StoreHeadFluxObservations: Boolean;
    function StoreRiverObservations: Boolean;
    function StoreHydrogeologicUnits: Boolean; virtual;
    function GetDisplayColumn: integer;
    function GetDisplayLayer: integer;
    function GetDisplayRow: integer;
    function GetSelectedColumn: integer;
    function GetSelectedLayer: integer;
    function GetSelectedRow: integer;
    procedure SetDisplayColumn(const Value: integer);
    procedure SetDisplayLayer(const Value: integer);
    procedure SetDisplayRow(const Value: integer);
    procedure SetSelectedColumn(const Value: integer);
    procedure SetSelectedLayer(const Value: integer);
    procedure SetSelectedRow(const Value: integer);
    function WettingActive: boolean; virtual;
    procedure SetNameFileWriter(const Value: TObject);
    function GetThreeDGridObserver: TObserver;
    function GetSaveBfhBoundaryConditions: boolean; virtual; abstract;
    procedure SetSaveBfhBoundaryConditions(const Value: boolean);  virtual; abstract;
    function GetModflowLocation: string;
    procedure SetModflowLocation(const Value: string);
    procedure SetHeadObsResults(const Value: THeadObsCollection);
    procedure CreateHeadObsResults;
    function GetHeadObsResults: THeadObsCollection;
    procedure InternalExportModflowModel(const FileName: string; ExportAllLgr: boolean);
    procedure ExportLakePackage(const FileName: string);
    procedure ExportSfrPackage(const FileName: string);
    procedure EvaluateSfrPackage;
    procedure ExportUzfPackage(const FileName: string);
    function Mt3dMSUsed(Sender: TObject): boolean;
    function Mt3dMSBulkDensityUsed(Sender: TObject): boolean;
    function Mt3dMSImmobPorosityUsed(Sender: TObject): boolean;
    procedure SetMt3dmsOutputControl(const Value: TMt3dmsOutputControl); virtual; abstract;
    function GetMt3dmsOutputControl: TMt3dmsOutputControl; virtual; abstract;
    function GetMt3dmsTimes: TMt3dmsTimeCollection; virtual; abstract;
    procedure SetMt3dmsTimes(const Value: TMt3dmsTimeCollection); virtual; abstract;
    function LongitudinalDispersionUsed(Sender: TObject): boolean;
    procedure UpdateMt3dmsActive(Sender: TObject);
    function CountStepsInMt3dExport: Integer;
    procedure SetMt3dmsHeadMassFluxObservations(
      const Value: TMt3dmsFluxObservationGroups);
    function StoreHeadMassFluxObservations: Boolean;
    procedure SetMt3dmsDrnMassFluxObservations(
      const Value: TMt3dmsFluxObservationGroups);
    procedure SetMt3dmsDrtMassFluxObservations(
      const Value: TMt3dmsFluxObservationGroups);
    procedure SetMt3dmsEtsMassFluxObservations(
      const Value: TMt3dmsFluxObservationGroups);
    procedure SetMt3dmsEvtMassFluxObservations(
      const Value: TMt3dmsFluxObservationGroups);
    procedure SetMt3dmsGhbMassFluxObservations(
      const Value: TMt3dmsFluxObservationGroups);
    procedure SetMt3dmsLakMassFluxObservations(
      const Value: TMt3dmsFluxObservationGroups);
    procedure SetMt3dmsMassLoadingMassFluxObservations(
      const Value: TMt3dmsFluxObservationGroups);
    procedure SetMt3dmsRchMassFluxObservations(
      const Value: TMt3dmsFluxObservationGroups);
    procedure SetMt3dmsResMassFluxObservations(
      const Value: TMt3dmsFluxObservationGroups);
    procedure SetMt3dmsRivMassFluxObservations(
      const Value: TMt3dmsFluxObservationGroups);
    procedure SetMt3dmsWellMassFluxObservations(
      const Value: TMt3dmsFluxObservationGroups);
    function StoreDrnMassFluxObservations: Boolean;
    function StoreDrtMassFluxObservations: Boolean;
    function StoreEtsMassFluxObservations: Boolean;
    function StoreEvtMassFluxObservations: Boolean;
    function StoreGhbMassFluxObservations: Boolean;
    function StoreLakMassFluxObservations: Boolean;
    function StoreMassLoadingMassFluxObservations: Boolean;
    function StoreRchMassFluxObservations: Boolean;
    function StoreResMassFluxObservations: Boolean;
    function StoreRivMassFluxObservations: Boolean;
    function StoreWellMassFluxObservations: boolean;
    procedure SetPathLine(const Value: TPathLineReader);
    function GetPathLine: TPathLineReader;
    function StorePathLine: boolean;
    function StoreTimeSeries: boolean;
    procedure SetTimeSeries(const Value: TTimeSeriesReader);
    function GetTimeSeries: TTimeSeriesReader;
    function StoreEndPoints: boolean;
    function GetEndPoints: TEndPointReader;
    procedure SetEndPoints(const Value: TEndPointReader);
    procedure SetSutraMesh(const Value: TSutraMesh3D);
    procedure RenameOldVerticalLeakance; virtual;
    function GetThreeDDataSet: TDataArray;
    procedure SetThreeDDataSet(const Value: TDataArray);
    function GetFrontDataSet: TDataArray;
    function GetSideDataSet: TDataArray;
    function GetTopDataSet: TDataArray;
    procedure SetFrontDataSet(const Value: TDataArray);
    procedure SetSideDataSet(const Value: TDataArray);
    procedure SetTopDataSet(const Value: TDataArray);
    function GetMesh: TSutraMesh3D;
    procedure SetSutraOptions(const Value: TSutraOptions);
    function ModpathBudgetNeeded(Sender: TObject): boolean;
    function ModpathRetardationNeeded(Sender: TObject): boolean;
    function GetModPathLocation: string;
    function ModpathZonesNeeded(Sender: TObject): boolean;
  var
    LakWriter: TObject;
    SfrWriter: TObject;
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
    FGageUnitNumber: integer;
    FGages: TStringList;
    FPathLine: TPathLineReader;
    FTimeSeries: TTimeSeriesReader;
    FEndPoints: TEndPointReader;
  strict private
    FHfbWriter: TObject;
    FMfHobHeads: THobDisplayTimeList;
    FUnitNumbers: TUnitNumbers;
    FHfbDisplayer: THfbDisplayer;

    // @name holds the @link(TCustomTimeList)s in the model.
    // See @link(TimeLists).
    FTimeLists: TList;

  protected
    procedure SetFileName(const Value: string); virtual;
    procedure SetFrontTimeList(const Value: TCustomTimeList); virtual;
    procedure SetSideTimeList(const Value: TCustomTimeList); virtual;
    procedure SetTopTimeList(const Value: TCustomTimeList); virtual;
    function GetFormulaManager: TFormulaManager; virtual; abstract;
    function GetSelectedModel: TCustomModel; virtual; abstract;
    procedure SetSelectedModel(const Value: TCustomModel); virtual; abstract;
    function GetScreenObjects(const Index: integer): TScreenObject;virtual;abstract;
    function GetScreenObjectCount: integer;virtual;abstract;
    function GetModflowSteadyParameters: TModflowSteadyParameters;virtual;abstract;
    procedure SetModflowSteadyParameters(const Value: TModflowSteadyParameters);virtual;abstract;
//    function GetModelSelection: TModelSelection;virtual;abstract;
    // Among other things, if @link(OnModelSelectionChange)
    // is assigned, @name calls @link(TfrmGoPhast.ModelSelectionChange
    // TfrmGoPhast.ModelSelectionChange).  @name also changes
    // the functions that are available. and sets @Link(TObserver)s
    // for the @link(Grid).
    procedure SetModelSelection(const Value: TModelSelection); override;
    // @name causes the grid to not be colored by any @link(TDataArray).
    function GetLayerStructure: TLayerStructure;virtual;abstract;
    procedure SetLayerStructure(const Value: TLayerStructure);virtual;abstract;
    procedure SetModflowOptions(const Value: TModflowOptions);
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
    procedure InitializeGages; virtual;
    procedure InitializeSfrWriter(EvaluationType: TEvaluationType); virtual;
    procedure FreeSfrWriter; virtual;
    procedure SetContourFont(const Value: TFont) ; virtual; abstract;
    procedure SetShowContourLabels(const Value: boolean);  virtual; abstract;
    function GetContourFont: TFont;  virtual; abstract;
    function GetShowContourLabels: boolean; virtual; abstract;
    function GetImmobileComponents: TChemSpeciesCollection; virtual; abstract;
    function GetMobileComponents: TMobileChemSpeciesCollection; virtual; abstract;
    procedure SetImmobileComponents(const Value: TChemSpeciesCollection);
      virtual; abstract;
    procedure SetMobileComponents(const Value: TMobileChemSpeciesCollection);
      virtual; abstract;
  public
    procedure RenameDataArray(DataArray: TDataArray;
      const NewName, NewDisplayName: string);
    // When a @link(TDataArray) or global variable is renamed, @name is
    // called to update all the formulas with the new names.
    procedure UpdateFormulas(OldNames, NewNames: TStringList);
    procedure UpdateDataArrayDimensions(DataArray: TDataArray);
    function IndexOfMt3dmsSpeciesName(const AChemSpecies: string): integer;
    property Gages: TStringList read FGages;
    function StoreHeadObsResults: boolean;
    function TestModpathOK: Boolean;
    property ModflowLocation: string read GetModflowLocation write SetModflowLocation;
    property ModPathLocation: string read GetModPathLocation;
    procedure ExportSeparateLgrModel(const FileName: string;
      RunModel, ExportModpath, ExportZoneBudget, ShowWarning: boolean);
    // @name returns the name of the most likely output file from which
    // model results will be imported.  If heads were saved, the name of the
    // file containing heads will be returned.
    // If heads were not saved by drawdowns were saved, the name of the
    // file containing drawdowns will be returned.
    // After that, it is the file containing the cell-by-cell flows.
    function DefaultModflowOutputFileName: string; virtual;
    procedure ModelObserversStopTalkingTo(Observer: TObserver);
    procedure UpdateActive(Sender: TObject);
    procedure UpdateWetDry(Sender: TObject);
    procedure UpdateLakeId(Sender: TObject);
    procedure FinalizeActive(Sender: TObject);
    procedure FinalizeWetDry(Sender: TObject);
    procedure FinalizeLakeId(Sender: TObject);
    procedure UpdateModPathZone(Sender: TObject);
      // @name updates @link(TDataArray.OnPostInitialize
    // TDataArray.OnPostInitialize) for several @link(TDataArray)s.
    procedure UpdateOnPostInitialize;
  // MODFLOW can not use a file name containing a space character.
    // @name replaces the space character in a file name with an underscore.
    // @name is the event handler for @link(TDataArray.OnDataSetUsed
    // TDataArray.OnDataSetUsed) for @link(TDataArray)s related to the HUF
    // package.
    function HufDataArrayUsed(Sender: TObject): boolean;
    // @name assigns frmGoPhast.Grid.@link(TCustomModelGrid.FrontDataSet)
    // to be the @link(TDataArray) in TimeList at Time.
    procedure UpdateFrontTimeDataSet(const TimeList: TCustomTimeList;
      const Time: double); virtual;
    // @name assigns frmGoPhast.Grid.@link(TCustomModelGrid.SideDataSet)
    // to be the @link(TDataArray) in TimeList at Time.
    procedure UpdateSideTimeDataSet(const TimeList: TCustomTimeList;
      const Time: double); virtual;
    // @name assigns frmGoPhast.Grid.@link(TCustomModelGrid.TopDataSet)
    // to be the @link(TDataArray) in TimeList at Time.
    procedure UpdateTopTimeDataSet(const TimeList: TCustomTimeList;
      const Time: double); virtual;
    // @name assigns frmGoPhast.Grid.@link(TCustomModelGrid.ThreeDDataSet)
    // to be the @link(TDataArray) in TimeList at Time.
    procedure UpdateThreeDTimeDataSet(const TimeList: TCustomTimeList;
      const Time: double); virtual;
    function GetTimeListByName(const AName: string): TCustomTimeList;
    property FormulaManager: TFormulaManager read GetFormulaManager;
    function AddDataSet(const DataSet: TDataArray): Integer; virtual;
    function LayerGroupUsed(LayerGroup: TLayerGroup): boolean; virtual;
    // When a layer group represents more than one layer, @name gives the
    // relative positions of the dividing lines between layers.
    // The values is @name should decrease monotonically.
    // For example, if a layer group is divided into 4 uniform layers,
    // @name should return [0.75, 0.5, 0.25].
    function LayerFractions(LayerGroup: TCustomLayerGroup): TDoubleDynArray; virtual;
    function LayerCount: integer; virtual;
    procedure UpdateDisplayUseList(NewUseList: TStringList;
      ParamType: TParameterType; DataIndex: integer; const DisplayName: string); virtual; abstract;
    procedure Assign(Source: TPersistent); override;
    property Clearing: Boolean read FClearing;
    property DataArrayManager: TDataArrayManager read FDataArrayManager;

    procedure OnActiveDataSetChanged(Sender: TObject);
    procedure Clear;
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddModelInputFile(const FileName: string);
    procedure AddFileToArchive(const FileName: string);

    property Grid: TCustomModelGrid read FGrid;

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

    property ThreeDGridObserver: TObserver read GetThreeDGridObserver;
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
    // via @link(TPhastModel.ObjectList).
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
    function InitialWaterTableUsed(Sender: TObject): boolean; virtual;
    function ReservoirLayerUsed(Sender: TObject): boolean; virtual;
    function ReservoirPackageUsed(Sender: TObject): boolean; virtual;
    function LakePackageUsed(Sender: TObject): boolean; virtual;
    function UzfPackageUsed(Sender: TObject): boolean; virtual;
    function UzfUnsatVertKUsed(Sender: TObject): boolean; virtual;
    function UzfInitialInfiltrationUsed(Sender: TObject): boolean; virtual;
    function UzfResidualWaterContentUsed(Sender: TObject): boolean; virtual;

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
    procedure ExportModflowModel(const FileName: string;
      RunModel, ExportModpath, NewBudgetFileForModpath, ExportZoneBudget,
      ShowWarning: boolean);
    procedure ExportModpathModel(FileName: string;
      RunModel, NewBudgetFile: boolean; EmbeddedExport: boolean = False);
    procedure ExportZoneBudgetModel(FileName: string;
      RunModel, EmbeddedExport: boolean);
    procedure ExportMt3dmsModel(const FileName: string;
      RunModel, ShowWarning: Boolean);


    // @name is the @link(TCustomTimeList) for
    // the transient data set used to color
    // the front view of the grid.
    // If the data set used to color the grid is not transient, or the grid
    // is not colored, @name has no meaning.
    // See @link(FrontDisplayTime) and
    // TCustomModelGrid.@link(TCustomModelGrid.FrontDataSet).
    property FrontTimeList: TCustomTimeList read FFrontTimeList
      write SetFrontTimeList;

    // @name is the TPhastTimeList for the transient data set used to color
    // the side view of the grid.
    // If the data set used to color the grid is not transient, or the grid
    // is not colored, @name has no meaning.
    // See @link(SideDisplayTime) and
    // TCustomModelGrid.@link(TCustomModelGrid.SideDataSet).
    property SideTimeList: TCustomTimeList read FSideTimeList
      write SetSideTimeList;
    // @name is the @link(TCustomTimeList)
    // for the transient data set used to color
    // the top view of the grid.
    // If the data set used to color the grid is not transient, or the grid
    // is not colored, @name has no meaning.
    // See @link(TopDisplayTime) and
    // TCustomModelGrid.@link(TCustomModelGrid.TopDataSet).
    property TopTimeList: TCustomTimeList read FTopTimeList
      write SetTopTimeList;
    // @name is the time for the transient data set used to color the
    // top view of the grid.
    // If the data set used to color the grid is not transient, or the grid
    // is not colored, @name has no meaning.
    // See @link(TopTimeList) and
    // TCustomModelGrid.@link(TCustomModelGrid.TopDataSet).
    property TopDisplayTime: double read FTopDisplayTime;
    // @name is the time for the transient data set used to color the
    // front view of the grid.
    // If the data set used to color the grid is not transient, or the grid
    // is not colored, @name has no meaning.
    // See @link(FrontTimeList) and
    // TCustomModelGrid.@link(TCustomModelGrid.FrontDataSet).
    property FrontDisplayTime: double read FFrontDisplayTime;
    // @name is the time for the transient data set used to color the
    // side view of the grid.
    // If the data set used to color the grid is not transient, or the grid
    // is not colored, @name has no meaning.
    // See @link(SideTimeList) and
    // TCustomModelGrid.@link(TCustomModelGrid.SideDataSet).
    property SideDisplayTime: double read FSideDisplayTime;
    // @name is the time for the transient data set used to color the
    // 3D view of the grid.
    // If the data set used to color the grid is not transient, or the grid
    // is not colored, @name has no meaning.
    // See @link(ThreeDTimeList) and
    // TCustomModelGrid.@link(TCustomModelGrid.ThreeDDataSet).
    property ThreeDDisplayTime: double read FThreeDDisplayTime;
    // @name is the @link(TCustomTimeList) for
    // the transient data set used to color
    // the 3D view of the grid.
    // If the data set used to color the grid is not transient, or the grid
    // is not colored, @name has no meaning.
    // See @link(ThreeDDisplayTime) and
    // TCustomModelGrid.@link(TCustomModelGrid.ThreeDDataSet).
    property ThreeDTimeList: TCustomTimeList read FThreeDTimeList
      write FThreeDTimeList;
    property ModflowSteadyParameters: TModflowSteadyParameters
      read GetModflowSteadyParameters write SetModflowSteadyParameters;

    property LayerStructure: TLayerStructure read GetLayerStructure
      write SetLayerStructure;


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

    property Mt3dmsOutputControl: TMt3dmsOutputControl
      read GetMt3dmsOutputControl write SetMt3dmsOutputControl;
    property Mt3dmsTimes: TMt3dmsTimeCollection read GetMt3dmsTimes
      write SetMt3dmsTimes;

    // @name stores the @link(TDataArray)s in GoPhast.
    property DataSetList: TDataSetCollection read GetDataSetCollection
      write FDataSetCollection;

    procedure InvalidateMfSfrStreamTop(Sender: TObject);
    procedure InvalidateMfSfrStreamSlope(Sender: TObject);
    procedure InvalidateMfSfrStreamThickness(Sender: TObject);
    procedure InvalidateMfSfrStreamK(Sender: TObject);
    procedure InvalidateMfSfrSaturatedWaterContent(Sender: TObject);
    procedure InvalidateMfSfrInitialWaterContent(Sender: TObject);
    procedure InvalidateMfSfrBrooksCorey(Sender: TObject);
    procedure InvalidateMfSfrVerticalUnsatK(Sender: TObject);
    procedure InvalidateMfSfrUpstreamHydraulicConductivity(Sender: TObject);
    procedure InvalidateMfSfrDownstreamHydraulicConductivity(Sender: TObject);
    procedure InvalidateMfSfrUpstreamWidth(Sender: TObject);
    procedure InvalidateMfSfrDownstreamWidth(Sender: TObject);
    procedure InvalidateMfSfrUpstreamThickness(Sender: TObject);
    procedure InvalidateMfSfrDownstreamThickness(Sender: TObject);
    procedure InvalidateMfSfrUpstreamElevation(Sender: TObject);
    procedure InvalidateMfSfrDownstreamElevation(Sender: TObject);
    procedure InvalidateMfSfrUpstreamUnsaturatedWaterContent(Sender: TObject);
    procedure InvalidateMfSfrDownstreamUnsaturatedWaterContent(Sender: TObject);
    procedure InvalidateMfSfrUpstreamUnsatInitialWaterContent(Sender: TObject);
    procedure InvalidateMfSfrDownstreamUnsatInitialWaterContent(
      Sender: TObject);
    procedure InvalidateMfSfrUpstreamBrooksCorey(Sender: TObject);
    procedure InvalidateMfSfrDownstreamBrooksCorey(Sender: TObject);
    procedure InvalidateMfSfrUpstreamUnsatKz(Sender: TObject);
    procedure InvalidateMfSfrDownstreamUnsatKz(Sender: TObject);
    procedure InvalidateMfRchLayer(Sender: TObject);
    function GetScreenObjectByName(AName: string): TScreenObject; virtual; abstract;
    // @name sets the event handlers for the discharge routing array in
    // the UZF package.
    procedure DischargeRoutingUpdate;
    property SelectedModel: TCustomModel read GetSelectedModel write SetSelectedModel;

    // name is called in @link(SetModelSelection).
    // The event handler for this event is
    // @link(TfrmGoPhast.ModelSelectionChange).
    property OnModelSelectionChange: TNotifyEvent read FOnModelSelectionChange
      write FOnModelSelectionChange;
    // @name updates @link(TDataArray.ParameterUsed TDataArray.ParameterUsed)
    // and @link(TDataArray.ParameterFormula TDataArray.ParameterFormula)
    // for the @link(TDataArray)s related to the LPF parameters.
    procedure UpdateDataArrayParameterUsed;
    // @name calls TScreenObject.@link(TScreenObject.Invalidate)
    // for every @link(TScreenObject) in @link(ScreenObjects).
    procedure InvalidateScreenObjects;
    property SelectedColumn: integer read GetSelectedColumn write SetSelectedColumn;
    property SelectedRow: integer read GetSelectedRow write SetSelectedRow;
    property SelectedLayer: integer read GetSelectedLayer write SetSelectedLayer;
    property DisplayColumn: integer read GetDisplayColumn write SetDisplayColumn;
    property DisplayRow: integer read GetDisplayRow write SetDisplayRow;
    property DisplayLayer: integer read GetDisplayLayer write SetDisplayLayer;
    // If @name is @true, there is at least one @link(TScreenObject)
    // in which TScreenObject.Segments.@link(
    // TCellElementSegmentList.UpToDate) is @True
    // @name is used in @link(TPhastModel.InvalidateSegments).
    property SomeSegmentsUpToDate: boolean read GetSomeSegmentsUpToDate
      write SetSomeSegmentsUpToDate;
    procedure InvalidateMfChdStartingHead(Sender: TObject);
    procedure InvalidateMfChdEndingHead(Sender: TObject);
    procedure InvalidateMfGhbConductance(Sender: TObject);
    procedure InvalidateMfGhbBoundaryHead(Sender: TObject);
    procedure InvalidateMfWellPumpage(Sender: TObject);
    procedure InvalidateMfRivConductance(Sender: TObject);
    procedure InvalidateMfRivStage(Sender: TObject);
    procedure InvalidateMfRivBottom(Sender: TObject);
    procedure InvalidateMfDrnConductance(Sender: TObject);
    procedure InvalidateMfDrnElevation(Sender: TObject);
    procedure InvalidateMfDrtConductance(Sender: TObject);
    procedure InvalidateMfDrtElevation(Sender: TObject);
    procedure InvalidateMfDrtReturnFraction(Sender: TObject);
    procedure InvalidateMfRchRate(Sender: TObject);
    procedure InvalidateMfUzfInfiltration(Sender: TObject);
    procedure InvalidateMfEvtEvapRate(Sender: TObject);
    procedure InvalidateMfEvtEvapSurface(Sender: TObject);
    procedure InvalidateMfEvtEvapDepth(Sender: TObject);
    procedure InvalidateMfEvtEvapLayer(Sender: TObject);
    procedure InvalidateMfEtsEvapRate(Sender: TObject);
    procedure InvalidateMfEtsEvapSurface(Sender: TObject);
    procedure InvalidateMfEtsEvapDepth(Sender: TObject);
    procedure InvalidateMfEtsEvapLayer(Sender: TObject);
    procedure InvalidateEtsDepthFractions(Sender: TObject);
    procedure InvalidateEtsRateFractions(Sender: TObject);
    procedure InvalidateMfUzfEtDemand(Sender: TObject);
    procedure InvalidateMfUzfExtinctionDepth(Sender: TObject);
    procedure InvalidateMfUzfWaterContent(Sender: TObject);
    procedure InvalidateMt3dmsChemSources(Sender: TObject);
    property NameFileWriter: TObject read FNameFileWriter write SetNameFileWriter;
    function ModflowLayerCount: integer; virtual;
    function ModflowConfiningBedCount: integer; virtual;
    procedure WriteLAYCB(const DiscretizationWriter: TObject); virtual;
    // @name returns true if a layer in the MODFLOW grid is simulated
    // LayerID is zero-based.
    function IsLayerSimulated(const LayerID: integer): boolean; virtual;
    Function Laytyp: TOneDIntegerArray; virtual;
    Function Layavg: TOneDIntegerArray; virtual;
    function Chani: TOneDIntegerArray; virtual;
    Function Layvka: TOneDIntegerArray; virtual;
    function Trpy: TOneDRealArray; virtual;
    Function TRPT: TOneDRealArray; virtual;
    function TRPV: TOneDRealArray; virtual;
    Function DMCOEF: TOneDRealArray; virtual;
    function GetLayerGroupByLayer(const Layer: integer): TLayerGroup; virtual;
    function ModflowLayerBottomDescription(const LayerID: integer): string; virtual;
    // @name converts a MODFLOW model layer (starting at 1) to the
    // appropriate index in a 3D data array;
    Function ModflowLayerToDataSetLayer(ModflowLayer: integer): integer; virtual;
    function DataSetLayerToModflowLayer(DataSetLayer: integer): integer; virtual;
    // @name is used to move a boundary to a new position when
    // the current position is invalid.  This version of @name does nothing
    // but in @link(TChildModel.AdjustCellPosition
    // TChildModel.AdjustCellPosition), it is used to move boundary cells away
    // from the edge of the model.
    procedure AdjustCellPosition(AValueCell: TValueCell); overload; virtual;
    procedure AdjustCellPosition(ACellAssignment: TCellAssignment); overload; virtual;
    // @name is used in local grid refinement to adjust the values
    // of recharge (RCH, UZF package) and max ET (in EVT, ETS, and UZF) packages.
    // at the interface between the grids.
    procedure AdjustDataArray(ADataArray: TDataArray); virtual; abstract;
    procedure AdjustResKvArray(Sender: TObject);
    // @name is the event handler for @link(TDataArray.OnDataSetUsed
    // TDataArray.OnDataSetUsed) for @link(TDataArray)s that have model results.
    function ModelResultsRequired(Sender: TObject): boolean;
    procedure BeginGridChange; virtual;
    procedure EndGridChange; virtual;
    property SaveBfhBoundaryConditions: boolean read GetSaveBfhBoundaryConditions
      write SetSaveBfhBoundaryConditions default True;
    procedure DrawHeadObservations(const BitMap: TBitmap32;
      const ZoomBox: TQrbwZoomBox2); virtual;
    procedure InvalidateMfHobHeads(Sender: TObject);
    property ContourFont: TFont read GetContourFont write SetContourFont;
    property ShowContourLabels: boolean read GetShowContourLabels
      write SetShowContourLabels default True;
    procedure UpdateMt3dmsChemDataSets; virtual; abstract;
    procedure GenerateSutraMesh(var ErrorMessage: string);
    procedure OnTopSutraMeshChanged(Sender: TObject);
    property ThreeDDataSet: TDataArray read GetThreeDDataSet
      write SetThreeDDataSet;
    property TopDataSet: TDataArray read GetTopDataSet
      write SetTopDataSet;
    property FrontDataSet: TDataArray read GetFrontDataSet
      write SetFrontDataSet;
    property SideDataSet: TDataArray read GetSideDataSet
      write SetSideDataSet;
    procedure DiscretizationChanged;
    property Mesh: TSutraMesh3D read GetMesh;
    function TwoDElementCenter(const Column, Row: integer): TPoint2D;
    function TwoDElementCorner(const Column, Row: integer): TPoint2D;
  published
    // @name defines the grid used with PHAST.
    property PhastGrid: TPhastGrid read FPhastGrid write SetPhastGrid;
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
      read FHeadFluxObservations write SetHeadFluxObservations
      stored StoreHeadFluxObservations;
    property DrainObservations: TFluxObservationGroups
      read FDrainObservations write SetDrainObservations
      stored StoreDrainObservations;
    property GhbObservations: TFluxObservationGroups
      read FGhbObservations write SetGhbObservations
      stored StoreGhbObservations;
    property RiverObservations: TFluxObservationGroups
      read FRiverObservations write SetRiverObservations
      stored StoreRiverObservations;
    property Mt3dmsHeadMassFluxObservations: TMt3dmsFluxObservationGroups
      read FMt3dmsHeadMassFluxObservations write SetMt3dmsHeadMassFluxObservations
      stored StoreHeadMassFluxObservations;

    property Mt3dmsWellMassFluxObservations: TMt3dmsFluxObservationGroups
      read FMt3dmsWellMassFluxObservations write SetMt3dmsWellMassFluxObservations
      stored StoreWellMassFluxObservations;

    property Mt3dmsDrnMassFluxObservations: TMt3dmsFluxObservationGroups
      read FMt3dmsDrnMassFluxObservations write SetMt3dmsDrnMassFluxObservations
      stored StoreDrnMassFluxObservations;

    property Mt3dmsRivMassFluxObservations: TMt3dmsFluxObservationGroups
      read FMt3dmsRivMassFluxObservations write SetMt3dmsRivMassFluxObservations
      stored StoreRivMassFluxObservations;

    property Mt3dmsGhbMassFluxObservations: TMt3dmsFluxObservationGroups
      read FMt3dmsGhbMassFluxObservations write SetMt3dmsGhbMassFluxObservations
      stored StoreGhbMassFluxObservations;

    property Mt3dmsRchMassFluxObservations: TMt3dmsFluxObservationGroups
      read FMt3dmsRchMassFluxObservations write SetMt3dmsRchMassFluxObservations
      stored StoreRchMassFluxObservations;

    property Mt3dmsEvtMassFluxObservations: TMt3dmsFluxObservationGroups
      read FMt3dmsEvtMassFluxObservations write SetMt3dmsEvtMassFluxObservations
      stored StoreEvtMassFluxObservations;

    property Mt3dmsMassLoadingMassFluxObservations: TMt3dmsFluxObservationGroups
      read FMt3dmsMassLoadingMassFluxObservations write SetMt3dmsMassLoadingMassFluxObservations
      stored StoreMassLoadingMassFluxObservations;

    property Mt3dmsResMassFluxObservations: TMt3dmsFluxObservationGroups
      read FMt3dmsResMassFluxObservations write SetMt3dmsResMassFluxObservations
      stored StoreResMassFluxObservations;

    property Mt3dmsLakMassFluxObservations: TMt3dmsFluxObservationGroups
      read FMt3dmsLakMassFluxObservations write SetMt3dmsLakMassFluxObservations
      stored StoreLakMassFluxObservations;

    property Mt3dmsDrtMassFluxObservations: TMt3dmsFluxObservationGroups
      read FMt3dmsDrtMassFluxObservations write SetMt3dmsDrtMassFluxObservations
      stored StoreDrtMassFluxObservations;

    property Mt3dmsEtsMassFluxObservations: TMt3dmsFluxObservationGroups
      read FMt3dmsEtsMassFluxObservations write SetMt3dmsEtsMassFluxObservations
      stored StoreEtsMassFluxObservations;

    property HydrogeologicUnits: THydrogeologicUnits read FHydrogeologicUnits
      write SetHydrogeologicUnits stored StoreHydrogeologicUnits;
    property FilesToArchive: TStrings read FFilesToArchive
      write SetFilesToArchive;
    property ModelInputFiles: TStrings read FModelInputFiles
      write SetModelInputFiles;
    property ModelFileName: string read FFileName write SetFileName;
    property ModflowOptions: TModflowOptions read FModflowOptions
      write SetModflowOptions;
    property ModflowWettingOptions: TWettingOptions read FModflowWettingOptions
      write SetModflowWettingOptions;
    property GlobalVariables: TGlobalVariables read FGlobalVariables
      write SetGlobalVariables;
    property HeadObsResults: THeadObsCollection read GetHeadObsResults
      write SetHeadObsResults stored StoreHeadObsResults;
    property MobileComponents: TMobileChemSpeciesCollection
      read GetMobileComponents write SetMobileComponents;
    property ImmobileComponents: TChemSpeciesCollection
      read GetImmobileComponents write SetImmobileComponents;
    // @name stores MODPATH pathline data.
    // @name is used only in MODFLOW models.
    property PathLines: TPathLineReader read GetPathLine write SetPathLine
      stored StorePathLine;
    // @name is retained for backwards compatibility. See @link(PathLines).
    property PathLine: TPathLineReader read GetPathLine write SetPathLine
      stored False;
    // @name stores MODPATH times series data.
    // @name is used only in MODFLOW models.
    property TimeSeries: TTimeSeriesReader read GetTimeSeries
      write SetTimeSeries stored StoreTimeSeries;
    // @name stores MODPATH endpoint data.
    // @name is used only in MODFLOW models.
    property EndPoints: TEndPointReader read GetEndPoints Write SetEndPoints
      stored StoreEndPoints;
    property SutraMesh: TSutraMesh3D read FSutraMesh write SetSutraMesh
      stored False;
    property SutraOptions: TSutraOptions read FSutraOptions
      write SetSutraOptions {$IFNDEF SUTRA} stored False {$ENDIF};
  end;

  TMapping = record
    ParentPostion: integer;
    ChildPositions: array of integer;
  end;

  TMappingArray = array of TMapping;

  TSaveDataSetValues = (sdsvNever, sdsvAlways);

  TFontChangeNotifyier = class(TFontAdapter)
  private
    FModel: TCustomModel;
  public
    procedure Changed; override;
    Constructor Create(AModel: TCustomModel; AFont: TFont);
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
  @link(FDataArrayManager).@link(TDataArrayManager.DataSets)
  and @link(TCustomModel.ScreenObjects) respectively.
  The number @Link(TDataArray)s or @Link(TScreenObject)s can be determined
  using @link(FDataArrayManager).@Link(TDataArrayManager.DataSetCount) and
  @Link(TCustomModel.ScreenObjectCount).
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
    FLayerStructure: TLayerStructure;
    FGuiSettings: TGuiSettings;
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
    FColorLegend: TLegend;
    FContourLegend: TLegend;
    FDisplaySettings: TDisplaySettingsCollection;
    FChildModels: TChildModelCollection;
    FImportingModel: boolean;
    FSelectedModel: TCustomModel;
    FColumnMapping: TMappingArray;
    FRowMapping: TMappingArray;
    FLayerMapping: TMappingArray;
    FCombinedDisplayColumn: integer;
    FCombinedDisplayLayer: integer;
    FCombinedDisplayRow: integer;
    FSaveDataSetValues: TSaveDataSetValues;
    FChildGridUpdateCount: Integer;
    FDataSetUpdateCount: Integer;
    FSaveBfhBoundaryConditions: Boolean;
    FContourFont: TFont;
    FShowContourLabels: Boolean;
    FSfrStreamLinkPlot: TSfrStreamLinkPlot;
    FMt3dmsOutputControl: TMt3dmsOutputControl;
    FMt3dmsTimes: TMt3dmsTimeCollection;
    FImmobileComponents: TChemSpeciesCollection;
    FMobileComponents: TMobileChemSpeciesCollection;
    FSutraLayerStructure: TSutraLayerStructure;
    // See @link(Exaggeration).
    function GetExaggeration: double;
    // See @link(OwnsScreenObjects).
    function GetOwnsScreenObjects: boolean;
    // See @link(ObjectList).
    function GetScreenObjectCollection: TScreenObjectCollection;
    // See @link(Version).
    function GetVersion: string;
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
    function DefaultArchiveName: string;
    function GetArchiveName: string;
    procedure SetArchiveName(const Value: string);
    procedure GetUnitID(var UnitID: Integer);
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
    procedure CreateInitialDataSetsForPhastTimeLists;
    procedure SetDisplaySettings(const Value: TDisplaySettingsCollection);
    procedure SetChildModels(const Value: TChildModelCollection);
    function StoreChildModels: Boolean;
    function GetChildModels: TChildModelCollection;
    function MaxChildColumnsPerColumn(ColIndex: Integer): integer;
    function MaxChildRowsPerRow(RowIndex: Integer): integer;
    function MaxChildLayersPerLayer(LayerIndex: Integer): integer;
    function CombinedCount(ViewDirection: TViewDirection): integer;
    function DirectionCount(ViewDirection: TViewDirection): integer;
    function MaxChildDisPerParentDis(ViewDirection: TViewDirection;
      Position: integer): integer;
    procedure UpdateAMapping(var AMapping: TMappingArray;
      ViewDirection: TViewDirection);
    procedure SetCombinedDisplayColumn(const Value: integer);
    procedure SetCombinedDisplayLayer(const Value: integer);
    procedure SetCombinedDisplayRow(const Value: integer);
    procedure SetSomeSegmentsUpToDate(const Value: boolean); override;
    function GetSomeSegmentsUpToDate: boolean; override;
    function GetNeedToRecalculateFrontCellColors: boolean;
    function GetNeedToRecalculateSideCellColors: boolean;
    function GetNeedToRecalculateTopCellColors: boolean;
    procedure SetNeedToRecalculateFrontCellColors(const Value: boolean);
    procedure SetNeedToRecalculateSideCellColors(const Value: boolean);
    procedure SetNeedToRecalculateTopCellColors(const Value: boolean);
    function AquiferPropertiesUsed(Sender: TObject): boolean; override;
    function KyUsed(Sender: TObject): boolean; override;
    function KzUsed(Sender: TObject): boolean; override;
    function PorosityUsed(Sender: TObject): boolean; override;
    function SpecificStorageUsed(Sender: TObject): boolean; override;
    function ChemistryUsed(Sender: TObject): boolean; override;
    function InitialHeadUsed(Sender: TObject): boolean; override;
    function EquilibriumPhasesUsed(Sender: TObject): boolean; override;
    function SurfacesUsed(Sender: TObject): boolean; override;
    function ExchangeUsed(Sender: TObject): boolean; override;
    function GasPhaseUsed(Sender: TObject): boolean; override;
    function SolidSolutionUsed(Sender: TObject): boolean; override;
    function KineticsUsed(Sender: TObject): boolean; override;
    function ModflowUsed(Sender: TObject): boolean; override;
    function RouteUzfDischarge(Sender: TObject): boolean; override;
    function ModflowInitialHeadUsed(Sender: TObject): boolean; override;
    function ConfiningBedKzUsed(Sender: TObject): boolean; override;
    function VerticalAnisotropyUsed(Sender: TObject): boolean; override;
    function HorizontalAnisotropyUsed(Sender: TObject): boolean; override;
    function SpecificYieldUsed(Sender: TObject): boolean; override;
    function WetDryUsed(Sender: TObject): boolean; override;
    function ModpathUsed(Sender: TObject): boolean; override;
    function HufReferenceSurfaceNeeded(Sender: TObject): boolean; override;
    function BcfUsed(Sender: TObject): boolean; override;
    function ConfinedStorageCoefUsed(Sender: TObject): boolean; override;
    function ZoneBudgetSelected(Sender: TObject): boolean; override;
    function SwtSelected(Sender: TObject): boolean; override;
    function SwtOffsetsUsed(Sender: TObject): boolean; override;
    function SwtSpecifiedUsed(Sender: TObject): boolean; override;
    function WettingActive: boolean; override;
    procedure InternalExportModflowLgrFile(const FileName: string);
    function GetCombinedDisplayColumn: integer;
    function GetCombinedDisplayLayer: integer;
    function GetCombinedDisplayRow: integer;
    function UpwIsSelected: Boolean;
    procedure SetSfrStreamLinkPlot(const Value: TSfrStreamLinkPlot);
    function SsmIsSelected: Boolean;
    function GetSutraLayerStructure: TSutraLayerStructure;
    procedure SetSutraLayerStructure(const Value: TSutraLayerStructure);
    procedure RenameOldVerticalLeakance; override;
  protected
    procedure SetFileName(const Value: string); override;
    function GetFormulaManager: TFormulaManager; override;
    function GetLayerStructure: TLayerStructure;override;
    procedure SetLayerStructure(const Value: TLayerStructure);override;
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
//    procedure SetModflowOptions(const Value: TModflowOptions);override;
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
//    procedure SetModelSelection(const Value: TModelSelection);override;
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
    function GetSelectedModel: TCustomModel; override;
    procedure SetSelectedModel(const Value: TCustomModel); override;
    procedure SetFrontTimeList(const Value: TCustomTimeList); override;
    procedure SetSideTimeList(const Value: TCustomTimeList); override;
    procedure SetTopTimeList(const Value: TCustomTimeList); override;
    function GetDisplayName: string; override;
    function GetSaveBfhBoundaryConditions: boolean; override;
    procedure SetSaveBfhBoundaryConditions(const Value: boolean);  override;
    procedure InitializeGages; override;
    procedure InitializeSfrWriter(EvaluationType: TEvaluationType); override;
    procedure FreeSfrWriter; override;
    procedure SetContourFont(const Value: TFont) ; override;
    procedure SetShowContourLabels(const Value: boolean);  override;
    function GetContourFont: TFont;  override;
    function GetShowContourLabels: boolean; override;
    procedure SetMt3dmsOutputControl(const Value: TMt3dmsOutputControl); override;
    function GetMt3dmsOutputControl: TMt3dmsOutputControl; override;
    function GetMt3dmsTimes: TMt3dmsTimeCollection; override;
    procedure SetMt3dmsTimes(const Value: TMt3dmsTimeCollection); override;
    function GetImmobileComponents: TChemSpeciesCollection; override;
    function GetMobileComponents: TMobileChemSpeciesCollection; override;
    procedure SetImmobileComponents(const Value: TChemSpeciesCollection); override;
    procedure SetMobileComponents(const Value: TMobileChemSpeciesCollection); override;
    procedure SetModelSelection(const Value: TModelSelection); override;
//    procedure SetGlobalVariables(const Value: TGlobalVariables); override;
  public
    function LakBathymetryUsed: Boolean;
    function TobIsSelected: Boolean;
//    procedure RenameDataArray(DataArray: TDataArray; const NewName: string);
    procedure DrawHeadObservations(const BitMap: TBitmap32;
      const ZoomBox: TQRbwZoomBox2); override;
    procedure DrawSfrStreamLinkages(const BitMap: TBitmap32;
      const ZoomBox: TQRbwZoomBox2);
    // Update relationships of parent grid with child grids.
    procedure UpdateMapping;
    function InitialWaterTableUsed(Sender: TObject): boolean; override;
    function ReservoirLayerUsed(Sender: TObject): boolean; override;
    function ReservoirPackageUsed(Sender: TObject): boolean; override;
    function LakePackageUsed(Sender: TObject): boolean; override;
    function UzfPackageUsed(Sender: TObject): boolean; override;
    function UzfUnsatVertKUsed(Sender: TObject): boolean; override;
    function UzfInitialInfiltrationUsed(Sender: TObject): boolean; override;
    function UzfResidualWaterContentUsed(Sender: TObject): boolean; override;
    procedure InvalidateMapping;
    procedure UpdateChildGrids;
    procedure UpdateDataSetConnections;
    function AddDataSet(const DataSet: TDataArray): Integer; override;
    function IsChildModelEdgeColRow(Col, Row, Layer: integer;
      out CModel: TBaseModel): boolean;
    function IsChildModelEdgeCell(Col, Row, Layer: integer;
      out CModel: TBaseModel): boolean;
    procedure AllowChildGridUpdates;
    procedure DisallowChildGridUpdates;
    property ImportingModel: boolean read FImportingModel write FImportingModel;
    procedure Assign(Source: TPersistent); override;
    // @name updates and invalidates data sets that may have been calculated
    // incorrectly in previous versions of ModelMuse.
    procedure FixOldModel;
    // @name is used when determining what data sets or global variables are
    // used when evaluating the formula for a MODFLOW boundary condition.
    // The names of all the @link(TDataArray)s and global variables are added
    // to NewUseList.
    procedure UpdateDisplayUseList(NewUseList: TStringList;
      ParamType: TParameterType; DataIndex: integer; const DisplayName: string); override;
    // @name invalidates all the @link(TModflowBoundaryDisplayTimeList)s.
    procedure InvalidateModflowBoundaries;
    // @name is the event handler for @link(TDataArray.OnDataSetUsed
    // TDataArray.OnDataSetUsed) in MODFLOW models for @link(TDataArray)s
    // that define the top of the model and the bottom of @link(TLayerGroup)s.
    function ModelLayerDataArrayUsed(Sender: TObject): boolean;
    function Mt3dMsInitialConcUsed(Sender: TObject): boolean;
    function ModDispDataArrayUsed(Sender: TObject): boolean;
    function Mt3dMsSorbImmobInitialConcUsed(Sender: TObject): boolean;
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
    // about @link(TDataArray)s from @link(DataSetList)
    // to @link(FDataArrayManager).@link(TDataArrayManager.DataSets).
    procedure UpdateDataSets;
    procedure UpdateFrontTimeDataSet(const TimeList: TCustomTimeList;
      const Time: double); override;
    // @name is called after reading a @classname to transfer the information
    // about @link(TScreenObject)s from @link(ObjectList) to
    // @link(ScreenObjects).
    procedure UpdateScreenObjects;
    procedure UpdateSideTimeDataSet(const TimeList: TCustomTimeList;
      const Time: double); override;
    procedure UpdateThreeDTimeDataSet(const TimeList: TCustomTimeList;
      const Time: double); override;
    // @name calls TPhastTimeList.@link(TPhastTimeList.Loaded)
    // for each @link(TPhastTimeList) in @link(TimeLists).
    procedure UpdateTimeLists;
    procedure UpdateTopTimeDataSet(const TimeList: TCustomTimeList;
      const Time: double); override;

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
    procedure InvalidateMfSfrData(Sender: TObject);
    procedure InvalidateMfSfrSegmentReachAndIcalc(Sender: TObject);
    procedure InvalidateMfSfrReachLength(Sender: TObject);
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
    procedure InvalidateMfSfrUpstreamDepth(Sender: TObject);
    procedure InvalidateMfSfrDownstreamDepth(Sender: TObject);

    property ArchiveName: string read GetArchiveName write SetArchiveName;
    procedure CreateArchive(const FileName: string; const ArchiveCommand: string = '');
//    procedure InvalidateMfHobHeads(Sender: TObject);
    function DefaultHigherElevationFormula(ViewDirection: TViewDirection): string;
    function DefaultLowerElevationFormula(ViewDirection: TViewDirection): string;
    function DefaultElevationFormula(ViewDirection: TViewDirection; EvalAt: TEvaluatedAt): string;
    function ParameterDataSetUsed(Sender: TObject): boolean;
    function GetScreenObjectByName(AName: string): TScreenObject; override;
    procedure CopyScreenObjectsToClipboard;
    procedure PasteObjectsFromClipboard(List: TList);
    property ModelMateProject: TProject read FModelMateProject
      write SetModelMateProject;
    procedure UpdateModelMateProject;
    procedure ImportFromModelMateProject(Project: TProject);
    procedure RegisterGlobalVariables(Parser: TRbwParser);

    property FormulaManager: TFormulaManager read GetFormulaManager;
    procedure ClearScreenObjectCollection;

    property ColorLegend: TLegend read FColorLegend;
    property ContourLegend: TLegend read FContourLegend;

    function FileVersionEqualOrEarlier(TestVersion: string): boolean;
    function CombinedColumnCount: integer;
    function CombinedRowCount: integer;
    function CombinedLayerCount: integer;
    property NeedToRecalculateTopCellColors: boolean read GetNeedToRecalculateTopCellColors write SetNeedToRecalculateTopCellColors;
    property NeedToRecalculateFrontCellColors: boolean read GetNeedToRecalculateFrontCellColors write SetNeedToRecalculateFrontCellColors;
    property NeedToRecalculateSideCellColors: boolean read GetNeedToRecalculateSideCellColors write SetNeedToRecalculateSideCellColors;
    procedure UpdateDataSetDimensions;
    function LgrUsed: boolean;
    function BcfIsSelected: Boolean;
    function ChdIsSelected: Boolean;
    function ChobIsSelected: Boolean;
    function De4IsSelected: Boolean;
    function DrnIsSelected: Boolean;
    function DrobIsSelected: Boolean;
    function DrtIsSelected: Boolean;
    function EtsIsSelected: Boolean;
    function EvtIsSelected: Boolean;
    function GbobIsSelected: Boolean;
    function GhbIsSelected: Boolean;
    function GmgIsSelected: Boolean;
    function HfbIsSelected: Boolean;
    function HobIsSelected: Boolean;
    function HufIsSelected: Boolean;
    function HydmodIsSelected: Boolean;
    function LakIsSelected: Boolean;
    function LpfIsSelected: Boolean;
    function Mnw2IsSelected: Boolean;
    function MODPATHIsSelected: Boolean;
    function Mt3dmsIsSelected: Boolean;
    function Mt3dmsSsmIsSelected: Boolean;
    function Mt3dmsTobIsSelected: Boolean;
    function PcgIsSelected: Boolean;
    function RchIsSelected: Boolean;
    function ResIsSelected: Boolean;
    function RivIsSelected: Boolean;
    function RvobIsSelected: Boolean;
    function SfrIsSelected: Boolean;
    function SipIsSelected: Boolean;
    function SubIsSelected: Boolean;
    function SwtIsSelected: Boolean;
    function UzfIsSelected: Boolean;
    function WelIsSelected: Boolean;
    function ZoneBudgetIsSelected: Boolean;
    function PackageIsSelected(APackage: TObject): Boolean;
    procedure ExportModflowLgrModel(const FileName: string;
      RunModel, ExportModpath, ExportZoneBudget, ShowWarning: boolean);
    procedure AdjustDataArray(ADataArray: TDataArray); override;
    function RchTimeVaryingLayers: boolean;
    function EvtTimeVaryingLayers: boolean;
    function EtsTimeVaryingLayers: boolean;
    procedure BeginGridChange; override;
    procedure EndGridChange; override;
    procedure BeginDataSetUpdate;
    procedure EndDataSetUpdate;
    property DataSetUpdateCount: integer read FDataSetUpdateCount;
    procedure UpdateCombinedDisplayColumn;
    procedure UpdateCombinedDisplayRow;
    procedure UpdateCombinedDisplayLayer;
    function DispersionSelected: boolean;
    function AnyDispersionMultiDiffusion: boolean;
    function AllDispersionMultiDiffusion: boolean;
    function AnyMt3dSorbImmobConc: boolean;
    function AnyMt3dSorbParameter: boolean;
    function AnyMt3dReactions: Boolean;
    procedure UpdateMt3dmsChemDataSets; override;
    function Mt3dMsFirstSorbParamUsed(Sender: TObject): boolean;
    function Mt3dMsSecondSorbParamUsed(Sender: TObject): boolean;
    function Mt3dmsReactionRateDisolvedUsed(Sender: TObject): boolean;
    function Mt3dmsReactionRateSorbedUsed(Sender: TObject): boolean;
    function CombinedLayerSimulated(ALayer: Integer): boolean;
  published
    // The following properties are obsolete.

    // @name stores FlowOnly option in PHAST.
    property FlowOnly: boolean write SetFlowOnly stored False;
    { @name is used to store fluid properties in PHAST.}
    { @name is only for backwards compatibility.  It is not used.}
    property FluidProperties: TFluidProperties  read FFluidProperties
      write FFluidProperties stored False;
    // @name stores the height in pixels of the front view of the model.
    property FrontHeight: integer // read GetFrontHeight
      write SetFrontHeight stored False;
    // @name stores the reference X-coordinate for the front view of the model.
    property FrontX: double {read GetFrontX} write SetFrontX stored False;
    // @name stores the reference Y-coordinate for the front view of the model.
    property FrontY: double {read GetFrontY} write SetFrontY stored False;
    // @name is the height of the main form in pixels.
    property Height: integer {read GetHeight} write SetHeight stored False;
    // @name is the X-coordinate of the main form in pixels.
    property Left: integer {read GetLeft} write SetLeft stored False;
    // @name is the magnification of the front view of the model.
    property MagnificationFront: double //read GetMagnificationFront
      write SetMagnificationFront stored False;
    // @name is the magnification of the side view of the model.
    property MagnificationSide: double //read GetMagnificationSide
      write SetMagnificationSide stored False;
    // @name is the magnification of the top view of the model.
    property MagnificationTop: double //read GetMagnificationTop
      write SetMagnificationTop stored False;
    // @name is the width in pixels of the side view of the model.
    property SideWidth: integer //read GetSideWidth
      write SetSideWidth stored False;
    // @name stores the reference X-coordinate for the side view of the model.
    property SideX: double {read GetSideX} write SetSideX stored False;
    // @name stores the reference Y-coordinate for the front view of the model.
    property SideY: double {read GetSideY} write SetSideY stored False;
    // @name is the Y-coordinate of the main form in pixels.
    property Top: integer {read GetTop} write SetTop stored False;
    // @name is the height of the top view of the model in pixels
    property TopViewHeight: integer //read GetTopViewHeight
      write SetTopViewHeight stored False;
    // @name is the width of the top view of the model in pixels
    property TopViewWidth: integer //read GetTopViewWidth
      write SetTopViewWidth stored False;
    // @name stores the reference X-coordinate for the top view of the model.
    property TopX: double {read GetTopX} write SetTopX stored False;
    // @name stores the reference Y-coordinate for the top view of the model.
    property TopY: double {read GetTopY} write SetTopY stored False;
    // @name is the width of the main form in GoPhast in pixels.
    property Width: integer {read GetWidth} write SetWidth stored False;
    // @name stores whether the model is maximized, minimized, or normal.
    property WindowState: TWindowState // read GetWindowState
      write SetWindowState stored False;

    // The following properties are used only in PHAST models.

    // @name is the diffusivity in PHAST.
    property Diffusivity: double read FDiffusivity write SetDiffusivity;
    // @name is used to store options related to the grid in PHAST.
    property GridOptions: TGridOptions read FGridOptions write FGridOptions;
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
    // @name represents a series of bitmaps that can be displayed on
    // the top, front, or side view of the model.
    // @name is used in both PHAST and MODFLOW models.
    property Bitmaps: TCompressedBitmapCollection read FBitmaps write
      SetBitmaps;
    // @name is the vertical exaggeration of the front, side, and 3D views
    // of the model in GoPhast.
    // @name is used in both PHAST and MODFLOW models.
    property Exaggeration: double read GetExaggeration write SetExaggeration;
    // @name is used to read or write @link(TScreenObject)s to or from files.
    // @name is used in both PHAST and MODFLOW models.
    property ObjectList: TScreenObjectCollection
      read GetScreenObjectCollection write SetScreenObjectCollection;
    // @name is the version of GoPhast that last saved the model that is being
    // edited.
    // @name is used in both PHAST and MODFLOW models.
    property Version: string read GetVersion write SetVersion;
    // @name specifies the size and appearance of various portions of the
    // ModelMuse main window.
    // @name is used in both PHAST and MODFLOW models.
    property GuiSettings: TGuiSettings read FGuiSettings write FGuiSettings;
    // @name is the ModelMate file associated with this model.
    property ModelMateProjectFileName: string read FModelMateProjectFileName
      write SetModelMateProjectFileName;
    //  see @link(TDisplaySettingsCollection).
    // @name is used in both PHAST and MODFLOW models.
    property DisplaySettings: TDisplaySettingsCollection read FDisplaySettings
      write SetDisplaySettings;
    property ChildModels: TChildModelCollection read GetChildModels
       write SetChildModels stored StoreChildModels;
    property SaveDataSetValues: TSaveDataSetValues read FSaveDataSetValues
      write FSaveDataSetValues default sdsvAlways;

    property ModflowSteadyParameters;
    property ModelSelection;
    property LayerStructure;
    property ModflowStressPeriods;
    property SoluteTransport;
    property UseWaterTable;
    property FreeSurface;
    property ChemistryOptions;
    property HufParameters;
    property ObservationPurpose;
    property ModflowTransientParameters;
    property ModflowOutputControl;
    property Mt3dmsOutputControl;
    property Mt3dmsTimes;
    property DataSetList;
    property CombinedDisplayColumn: integer read GetCombinedDisplayColumn
      write SetCombinedDisplayColumn;
    property CombinedDisplayRow: integer read GetCombinedDisplayRow
      write SetCombinedDisplayRow;
    property CombinedDisplayLayer: integer read GetCombinedDisplayLayer
      write SetCombinedDisplayLayer;
    property SaveBfhBoundaryConditions;
    property ContourFont;
    property ShowContourLabels;
    property SfrStreamLinkPlot: TSfrStreamLinkPlot read FSfrStreamLinkPlot
      write SetSfrStreamLinkPlot;
    property SutraLayerStructure: TSutraLayerStructure read GetSutraLayerStructure
      write SetSutraLayerStructure {$IFNDEF Sutra} stored False {$ENDIF};

  end;

  TChildDiscretization = class(TOrderedItem)
  private
    FParentLayerNumber: integer;
    FLayerGroup: TLayerGroup;
    FLayerGroupName: string;
    FDiscretization: integer;
    procedure SetDiscretization(const Value: integer);
    procedure SetLayerGroup(const Value: TLayerGroup);
    procedure SetLayerGroupName(const Value: string);
    procedure SetParentLayerNumber(const Value: integer);
    function GetLayerGroupName: string;
    function GetLayerGroup: TLayerGroup;
    function GetDiscretization: integer;
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure Loaded;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    property LayerGroup: TLayerGroup read GetLayerGroup write SetLayerGroup;
  published
    property LayerGroupName: string read GetLayerGroupName
      write SetLayerGroupName;
    // @name refers to the layer within the @link(LayerGroup).
    // @name starts at 0.
    property ParentLayerNumber: integer read FParentLayerNumber
      write SetParentLayerNumber;
    property Discretization: integer read GetDiscretization
      write SetDiscretization default 1;
  end;

  TChildDiscretizationCollection = class(TOrderedCollection)
  private
    FBottomUnitName: string;
    FBottomLayerInUnit: integer;
    FBottomLayerGroup: TLayerGroup;
    FChanged: Boolean;
    function GetBottomUnitName: string;
    procedure SetBottomLayerGroup(const Value: TLayerGroup);
    procedure SetBottomLayerInUnit(const Value: integer);
    procedure SetBottomUnitName(const Value: string);
    function GetItem(Index: integer): TChildDiscretization;
    procedure SetItem(Index: integer; const Value: TChildDiscretization);
    procedure Sort;
    function GetBottomLayerGroup: TLayerGroup;
  protected
    procedure Loaded;
    procedure Update(Item: TCollectionItem); override;
  public
    function IsSame(AnOrderedCollection: TOrderedCollection): boolean; override;
    constructor Create(Model: TBaseModel);
    procedure Assign(Source: TPersistent); override;
    property BottomLayerGroup: TLayerGroup read GetBottomLayerGroup
      write SetBottomLayerGroup;
    // SubLayer starts at zero.
    function GetAnItemByGroupAndLayer(LayerGroup: TLayerGroup;
      SubLayer: integer): TChildDiscretization;
    property Items[Index: integer]: TChildDiscretization read GetItem
      write SetItem; default;
    procedure SortAndDeleteExtraItems;
    // @name treats the top layer as layer 1 and only counts simulated layers.
    // This is a MODFLOW layer number.
    function BottomModflowParentLayerNumber: integer;
    // @name returns the index of the parent layer at the bottom of the
    // local grid.  The first layer is treated as zero.
    function BottomLayerIndex: integer;
    function ModflowLayerCount: integer;
    function ModflowConfiningBedCount: integer;
    procedure WriteLAYCB(const DiscretizationWriter: TObject); virtual;
  published
    property BottomUnitName: string read GetBottomUnitName
      write SetBottomUnitName;
    // @name indicates the lowermost layer in @link(BottomLayerGroup)
    // that will be part of the child model.
    // @name starts as 0.
    property BottomLayerInUnit: integer read FBottomLayerInUnit
      write SetBottomLayerInUnit;
  end;

  TGridRange = record
    First: integer;
    Last: integer;
  end;

  TStartingHeadSource = (shsSelf, shsParent);
  TLgrPrintChoice = (lpcScreen, lpcListing, lpcNone);
  TCouplingMethod = (cmOneWay, cmTwoWay);

  TChildModel = class(TCustomModel)
  private
    FParentModel: TCustomModel;
    FModelName: string;
    FDiscretization: TChildDiscretizationCollection;
    FChildCellsPerParentCell: integer;
    FHorizontalPositionScreenObject: TScreenObject;
    FCanUpdateGrid: Boolean;
    FShouldUpdateGrid: Boolean;
    FFirstCol: Integer;
    FLastCol: Integer;
    FFirstRow: Integer;
    FLastRow: Integer;
    FCreating: Boolean;
    FStartingHeadSource: TStartingHeadSource;
    FMaxIterations: integer;
    FLgrPrintChoice: TLgrPrintChoice;
    FFluxRelaxationFactor: double;
    FHeadRelaxationFactor: double;
    FFluxClosureCriterion: double;
    FHeadClosureCriterion: double;
    FCouplingMethod: TCouplingMethod;
    function GetSomeSegmentsUpToDate: boolean; override;
    procedure SetSomeSegmentsUpToDate(const Value: boolean); override;
    procedure SetModelName(const Value: string);
    procedure SetDiscretization(const Value: TChildDiscretizationCollection);
    procedure SetChildCellsPerParentCell(const Value: integer);
    procedure SetHorizontalPositionScreenObject(const Value: TScreenObject);
    procedure GetRowColPositions(const StartPosition, EndPosition: integer;
      const ParentPositions: TOneDRealArray; out ChildPostions: TOneDRealArray);
    procedure SetCanUpdateGrid(const Value: Boolean);
    function ParentPositionToChildPositions(ViewDirection: TViewDirection;
      APosition: integer): TGridRange;
    function MaxPosition(ViewDirection: TViewDirection): integer;
    procedure SetStartingHeadSource(const Value: TStartingHeadSource);
    procedure SetMaxIterations(const Value: integer);
    procedure SetLgrPrintChoice(const Value: TLgrPrintChoice);
    procedure SetFluxRelaxationFactor(const Value: double);
    procedure SetHeadRelaxationFactor(const Value: double);
    procedure SetFluxClosureCriterion(const Value: double);
    procedure SetHeadClosureCriterion(const Value: double);
    function ConvertIntegerParentArray(ParentArray: TOneDIntegerArray): TOneDIntegerArray;
    function ConvertRealParentArray(ParentArray: TOneDRealArray): TOneDRealArray;
    procedure SetCouplingMethod(const Value: TCouplingMethod);
    procedure AdjustCellPosition(var Column, Row, Layer: integer); overload;
    function GetSaveBfhBoundaryConditions: boolean; override;
    procedure SetSaveBfhBoundaryConditions(const Value: boolean);  override;
  protected
    function StoreHydrogeologicUnits: Boolean; override;
    function GetScreenObjects(const Index: integer): TScreenObject; override;
    function GetScreenObjectCount: integer; override;
    function GetModflowSteadyParameters: TModflowSteadyParameters; override;
    function GetLayerStructure: TLayerStructure; override;
    procedure SetLayerStructure(const Value: TLayerStructure); override;
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
    function GetSelectedModel: TCustomModel; override;
    procedure SetSelectedModel(const Value: TCustomModel); override;
    function GetFormulaManager: TFormulaManager; override;
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadFluxRelaxationFactor(Reader: TReader);
    procedure ReadHeadRelaxationFactor(Reader: TReader);
    procedure WriteFluxRelaxationFactor(Writer: TWriter);
    procedure WriteHeadRelaxationFactor(Writer: TWriter);
    procedure ReadHeadClosureCriterion(Reader: TReader);
    procedure ReadFluxClosureCriterion(Reader: TReader);
    procedure WriteHeadClosureCriterion(Writer: TWriter);
    procedure WriteFluxClosureCriterion(Writer: TWriter);
    function GetDisplayName: string; override;
    procedure SetContourFont(const Value: TFont) ; override;
    procedure SetShowContourLabels(const Value: boolean);  override;
    function GetContourFont: TFont;  override;
    function GetShowContourLabels: boolean; override;
    procedure SetMt3dmsOutputControl(const Value: TMt3dmsOutputControl); override;
    function GetMt3dmsOutputControl: TMt3dmsOutputControl; override;
    function GetMt3dmsTimes: TMt3dmsTimeCollection; override;
    procedure SetMt3dmsTimes(const Value: TMt3dmsTimeCollection); override;
    function GetImmobileComponents: TChemSpeciesCollection; override;
    function GetMobileComponents: TMobileChemSpeciesCollection; override;
    procedure SetImmobileComponents(const Value: TChemSpeciesCollection); override;
    procedure SetMobileComponents(const Value: TMobileChemSpeciesCollection); override;
  public
    property CanUpdateGrid: Boolean read FCanUpdateGrid write SetCanUpdateGrid;
    function LayerGroupUsed(LayerGroup: TLayerGroup): boolean; override;
    function LayerFractions(LayerGroup: TCustomLayerGroup): TDoubleDynArray; override;
    function LayerCount: integer;  override;
    function FirstOverlappedLayer: integer;
    procedure UpdateDisplayUseList(NewUseList: TStringList;
      ParamType: TParameterType; DataIndex: integer; const DisplayName: string); override;
    procedure Assign(Source: TPersistent); override;
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    property ParentModel: TCustomModel read FParentModel;
    function GetScreenObjectByName(AName: string): TScreenObject; override;
    procedure UpdateLayerCount;
    property HorizontalPositionScreenObject: TScreenObject
      read FHorizontalPositionScreenObject
      write SetHorizontalPositionScreenObject;
    procedure UpdateGrid;
    procedure UpdateDataSetConnections;
    function ChildColToParentCol(ACol: integer): integer;
    function ChildRowToParentRow(ARow: integer): integer;
    function ChildLayerToParentLayer(ALayer: integer): integer;
    function ParentColToChildCols(ACol: integer): TGridRange;
    function ParentRowToChildRows(ARow: integer): TGridRange;
    function ParentLayerToChildLayers(ALayer: integer): TGridRange;
    function EdgeIndex: integer;
    function Child_NameFile_Name(const Parent_NameFile_Name: string): string;
    property FirstCol: Integer read FFirstCol;
    property LastCol: Integer read FLastCol;
    property FirstRow: Integer read FFirstRow;
    property LastRow: Integer read FLastRow;
    function ModflowLayerCount: integer; override;
    function ModflowConfiningBedCount: integer; override;
    procedure WriteLAYCB(const DiscretizationWriter: TObject); override;
    function IsLayerSimulated(const LayerID: integer): boolean; override;
    Function Laytyp: TOneDIntegerArray; override;
    Function Layavg: TOneDIntegerArray; override;
    function Chani: TOneDIntegerArray; override;
    Function Layvka: TOneDIntegerArray; override;
    function Trpy: TOneDRealArray; override;
    Function TRPT: TOneDRealArray; override;
    function TRPV: TOneDRealArray; override;
    Function DMCOEF: TOneDRealArray; override;
    function GetLayerGroupByLayer(const Layer: integer): TLayerGroup; override;
    function ModflowLayerBottomDescription(const LayerID: integer): string; override;
    Function ModflowLayerToDataSetLayer(ModflowLayer: integer): integer; override;
    function DataSetLayerToModflowLayer(DataSetLayer: integer): integer; override;
    procedure AdjustCellPosition(AValueCell: TValueCell); overload; override;
    procedure AdjustCellPosition(ACellAssignment: TCellAssignment); overload; override;
    procedure AdjustDataArray(ADataArray: TDataArray); override;
    function DefaultModflowOutputFileName: string; override;
    procedure UpdateMt3dmsChemDataSets; override;
  published
    property ModelName: string read FModelName write SetModelName;
    property Discretization: TChildDiscretizationCollection
      read FDiscretization write SetDiscretization;
    property ChildCellsPerParentCell: integer read FChildCellsPerParentCell
      write SetChildCellsPerParentCell default 3;
    property StartingHeadSource: TStartingHeadSource read FStartingHeadSource
      write SetStartingHeadSource default shsSelf;
    property CouplingMethod: TCouplingMethod read FCouplingMethod
      write SetCouplingMethod stored True;
    property MaxIterations: integer read FMaxIterations
      write SetMaxIterations default 20;
    property LgrPrintChoice: TLgrPrintChoice read FLgrPrintChoice
      write SetLgrPrintChoice default lpcListing;
    property HeadRelaxationFactor: double read FHeadRelaxationFactor
      write SetHeadRelaxationFactor;
    property FluxRelaxationFactor: double read FFluxRelaxationFactor
      write SetFluxRelaxationFactor;
    property HeadClosureCriterion: double read FHeadClosureCriterion
      write SetHeadClosureCriterion;
    property FluxClosureCriterion: double read FFluxClosureCriterion
      write SetFluxClosureCriterion;
  end;

  TChildModelEdit = class(TOrderedItem)
  private
    FModelName: string;
    FDiscretization: TChildDiscretizationCollection;
    FChildCellsPerParentCell: integer;
    FChildModel: TBaseModel;
    FStartingHeadSource: TStartingHeadSource;
    FMaxIterations: integer;
    FLgrPrintChoice: TLgrPrintChoice;
    FFluxRelaxationFactor: double;
    FHeadRelaxationFactor: double;
    FFluxClosureCriterion: double;
    FHeadClosureCriterion: double;
    FCouplingMethod: TCouplingMethod;
    procedure SetDiscretization(const Value: TChildDiscretizationCollection);
    procedure SetModelName(const Value: string);
    procedure SetChildCellsPerParentCell(const Value: integer);
    procedure SetStartingHeadSource(const Value: TStartingHeadSource);
    procedure SetMaxIterations(const Value: integer);
    procedure SetLgrPrintChoice(const Value: TLgrPrintChoice);
    procedure SetFluxRelaxationFactor(const Value: double);
    procedure SetHeadRelaxationFactor(const Value: double);
    procedure SetFluxClosureCriterion(const Value: double);
    procedure SetHeadClosureCriterion(const Value: double);
    procedure SetCouplingMethod(const Value: TCouplingMethod);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    Constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property ModelName: string read FModelName write SetModelName;
    property Discretization: TChildDiscretizationCollection
      read FDiscretization write SetDiscretization;
    property ChildCellsPerParentCell: integer read FChildCellsPerParentCell
      write SetChildCellsPerParentCell default 3;
    property StartingHeadSource: TStartingHeadSource read FStartingHeadSource
      write SetStartingHeadSource default shsSelf;
    property CouplingMethod: TCouplingMethod read FCouplingMethod
      write SetCouplingMethod stored True;
    property MaxIterations: integer read FMaxIterations
      write SetMaxIterations default 20;
    property LgrPrintChoice: TLgrPrintChoice read FLgrPrintChoice
      write SetLgrPrintChoice default lpcListing;
    property HeadRelaxationFactor: double read FHeadRelaxationFactor
      write SetHeadRelaxationFactor;
    property FluxRelaxationFactor: double read FFluxRelaxationFactor
      write SetFluxRelaxationFactor;
    property HeadClosureCriterion: double read FHeadClosureCriterion
      write SetHeadClosureCriterion;
    property FluxClosureCriterion: double read FFluxClosureCriterion
      write SetFluxClosureCriterion;
  end;

  TChildModelEditCollection = class(TOrderedCollection)
  public
    constructor Create;
  end;

  TChildModelItem = class(TOrderedItem)
  private
    FChildModel: TChildModel;
    procedure SetChildModel(const Value: TChildModel);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure Loaded;
  public
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ChildModel : TChildModel read FChildModel write SetChildModel;
  end;

  TChildModelCollection = class(TOrderedCollection)
  private
    function GetItem(Index: integer): TChildModelItem;
    procedure SetItem(Index: integer; const Value: TChildModelItem);
    procedure UpdateUnitNumbers;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Loaded;
    property Items[Index: integer]: TChildModelItem read GetItem
      write SetItem; default;
  published
    constructor Create(Model: TBaseModel);
  end;

  procedure EnableLighting;

  // @name generates a name for a data set that is valid
  // and does not conflict with the names of any existing data sets.
  function GenerateNewName( Root: string = '';
    InvalidNames: TStringList = nil; Connector: string = ''): string;

  {@name is used to generate a valid name from one that may be invalid.
  Valid names must begin with a letter or underscore.  The remaining
  characters must be letters, digits or the underscore character.}
  function GenerateNewRoot(const Root: string): string;

resourcestring
  StrGlobalVariables = 'Global Variables';
const
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
  //      that display hydraulic properties that result from applying the
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
  //     Bug fix: Fixed deletion of vertices in an object when all but one
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
  //   '2.4.0.1' Enhancement: It is now possible to copy and paste multiple
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
  //       LPF package was anything besides a zero or one.
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
  //       area or just the grid lines inside the active area.
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
  //       MODFLOW Packages and Programs dialog box.
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
  //     Change: The elapsed time is now included in the data set comment
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

  //   '2.8.0.1' Bug fix: It is no longer possible to show the Manage Head
  //       Observations dialog box before any head observations have been
  //       defined. Doing so could cause access violations.
  //     Bug fix: Under certain circumstances, the "Add point sections",
  //       "Add polyline sections", or "Add polygon sections" buttons could
  //       be pressed when there is no selected object causing an
  //       assertion failure. The buttons now become disabled under those
  //       conditions.
  //     Enhancement: Grid data can now be exported to 3D Shapefiles.
  //   '2.8.0.2' Enhancement: In the Search for Objects dialog box, the
  //       tree remains open after changing which checkboxes are
  //       checked.
  //     Bug fix: Attempting to import an empty or invalid results file
  //       now generates an error message for the user instead of a bug report.
  //   '2.8.0.3' Enhancement: Improved warning messages when a specified
  //       time for a boundary condition is outside of the range of times
  //       defined for the stress periods.
  //     Bug fix: Removed incorrect warning message generated when the SFR
  //       package was used and the starting time was greater than zero.
  //     Bug fix: Fixed assignment of custom layer discretization in
  //       Layer Groups dialog box.
  //     Bug fix: Deleting a layer group and then undoing the deletion no
  //       longer causes an assertion failure.
  //     Bug fix: Fixed a problem in which operating ModelMuse on a computer
  //       for which the language settings specify the decimal point
  //       to be something other than a period caused conversion errors.
  //   '2.8.0.4' Change: When creating polygon objects or adding
  //       polygon sections to existing objects, clicking on the first
  //       point of the polygon will close the polygon instead of continuing it.
  //     Enhancement: Improved speed of "Select Object by Name" dialog box.
  //     Bug fix: the "Set Widths of Columns, Rows, and Layers" dialog box now
  //       shows the selected columns, rows, and layers when it is displayed.
  //     Bug fix: It is no longer possible to attempt to color or contour
  //       the grid before the grid is defined.
  //     Bug fix: When sampling a DEM, only one DEM at a time could be selected.
  //     Enhancement: Added option to import ASCII raster file.
  //   '2.8.0.5' Bug fix: Fixed reversal of imported ASCII raster files.
  //     (bug is not in released version.)
  //   '2.8.0.6' Enhancement: added additional error checking in MNW2 package.
  //     Bug fix: Fixed the importing of world files in "Import Bitmap"
  //       dialog box. Previously, attempting to import some world files
  //       would fail because of extra spaces before the beginning of a number.
  //   '2.8.0.7' Change: When exporting Shapefiles attribute names with
  //       an ending trailing underscore have the underscore removed.
  //     Bug fix: When exporting Shapefiles of contours or MODPATH data,
  //       the bounding boxes of each shape are now set correctly.
  //   '2.8.0.8' Bug fix: Exporting contour Shapefiles when the specify contour
  //        option is checked and contour values are copied from another
  //        data set no longer causes an error.
  //      Bug fix: Previewing or exporting a series of images containing
  //        contours no longer causes an error when the range of values
  //        varies among the images.
  //   '2.8.0.9' Bug fix: Attempting so save a model archive on a disk with
  //        insufficient space now generates an error message instead of a
  //        bug report.
  //      Enhancement: There is a new way to display the Vertex Values dialog
  //        box. If no objects are selected on the view of the model
  //        with which the user is working, the user can double-click on a
  //        vertex of any object to display the Vertex Values dialog box.
  //      Enhancement: When importing model results, the default choice for
  //        how the imported results should be displayed now depends on the
  //        user's past choices on how the results should be displayed.
  //      Enhancement: There is now a button on the tool bar for importing
  //        model results.
  //      Enhancement: Vertex values can now be edited in the Object Properties
  //        dialog box.
  //      Bug fix: Previously, it you contoured a data set, closed the file,
  //        opened another one without closing ModelMuse and attempted to
  //        contour a data set, an access violation would occur. That access
  //        violation has been eliminated.
  //      Bug fix: When importing existing MODFLOW models, ModelMuse will now
  //        check that in all the inactive cells, the top of the layer is above
  //        the bottom of the layer. The elevations will be fixed in
  //        any inactive cells that do not meet this criterion.
  //   '2.8.0.10' Bug fix: ModelMuse can no longer enter an infinite loop when
  //        generating parameter instance names.
  //   '2.8.0.11' Bug fix: Eliminated an integer overflow error that could
  //        occur when exporting the RCH, EVT, or EVT packages.
  //   '2.8.0.12' Bug fix: Creating a new integer, Boolean, or text data set no
  //        longer fails.
  //      Enhancement: The user can now choose whether or not to save data set
  //        values when those values are up-to-date.
  //      Change: When data sets are deleted, the user is now prompted
  //        to delete unused objects.
  //      Change: The column, row and layer displayed in on the Grid Value
  //        dialog box is now the column row, and layer of the cell under the
  //        cursor even if a 2D data set is used to color the grid or
  //        is being contoured.
  //      Bug fix: In the SFR package, a warning about SFR segments circling
  //        back on themselves was sometimes generated when it should not have
  //        been generated and the segments involved would not be included in
  //        in the SFR package input file.
  //    '2.8.0.13' Enhancement: The speed of exporting the input files has
  //        increased
  //      Bug fix: Deleting all the vertices of the last section of an object
  //        when that section was a polygon caused an error.
  //      Bug fix: Making a background image too big now causes it to be
  //        hidden rather than generating an error message.
  //      Change: When defining CHD boundaries, an object with multiple
  //        sections will not define separate boundaries for each section.
  //    '2.8.0.14' Enhancement: In the SFR package, a warning is generated
  //        if the stream segment numbers are in strict numerical order but a
  //        segment with a higher number provides flow to one with a lower
  //        segment number.
  //      Enhancement: The Grid Value dialog box now displays information
  //        about the closest MODPATH pathline if it is within 1 cell or
  //        five pixels of the cursor.
  //    '2.8.0.15' Bug fix: Attempting to export a model without first
  //        defining a grid now results in an error message instead of
  //        generating a bug report.
  //      Bug fix: invalid formulas for river conductance and other, similar
  //        data no longer causes an access violation.
  //    '2.8.0.16' Bug fix: Fixed bug that prevented data for SFR data for a
  //        time period from being deleted.
  //    '2.8.0.17' It is now possible for the recharge from several sources to
  //        be added together.
  //    '2.8.0.18' reduced memory usage when recharge from several sources is
  //        added together.
  //    '2.8.0.19' It is now possible for the infiltration in the UZF package
  //        from several sources to be added together.
  //    '2.8.0.20' Bug fix: In some models, the SFR package could not be
  //        exported correctly.
  //      Bug fix: In some models, the Lake_ID numbers were not set incorrectly.
  //    '2.8.0.21' Bug fix: the selected layer wasn't being restored properly
  //        when opening a model (not in released version).
  //      Bug fix: Changing a data set orientation could cause error messages
  //        to be incorrectly generated for other data sets.
  //    '2.8.0.22' Bug fix: Fixed error messages for stream segments that
  //        are out of order.  (Bug was not in released version.)
  //    '2.8.0.23' ---
  //    '2.8.0.24' Bug fix: Object used to define the return location in the
  //        DRT package are now displayed in the Show or Hide Objects
  //        dialog box under an appropriate heading.
  //      Bug fix: If you undo and then redo the creation of parameters,
  //        objects that use those parameters will no longer lose them.
  //      Change: It is now possible to select multiple cells in all tables.
  //    '2.8.0.25' Enhancement: When importing Shapefiles, the numbers of any
  //        shapes with multiple parts will be displayed.
  //    '2.8.0.26' ModelMonitor has been updated to work with LGR.
  //    '2.8.0.27' Enhancement: The export image dialog box now has a "Copy
  //        image" button that copies the image to the clipboard.
  //      Enhancement: The Data Set Values dialog box now shows 2D Front and
  //        2D Side data sets in a single table instead of one table for each
  //        layer.
  //      Bug fix: The Object Properties dialog box no longer shows times
  //        related to time-varying layers in the RCH, EVT, and ETS packages
  //        unless time-varying layers have been selected in the corresponding
  //        package.
  //    '2.8.0.28' Bug fix: Multiplier and zone array names are now
  //        no longer than 10 characters in length.
  //      Enhancement: Improved speed of importing model results.
  //    '2.8.0.29' Bug fix: Fixed reading heads. Bug not in released version.
  //    '2.8.0.30' Bug fix: Fixed bug that caused access violations when
  //        editing packages with the HFB package selected. Bug not
  //        in released version.
  //    '2.8.0.31' Bug fix: Fixed LGR related bug that could cause, wells,
  //        drains, etc to be exported incorrectly (not in released version).
  //    '2.8.0.32' Bug fix: Fixed bug in editing the objects that control
  //        the horizontal placement of LGR Grids.
  //    '2.8.0.33' Bug fix: Fixed bug that would cause range check errors or
  //        access violations when animating contours.
  //      Enhancement: When deleting vertices of an object with imported data,
  //        the corresponding imported data will be deleted too.
  //      Enhancement: New command "Object|Edit|Edit|Invert Selected Vertices"
  //        to invert the selected nodes.
  //      Enhancement: New command "Object|Edit|Split Selected Objects"
  //        to convert each part of an object to a separate object.
  //      Enhancement: New command "Object|Edit|Make Selected Vertices
  //        a Separate Object" converts the selected vertices of an object to
  //        a new object while deleting them from the existing object.
  //      Enhancement: New command "Object|Edit|Split Object at Selected
  //        Vertices" to convert split an object into two separate objects.
  //        The objects will be split at the location(s) of any selected
  //        vertices with both objects sharing the selected vertices.
  //      Bug fix: Adding recharge, evapotranspiration, or ETS parameters
  //        formerly could lead to access violations later on.
  //      Enhancement: added support for running a single model with BFH data
  //        with MODFLOW-LGR.
  //      Bug fix: Fixed an assertion failure in specifying
  //        zero flow observations.
  //      Bug fix: When reversing the order of vertices in an object, the
  //        order of any imported data is reversed too.
  //    '2.8.0.34' Bug fix: new commands for editing objects were never
  //        enabled. (not in released version).
  //    '2.8.0.35' Enhancement: In the Import Gridded Data dialog box, data
  //        for a single grid row may not be spread over several lines when
  //        pasting data into the grid for arrays.
  //    '2.8.0.36' Enhancement: Three new functions added:
  //        ObjectCurrentSegmentAngle, ObjectCurrentSegmentAngleDegrees, and
  //        ObjectCurrentSegmentAngleLimitedDegrees. See help for
  //        descriptions.
  //    '2.9.0.0' Enhancement: Added support for MODFLOW-LGR.
  //      Change: When importing head observations from a Shapefile, a default
  //        value for the observation name is used if the user has not
  //        assigned a name.
  //      Enhancement: Improved responsiveness in the Manage Flow Observations
  //        dialog box.
  //      Enhancement: Decreased time required to display the
  //        MODFLOW Time dialog box.
  //      Bug fix: Fixed bug that could cause access violations when displaying
  //        the Object Properties dialog box.
  //      Bug fix: Fixed a bug that caused the multiplier and zone array names
  //        for the LPF package to change each time the model was exported.
  //      Bug fix: Fixed bug that could cause range check errors when coloring
  //        the grid with the RCH, EVT, or ETS packages.
  //    '2.9.0.1' Bug fix: Fixed bug in which a change in a vertex value of
  //        an object failed to cause the data sets dependant on that value
  //        to be updated.
  //      Bug fix: It is no longer possible to create an object with no
  //        vertices by starting creating an object and then deleting all the
  //        vertices by pressing the ESC key.
  //      Bug fix: It is no longer possible to generate an assertion failure by
  //        attempting to import a results file by typing the name of a file
  //        with an extension not recognized by ModelMuse.
  //      Bug fix: The STORAGECOEFFICIENT option in the LPF package can now
  //        be edited and stored properly.
  //      Bug fix: Fixed a bug in which the selected column, row, or layer
  //        was not always displayed properly by the ModelCube.
  //      Bug fix: Attempting to import gridded data before defining the grid
  //        now results in an error message instead of generating a bug report.
  //      Bug fix: When editing Flow observations, some observations
  //        were not saved.
  //    '2.9.1.0' Bug fix: Entering an unreasonable value for the grid origin
  //        when creating a new model no longer causes an access violation.
  //      Bug fix: Sampling a DEM is now possible when the language setting on
  //        the computer specify a comma as the decimal separator.
  //      Bug fix: If a ModelMuse file can not be saved to the disk, a warning
  //        message is displayed rather than generating a bug report.
  //      Bug fix: If there are no valid stress periods defined, in the
  //        MODFLOW Time dialog box, the dialog box does not save the stress
  //        periods.
  //      Bug fix: It is no longer possible to edit the cell in the table
  //        of the start-up dialog box for MODFLOW by tabbing to the cell.
  //      Bug fix: The variables in the LPF package are now initialized
  //        properly in a new model.
  //      Bug fix: Importing values from ModelMate now works properly when
  //        a decimal separator other than a period is used.
  //      Bug fix: When exporting an or updating ModelMate files where the
  //        user has not previously specified a the ModelMate location,
  //        the file is still correctly opened by ModelMate.
  //      Bug fix: Fixed a bug that could cause access violations when
  //        ModelMuse was closing.
  //      Bug fix: Fixed bug that would cause access violations when closing
  //        the Object Properties dialog box.
  //      Bug fix: Incorrect error messages are no longer generated when
  //        coloring the grid with the Head observations.
  //    '2.9.1.1' Fixed bug that could cause the incorrect text to be replaced
  //         in the formula editor when editing a formula.
  //      Bug fix: If the selected object is hidden, the dotted box that
  //         sometimes surrounds the selected object is also hidden.
  //      Enhancement: Added support for MODFLOW-NWT.
  //      Change: when importing Surfer grid files, the "files of type" is
  //        set to the value at which it was set the previous time a Surfer
  //        grid file was imported since the program was started.
  //    '2.9.1.2' Bug fix: "Objects|Hide All Objects" didn't work if only
  //        one object was visible.
  //    '2.9.1.3' ---
  //    '2.9.1.4' Change: The reference stress period for head and flow
  //        observations will now be exported as the stress period containing
  //        the observation instead of the first stress period.
  //      Change: ModelMuse is now compiled with Delphi XE instead of
  //        Delphi 2006. Small changes in the model input changes may occur
  //        due to the change in compiler.
  //      Enhancement: When exporting the MODFLOW input files, the user also
  //        has an opportunity to export the MODPATH or ZONEBUDGET input files
  //        too.
  //      Enhancement: It is now possible to import CHD, DRN, GHB, RIV, and WEL
  //        MODFLOW boundaries from the Import Points dialog box.
  //    '2.9.1.5' Enhancement: In the Object Properties dialog box, times
  //        for MODFLOW boundary conditions are automatically filled in when
  //        a previous time is selected.
  //      Bug fix: When importing an existing MODFLOW model, Vertical anisotropy
  //        in the LPF package was not imported correctly if vertical anisotropy
  //        was used in all layers.
  //    '2.9.1.6' Bug fix: The ObjectCurrentSegmentAngle,
  //        ObjectCurrentSegmentAngleDegrees, and
  //        ObjectCurrentSegmentAngleLimitedDegrees functions did not return
  //        values that were relative to the grid.
  //      Bug fix: Fixed a problem with setting an HFB parameter name
  //        that caused access violations when closing the MODFLOW Packages
  //        and Programs dialog box.
  //    '2.9.1.7' Enhancement: ModelMuse can now import and display the
  //        head observation output.
  //    '2.9.1.8' ----
  //    '2.9.1.9' Bug fix: In the GMG solver, the maximum number of iterations
  //        could not be edited.
  //    '2.9.1.10' Bug fix: Fixed export of head observations with LGR child
  //        models.
  //      Bug fix: In some cases, ModelMuse files in which the HFB package
  //        was used could not be read.
  //      Bug fix: Renaming a global variable that is used in the formula
  //        for the HFB package will now cause the formula to be updated.
  //      Bug fix: Fixed reading binary result files. (Bug not in released
  //        version of ModelMuse.)
  //    '2.9.1.11' Bug fix. When opening a browser, ModelMuse no longer
  //        attempts to delete files that don't exist.
  //    '2.9.1.12' ----
  //    '2.9.1.13' ----
  //    '2.9.1.14' Enhancement: It is now possible to import CHD, DRN, GHB,
  //        RIV, and WEL MODFLOW boundaries as well as head observations
  //        from the Import Points dialog box.
  //    '2.9.1.15' ----
  //    '2.9.1.16' Fixed naming of objects when importing head observations
  //        from the Import Points dialog box. (Bug not in released
  //        version of ModelMuse.)
  //    '2.9.1.17' ----
  //    '2.10.0.0' No additional changes.
  //    '2.10.0.1' Bug fix: Importing a feature that allows for a MODFLOW
  //        parameter from a Shapefile no longer causes a range check error
  //        if no parameter is being imported.
  //    '2.10.1.0' Change: When a message about a new version is displayed,
  //        the version numbers of the new version as well as the version
  //        that is being used are both displayed.
  //    '2.10.2.0' Bug fix: Undoing certain operations no longer results
  //        in a range check error.
  //      Enhancement: Attempting to import a formatted head file that
  //        contains "NaN" no longer results in a bug report.
  //    '2.10.2.1' Bug fix: SYTP parameters in the HUF package are now written
  //        to the PVAL and UCODE template files.
  //    '2.10.2.2' Bug fix: If the HFB package was selected, using the
  //        MODFLOW Packages and Programs dialog box no longer causes access
  //        violations.
  //      Bug fix: When the HFB package was selected, exporting a model that
  //        uses zone arrays no longer causes an Assertion failure.
  //    ''2.10.2.3' Bug fix: Fixed a problem in which deleting vertices of an
  //        object that included imported text data, more of the text data was
  //        deleted than should have been deleted.
  //      Bug fix: Fixed bugs that could cause access violations when
  //        ModelMuse was closed.
  //      Bug fix: Reading World Files on computers where the decimal separator
  //        is set to a value other than '.' now works correctly.
  //      Bug fix: When importing points, it is no longer possible to
  //        attempt to define an invalid object name.
  //    '2.10.3.0' No further changes.
  //    '2.10.3.1' Bug fix: Fixed bug that could cause a
  //        "List index out of bounds" error when exporting Shapefiles.
  //    '2.10.3.2' Bug fix: fixed bug that could ModelMuse to hang when
  //        starting a new model.
  //      Bug fix: When reading MODPATH pathline files, negative values of time
  //        were not handled properly.
  //      Bug fix: Attempting to export a model in which the reservoir package
  //        is selected but no reservoirs have been defined now results in
  //        an error message instead of a bug report.
  //    '2.10.3.3' Enhancement: Error or warning messages are now issued when
  //        a boundary condition package has been activated but no boundaries
  //        for it have been defined.
  //      Change: When closing a model, the prompt asking the user if they
  //        wish to save the model now includes the file name if the file name
  //        has been specified.
  //      Enhancement: ModelMuse now warns the user if duplicate SFR
  //        parameter instances are being used.
  //      Bug fix: Fixed export of TBEGIN in MODPATH main file.
  //      Enhancement: In LGR models, Streams in the SFR package are now linked
  //        between grids.
  //    '2.10.3.4' Bug fix: Fixed bug with selecting and drawing objects when
  //        zoomed in a great deal.
  //    '2.10.3.5' Bug fix: In LGR models, the selected column, row, and layer
  //        are now read correctly when opening a ModelMuse file.
  //      Bug fix: Fixed bug that could cause an access violation if the number
  //        of columns or rows was set to zero.
  //    '2.10.3.6' Bug fix: In PHAST models, switching an object between being
  //        evaluated at nodes and elements no longer causes a bug report to
  //        be generated.
  //    '2.10.3.7' Change: The most recent version of PHAST no longer supports
  //        specifying an initial water table. However, you can still use
  //        the initial water table option in ModelMuse. ModelMuse will use
  //        the data in the Initial_Water_Table data set to specify the
  //        initial head in a vertical column of nodes.
  //      Enhancement: The Grid Value dialog box can now display data about
  //        the nearest visible MODPATH end point.
  //      Bug fix: When attempting to import model results, trying to import
  //        from a file that is being used by another program now results in an
  //        error message to the user instead of a bug report.
  //    '2.11.0.0' Enhancement: Added support for GOFAIL option in MODFLOW-NWT.
  //    '2.11.0.1' Enhancement: When importing an existing MODFLOW model,
  //        .mfn is now recognized as a valid extension for a MODFLOW name file.
  //      Bug fix: Previously some erroneous error or warning messages were
  //        generated for data set values in inactive cells.
  //      Bug fix: When a background image was replaced with a new one, the
  //        name of the image stored in ModelMuse is now updated.
  //      Enhancement: It is now possible to import multiple ASCII raster files
  //        at one time.
  //    '2.11.0.2' Enhancement: Added a measurement function that allows the
  //        user to measure distances easily.
  //      Enhancement: Contours are now labeled.
  //      Change: "GOFAIL" option in MODFLOW-NWT has been renamed "CONTINUE".
  //    '2.11.0.3' Bug fix: In the Object Properties dialog box,
  //        a "no parameter" checkbox has been added to the beginning of the
  //        list of parameters for MODFLOW features that allow multiple
  //        parameters to be used with a single object. The "no parameter"
  //        check box can be unchecked to turn off the definition of a boundary
  //        that does not use parameters.
  //      Bug fix: When a data set defines the multiplier or zone array of a
  //        MODFLOW parameter, it now is possible to delete the data set if
  //        the associated parameter is deleted.
  //      Enhancement: The positions of objects can now be locked so that they
  //        can't be moved accidentally.
  //      Change: The multiplier and zone arrays used by the RCH, EVT, and ETS
  //        packages are now exported to separate files that are accessed
  //        using the OPEN/CLOSE option. This allows the zone and multiplier
  //        arrays in other packages to be changed without the need to
  //        export the RCH, EVT, and ETS packages. The files are stored in
  //        a subdirectory named "arrays."
  //      Bug fix: When importing the UZF package from an existing MODFLOW
  //        model, NUZTOP is now imported correctly.
  //      Change: The following dialog boxes have been deleted and their
  //        functionality has been moved to the new Data Visualization
  //        dialog box: Color Grid, Contour Data, MODPATH Pathline Display,
  //        MODPATH Endpoint Display, MODPATH Time Series Display, and
  //        Head Observation Results.
  //      Enhancement: In the Data Visualization dialog box, it is now possible
  //        to display linkages between streams in the stream package.
  //      Bug fix: When editing objects that define SFR streams, the controls
  //        for editing multiple cells in tables are now positioned properly.
  //      Bug fix: Fixed a bug that caused access violations when the system
  //        color was changed.
  //    '2.11.0.4' Bug fix. Exporting a grid data to a shape file for data sets
  //        whose names are longer than 10 characters now works properly.
  //    '2.11.0.5' Change: The zone and multiplier arrays files for
  //        RCH, EVT, and ETS are stored in a subdirectory named "arrays."
  //    '2.12.0.0' Change: In the NWT solver, the default value for flux
  //        tolerance has been increased from 0.006 to 0.06
  //      Bug fix: Deleting data sets and then opening another ModelMuse project
  //        no longer results in an assertion failure.
  //      Bug fix: Fixed a problem with invalid cast errors in PHAST models
  //        when creating or editing objects.
  //      Enhancement: In the Manage Head Observations dialog box, the user
  //        can now select multiple rows and then click the "Highlight selected
  //        objects" button to select all the objects that define objects
  //        on the selected rows.
  //      Enhancement: In the Head Observation Results pane of the
  //        Data Visualization dialog box, the data can be sorted by clicking
  //        on the column headers
  //      Enhancement: In the Head Observation Results pane of the
  //        Data Visualization dialog box, the objects that define the
  //        observations in the selected row of the results table can be
  //        selected by clicking the "Highlight selected objects" button.
  //      Bug fix: Attempting to read an invalid ModelMate file now generates
  //        a warning message instead of a bug report.
  //      Bug fix: Attempting to open a ModelMuse file with a length of zero
  //        now results in an error message to the user rather than a bug
  //        report.
  //      Bug fix: In the PHAST Print Frequency dialog box, it is no longer
  //        possible to delete all the columns in the table.
  //    '2.12.0.1' Enhancement: ModelMuse now checks the file date for MODFLOW
  //        and related models and warns the user if a more recent version has
  //        been released.
  //      Bug fix: ModelMuse again responds to the mouse wheel.
  //    '2.12.0.2' Bug fix: ModelMuse now only allows a ModelMuse file to be
  //        saved if it has one of the correct extensions.
  //      Bug fix: Access violations that sometimes occurred when changing
  //        the names of layer groups have been fixed.
  //      Bug fix: In the MODFLOW Program Locations dialog box, "ModelMuse.exe"
  //        is no longer accepted as a valid name for any of the programs.
  //      Bug fix: Undo/Redo capability added for changes to the output control.
  //      Bug fix: ModelMuse can now import the gage package correctly when
  //        a lake gage has an OUTTYPE of 4 or a stream gage has an OUTTYPE
  //        of 8.
  //      Bug fix: ModelMuse now updates user-entered real numbers to the
  //        proper format when the the user changes the language settings.
  //      Bug fix: (bug not in released version) In MODFLOW-NWT models,
  //        switching to use the NWT or UPW packages no longer causes a stack
  //        overflow.
  //      Bug fix: Fixed in bug in which attempting to delete a row from a
  //        table in which no row was selected caused an exception.
  //      Bug fix: Fixed bug in which SFR data was corrupted if ICALC was not
  //        specified.
  //    '2.12.0.3' Enhancement: Reduced memory usage when importing MODFLOW
  //        models that use the UZF package.
  //      Enhancement: When importing MODFLOW features from Shapefiles,
  //        most features can be imported into a single, multi-part object.
  //      Bug fix: Fixed bug importing the MNWI package from an existing
  //        MODFLOW-2005 model.
  //      Bug fix: Fixed bug that could sometimes prevent the Object Properties
  //        dialog box from being displayed when attempting to edit
  //        multiple objects.
  //      Bug fix: In the MODFLOW Time dialog box, attempting to use the
  //        Time Step Length Calculator without first specifying the Stress
  //        period length and multiplier now generates a warning message to the
  //        user instead of a bug report.
  //      Change: When running MODFLOW from ModelMuse, the lines in the batch
  //        file to display the listing file now occurs immediately after
  //        MODFLOW has finished instead of after the lines to run MODPATH
  //        and Zonebudget.
  //    '2.12.0.4' Bug fix: When the user has customized the regional settings
  //        to use a decimal separator that is different from the usual one,
  //        this no longer causes an error when converting text to
  //        floating point values.
  //    '2.12.0.5' Enhancement: The user can now specify the precision with
  //        which contour labels are written.
  //    '2.12.0.6' Bug fix: The menus and buttons are now disabled while
  //        opening a file.
  //      Bug fix: Fixed bug importing model results.
  //        (bug not in released version.)
  //    '2.12.0.7' Bug fix: When a message box is displayed, the mouse will
  //        move to the default button if the user has specified that option
  //        in the Windows Control Panel.
  //    '2.12.0.8' Bug fix: When importing Shapefiles, failing to specify
  //        an attribute name or value no longer results in a range check error.
  //    '2.12.0.9' Failing to completely specify information streams in the
  //        SFR package now results in an error message instead of a bug report.
  //      Bug fix: Fixed bug in setting data for SFR when multiple objects
  //        are being edited at once.
  //    '2.12.0.10' Enhancement: added support for MT3DMS.
  //      Bug fix: Fixed bugs relating to display of data in MODFLOW-LGR models.
  //    '2.12.0.11' Bug fix: Fixed another bug relating to display of data in
  //        MODFLOW-LGR models.
  //    '2.12.0.12' Bug fix: Fixed another bug relating to display of data in
  //        MODFLOW-LGR models.
  //      Change: Added option to write SFR package input in the format used
  //        by GSFLOW.
  //    '2.12.0.13' Bug fix: fixed export of SFR in MODFLOW-NWT or for GSFLOW.
  //    '2.12.0.14' Enhancement: Added help for MT3DMS.
  //    '2.12.0.15' Fixed bugs in generation of MT3DMS files.
    //      Bug not in released version.
  //    '2.12.0.16' Fixed MT3DMS bugs.  Bugs not in released version.
  //    '2.12.0.17' Bug fix: Fixed bug with setting multiple flux observation
  //        values at once not working.
  //      Bug fix: Fixed bug in storing imported values when splitting objects.
  //    '2.12.0.18' Enhancement: Added new macro for exporting images; %TrS
  //        will be replaced by the transport step.
  //    '2.12.0.19' Bug fix: When reading an exporting a file from the command
  //        line, the "arrays" directory will be created as a subdirectory
  //        of the directory containing the file even if the full file path
  //        is not specified.
  //    '2.12.0.20' Bug fix: attempting to animate a series of data sets in
  //        the Export Image dialog box without first displaying the
  //        Display Data dialog box no longer causes access violations.
  //    '2.12.0.21' Bug fix:  Fixed bug with reading .ini file when multiple
  //        copies of ModelMuse are running.
  //    '2.12.0.22' Bug fix: When more than one instance of ModelMuse was
  //        running, sometimes one copy would not detect that another copy
  //        was running and would try to delete it's temporary files. That
  //        should no longer happen.
  //    '2.12.0.23' Bug fix: (second attempt) When more than one instance of
  //        ModelMuse was running, sometimes one copy would interfere with the
  //        other's temporary files.
  //      Bug fix: fixed export of OC file when the defaults are selected.
  //    '2.12.0.24' Bug fix: (third attempt) When more than one instance of
  //        ModelMuse was running, sometimes one copy would interfere with the
  //        other's temporary files.
  //    '2.12.0.25' Enhancement: The "Run model" button now has a drop down
  //        menu next to it which can be used to run MODPATH, ZONEBUDGET or
  //        MT3DMS.
  //      Enhancement: It is now possible to import parameter values from a
  //        MODFLOW PVAL file. This can be done either from the
  //        Manage Parameters dialog box or from the command line. To import
  //        a PVAL file from the command line, add -p "Filename" to the
  //        command line where Filename
  //        is the full path of the PVAL file. If the path contains any spaces,
  //        Filename should be enclosed in double quotes.
  //      Enhancement: It is now possible to import global variables from a
  //        text file. This can be done either from the
  //        Global Variables dialog box or from the command line. To import
  //        a global variables file from the command line, add -g "Filename"
  //        to the command line where Filename
  //        is the full path of the global variables file. If the path contains
  //        any spaces, Filename should be enclosed in double quotes.
  //        Each line of the global variables files must be either empty,
  //        start with the # character to identify the line as a comment or
  //        list the name and value of an existing global variable.
  //        On a line defining a new value for a global variable, the name
  //        of the global variable must be at the beginning of the line
  //        must be first followed by one or more spaces followed by the value.
  //        A period must be used as the decimal separator. For boolean
  //        global variables, the value must be either "True" or "False"
  //        (without the quotes). For text variables, the value may optionally
  //        be enclosed in quotes.  A global variables may also be saved from
  //        the Global Variables dialog box.
  //    '2.12.0.26' Bug fix: Contour lines are now drawn at the correct
  //        positions on the front and side views when the grid is rotated.
  //      Bug fix: (fourth attempt) When more than one instance of
  //        ModelMuse was running, sometimes one copy would interfere with the
  //        other's temporary files.
  //      Bug fix: Fixed a bug in which the Data Sets dialog box sometimes
  //        did not prevent the user from setting up a formula that caused
  //        a data set to depend on itself.
  //    '2.12.0.27' Bug fix: Fixed bug that could cause an assertion failure
  //        when coloring the grid with transient data such as the recharge
  //        rate.
  //      Bug fix (not in released version); global variable and .pval files
  //        weren't being imported correctly.
  //    '2.12.0.28' Fixed bug in exporting MT3DMS BTN data set A6 when there
  //        are more than 40 layers.
  //    '2.13.0.0' No additional changes.
  //    '2.13.0.1' Bug fix: Fixed a bug that would cause interpolation to fail
  //        if an object did not intersect the grid.
  //    '2.13.0.2' Enhancement: added support for running MODPATH with
  //        a child model in MODFLOW-LGR.
  //    '2.13.0.3' Enhancement: added support for visualizing MODPATH output
  //        with a child model in MODFLOW-LGR.
  //    '2.13.0.4' Bug fix: Fixed labeling of contours when contouring
  //        data sets whose data type is Text.
  //      Bug fix: fixed labels for TRPT and TRPV in MT3DMS.
  //      Bug fix: fixed export of SSM package when no source or sink
  //        concentrations have been defined.
  //    '2.13.0.5' Bug fix: Fixed bug that would cause an error if the user
  //        attempted to display data set values before generating the grid.
  //       Bug fix: Fixed a bug that would cause an error if the user attempted
  //         to give an aquifer a name that started with a number or a name
  //         that was too similar to the name of another aquifer.
  //    '2.13.0.6' Change: In the hydmod package, The name of each observation
  //         has a unique number added to the end of the label.
  //    '2.13.0.7' Bug fix: fixed bug that prevented the LPF options
  //         STORAGECOEFFICIENT and CONSTANTCV from being used simultaneously.
  //       Enhancement: Added support for the new STOPERROR option in the
  //         MODFLOW Basic package.
  //       Enhancement: Added support for the new NOPARCHECK option in the
  //         MODFLOW LPF package.
  //       Enhancement: Added support for the PCGN solver.
  //       Change: Changed SFR package to use new SFR format for MODFLOW-2000.
  //       Enhancement: Added IFACE to SFR input.
  //       Bug fix: fixed a bug that would cause range-check errors in certain
  //         MODFLOW-LGR models.
  //    '2.13.0.8' Bug fix: Fixed bug that would sometimes cause invalid SFR
  //         input files to be created when unsaturated flow was simulated
  //         beneath streams.
  //       Enhancement: When importing ASCII raster files into a MODFLOW-LGR
  //         model, it is now possible to specify the grid to use
  //         for the imported data.
  //       Bug fix: fixed bug that would sometimes cause the assigning data
  //         set values to fail in a MODFLOW-LGR model.
  //    '2.13.0.9' Change: Reduced memory usage.
  //       Bug fix: Checking elevations in MODFLOW-LGR child models no longer
  //         results in a range check error.
  //       Bug fix: Fixed problem with reading invalid files.
  //    '2.13.0.10' Bug fix: Previously, attempting to display the
  //         "Show or Hide Objects" dialog box would sometimes cause an error.
  //       Bug fix: In MODFLOW-LGR models, the legend displayed when
  //         coloring or contouring would not reflect more extreme values
  //         displayed in a child grid.
  //       Enhancement: You can now export grid data to a Shapefile for
  //         MODFLOW-LGR child grids.
  //    '2.13.0.11' Bug fix: Fixed export of IFMTCN, IFMTNP, IFMTRF,
  //         and IFMTDP in the MT3DMS basic transport package.
  //       Enhancement: You can now use the command line option -mte to export
  //         just the MT3DMS input files.
  //    '2.13.0.12' Bug fix: when importing a Shapefile to a multipart object,
  //         formulas for the Z-coordinate can now be imported properly.
  //    '2.14.0.0' No additional changes.
  //    '2.14.0.1' Enhancement: Added the ability to export
  //         .png and .jpg images.
  //       Bug fix: Importing model results for MODFLOW-LGR model and
  //         contouring them no longer causes a range-check error.
  //    '2.14.0.2' Bug fix: Fixed bug that caused access violations when
  //         importing results from a MODFLOW-LGR child model but not the
  //         MODFLOW-LGR parent model.
  //       Bug fix: Fixed bug in which the Kz data set was not created when
  //         a MODFLOW model was first created.
  //       Bug fix: Fixed bug that sometimes caused invalid elevation formulas
  //         in objects created by importing Shapefiles.
  //    '2.14.1.0' Change: Date for the current version of MODFLOW-2005
  //         changed to the date for MODFLOW-2005 version 1.9.01.
  //    '2.14.1.1' Bug fix: In order to prevent range check errors, it is no
  //         longer possible to display the Manage Flux Observations dialog box
  //         when MT3D has been selected unless at least one chemical species
  //         has been defined.
  //       Enhancement: The "Data Sets" dialog box can now remain open
  //         while the user works with other parts of ModelMuse. However, it
  //         can not remain open when the Object Properties dialog box is open
  //         so if the user attempts to open the Object Properties dialog box,
  //         The Edit Data Sets dialog box will automatically close.
  //       Bug fix: Specifying values of Stress period length,
  //         time step multiplier, and initial time step size that would cause
  //         the number of time steps to exceed the limits of a 32-bit signed
  //         integer causes an error message to the user rather than a bug
  //         report.
  //       Bug fix: In the object properties dialog box, clicking the button
  //         for the formula editor when editing the formula for the Factor
  //         in an MT3DMS Flux observation, no longer causes an
  //         Assertion failure.
  //       Bug fix: In the MODFLOW Packages and Programs dialog box, changing
  //         the number of parameters in the UPW package no longer causes an
  //         Assertion failure.
  //       Enhancement: Tab ordering has been improved.
  //    '2.14.1.2' Bug fix: Importing models containing the UZF package
  //         in which ET is not simulated, now works properly.
  //       Enhancement: X, Y, and Z axes are now drawn in the 3D view.
  //       Bug fix: Fixed bug that caused empty .bfh_head and .bfh_flux files
  //         to be created in MODFLOW-LGR models when one-way coupling is used.
  //       Bug fix: Merely selecting a data set in the Data Visualization
  //         dialog box no longer causes the data set values to be calculated.
  //         The user must now click "Apply" to cause the data set values to be
  //         calculated.
  //       Bug fix: Fixed bug importing the Subsidence package
  //         from existing models.
  //    '2.14.1.3' no real change.
  //    '2.14.1.4' Enhancement: Added support for importing the concentration
  //         in MT3DMS associated with a sink or source package.
  //       Change: In MODFLOW models using the BCF package, the
  //         "Vertical_Conductance" data set is renamed "Vertical_Leakance".
  //       Enhancement: Reduced the time required to open the Edit Data Sets
  //         dialog box in models with large numbers of data sets.
  //    '2.15.0.0' Bug fix: Fixed bug in importing Shapefiles that could
  //         allow an object to have one or more Z-formulas when setting
  //         the value of a layer definition data set.
  //       Enhancement: Added partial support for Unicode. Complete support
  //         is not possible because the groundwater models do not currently
  //         support Unicode.
  //       Change: The extension for MT3DMS configuration file changed from
  //         ._cnf to .cnf to facilitate viewing the output in Model Viewer.
  //    '2.15.0.1' Bug fix: Importing a Global Variables file was from the
  //         command line now works properly.
  //    '2.15.0.2' Bug fix: fixed a bug that would sometimes cause contouring
  //         to fail.
  //       Bug fix: Certain shapefiles that were previously rejected by
  //         ModelMuse as self-intersecting are now accepted.
  //       Enhancement: When importing images .pgw files are now accepted as
  //         a valid world file type.
  //       Bug fix: Fixed bug in which observation names were allowed
  //         to include the '/' character even though that character would
  //         not be processed by MODFLOW correctly.
  //     '2.15.0.3' Bug fix: Fixed a bug in which attempting to import gridded
  //         data using an invalid layer caused an error. Invalid layers can no
  //         longer be specified.
  //       Enhancement: MT3DMS simulations can now have fewer stress periods
  //         than the corresponding MODFLOW model.
  //     '2.15.0.4' Bug fix: Fixed bug that caused incorrect stress periods to
  //         be exported if the stress periods were changed and the SFR package
  //         was in used.
  //       Bug fix: Fixed a bug that could cause interpolation to fail.
  //     '2.16.0.0' Enhancement: Added 64-bit version for 64-bit operating
  //         systems.  TIFF and PCX files are not supported in the 64-bit
  //         version.
  //       Change: The red background for missing files in the Files to Archive"
  //         dialog box has been changed to a red font.
  //       Change: The light blue background for a parenthesis and it's
  //         match in the formula editor has been changed to a light blue font.
  //     '2.16.1.0' Bug fix: Fixed bug that caused importing model results to
  //         fail.
  //     '2.16.1.1' Bug fix: When exporting ModelMate files, ModelMuse now
  //         specifies the correct version of MODFLOW to use.
  //       Bug fix: The "Function Help" button on the Formula editor works again.
  //     '2.16.1.2' no real change. Converted some text to make it easier to
  //         translate.
  //     '2.16.1.3' Bug fix: Fixed export and import of head change
  //         observations to ModelMate.
  //     '2.16.1.4' no change.
  //     '2.16.1.5' Bug fix: Fixed bug in export of gages for the Lake package.
  //       Bug fix: Fixed bug in export of Shapefiles in which the last
  //         character of the last field was not written to the file.
  //       Bug fix: Fixed export of unit numbers for lake gages.
  //     '2.16.1.6' (Changed version of GLScene used to 5991.)
  //     '2.16.1.7' no real change.
  //     '2.16.1.8' Bug fix: Renaming parameters for the HUF package now
  //         causes any formulas based on data arrays for those parameters
  //         to be updated.
  //       Enhancement: Added support for MODPATH version 6.
  //       Change: Head observations that are at the end of a steady-state
  //         stress period now have their reference stress period set to that
  //         stress period rather than the beginning of the following
  //         stress period.
  //     '2.16.1.9' Change: Updated link for model archiving policy.
  //       Bug fix: Fixed display of file names in the Files to Archive
  //         dialog box.
  //       Bug fix: The name file for MODFLOW models is now included in
  //         archives.
  //       Bug fix: Fixed import of binary Surfer grid files.
  //     '2.16.1.10' Enhancement: Added support for exporting data set values
  //         for display in Model Viewer.
  //     '2.16.1.11' Bug fix: Fixed coloring the grid for certain PHAST data
  //         sets.
  //     '2.16.1.12' Bug fix: Fixed display of layer numbers on the status bar
  //         for the front and side views. {Bug not in released version.}
  //       Bug fix: fixed bug in export of MODPATH version 6 input
  //         (Bug not in released version.)
  //     '2.16.1.13' Bug fix: Fixed how the "Child Models" dialog box for
  //         MODFLOW-LGR models responds to the user entering an invalid number
  //         for NCPP.
  //       Bug fix: Fixed bug that could cause some MODFLOW budget files to
  //         be incorrectly identified as invalid.
  //     '2.16.1.14' Bug fix: Fixed detecting the version number of MODPATH
  //         used to create a pathline file. (Bug not in released version.)
  //       Bug fix: Fixed display of endpoints, pathlines and time series data
  //         from MODPATH on the front and side views when the grid is rotated.
  //     '2.16.1.15' Bug fix: Fixed bug that caused too much memory to be used
  //         when exporting the SSM package in MT3DMS.
  //     '2.16.1.16' Bug fix: Fixed bug that caused exporting the SSM file in
  //         MT3DMS to take more time than it should have.
  //     '2.16.1.17' Enhancement: The Selection Cube for the top view of the
  //         model draws the grid outline only for simulated layers.
  //       Bug fix: Fixed bug that could cause an error in the export of the
  //         MT3DMS TOB package if duplicate times were specified.
  //     '2.17.0.0' No further changes.
  //     '2.17.1.0'  Bug fix: Fixed bug in which objects were not drawn as
  //         selected after clicking the OK button in the Object Properties
  //         dialog box.
  //       Bug fix: The tree control in the Show Or Hide Objects dialog box
  //         no longer collapses all its nodes just because there has been a
  //         change in which object is selected.
  //       Bug fix: The display of the data for the RCH, EVT and ETS packages
  //         has been fixed.

const
  ModelVersion = '2.17.1.0';
  StrPvalExt = '.pval';
  StrJtf = '.jtf';
  StandardLock : TDataLock = [dcName, dcType, dcOrientation, dcEvaluatedAt];
  StrHUF = 'HUF2';
  kTop = '_Top';
  StrZonebudget = 'ZoneBudget';
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
  StrMt3dConcFile = '.ucn';
  StrMtName = '.mt_nam';
  strMtObs = '.mto';
  kSUTRAMeshTop = 'SUTRA_Mesh_Top';



  MaxString12 = 12;
  MaxString20 = 20;
  MaxString255 = 255;

resourcestring
  StrTop = kTop;

resourcestring
  StrSUTRAMeshTop = kSUTRAMeshTop;


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
  ModflowHydmodWriterUnit, IniFileUtilities, TempFiles, AbExcept,
  ModflowLakUnit, ModflowLgr_WriterUnit, ModflowNWT_WriterUnit,
  ModflowUPW_WriterUnit, frmCustomGoPhastUnit, ModflowSfrParamIcalcUnit,
  BigCanvasMethods, Mt3dmsBtnWriterUnit, Mt3dmsAdvWriterUnit,
  Mt3dmsDspWriterUnit, Mt3dmsSsmWriterUnit, Mt3dmsRctWriterUnit,
  Mt3dmsGcgWriterUnit, Mt3dmsTobWriterUnit, ModflowMt3dmsLinkWriterUnit,
  QuadMeshGenerator, MeshRenumbering, ModflowPCGN_WriterUnit, Character;

resourcestring
  StrMpathDefaultPath = 'C:\WRDAPP\Mpath.5_0\setup\Mpathr5_0.exe';
  StrMpathDefaultPathVersion6 = 'C:\WRDAPP\modpath.6_0\bin\mp6.exe';
  StrModflowDefaultPath = 'C:\WRDAPP\MF2005.1_9\Bin\mf2005.exe';
  StrPhastDefaultPath = 'C:\Program Files\USGS\phast-1.5.1\bin\phast.bat';
  StrPhastDefaultPath64 = 'C:\Program Files (x86)\USGS\phast-1.5.1\bin\phast.bat';
  StrZoneBudgetDefaultPath = 'C:\WRDAPP\Zonbud.3_01\Bin\zonbud.exe';
  StrModelMateDefaultPath = 'C:\WRDAPP\ModelMate_1_0_1\Bin\ModelMate.exe';
  strModflowLgrDefaultPath = 'C:\WRDAPP\mflgr.1_2\bin\mflgr.exe';
  strModflowNwtDefaultPath = 'C:\WRDAPP\MODFLOW-NWT_1.0.5\bin\MODFLOW-NWT.exe';

  StrProgramLocations = 'Program Locations';
  StrMODFLOW2005 = 'MODFLOW-2005';
  StrTextEditor = 'Text Editor';
  StrMODPATH = 'MODPATH';
  StrMODPATHVersion6 = 'MODPATH Version 6';
  StrModelMonitor = 'ModelMonitor';
  StrModelMonitorDefaultPath = 'ModelMonitor.exe';
  StrPHAST = 'PHAST';
  StrModelMate = 'ModelMate';
  StrAnyTimesAfterThe = 'Any times after the end of the last defined stress ' +
  'period will be ignored.';
  StrAnyTimesBeforeThe = 'Any times before the beginning of the first define' +
  'd stress period will be ignored.';
  strModflowLgr = 'MODFLOW-LGR';
  strModflowNWT = 'MODFLOW-NWT';
  StrAtLeastOneConvert = 'At least one layer must be convertible.';
  StrAtLeastOneUnconfConvert = 'At least one layer must be unconfined or ful' +
  'ly convertible.';
  StrSorryNoFilesToA = 'Sorry, no files to archive.';
  StrDOfTheFilesThat = '%d of the files that were to be archived can not be ' +
  'found.';
  StrTheRemainingFiles = 'The remaining files will be archived.';
  StrErrorSavingArchive = 'Error saving archive.  The archive may be too big' +
  '.';
  StrErrorSavingArchive2 = 'Error saving archive. The disk may not have suffi' +
  'cient space for the archive.';
  StrYourModelMateFile = 'Your ModelMate file contains the following unused ' +
  'parameters. Do you want to delete them?';
  StrYourModelMateFile2 = 'Your ModelMate file contains %d unused parameters.' +
  ' Do you want to delete them?';
  StrYourModelMateFile3 = 'Your ModelMate file contains the following unused ' +
  '%s. Do you want to delete them?';
  StrYourModelMateFile4 = 'Your ModelMate file contains %0:d unused %1:s. Do' +
  ' you want to delete them?';
  StrOneOrMore0sFro = 'One or more %0:s from the ModelMate file are not pres' +
  'ent in the ModelMuse file and have been ignored. If an %1:s has been rena' +
  'med, change the %2:s name so that it matches in ModelMuse and ModelMate a' +
  'nd then try importing again.';
  StrObservation = 'observation';
  StrObservations = 'observations';
  StrPrediction = 'prediction';
  StrPredictions = 'predictions';
  StrOneOrMoreParamete = 'One or more parameters from the ModelMate file are' +
  ' not present in the ModelMuse file and have been ignored. If a parameter ' +
  'has been renamed, change the parameter name so that it matches in ModelMu' +
  'se and ModelMate and then try importing again.';
  StrYourModelHasSTi = 'Your model has %d time steps. Do you want to continu' +
  'e?';
  StrMODPATHRequiresTha = 'MODPATH requires that the heads and flows be save' +
  'd for every time step of the model. That isn''t the case for this model. ' +
  'Do you want to export the MODPATH input anyway?';
//  StrMT3DMS5 = 'MT3DMS-5';
  strMt3dmsDefaultPath = 'C:\mt3dms5\bin\mt3dms5b.exe';
  StrTheBeginningOfThe = 'The beginning of the first stress period is %0:g. ' +
  'The first defined time is  %1:g. The following objects have defined times' +
  ' before the beginning of the first stress period.';
  StrTheEndOfTheLast = 'The end of the last stress period is %0:g. The last ' +
  'defined time is %1:g. The following objects have defined times before the' +
  ' beginning of the first stress period.';
  StrNoObjects = 'The mesh was not created because no objects define the ele' +
  'ment size on the top view of the model.';
  StrNoPolygons = 'The mesh was not created because no polygons define the e' +
  'lement size on the top view of the model.';
  StrInvalidTimesForMT = 'Invalid times for MT3DMS';
  StrTheStressPeriodsD = 'The stress periods defined for MT3DMS are not with' +
  'in the stress periods defined for MODFLOW.';
  StrAndNegatedAtCons = ' and negated at constant head cell';
  StrAndMadePositiveA = ' and made positive at constant head cell';
  StrValueOfZeroConver = 'Value of zero converted to 1 at active cell.';
  StrNonzeroValueOfZe = 'Non-zero value converted to 0 at inactive cell.';
  StrAddedTimes = 'The following objects define times at which stresses ' +
    'change that were not defined as the beginning or ending or stress ' +
    'periods. The stress periods will be adjusted to uses these times.';
  StrTimesDoNotMatch = 'Times do not match';
  StrTheFinalTimeSpeci = 'The final time specified in the MT3DMS simulation ' +
  '(%0:g) is earlier than the final time specified in the MODFLOW simulation' +
  ' (%1:g). Do you want to stop the MT3DMS simulation when it reaches the ti' +
  'me you specified for the end of the MT3DMS simulation? ("Cancel" aborts ' +
  'the export of the MT3DMS files.)';

const
  StatFlagStrings : array[Low(TStatFlag)..High(TStatFlag)] of string
    = ('VAR', 'SD', 'CV', 'WT', 'SQRWT');
const
  UcodeDelimiter = '@';

function DefaultPhastPath: string;
begin
  if IsWOW64 then
  begin
    result := StrPhastDefaultPath64;
  end
  else
  begin
    result := StrPhastDefaultPath
  end;
end;

function GenerateNewRoot(const Root: string): string;
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
begin
  result := Trim(Root);
  Assert(result <> '');
  IsValidIdent(result, False);
  if not Alpha(result[1]) then
  begin
    result[1] := '_';
  end;

  for Index := 2 to Length(result) do
  begin
    if not AlphaNumeric(result[Index]) then
    begin
      result[Index] := '_';
    end;
  end;
end;

function GenerateNewName(Root: string = '';
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
    Root := StrNewDataSet;
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

function TPhastModel.AddDataSet(const DataSet: TDataArray): Integer;
var
  ChildIndex: Integer;
  ChildItem: TChildModelItem;
  ChildDataArray: TDataArray;
begin
  result := inherited AddDataSet(DataSet);
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildItem := ChildModels[ChildIndex];
    ChildDataArray := TDataArrayType(DataSet.ClassType).Create(ChildItem.ChildModel);
    ChildDataArray.AssignProperties(DataSet);
    ChildItem.ChildModel.DataArrayManager.AddDataSet(ChildDataArray);
    ChildDataArray.Formula := DataSet.Formula;
    DataSet.TalksTo(ChildDataArray);
  end;
end;

function TPhastModel.AddScreenObject(const AScreenObject: TScreenObject):
  integer;
begin
  result := FScreenObjectList.Add(AScreenObject);
  Invalidate;
end;

procedure TPhastModel.AdjustDataArray(ADataArray: TDataArray);
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ColIndex: Integer;
  LayerIndex: Integer;
  AnnotationList: TStringList;
  NewAnnotation: string;
  AnnotationPosition: integer;
  RowIndex: Integer;
begin
  if LgrUsed then
  begin
    if ADataArray.DataType = rdtBoolean then
    begin
      Exit;
    end;
    Assert(ADataArray.DataType = rdtDouble);
    Assert(ADataArray.ColumnCount >= 3);
    Assert(ADataArray.RowCount >= 3);
    AnnotationList := TStringList.Create;
    try
      AnnotationList.Sorted := True;
      for ChildIndex := 0 to ChildModels.Count - 1 do
      begin
        ChildModel := ChildModels[ChildIndex].ChildModel;
        if (ChildModel.FirstRow >= 0) and (ChildModel.LastRow >= 0)
          and (ChildModel.FirstCol >= 0) and (ChildModel.LastCol >= 0) then
        begin
          for LayerIndex := 0 to ADataArray.LayerCount - 1 do
          begin
            if ADataArray.IsValue[LayerIndex,
              ChildModel.FirstRow, ChildModel.FirstCol] then
            begin
              NewAnnotation := ' Adjusted for LGR: ' + ADataArray.Annotation[
                LayerIndex, ChildModel.FirstRow, ChildModel.FirstCol];
              AnnotationPosition := AnnotationList.IndexOf(NewAnnotation);
              if AnnotationPosition >= 0 then
              begin
                NewAnnotation := AnnotationList[AnnotationPosition];
              end
              else
              begin
                AnnotationList.Add(NewAnnotation);;
              end;
              ADataArray.Annotation[LayerIndex,
                ChildModel.FirstRow, ChildModel.FirstCol] := NewAnnotation;
              ADataArray.RealData[LayerIndex,
                ChildModel.FirstRow, ChildModel.FirstCol]
                := ADataArray.RealData[LayerIndex,
                ChildModel.FirstRow, ChildModel.FirstCol] * 0.75;
            end;

            if ADataArray.IsValue[LayerIndex,
              ChildModel.FirstRow, ChildModel.LastCol] then
            begin
              NewAnnotation := ' Adjusted for LGR: ' + ADataArray.Annotation[
                LayerIndex, ChildModel.FirstRow, ChildModel.LastCol];
              AnnotationPosition := AnnotationList.IndexOf(NewAnnotation);
              if AnnotationPosition >= 0 then
              begin
                NewAnnotation := AnnotationList[AnnotationPosition];
              end
              else
              begin
                AnnotationList.Add(NewAnnotation);;
              end;
              ADataArray.Annotation[LayerIndex,
                ChildModel.FirstRow, ChildModel.LastCol] := NewAnnotation;
              ADataArray.RealData[LayerIndex,
                ChildModel.FirstRow, ChildModel.LastCol]
                := ADataArray.RealData[LayerIndex,
                ChildModel.FirstRow, ChildModel.LastCol] * 0.75;
            end;

            if ADataArray.IsValue[LayerIndex,
              ChildModel.LastRow, ChildModel.FirstCol] then
            begin
              NewAnnotation := ' Adjusted for LGR: ' + ADataArray.Annotation[
                LayerIndex, ChildModel.LastRow, ChildModel.FirstCol];
              AnnotationPosition := AnnotationList.IndexOf(NewAnnotation);
              if AnnotationPosition >= 0 then
              begin
                NewAnnotation := AnnotationList[AnnotationPosition];
              end
              else
              begin
                AnnotationList.Add(NewAnnotation);;
              end;
              ADataArray.Annotation[LayerIndex,
                ChildModel.LastRow, ChildModel.FirstCol] := NewAnnotation;
              ADataArray.RealData[LayerIndex,
                ChildModel.LastRow, ChildModel.FirstCol]
                := ADataArray.RealData[LayerIndex,
                ChildModel.LastRow, ChildModel.FirstCol] * 0.75;
            end;

            if ADataArray.IsValue[LayerIndex,
              ChildModel.LastRow, ChildModel.LastCol] then
            begin
              NewAnnotation := ' Adjusted for LGR: ' + ADataArray.Annotation[
                LayerIndex, ChildModel.LastRow, ChildModel.LastCol];
              AnnotationPosition := AnnotationList.IndexOf(NewAnnotation);
              if AnnotationPosition >= 0 then
              begin
                NewAnnotation := AnnotationList[AnnotationPosition];
              end
              else
              begin
                AnnotationList.Add(NewAnnotation);;
              end;
              ADataArray.Annotation[LayerIndex,
                ChildModel.LastRow, ChildModel.LastCol] := NewAnnotation;
              ADataArray.RealData[LayerIndex,
                ChildModel.LastRow, ChildModel.LastCol]
                := ADataArray.RealData[LayerIndex,
                ChildModel.LastRow, ChildModel.LastCol] * 0.75;
            end;

            for ColIndex := ChildModel.FirstCol + 1 to ChildModel.LastCol - 1 do
            begin
              if ADataArray.IsValue[LayerIndex, ChildModel.FirstRow, ColIndex] then
              begin
                NewAnnotation := ' Adjusted for LGR: ' + ADataArray.Annotation[
                  LayerIndex, ChildModel.FirstRow, ColIndex];
                AnnotationPosition := AnnotationList.IndexOf(NewAnnotation);
                if AnnotationPosition >= 0 then
                begin
                  NewAnnotation := AnnotationList[AnnotationPosition];
                end
                else
                begin
                  AnnotationList.Add(NewAnnotation);;
                end;
                ADataArray.Annotation[LayerIndex,
                  ChildModel.FirstRow, ColIndex] := NewAnnotation;
                ADataArray.RealData[LayerIndex, ChildModel.FirstRow, ColIndex]
                  := ADataArray.RealData[LayerIndex, ChildModel.FirstRow, ColIndex] /2;
              end;
              if ADataArray.IsValue[LayerIndex, ChildModel.LastRow, ColIndex] then
              begin
                NewAnnotation := ' Adjusted for LGR: ' + ADataArray.Annotation[
                  LayerIndex, ChildModel.LastRow, ColIndex];
                AnnotationPosition := AnnotationList.IndexOf(NewAnnotation);
                if AnnotationPosition >= 0 then
                begin
                  NewAnnotation := AnnotationList[AnnotationPosition];
                end
                else
                begin
                  AnnotationList.Add(NewAnnotation);;
                end;
                ADataArray.Annotation[LayerIndex,
                  ChildModel.LastRow, ColIndex] := NewAnnotation;
                ADataArray.RealData[LayerIndex, ChildModel.LastRow, ColIndex]
                  := ADataArray.RealData[LayerIndex, ChildModel.LastRow, ColIndex] /2;
              end;
            end;
            for RowIndex := ChildModel.FirstRow +1 to ChildModel.LastRow - 1 do
            begin
              if ADataArray.IsValue[LayerIndex, RowIndex, ChildModel.FirstCol] then
              begin
                NewAnnotation := ' Adjusted for LGR: ' + ADataArray.Annotation[
                  LayerIndex, RowIndex, ChildModel.FirstCol];
                AnnotationPosition := AnnotationList.IndexOf(NewAnnotation);
                if AnnotationPosition >= 0 then
                begin
                  NewAnnotation := AnnotationList[AnnotationPosition];
                end
                else
                begin
                  AnnotationList.Add(NewAnnotation);;
                end;
                ADataArray.Annotation[LayerIndex, RowIndex,
                  ChildModel.FirstCol] := NewAnnotation;
                ADataArray.RealData[LayerIndex, RowIndex, ChildModel.FirstCol]
                  := ADataArray.RealData[LayerIndex, RowIndex, ChildModel.FirstCol] /2;
              end;
              if ADataArray.IsValue[LayerIndex, RowIndex, ChildModel.LastCol] then
              begin
                NewAnnotation := ' Adjusted for LGR: ' + ADataArray.Annotation[
                  LayerIndex, RowIndex, ChildModel.LastCol];
                AnnotationPosition := AnnotationList.IndexOf(NewAnnotation);
                if AnnotationPosition >= 0 then
                begin
                  NewAnnotation := AnnotationList[AnnotationPosition];
                end
                else
                begin
                  AnnotationList.Add(NewAnnotation);;
                end;
                ADataArray.Annotation[LayerIndex, RowIndex,
                  ChildModel.LastCol] := NewAnnotation;
                ADataArray.RealData[LayerIndex, RowIndex, ChildModel.LastCol]
                  := ADataArray.RealData[LayerIndex, RowIndex, ChildModel.LastCol] /2;
              end
            end;
          end;
        end;
      end;
    finally
      AnnotationList.Free;
    end;
  end;

end;


function TPhastModel.AllDispersionMultiDiffusion: boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := not ModflowPackages.Mt3dmsDispersion.IsSelected
    or ModflowPackages.Mt3dmsDispersion.MultiDifussion;
  if Result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      result := not ChildModel.ModflowPackages.Mt3dmsDispersion.IsSelected
        or ChildModel.ModflowPackages.Mt3dmsDispersion.MultiDifussion;
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TPhastModel.AllowChildGridUpdates;
var
  ChildModel: TChildModel;
  Index: Integer;
begin
  Dec(FChildGridUpdateCount);
  if FChildGridUpdateCount = 0 then
  begin
    for Index := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[Index].ChildModel;
      ChildModel.CanUpdateGrid := True;
    end;
  end;
end;

function TPhastModel.AnyDispersionMultiDiffusion: boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.Mt3dmsDispersion.IsSelected
    and ModflowPackages.Mt3dmsDispersion.MultiDifussion;
  if not Result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      result := ChildModel.ModflowPackages.Mt3dmsDispersion.IsSelected
        and ChildModel.ModflowPackages.Mt3dmsDispersion.MultiDifussion;
      if result then
      begin
        Exit;
      end;
    end;
  end;
end;

function TPhastModel.AnyMt3dReactions: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.Mt3dmsChemReact.IsSelected
    and (ModflowPackages.Mt3dmsChemReact.KineticChoice <> kcNone);
  if not Result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      result := ChildModel.ModflowPackages.Mt3dmsChemReact.IsSelected
        and (ChildModel.ModflowPackages.Mt3dmsChemReact.KineticChoice <> kcNone);
      if result then
      begin
        Exit;
      end;
    end;
  end;
end;

function TPhastModel.AnyMt3dSorbImmobConc: boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.Mt3dmsChemReact.IsSelected
    and (ModflowPackages.Mt3dmsChemReact.OtherInitialConcChoice = oicUse);
  if not Result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      result := ChildModel.ModflowPackages.Mt3dmsChemReact.IsSelected
        and (ChildModel.ModflowPackages.Mt3dmsChemReact.OtherInitialConcChoice = oicUse);
      if result then
      begin
        Exit;
      end;
    end;
  end;
end;

function TPhastModel.AnyMt3dSorbParameter: boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.Mt3dmsChemReact.IsSelected
    and (ModflowPackages.Mt3dmsChemReact.SorptionChoice <> scNone);
  if not Result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      result := ChildModel.ModflowPackages.Mt3dmsChemReact.IsSelected
        and (ChildModel.ModflowPackages.Mt3dmsChemReact.SorptionChoice <> scNone);
      if result then
      begin
        Exit;
      end;
    end;
  end;
end;

function TPhastModel.AquiferPropertiesUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited AquiferPropertiesUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.AquiferPropertiesUsed(Sender);
    end;
  end;
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
    PathLines := SourceModel.PathLines;
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
    ModflowGrid := SourceModel.ModflowGrid;
    ModflowStressPeriods := SourceModel.ModflowStressPeriods;
    SoluteTransport := SourceModel.SoluteTransport;
    UseWaterTable := SourceModel.UseWaterTable;
    FreeSurface := SourceModel.FreeSurface;
    ChemistryOptions := SourceModel.ChemistryOptions;
    HufParameters := SourceModel.HufParameters;
    ObservationPurpose := SourceModel.ObservationPurpose;
    ModflowTransientParameters := SourceModel.ModflowTransientParameters;
    ModflowOutputControl := SourceModel.ModflowOutputControl;
    Mt3dmsOutputControl := SourceModel.Mt3dmsOutputControl;
    Mt3dmsTimes := SourceModel.Mt3dmsTimes;
    MobileComponents := SourceModel.MobileComponents;
    ImmobileComponents := SourceModel.ImmobileComponents;
    DataSetList := SourceModel.DataSetList;
    SfrStreamLinkPlot := SourceModel.SfrStreamLinkPlot;
  end;
  inherited;

end;

function TPhastModel.BcfIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.BcfPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.BcfPackage.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.BcfUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited BcfUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.BcfUsed(Sender);
    end;
  end;
end;

procedure TPhastModel.BeginDataSetUpdate;
begin
  Inc(FDataSetUpdateCount);
end;

procedure TPhastModel.BeginGridChange;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  inherited;
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModel := ChildModels[ChildIndex].ChildModel;
    if ChildModel <> nil then
    begin
      ChildModel.BeginGridChange;
    end;
  end;
end;

procedure TPhastModel.BeginScreenObjectUpdate;
begin
  Inc(FScreenObjectUpdateCount);
end;

constructor TPhastModel.Create(AnOwner: TComponent);
var
  ChangeNotifier: IChangeNotifier;
//  LayerGroup: TSutraLayerGroup;
begin
  inherited;

  FSaveBfhBoundaryConditions := True;
  FSaveDataSetValues := sdsvAlways;
  FSelectedModel := self;
//  FCurrentModel := self;
  UpdateCurrentModel(self);
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

  FSelectedScreenObjectCount := 0;
  FClearing := False;

  FProgramLocations := TProgramLocations.Create;

  FCachedScreenObjectIndex := -1;

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
  FPhastGrid := TPhastGrid.Create(self);
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
  FMt3dmsOutputControl := TMt3dmsOutputControl.Create(Self);
  FMt3dmsTimes:= TMt3dmsTimeCollection.Create(Self);

  FModflowSteadyParameters:= TModflowSteadyParameters.Create(Self);
  FModflowTransientParameters := TModflowTransientListParameters.Create(self);

  FDataArrayManager.CreateInitialDataSets;
  FDataArrayManager.CreateInitialBoundaryDataSets;

  CreatePhastTimeLists;
  CreateInitialDataSetsForPhastTimeLists;
  CreatePhastTimeListGroups;

  FDataArrayManager.CreateInitialDataSets;

  FContourFont := TFont.Create;
  ChangeNotifier := TFontChangeNotifyier.Create(self, FContourFont);
  FContourFont.FontAdapter := ChangeNotifier;
  FShowContourLabels := True;

  FSfrStreamLinkPlot := TSfrStreamLinkPlot.Create(self);

  FImmobileComponents := TChemSpeciesCollection.Create(Self);
  FMobileComponents := TMobileChemSpeciesCollection.Create(Self);

  {$IFDEF Sutra}
  FSutraLayerStructure:= TSutraLayerStructure.Create(Self);
//  LayerGroup := FSutraLayerStructure.Add as TSutraLayerGroup;
//  LayerGroup.AquiferName := 'SUTRA_Mesh_Top';
  {$ENDIF}

end;

procedure TPhastModel.CreateArchive(const FileName: string; const ArchiveCommand: string = '');
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
      MessageDlg(StrSorryNoFilesToA, mtInformation, [mbOK], 0);
    end
    else
    begin

      if DeletedFiles.Count > 0 then
      begin
        if DeletedFiles.Count > 10 then
        begin
          ErrorMessage := Format(StrDOfTheFilesThat, [DeletedFiles.Count])
            + sLineBreak + sLineBreak + StrTheRemainingFiles;
        end
        else
        begin
          ErrorMessage := 'The following files that '
            + 'were to be archived can not be found: ' + sLineBreak + sLineBreak
            + DeletedFiles.Text
            + sLineBreak + 'The remaining files will be archived.';
          if ArchiveCommand <> '' then
          begin
            ErrorMessage := ErrorMessage + ' Select "' + ArchiveCommand
              + '" to see or change the files for the archive.';
          end;
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
            MessageDlg(StrErrorSavingArchive, mtError, [mbOK], 0);
          end;
          on E: EAbZipSpanOverwrite do
          begin
            Beep;
            MessageDlg(StrErrorSavingArchive2, mtError, [mbOK], 0);
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
  LayerGroup: TCustomLayerGroup;
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
  if (SutraLayerStructure <> nil) and (SutraLayerStructure.Count > 0)
    and (SutraMesh <> nil) and (SutraMesh.MeshType = mt3D) then
  begin
    for Index := 0 to SutraLayerStructure.Count - 1 do
    begin
      LayerGroup := SutraLayerStructure.LayerGroups[Index];
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

procedure TPhastModel.UpdateSideTimeDataSet(const TimeList: TCustomTimeList;
  const Time: double);
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildList: TCustomTimeList;
begin
  inherited;
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModel := ChildModels[ChildIndex].ChildModel;
    ChildList := ChildModel.GetTimeListByName(TimeList.Name);
    ChildModel.UpdateSideTimeDataSet(ChildList, Time);
  end;
end;

destructor TPhastModel.Destroy;
var
  Index: Integer;
//  DataArray: TDataArray;
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

    FImmobileComponents.Free;
    FMobileComponents.Free;

    FSfrStreamLinkPlot.Free;
    FChildModels.Free;
    FHufParameters.Free;

    FModelMateProject.Free;

    FreeAndNil(FSortedObjectList);

    FModflowTransientParameters.Free;
    FModflowSteadyParameters.Free;
    FMt3dmsTimes.Free;
    FMt3dmsOutputControl.Free;
    FModflowOutputControl.Free;
    FModflowFullStressPeriods.Free;
    FModflowStressPeriods.Free;


    frmFileProgress.pbProgress.Position := 0;
    frmFileProgress.pbProgress.Max := FDataArrayManager.DataSetCount + GlobalVariables.Count
      + ScreenObjectCount;
    frmFileProgress.Show;




    FTopBoundaryType.StopTalkingToAnyone;
    FFrontBoundaryType.StopTalkingToAnyone;
    FSideBoundaryType.StopTalkingToAnyone;
    FTop2DBoundaryType.StopTalkingToAnyone;


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
    FContourFont.Free;

    FSutraLayerStructure.Free;
  finally
    FreeAndNil(frmFileProgress);
  end;

  inherited;
end;

procedure TCustomModel.FinalizeActive(Sender: TObject);
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

procedure TCustomModel.FinalizeLakeId(Sender: TObject);
begin
  FinalizeActive(Sender);
  FinalizeWetDry(Sender);
end;

function TPhastModel.GetProgramLocations: TProgramLocations;
begin
  result := FProgramLocations;
end;

procedure TCustomModel.FinalizeWetDry(Sender: TObject);
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

function TPhastModel.GetSelectedModel: TCustomModel;
begin
  result := FSelectedModel
end;

function TPhastModel.GetShowContourLabels: boolean;
begin
  result := FShowContourLabels
end;

function TCustomModel.HufDataArrayUsed(Sender: TObject): boolean;
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

function TPhastModel.HufIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.HufPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.GmgPackage.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.HufReferenceSurfaceNeeded(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited HufReferenceSurfaceNeeded(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.HufReferenceSurfaceNeeded(Sender);
    end;
  end;
end;

function TPhastModel.HydmodIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.HydmodPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.HydmodPackage.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.ModelLayerDataArrayUsed(Sender: TObject): boolean;
var
  Index: Integer;
  Group: TCustomLayerGroup;
  DataArray: TDataArray;
begin
  Result := False;
  case ModelSelection of
    msUndefined, msPhast: Exit;
    msModflow, msModflowLGR, msModflowNWT:
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
    {$IFDEF SUTRA}
    msSutra:
      begin
        result := (SutraMesh <> nil) and (SutraMesh.MeshType = mt3D);
        if result then
        begin
          DataArray := Sender as TDataArray;
          for Index := 0 to SutraLayerStructure.Count - 1 do
          begin
            Group := SutraLayerStructure[Index];
            result := (Group.DataArrayName = DataArray.Name);
            if result then
            begin
              Exit;
            end;
          end;
        end;
      end;
    {$ENDIF}
    else Assert(False);
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
  SectionIndex: Integer;
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
          OutFlowLocation, TestLocation, 1, SectionIndex);
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

function TPhastModel.LpfIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.LpfPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.LpfPackage.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.UpwIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.UpwPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.UpwPackage.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.SsmIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.Mt3dmsSourceSink.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.Mt3dmsSourceSink.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.TobIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.Mt3dmsTransObs.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.Mt3dmsTransObs.IsSelected;
      end;
    end;
  end;
end;

procedure TPhastModel.InternalClear;
var
  Index: Integer;
//  DataSet: TDataArray;
  ScreenObject: TScreenObject;
  Variable: TGlobalVariable;
  ChildIndex: Integer;
  DataArray: TDataArray;
begin
  FSaveBfhBoundaryConditions := True;

  AllObserversStopTalking;
  FLayerStructure.StopTalkingToAnyone;
//  FSutraLayerStructure.StopTalkingToAnyone;
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
    DataArray := FDataArrayManager.DataSets[Index];
    DataArray.StopTalkingToAnyone;
//      frmFileProgress.pbProgress.StepIt;
//      Application.ProcessMessages;
  end;
  Bitmaps.Clear;
  ClearViewedItems;
  FColorLegend.ValueSource := nil;
  FContourLegend.ValueSource := nil;

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

  // Clear screen objects before clearing child models because
  // the screen objects access the child models while being destroyed.
  FScreenObjectList.Clear;

  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModels[ChildIndex].ChildModel.Clear;
  end;
  ChildModels.Clear;
  ModelSelection := msUndefined;


  FDataArrayManager.ClearAllDataSets;
  ClearParsers;

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
  Mt3dmsOutputControl.Initialize;

  HydrogeologicUnits.Clear;
  FHufParameters.Clear;

  HeadFluxObservations.Clear;
  DrainObservations.Clear;
  GhbObservations.Clear;
  RiverObservations.Clear;
  Mt3dmsHeadMassFluxObservations.Clear;
  Mt3dmsWellMassFluxObservations.Clear;
  Mt3dmsDrnMassFluxObservations.Clear;
  Mt3dmsRivMassFluxObservations.Clear;
  Mt3dmsGhbMassFluxObservations.Clear;
  Mt3dmsRchMassFluxObservations.Clear;
  Mt3dmsEvtMassFluxObservations.Clear;
  Mt3dmsMassLoadingMassFluxObservations.Clear;
  Mt3dmsResMassFluxObservations.Clear;
  Mt3dmsLakMassFluxObservations.Clear;
  Mt3dmsDrtMassFluxObservations.Clear;
  Mt3dmsEtsMassFluxObservations.Clear;

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

  if GlobalFont <> nil then
  begin
    ContourFont := GlobalFont;
  end;
  FImmobileComponents.Clear;
  FMobileComponents.Clear;

  Mt3dmsTimes.Clear;
  ModflowStressPeriods.Clear;

  Invalidate;
  inherited;
end;

function TPhastModel.ExchangeUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited ExchangeUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.ExchangeUsed(Sender);
    end;
  end;
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

function TPhastModel.ChdIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.ChdBoundary.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.ChdBoundary.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.ChemistryUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited ChemistryUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.ChemistryUsed(Sender);
    end;
  end;
end;

function TPhastModel.ChobIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.ChobPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.ChobPackage.IsSelected;
      end;
    end;
  end;
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

procedure TPhastModel.RenameOldVerticalLeakance;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  Index: integer;
  ADataSet, ChildDataArray: TDataArray;
begin
  if FileVersionEqualOrEarlier('2.14.1.3') then
  begin
    inherited;
    if ChildModels.Count > 0 then
    begin
      for ChildIndex := 0 to ChildModels.Count - 1 do
      begin
        ChildModel := ChildModels[ChildIndex].ChildModel;
        ChildModel.RenameOldVerticalLeakance
      end;
      for Index := 0 to FDataArrayManager.DataSetCount - 1 do
      begin
        ADataSet := FDataArrayManager[Index];
        for ChildIndex := 0 to ChildModels.Count - 1 do
        begin
          ChildModel := ChildModels[ChildIndex].ChildModel;
          ChildDataArray := ChildModel.DataArrayManager.GetDataSetByName(ADataSet.Name);
          Assert(ChildDataArray <> nil);
          ChildDataArray.AssignProperties(ADataSet);
          ChildDataArray.Formula := ADataSet.Formula;
          ChildDataArray.Limits := ADataSet.Limits;
          ChildDataArray.ContourLimits := ADataSet.ContourLimits;
        end;
      end;
    end;
  end;
end;

procedure TCustomModel.RenameDataArray(DataArray: TDataArray;
  const NewName, NewDisplayName: string);
var
  OldNames: TStringList;
  Compiler: TRbwParser;
  NewNames: TStringList;
  OldName: TComponentName;
  Position: Integer;
begin
  OldName := DataArray.Name;
  // rename data array.
  TopGridObserver.StopsTalkingTo(DataArray);
  DataArray.StopsTalkingTo(ThreeDGridObserver);
  OldNames := TStringList.Create;
  NewNames := TStringList.Create;
  try
    OldNames.Add(OldName);
    NewNames.Add(NewName);
    UpdateFormulas(OldNames, NewNames);
  finally
    NewNames.Free;
    OldNames.Free;
  end;
  DataArray.Name := NewName;
  DataArray.DisplayName := NewDisplayName;
  Compiler := GetCompiler(DataArray.Orientation, DataArray.EvaluatedAt);
  Position := Compiler.IndexOfVariable(OldName);
  if Position >= 0 then
  begin
    Compiler.RenameVariable(Position, NewName, NewDisplayName);
  end;
  Compiler := GetCompiler(dso3D, DataArray.EvaluatedAt);
  Position := Compiler.IndexOfVariable(OldName);
  if Position >= 0 then
  begin
    Compiler.RenameVariable(Position, NewName, NewDisplayName);
  end;
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

procedure TCustomModel.SetPhastGrid(const Value: TPhastGrid);
begin
  FPhastGrid.Assign(Value);
  Invalidate;
end;

function TPhastModel.GetHufParameters: THufModflowParameters;
begin
  result := FHufParameters;
end;

function TPhastModel.GetImmobileComponents: TChemSpeciesCollection;
begin
  Result := FImmobileComponents;
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

procedure TPhastModel.SetImmobileComponents(
  const Value: TChemSpeciesCollection);
begin
  FImmobileComponents.Assign(Value);
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

procedure TPhastModel.SetTopTimeList(const Value: TCustomTimeList);
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildTimeList: TCustomTimeList;
begin
  inherited;
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModel := ChildModels[ChildIndex].ChildModel;
    if Value = nil then
    begin
      ChildTimeList := nil;
    end
    else
    begin
      ChildTimeList := ChildModel.GetTimeListByName(Value.Name)
    end;
    ChildModel.TopTimeList := ChildTimeList;
  end;
end;

procedure TPhastModel.SetWidth(const Value: integer);
begin
  if GuiSettings <> nil then
  begin
    GuiSettings.Width := Value
  end;
end;

function TPhastModel.PhastUsed(Sender: TObject): boolean;
begin
  result := ModelSelection = msPhast;
end;

function TPhastModel.PorosityUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited PorosityUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.PorosityUsed(Sender);
    end;
  end;
end;

function TPhastModel.StoreChildModels: Boolean;
begin
  result := FChildModels.Count > 0;
end;

function TPhastModel.GetMobileComponents: TMobileChemSpeciesCollection;
begin
  result := FMobileComponents;
end;

function TPhastModel.GetModelSelection: TModelSelection;
begin
  result := FModelSelection;
end;

function TPhastModel.GetModflowFullStressPeriods: TModflowStressPeriods;
begin
  result := FModflowFullStressPeriods;
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

function TPhastModel.GetMt3dmsOutputControl: TMt3dmsOutputControl;
begin
  result := FMt3dmsOutputControl;
end;

function TPhastModel.GetMt3dmsTimes: TMt3dmsTimeCollection;
begin
  result := FMt3dmsTimes;
end;

function TPhastModel.GetNeedToRecalculateFrontCellColors: boolean;
begin
  result := Grid.NeedToRecalculateFrontCellColors;
end;

function TPhastModel.GetNeedToRecalculateSideCellColors: boolean;
begin
  result := Grid.NeedToRecalculateSideCellColors;
end;

function TPhastModel.GetNeedToRecalculateTopCellColors: boolean;
begin
  result := Grid.NeedToRecalculateTopCellColors;
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
      frmFormulaErrors.AddFormulaError(ScreenObject.Name, StrModflowSfrReachLength,
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

procedure TPhastModel.SetMobileComponents(
  const Value: TMobileChemSpeciesCollection);
begin
  FMobileComponents.Assign(Value);
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
    {$IFDEF SUTRA}
var
  LayerGroup: TSutraLayerGroup;
    {$ENDIF}
begin
  inherited;
  case Value of
    msUndefined: ;
    msPhast: ;
    msModflow: ;
    msModflowLGR: ;
    msModflowNWT: ;
    {$IFDEF SUTRA}
    msSutra:
      begin
        if FSutraLayerStructure.Count = 0 then
        begin
          LayerGroup := FSutraLayerStructure.Add as TSutraLayerGroup;
          LayerGroup.AquiferName := kSUTRAMeshTop;
        end
      end
    {$ENDIF}
    else Assert(False);
  end;

end;

procedure TCustomModel.SetModelSelection(const Value: TModelSelection);
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
      msModflow, msModflowLGR, msModflowNWT:
        begin
          ModflowGrid.TopGridObserver := nil;
          ModflowGrid.ThreeDGridObserver := nil;
        end;
      {$IFDEF SUTRA}
      msSutra:
        begin

        end
      {$ENDIF}
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
      msModflow, msModflowLGR, msModflowNWT:
        begin
          FGrid := ModflowGrid;
          ThreeDGridObserver.OnUpToDateSet := ModflowGrid.NotifyGridChanged;
        end;
      {$IFDEF SUTRA}
      msSutra:
        begin
          FGrid := nil;
        end
      {$ENDIF}
      else Assert(False);
    end;
    if (FModelSelection <> msModflowNWT) then
    begin
      if ModflowPackages.UpwPackage.IsSelected then
      begin
        ModflowPackages.UpwPackage.IsSelected := False;
        ModflowPackages.LpfPackage.IsSelected := True;
      end;
      if ModflowPackages.NwtPackage.IsSelected then
      begin
        ModflowPackages.NwtPackage.IsSelected := False;
        ModflowPackages.PcgPackage.IsSelected := True;
      end;
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
      if (frmGoPhast.PhastModel <> nil)
        and not (csReading in frmGoPhast.PhastModel.ComponentState)
        and not (csDestroying in frmGoPhast.PhastModel.ComponentState) then
      begin
        FDataArrayManager.CreateInitialDataSets;
      end;
    end;


    for Index := 0 to FDataArrayManager.DataSetCount - 1 do
    begin
      DataSet := FDataArrayManager.DataSets[Index] as TDataArray;
      UpdateDataArrayDimensions(DataSet);
    end;
    if not (csDestroying in ComponentState) then
    begin
      InvalidateScreenObjects;
    end;
    DataArrayManager.UpdateClassifications;
    Invalidate;
  end;
end;

//procedure TPhastModel.SetModflowOptions(const Value: TModflowOptions);
//begin
//  FModflowOptions.Assign(Value);
//end;

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

procedure TPhastModel.SetMt3dmsOutputControl(const Value: TMt3dmsOutputControl);
begin
  FMt3dmsOutputControl.Assign(Value);
end;

procedure TPhastModel.SetMt3dmsTimes(const Value: TMt3dmsTimeCollection);
begin
  FMt3dmsTimes.Assign(Value);
end;

procedure TPhastModel.SetNeedToRecalculateFrontCellColors(const Value: boolean);
var
  ChildIndex: Integer;
  ChildGrid: TCustomModelGrid;
  ChildModel: TChildModel;
begin
  Grid.NeedToRecalculateFrontCellColors := Value;
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModel := ChildModels[ChildIndex].ChildModel;
    if ChildModel <> nil then
    begin
      ChildGrid := ChildModel.Grid;
      if ChildGrid <> nil then
      begin
        ChildGrid.NeedToRecalculateFrontCellColors := Value;
      end;
    end;
  end;
end;

procedure TPhastModel.SetNeedToRecalculateSideCellColors(const Value: boolean);
var
  ChildIndex: Integer;
  ChildGrid: TCustomModelGrid;
  ChildModel: TChildModel;
begin
  Grid.NeedToRecalculateSideCellColors := Value;
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModel := ChildModels[ChildIndex].ChildModel;
    if ChildModel <> nil then
    begin
      ChildGrid := ChildModel.Grid;
      if ChildGrid <> nil then
      begin
        ChildGrid.NeedToRecalculateSideCellColors := Value;
      end;
    end;
  end;
end;

procedure TPhastModel.SetNeedToRecalculateTopCellColors(const Value: boolean);
var
  ChildIndex: Integer;
  ChildGrid: TCustomModelGrid;
  ChildModel: TChildModel;
begin
  Grid.NeedToRecalculateTopCellColors := Value;
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModel := ChildModels[ChildIndex].ChildModel;

    if ChildModel <> nil then
    begin
      ChildGrid := ChildModel.Grid;
      if ChildGrid <> nil then
      begin
        ChildGrid.NeedToRecalculateTopCellColors := Value;
      end;
    end;
  end;
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

function TPhastModel.GetFormulaManager: TFormulaManager;
begin
  result := FFormulaManager;
end;

function TPhastModel.GetFreeSurface: boolean;
begin
  result := FFreeSurface;
end;

//function TPhastModel.GetFrontHeight: integer;
//begin
//  if GuiSettings = nil then
//  begin
//    result := 0;
//  end
//  else
//  begin
//    result := GuiSettings.FrontHeight;
//  end;
//end;

//function TPhastModel.GetFrontX: double;
//begin
//  if GuiSettings = nil then
//  begin
//    result := 0;
//  end
//  else
//  begin
//    result := GuiSettings.FrontX;
//  end;
//end;

//function TPhastModel.GetFrontY: double;
//begin
//  if GuiSettings = nil then
//  begin
//    result := 0;
//  end
//  else
//  begin
//    result := GuiSettings.FrontY;
//  end;
//end;

//function TPhastModel.GetSideX: double;
//begin
//  if GuiSettings = nil then
//  begin
//    result := 0;
//  end
//  else
//  begin
//    result := GuiSettings.SideX;
//  end;
//end;

//function TPhastModel.GetSideY: double;
//begin
//  if GuiSettings = nil then
//  begin
//    result := 0;
//  end
//  else
//  begin
//    result := GuiSettings.SideY;
//  end;
//end;

function TPhastModel.GetSoluteTransport: boolean;
begin
  result := FSoluteTransport;
end;

function TPhastModel.GetSomeSegmentsUpToDate: boolean;
begin
  result := FSomeSegmentsUpToDate;
end;

function TPhastModel.GetSutraLayerStructure: TSutraLayerStructure;
begin
  if FSutraLayerStructure = nil then
  begin
    FSutraLayerStructure := TSutraLayerStructure.Create(self);
  end;
  result := FSutraLayerStructure
end;

//function TPhastModel.GetTopX: double;
//begin
//  if GuiSettings = nil then
//  begin
//    result := 0;
//  end
//  else
//  begin
//    result := GuiSettings.TopX;
//  end;
//end;

//function TPhastModel.GetTopY: double;
//begin
//  if GuiSettings = nil then
//  begin
//    result := 0;
//  end
//  else
//  begin
//    result := GuiSettings.TopY;
//  end;
//end;

procedure TPhastModel.SetFrontHeight(Value: integer);
begin
  if GuiSettings <> nil then
  begin
    GuiSettings.FrontHeight := Value;
  end;
end;

procedure TPhastModel.SetFrontTimeList(const Value: TCustomTimeList);
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildTimeList: TCustomTimeList;
begin
  inherited;
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModel := ChildModels[ChildIndex].ChildModel;
    if Value = nil then
    begin
      ChildTimeList := nil;
    end
    else
    begin
      ChildTimeList := ChildModel.GetTimeListByName(Value.Name)
    end;
    ChildModel.FrontTimeList := ChildTimeList;
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

//function TPhastModel.GetTopViewHeight: integer;
//begin
//  if GuiSettings = nil then
//  begin
//    result := 0;
//  end
//  else
//  begin
//    result := GuiSettings.TopViewHeight;
//  end;
//end;

//function TPhastModel.GetTopViewWidth: integer;
//begin
//  if GuiSettings = nil then
//  begin
//    result := 0;
//  end
//  else
//  begin
//    result := GuiSettings.TopViewWidth;
//  end;
//end;

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

procedure TCustomModel.UpdateActive(Sender: TObject);
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
  if (ModelSelection in [msMODFLOW, msModflowLGR, msModflowNWT])
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
  if (ModelSelection in [msMODFLOW, msModflowLGR, msModflowNWT])
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

procedure TPhastModel.UpdateChildGrids;
var
  ChildIndex: Integer;
  ScreenObjectIndex: Integer;
  ChildModel: TChildModel;
  ScreenObject: TScreenObject;
begin
  if ChildModels.Count > 0 then
  begin
    for ScreenObjectIndex := 0 to ScreenObjectCount - 1 do
    begin
      ScreenObject := ScreenObjects[ScreenObjectIndex];
      ChildModel := ScreenObject.ChildModel as TChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.HorizontalPositionScreenObject := ScreenObject;
      end;
    end;
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModels[ChildIndex].ChildModel.UpdateGrid;
    end;
  end;
end;

procedure TPhastModel.UpdateCombinedDisplayColumn;
var
  ModelIndex: Integer;
  ChildIndex: Integer;
  Index: Integer;
begin
  if LgrUsed then
  begin
    if SelectedModel = self then
    begin
      UpdateMapping;
      for Index := 0 to Length(FColumnMapping) - 1 do
      begin
        if FColumnMapping[Index].ParentPostion
          = SelectedColumn then
        begin
          CombinedDisplayColumn := Index;
          break;
        end;
      end;
    end
    else
    begin
      ModelIndex := -1;
      for ChildIndex := 0 to ChildModels.Count - 1 do
      begin
        if ChildModels[ChildIndex].ChildModel = SelectedModel then
        begin
          ModelIndex := ChildIndex;
          break;
        end;
      end;
      Assert(ModelIndex >= 0);
      UpdateMapping;
      for Index := 0 to Length(FColumnMapping) - 1 do
      begin
        if FColumnMapping[Index].ChildPositions[ModelIndex]
          = SelectedModel.SelectedColumn then
        begin
          CombinedDisplayColumn := Index;
          break;
        end;
      end;
    end;
  end;
end;

procedure TPhastModel.UpdateCombinedDisplayLayer;
var
  ModelIndex: Integer;
  ChildIndex: Integer;
  Index: Integer;
begin
  if LgrUsed then
  begin
    if SelectedModel = self then
    begin
      UpdateMapping;
      for Index := 0 to Length(FLayerMapping) - 1 do
      begin
        if FLayerMapping[Index].ParentPostion
          = SelectedLayer then
        begin
          CombinedDisplayLayer := Index;
          break;
        end;
      end;
    end
    else
    begin
      ModelIndex := -1;
      for ChildIndex := 0 to ChildModels.Count - 1 do
      begin
        if ChildModels[ChildIndex].ChildModel = SelectedModel then
        begin
          ModelIndex := ChildIndex;
          break;
        end;
      end;
      Assert(ModelIndex >= 0);
      UpdateMapping;
      for Index := 0 to Length(FLayerMapping) - 1 do
      begin
        if FLayerMapping[Index].ChildPositions[ModelIndex]
          = SelectedModel.SelectedLayer then
        begin
          CombinedDisplayLayer := Index;
          break;
        end;
      end;
    end;
  end;
end;

procedure TPhastModel.UpdateCombinedDisplayRow;
var
  ModelIndex: Integer;
  ChildIndex: Integer;
  Index: Integer;
begin
  if LgrUsed then
  begin
    if SelectedModel = self then
    begin
      UpdateMapping;
      for Index := 0 to Length(FRowMapping) - 1 do
      begin
        if FRowMapping[Index].ParentPostion
          = SelectedRow then
        begin
          CombinedDisplayRow := Index;
          break;
        end;
      end;
    end
    else
    begin
      ModelIndex := -1;
      for ChildIndex := 0 to ChildModels.Count - 1 do
      begin
        if ChildModels[ChildIndex].ChildModel = SelectedModel then
        begin
          ModelIndex := ChildIndex;
          break;
        end;
      end;
      Assert(ModelIndex >= 0);
      UpdateMapping;
      for Index := 0 to Length(FRowMapping) - 1 do
      begin
        if FRowMapping[Index].ChildPositions[ModelIndex]
          = SelectedModel.SelectedRow then
        begin
          CombinedDisplayRow := Index;
          break;
        end;
      end;
    end;
  end;
end;

procedure TCustomModel.UpdateDataArrayParameterUsed;
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
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildDataArray: TDataArray;
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
      AddDataSet(ADataSet);
      CreateVariables(ADataSet);
      UpdateDataArrayDimensions(ADataSet);
//      ADataSet.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
//        Grid.ColumnCount);
    end
    else
    begin
      UpdateDataArrayDimensions(ExistingDataSet);
//      ExistingDataSet.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
//        Grid.ColumnCount);

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
  for Index := 0 to FDataArrayManager.DataSetCount - 1 do
  begin
    ADataSet := FDataArrayManager[Index];
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      ChildDataArray := ChildModel.DataArrayManager.GetDataSetByName(ADataSet.Name);
      Assert(ChildDataArray <> nil);
      ChildDataArray.AssignProperties(ADataSet);
      ChildDataArray.Formula := ADataSet.Formula;
      ChildDataArray.Limits := ADataSet.Limits;
      ChildDataArray.ContourLimits := ADataSet.ContourLimits;
    end;
  end;

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

procedure TPhastModel.UpdateDataSetConnections;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModel := ChildModels[ChildIndex].ChildModel;
    ChildModel.UpdateDataSetConnections;
    TopGridObserver.TalksTo(ChildModel.TopGridObserver);
    ThreeDGridObserver.TalksTo(ChildModel.ThreeDGridObserver);
    HufKxNotifier.TalksTo(ChildModel.HufKxNotifier);
    HufKyNotifier.TalksTo(ChildModel.HufKyNotifier);
    HufKzNotifier.TalksTo(ChildModel.HufKzNotifier);
    HufSsNotifier.TalksTo(ChildModel.HufSsNotifier);
    HufSyNotifier.TalksTo(ChildModel.HufSyNotifier);
  end;
end;

procedure TPhastModel.UpdateDataSetDimensions;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  DataArrayManager.UpdateDataSetDimensions;
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModel := ChildModels[ChildIndex].ChildModel;
    if ChildModel <> nil then
    begin
      ChildModel.DataArrayManager.UpdateDataSetDimensions;
    end;
  end;
end;

//procedure TPhastModel.UpdateDataSets;
//var
//  index: integer;
//begin
//  inherited;
//  for index := 0 to ChildModels.Count - 1 do
//  begin
//    ChildModels[index].ChildModel.UpdateDataSets;
//  end;
//end;

function TPhastModel.GetSaveBfhBoundaryConditions: boolean;
begin
  result := FSaveBfhBoundaryConditions
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

procedure TCustomModel.SetThreeDDataSet(const Value: TDataArray);
begin
  case ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast, msModflow, msModflowLGR, msModflowNWT:
      begin
        Grid.ThreeDDataSet := Value;
      end;
    {$IFDEF SUTRA}
    msSutra:
      begin
        if (Mesh <> nil) then
        begin
          Mesh.ThreeDDataSet := Value;
        end;
      end;
    {$ENDIF}
    else
       Assert(False);
  end;
end;

procedure TPhastModel.SetTimes(const Value: TTimeCollection);
begin
  FTimes.Assign(Value);
  Invalidate;
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
          Parser.CreateVariable(Variable.Name, StrGlobalVariables, Variable.RealValue, Variable.Name);
        end;
      rdtInteger:
        begin
          Parser.CreateVariable(Variable.Name, StrGlobalVariables, Variable.IntegerValue, Variable.Name);
        end;
      rdtBoolean:
        begin
          Parser.CreateVariable(Variable.Name, StrGlobalVariables, Variable.BooleanValue, Variable.Name);
        end;
      rdtString:
        begin
          Parser.CreateVariable(Variable.Name, StrGlobalVariables, Variable.StringValue, Variable.Name);
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

//function TPhastModel.GetSideWidth: integer;
//begin
//  if GuiSettings = nil then
//  begin
//    result := 0;
//  end
//  else
//  begin
//    result := GuiSettings.SideWidth;
//  end;
//end;

procedure TPhastModel.SetSideTimeList(const Value: TCustomTimeList);
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildTimeList: TCustomTimeList;
begin
  inherited;
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModel := ChildModels[ChildIndex].ChildModel;
    if Value = nil then
    begin
      ChildTimeList := nil;
    end
    else
    begin
      ChildTimeList := ChildModel.GetTimeListByName(Value.Name)
    end;
    ChildModel.SideTimeList := ChildTimeList;
  end;
end;

procedure TPhastModel.SetSideWidth(const Value: integer);
begin
  if GuiSettings <> nil then
  begin
    GuiSettings.SideWidth := Value
  end;
end;

function TPhastModel.GasPhaseUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited GasPhaseUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.GasPhaseUsed(Sender);
    end;
  end;
end;

function TPhastModel.GbobIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.GbobPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.GbobPackage.IsSelected;
      end;
    end;
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

function TPhastModel.GetChildModels: TChildModelCollection;
begin
  result := FChildModels;
end;

function TPhastModel.GetCombinedDisplayColumn: integer;
begin
  result := FCombinedDisplayColumn;
  if (Grid <> nil) and not LgrUsed then
  begin
    result := Grid.DisplayColumn;
  end;
end;

function TPhastModel.GetCombinedDisplayLayer: integer;
begin
  result := FCombinedDisplayLayer;
  if (Grid <> nil) and not LgrUsed then
  begin
    result := Grid.DisplayLayer;
  end;
end;

function TPhastModel.GetCombinedDisplayRow: integer;
begin
  result := FCombinedDisplayRow;
  if (Grid <> nil) and not LgrUsed then
  begin
    result := Grid.DisplayRow;
  end;
end;

function TPhastModel.GetContourFont: TFont;
begin
  result := FContourFont
end;

function TPhastModel.GetCurrentScreenObject(VD: TViewDirection): TScreenObject;
begin
  if Assigned(OnGetCurrentScreenObject) then
  begin
    OnGetCurrentScreenObject(self, VD, result);
  end;
end;

function TPhastModel.GetDisplayName: string;
begin
  case ModelSelection of
    msUndefined: Assert(False);
    msPhast: result := 'PHAST';
    msModflow, msModflowNWT: result := 'MODFLOW';
    msModflowLGR: result := 'Parent model';
    {$IFDEF SUTRA} msSutra: result := 'SUTRA'; {$ENDIF}
    else Assert(False);
  end;
end;

function TCustomModel.GetThreeDDataSet: TDataArray;
begin
  result := nil;
  case ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast, msModflow, msModflowLGR, msModflowNWT:
      begin
        result := Grid.ThreeDDataSet;
      end;
    {$IFDEF SUTRA}
    msSutra:
      begin
        if (Mesh <> nil) then
        begin
          result := Mesh.ThreeDDataSet;
        end;
      end;
    {$ENDIF}
    else
       Assert(False);
  end;
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

procedure TCustomModel.UpdateFormulas(OldNames, NewNames: TStringList);
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
          Compiler.RenameVariable(VarIndex, NewNames[VariableIndex], NewNames[VariableIndex]);
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
          Compiler.RenameVariable(VarIndex, OldNames[VariableIndex], OldNames[VariableIndex]);
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
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildList: TCustomTimeList;
begin
  inherited;
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModel := ChildModels[ChildIndex].ChildModel;
    ChildList := ChildModel.GetTimeListByName(TimeList.Name);
    ChildModel.UpdateFrontTimeDataSet(ChildList, Time);
  end;
end;

procedure TCustomModel.UpdateFrontTimeDataSet(const TimeList: TCustomTimeList;
  const Time: double);
var
  TimeIndex: integer;
  LocalSelectedLayer, LocalSelectedRow, LocalSelectedColumn: integer;
begin
  if (Grid.LayerCount <= 0) or (Grid.RowCount <= 0) or (Grid.ColumnCount <= 0) then
  begin
    FFrontTimeList := nil;
    Grid.FrontDataSet := nil;
    Exit;
  end;
  LocalSelectedLayer := Grid.SelectedLayer;
  LocalSelectedRow := Grid.SelectedRow;
  LocalSelectedColumn := Grid.SelectedColumn;
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
    Grid.SelectedLayer := LocalSelectedLayer;
    Grid.SelectedRow := LocalSelectedRow;
    Grid.SelectedColumn := LocalSelectedColumn;
  end;
end;

procedure TCustomModel.UpdateMt3dmsActive(Sender: TObject);
const
  SetToFalse = 'Set to false because the cell is inactive in MODFLOW';
var
  Mt3dmsActive: TDataArray;
  ActiveDataArray: TDataArray;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
begin
  Mt3dmsActive := DataArrayManager.GetDataSetByName(StrMT3DMSActive);
  if Mt3dmsActive <> nil then
  begin
    ActiveDataArray := DataArrayManager.GetDataSetByName(rsActive);
    ActiveDataArray.Initialize;
    for ColIndex := 0 to ModflowGrid.ColumnCount - 1 do
    begin
      for RowIndex := 0 to ModflowGrid.RowCount - 1 do
      begin
        for LayerIndex := 0 to ModflowGrid.LayerCount -1 do
        begin
          if Mt3dmsActive.BooleanData[LayerIndex, RowIndex, ColIndex]
            and not ActiveDataArray.BooleanData[LayerIndex, RowIndex, ColIndex] then
          begin
            Mt3dmsActive.BooleanData[LayerIndex, RowIndex, ColIndex] := False;
            Mt3dmsActive.Annotation[LayerIndex, RowIndex, ColIndex] := SetToFalse;
          end;
        end;
      end;
    end;

  end;
end;

procedure TCustomModel.UpdateLakeId(Sender: TObject);
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
  if (ModelSelection in [msMODFLOW, msModflowLGR, msModflowNWT])
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

procedure TCustomModel.UpdateDischargeRouting(Sender: TObject);
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
  ID_Position: integer;
begin
  if (ModelSelection in [msMODFLOW, msModflowLGR, msModflowNWT])
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
            ID_Position := LakeList.IndexOf(-DischargeId);
            if ID_Position >= 0 then
            begin
              NewLakeID := -(ID_Position + 1);
              if DischargeId <> NewLakeID then
              begin
                DischargeRoutingArray.IntegerData[LayerIndex,RowIndex,ColIndex]
                  := NewLakeID;
                DischargeRoutingArray.Annotation[LayerIndex,RowIndex,ColIndex]
                  := DischargeRoutingArray.Annotation[
                  LayerIndex,RowIndex,ColIndex]
                  + ' and then renumbered';
              end;
            end;
          end
          else if DischargeId > 0 then
          begin
            ID_Position := SfrList.IndexOf(DischargeId);
            if ID_Position >= 0 then
            begin
              NewStreamID := (ID_Position + 1);
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
      end;
    finally
      LakeList.Free;
      SfrList.Free;
    end;
  end;
end;

procedure TCustomModel.UpdateOnPostInitialize;
var
  ActiveArray: TDataArray;
  WetDryArray: TDataArray;
  LakeIdArray: TDataArray;
  SpecifiedHeadArray: TDataArray;
  ModPathZoneArray: TDataArray;
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ResKvArray: TDataArray;
  Mt3dActiveArray: TDataArray;
begin
  LakeIdArray := FDataArrayManager.GetDataSetByName(rsLakeID);
  ActiveArray := FDataArrayManager.GetDataSetByName(rsActive);
  WetDryArray := FDataArrayManager.GetDataSetByName(rsWetDryFlag);
  SpecifiedHeadArray := FDataArrayManager.GetDataSetByName(rsModflowSpecifiedHead);
  ModPathZoneArray := FDataArrayManager.GetDataSetByName(StrModpathZone);
  ResKvArray := FDataArrayManager.GetDataSetByName(rsResKv);
  Mt3dActiveArray := FDataArrayManager.GetDataSetByName(StrMT3DMSActive);

  if Mt3dActiveArray <> nil then
  begin
    Mt3dActiveArray.OnPostInitialize := UpdateMt3dmsActive;
  end;

  if ResKvArray <> nil then
  begin
    ResKvArray.OnPostInitialize := AdjustResKvArray;
  end;

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
  if self is TPhastModel then
  begin
    PhastModel := TPhastModel(self);
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      PhastModel.ChildModels[ChildIndex].ChildModel.UpdateOnPostInitialize
    end;
  end;
end;

procedure TCustomModel.UpdateSideTimeDataSet(const TimeList: TCustomTimeList;
  const Time: double);
var
  TimeIndex: integer;
  LocalSelectedLayer, LocalSelectedRow, LocalSelectedColumn: integer;
begin
  if (Grid.LayerCount <= 0) or (Grid.RowCount <= 0) or (Grid.ColumnCount <= 0) then
  begin
    FSideTimeList := nil;
    Grid.SideDataSet := nil;
    Exit;
  end;
  LocalSelectedLayer := Grid.SelectedLayer;
  LocalSelectedRow := Grid.SelectedRow;
  LocalSelectedColumn := Grid.SelectedColumn;
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
    Grid.SelectedLayer := LocalSelectedLayer;
    Grid.SelectedRow := LocalSelectedRow;
    Grid.SelectedColumn := LocalSelectedColumn;
  end;
end;

procedure TCustomModel.UpdateTopTimeDataSet(const TimeList: TCustomTimeList;
  const Time: double);
var
  TimeIndex: integer;
  LocalSelectedLayer, LocalSelectedRow, LocalSelectedColumn: integer;
begin
  if (Grid.LayerCount <= 0) or (Grid.RowCount <= 0) or (Grid.ColumnCount <= 0) then
  begin
    FTopTimeList := nil;
    Grid.TopDataSet := nil;
    Exit;
  end;
  LocalSelectedLayer := Grid.SelectedLayer;
  LocalSelectedRow := Grid.SelectedRow;
  LocalSelectedColumn := Grid.SelectedColumn;
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
    Grid.SelectedLayer := LocalSelectedLayer;
    Grid.SelectedRow := LocalSelectedRow;
    Grid.SelectedColumn := LocalSelectedColumn;
  end;
end;

procedure TCustomModel.UpdateModPathZone(Sender: TObject);
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

procedure TCustomModel.UpdateWetDry(Sender: TObject);
var
  LakeIdArray: TDataArray;
  WetDryArray: TDataArray;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  IsLake: boolean;
begin
  if (ModelSelection in [msMODFLOW, msModflowLGR, msModflowNWT])
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

function TPhastModel.UzfInitialInfiltrationUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited UzfInitialInfiltrationUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.UzfInitialInfiltrationUsed(Sender);
    end;
  end;
end;

function TPhastModel.UzfIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.UzfPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.UzfPackage.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.UzfPackageUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited UzfPackageUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.UzfPackageUsed(Sender);
    end;
  end;
end;

function TPhastModel.UzfResidualWaterContentUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited UzfResidualWaterContentUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.UzfResidualWaterContentUsed(Sender);
    end;
  end;
end;

function TPhastModel.UzfUnsatVertKUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited UzfUnsatVertKUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.UzfUnsatVertKUsed(Sender);
    end;
  end;
end;

function TPhastModel.VerticalAnisotropyUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited VerticalAnisotropyUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.VerticalAnisotropyUsed(Sender);
    end;
  end;
end;

function TPhastModel.WelIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.WelPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.WelPackage.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.WetDryUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited WetDryUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.WetDryUsed(Sender);
    end;
  end;
end;

function TPhastModel.WettingActive: boolean;
var
  ChildIndex: Integer;
begin
  result := inherited WettingActive;
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.WettingActive;
    end;
  end;
end;

function TPhastModel.ZoneBudgetIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.ZoneBudget.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.ZoneBudget.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.ZoneBudgetSelected(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited ZoneBudgetSelected(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.ZoneBudgetSelected(Sender);
    end;
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

function TPhastModel.ReservoirLayerUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited ReservoirLayerUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.ReservoirLayerUsed(Sender);
    end;
  end;
end;

function TPhastModel.ReservoirPackageUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited ReservoirPackageUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.ReservoirPackageUsed(Sender);
    end;
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

function TPhastModel.ResIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.ResPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.ResPackage.IsSelected;
      end;
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
      ThreeDDataSet := nil;
    end
    else
    begin
      ThreeDDataSet := FThreeDTimeList.Items[TimeIndex];
    end;
  end;
end;

function TPhastModel.RivIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.RivPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.RivPackage.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.RouteUzfDischarge(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited RouteUzfDischarge(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.RouteUzfDischarge(Sender);
    end;
  end;
end;

function TPhastModel.RvobIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.RvobPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.RvobPackage.IsSelected;
      end;
    end;
  end;
end;

procedure TPhastModel.ClearViewedItems;
var
  Index: Integer;
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
  for Index := 0 to ChildModels.Count - 1 do
  begin
    ChildModels[Index].ChildModel.ClearViewedItems;
  end;
end;

function TPhastModel.DirectionCount(ViewDirection: TViewDirection): integer;
begin
  result := 0;
  case ViewDirection of
    vdTop: result := ModflowGrid.LayerCount;
    vdFront: result := ModflowGrid.RowCount;
    vdSide: result := ModflowGrid.ColumnCount;
    else Assert(False);
  end;
end;

procedure TPhastModel.DisallowChildGridUpdates;
var
  ChildModel: TChildModel;
  Index: Integer;
begin
  Inc(FChildGridUpdateCount);
  for Index := 0 to ChildModels.Count - 1 do
  begin
    ChildModel := ChildModels[Index].ChildModel;
    ChildModel.CanUpdateGrid := False;
  end;
end;

function TPhastModel.DispersionSelected: boolean;
var
  ChildIndex: integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.Mt3dmsDispersion.IsSelected;
  if not Result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      result := ChildModel.ModflowPackages.Mt3dmsDispersion.IsSelected;
      if result then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TPhastModel.UpdateMapping;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if (Length(FColumnMapping) = 0)
    and (Grid <> nil) and (Grid.ColumnCount > 0)
    and (Grid.RowCount > 0)and (Grid.LayerCount > 0) then
  begin
    UpdateAMapping(FColumnMapping, vdSide);
    UpdateAMapping(FRowMapping, vdFront);
    UpdateAMapping(FLayerMapping, vdTop);
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      ChildModel.DataArrayManager.InvalidateAllDataSets;
    end;
  end;
end;

function TPhastModel.CombinedCount(ViewDirection: TViewDirection): integer;
begin
  result := 0;
  case ViewDirection of
    vdTop: result := CombinedLayerCount;
    vdFront: result := CombinedRowCount;
    vdSide: result := CombinedColumnCount;
    else Assert(False);
  end;
end;

function TPhastModel.CombinedColumnCount: integer;
var
  ColIndex: Integer;
begin
  result := 0;
  case ModelSelection of
    msPhast: result := PhastGrid.ColumnCount;
    msModflow, msModflowNWT: result := ModflowGrid.ColumnCount;
    msModflowLGR:
      begin
        result := 0;
        for ColIndex := 0 to ModflowGrid.ColumnCount - 1 do
        begin
          Inc(result, MaxChildColumnsPerColumn(ColIndex));
        end;
      end
    else Assert(False);
  end;
end;

procedure TPhastModel.UpdateAMapping(var AMapping: TMappingArray;
  ViewDirection: TViewDirection);
var
  SubDisCount: Integer;
  ARange: TGridRange;
  ChildModel: TChildModel;
  ChildIndex: Integer;
  ChildCombinedIndex: Integer;
  SubDisIndex: Integer;
  MaxSubDiscretization: Integer;
  ParentDisIndex: Integer;
  CombinedIndex: Integer;
  Index: Integer;
  ArrayLength: Integer;
  MaximumPosition: Integer;
begin
  ArrayLength := CombinedCount(ViewDirection);
  SetLength(AMapping, ArrayLength);
  for Index := 0 to Length(AMapping) - 1 do
  begin
    SetLength(AMapping[Index].ChildPositions, ChildModels.Count);
  end;
  CombinedIndex := 0;
  for ParentDisIndex := 0 to DirectionCount(ViewDirection) - 1 do
  begin
    MaxSubDiscretization := MaxChildDisPerParentDis(
      ViewDirection, ParentDisIndex);
    for SubDisIndex := 0 to MaxSubDiscretization - 1 do
    begin
      ChildCombinedIndex := CombinedIndex + SubDisIndex;
      AMapping[ChildCombinedIndex].ParentPostion := ParentDisIndex;
    end;
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      MaximumPosition := ChildModel.MaxPosition(ViewDirection);
      ARange := ChildModel.ParentPositionToChildPositions(ViewDirection,
        ParentDisIndex);
      SubDisCount := ARange.Last - ARange.First + 1;
      for SubDisIndex := 0 to MaxSubDiscretization - 1 do
      begin
        ChildCombinedIndex := CombinedIndex + SubDisIndex;
        if (ARange.First < 0) then
        begin
          AMapping[ChildCombinedIndex].ChildPositions[ChildIndex] := -1;
        end
        else if ARange.First >= MaximumPosition then
        begin
          AMapping[ChildCombinedIndex].ChildPositions[ChildIndex] :=
            MaximumPosition;
        end
        else if (SubDisIndex >= SubDisCount) then
        begin
          AMapping[ChildCombinedIndex].ChildPositions[ChildIndex]
            := AMapping[ChildCombinedIndex-1].ChildPositions[ChildIndex]
        end
        else
        begin
          AMapping[ChildCombinedIndex].ChildPositions[ChildIndex] :=
            ARange.First + SubDisIndex;
        end;
      end;
    end;
    Inc(CombinedIndex, MaxSubDiscretization);
  end;
end;

function TPhastModel.CombinedRowCount: integer;
var
  RowIndex: Integer;
begin
  result := 0;
  case ModelSelection of
    msPhast: result := PhastGrid.RowCount;
    msModflow, msModflowNWT: result := ModflowGrid.RowCount;
    msModflowLGR:
      begin
        result := 0;
        for RowIndex := 0 to ModflowGrid.RowCount - 1 do
        begin
          Inc(result, MaxChildRowsPerRow(RowIndex));
        end;
      end
    else Assert(False);
  end;
end;

function TPhastModel.CombinedLayerCount: integer;
var
  LayerIndex: Integer;
begin
  result := 0;
  case ModelSelection of
    msPhast: result := PhastGrid.LayerCount;
    msModflow, msModflowNWT: result := ModflowGrid.LayerCount;
    msModflowLGR:
      begin
        result := 0;
        for LayerIndex := 0 to ModflowGrid.LayerCount - 1 do
        begin
          Inc(result, MaxChildLayersPerLayer(LayerIndex));
        end;
      end
    else Assert(False);
  end;
end;

function TPhastModel.MaxChildDisPerParentDis(ViewDirection: TViewDirection;
  Position: integer): integer;
begin
  result := 0;
  case ViewDirection of
    vdTop: result := MaxChildLayersPerLayer(Position);
    vdFront: result := MaxChildRowsPerRow(Position);
    vdSide: result := MaxChildColumnsPerColumn(Position);
    else Assert(False);
  end;
end;


function TPhastModel.MaxChildColumnsPerColumn(ColIndex: Integer): integer;
var
  Range: TGridRange;
  ChildModel: TChildModel;
  ChildIndex: Integer;
begin
  result := 1;
  if not LgrUsed then
  begin
    Exit;
  end;
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModel := ChildModels[ChildIndex].ChildModel;
    if ChildModel <> nil then
    begin
      Range := ChildModel.ParentColToChildCols(ColIndex);
      result := Max(result, Range.Last - Range.First + 1);
    end;
  end;
end;

function TPhastModel.MaxChildRowsPerRow(RowIndex: Integer): integer;
var
  Range: TGridRange;
  ChildModel: TChildModel;
  ChildIndex: Integer;
begin
  result := 1;
  if not LgrUsed then
  begin
    Exit;
  end;
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModel := ChildModels[ChildIndex].ChildModel;
    if ChildModel <> nil then
    begin
      Range := ChildModel.ParentRowToChildRows(RowIndex);
      result := Max(result, Range.Last - Range.First + 1);
    end;
  end;
end;

function TPhastModel.Mnw2IsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.Mnw2Package.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.Mnw2Package.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.MaxChildLayersPerLayer(LayerIndex: Integer): integer;
var
  Range: TGridRange;
  ChildModel: TChildModel;
  ChildIndex: Integer;
begin
  result := 1;
  if not LgrUsed then
  begin
    Exit;
  end;
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModel := ChildModels[ChildIndex].ChildModel;
    if ChildModel <> nil then
    begin
      Range := ChildModel.ParentLayerToChildLayers(LayerIndex);
      result := Max(result, Range.Last - Range.First + 1);
    end;
  end;
end;

function TPhastModel.ConfinedStorageCoefUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited ConfinedStorageCoefUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.ConfinedStorageCoefUsed(Sender);
    end;
  end;
end;

function TPhastModel.ConfiningBedKzUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited ConfiningBedKzUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.ConfiningBedKzUsed(Sender);
    end;
  end;
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

function TPhastModel.InitialWaterTableUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited InitialWaterTableUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.InitialWaterTableUsed(Sender);
    end;
  end;
end;

function TPhastModel.RchIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.RchPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.RchPackage.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.RchTimeVaryingLayers: boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.RchPackage.TimeVaryingLayers;
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      result := result or ChildModel.ModflowPackages.RchPackage.TimeVaryingLayers;
    end;
  end;
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

function TPhastModel.InitialHeadUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited InitialHeadUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.InitialHeadUsed(Sender);
    end;
  end;
end;

procedure TPhastModel.InitializeGages;
var
  ChildIndex: integer;
begin
  inherited;
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModels[ChildIndex].ChildModel.InitializeGages
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

procedure TPhastModel.InitializeSfrWriter(EvaluationType: TEvaluationType);
var
  ChildIndex: integer;
begin
  inherited;
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModels[ChildIndex].ChildModel.InitializeSfrWriter(EvaluationType);
  end;
end;

type
  TComponentCrack = class(TComponent);

function TCustomModel.Layavg: TOneDIntegerArray;
begin
  result := LayerStructure.Layavg;
end;

function TCustomModel.LayerCount: integer;
begin
  result := LayerStructure.LayerCount;
end;

function TCustomModel.LayerGroupUsed(LayerGroup: TLayerGroup): boolean;
begin
  result := True;
end;


function TCustomModel.Laytyp: TOneDIntegerArray;
begin
  result := LayerStructure.Laytyp;
end;

function TCustomModel.Layvka: TOneDIntegerArray;
begin
  result := LayerStructure.Layvka;
end;

function TCustomModel.LayerFractions(LayerGroup: TCustomLayerGroup): TDoubleDynArray;
var
  FractionIndex: Integer;
//  Fraction: Real;
begin
  if LayerGroup.Simulated then
  begin
    SetLength(result, LayerGroup.LayerCollection.Count);
    for FractionIndex := 0 to LayerGroup.LayerCollection.Count - 1 do
    begin
      result[FractionIndex] := (LayerGroup.LayerCollection.Items[FractionIndex]
        as TLayerFraction).Fraction;
    end;
  end
  else
  begin
    result := nil;
  end;
end;

function TPhastModel.LakePackageUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited LakePackageUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.LakePackageUsed(Sender);
    end;
  end;
end;

function TPhastModel.LakIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.LakPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.LakPackage.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.LakBathymetryUsed: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.LakPackage.IsSelected
    and (ModflowPackages.LakPackage.ExternalLakeChoice = elcAll);
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or (ChildModel.ModflowPackages.LakPackage.IsSelected
          and (ChildModel.ModflowPackages.LakPackage.ExternalLakeChoice = elcAll));
      end;
    end;
  end;
end;

function TPhastModel.LgrUsed: boolean;
begin
  result := (ModelSelection in [msModflowLGR])
    and (ChildModels.Count > 0);
end;

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

  if GuiSettings.FrontHeight <= 0 then
  begin
    GuiSettings.FrontHeight := 1;
  end;
  if GuiSettings.SideWidth <= 0 then
  begin
    GuiSettings.SideWidth := 1;
  end;
  ModflowTransientParameters.Loaded;
  if (ModelSelection in [msModflow, msModflowLGR, msModflowNWT])
    and not LgrUsed then
  begin
    CombinedDisplayColumn := Grid.DisplayColumn;
    CombinedDisplayRow := Grid.DisplayRow;
    CombinedDisplayLayer := Grid.DisplayLayer;
  end;
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

function TPhastModel.IsChildModelEdgeCell(Col, Row, Layer: integer;
  out CModel: TBaseModel): boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  LastLayer: integer;
begin
  result := False;
  CModel := nil;
  if LgrUsed then
  begin
    result := IsChildModelEdgeColRow(Col, Row, Layer, CModel);
    if not result then
//    begin
//      ChildModel := CModel as TChildModel;
//    end;
    begin
      for ChildIndex := 0 to ChildModels.Count - 1 do
      begin
        ChildModel := ChildModels[ChildIndex].ChildModel;
        LastLayer := ChildModel.Discretization.BottomLayerIndex;
        if (Layer = LastLayer)
          and (LastLayer <> ModflowGrid.LayerCount - 1)
          and (Col >= ChildModel.FirstCol)
          and (Col <= ChildModel.LastCol)
          and (Row >= ChildModel.FirstRow)
          and (Row <= ChildModel.LastRow) then
        begin
          result := True;
          CModel := ChildModel;
          break;
        end;
      end;
    end;
  end;
end;

function TPhastModel.IsChildModelEdgeColRow(Col, Row, Layer: integer;
  out CModel: TBaseModel) : boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  ChildModel := nil;
  result := False;
  if LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        if (Col = ChildModel.FirstCol)
          or (Col = ChildModel.LastCol) then
        begin
          if (Row >= ChildModel.FirstRow)
            and (Row <= ChildModel.LastRow) then
          begin
            if Layer <= ChildModel.Discretization.BottomLayerIndex then
            begin
              result := True;
              break;
            end;
          end;
        end
        else if (Row = ChildModel.FirstRow)
          or (Row = ChildModel.LastRow) then
        begin
          if (Col >= ChildModel.FirstCol)
            and (Col <= ChildModel.LastCol) then
          begin
            if Layer <= ChildModel.Discretization.BottomLayerIndex then
            begin
              result := True;
              break;
            end;
          end;
        end;
      end;
    end;
  end;
  CModel := ChildModel;
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

function TPhastModel.KineticsUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited KineticsUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.KineticsUsed(Sender);
    end;
  end;
end;

function TPhastModel.KyUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited KyUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.KyUsed(Sender);
    end;
  end;
end;

function TPhastModel.KzUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited KzUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.KzUsed(Sender);
    end;
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

procedure TPhastModel.SetFileName(const Value: string);
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  inherited;
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModel := ChildModels[ChildIndex].ChildModel;
    if ChildModel <> nil then
    begin
      ChildModel.SetFileName(Value);
    end;
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

function TCustomModel.ModelResultsRequired(Sender: TObject): boolean;
begin
  result := False;
end;

function TPhastModel.ModflowInitialHeadUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited ModflowInitialHeadUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.ModflowInitialHeadUsed(Sender);
    end;
  end;
end;

function TPhastModel.ModflowUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited ModflowUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.ModflowUsed(Sender);
    end;
  end;
end;

function TPhastModel.Mt3dmsIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.Mt3dBasic.IsSelected;
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.Mt3dBasic.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.MODPATHIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.Modpath.IsSelected;
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.Modpath.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.ModpathUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited ModpathUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.ModpathUsed(Sender);
    end;
  end;
end;

function TPhastModel.ModDispDataArrayUsed(Sender: TObject): boolean;
var
  DataArray: TDataArray;
  function DataArrayUsed(ChemSpecies: TMobileChemSpeciesCollection): boolean;
  var
    Index: Integer;
    AChemItem: TMobileChemSpeciesItem;
  begin
    result := False;
    for Index := 0 to ChemSpecies.Count - 1 do
    begin
      AChemItem := ChemSpecies[Index];
      result := AChemItem.DiffusionCoefDataArrayName = DataArray.Name;
      if result then
      begin
        Exit;
      end;
    end;
  end;
begin
  result := (ModelSelection in [msModflow, msModflowLGR, msModflowNWT])
    and ModflowPackages.Mt3dBasic.IsSelected
    and AnyDispersionMultiDiffusion;
  if result then
  begin
    DataArray := Sender as TDataArray;
    result := DataArrayUsed(MobileComponents);
  end;
end;

function TPhastModel.Mt3dMsInitialConcUsed(Sender: TObject): boolean;
var
  DataArray: TDataArray;
  function DataArrayUsed(ChemSpecies: TCustomChemSpeciesCollection): boolean;
  var
    Index: Integer;
    AChemItem: TChemSpeciesItem;
  begin
    result := False;
    for Index := 0 to ChemSpecies.Count - 1 do
    begin
      AChemItem := ChemSpecies[Index];
      result := AChemItem.InitialConcDataArrayName = DataArray.Name;
      if result then
      begin
        Exit;
      end;
    end;
  end;
begin
  result := (ModelSelection in [msModflow, msModflowLGR, msModflowNWT])
    and ModflowPackages.Mt3dBasic.IsSelected;
  if result then
  begin
    DataArray := Sender as TDataArray;
    result := DataArrayUsed(MobileComponents)
      or DataArrayUsed(ImmobileComponents);
  end;
end;

function TPhastModel.Mt3dMsFirstSorbParamUsed(Sender: TObject): boolean;
var
  DataArray: TDataArray;
  function DataArrayUsed(ChemSpecies: TCustomChemSpeciesCollection): boolean;
  var
    Index: Integer;
    AChemItem: TChemSpeciesItem;
  begin
    result := False;
    for Index := 0 to ChemSpecies.Count - 1 do
    begin
      AChemItem := ChemSpecies[Index];
      result := AChemItem.FirstSorbParamDataArrayName = DataArray.Name;
      if result then
      begin
        Exit;
      end;
    end;
  end;
begin
  result := (ModelSelection in [msModflow, msModflowLGR, msModflowNWT])
    and ModflowPackages.Mt3dBasic.IsSelected
    and ModflowPackages.Mt3dmsChemReact.IsSelected
    and (ModflowPackages.Mt3dmsChemReact.SorptionChoice <> scNone);
  if result then
  begin
    DataArray := Sender as TDataArray;
    result := DataArrayUsed(MobileComponents)
      or DataArrayUsed(ImmobileComponents);
  end;
end;

function TPhastModel.Mt3dMsSecondSorbParamUsed(Sender: TObject): boolean;
var
  DataArray: TDataArray;
  function DataArrayUsed(ChemSpecies: TCustomChemSpeciesCollection): boolean;
  var
    Index: Integer;
    AChemItem: TChemSpeciesItem;
  begin
    result := False;
    for Index := 0 to ChemSpecies.Count - 1 do
    begin
      AChemItem := ChemSpecies[Index];
      result := AChemItem.SecondSorbParamDataArrayName = DataArray.Name;
      if result then
      begin
        Exit;
      end;
    end;
  end;
begin
  result := (ModelSelection in [msModflow, msModflowLGR, msModflowNWT])
    and ModflowPackages.Mt3dBasic.IsSelected
    and ModflowPackages.Mt3dmsChemReact.IsSelected
    and (ModflowPackages.Mt3dmsChemReact.SorptionChoice <> scNone);
  if result then
  begin
    DataArray := Sender as TDataArray;
    result := DataArrayUsed(MobileComponents)
      or DataArrayUsed(ImmobileComponents);
  end;
end;

function TPhastModel.Mt3dmsReactionRateDisolvedUsed(Sender: TObject): boolean;
var
  DataArray: TDataArray;
  function DataArrayUsed(ChemSpecies: TCustomChemSpeciesCollection): boolean;
  var
    Index: Integer;
    AChemItem: TChemSpeciesItem;
  begin
    result := False;
    for Index := 0 to ChemSpecies.Count - 1 do
    begin
      AChemItem := ChemSpecies[Index];
      result := AChemItem.ReactionRateDisolvedDataArrayName = DataArray.Name;
      if result then
      begin
        Exit;
      end;
    end;
  end;
begin
  result := (ModelSelection in [msModflow, msModflowLGR, msModflowNWT])
    and ModflowPackages.Mt3dBasic.IsSelected
    and ModflowPackages.Mt3dmsChemReact.IsSelected
    and (ModflowPackages.Mt3dmsChemReact.KineticChoice <> kcNone);
  if result then
  begin
    DataArray := Sender as TDataArray;
    result := DataArrayUsed(MobileComponents)
      or DataArrayUsed(ImmobileComponents);
  end;
end;

function TPhastModel.Mt3dmsReactionRateSorbedUsed(Sender: TObject): boolean;
var
  DataArray: TDataArray;
  function DataArrayUsed(ChemSpecies: TCustomChemSpeciesCollection): boolean;
  var
    Index: Integer;
    AChemItem: TChemSpeciesItem;
  begin
    result := False;
    for Index := 0 to ChemSpecies.Count - 1 do
    begin
      AChemItem := ChemSpecies[Index];
      result := AChemItem.ReactionRateSorbedDataArrayName = DataArray.Name;
      if result then
      begin
        Exit;
      end;
    end;
  end;
begin
  result := (ModelSelection in [msModflow, msModflowLGR, msModflowNWT])
    and ModflowPackages.Mt3dBasic.IsSelected
    and ModflowPackages.Mt3dmsChemReact.IsSelected
    and (ModflowPackages.Mt3dmsChemReact.KineticChoice <> kcNone);
  if result then
  begin
    DataArray := Sender as TDataArray;
    result := DataArrayUsed(MobileComponents)
      or DataArrayUsed(ImmobileComponents);
  end;
end;

procedure TCustomModel.RenameOldVerticalLeakance;
const
  OldVerticalConductance = 'Vertical_Conductance';
var
  DataArray: TDataArray;
  VarIndex: Integer;
  DA: TDataArray;
  CompilerIndex: Integer;
  CompilerList: TList;
  Compiler: TRbwParser;
begin
  DataArray := DataArrayManager.GetDataSetByName(OldVerticalConductance);
  if DataArray <> nil then
  begin
    DA := DataArrayManager.GetDataSetByName(StrVerticalConductance);
    if DA <> nil then
    begin
      DataArrayManager.ExtractDataSet(DA);
      CompilerList := TList.Create;
      try
        FillCompilerList(CompilerList);
        for CompilerIndex := 0 to CompilerList.Count - 1 do
        begin
          Compiler := CompilerList[CompilerIndex];
          VarIndex := Compiler.IndexOfVariable(DA.Name);
          if VarIndex >= 0 then
          begin
            Compiler.RemoveVariable(Compiler.Variables[VarIndex] as TCustomVariable);
          end;
        end;
      finally
        CompilerList.Free;
      end;
      RenameDataArray(DataArray, StrVerticalConductance,
        StrVerticalConductanceDisplayName);
      DataArray.OnDataSetUsed := DA.OnDataSetUsed;
      DA.Free;
    end;
  end;
//  end;
end;


function TPhastModel.Mt3dMsSorbImmobInitialConcUsed(Sender: TObject): boolean;
var
  DataArray: TDataArray;
  function DataArrayUsed(ChemSpecies: TCustomChemSpeciesCollection): boolean;
  var
    Index: Integer;
    AChemItem: TChemSpeciesItem;
  begin
    result := False;
    for Index := 0 to ChemSpecies.Count - 1 do
    begin
      AChemItem := ChemSpecies[Index];
      result := AChemItem.SorbOrImmobInitialConcDataArrayName = DataArray.Name;
      if result then
      begin
        Exit;
      end;
    end;
  end;
begin
  result := (ModelSelection in [msModflow, msModflowLGR, msModflowNWT])
    and ModflowPackages.Mt3dBasic.IsSelected
    and ModflowPackages.Mt3dmsChemReact.IsSelected
    and (ModflowPackages.Mt3dmsChemReact.OtherInitialConcChoice = oicUse);
  if result then
  begin
    DataArray := Sender as TDataArray;
    result := DataArrayUsed(MobileComponents)
      or DataArrayUsed(ImmobileComponents);
  end;
end;

function TPhastModel.Mt3dmsSsmIsSelected: Boolean;
var
  ChildIndex: integer;
  ChildModel: TChildModel;
begin
  result := Mt3dmsIsSelected;
  if result then
  begin
    result := ModflowPackages.Mt3dmsSourceSink.IsSelected;
    if not result and LgrUsed then
    begin
      for ChildIndex := 0 to ChildModels.Count - 1 do
      begin
        ChildModel := ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          result := result or ChildModel.ModflowPackages.Mt3dmsSourceSink.IsSelected;
        end;
      end;
    end;
  end;
end;

function TPhastModel.Mt3dmsTobIsSelected: Boolean;
var
  ChildIndex: integer;
  ChildModel: TChildModel;
begin
  result := Mt3dmsIsSelected;
  if result then
  begin
    result := ModflowPackages.Mt3dmsTransObs.IsSelected;
    if not result and LgrUsed then
    begin
      for ChildIndex := 0 to ChildModels.Count - 1 do
      begin
        ChildModel := ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          result := result or ChildModel.ModflowPackages.Mt3dmsTransObs.IsSelected;
        end;
      end;
    end;
  end;
end;

procedure TPhastModel.EndDataSetUpdate;
begin
  Dec(FDataSetUpdateCount);
end;

procedure TPhastModel.EndGridChange;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  inherited;
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModel := ChildModels[ChildIndex].ChildModel;
    if ChildModel <> nil then
    begin
      ChildModel.EndGridChange;
    end;
  end;
end;

procedure TPhastModel.EndScreenObjectUpdate;
begin
  Dec(FScreenObjectUpdateCount);
  ScreenObjectsChanged(nil);
end;

function TPhastModel.SubIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.SubPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.SubPackage.IsSelected;
      end;
    end;
  end;
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
  result := (ModelSelection in [msMODFLOW, msModflowLGR, msModflowNWT])
    and ModflowPackages.SubPackage.IsSelected;
  if result then
  begin
//    result := False;
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
  result := (ModelSelection in [msMODFLOW, msModflowLGR, msModflowNWT])
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

function TPhastModel.SurfacesUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited SurfacesUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.SurfacesUsed(Sender);
    end;
  end;
end;

function TPhastModel.SwtIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.SwtPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.SwtPackage.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.SwtOffsetsUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited SwtOffsetsUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.SwtOffsetsUsed(Sender);
    end;
  end;
end;

function TPhastModel.SwtSelected(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited SwtSelected(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.SwtSelected(Sender);
    end;
  end;
end;

function TPhastModel.SwtSpecifiedUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited SwtSpecifiedUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.SwtSpecifiedUsed(Sender);
    end;
  end;
end;

procedure TPhastModel.FixOldModel;
var
  ModpathZone: TDataArray;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  LakeIDArray: TDataArray;
  ModflowLakBoundary: TLakBoundary;
  SfrBoundary: TSfrBoundary;
  ParamItem: TSfrParamIcalcItem;
  SegItem: TCustomModflowBoundaryItem;
//  ChildIndex: Integer;
//  ChildModel: TChildModel;
//  Index: integer;
//  ADataSet, ChildDataArray: TDataArray;
begin
   RenameOldVerticalLeakance;

//  if FileVersionEqualOrEarlier('2.14.1.3') then
//  begin
//    RenameOldVerticalLeakance;
//
//    for ChildIndex := 0 to ChildModels.Count - 1 do
//    begin
//      ChildModel := ChildModels[ChildIndex].ChildModel;
//      ChildModel.RenameOldVerticalLeakance
//    end;
//    for Index := 0 to FDataArrayManager.DataSetCount - 1 do
//    begin
//      ADataSet := FDataArrayManager[Index];
//      for ChildIndex := 0 to ChildModels.Count - 1 do
//      begin
//        ChildModel := ChildModels[ChildIndex].ChildModel;
//        ChildDataArray := ChildModel.DataArrayManager.GetDataSetByName(ADataSet.Name);
//        Assert(ChildDataArray <> nil);
//        ChildDataArray.AssignProperties(ADataSet);
//        ChildDataArray.Formula := ADataSet.Formula;
//        ChildDataArray.Limits := ADataSet.Limits;
//        ChildDataArray.ContourLimits := ADataSet.ContourLimits;
//      end;
//    end;
//  end;

  if (Grid <> nil) and (Grid.GridAngle <> 0)
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
  if (Grid <> nil) and (Grid.GridAngle <> 0)
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
//  if FileVersionEqualOrEarlier('2.8.0.16')
//    and ModflowPackages.RchPackage.IsSelected then
//  begin
//    ModflowPackages.RchPackage.AssignmentMethod := umAssign;
//  end;
//  if FileVersionEqualOrEarlier('2.8.0.18')
//    and ModflowPackages.UzfPackage.IsSelected then
//  begin
//    ModflowPackages.UzfPackage.AssignmentMethod := umAssign;
//  end;
  if FileVersionEqualOrEarlier('2.8.0.18') then
  begin
    LakeIDArray := DataArrayManager.GetDataSetByName(rsLakeID);
    if LakeIDArray <> nil then
    begin
      for ScreenObjectIndex := 0 to ScreenObjectCount - 1 do
      begin
        AScreenObject := ScreenObjects[ScreenObjectIndex];
        ModflowLakBoundary := AScreenObject.ModflowLakBoundary;
        if (ModflowLakBoundary = nil) or not ModflowLakBoundary.Used then
        begin
          AScreenObject.RemoveDataSet(LakeIDArray);
        end;
      end;
      LakeIDArray.Invalidate;
    end;
  end;
  // angles
  if (Grid <> nil) and (Grid.GridAngle <> 0)
    and FileVersionEqualOrEarlier('2.9.1.9')
    and (FormulaManager.FunctionUsed(ObjectCurrentSegmentAngle)
    or FormulaManager.FunctionUsed(ObjectDegrees)
    or FormulaManager.FunctionUsed(ObjectDegreesLimited)) then
  begin
    // The VertexInterpolate function gave incorrect results in
    // versions '2.6.0.3' and earlier if the grid was rotated.
    // NodeInterpolate is a synonym for VertexInterpolate.
    FDataArrayManager.InvalidateAllDataSets;
  end;

  if FileVersionEqualOrEarlier('2.11.0.5')
    and not ModflowPackages.NwtPackage.IsSelected then
  begin
    ModflowPackages.NwtPackage.FluxTolerance.Value := 0.06;
  end;

  if FileVersionEqualOrEarlier('2.12.0.1') then
  begin
    for ScreenObjectIndex := 0 to ScreenObjectCount - 1 do
    begin
      AScreenObject := ScreenObjects[ScreenObjectIndex];
      SfrBoundary := AScreenObject.ModflowSfrBoundary;
      if SfrBoundary <> nil then
      begin
        while SfrBoundary.ParamIcalc.Count < SfrBoundary.SegmentFlows.Count do
        begin
          ParamItem := SfrBoundary.ParamIcalc.Add as TSfrParamIcalcItem;
          if ParamItem.Index > 0 then
          begin
            ParamItem.Assign(SfrBoundary.ParamIcalc.Items[ParamItem.Index-1]);
          end;
          SegItem := SfrBoundary.SegmentFlows.Items[ParamItem.Index];
          ParamItem.StartTime := SegItem.StartTime;
          ParamItem.EndTime := SegItem.EndTime;
        end;
      end;
    end;
  end;
  if FileVersionEqualOrEarlier('2.16.1.1')
    and ModflowPackages.ModPath.IsSelected then
  begin
    ModflowPackages.ModPath.MpathVersion := mp5;
    ModflowPackages.ModPath.StopOption := soExtend;
  end;
end;

procedure TPhastModel.FreeSfrWriter;
var
  ChildIndex: integer;
begin
  inherited;
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModels[ChildIndex].ChildModel.FreeSfrWriter;
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
        if not (ModflowPackages.LpfPackage.IsSelected
          or ModflowPackages.UpwPackage.IsSelected) then
        begin
          Exit;
        end;
      end;
    ptLPF_SS,ptLPF_SY:
      begin
        if not (ModflowPackages.LpfPackage.IsSelected
          or ModflowPackages.UpwPackage.IsSelected)
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
      Assert(Length(PARNAM) <= MaxString12);
      ModelMateParam.Name := ConvertString12(PARNAM);
      GroupAttribute := ModelMateParam.AllAtts.Items[ParAttPos(patGroupName)];
      GroupAttribute.Text := GroupNames[ModelMuseParam.ParameterType];
      FoundGroup := False;
      for GroupIndex := 0 to Project.ParGpSet.Count - 1 do
      begin
        Group := Project.ParGpSet.Items[GroupIndex];
        if SameText(string(Group.Name), GroupAttribute.Text) then
        begin
          FoundGroup := True;
          Break;
        end;
      end;
      if not FoundGroup then
      begin
        Group := Project.ParGpSet.Add;
        Assert(Length(GroupAttribute.Text) <= MaxString12);
        Group.Initialize(ConvertString12(GroupAttribute.Text), ConvertString12(GroupAttribute.Text));
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
        Assert(Length(OBSNAM) <= MaxString20);
        ModelMateObs.Name := ConvertString20(OBSNAM);
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
        Assert(Length(OBSNAM) <= MaxString20);
        ModelMateObs.Name := ConvertString20(OBSNAM);

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
          case Method of
            momAllHeads: ModelMuseHeadObs.Head := StrToFloatDef(ValAttribute.Text, 0);
            momHeadAndDrawdown: ModelMuseHeadObs.HeadChange := StrToFloatDef(ValAttribute.Text, 0);
            else Assert(False);
          end;
          ModelMuseHeadObs.Statistic := StrToFloatDef(StatAttribute.Text, 0);
          ModelMuseHeadObs.StatFlag := StrToStatFlag(StatFlagAttribute.Text);
        end;
      mmoExport:
        begin
          case Method of
            momAllHeads: ValAttribute.Text := FortranFloatToStr(ModelMuseHeadObs.Head);
            momHeadAndDrawdown: ValAttribute.Text := FortranFloatToStr(ModelMuseHeadObs.HeadChange);
            else Assert(False);
          end;
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
  ModName: string;
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
      ConvertString12(Copy(ModflowOptions.ProjectName, 1, MaxString12));
  end
  else if ModelFileName <> '' then
  begin
    ModName := Copy(ChangeFileExt(ExtractFileName(ModelFileName),''), 1, MaxString12);
    ModelMateProject.UcProject.ModelName := ConvertString12(ModName);
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
          AMessage := StrYourModelMateFile + sLineBreak + sLineBreak
            + ParameterList.Text
        end
        else
        begin
          AMessage := Format(StrYourModelMateFile2, [ParameterList.Count])
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
          AMessage := Format(StrYourModelMateFile3, [DepType])
            + sLineBreak + sLineBreak
            + ObservationList.Text;
        end
        else
        begin
          AMessage := Format(StrYourModelMateFile4, [ObservationList.Count, DepType])
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
              if ModelMateObs.AllAtts[DepAttPos(datGroupName)].Text = string(Group.Name) then
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

procedure TPhastModel.UpdateMt3dmsChemDataSets;
begin
  MobileComponents.UpdateDataArrays;
  ImMobileComponents.UpdateDataArrays;
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
  OldDecSeparator: Char;
begin
  OldDecSeparator := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := '.';
    if Project.UcProject.ModelName <> '' then
    begin
      ModflowOptions.ProjectName := string(Project.UcProject.ModelName);
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
  finally
    FormatSettings.DecimalSeparator := OldDecSeparator;
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
    if SameText(string(Group.Name), GroupName) then
    begin
      FoundGroup := True;
      break;
    end;
  end;
  if not FoundGroup then
  begin
    Group := Project.ObsGpSet.Add;

    Assert(Length(GroupName) <= MaxString12);
    Group.Initialize(ConvertString20(GroupName), ConvertString12(GroupName), dcObs);
    Group.AllAtts[DepAttPos(datPlotSymbol)].Text := IntToStr(PlotSymbol);

    IAtt := PosDepCap('Statistic');
    Project.ObservationSetup.ObsAttributes.Items[IAtt].ControlMethod := cmByItem;
    IAtt := PosDepCap('StatFlag');
    Project.ObservationSetup.ObsAttributes.Items[IAtt].ControlMethod := cmByItem;
  end;
end;

function TPhastModel.EquilibriumPhasesUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited EquilibriumPhasesUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.EquilibriumPhasesUsed(Sender);
    end;
  end;
end;

function TPhastModel.EtsIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.EtsPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.EtsPackage.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.EtsTimeVaryingLayers: boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.EtsPackage.TimeVaryingLayers;
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      result := result or ChildModel.ModflowPackages.EtsPackage.TimeVaryingLayers;
    end;
  end;
end;

function TPhastModel.EvtIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.EvtPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.EvtPackage.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.EvtTimeVaryingLayers: boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.EvtPackage.TimeVaryingLayers;
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      result := result or ChildModel.ModflowPackages.EvtPackage.TimeVaryingLayers;
    end;
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
  ObsVersionSingular: string;
  ObsVersionPlural: string;
  WarningMessage: string;
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
    ObservationList.AddObject(string(ModelMateObs.Name), ModelMateObs);
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
          if Length(OBSNAM) > MaxString12 then
          begin
            OBSNAM := Observations.ObservationName + IntToStr(ValueIndex + 1);
          end;
          if Length(OBSNAM) > MaxString12 then
          begin
            // The GUI is designed to prevent this from ever being required.
            SetLength(OBSNAM, MaxString12);
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
            ofObserved:
              begin
                ObsVersionSingular := StrObservation;
                ObsVersionPlural := StrObservations;
              end;

            ofPredicted:
              begin
                ObsVersionSingular := StrPrediction;
                ObsVersionPlural := StrPredictions;
              end
            else Assert(False);
          end;
          Beep;
          WarningMessage := Format(StrOneOrMore0sFro,
            [ObsVersionPlural, ObsVersionSingular, ObsVersionSingular]);
          MessageDlg(WarningMessage + sLineBreak + sLineBreak
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
    ParameterList.AddObject(string(ModelMateParam.Name), ModelMateParam);
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
          MessageDlg(StrOneOrMoreParamete + sLineBreak + sLineBreak
            + ParameterList.Text, mtWarning, [mbOK], 0);
        end;
      end;
    mmoExport: ;
    else Assert(False);
  end;
end;

function TPhastModel.HfbIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.HfbPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.HfbPackage.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.HobIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.HobPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.HobPackage.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.HorizontalAnisotropyUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited HorizontalAnisotropyUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.HorizontalAnisotropyUsed(Sender);
    end;
  end;
end;

procedure TPhastModel.IncreaseScreenObjectCapacity(const Delta: integer);
begin
  if Delta <= 0 then Exit;
  FScreenObjectList.Capacity := FScreenObjectList.Capacity + Delta;
end;

//function TPhastModel.GetWindowState: TWindowState;
//begin
//  if GuiSettings = nil then
//  begin
//    result := wsNormal;
//  end
//  else
//  begin
//    result := GuiSettings.WindowState;
//  end;
//end;

function TPhastModel.GhbIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.GhbBoundary.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.GhbBoundary.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.GmgIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.GmgPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.GmgPackage.IsSelected;
      end;
    end;
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

function TPhastModel.SfrIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.SfrPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.SfrPackage.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.SipIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.SipPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.SipPackage.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.SolidSolutionUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited SolidSolutionUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.SolidSolutionUsed(Sender);
    end;
  end;
end;

function TPhastModel.SpecificStorageUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited SpecificStorageUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.SpecificStorageUsed(Sender);
    end;
  end;
end;

function TPhastModel.SpecificYieldUsed(Sender: TObject): boolean;
var
  ChildIndex: Integer;
begin
  result := inherited SpecificYieldUsed(Sender);
  if not result and LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      result := result or
        ChildModels[ChildIndex].ChildModel.SpecificYieldUsed(Sender);
    end;
  end;
end;

procedure TPhastModel.SetSoluteTransport(const Value: boolean);
begin
  if FSoluteTransport <> Value then
  begin
    FSoluteTransport := Value;
    Invalidate;
  end;
end;

procedure TPhastModel.SetSomeSegmentsUpToDate(const Value: boolean);
begin
  FSomeSegmentsUpToDate := Value;
end;

procedure TPhastModel.SetSutraLayerStructure(const Value: TSutraLayerStructure);
begin
  FSutraLayerStructure.Assign(Value);
end;

procedure TPhastModel.SetSaveBfhBoundaryConditions(const Value: boolean);
begin
  FSaveBfhBoundaryConditions := Value;
end;

procedure TPhastModel.SetScreenObjectCollection(
  const Value: TScreenObjectCollection);
begin
  FScreenObjectCollection.Assign(Value);
end;

procedure TPhastModel.SetSelectedModel(const Value: TCustomModel);
begin
  FSelectedModel := Value;
end;

procedure TPhastModel.SetSfrStreamLinkPlot(const Value: TSfrStreamLinkPlot);
begin
  FSfrStreamLinkPlot.Assign(Value);
end;

procedure TPhastModel.SetShowContourLabels(const Value: boolean);
begin
  FShowContourLabels := Value;
end;

procedure TPhastModel.UpdateThreeDTimeDataSet(const TimeList: TCustomTimeList;
  const Time: double);
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildList: TCustomTimeList;
begin
  inherited;
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModel := ChildModels[ChildIndex].ChildModel;
    ChildList := ChildModel.GetTimeListByName(TimeList.Name);
    ChildModel.UpdateThreeDTimeDataSet(ChildList, Time);
  end;
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

procedure TPhastModel.UpdateTopTimeDataSet(const TimeList: TCustomTimeList;
  const Time: double);
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildList: TCustomTimeList;
begin
  inherited;
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModel := ChildModels[ChildIndex].ChildModel;
    ChildList := ChildModel.GetTimeListByName(TimeList.Name);
    ChildModel.UpdateTopTimeDataSet(ChildList, Time);
  end;
end;

procedure TCustomModel.UpdateThreeDTimeDataSet(const TimeList: TCustomTimeList;
  const Time: double);
var
  TimeIndex: integer;
  LocalSelectedLayer, LocalSelectedRow, LocalSelectedColumn: integer;
begin
  if (Grid.LayerCount <= 0) or (Grid.RowCount <= 0) or (Grid.ColumnCount <= 0) then
  begin
    FThreeDTimeList := nil;
    ThreeDDataSet := nil;
    Exit;
  end;
  LocalSelectedLayer := Grid.SelectedLayer;
  LocalSelectedRow := Grid.SelectedRow;
  LocalSelectedColumn := Grid.SelectedColumn;
  try
    FThreeDDisplayTime := Time;
    if not TimeList.UpToDate then
    begin
      TimeList.Initialize;
    end;
    TimeIndex := TimeList.FirstTimeGreaterThan(Time) - 1;
    if TimeIndex < 0 then
    begin
      ThreeDDataSet := nil;
    end
    else
    begin
      ThreeDDataSet := TimeList.Items[TimeIndex];
      ThreeDDataSet.UpdateMinMaxValues
    end;
    FThreeDTimeList := TimeList;
  finally
    Grid.SelectedLayer := LocalSelectedLayer;
    Grid.SelectedRow := LocalSelectedRow;
    Grid.SelectedColumn := LocalSelectedColumn;
  end;
end;

procedure TPhastModel.DrawHeadObservations(const BitMap: TBitmap32;
  const ZoomBox: TQRbwZoomBox2);
var
  MaxResid: Double;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  MaxResid := 0;
  if StoreHeadObsResults and FHeadObsResults.Visible then
  begin
    FHeadObsResults.CalculateMaxResidual;
    MaxResid := FHeadObsResults.MaxResidual;
  end;
  if LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel.StoreHeadObsResults and ChildModel.HeadObsResults.Visible then
      begin
        ChildModel.HeadObsResults.CalculateMaxResidual;
        if MaxResid < ChildModel.HeadObsResults.MaxResidual then
        begin
          MaxResid := ChildModel.HeadObsResults.MaxResidual;
        end;
      end;
    end;
    if StoreHeadObsResults and FHeadObsResults.Visible then
    begin
      FHeadObsResults.MaxResidual := MaxResid;
    end;
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel.StoreHeadObsResults and ChildModel.HeadObsResults.Visible then
      begin
        ChildModel.HeadObsResults.MaxResidual := MaxResid;
      end;
    end;
  end;
  inherited;
  for ChildIndex := 0 to ChildModels.Count - 1 do
  begin
    ChildModel := ChildModels[ChildIndex].ChildModel;
    ChildModel.DrawHeadObservations(BitMap, ZoomBox);
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

procedure TPhastModel.DrawSfrStreamLinkages(const BitMap: TBitmap32;
  const ZoomBox: TQRbwZoomBox2);
const
  SquareSize = 6;
var
  StreamList: TSfrStreamPlotList;
  LakeList: TLakePlotList;
  StreamNumbers: TIntegerList;
  LakeNumbers: TIntegerList;
  Index: Integer;
  StreamToPlot: TSfrStreamPlot;
  LakeToPlot: TLakePlot;
  StreamIndex: Integer;
  OtherStream: TSfrStreamPlot;
  LakeIndex: Integer;
  UpstreamPoint: TPoint2D;
  Points: array[0..1] of TPoint;
  DownstreamPoint: TPoint2D;
  StreamColor: TColor32;
  DiversionColor: TColor32;
  UnconnectedColor: TColor32;
  DownstreamObject: TScreenObject;
  UpstreamObject: TScreenObject;
  StreamObject: TScreenObject;
begin
  inherited;
  StreamList := TSfrStreamPlotList.Create;
  LakeList := TLakePlotList.Create;
  try
    SfrStreamLinkPlot.GetObjectsToPlot(StreamList, LakeList);
    if StreamList.Count > 0 then
    begin
      StreamColor := Color32(SfrStreamLinkPlot.StreamColor);
      DiversionColor := Color32(SfrStreamLinkPlot.DiversionColor);
      UnconnectedColor := Color32(SfrStreamLinkPlot.UnconnectedColor);

      StreamNumbers := TIntegerList.Create;
      LakeNumbers := TIntegerList.Create;
      try
        for Index := 0 to StreamList.Count - 1 do
        begin
          StreamToPlot := StreamList[Index];
          StreamNumbers.Add(StreamToPlot.Segment);
        end;
        StreamNumbers.Sorted := True;
        for Index := 0 to LakeList.Count - 1 do
        begin
          LakeToPlot := LakeList[Index];
          LakeNumbers.Add(LakeToPlot.LakeId);
        end;
        LakeNumbers.Sorted := True;
        for Index := 0 to StreamList.Count - 1 do
        begin
          StreamToPlot := StreamList[Index];
          if SfrStreamLinkPlot.PlotUnconnected then
          begin
            if StreamToPlot.OutflowSegment = 0 then
            begin
              StreamObject := StreamToPlot.StreamObject as TScreenObject;
              UpstreamPoint := StreamObject.Points[StreamObject.Count-1];
              Points[0].X := ZoomBox.XCoord(UpstreamPoint.x);
              Points[0].Y := ZoomBox.YCoord(UpstreamPoint.y);
              DrawBigRectangle32(BitMap, UnconnectedColor, UnconnectedColor, 1,
                Points[0].X - SquareSize, Points[0].Y - SquareSize,
                Points[0].X + SquareSize, Points[0].Y + SquareSize);
            end;
          end;
          if SfrStreamLinkPlot.PlotStreamConnections then
          begin
            DownstreamObject := nil;
            if StreamToPlot.OutflowSegment > 0 then
            begin
              StreamIndex := StreamNumbers.IndexOf(StreamToPlot.OutflowSegment);
              if StreamIndex >= 0 then
              begin
                OtherStream := StreamList[StreamIndex];
                DownstreamObject := OtherStream.StreamObject as TScreenObject;
                Assert(StreamToPlot.OutflowSegment = OtherStream.Segment)
              end;
            end
            else if StreamToPlot.OutflowSegment < 0 then
            begin
              LakeIndex := LakeNumbers.IndexOf(-StreamToPlot.OutflowSegment);
              if LakeIndex >= 0 then
              begin
                LakeToPlot := LakeList[LakeIndex];
                DownstreamObject := LakeToPlot.LakeObject as TScreenObject;
                Assert(-StreamToPlot.OutflowSegment = LakeToPlot.LakeId)
              end;
            end;
            if DownstreamObject <> nil then
            begin
              StreamObject := StreamToPlot.StreamObject as TScreenObject;
              UpstreamPoint := StreamObject.Points[StreamObject.Count-1];
              Points[0].X := ZoomBox.XCoord(UpstreamPoint.x);
              Points[0].Y := ZoomBox.YCoord(UpstreamPoint.y);

              DownstreamPoint := DownstreamObject.Points[0];
              Points[1].X := ZoomBox.XCoord(DownstreamPoint.x);
              Points[1].Y := ZoomBox.YCoord(DownstreamPoint.y);

              if (Points[0].X = Points[1].X) and (Points[0].Y = Points[1].Y) then
              begin
                DrawBigRectangle32(BitMap, StreamColor, StreamColor, 1,
                  Points[0].X - SquareSize, Points[0].Y - SquareSize,
                  Points[0].X + SquareSize, Points[0].Y + SquareSize);
              end
              else
              begin
                DrawBigPolyline32(BitMap, StreamColor, 2, Points, True);
              end;
            end;
          end;
          if SfrStreamLinkPlot.PlotDiversions then
          begin
            UpstreamObject := nil;
            if StreamToPlot.DiversionSegment > 0 then
            begin
              StreamIndex := StreamNumbers.IndexOf(StreamToPlot.DiversionSegment);
              if StreamIndex >= 0 then
              begin
                OtherStream := StreamList[StreamIndex];
                UpstreamObject := OtherStream.StreamObject as TScreenObject;
                Assert(StreamToPlot.DiversionSegment = OtherStream.Segment)
              end;
            end
            else if StreamToPlot.DiversionSegment < 0 then
            begin
              LakeIndex := LakeNumbers.IndexOf(-StreamToPlot.DiversionSegment);
              if LakeIndex >= 0 then
              begin
                LakeToPlot := LakeList[LakeIndex];
                UpstreamObject := LakeToPlot.LakeObject as TScreenObject;
                Assert(-StreamToPlot.DiversionSegment = LakeToPlot.LakeId)
              end;
            end;
            if UpstreamObject <> nil then
            begin
              UpstreamPoint := UpstreamObject.Points[UpstreamObject.Count-1];
              Points[0].X := ZoomBox.XCoord(UpstreamPoint.x);
              Points[0].Y := ZoomBox.YCoord(UpstreamPoint.y);

              StreamObject := StreamToPlot.StreamObject as TScreenObject;
              DownstreamPoint := StreamObject.Points[0];
              Points[1].X := ZoomBox.XCoord(DownstreamPoint.x);
              Points[1].Y := ZoomBox.YCoord(DownstreamPoint.y);

              if (Points[0].X = Points[1].X) and (Points[0].Y = Points[1].Y) then
              begin
                DrawBigRectangle32(BitMap, DiversionColor, DiversionColor, 1,
                  Points[0].X - SquareSize, Points[0].Y - SquareSize,
                  Points[0].X + SquareSize, Points[0].Y + SquareSize);
              end
              else
              begin
                DrawBigPolyline32(BitMap, DiversionColor, 2, Points, True);
              end;
            end;
          end;
        end;
      finally
        LakeNumbers.Free;
        StreamNumbers.Free;
      end;
    end;
  finally
    LakeList.Free;
    StreamList.Free;
  end;
end;

function TPhastModel.DrnIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.DrnPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.DrnPackage.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.DrobIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.DrobPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.DrobPackage.IsSelected;
      end;
    end;
  end;
end;

function TPhastModel.DrtIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.DrtPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.DrtPackage.IsSelected;
      end;
    end;
  end;
end;

procedure TCustomModel.InvalidateEtsDepthFractions(Sender: TObject);
begin
  ModflowPackages.EtsPackage.InvalidateEtsDepthFractions(Sender);
end;

procedure TCustomModel.InvalidateEtsRateFractions(Sender: TObject);
begin
  ModflowPackages.EtsPackage.InvalidateEtsRateFractions(Sender);
end;

procedure TPhastModel.InvalidateMapping;
begin
  SetLength(FColumnMapping, 0);
  SetLength(FRowMapping, 0);
  SetLength(FLayerMapping, 0);
end;

procedure TCustomModel.InvalidateMfChdEndingHead(Sender: TObject);
begin
  ModflowPackages.ChdBoundary.MfChdEndingHead.Invalidate;
end;

procedure TCustomModel.InvalidateMfChdStartingHead(Sender: TObject);
begin
  ModflowPackages.ChdBoundary.MfChdStartingHead.Invalidate;
end;

procedure TCustomModel.InvalidateMfDrnConductance(Sender: TObject);
begin
  ModflowPackages.DrnPackage.MfDrnConductance.Invalidate;
end;

procedure TCustomModel.InvalidateMfDrnElevation(Sender: TObject);
begin
  ModflowPackages.DrnPackage.MfDrnElevation.Invalidate;
end;

procedure TCustomModel.InvalidateMfDrtConductance(Sender: TObject);
begin
  ModflowPackages.DrtPackage.MfDrtConductance.Invalidate;
end;

procedure TCustomModel.InvalidateMfDrtElevation(Sender: TObject);
begin
  ModflowPackages.DrtPackage.MfDrtElevation.Invalidate;
end;

procedure TCustomModel.InvalidateMfDrtReturnFraction(Sender: TObject);
begin
  ModflowPackages.DrtPackage.MfDrtReturnFraction.Invalidate;
end;

procedure TCustomModel.InvalidateMfEtsEvapDepth(Sender: TObject);
begin
  ModflowPackages.EtsPackage.MfEtsEvapDepth.Invalidate;
end;

procedure TCustomModel.InvalidateMfEtsEvapLayer(Sender: TObject);
begin
  ModflowPackages.EtsPackage.MfEtsEvapLayer.Invalidate;
end;

procedure TCustomModel.InvalidateMfEtsEvapRate(Sender: TObject);
begin
  ModflowPackages.EtsPackage.MfEtsEvapRate.Invalidate;
end;

procedure TCustomModel.InvalidateMfEtsEvapSurface(Sender: TObject);
begin
  ModflowPackages.EtsPackage.MfEtsEvapSurface.Invalidate;
end;

procedure TCustomModel.InvalidateMfEvtEvapDepth(Sender: TObject);
begin
  ModflowPackages.EvtPackage.MfEvtEvapDepth.Invalidate;
end;

procedure TCustomModel.InvalidateMfEvtEvapLayer(Sender: TObject);
begin
  ModflowPackages.EvtPackage.InvalidateMfEvtEvapLayer(Sender);
end;

procedure TCustomModel.InvalidateMfEvtEvapRate(Sender: TObject);
begin
  ModflowPackages.EvtPackage.MfEvtEvapRate.Invalidate;
end;

procedure TCustomModel.InvalidateMfEvtEvapSurface(Sender: TObject);
begin
  ModflowPackages.EvtPackage.MfEvtEvapSurface.Invalidate;
end;

procedure TCustomModel.InvalidateMfGhbBoundaryHead(Sender: TObject);
begin
  ModflowPackages.GhbBoundary.MfGhbBoundaryHead.Invalidate;
end;

procedure TCustomModel.InvalidateMfGhbConductance(Sender: TObject);
begin
  ModflowPackages.GhbBoundary.MfGhbConductance.Invalidate;
end;

procedure TCustomModel.InvalidateMfHobHeads(Sender: TObject);
begin
  MfHobHeads.Invalidate;
end;

procedure TCustomModel.InvalidateMfRchLayer(Sender: TObject);
begin
  ModflowPackages.RchPackage.InvalidateMfRchLayer(Sender);
end;

procedure TCustomModel.InvalidateMfRchRate(Sender: TObject);
begin
  ModflowPackages.RchPackage.MfRchRate.Invalidate;
end;

procedure TCustomModel.InvalidateMfRivBottom(Sender: TObject);
begin
  ModflowPackages.RivPackage.MfRivBottom.Invalidate;
end;

procedure TCustomModel.InvalidateMfRivConductance(Sender: TObject);
begin
  ModflowPackages.RivPackage.MfRivConductance.Invalidate;
end;

procedure TCustomModel.InvalidateMfRivStage(Sender: TObject);
begin
  ModflowPackages.RivPackage.MfRivStage.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrBankRoughness(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrBankRoughness.Invalidate;
end;

procedure TCustomModel.InvalidateMfSfrBrooksCorey(Sender: TObject);
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

procedure TCustomModel.InvalidateMfSfrDownstreamBrooksCorey(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrDownstreamBrooksCorey.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrDownstreamDepth(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrDownstreamDepth.Invalidate;
end;

procedure TCustomModel.InvalidateMfSfrDownstreamElevation(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrDownstreamElevation.Invalidate;
end;

procedure TCustomModel.InvalidateMfSfrDownstreamHydraulicConductivity(
  Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrDownstreamHydraulicConductivity.Invalidate;
end;

procedure TCustomModel.InvalidateMfSfrDownstreamThickness(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrDownstreamThickness.Invalidate;
end;

procedure TCustomModel.InvalidateMfSfrDownstreamUnsatInitialWaterContent(
  Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrDownstreamUnsatInitialWaterContent.Invalidate;
end;

procedure TCustomModel.InvalidateMfSfrDownstreamUnsatKz(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrDownstreamUnsatKz.Invalidate;
end;

procedure TCustomModel.DischargeRoutingUpdate;
var
  LakeIdArray: TDataArray;
  DischargeRoutingArray: TDataArray;
begin
  LakeIdArray := FDataArrayManager.GetDataSetByName(rsLakeID);
  DischargeRoutingArray := FDataArrayManager.GetDataSetByName(StrUzfDischargeRouting);

  if (ModflowPackages <> nil) and
    (ModflowPackages.SfrPackage.IsSelected
    or ModflowPackages.LakPackage.IsSelected) then
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

procedure TCustomModel.DiscretizationChanged;
begin
  case ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast, msModflow, msModflowLGR, msModflowNWT:
      begin
        Grid.GridChanged;
      end;
    {$IFDEF SUTRA}
    msSutra:
      begin
        if (Mesh <> nil) then
        begin
          Mesh.MeshChanged;
        end;
      end;
    {$ENDIF}
    else
       Assert(False);
  end;
end;

function TCustomModel.DMCOEF: TOneDRealArray;
begin
  result := LayerStructure.DMCOEF;
end;

procedure TCustomModel.DrawHeadObservations(const BitMap: TBitmap32;
  const ZoomBox: TQRbwZoomBox2);
begin
  if StoreHeadObsResults and FHeadObsResults.Visible then
  begin
    HeadObsResults.Draw(BitMap, ZoomBox);
  end;
end;

function TPhastModel.De4IsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.De4Package.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.De4Package.IsSelected;
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
                result := FortranFloatToStr((Grid.HighestElevation + Grid.LowestElevation)/2);
              end;
            msModflow, msModflowLGR, msModflowNWT:
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
                    '(' + result + ' + '
                    + LayerGroup.DataArrayName + ')/2';
                end
                else
                begin
                  result :=
                    FortranFloatToStr((Grid.HighestElevation + Grid.LowestElevation)/2);
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
              FortranFloatToStr((Grid.RowPositions[Row]+ Grid.RowPositions[Row+1])/2);
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
            FortranFloatToStr((Grid.ColumnPositions[Column]
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
                result := FortranFloatToStr(Grid.HighestElevation);
              end;
            msModflow, msModflowLGR, msModflowNWT:
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
                    FortranFloatToStr(Grid.HighestElevation);
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
                  FortranFloatToStr(Grid.RowPositions[Row+1]);
              end;
            rdNorthToSouth:
              begin
                result :=
                  FortranFloatToStr(Grid.RowPositions[Row]);
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
                  FortranFloatToStr(Grid.ColumnPositions[Column+1]);
              end;
            cdEastToWest:
              begin
                result :=
                  FortranFloatToStr(Grid.ColumnPositions[Column]);
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
                result := FortranFloatToStr(Grid.LowestElevation);
              end;
            msModflow, msModflowLGR, msModflowNWT:
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
                    FortranFloatToStr(Grid.LowestElevation);
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
                  FortranFloatToStr(Grid.RowPositions[Row]);
              end;
            rdNorthToSouth:
              begin
                result :=
                  FortranFloatToStr(Grid.RowPositions[Row+1]);
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
                  FortranFloatToStr(Grid.ColumnPositions[Column]);
              end;
            cdEastToWest:
              begin
                result :=
                  FortranFloatToStr(Grid.ColumnPositions[Column+1]);
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

function TCustomModel.DefaultModflowOutputFileName: string;
var
  Extension: string;
begin
  GetDefaultOutputFileExtension(Extension);
  if Extension = '' then
  begin
    result := '';
    Exit;
  end;
  result := ChangeFileExt(ModelFileName, Extension);
  result := FixFileName(result);
end;

procedure TCustomModel.InvalidateMfSfrDownstreamUnsaturatedWaterContent(
  Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrDownstreamUnsaturatedWaterContent.Invalidate;
end;

procedure TCustomModel.InvalidateMfSfrDownstreamWidth(Sender: TObject);
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

procedure TCustomModel.InvalidateMfSfrInitialWaterContent(Sender: TObject);
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

procedure TCustomModel.InvalidateMfSfrStreamK(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrStreamK.Invalidate;
end;

procedure TCustomModel.InvalidateMfSfrSaturatedWaterContent(
  Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrSaturatedWaterContent.Invalidate;
end;

procedure TCustomModel.InvalidateMfSfrStreamSlope(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrStreamSlope.Invalidate;
end;

procedure TCustomModel.InvalidateMfSfrStreamThickness(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrStreamThickness.Invalidate;
end;

procedure TCustomModel.InvalidateMfSfrStreamTop(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrStreamTop.Invalidate;
end;

procedure TCustomModel.InvalidateMfSfrUpstreamBrooksCorey(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrUpstreamBrooksCorey.Invalidate;
end;

procedure TPhastModel.InvalidateMfSfrUpstreamDepth(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrUpstreamDepth.Invalidate;
end;

procedure TCustomModel.InvalidateMfSfrUpstreamElevation(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrUpstreamElevation.Invalidate;
end;

procedure TCustomModel.InvalidateMfSfrUpstreamHydraulicConductivity(
  Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrUpstreamHydraulicConductivity.Invalidate;
end;

procedure TCustomModel.InvalidateMfSfrUpstreamThickness(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrUpstreamThickness.Invalidate;
end;

procedure TCustomModel.InvalidateMfSfrUpstreamUnsatInitialWaterContent(
  Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrUpstreamUnsatInitialWaterContent.Invalidate;
end;

procedure TCustomModel.InvalidateMfSfrUpstreamUnsatKz(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrUpstreamUnsatKz.Invalidate;
end;

procedure TCustomModel.InvalidateMfSfrUpstreamUnsaturatedWaterContent(
  Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrUpstreamUnsaturatedWaterContent.Invalidate;
end;

procedure TCustomModel.InvalidateMfSfrUpstreamWidth(Sender: TObject);
begin
  ModflowPackages.SfrPackage.MfSfrUpstreamWidth.Invalidate;
end;

procedure TCustomModel.InvalidateMfSfrVerticalUnsatK(Sender: TObject);
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

procedure TCustomModel.InvalidateMfUzfEtDemand(Sender: TObject);
begin
  ModflowPackages.UzfPackage.MfUzfEtDemand.Invalidate;
end;

procedure TCustomModel.InvalidateMfUzfExtinctionDepth(Sender: TObject);
begin
  ModflowPackages.UzfPackage.MfUzfExtinctionDepth.Invalidate;
end;

procedure TCustomModel.InvalidateMfUzfInfiltration(Sender: TObject);
begin
  ModflowPackages.UzfPackage.MfUzfInfiltration.Invalidate;
end;

procedure TCustomModel.InvalidateMfUzfWaterContent(Sender: TObject);
begin
  ModflowPackages.UzfPackage.MfUzfWaterContent.Invalidate;
end;

function TCustomModel.CountStepsInMt3dExport: Integer;
begin
  result := 1;
  if ModflowPackages.Mt3dmsAdvection.IsSelected then
  begin
    Inc(result);
  end;
  if ModflowPackages.Mt3dmsDispersion.IsSelected then
  begin
    Inc(result);
  end;
  if ModflowPackages.Mt3dmsSourceSink.IsSelected then
  begin
    Inc(result);
  end;
  if ModflowPackages.Mt3dmsChemReact.IsSelected then
  begin
    Inc(result);
  end;
  if ModflowPackages.Mt3dmsGCGSolver.IsSelected then
  begin
    Inc(result);
  end;
  if ModflowPackages.Mt3dmsTransObs.IsSelected then
  begin
    Inc(result);
  end;
end;

function TCustomModel.CountStepsInExport: Integer;
var
  HufParam: THufUsedParameter;
  HGU: THydrogeologicUnit;
  HufUnitIndex: Integer;
  SteadyParam: TModflowSteadyParameter;
  PIndex: Integer;
  ZoneUsed: Boolean;
  MultipliersUsed: Boolean;
begin
  // The following tasks are always required.
  // 1. Full Stress periods,
  // 2. Discretization,
  // 3. Basic,
  // 4. Output Control,
  // 5. Zone Arrays,
  // 6. Multiplier Arrays
  result := 3;
  result := result + ModflowPackages.SelectedModflowPackageCount;
  if ModflowPackages.SfrPackage.IsSelected or ModflowPackages.LakPackage.IsSelected then
  begin
    // gages
    Inc(result);
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
    if (ModflowPackages.LpfPackage.IsSelected
      or ModflowPackages.UpwPackage.IsSelected) then
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
    Inc(result);
  end;
  if ZoneUsed then
  begin
    Inc(result);
  end;
end;

function TCustomModel.PrepareModflowFullStressPeriods(ShowWarning: boolean): Boolean;
var
  StressPeriod: TModflowStressPeriod;
  Index: Integer;
  StepCount: Integer;
begin
  result := True;
  UpdateModflowFullStressPeriods;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    result := False;
    Exit;
  end;
  frmProgressMM.StepIt;
  if ShowWarning then
  begin
    StepCount := 0;
    for Index := 0 to ModflowFullStressPeriods.Count - 1 do
    begin
      StressPeriod := ModflowFullStressPeriods.Items[Index];
      StepCount := StepCount + StressPeriod.NumberOfSteps;
    end;
    if StepCount > 1000 then
    begin
      if MessageDlg(Format(StrYourModelHasSTi, [StepCount]),
        mtWarning, [mbYes, mbNo], 0) <> mrYes then
      begin
        result := False;
      end;
    end;
  end;
end;

procedure TPhastModel.InvalidateMfSfrSegmentReachAndIcalc(Sender: TObject);
begin
  ModflowPackages.SfrPackage.
    InvalidateMfSfrSegmentReachAndIcalc(Sender);
end;

procedure TCustomModel.InvalidateMfWellPumpage(Sender: TObject);
begin
  ModflowPackages.WelPackage.MfWellPumpage.Invalidate;
end;

procedure TCustomModel.InvalidateMt3dmsChemSources(Sender: TObject);
begin
  // this needs to change.
  { TODO 1 -cMT3DMS : This needs to change }
//  Assert(False);
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

procedure TCustomModel.InvalidateScreenObjects;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  BeginGridChange;
  try
    for Index := 0 to ScreenObjectCount - 1 do
    begin
      AScreenObject := ScreenObjects[Index];
      AScreenObject.Invalidate;
    end;
  finally
    EndGridChange;
  end;
end;

function TCustomModel.IsLayerSimulated(const LayerID: integer): boolean;
begin
  result := LayerStructure.IsLayerSimulated(LayerID);
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

procedure TPhastModel.SetCombinedDisplayColumn(const Value: integer);
var
  LocalCombinedCount: Integer;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  NewPosition: Integer;
begin
  FCombinedDisplayColumn := Value;
  if FCombinedDisplayColumn < 0 then
  begin
    FCombinedDisplayColumn := 0;
  end;
  if ModelSelection = msPhast then
  begin
    Grid.DisplayColumn := FCombinedDisplayColumn;
    FCombinedDisplayColumn := Grid.DisplayColumn;
    Exit;
  end;
  if (Grid <> nil) and (Grid.ColumnCount > 0)
    and (Grid.RowCount > 0)and (Grid.LayerCount > 0) then
  begin

    UpdateMapping;
    LocalCombinedCount := Length(FColumnMapping);
    if FCombinedDisplayColumn > LocalCombinedCount then
    begin
      FCombinedDisplayColumn := LocalCombinedCount;
    end;
    if FCombinedDisplayColumn >= 0 then
    begin
      if FCombinedDisplayColumn < LocalCombinedCount then
      begin
        ModflowGrid.DisplayColumn :=
          FColumnMapping[FCombinedDisplayColumn].ParentPostion;
      end
      else
      begin
        ModflowGrid.DisplayColumn :=
          FColumnMapping[FCombinedDisplayColumn-1].ParentPostion+1;
      end;
      for ChildIndex := 0 to ChildModels.Count - 1 do
      begin
        ChildModel := ChildModels[ChildIndex].ChildModel;
        if FCombinedDisplayColumn < LocalCombinedCount then
        begin
          NewPosition := FColumnMapping[FCombinedDisplayColumn].
            ChildPositions[ChildIndex];
        end
        else
        begin
          NewPosition := FColumnMapping[FCombinedDisplayColumn-1].
            ChildPositions[ChildIndex]+1;
        end;
        ChildModel.ModflowGrid.DisplayColumn := NewPosition;
      end;
    end;
  end;
end;

function TPhastModel.CombinedLayerSimulated(ALayer: Integer): boolean;
var
  LocalCombinedCount: Integer;
begin
  if ALayer < 0 then
  begin
    ALayer := 0;
  end;
  if ModelSelection = msPhast then
  begin
    result := True;
    Exit;
  end;
  if (Grid <> nil) and (Grid.ColumnCount > 0)
    and (Grid.RowCount > 0)and (Grid.LayerCount > 0) then
  begin
    UpdateMapping;
    LocalCombinedCount := Length(FLayerMapping);
    if ALayer > LocalCombinedCount then
    begin
      ALayer := LocalCombinedCount;
    end;
    Assert(ALayer >= 0);
    if ALayer < LocalCombinedCount then
    begin
      ALayer :=
        FLayerMapping[ALayer].ParentPostion;
    end
    else
    begin
      ALayer :=
        FLayerMapping[ALayer-1].ParentPostion+1;
    end;
    result := LayerStructure.IsLayerSimulated(ALayer)
  end
  else
  begin
    result := True
  end;
end;

procedure TPhastModel.SetCombinedDisplayLayer(const Value: integer);
var
  LocalCombinedCount: Integer;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  NewPosition: Integer;
begin
  FCombinedDisplayLayer := Value;
  if FCombinedDisplayLayer < 0 then
  begin
    FCombinedDisplayLayer := 0;
  end;
  if ModelSelection = msPhast then
  begin
    Grid.DisplayLayer := FCombinedDisplayLayer;
    FCombinedDisplayLayer := Grid.DisplayLayer;
    Exit;
  end;
  if (Grid <> nil) and (Grid.ColumnCount > 0)
    and (Grid.RowCount > 0)and (Grid.LayerCount > 0) then
  begin
    UpdateMapping;
    LocalCombinedCount := Length(FLayerMapping);
    if FCombinedDisplayLayer > LocalCombinedCount then
    begin
      FCombinedDisplayLayer := LocalCombinedCount;
    end;
    if FCombinedDisplayLayer >= 0 then
    begin
      if FCombinedDisplayLayer < LocalCombinedCount then
      begin
        ModflowGrid.DisplayLayer :=
          FLayerMapping[FCombinedDisplayLayer].ParentPostion;
      end
      else
      begin
        ModflowGrid.DisplayLayer :=
          FLayerMapping[FCombinedDisplayLayer-1].ParentPostion+1;
      end;
      for ChildIndex := 0 to ChildModels.Count - 1 do
      begin
        ChildModel := ChildModels[ChildIndex].ChildModel;
        if FCombinedDisplayLayer < LocalCombinedCount then
        begin
          NewPosition := FLayerMapping[FCombinedDisplayLayer].
            ChildPositions[ChildIndex];
        end
        else
        begin
          NewPosition := FLayerMapping[FCombinedDisplayLayer-1].
            ChildPositions[ChildIndex]+1;
        end;
        ChildModel.ModflowGrid.DisplayLayer := NewPosition;
      end;
    end;
  end;
end;

procedure TPhastModel.SetCombinedDisplayRow(const Value: integer);
var
  LocalCombinedCount: Integer;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  NewPosition: Integer;
begin
  FCombinedDisplayRow := Value;
  if FCombinedDisplayRow < 0 then
  begin
    FCombinedDisplayRow := 0;
  end;
  if ModelSelection = msPhast then
  begin
    Grid.DisplayRow := FCombinedDisplayRow;
    FCombinedDisplayRow := Grid.DisplayRow;
    Exit;
  end;
  if (Grid <> nil) and (Grid.ColumnCount > 0)
    and (Grid.RowCount > 0)and (Grid.LayerCount > 0) then
  begin
    UpdateMapping;
    LocalCombinedCount := Length(FRowMapping);
    if FCombinedDisplayRow > LocalCombinedCount then
    begin
      FCombinedDisplayRow := LocalCombinedCount;
    end;
    if FCombinedDisplayRow >= 0 then
    begin
      if FCombinedDisplayRow < LocalCombinedCount then
      begin
        ModflowGrid.DisplayRow :=
          FRowMapping[FCombinedDisplayRow].ParentPostion;
      end
      else
      begin
        ModflowGrid.DisplayRow :=
          FRowMapping[FCombinedDisplayRow-1].ParentPostion+1;
      end;
      for ChildIndex := 0 to ChildModels.Count - 1 do
      begin
        ChildModel := ChildModels[ChildIndex].ChildModel;
        if FCombinedDisplayRow < LocalCombinedCount then
        begin
          NewPosition := FRowMapping[FCombinedDisplayRow].
            ChildPositions[ChildIndex];
        end
        else
        begin
          NewPosition := FRowMapping[FCombinedDisplayRow-1].
            ChildPositions[ChildIndex]+1;
        end;
        ChildModel.ModflowGrid.DisplayRow := NewPosition;
      end;
    end;
  end;
end;

procedure TPhastModel.SetContourFont(const Value: TFont);
begin
  FContourFont.Assign(Value);
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

function TPhastModel.PackageIsSelected(APackage: TObject): Boolean;
begin
  result := False;
  if APackage = frmGoPhast.PhastModel.ModflowPackages.ChdBoundary then
  begin
    result := ChdIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.GhbBoundary then
  begin
    result := GhbIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.GhbBoundary then
  begin
    result := GhbIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.LpfPackage then
  begin
    result := LpfIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.PcgPackage then
  begin
    result := PcgIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.WelPackage then
  begin
    result := WelIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.RivPackage then
  begin
    result := RivIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.DrnPackage then
  begin
    result := DrnIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.DrtPackage then
  begin
    result := DrtIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.RchPackage then
  begin
    result := RchIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.EvtPackage then
  begin
    result := EvtIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.EtsPackage then
  begin
    result := EtsIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.ResPackage then
  begin
    result := ResIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.LakPackage then
  begin
    result := LakIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.SfrPackage then
  begin
    result := SfrIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.UzfPackage then
  begin
    result := UzfIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.GmgPackage then
  begin
    result := GmgIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.SipPackage then
  begin
    result := SipIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.De4Package then
  begin
    result := De4IsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.HobPackage then
  begin
    result := HobIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.HfbPackage then
  begin
    result := HfbIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.ModPath then
  begin
    result := ModPathIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.ChobPackage then
  begin
    result := ChobIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.DrobPackage then
  begin
    result := DrobIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.GbobPackage then
  begin
    result := GbobIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.RvobPackage then
  begin
    result := RvobIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.HufPackage then
  begin
    result := HufIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.Mnw2Package then
  begin
    result := Mnw2IsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.BcfPackage then
  begin
    result := BcfIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.SubPackage then
  begin
    result := SubIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.ZoneBudget then
  begin
    result := ZoneBudgetIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.SwtPackage then
  begin
    result := SwtIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.HydmodPackage then
  begin
    result := HydmodIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.UpwPackage then
  begin
    result := UpwIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.Mt3dmsTransObs then
  begin
    result := TobIsSelected;
  end
  else if APackage = frmGoPhast.PhastModel.ModflowPackages.Mt3dmsSourceSink then
  begin
    result := SsmIsSelected;
  end
  else
  begin
    Assert(False);
  end;
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

function TPhastModel.PcgIsSelected: Boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  result := ModflowPackages.PcgPackage.IsSelected;
  if frmGoPhast.PhastModel.LgrUsed then
  begin
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        result := result or ChildModel.ModflowPackages.PcgPackage.IsSelected;
      end;
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
    ModPathLocationVersion6 := SourceLocations.ModPathLocationVersion6;
    ModelMonitorLocation := SourceLocations.ModelMonitorLocation;
    PhastLocation := SourceLocations.PhastLocation;
    ZoneBudgetLocation := SourceLocations.ZoneBudgetLocation;
    ModelMateLocation := SourceLocations.ModelMateLocation;
    ModflowLgrLocation := SourceLocations.ModflowLgrLocation;
    ModflowNwtLocation := SourceLocations.ModflowNwtLocation;
    Mt3dmsLocation := SourceLocations.Mt3dmsLocation;
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
  PhastLocation := DefaultPhastPath;
  ZoneBudgetLocation := StrZoneBudgetDefaultPath;
  ModflowLgrLocation := strModflowLgrDefaultPath;
  ModflowNwtLocation := strModflowNwtDefaultPath;
  Mt3dmsLocation := strMt3dmsDefaultPath;
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
  ModPathLocationVersion6 := IniFile.ReadString(StrProgramLocations, StrMODPATHVersion6,
    StrMpathDefaultPathVersion6);
  PhastLocation := IniFile.ReadString(StrProgramLocations, StrPHAST,
    DefaultPhastPath);
  ZoneBudgetLocation := IniFile.ReadString(StrProgramLocations, StrZonebudget,
    StrZoneBudgetDefaultPath);
  ModelMateLocation := IniFile.ReadString(StrProgramLocations, StrModelMate,
    StrModelMateDefaultPath);
  ModflowLgrLocation := IniFile.ReadString(StrProgramLocations, strModflowLgr,
    strModflowLgrDefaultPath);
  ModflowNwtLocation := IniFile.ReadString(StrProgramLocations, strModflowNWT,
    strModflowNwtDefaultPath);
  Mt3dmsLocation := IniFile.ReadString(StrProgramLocations, StrMT3DMS,
    strMt3dmsDefaultPath);

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

procedure TProgramLocations.SetModflowLgrLocation(const Value: string);
begin
  FModflowLgrLocation := RemoveQuotes(Value);
end;

procedure TProgramLocations.SetModflowLocation(const Value: string);
begin
  FModflowLocation := RemoveQuotes(Value);
end;

procedure TProgramLocations.SetModflowNwtLocation(const Value: string);
begin
  FModflowNwtLocation := RemoveQuotes(Value);
end;

procedure TProgramLocations.SetModPathLocation(const Value: string);
begin
  FModPathLocation := RemoveQuotes(Value);
end;

procedure TProgramLocations.SetModPathLocationV6(const Value: string);
begin
  FModPathLocationV6 := RemoveQuotes(Value);
end;

procedure TProgramLocations.SetMt3dmsLocation(const Value: string);
begin
  FMt3dmsLocation := RemoveQuotes(Value);
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
  IniFile.WriteString(StrProgramLocations, StrMODPATHVersion6, ModPathLocationVersion6);
  IniFile.WriteString(StrProgramLocations, StrModelMonitor, ModelMonitorLocation);
  IniFile.WriteString(StrProgramLocations, StrPHAST, PhastLocation);
  IniFile.WriteString(StrProgramLocations, StrZonebudget, ZoneBudgetLocation);
  IniFile.WriteString(StrProgramLocations, StrModelMate, ModelMateLocation);
  IniFile.WriteString(StrProgramLocations, strModflowLgr, ModflowLgrLocation);
  IniFile.WriteString(StrProgramLocations, strModflowNWT, ModflowNwtLocation);
  IniFile.WriteString(StrProgramLocations, StrMT3DMS, Mt3dmsLocation);
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
  result := FDataArray.DisplayName;
end;

{ TLookUpList }

constructor TLookUpList.Create;
begin
  inherited;
  FLastIndex := -1;
end;

constructor TCustomModel.Create(AnOwner: TComponent);
begin
  inherited;
  FGages := TStringList.Create;
  FHfbDisplayer:= THfbDisplayer.Create(self);
  FHfbDisplayer.OnNeedToUpdate := UpdateHfb;

  FModflowOptions := TModflowOptions.Create(self);
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

  FModflowNameFileLines := TStringList.Create;
  FBatchFileAdditionsBeforeModel := TStringList.Create;
  FBatchFileAdditionsAfterModel := TStringList.Create;
  FModflowPackages := TModflowPackages.Create(self);
  FModflowPackages.LpfPackage.IsSelected := True;
  FModflowGrid := TModflowGrid.Create(self);

  FHeadFluxObservations := TFluxObservationGroups.Create(self);
  FRiverObservations := TFluxObservationGroups.Create(self);
  FDrainObservations := TFluxObservationGroups.Create(self);
  FGhbObservations := TFluxObservationGroups.Create(self);

  FHeadFluxObservations.FluxObservationType := fotHead;
  FRiverObservations.FluxObservationType := fotRiver;
  FDrainObservations.FluxObservationType := fotDrain;
  FGhbObservations.FluxObservationType := fotGHB;

  FMt3dmsHeadMassFluxObservations := TMt3dmsFluxObservationGroups.Create(self);
  FMt3dmsHeadMassFluxObservations.FluxObservationType := mfotHead;

  FMt3dmsWellMassFluxObservations := TMt3dmsFluxObservationGroups.Create(self);
  FMt3dmsWellMassFluxObservations.FluxObservationType := mfotWell;

  FMt3dmsMassLoadingMassFluxObservations := TMt3dmsFluxObservationGroups.Create(self);
  FMt3dmsMassLoadingMassFluxObservations.FluxObservationType := mfotMassLoading;

  FMt3dmsGhbMassFluxObservations := TMt3dmsFluxObservationGroups.Create(self);
  FMt3dmsGhbMassFluxObservations.FluxObservationType := mfotGHB;

  FMt3dmsRivMassFluxObservations := TMt3dmsFluxObservationGroups.Create(self);
  FMt3dmsRivMassFluxObservations.FluxObservationType := mfotRiver;

  FMt3dmsResMassFluxObservations := TMt3dmsFluxObservationGroups.Create(self);
  FMt3dmsResMassFluxObservations.FluxObservationType := mfotReservoir;

  FMt3dmsRchMassFluxObservations := TMt3dmsFluxObservationGroups.Create(self);
  FMt3dmsRchMassFluxObservations.FluxObservationType := mfotRecharge;

  FMt3dmsDrtMassFluxObservations := TMt3dmsFluxObservationGroups.Create(self);
  FMt3dmsDrtMassFluxObservations.FluxObservationType := mfotDRT;

  FMt3dmsEtsMassFluxObservations := TMt3dmsFluxObservationGroups.Create(self);
  FMt3dmsEtsMassFluxObservations.FluxObservationType := mfotETS;

  FMt3dmsEvtMassFluxObservations := TMt3dmsFluxObservationGroups.Create(self);
  FMt3dmsEvtMassFluxObservations.FluxObservationType := mfotEVT;

  FMt3dmsDrnMassFluxObservations := TMt3dmsFluxObservationGroups.Create(self);
  FMt3dmsDrnMassFluxObservations.FluxObservationType := mfotDrain;

  FMt3dmsLakMassFluxObservations := TMt3dmsFluxObservationGroups.Create(self);
  FMt3dmsLakMassFluxObservations.FluxObservationType := mfotLake;

  FHydrogeologicUnits := THydrogeologicUnits.Create(self);
  FFilesToArchive := TStringList.Create;
  FModelInputFiles := TStringList.Create;
  FModflowWettingOptions := TWettingOptions.Create(Self);
  FGlobalVariables := TGlobalVariables.Create(self);

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

  FSutraOptions := TSutraOptions.Create(self);

  FDataArrayManager.DefinePackageDataArrays;
  CreateModflowDisplayTimeLists;
end;

function TCustomModel.DataSetLayerToModflowLayer(
  DataSetLayer: integer): integer;
begin
  result := LayerStructure.DataSetLayerToModflowLayer(DataSetLayer);
end;

destructor TCustomModel.Destroy;
begin
//  FChangedDataArrayNames.Free;
//  FBoundaryDataSets.Free;
//  FDataSets.Free;
//  FDataSetsToCache.Free;

  FSutraMesh.Free;
  FSutraOptions.Free;
  FModflowWettingOptions.Free;
  FFilesToArchive.Free;
  FModelInputFiles.Free;
  FGrid := nil;
  FModflowGrid.Free;
  FModflowPackages.Free;
  FBatchFileAdditionsAfterModel.Free;
  FBatchFileAdditionsBeforeModel.Free;
  FModflowNameFileLines.Free;

  FMt3dmsHeadMassFluxObservations.Free;
  FMt3dmsWellMassFluxObservations.Free;
  FMt3dmsMassLoadingMassFluxObservations.Free;
  FMt3dmsGhbMassFluxObservations.Free;
  FMt3dmsRivMassFluxObservations.Free;
  FMt3dmsResMassFluxObservations.Free;
  FMt3dmsRchMassFluxObservations.Free;
  FMt3dmsDrtMassFluxObservations.Free;
  FMt3dmsEtsMassFluxObservations.Free;
  FMt3dmsEvtMassFluxObservations.Free;
  FMt3dmsDrnMassFluxObservations.Free;
  FMt3dmsLakMassFluxObservations.Free;

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
  FModflowOptions.Free;
  FHeadObsResults.Free;
  FGages.Free;
  FPathLine.Free;
  FTimeSeries.Free;
  FEndPoints.Free;
  inherited;
end;

procedure TCustomModel.EndGridChange;
begin
  if Grid <> nil then
  begin
    Grid.EndGridChange;
  end;
end;

function TCustomModel.TestModpathOK: Boolean;
begin
  result := True;
  if (ModflowOutputControl.SaveCellFlows = csfNone)
    or (ModflowOutputControl.BudgetFrequency <> 1)
    or (ModflowOutputControl.BudgetFrequencyChoice <> fcTimeSteps)
    or (not ModflowOutputControl.HeadOC.SaveInExternalFile)
    or (ModflowOutputControl.HeadOC.Frequency <> 1)
    or (ModflowOutputControl.HeadOC.FrequencyChoice <> fcTimeSteps) then
  begin
    Beep;
    if (MessageDlg(StrMODPATHRequiresTha,
      mtWarning, [mbYes, mbNo], 0, mbNo) <> mrYes) then
    begin
      frmGoPhast.miOutputControlClick(nil);
      result := false;
    end;
  end;
end;

function TCustomModel.TRPT: TOneDRealArray;
begin
  result := LayerStructure.TRPT;
end;

function TCustomModel.TRPV: TOneDRealArray;
begin
  result := LayerStructure.TRPV;
end;

function TCustomModel.EquilibriumPhasesUsed(Sender: TObject): boolean;
begin
  result := ChemistryUsed(Sender) and ChemistryOptions.UseEquilibriumPhases;
end;

procedure TCustomModel.SetModflowGrid(const Value: TModflowGrid);
begin
  FModflowGrid.Assign(Value);
  Invalidate;
end;

procedure TCustomModel.SetModflowLocation(const Value: string);
begin
  case ModelSelection of
    msModflow:
      ProgramLocations.ModflowLocation := Value;
    msModflowLGR:
      ProgramLocations.ModflowLgrLocation := Value;
    msModflowNWT:
      ProgramLocations.ModflowNwtLocation := Value;
    else Assert(False);
  end;
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

procedure TCustomModel.SetModflowOptions(const Value: TModflowOptions);
begin
  if Value <> FModflowOptions then
  begin
    FModflowOptions.Assign(Value);
  end;
end;

procedure TCustomModel.SetHeadFluxObservations(const Value: TFluxObservationGroups);
begin
  FHeadFluxObservations.Assign(Value);
end;

procedure TCustomModel.CreateHeadObsResults;
begin
  if FHeadObsResults = nil then
  begin
    FHeadObsResults := THeadObsCollection.Create(self);
  end;
end;

procedure TCustomModel.SetHeadObsResults(const Value: THeadObsCollection);
begin
  CreateHeadObsResults;
  FHeadObsResults.Assign(Value);
end;

procedure TCustomModel.SetRiverObservations(const Value: TFluxObservationGroups);
begin
  FRiverObservations.Assign(Value);
end;

procedure TCustomModel.SetDisplayColumn(const Value: integer);
begin
  Grid.DisplayColumn := Value;
end;

procedure TCustomModel.SetDisplayLayer(const Value: integer);
begin
  Grid.DisplayLayer := Value;
end;

procedure TCustomModel.SetDisplayRow(const Value: integer);
begin
  Grid.DisplayRow := Value;
end;

procedure TCustomModel.SetSelectedColumn(const Value: integer);
begin
  Grid.SelectedColumn := Value;
end;

procedure TCustomModel.SetSelectedLayer(const Value: integer);
begin
  case ModelSelection of
    msPhast, msModflow, msModflowLGR, msModflowNWT:
      begin
        Grid.SelectedLayer:= Value;
      end;
    {$IFDEF SUTRA}
    msSutra:
      begin
        if Mesh <> nil then
        begin
          Mesh.SelectedLayer := Value;
        end;
      end
    {$ENDIF}
  else
    Assert(False);
  end;
end;

procedure TCustomModel.SetSelectedRow(const Value: integer);
begin
  Grid.SelectedRow := Value;
end;

procedure TCustomModel.SetSideDataSet(const Value: TDataArray);
begin
  case ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast, msModflow, msModflowLGR, msModflowNWT:
      begin
        Grid.SideDataSet := Value;
      end;
    {$IFDEF SUTRA}
    msSutra:
      begin
        // do nothing
      end;
    {$ENDIF}
    else
       Assert(False);
  end;
end;

procedure TCustomModel.SetSideTimeList(const Value: TCustomTimeList);
begin
  FSideTimeList := Value;
end;

procedure TCustomModel.SetSutraMesh(const Value: TSutraMesh3D);
begin
  if FSutraMesh = nil then
  begin
    FSutraMesh := TSutraMesh3D.Create(self);
  end;
  FSutraMesh.Assign(Value);
end;

procedure TCustomModel.SetSutraOptions(const Value: TSutraOptions);
begin
  FSutraOptions.Assign(Value);
end;

procedure TCustomModel.SetTopDataSet(const Value: TDataArray);
begin
  case ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast, msModflow, msModflowLGR, msModflowNWT:
      begin
        Grid.TopDataSet := Value;
      end;
    {$IFDEF SUTRA}
    msSutra:
      begin
        if (Mesh <> nil) then
        begin
          Mesh.TopDataSet := Value;
        end;
      end;
    {$ENDIF}
    else
       Assert(False);
  end;
end;

procedure TCustomModel.SetTopTimeList(const Value: TCustomTimeList);
begin
  FTopTimeList := Value;
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

procedure TCustomModel.SetFrontDataSet(const Value: TDataArray);
begin
  case ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast, msModflow, msModflowLGR, msModflowNWT:
      begin
        Grid.FrontDataSet := Value;
      end;
    {$IFDEF SUTRA}
    msSutra:
      begin
        // do nothing
      end;
    {$ENDIF}
    else
       Assert(False);
  end;
end;

procedure TCustomModel.SetFrontTimeList(const Value: TCustomTimeList);
begin
  FFrontTimeList := Value;
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

procedure TCustomModel.SetMt3dmsDrnMassFluxObservations(
  const Value: TMt3dmsFluxObservationGroups);
begin
  FMt3dmsDrnMassFluxObservations.Assign(Value);
end;

procedure TCustomModel.SetMt3dmsDrtMassFluxObservations(
  const Value: TMt3dmsFluxObservationGroups);
begin
  FMt3dmsDrtMassFluxObservations.Assign(Value);
end;

procedure TCustomModel.SetMt3dmsEtsMassFluxObservations(
  const Value: TMt3dmsFluxObservationGroups);
begin
  FMt3dmsEtsMassFluxObservations.Assign(Value);
end;

procedure TCustomModel.SetMt3dmsEvtMassFluxObservations(
  const Value: TMt3dmsFluxObservationGroups);
begin
  FMt3dmsEvtMassFluxObservations.Assign(Value);
end;

procedure TCustomModel.SetMt3dmsGhbMassFluxObservations(
  const Value: TMt3dmsFluxObservationGroups);
begin
  FMt3dmsGhbMassFluxObservations.Assign(Value);
end;

procedure TCustomModel.SetMt3dmsHeadMassFluxObservations(
  const Value: TMt3dmsFluxObservationGroups);
begin
  FMt3dmsHeadMassFluxObservations.Assign(Value);
end;

procedure TCustomModel.SetMt3dmsLakMassFluxObservations(
  const Value: TMt3dmsFluxObservationGroups);
begin
  FMt3dmsLakMassFluxObservations.Assign(Value);
end;

procedure TCustomModel.SetMt3dmsMassLoadingMassFluxObservations(
  const Value: TMt3dmsFluxObservationGroups);
begin
  FMt3dmsMassLoadingMassFluxObservations.Assign(Value);
end;

procedure TCustomModel.SetMt3dmsRchMassFluxObservations(
  const Value: TMt3dmsFluxObservationGroups);
begin
  FMt3dmsRchMassFluxObservations.Assign(Value);
end;

procedure TCustomModel.SetMt3dmsResMassFluxObservations(
  const Value: TMt3dmsFluxObservationGroups);
begin
  FMt3dmsResMassFluxObservations.Assign(Value);
end;

procedure TCustomModel.SetMt3dmsRivMassFluxObservations(
  const Value: TMt3dmsFluxObservationGroups);
begin
  FMt3dmsRivMassFluxObservations.Assign(Value);
end;

procedure TCustomModel.SetMt3dmsWellMassFluxObservations(
  const Value: TMt3dmsFluxObservationGroups);
begin
  FMt3dmsWellMassFluxObservations.Assign(Value);
end;

procedure TCustomModel.SetNameFileWriter(const Value: TObject);
begin
  if Assigned(Value) then
  begin
    Assert(Value is TCustomNameFileWriter);
  end;
  FNameFileWriter := Value;
end;

procedure TCustomModel.AddModelInputFile(const FileName: string);
begin
  if ModelInputFiles.IndexOf(FileName) < 0 then
  begin
    ModelInputFiles.Add(FileName);
    Invalidate;
  end;
end;

function TCustomModel.AddDataSet(const DataSet: TDataArray): Integer;
begin
  result := DataArrayManager.AddDataSet(DataSet);
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
var
  Index: Integer;
  DataSet: TDataArray;
begin
  if FSutraMesh <> nil then
  begin
    FSutraMesh.Clear;
  end;
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
  AllObserversStopTalking;
  FModflowOptions.Clear;
  FDataArrayManager.ClearDataSetsToCache;
  ClearAllTimeLists;
  FreeAndNil(FPathline);
  FreeAndNil(FTimeSeries);
  FreeAndNil(FEndPoints);
end;

procedure TCustomModel.GenerateSutraMesh(var ErrorMessage: string);
var
  List: TList;
  ScreenObjectIndex: integer;
  ScreenObject: TScreenObject;
  Area: double;
  BiggestIndex: Integer;
  ObjectArea: Real;
  MeshCreator: TQuadMeshCreator;
  SectionIndex: Integer;
  NodeIndex: Integer;
  ABoundary: TBoundary;
  ANode: TNode;
  FirstNode: TNode;
  SutraNode: TSutraNode2D;
  AdjustIndex: Integer;
  MeshNode: INode;
  ElementIndex: Integer;
  SutraElement: TSutraElement2D;
  MeshElement: IElement;
  StartIndex: Integer;
  EndIndex: Integer;
begin
  ErrorMessage := '';
  List := TList.Create;
  try
    for ScreenObjectIndex := 0 to ScreenObjectCount - 1 do
    begin
      ScreenObject := ScreenObjects[ScreenObjectIndex];
      if ScreenObject.CellSizeUsed and (ScreenObject.ViewDirection = vdTop)
        and not ScreenObject.Deleted then
      begin
        List.Add(ScreenObject);
      end;
    end;
    if List.Count = 0 then
    begin
      ErrorMessage := StrNoObjects;
      Exit;
    end;
    Area := 0;
    BiggestIndex := -1;
    for ScreenObjectIndex := 0 to List.Count - 1 do
    begin
      ScreenObject := List[ScreenObjectIndex];
      ObjectArea := ScreenObject.ScreenObjectArea;
      if ObjectArea > Area then
      begin
        Area := ObjectArea;
        BiggestIndex := ScreenObjectIndex;
      end;
    end;
    if BiggestIndex < 0 then
    begin
      ErrorMessage := StrNoPolygons;
      Exit;
    end;

    if BiggestIndex > 0 then
    begin
      List.Exchange(0, BiggestIndex);
    end;

    MeshCreator := TQuadMeshCreator.Create;
    try
      MeshCreator.NodeAdjustmentMethod := namGiuliani;
      for ScreenObjectIndex := 0 to List.Count - 1 do
      begin
        ScreenObject := List[ScreenObjectIndex];
        for SectionIndex := 0 to ScreenObject.SectionCount - 1 do
        begin
          ABoundary := MeshCreator.AddBoundary(ScreenObject.CellSize);
          FirstNode := nil;
          StartIndex := ScreenObject.SectionStart[SectionIndex];
          EndIndex := ScreenObject.SectionEnd[SectionIndex];
          if ScreenObject.SectionClosed[SectionIndex] then
          begin
            Dec(EndIndex);
          end;
          for NodeIndex := StartIndex to EndIndex do
          begin
            ANode := TNode.Create(MeshCreator, ScreenObject.CellSize);
            ANode.Location := ScreenObject.Points[NodeIndex];
            if FirstNode = nil then
            begin
              FirstNode := ANode;
            end;
            ABoundary.AddNode(ANode);
          end;
          if ScreenObject.SectionClosed[SectionIndex] then
          begin
            ABoundary.AddNode(FirstNode);
          end;
        end;
      end;

      MeshCreator.GenerateMesh;
      for AdjustIndex := 1 to 10 do
      begin
        MeshCreator.AdjustNodes;
      end;

      if FSutraMesh = nil then
      begin
        FSutraMesh := TSutraMesh3D.Create(self);
        if Self is TPhastModel then
        begin
          TPhastModel(Self).SutraLayerStructure.Loaded;
        end;
      end;
      FSutraMesh.BeginUpdate;
      try
        FSutraMesh.Mesh2D.BeginUpdate;
        try
          FSutraMesh.Clear;
          FSutraMesh.Mesh2D.Nodes.Capacity := MeshCreator.NodeCount;
          FSutraMesh.Mesh2D.Elements.Capacity := MeshCreator.ElementCount;
          for NodeIndex := 0 to MeshCreator.NodeCount - 1 do
          begin
            SutraNode := FSutraMesh.Mesh2D.Nodes.Add;
            MeshNode := MeshCreator.Nodes[NodeIndex];
            SutraNode.AssignINode(MeshNode);
          end;
          for ElementIndex := 0 to MeshCreator.ElementCount - 1 do
          begin
            SutraElement := FSutraMesh.Mesh2D.Elements.Add;
            MeshElement := MeshCreator.Elements[ElementIndex];
            SutraElement.AssignIElement(MeshElement);
          end;
        finally
          FSutraMesh.Mesh2D.EndUpdate;
        end;
//        FSutraMesh.UpdateElevations;
      finally
        FSutraMesh.EndUpdate;
      end;
      frmGoPhast.InvalidateGrid;

    finally
      MeshCreator.Free;
    end;

  finally
    List.Free;
  end;
end;

procedure TCustomModel.UpdateDataArrayDimensions(DataArray: TDataArray);
begin
  if Grid <> nil then
  begin
    DataArray.UpdateDimensions(Grid.LayerCount, Grid.RowCount, Grid.ColumnCount);
  end
  {$IFDEF Sutra}
  else if (FModelSelection = msSutra) and (Mesh <> nil) then
  begin
    case DataArray.EvaluatedAt of
      eaBlocks:
        begin
          DataArray.UpdateDimensions((Self as TPhastModel).
            SutraLayerStructure.LayerCount, 1, Mesh.Mesh2D.Elements.Count);
        end;
      eaNodes:
        begin
          DataArray.UpdateDimensions((Self as TPhastModel).
            SutraLayerStructure.NodeLayerCount, 1, Mesh.Mesh2D.Nodes.Count);
        end;
    else
      Assert(False);
    end;
  end
  {$ENDIF}
  else
  begin
    DataArray.UpdateDimensions(-1, -1, -1);
  end;
end;

function TCustomModel.GetCompiler(const Orientation: TDataSetOrientation;
      const EvaluatedAt: TEvaluatedAt): TRbwParser;
begin
  result := nil;

  {$IFDEF Sutra}
//  Assert(ModelSelection <> msSutra);
  { TODO -cSUTRA : Need to get TRbwParser for SUTRA }
  {$ENDIF}

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

  if Mesh <> nil then
  begin
    Mesh.TopDataSet := nil;
//    Mesh.FrontDataSet := nil;
//    Mesh.SideDataSet := nil;
    Mesh.ThreeDDataSet := nil;
  end;
end;

function TCustomModel.GetParsers(Index: integer): TRbwParser;
begin
  result := FParsers[Index];
end;

function TCustomModel.GetSelectedColumn: integer;
begin
  if Grid <> nil then
  begin
    result := Grid.SelectedColumn;
  end
  else
  begin
    result := 0;
  end;
end;

function TCustomModel.GetSelectedLayer: integer;
begin
  result := 0;
  case ModelSelection of
    msPhast, msModflow, msModflowLGR, msModflowNWT:
      begin
        result := Grid.SelectedLayer;
      end;
    {$IFDEF SUTRA}
    msSutra:
      begin
        if Mesh <> nil then
        begin
          result := Mesh.SelectedLayer;
        end;
      end
    {$ENDIF}
  else
    Assert(False);
  end;
end;

function TCustomModel.GetSelectedRow: integer;
begin
  if Grid <> nil then
  begin
    result := Grid.SelectedRow;
  end
  else
  begin
    result := 0;
  end;
end;

function TCustomModel.GetSideDataSet: TDataArray;
begin
  result := nil;
  case ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast, msModflow, msModflowLGR, msModflowNWT:
      begin
        result := Grid.SideDataSet;
      end;
    {$IFDEF SUTRA}
    msSutra:
      begin
        if (Mesh <> nil) then
        begin
          // do nothing
        end;
      end;
    {$ENDIF}
    else
       Assert(False);
  end;
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
  procedure UpdateGlobalVariable(Variable: TGlobalVariable);
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
                Variable.RealValue, Variable.Name);
            end;
          rdtInteger:
            begin
              Compiler.CreateVariable(Variable.Name, StrGlobalVariables,
                Variable.IntegerValue, Variable.Name);
            end;
          rdtBoolean:
            begin
              Compiler.CreateVariable(Variable.Name, StrGlobalVariables,
                Variable.BooleanValue, Variable.Name);
            end;
          rdtString:
            begin
              Compiler.CreateVariable(Variable.Name, StrGlobalVariables,
                Variable.StringValue, Variable.Name);
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
      UpdateGlobalVariable(Variable);
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
              Variable.RealValue, Variable.Name);
          end;
        rdtInteger:
          begin
            Compiler.CreateVariable(Variable.Name, StrGlobalVariables,
              Variable.IntegerValue, Variable.Name);
          end;
        rdtBoolean:
          begin
            Compiler.CreateVariable(Variable.Name, StrGlobalVariables,
              Variable.BooleanValue, Variable.Name);
          end;
        rdtString:
          begin
            Compiler.CreateVariable(Variable.Name, StrGlobalVariables,
              Variable.StringValue, Variable.Name);
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
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildItem: TChildModelItem;
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
    Variable := TempCompiler.Variables[VarIndex];
    if (Variable.ResultType <> DataSet.DataType) then
    begin
      TempCompiler.RemoveVariable(Variable as TCustomVariable);
      if TempCompiler <> Local3DCompiler then
      begin
        VarIndex := Local3DCompiler.IndexOfVariable(DataSet.Name);
        if VarIndex >= 0 then
        begin
          Variable := Local3DCompiler.Variables[VarIndex];
          Local3DCompiler.RemoveVariable(Variable as TCustomVariable);
        end;
      end;
      VarIndex := -1;
    end;
  end;
  if VarIndex < 0 then
  begin
    case DataSet.Datatype of
      rdtDouble:
        begin
          TempCompiler.CreateVariable(DataSet.Name,
            DataSet.FullClassification, 0.0, DataSet.DisplayName);
          if TempCompiler <> Local3DCompiler then
          begin
            Local3DCompiler.CreateVariable(DataSet.Name,
              DataSet.FullClassification, 0.0, DataSet.DisplayName);
          end;
        end;
      rdtInteger:
        begin
          TempCompiler.CreateVariable(DataSet.Name,
            DataSet.FullClassification, 0, DataSet.DisplayName);
          if TempCompiler <> Local3DCompiler then
          begin
            Local3DCompiler.CreateVariable(DataSet.Name,
              DataSet.FullClassification, 0, DataSet.DisplayName);
          end;
        end;
      rdtBoolean:
        begin
          TempCompiler.CreateVariable(DataSet.Name,
            DataSet.FullClassification, False, DataSet.DisplayName);
          if TempCompiler <> Local3DCompiler then
          begin
            Local3DCompiler.CreateVariable(DataSet.Name,
              DataSet.FullClassification, False, DataSet.DisplayName);
          end;
        end;
      rdtString:
        begin
          TempCompiler.CreateVariable(DataSet.Name,
            DataSet.FullClassification, '', DataSet.DisplayName);
          if TempCompiler <> Local3DCompiler then
          begin
            Local3DCompiler.CreateVariable(DataSet.Name,
              DataSet.FullClassification, '', DataSet.DisplayName);
          end;
        end;
    else
      Assert(False);
    end;
  end
  else
  begin
    Variable := TempCompiler.Variables[VarIndex];
    Assert(Variable.Name = DataSet.Name);
    Assert(Variable.ResultType = DataSet.DataType);
    Variable.Classification := DataSet.FullClassification;
    if TempCompiler <> Local3DCompiler then
    begin
      VarIndex := Local3DCompiler.IndexOfVariable(DataSet.Name);
      Variable := Local3DCompiler.Variables[VarIndex];
      Assert(Variable.Name = DataSet.Name);
      Assert(Variable.ResultType = DataSet.DataType);
      Variable.Classification := DataSet.FullClassification;
    end;
  end;
  if self is TPhastModel then
  begin
    LocalModel := TPhastModel(self);
    for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
    begin
      ChildItem := LocalModel.ChildModels[ChildIndex];
      ChildItem.ChildModel.CreateVariables(DataSet);
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
    DataArray := ThreeDDataSet;
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

procedure TCustomModel.OnTopSutraMeshChanged(Sender: TObject);
begin
  {$IFDEF SUTRA}
  if (ModelSelection = msSutra) and (FSutraMesh <> nil) then
  begin
    FSutraMesh.ElevationsNeedUpdating := True;
  end;
  {$ENDIF}
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

procedure TCustomModel.FreeSfrWriter;
begin
  FreeAndNil(SfrWriter);
  FreeAndNil(LakWriter);

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
var
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildDataArray: TDataArray;
begin
  if FDataSetsToCache.IndexOf(DataArray) < 0 then
  begin
    if (DataArray.Name <> '')
      and not (DataArray is TSparseArrayPhastInterpolationDataSet)
      and not (DataArray is TCustomSparseDataSet)
      then
    begin
      FDataSetsToCache.Add(DataArray);
      if FCustomModel is TPhastModel then
      begin
        LocalModel := TPhastModel(FCustomModel);
        for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
        begin
          ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
          ChildDataArray := ChildModel.DataArrayManager.GetDataSetByName(DataArray.Name);
          if (ChildDataArray <> nil) then
          begin
            ChildModel.DataArrayManager.AddDataSetToCache(ChildDataArray);
          end;
        end;
      end;
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
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  for Index := 0 to FDataSetsToCache.Count - 1 do
  begin
    DataArray := FDataSetsToCache[Index];
    if (FCustomModel.Grid = nil)
      or ((DataArray <> FCustomModel.TopDataSet)
      and (DataArray <> FCustomModel.FrontDataSet)
      and (DataArray <> FCustomModel.SideDataSet)
      and (DataArray <> FCustomModel.ThreeDDataSet)
      and (DataArray <> FCustomModel.Grid.TopContourDataSet)
      and (DataArray <> FCustomModel.Grid.FrontContourDataSet)
      and (DataArray <> FCustomModel.Grid.SideContourDataSet)
      and (DataArray <> FCustomModel.Grid.ThreeDContourDataSet)) then
    begin
      DataArray.CacheData;
    end;
  end;
  FDataSetsToCache.Clear;
  if FCustomModel is TPhastModel then
  begin
    LocalModel := TPhastModel(FCustomModel);
    for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.DataArrayManager.CacheDataArrays;
    end;
  end;
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
  FDispersivityIndex := -1;
  FPorosityIndex := -1;
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
  DisplayName: string;
begin
  { TODO : Find a way to extract common code from
TPhastModel.CreateModflowDataSets and
TCustomCreateRequiredDataSetsUndo.UpdateDataArray}

  // See DefinePackageDataArrays for the definition of the
  // contents of DataArrayCreationRecords.
  for Index := 0 to Length(FDataArrayCreationRecords) - 1 do
  begin
    DataSetName := FDataArrayCreationRecords[Index].Name;
    DisplayName := FDataArrayCreationRecords[Index].DisplayName;
    Orientation := FDataArrayCreationRecords[Index].Orientation;
    DataType := FDataArrayCreationRecords[Index].DataType;
    ArrayNeeded := FDataArrayCreationRecords[Index].DataSetNeeded;
    ArrayArrayShouldBeCreated :=
      FDataArrayCreationRecords[Index].DataSetShouldBeCreated;
    NewFormula := FDataArrayCreationRecords[Index].Formula;
    Classification := FDataArrayCreationRecords[Index].Classification;
    Lock := FDataArrayCreationRecords[Index].Lock;

    DataArray := GetDataSetByName(DataSetName);
    Assert(Assigned(ArrayNeeded));
    if DataArray <> nil then
    begin
      DataArray.Name := DataSetName;
      DataArray.DisplayName := DisplayName;
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
        DisplayName,
        Lock, DataType, FDataArrayCreationRecords[Index].EvaluatedAt,
        Orientation, Classification);
      DataArray.OnDataSetUsed := ArrayNeeded;
      DataArray.Lock := Lock;
      DataArray.CheckMax := FDataArrayCreationRecords[Index].CheckMax;
      DataArray.CheckMin := FDataArrayCreationRecords[Index].CheckMin;
      DataArray.Max := FDataArrayCreationRecords[Index].Max;
      DataArray.Min := FDataArrayCreationRecords[Index].Min;
      DataArray.DisplayName := DisplayName;
    end;
    if DataArray <> nil then
    begin
      FCustomModel.UpdateDataArrayDimensions(DataArray);
//      if FCustomModel.Grid <> nil then
//      begin
//        DataArray.UpdateDimensions(FCustomModel.Grid.LayerCount, FCustomModel.Grid.RowCount,
//          FCustomModel.Grid.ColumnCount);
//      end;
      DataArray.AssociatedDataSets := FDataArrayCreationRecords[
        Index].AssociatedDataSets;
      DataArray.Classification := FDataArrayCreationRecords[Index].Classification;
    end;
  end;

  DataArray := GetDataSetByName(rsActive);
  if (DataArray <> nil) and not Assigned(DataArray.OnUpToDateSet) then
  begin
    DataArray.OnUpToDateSet := FCustomModel.OnActiveDataSetChanged;
  end;
end;

function TDataArrayManager.CreateNewDataArray(const ClassType: TDataArrayType;
  const Name, Formula, DisplayName: string; Lock: TDataLock; DataType: TRbwDataType;
  EvaluatedAt: TEvaluatedAt; Orientation: TDataSetOrientation;
  const Classification: string): TDataArray;
var
  ChildManagerIndex: Integer;
  ChildMan: TDataArrayManager;
begin
  result := ClassType.Create(FCustomModel);
  result.Lock := Lock;
  result.UpdateWithName(Name);
  result.DisplayName := DisplayName;
  result.DataType := DataType;
  result.EvaluatedAt := EvaluatedAt;
  result.Orientation := Orientation;
  result.Formula := Formula;
  result.Classification := Classification;
  AddDataSet(result);
  FCustomModel.CreateVariables(result);
  for ChildManagerIndex := 0 to ChildDataArrayManagerCount - 1 do
  begin
    ChildMan := ChildDataArrayManagers[ChildManagerIndex];
    if ChildMan.GetDataSetByName(Name) = nil then
    begin
      ChildMan.CreateNewDataArray(ClassType, Name, Formula, DisplayName, Lock, DataType,
        EvaluatedAt, Orientation, Classification);
    end;
  end;
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
  SetLength(FDataArrayCreationRecords, 70);
  Index := 0;

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtBoolean;
  FDataArrayCreationRecords[Index].Name := rsActive;
  FDataArrayCreationRecords[Index].DisplayName := rsActiveDisplayName;
  FDataArrayCreationRecords[Index].Formula := 'True';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded :=
    FCustomModel.ModflowOrPhastUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: MEDIA-active'+ sLineBreak + 'MODFLOW BAS: IBOUND';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsKx;
  FDataArrayCreationRecords[Index].DisplayName := rsKxDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.0001';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded :=
    FCustomModel.AquiferPropertiesUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: MEDIA-Kx'+ sLineBreak + 'MODFLOW LPF: HK'
    + sLineBreak + 'MODFLOW BCF: TRAN,HY';
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsKy;
  FDataArrayCreationRecords[Index].DisplayName := rsKyDisplayName;
  FDataArrayCreationRecords[Index].Formula := rsKx;
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.KyUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: MEDIA-Ky'+ sLineBreak + 'MODFLOW LPF: HANI'
    + sLineBreak + 'MODFLOW HUF and BCF: (not used)';
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsKz;
  FDataArrayCreationRecords[Index].DisplayName := rsKzDisplayName;
  FDataArrayCreationRecords[Index].Formula := rsKx + ' / 10';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.KzUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: MEDIA-Kz'+ sLineBreak + 'MODFLOW LPF: VKA'
    + sLineBreak + 'MODFLOW BCF: Vcont';
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  FPorosityIndex := Index;
  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsPorosity;
  FDataArrayCreationRecords[Index].DisplayName := rsPorosityDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.25';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.PorosityUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: MEDIA-porosity'
    + sLineBreak + 'MODPATH: POR'
    + sLineBreak + 'MT3DMS BTN Package: PRSITY';
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsSpecific_Storage;
  FDataArrayCreationRecords[Index].DisplayName := rsSpecific_StorageDisplayName;
  FDataArrayCreationRecords[Index].Formula := '1e-5';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded :=
    FCustomModel.SpecificStorageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: MEDIA-specific_storage'+ sLineBreak + 'MODFLOW LPF: Ss'
    + sLineBreak + 'MODFLOW BCF: Sf1';
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsLong_Dispersivity;
  FDataArrayCreationRecords[Index].DisplayName := rsLong_DispersivityDisplayName;
  FDataArrayCreationRecords[Index].Formula := '10.';
  FDispersivityIndex := Index;
  if (FCustomModel = nil) or (FCustomModel.ModelSelection = msPhast) then
  begin
    FDataArrayCreationRecords[Index].Classification := StrHydrology;
  end
  else
  begin
    FDataArrayCreationRecords[Index].Classification := StrMT3DMS_Classificaton;
  end;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.LongitudinalDispersionUsed;

  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'PHAST: MEDIA-longitudinal_dispersivity; '#13#10'MT3DMS: Dispersion Package Data Sets C1: AL';
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsHorizontal_Transv_Dispersivity;
  FDataArrayCreationRecords[Index].DisplayName := rsHorizontal_Transv_DispersivityDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsVertical_Transv_DispersivityDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsInitial_HeadDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsInitial_Water_TableDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsChemistry_Initial_SolutionDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsChemistry_Initial_Equilibrium_PhasesDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsChemistry_Initial_SurfaceDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsChemistry_Initial_ExchangeDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsChemistry_Initial_Gas_PhaseDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsChemistry_Initial_Solid_SolutionsDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsChemistry_Initial_KineticsDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsPrint_ChemistryDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsPrint_XYZ_ChemistryDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsResLayerDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsResBottomDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsResKvDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsResBedThicknessDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsLakeIDDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsLakeLeakanceDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := StrUzfLandSurfaceDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsModflowSpecifiedHeadDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := StrUzfLayerDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := StrUzfDischargeRoutingDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := StrUzfVerticalKDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := StrUzfBrooksCoreyEpsilonDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := StrUzfSaturatedWaterContentDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := StrUzfInitialUnsaturatedWaterContentDisplayName;
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
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrUzfReisidualWaterContent;
  FDataArrayCreationRecords[Index].DisplayName := StrUzfReisidualWaterContentDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.2';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfResidualWaterContentUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := 'UZF: THTR';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := StrUzfGage_1_and_2;
  FDataArrayCreationRecords[Index].DisplayName := StrUzfGage_1_and_2DisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := StrUzfGage3DisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsModflow_Initial_HeadDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsModflow_CBKzDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsVerticalAnisotropyDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsHorizontalAnisotropyDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsSpecificYieldDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsWetDryThresholdDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := rsWetDryFlagDisplayName;
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
    + sLineBreak + sLineBreak + 'A value < 0 indicates that only the cell below the '
      + 'dry cell can cause the dry cell to become active again.'
    + sLineBreak + sLineBreak + 'A value > 0 indicates that the cell below the dry cell '
      + 'or the cells next to the dry cell can cause the dry cell to become '
      + 'active again.'
    + sLineBreak + sLineBreak + 'A value = 0 indicates that the dry cell can not become '
      +'active again.';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsWetDry;
  FDataArrayCreationRecords[Index].DisplayName := rsWetDryDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := StrModpathZoneDisplayName;
  FDataArrayCreationRecords[Index].Formula := '1';
  FDataArrayCreationRecords[Index].Classification := StrMODPATH;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ModpathZonesNeeded;
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
  FDataArrayCreationRecords[Index].DisplayName := StrHufReferenceSurfaceDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := StrTransmissivityDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := StrVerticalConductanceDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := StrConfinedStorageCoeDisplayName;
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
  FDataArrayCreationRecords[Index].Name := StrHUFKxName;
  FDataArrayCreationRecords[Index].DisplayName := StrHUFKxNameDisplayName;
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
  FDataArrayCreationRecords[Index].Name := StrHUFKyName;
  FDataArrayCreationRecords[Index].DisplayName := StrHUFKyNameDisplayName;
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
  FDataArrayCreationRecords[Index].Name := StrHUFInterlayerKz;
  FDataArrayCreationRecords[Index].DisplayName := StrHUFInterlayerKzDisplayName;
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
  FDataArrayCreationRecords[Index].Name := StrHUFSSName;
  FDataArrayCreationRecords[Index].DisplayName := StrHUFSSNameDisplayName;
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
  FDataArrayCreationRecords[Index].Name := StrHUFAverageSYName;
  FDataArrayCreationRecords[Index].DisplayName := StrHUFAverageSYNameDisplayName;
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
  FDataArrayCreationRecords[Index].Name := StrHUFSYName;
  FDataArrayCreationRecords[Index].DisplayName := StrHUFSYNameDisplayName;
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
  FDataArrayCreationRecords[Index].Name := StrHUFSYTPName;
  FDataArrayCreationRecords[Index].DisplayName := StrHUFSYTPNameDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := StrZonesDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := StrGeostaticStressDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := StrSpecificGravityUnsDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := StrSpecificGravitySatDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := StrInitialPreOffsetsDisplayName;
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
  FDataArrayCreationRecords[Index].DisplayName := StrInitialPreconsolidaDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrSubsidence;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SwtSpecifiedUsed;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.SwtSpecifiedUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'SWT: PCS (data set 15)';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtBoolean;
  FDataArrayCreationRecords[Index].Name := STR_MT3DMS_Observation_Locations;
  FDataArrayCreationRecords[Index].DisplayName := STR_MT3DMS_Observation_LocationsDisplayName;
  FDataArrayCreationRecords[Index].Formula := 'False';
  FDataArrayCreationRecords[Index].Classification := StrMT3DMS_Classificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Mt3dMSUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MT3DMS Basic Package Data Sets 18 and 19';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtBoolean;
  FDataArrayCreationRecords[Index].Name := StrMT3DMSActive;
  FDataArrayCreationRecords[Index].DisplayName := StrMT3DMSActiveDisplayName;
  FDataArrayCreationRecords[Index].Formula := rsActive;
  FDataArrayCreationRecords[Index].Classification := StrMT3DMS_Classificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Mt3dMSUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MT3DMS Basic Package Data Sets 12: ICBUND';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsBulkDensity;
  FDataArrayCreationRecords[Index].DisplayName := rsBulkDensityDisplayName;
  FDataArrayCreationRecords[Index].Formula := '2000000';
  FDataArrayCreationRecords[Index].Classification := StrMT3DMS_Classificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Mt3dMSBulkDensityUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MT3DMS Chemical Reaction Package Data Set E2A: RHOB';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsImmobPorosity;
  FDataArrayCreationRecords[Index].DisplayName := rsImmobPorosityDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.2';
  FDataArrayCreationRecords[Index].Classification := StrMT3DMS_Classificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Mt3dMSImmobPorosityUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MT3DMS Chemical Reaction Package Data Set E2B: PRSITY2';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsMT3DMS_Layer_Thickness;
  FDataArrayCreationRecords[Index].DisplayName := rsMT3DMS_Layer_ThicknessDisplayName;
  FDataArrayCreationRecords[Index].Formula :=StrLayerHeight;
  FDataArrayCreationRecords[Index].Classification := StrMT3DMS_Classificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Mt3dMSUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MT3DMS Basic package DZ data set 10';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtBoolean;
  FDataArrayCreationRecords[Index].Name := KModpathBudget;
  FDataArrayCreationRecords[Index].DisplayName := KModpathBudgetDisplayName;
  FDataArrayCreationRecords[Index].Formula := 'False';
  FDataArrayCreationRecords[Index].Classification := StrMODPATH;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ModpathBudgetNeeded;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODPATH Simulation file, Item 27';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KModpathRetardation;
  FDataArrayCreationRecords[Index].DisplayName := KModpathRetardationDisplayName;
  FDataArrayCreationRecords[Index].Formula := '2.';
  FDataArrayCreationRecords[Index].Classification := StrMODPATH;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ModpathRetardationNeeded;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODPATH Simulation file, Items 32 and 33: RetardationFactor and RetardationFactorCB';
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
//    DataSet.SetModelToNil;
  end;
  for Index := 0 to LocalCount - 1 do
  begin
    DataSet := DataSets[Index];
    DataSet.StopTalkingToAnyone;
//    DataSet.SetModelToNil;
  end;
  for Index := 0 to FDeletedDataSets.Count - 1 do
  begin
    DataSet := FDeletedDataSets[Index];
//    DataSet.StopTalkingToAnyone;
    DataSet.SetModelToNil;
  end;
  for Index := 0 to LocalCount - 1 do
  begin
    DataSet := DataSets[Index];
//    DataSet.StopTalkingToAnyone;
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
  ChildModel: TChildModel;
begin
  PhastModel := FCustomModel as TPhastModel;
  ChildModel := PhastModel.ChildModels[Index].ChildModel;
  if ChildModel = nil then
  begin
    result := nil;
  end
  else
  begin
    result := ChildModel.DataArrayManager;
  end;
end;

function TDataArrayManager.GetChildDataArrayManagerCount: integer;
var
  PhastModel: TPhastModel;
begin
  if FCustomModel is TPhastModel then
  begin
    PhastModel := TPhastModel(FCustomModel);
    if PhastModel.LgrUsed then
    begin
      result := PhastModel.ChildModels.Count;
    end
    else
    begin
      result := 0;
    end;
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
  if LocalCount = 0 then
  begin
    result := nil;
    Exit;
  end;
  if FDataSetLookUpList = nil then
  begin
    FDataSetLookUpList := THashTableFacade.Create( Max(211, DataSetCount*2-1));
    FDataSetLookUpList.IgnoreCase := True;
//    FDataSetLookUpList.TableSize := Max(211, DataSetCount*2-1);
    for Index := 0 to LocalCount - 1 do
    begin
      DataArray := DataSets[Index];
      FDataSetLookUpList.Insert(DataArray.Name, DataArray)
    end;
  end;
  if FDataSetLookUpList.Search(DataSetName, APointer) then
  begin
    result := APointer;
  end
  else
  begin
    result := nil;
  end;
end;

function TDataArrayManager.LocalCount: integer;
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

function TDataArrayManager.GetDataSetCount: integer;
begin
  if (FCustomModel <> nil) and (FCustomModel is TChildModel) then
  begin
    if TChildModel(FCustomModel).FParentModel = nil then
    begin
      result := LocalCount;
    end
    else
    begin
      result := TChildModel(FCustomModel).FParentModel.DataArrayManager.DataSetCount;
    end;
  end
  else
  begin
    result := LocalCount;
  end;
end;

function TDataArrayManager.GetDataSet(const Index: integer): TDataArray;
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
//  ChildGrid: TCustomModelGrid;
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
          ChildDataArray := ChildManager.CreateNewDataArray(
            TDataArrayType(DataArray.ClassType),
            DataArray.Name, DataArray.Formula, DataArray.DisplayName,  DataArray.Lock,
            DataArray.DataType, DataArray.EvaluatedAt, DataArray.Orientation,
            DataArray.Classification);
          ChildDataArray.AssignProperties(DataArray);
          ChildManager.FCustomModel.UpdateDataArrayDimensions(ChildDataArray);
//          ChildGrid := ChildManager.FCustomModel.Grid;
//          ChildDataArray.UpdateDimensions(ChildGrid.LayerCount,
//            ChildGrid.RowCount, ChildGrid.ColumnCount);
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
        if ChildDataArray <> nil then
        begin
          NewDeletedList.Add(ChildDataArray);
        end;
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

procedure TDataArrayManager.InvalidateAll3DDataSets;
var
  Index: Integer;
  DS: TDataArray;
begin
  for Index := 0 to DataSetCount - 1 do
  begin
    DS := DataSets[Index];
    if DS.Orientation = dso3D then
    begin
      DS.Invalidate;
    end;
  end;
  for Index := 0 to BoundaryDataSetCount - 1 do
  begin
    DS := BoundaryDataSets[Index];
    if DS.Orientation = dso3D then
    begin
      DS.Invalidate;
    end;
  end;
  for Index := 0 to ChildDataArrayManagerCount - 1 do
  begin
    ChildDataArrayManagers[Index].InvalidateAll3DDataSets;
  end;
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
  ChildModel: TChildModel;
  ParentDataSet: TDataArray;
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
  if FCustomModel is TChildModel then
  begin
    ChildModel := TChildModel(FCustomModel);
    if ChildModel.ParentModel <> nil then
    begin
      for Index := 0 to DataSetCount - 1 do
      begin
        DataSet := DataSets[Index];
        ParentDataSet := ChildModel.ParentModel.
          DataArrayManager.GetDataSetByName(DataSet.Name);
        if (ParentDataSet <> nil) then
        begin
          ParentDataSet.StopsTalkingTo(DataSet);
        end;
      end;
    end;
  end;
end;

procedure TDataArrayManager.UpdateClassifications;
var
  DataArray: TDataArray;
  Packages: TModflowPackages;
begin
  if FDispersivityIndex >= 0 then
  begin
    if FCustomModel.ModelSelection = msPhast then
    begin
      FDataArrayCreationRecords[FDispersivityIndex].Classification := StrHydrology;
    end
    else
    begin
      FDataArrayCreationRecords[FDispersivityIndex].Classification := StrMT3DMS_Classificaton;
    end;
    DataArray := GetDataSetByName(FDataArrayCreationRecords[FDispersivityIndex].Name);
    if DataArray <> nil then
    begin
      DataArray.Classification :=
        FDataArrayCreationRecords[FDispersivityIndex].Classification;
    end;
  end;

  if FPorosityIndex >= 0 then
  begin
    if FCustomModel.ModelSelection = msPhast then
    begin
      FDataArrayCreationRecords[FPorosityIndex].Classification := StrHydrology;
    end
    else
    begin
      Packages := FCustomModel.ModflowPackages;
      if Packages.Mt3dBasic.IsSelected and Packages.ModPath.IsSelected then
      begin
        FDataArrayCreationRecords[FPorosityIndex].Classification := StrModpath + ' \ ' + StrMT3DMS_Classificaton;
      end
      else if Packages.Mt3dBasic.IsSelected then
      begin
        FDataArrayCreationRecords[FPorosityIndex].Classification := StrMT3DMS_Classificaton;
      end
      else if Packages.ModPath.IsSelected then
      begin
        FDataArrayCreationRecords[FPorosityIndex].Classification := StrModpath;
      end
      else
      begin
        FDataArrayCreationRecords[FPorosityIndex].Classification := StrHydrology;
      end;
    end;
    DataArray := GetDataSetByName(FDataArrayCreationRecords[FPorosityIndex].Name);
    if DataArray <> nil then
    begin
      DataArray.Classification :=
        FDataArrayCreationRecords[FPorosityIndex].Classification;
    end;
  end;
end;

procedure TDataArrayManager.UpdateDataSetDimensions;
var
  Index: integer;
  DataSet: TDataArray;
  Grid: TCustomModelGrid;
  Manager: TDataArrayManager;
begin
  Grid := FCustomModel.Grid;
//  if Grid = nil then
//  begin
//    for Index := 0 to LocalCount - 1 do
//    begin
//      DataSet := DataSets[Index];
//      DataSet.UpdateDimensions(-1, -1, -1);
//    end;
//    for Index := 0 to BoundaryDataSetCount - 1 do
//    begin
//      DataSet := BoundaryDataSets[Index];
//      DataSet.UpdateDimensions(-1, -1, -1);
//    end;
//    for Index := 0 to FDeletedDataSets.Count - 1 do
//    begin
//      DataSet := FDeletedDataSets[Index];
//      DataSet.UpdateDimensions(-1, -1, -1);
//    end;
//  end
//  else
//  begin
    for Index := 0 to LocalCount - 1 do
    begin
      DataSet := DataSets[Index];
      FCustomModel.UpdateDataArrayDimensions(DataSet);
//      DataSet.UpdateDimensions(Grid.LayerCount,
//        Grid.RowCount, Grid.ColumnCount);
    end;
    for Index := 0 to BoundaryDataSetCount - 1 do
    begin
      DataSet := BoundaryDataSets[Index];
      FCustomModel.UpdateDataArrayDimensions(DataSet);
//      DataSet.UpdateDimensions(Grid.LayerCount,
//        Grid.RowCount, Grid.ColumnCount);
    end;
    for Index := 0 to FDeletedDataSets.Count - 1 do
    begin
      DataSet := FDeletedDataSets[Index];
      FCustomModel.UpdateDataArrayDimensions(DataSet);
//      DataSet.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
//        Grid.ColumnCount);
    end;
  if Grid <> nil then
  begin
    Grid.NeedToRecalculateTopCellColors := True;
    Grid.NeedToRecalculateFrontCellColors := True;
    Grid.NeedToRecalculateSideCellColors := True;
  end;
  for Index := 0 to ChildDataArrayManagerCount - 1 do
  begin
    Manager := ChildDataArrayManagers[Index];
    if Manager <> nil then
    begin
      Manager.UpdateDataSetDimensions;
    end;
  end;
end;

function TCustomModel.AquiferPropertiesUsed(Sender: TObject): boolean;
begin
  result := ModflowOrPhastUsed(Sender) and ((ModelSelection = msPhast)
    or not ModflowPackages.HufPackage.IsSelected);
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
    ModflowOptions := SourceModel.ModflowOptions;
    HeadObsResults := SourceModel.HeadObsResults;
    Mt3dmsHeadMassFluxObservations := SourceModel.Mt3dmsHeadMassFluxObservations;
    Mt3dmsWellMassFluxObservations := SourceModel.Mt3dmsWellMassFluxObservations;
    Mt3dmsDrnMassFluxObservations := SourceModel.Mt3dmsDrnMassFluxObservations;
    Mt3dmsRivMassFluxObservations := SourceModel.Mt3dmsRivMassFluxObservations;
    Mt3dmsGhbMassFluxObservations := SourceModel.Mt3dmsGhbMassFluxObservations;
    Mt3dmsRchMassFluxObservations := SourceModel.Mt3dmsRchMassFluxObservations;
    Mt3dmsEvtMassFluxObservations := SourceModel.Mt3dmsEvtMassFluxObservations;
    Mt3dmsMassLoadingMassFluxObservations := SourceModel.Mt3dmsMassLoadingMassFluxObservations;
    Mt3dmsResMassFluxObservations := SourceModel.Mt3dmsResMassFluxObservations;
    Mt3dmsLakMassFluxObservations := SourceModel.Mt3dmsLakMassFluxObservations;
    Mt3dmsDrtMassFluxObservations := SourceModel.Mt3dmsDrtMassFluxObservations;
    Mt3dmsEtsMassFluxObservations := SourceModel.Mt3dmsEtsMassFluxObservations;
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
  result := ModflowOrPhastUsed(Sender)
    and ((ModelSelection = msPhast)
    or ModflowPackages.LpfPackage.IsSelected
    or ModflowPackages.UpwPackage.IsSelected)
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
    msUndefined: result := False;
    msPhast: result := True;
    msModflow, msModflowLGR, msModflowNWT:
      begin
        if ModflowPackages.HufPackage.IsSelected then
        begin
          result := False;
          Exit;
        end;
        if ModflowPackages.LpfPackage.IsSelected
          or ModflowPackages.UpwPackage.IsSelected then
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
    {$IFDEF SUTRA} msSutra: result := False; {$ENDIF}
    else Assert(False);
  end;
end;

function TCustomModel.PorosityUsed(Sender: TObject): boolean;
begin
  result := (ModelSelection = msPhast)
    or ((ModelSelection in [msModflow, msModflowLGR, msModflowNWT])
    and (ModflowPackages.ModPath.IsSelected
    or ModflowPackages.Mt3dBasic.IsSelected));
end;

function TCustomModel.SpecificStorageUsed(Sender: TObject): boolean;
begin
  result := False;
  case ModelSelection of
    msUndefined: result := False;
    msPhast: result := True;
    msModflow, msModflowLGR, msModflowNWT:
      begin
        if ModflowPackages.LpfPackage.IsSelected
          or ModflowPackages.BcfPackage.IsSelected
          or ModflowPackages.UpwPackage.IsSelected then
        begin
          result := ModflowStressPeriods.TransientModel;
        end
        else
        begin
          result := False;
        end;
      end;
    {$IFDEF SUTRA} msSutra: result := False; {$ENDIF}
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

function TCustomModel.SutraUsed(Sender: TObject): boolean;
begin
  {$IFDEF Sutra}
  result := (ModelSelection = msSutra)
  {$ELSE}
  result := False;
  {$ENDIF}
end;

function TCustomModel.ModflowOrPhastUsed(Sender: TObject): boolean;
begin
  result := (ModelSelection in [msPhast, msModflow, msModflowLGR,
    msModflowNWT])
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
  result := ModflowUsed(Sender) and ModflowPackages.ResPackage.IsSelected
    and (ModflowPackages.ResPackage.LayerOption = loSpecified);
end;

function TCustomModel.ReservoirPackageUsed(Sender: TObject): boolean;
begin
  result := ModflowUsed(Sender) and ModflowPackages.ResPackage.IsSelected;
end;

function TCustomModel.LakePackageUsed(Sender: TObject): boolean;
begin
  result := ModflowUsed(Sender) and ModflowPackages.LakPackage.IsSelected;
end;

function TCustomModel.UzfPackageUsed(Sender: TObject): boolean;
begin
  result := ModflowUsed(Sender) and ModflowPackages.UzfPackage.IsSelected;
end;

function TCustomModel.UzfResidualWaterContentUsed(Sender: TObject): boolean;
begin
  result := UzfPackageUsed(Sender);
  if result then
  begin
//    if (Sender <> nil) and (Sender is TUndoChangeLgrPackageSelection) then
//    begin
//      result := True;
//    end
//    else
//    begin
      result := ModflowPackages.UzfPackage.SpecifyResidualWaterContent
        and (ModelSelection in [msModflow, {msModflowLGR,} msModflowNWT]);
//    end;
  end;
end;

function TCustomModel.UzfUnsatVertKUsed(Sender: TObject): boolean;
begin
  result := UzfPackageUsed(Sender) and
    (ModflowPackages.UzfPackage.VerticalKSource = 1);
end;

function TCustomModel.ModflowUsed(Sender: TObject): boolean;
begin
  result := ModelSelection in [msModflow, msModflowLGR, msModflowNWT];
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
//    if (Sender <> nil) and (Sender is TUndoChangeLgrPackageSelection) then
//    begin
//      result := True;
//    end
//    else
//    begin
      result := ModflowStressPeriods.CompletelyTransient or
        (ModflowPackages.UzfPackage.SpecifyInitialWaterContent
        and (ModelSelection in [msModflow, {msModflowLGR,} msModflowNWT]));
//    end;
  end;
end;

function TCustomModel.ModflowInitialHeadUsed(Sender: TObject): boolean;
begin
  result := ModflowUsed(Sender) and (ModflowOptions.InitialHeadFileName = '');
end;

function TCustomModel.ModflowLayerBottomDescription(
  const LayerID: integer): string;
begin
  result := LayerStructure.ModflowLayerBottomDescription(LayerID)
end;

function TCustomModel.ModflowLayerCount: integer;
begin
  result := LayerStructure.ModflowLayerCount
end;

function TCustomModel.ModflowLayerToDataSetLayer(
  ModflowLayer: integer): integer;
begin
  result := LayerStructure.ModflowLayerToDataSetLayer(ModflowLayer);
end;

function TCustomModel.ConfiningBedKzUsed(Sender: TObject): boolean;
var
  UnitIndex: Integer;
  LayerGroup: TLayerGroup;
begin
  result := ((ModflowPackages.LpfPackage.IsSelected
    or ModflowPackages.UpwPackage.IsSelected)
    and
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
  if ModflowPackages.LpfPackage.IsSelected
    or ModflowPackages.UpwPackage.IsSelected then
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
  result := (ModelSelection in [msModflow, msModflowLGR, msModflowNWT])
    and (ModflowPackages.LpfPackage.IsSelected
    or ModflowPackages.UpwPackage.IsSelected);
end;

function TCustomModel.SpecificYieldUsed(Sender: TObject): boolean;
var
  UnitIndex: Integer;
  LayerGroup: TLayerGroup;
begin
  result := ((ModflowPackages.LpfPackage.IsSelected
    or ModflowPackages.UpwPackage.IsSelected)
    and
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

function TCustomModel.StoreDrainObservations: Boolean;
begin
  result := FDrainObservations.Count > 0;
end;

function TCustomModel.StoreDrnMassFluxObservations: Boolean;
begin
  result := FMt3dmsDrnMassFluxObservations.Count > 0;
end;

function TCustomModel.StoreDrtMassFluxObservations: Boolean;
begin
  result := FMt3dmsDrtMassFluxObservations.Count > 0;
end;

function TCustomModel.StoreEtsMassFluxObservations: Boolean;
begin
  result := FMt3dmsEtsMassFluxObservations.Count > 0;
end;

function TCustomModel.StoreEvtMassFluxObservations: Boolean;
begin
  result := FMt3dmsEvtMassFluxObservations.Count > 0;
end;

function TCustomModel.StoreGhbMassFluxObservations: Boolean;
begin
  result := FMt3dmsGhbMassFluxObservations.Count > 0;
end;

function TCustomModel.StoreGhbObservations: Boolean;
begin
  result := FGhbObservations.Count > 0;
end;

function TCustomModel.StoreHeadFluxObservations: Boolean;
begin
  result := FHeadFluxObservations.Count > 0;
end;

function TCustomModel.StoreHeadMassFluxObservations: Boolean;
begin
  result := FMt3dmsHeadMassFluxObservations.Count > 0;
end;

function TCustomModel.StoreHeadObsResults: boolean;
begin
  Result := (FHeadObsResults <> nil) and (FHeadObsResults.Count > 0);
end;

function TCustomModel.StoreHydrogeologicUnits: Boolean;
begin
  result := FHydrogeologicUnits.Count > 0;
end;

function TCustomModel.StoreLakMassFluxObservations: Boolean;
begin
  result := FMt3dmsLakMassFluxObservations.Count > 0;
end;

function TCustomModel.StoreMassLoadingMassFluxObservations: Boolean;
begin
  result := FMt3dmsMassLoadingMassFluxObservations.Count > 0;
end;

function TCustomModel.StoreRchMassFluxObservations: Boolean;
begin
  result := FMt3dmsRchMassFluxObservations.Count > 0;
end;

function TCustomModel.StoreResMassFluxObservations: Boolean;
begin
  result := FMt3dmsResMassFluxObservations.Count > 0;
end;

function TCustomModel.StoreRiverObservations: Boolean;
begin
  result := FRiverObservations.Count > 0;
end;

function TCustomModel.StoreRivMassFluxObservations: Boolean;
begin
  result := FMt3dmsRivMassFluxObservations.Count > 0;
end;

function TCustomModel.StoreWellMassFluxObservations: Boolean;
begin
  result := FMt3dmsWellMassFluxObservations.Count > 0;
end;

function TCustomModel.WetDryUsed(Sender: TObject): boolean;
begin
  result := ModflowWettingOptions.WettingActive
    and not ModflowPackages.UpwPackage.IsSelected;
end;

function TCustomModel.ModpathBudgetNeeded(Sender: TObject): boolean;
begin
  result := ModpathUsed(Sender)
    and (ModflowPackages.ModPath.BudgetChecking = bcList);
end;

function TCustomModel.ModpathRetardationNeeded(Sender: TObject): boolean;
begin
  result := ModpathUsed(Sender)
    and (ModflowPackages.ModPath.RetardationOption = roUsed);
end;

function TCustomModel.ModpathZonesNeeded(Sender: TObject): boolean;
begin
  result := ModpathUsed(Sender);
  if result and (ModflowPackages.ModPath.MpathVersion = mp6) then
  begin
    result := ModflowPackages.ModPath.StopInZone;
  end;
end;

function TCustomModel.ModpathUsed(Sender: TObject): boolean;
begin
  result := (ModelSelection in [msModflow, msModflowLGR, msModflowNWT])
    and ModflowPackages.ModPath.IsSelected;
end;

function TCustomModel.HufReferenceSurfaceNeeded(Sender: TObject): boolean;
begin
  { TODO : In LGR, this function may need to updated. }
  result := (ModelSelection in [msModflow, msModflowLGR, msModflowNWT])
    and ModflowPackages.HufPackage.IsSelected
    and (ModflowPackages.HufPackage.ReferenceChoice = hrcReferenceLayer)
    and (HufParameters.CountParameters([ptHUF_KDEP]) > 0);
end;

function TCustomModel.BcfUsed(Sender: TObject): boolean;
begin
  result := (ModelSelection in [msModflow, msModflowLGR, msModflowNWT])
    and ModflowPackages.BcfPackage.IsSelected
end;

procedure TCustomModel.BeginGridChange;
begin
  if Grid <> nil then
  begin
    Grid.BeginGridChange
  end;
end;

function TCustomModel.ConfinedStorageCoefUsed(Sender: TObject): boolean;
begin
  result := False;
  case ModelSelection of
    msUndefined: result := False;
    msPhast {$IFDEF SUTRA}, msSutra {$ENDIF}: result := False;
    msModflow, msModflowLGR, msModflowNWT:
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
  result := (ModelSelection in [msModflow, msModflowLGR, msModflowNWT])
    and ModflowPackages.HufPackage.IsSelected
end;

function TCustomModel.HufStorageUsed(Sender: TObject): boolean;
begin
  result := HufSelected(Sender)
    and ModflowStressPeriods.TransientModel;
end;

function TCustomModel.ZoneBudgetSelected(Sender: TObject): boolean;
begin
  result := (ModelSelection in [msModflow, msModflowLGR, msModflowNWT])
    and ModflowPackages.ZoneBudget.IsSelected
end;

function TCustomModel.SwtSelected(Sender: TObject): boolean;
begin
  result := (ModelSelection in [msModflow, msModflowLGR, msModflowNWT])
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

function TCustomModel.LongitudinalDispersionUsed(Sender: TObject): boolean;
begin
  result := ChemistryUsed(Sender)
    or (Mt3dMSUsed(Sender) and ModflowPackages.Mt3dmsDispersion.IsSelected);
end;

function TCustomModel.Mt3dMSBulkDensityUsed(Sender: TObject): boolean;
begin
  result := Mt3dMSUsed(Sender) and ModflowPackages.Mt3dmsChemReact.IsSelected
    and (ModflowPackages.Mt3dmsChemReact.SorptionChoice in [scLinear,
    scFreundlich, scLangmuir, scFirstOrderKinetic, scDualDomainWithSorption]);
end;

function TCustomModel.Mt3dMSImmobPorosityUsed(Sender: TObject): boolean;
begin
  result := Mt3dMSUsed(Sender) and ModflowPackages.Mt3dmsChemReact.IsSelected
    and (ModflowPackages.Mt3dmsChemReact.SorptionChoice in
    [scDualDomainNoSorption, scDualDomainWithSorption]);
end;

function TCustomModel.Mt3dMSUsed(Sender: TObject): boolean;
begin
  result := (ModelSelection in [msModflow, msModflowLGR, msModflowNWT])
    and ModflowPackages.Mt3dBasic.IsSelected;
end;

function TCustomModel.Trpy: TOneDRealArray;
begin
  result := LayerStructure.Trpy;
end;

function TCustomModel.TwoDElementCenter(const Column, Row: integer): TPoint2D;
var
  LocalGrid: TCustomModelGrid;
  LocalMesh: TSutraMesh3D;
begin
  LocalGrid := Grid;
  if LocalGrid <> nil then
  begin
    result := LocalGrid.TwoDElementCenter(Column, Row);
  end
  else
  begin
    LocalMesh := Mesh;
    Assert(LocalMesh <> nil);
    Assert(Row = 0);
    result := LocalMesh.Mesh2D.Elements[Column].Center;
  end;
end;

function TCustomModel.TwoDElementCorner(const Column, Row: integer): TPoint2D;
var
  LocalGrid: TCustomModelGrid;
  LocalMesh: TSutraMesh3D;
begin
  LocalGrid := Grid;
  if LocalGrid <> nil then
  begin
    result := LocalGrid.TwoDElementCorner(Column, Row);
  end
  else
  begin
    LocalMesh := Mesh;
    Assert(LocalMesh <> nil);
    Assert(Row = 0);
    result := LocalMesh.Mesh2D.Nodes[Column].Location;
  end;
end;

function TCustomModel.CheckMt3dTimes(ShowWarning: Boolean): boolean;
var
  ModflowEndTime: Double;
  MbResult: Integer;
  TimeIndex: Integer;
  Mt3dmsEndTime: Double;
begin
  result := True;
  ModflowEndTime := ModflowFullStressPeriods[
    ModflowFullStressPeriods.Count - 1].EndTime;
  Mt3dmsEndTime := Mt3dmsTimes[Mt3dmsTimes.Count - 1].EndTime;
  if Mt3dmsEndTime < ModflowEndTime then
  begin
    if ShowWarning then
    begin
      MbResult := Application.MessageBox(PChar(Format(StrTheFinalTimeSpeci,
        [Mt3dmsEndTime, ModflowEndTime])), PChar(StrTimesDoNotMatch),
        MB_ICONASTERISK or MB_YESNOCANCEL);
    end
    else
    begin
      MbResult := idYes
    end;
    if MbResult = idYes then
    begin
      for TimeIndex := ModflowFullStressPeriods.Count - 1 downto 0 do
      begin
        if ModflowFullStressPeriods[TimeIndex].EndTime > Mt3dmsEndTime then
        begin
          ModflowFullStressPeriods.Delete(TimeIndex);
        end
        else
        begin
          break;
        end;
      end;
    end;
    result := not (MbResult in [0,IDCANCEL]);
  end;
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
    // The most recent array is the one most likely to be identical.
    for Index := DataArrays.Count - 1 downto 0 do
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
      // a different data array was found so CachedDataArray
      // won't be checked first next time.
      // Cache its data.
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

function TCustomModel.IndexOfMt3dmsSpeciesName(
  const AChemSpecies: string): integer;
begin
  result := MobileComponents.IndexOfName(AChemSpecies);
  if result < 0 then
  begin
    result := ImmobileComponents.IndexOfName(AChemSpecies);
    if result >= 0 then
    begin
      result := result + MobileComponents.Count;
    end;
  end;
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
  TestFirstTime: Double;
  LastTestTime: Double;
  OutOfStartRange: Boolean;
  OutOfEndRange: Boolean;
  OutOfStartRangeScreenObjects: TStringList;
  OutOfEndRangeScreenObjects: TStringList;
  ErrorMessage: string;
  SpressPeriodCount: Integer;
  MtTime: TMt3dmsTimeItem;
  OutOfRangeMt3dTime: Boolean;
//  StartTimeIndex: integer;
begin
  if FUpdatingFullStressPeriods then
  begin
    Exit;
  end;
  frmErrorsAndWarnings.RemoveWarningGroup(self, StrAnyTimesAfterThe);
  frmErrorsAndWarnings.RemoveWarningGroup(self, StrAnyTimesBeforeThe);
  frmErrorsAndWarnings.RemoveWarningGroup(self, StrAddedTimes);


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
    TestFirstTime := TimeList[0];
    LastTestTime := TimeList[TimeList.Count-1];

    OutOfRangeMt3dTime := False;
    for TimeIndex := 0 to Mt3dmsTimes.Count - 1 do
    begin
      MtTime := Mt3dmsTimes[TimeIndex];
      if (MtTime.StartTime >= TestFirstTime)
        and (MtTime.StartTime <= LastTestTime) then
      begin
        TimeList.AddUnique(MtTime.StartTime);
      end
      else
      begin
        OutOfRangeMt3dTime := True;
      end;
      if (MtTime.EndTime >= TestFirstTime)
        and (MtTime.EndTime <= LastTestTime) then
      begin
        TimeList.AddUnique(MtTime.EndTime);
      end
      else
      begin
        OutOfRangeMt3dTime := True;
      end;
    end;

    if OutOfRangeMt3dTime then
    begin
      frmErrorsAndWarnings.AddWarning(self,
        StrInvalidTimesForMT, StrTheStressPeriodsD);
    end;

    OutOfStartRangeScreenObjects := TStringList.Create;
    OutOfEndRangeScreenObjects := TStringList.Create;
    try

      for ScreenObjectIndex := 0 to ScreenObjectCount - 1 do
      begin
        ScreenObject := ScreenObjects[ScreenObjectIndex];
        if ScreenObject.Deleted then
        begin
          Continue;
        end;
        SpressPeriodCount := TimeList.Count;
        ScreenObject.UpdateModflowTimes(TimeList,
          TestFirstTime, LastTestTime, OutOfStartRange, OutOfEndRange);
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if OutOfStartRange then
        begin
          OutOfStartRangeScreenObjects.Add(ScreenObject.Name);
        end;
        if OutOfEndRange then
        begin
          OutOfEndRangeScreenObjects.Add(ScreenObject.Name);
        end;
        if SpressPeriodCount <> TimeList.Count then
        begin
          frmErrorsAndWarnings.AddWarning(self, StrAddedTimes, ScreenObject.Name);
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
        ErrorMessage := Format(StrTheBeginningOfThe,
          [StressPeriod.StartTime, FirstTime]);
        OutOfStartRangeScreenObjects.Insert(0, ErrorMessage);

        frmErrorsAndWarnings.AddWarning(self,
          StrAnyTimesBeforeThe, OutOfStartRangeScreenObjects.Text);
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
        ErrorMessage := Format(StrTheEndOfTheLast,
          [StressPeriod.EndTime, EndTime]);
        OutOfEndRangeScreenObjects.Insert(0, ErrorMessage);
        frmErrorsAndWarnings.AddWarning(self,
          StrAnyTimesAfterThe,OutOfEndRangeScreenObjects.Text);
      end;
    finally
      OutOfEndRangeScreenObjects.Free;
      OutOfStartRangeScreenObjects.Free;
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

procedure TCustomModel.AdjustCellPosition(ACellAssignment: TCellAssignment);
begin
  // do nothing;
end;

procedure TCustomModel.AdjustResKvArray(Sender: TObject);
begin
  AdjustDataArray(Sender as TDataArray);
end;

procedure TCustomModel.AdjustCellPosition(AValueCell: TValueCell);
begin
  // do nothing
end;

procedure TCustomModel.GetDefaultOutputFileExtension(var Extension: string);
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
    else
      Assert(False);
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
    else
      Assert(False);
    end;
  end
  else if ModflowOutputControl.SaveCellFlows = csfBinary then
  begin
    Extension := StrCbcExt;
  end
  else
  begin
    Extension := '';
  end;
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

function TCustomModel.GetThreeDGridObserver: TObserver;
begin
  result := FThreeDGridObserver;
end;

function TCustomModel.GetTimeListByName(const AName: string): TCustomTimeList;
var
  Index: Integer;
  TimeList: TCustomTimeList;
begin
  Assert(AName <> '');
  result := nil;
  for Index := 0 to FTimeLists.Count - 1 do
  begin
    TimeList := FTimeLists[Index];
    if TimeList.Name = AName then
    begin
      result := TimeList;
      Exit;
    end;
  end;
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
  HobWriter := TModflowHobWriter.Create(Self, etDisplay);
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

procedure TCustomModel.InitializeSfrWriter(EvaluationType: TEvaluationType);
begin
  FreeAndNil(SfrWriter);
  FreeAndNil(LakWriter);
  SfrWriter := TModflowSFR_Writer.Create(self, EvaluationType);
  LakWriter := TModflowLAK_Writer.Create(self, EvaluationType);
end;

procedure TCustomModel.ModelObserversStopTalkingTo(Observer: TObserver);
begin
  FTopGridObserver.StopsTalkingTo(Observer);
  FThreeDGridObserver.StopsTalkingTo(Observer);
  FHufSyNotifier.StopsTalkingTo(Observer);
  FHufSsNotifier.StopsTalkingTo(Observer);
  FHufKzNotifier.StopsTalkingTo(Observer);
  FHufKyNotifier.StopsTalkingTo(Observer);
  FHufKxNotifier.StopsTalkingTo(Observer);

end;

function TCustomModel.ModflowConfiningBedCount: integer;
begin
  result := LayerStructure.ModflowConfiningBedCount;
end;

function TCustomModel.ModflowHobPackageUsed(Sender: TObject): boolean;
begin
  result := ModflowPackages.HobPackage.IsSelected;
end;

function TCustomModel.GetMesh: TSutraMesh3D;
begin
  {$IFDEF SUTRA}
  if ModelSelection = msSutra then
  begin
    result := SutraMesh;
  end
  else
  begin
    result := nil;
  end;
  {$ELSE}
  result := nil;
  {$ENDIF}
end;

procedure TCustomModel.GetMfHobHeadsUseList(Sender: TObject; NewUseList: TStringList);
begin
  NewUseList.Add(rsActive);
  // do nothing
end;

function TCustomModel.GetModflowLocation: string;
begin
  case ModelSelection of
    msModflow: result := ProgramLocations.ModflowLocation;
    msModflowLGR: result := ProgramLocations.ModflowLgrLocation;
    msModflowNWT: result := ProgramLocations.ModflowNwtLocation;
    else result := ProgramLocations.ModflowLocation;
  end;
end;

function TCustomModel.GetModPathLocation: string;
begin
  Result := '';
  case ModflowPackages.ModPath.MpathVersion of
    mp5: result := ProgramLocations.ModPathLocation;
    mp6: result := ProgramLocations.ModPathLocationVersion6;
    else Assert(False);
  end;
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
      if not CharInSet(AChar, [',', ' ']) then
      begin
        FirstChar := CharIndex;
        break;
      end;
    end;
    LastChar := Length(ALine);
    for CharIndex := FirstChar + 1 to Length(ALine) do
    begin
      AChar := ALine[CharIndex];
      if CharInSet(AChar, [',', ' ']) then
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

procedure TCustomModel.WriteLAYCB(const DiscretizationWriter: TObject);
begin
  LayerStructure.WriteLAYCB(DiscretizationWriter);
end;

procedure TCustomModel.WritePValAndTemplate(const ParameterName: string;
      const Value: double);
begin
  FPValFile.Add(ParameterName + ' ' + FortranFloatToStr(Value));
  FTemplate.Add(ParameterName + ' ' + UcodeDelimiter + ParameterName
    + '                  ' + UcodeDelimiter);
end;

procedure TCustomModel.UpdateHfb(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
begin
  frmProgressMM.Caption := 'Progress';
  frmProgressMM.Show;
  frmProgressMM.ShouldContinue := True;
  if FHfbWriter = nil then
  begin
    FHfbWriter := TModflowHfb_Writer.Create(Self, etDisplay);
  end;
  (FHfbWriter as TModflowHfb_Writer).UpdateDisplay;
  frmProgressMM.Hide;
  if frmErrorsAndWarnings.HasMessages then
  begin
    frmErrorsAndWarnings.Show;
  end;
  if self is TPhastModel then
  begin
    PhastModel := TPhastModel(self);
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      PhastModel.ChildModels[ChildIndex].ChildModel.UpdateHfb(Sender);
    end;
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

function TCustomModel.GetDisplayColumn: integer;
begin
  result := Grid.DisplayColumn;
end;

function TCustomModel.GetDisplayLayer: integer;
begin
  result := Grid.DisplayLayer;
end;

function TCustomModel.GetDisplayRow: integer;
begin
  result := Grid.DisplayRow;
end;

function TCustomModel.GetHeadObsResults: THeadObsCollection;
begin
  CreateHeadObsResults;
  result := FHeadObsResults;
end;

function TCustomModel.GetLayerGroupByLayer(const Layer: integer): TLayerGroup;
begin
  result := LayerStructure.GetLayerGroupByLayer(Layer);
end;

function TCustomModel.WettingActive: boolean;
begin
  result := ModflowWettingOptions.WettingActive
    and not ModflowPackages.UpwPackage.IsSelected;
end;

function TCustomModel.Chani: TOneDIntegerArray;
begin
  result := LayerStructure.Chani;
end;

function TCustomModel.CheckWetting: boolean;
var
  Group: TLayerGroup;
  LayerGroupIndex: Integer;
var
  WetErrorMessage: string;
begin
  result := False;
  frmErrorsAndWarnings.RemoveErrorGroup(self, WetError);
  if WettingActive then
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
    if ModflowPackages.LpfPackage.IsSelected
      or ModflowPackages.HufPackage.IsSelected then
    begin
      WetErrorMessage := StrAtLeastOneConvert;
    end
    else if ModflowPackages.BcfPackage.IsSelected then
    begin
      WetErrorMessage := StrAtLeastOneUnconfConvert;
    end
    else
    begin
      Assert(False);
    end;
    frmErrorsAndWarnings.AddError(self, WetError, WetErrorMessage);
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
  end;
  FileDir := ExtractFileDir(AFileName);
  if FileDir <> '' then
  begin
    FileName := IncludeTrailingPathDelimiter(FileDir) + FileName;
  end;
  result := FileName;
end;

procedure TPhastModel.ExportModflowLgrModel(const FileName: string;
  RunModel, ExportModpath, ExportZoneBudget, ShowWarning: boolean);
var
  NumberOfSteps: Integer;
  BatchFileLocation: string;
  ListFileName: string;
  ChildIndex2: Integer;
  ListFileNames: TStringList;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin

  if frmProgressMM = nil then
  begin
    frmProgressMM := TfrmProgressMM.Create(nil);
  end;
  try
    frmFormulaErrors.DelayShowing := True;
    try
      frmProgressMM.Prefix := 'File ';
      frmProgressMM.Caption := 'Exporting MODFLOW-LGR input files';
      frmProgressMM.btnAbort.Visible := True;
      frmProgressMM.ShouldContinue := True;
      frmProgressMM.Show;

      NumberOfSteps := CountStepsInExport+1;
      for ChildIndex2 := 0 to ChildModels.Count - 1 do
      begin
        NumberOfSteps := NumberOfSteps +
          ChildModels[ChildIndex2].ChildModel.CountStepsInExport;
      end;

      frmProgressMM.pbProgress.Max := NumberOfSteps;
      frmProgressMM.pbProgress.Position := 0;

      if not PrepareModflowFullStressPeriods(ShowWarning) then
      begin
        Exit;
      end;

      InternalExportModflowLgrFile(FileName);

      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      ListFileNames := TStringList.Create;
      try

        ListFileName := (NameFileWriter as TNameFileWriter).ListFileName;
        ListFileNames.Add(ListFileName);
        for ChildIndex := 0 to ChildModels.Count - 1 do
        begin
          ChildModel := ChildModels[ChildIndex].ChildModel;
          ListFileName := (ChildModel.NameFileWriter as TNameFileWriter).ListFileName;
          ListFileNames.Add(ListFileName);
        end;

        BatchFileLocation := WriteModflowBatchFile(
          ProgramLocations,
          ChangeFileExt(FileName, '.lgr'), ListFileNames,
          ModflowOptions.OpenInTextEditor, BatchFileAdditionsBeforeModel,
          BatchFileAdditionsAfterModel, ExportModpath, ExportZoneBudget);

      finally
        ListFileNames.Free;
      end;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      if RunModel then
      begin
        RunAProgram('"' + BatchFileLocation + '"');
//        WinExec(PAnsiChar(AnsiString('"' + BatchFileLocation + '"')), SW_SHOW);
      end;
    finally
      frmProgressMM.btnAbort.Visible := False;
      frmProgressMM.Hide;
      if frmProgressMM.Owner = nil then
      begin
        FreeAndNil(frmProgressMM);
      end;
      frmFormulaErrors.DelayShowing := False;
    end;
  except on E: EFCreateError do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TPhastModel.InternalExportModflowLgrFile(const FileName: string);
var
  ChildNameFile: string;
  ChildModel: TChildModel;
  ChildIndex: Integer;
  LgrWriter: TLgrWriter;
begin
  LgrWriter := TLgrWriter.Create(Self, etExport);
  try
    LgrWriter.WriteFile(FileName);
  finally
    LgrWriter.Free;
  end;

  InitializeGages;
  InitializeSfrWriter(etExport);
  try
    FPValFile.Clear;
    FTemplate.Clear;
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      ChildModel.FPValFile.Clear;
      ChildModel.FTemplate.Clear;
    end;

    InternalExportModflowModel(FileName, True);
    for ChildIndex := 0 to ChildModels.Count - 1 do
    begin
      ChildModel := ChildModels[ChildIndex].ChildModel;
      ChildNameFile := ChildModel.Child_NameFile_Name(FileName);
      ChildModel.InternalExportModflowModel(ChildNameFile, True);
    end;
  finally
    FreeSfrWriter;
  end;
end;

procedure TCustomModel.ExportModflowModel(const FileName: string; RunModel,
  ExportModpath, NewBudgetFileForModpath, ExportZoneBudget, ShowWarning: boolean);
var
  NumberOfSteps: Integer;
  BatchFileLocation: string;
  ListFileName: string;
  ListFileNames: TStringList;
begin

  if frmProgressMM = nil then
  begin
    frmProgressMM := TfrmProgressMM.Create(nil);
  end;
  try
    // Note: MODFLOW can not read Unicode text files.

    frmFormulaErrors.DelayShowing := True;
    try
      frmProgressMM.Prefix := 'File ';
      frmProgressMM.Caption := 'Exporting MODFLOW input files';
      frmProgressMM.btnAbort.Visible := True;
      frmProgressMM.ShouldContinue := True;
      frmProgressMM.Show;

      NumberOfSteps := CountStepsInExport;

      frmProgressMM.pbProgress.Max := NumberOfSteps;
      frmProgressMM.pbProgress.Position := 0;

      if not PrepareModflowFullStressPeriods(ShowWarning) then
      begin
        Exit;
      end;

      InitializeGages;
      InitializeSfrWriter(etExport);
      try
        FPValFile.Clear;
        FTemplate.Clear;
        InternalExportModflowModel(FileName, False);
      finally
        FreeSfrWriter;
      end;


      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      if ExportModpath then
      begin
        ExportModpathModel(ChangeFileExt(FileName, '.mpn'), False,
          NewBudgetFileForModpath, True);
      end;
      if ExportZoneBudget then
      begin
        ExportZoneBudgetModel(ChangeFileExt(FileName, StrZbzones), False, True);
      end;

      ListFileNames := TStringList.Create;
      try
        ListFileName := (NameFileWriter as TCustomNameFileWriter).ListFileName;
        ListFileNames.Add(ListFileName);

        BatchFileLocation := WriteModflowBatchFile(
          ProgramLocations,
          ChangeFileExt(FileName, '.nam'), ListFileNames,
          ModflowOptions.OpenInTextEditor, BatchFileAdditionsBeforeModel,
          BatchFileAdditionsAfterModel, ExportModpath, ExportZoneBudget);
          //RunZoneBudget.Bat
      finally
        ListFileNames.Free;
      end;

      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      if RunModel then
      begin
        RunAProgram('"' + BatchFileLocation + '"');
//        WinExec(PAnsiChar(AnsiString('"' + BatchFileLocation + '"')), SW_SHOW);
      end;
    finally
      frmProgressMM.btnAbort.Visible := False;
      frmProgressMM.Hide;
      if frmProgressMM.Owner = nil then
      begin
        FreeAndNil(frmProgressMM);
      end;
      frmFormulaErrors.DelayShowing := False;
    end;
  except on E: EFCreateError do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TCustomModel.InitializeGages;
begin
  FGages.Clear;
  FGageUnitNumber:= UnitNumbers.UnitNumber(StrUNIT);
end;

procedure TCustomModel.ExportLakePackage(const FileName: string);
var
  LocalNameWriter: TNameFileWriter;
begin
  LocalNameWriter := NameFileWriter as TNameFileWriter;
  SetCurrentNameFileWriter(LocalNameWriter);
  (LakWriter as TModflowLAK_Writer).WriteFile(FileName, FGageUnitNumber, Gages);
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
end;

procedure TCustomModel.ExportUzfPackage(const FileName: string);
var
  UzfWriter: TModflowUzfWriter;
begin
  UzfWriter := TModflowUzfWriter.Create(Self, etExport);
  try
    UzfWriter.WriteFile(FileName, FGageUnitNumber);
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
end;


procedure TCustomModel.EvaluateSfrPackage;
begin
  (SfrWriter as TModflowSFR_Writer).Evaluate;
end;

procedure TCustomModel.ExportSfrPackage(const FileName: string);
var
  LocalNameWriter: TNameFileWriter;
begin
  LocalNameWriter := NameFileWriter as TNameFileWriter;
  SetCurrentNameFileWriter(LocalNameWriter);
  (SfrWriter as TModflowSFR_Writer).WriteFile(FileName, FGageUnitNumber, Gages);
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
end;

procedure TCustomModel.InternalExportModflowModel(const FileName: string; ExportAllLgr: boolean);
var
  LocalNameWriter: TNameFileWriter;
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
  Mnw2Writer: TModflowMNW2_Writer;
  ZoneWriter: TModflowZoneWriter;
  MultiplierWriter: TModflowMultiplierWriter;
//  BatchFileLocation: string;
  GmgWriter: TGmgWriter;
  SipWriter: TSipWriter;
  De4Writer: TDe4Writer;
  OCWriter: TOutputControlWriter;
  GagWriter: TModflowGAG_Writer;
  HobWriter: TModflowHobWriter;
  HfbWriter: TModflowHfb_Writer;
//  NumberOfSteps: Integer;
  SubWriter: TModflowSUB_Writer;
  SwtWriter: TModflowSWT_Writer;
  HydModWriter: TModflowHydmodWriter;
//  ShouldExit: Boolean;
  MultipUsed: Boolean;
  ZUsed: Boolean;
  NwtWriter: TNwtWriter;
  UPW_Writer: TModflowUPW_Writer;
  ChildIndex: integer;
  LocalPhastModel : TPhastModel;
  ChildModel: TChildModel;
  ChildNameFile: string;
  WriterList: TSfrWriterList;
  ParentPhastModel: TPhastModel;
  LinkWriter: TModflowMt3dmsLinkWriter;
  PcgnWriter: TPcgnWriter;
begin
  // Note: MODFLOW can not read Unicode text files.

  Assert(Assigned(NameFileWriter));
  LocalNameWriter := NameFileWriter as TNameFileWriter;
  UpdateCurrentModel(self);
  try
    CheckWetting;

    SetCurrentDir(ExtractFileDir(FileName));
    TransientMultiplierArrays.Clear;
    TransientZoneArrays.Clear;
    ModflowSteadyParameters.ClearArrayNames;



    UsedMultiplierArrayNames.Clear;
    UsedZoneArrayNames.Clear;
    UsedMultiplierArrayNames.Sorted := True;
    UsedZoneArrayNames.Sorted := True;
    try
      SetCurrentNameFileWriter(LocalNameWriter);
      DisWriter := TModflowDiscretizationWriter.Create(self, etExport);
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

      BasicWriter := TModflowBasicWriter.Create(self, etExport);
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

      OCWriter := TOutputControlWriter.Create(self, etExport);
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

      PcgWriter := TPcgWriter.Create(self, etExport);
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

      PcgnWriter := TPcgnWriter.Create(self, etExport);
      try
        PcgnWriter.WriteFile(FileName);
      finally
        PcgnWriter.Free;
      end;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      if ModflowPackages.PcgnPackage.IsSelected then
      begin
        frmProgressMM.StepIt;
      end;

      GmgWriter := TGmgWriter.Create(self, etExport);
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

      SipWriter := TSipWriter.Create(self, etExport);
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

      De4Writer := TDe4Writer.Create(self, etExport);
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

      NwtWriter := TNwtWriter.Create(self, etExport);
      try
        NwtWriter.WriteFile(FileName);
      finally
        NwtWriter.Free;
      end;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      if ModflowPackages.NwtPackage.IsSelected then
      begin
        frmProgressMM.StepIt;
      end;

      LPF_Writer := TModflowLPF_Writer.Create(self, etExport);
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

      BCF_Writer := TModflowBCF_Writer.Create(self, etExport);
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

      HUF_Writer := TModflowHUF_Writer.Create(self, etExport);
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

      KDEP_Writer := TModflowKDEP_Writer.Create(self, etExport);
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

      LVDA_Writer := TModflowLVDA_Writer.Create(self, etExport);
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

      UPW_Writer := TModflowUPW_Writer.Create(self, etExport);
      try
        UPW_Writer.WriteFile(FileName);
      finally
        UPW_Writer.Free;
      end;
      FDataArrayManager.CacheDataArrays;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      if ModflowPackages.UpwPackage.IsSelected then
      begin
        frmProgressMM.StepIt;
      end;

      LinkWriter := TModflowMt3dmsLinkWriter.Create(self, etExport);
      try
        LinkWriter.WriteFile(FileName);
      finally
        LinkWriter.Free;
      end;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      ChdWriter := TModflowCHD_Writer.Create(self, etExport);
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

      GhbWriter := TModflowGHB_Writer.Create(self, etExport);
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

      WellWriter := TModflowWEL_Writer.Create(self, etExport);
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

      RivWriter := TModflowRIV_Writer.Create(self, etExport);
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

      DrnWriter := TModflowDRN_Writer.Create(self, etExport);
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

      DrtWriter := TModflowDRT_Writer.Create(self, etExport);
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

      RchWriter := TModflowRCH_Writer.Create(self, etExport);
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

      EvtWriter := TModflowEVT_Writer.Create(self, etExport);
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

      EtsWriter := TModflowETS_Writer.Create(self, etExport);
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

      ResWriter := TModflowRES_Writer.Create(self, etExport);
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

      Mnw2Writer := TModflowMNW2_Writer.Create(self, etExport);
      try
        Mnw2Writer.WriteFile(FileName);
        Mnw2Writer.WriteMnwiFile(FileName, FGageUnitNumber);
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

      if Self is TPhastModel then
      begin
        LocalPhastModel := TPhastModel(Self);
        ExportLakePackage(FileName);
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ExportAllLgr then
        begin
          for ChildIndex := 0 to LocalPhastModel.ChildModels.Count - 1 do
          begin
            ChildModel := LocalPhastModel.ChildModels[ChildIndex].ChildModel;
            ChildNameFile := ChildModel.Child_NameFile_Name(FileName);
            ChildModel.ExportLakePackage(ChildNameFile);
            if not frmProgressMM.ShouldContinue then
            begin
              Exit;
            end;
          end;
        end;
      end
      else
      begin
        if not ExportAllLgr then
        begin
          ExportLakePackage(FileName);
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
        end;
      end;
      SetCurrentNameFileWriter(LocalNameWriter);

      // SfrWriter requires that LakWriter be completed first
      // so that TScreenObject.ModflowLakBoundary.TrueLakeID
      // is set properly.
      if Self is TPhastModel then
      begin
        LocalPhastModel := TPhastModel(Self);
        EvaluateSfrPackage;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;

        if LocalPhastModel.LgrUsed then
        begin
          WriterList := TSfrWriterList.Create;
          try
            WriterList.Add(SfrWriter as TModflowSFR_Writer);
            for ChildIndex := 0 to LocalPhastModel.ChildModels.Count - 1 do
            begin
              ChildModel := LocalPhastModel.ChildModels[ChildIndex].ChildModel;
              WriterList.Add(ChildModel.SfrWriter as TModflowSFR_Writer);
              ChildModel.EvaluateSfrPackage;
              if not frmProgressMM.ShouldContinue then
              begin
                Exit;
              end;
            end;
            (SfrWriter as TModflowSFR_Writer).AssociateLgrSubSegments(WriterList);
          finally
            WriterList.Free;
          end;
        end;

        ExportSfrPackage(FileName);
        ExportUzfPackage(FileName);
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        if ExportAllLgr then
        begin
          for ChildIndex := 0 to LocalPhastModel.ChildModels.Count - 1 do
          begin
            ChildModel := LocalPhastModel.ChildModels[ChildIndex].ChildModel;
            ChildNameFile := ChildModel.Child_NameFile_Name(FileName);
            ChildModel.ExportSfrPackage(ChildNameFile);
            if not frmProgressMM.ShouldContinue then
            begin
              Exit;
            end;
            ChildModel.ExportUzfPackage(ChildNameFile);
            if not frmProgressMM.ShouldContinue then
            begin
              Exit;
            end;
          end;
        end;
      end
      else
      begin
        if not ExportAllLgr then
        begin
          ParentPhastModel := (self as TChildModel).ParentModel as TPhastModel;
          ParentPhastModel.EvaluateSfrPackage;
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
          WriterList := TSfrWriterList.Create;
          try
            WriterList.Add(ParentPhastModel.SfrWriter as TModflowSFR_Writer);
            for ChildIndex := 0 to ParentPhastModel.ChildModels.Count - 1 do
            begin
              ChildModel := ParentPhastModel.ChildModels[ChildIndex].ChildModel;
              WriterList.Add(ChildModel.SfrWriter as TModflowSFR_Writer);
              ChildModel.EvaluateSfrPackage;
              if not frmProgressMM.ShouldContinue then
              begin
                Exit;
              end;
            end;
            (ParentPhastModel.SfrWriter as TModflowSFR_Writer).AssociateLgrSubSegments(WriterList);
          finally
            WriterList.Free;
          end;
          ExportSfrPackage(FileName);
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
          ExportUzfPackage(FileName);
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
        end;
      end;
      SetCurrentNameFileWriter(LocalNameWriter);

      HydModWriter := TModflowHydmodWriter.Create(self, etExport);
      try
        HydModWriter.WriteFile(FileName, (SfrWriter as TModflowSFR_Writer));
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


      // GagWriter requires that LakWriter and SfrWriter be completed
      // first so that the data in Gages is set.
      GagWriter := TModflowGAG_Writer.Create(self, etExport);
      try
        GagWriter.WriteFile(FileName, Gages, (SfrWriter as TModflowSFR_Writer), FGageUnitNumber);
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

      HfbWriter := TModflowHfb_Writer.Create(Self, etExport);
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

      SubWriter := TModflowSUB_Writer.Create(Self, etExport);
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

      SwtWriter := TModflowSWT_Writer.Create(Self, etExport);
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
      ZoneWriter := TModflowZoneWriter.Create(self, etExport);
      try
        ZUsed := ZoneWriter.WriteFile(FileName);
      finally
        ZoneWriter.Free;
      end;
      FDataArrayManager.CacheDataArrays;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      if ZUsed then
      begin
        frmProgressMM.StepIt;
      end;

      // It is important that the TModflowMultiplierWriter not be used
      // until after all the multiplier arrays have been identified through the
      // export of TModflowLPF_Writer and any other packages that use
      // multiplier arrays.
      MultiplierWriter := TModflowMultiplierWriter.Create(self, etExport);
      try
        MultipUsed := MultiplierWriter.WriteFile(FileName);
      finally
        MultiplierWriter.Free;
      end;
      FDataArrayManager.CacheDataArrays;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      if MultipUsed then
      begin
        frmProgressMM.StepIt;
      end;

      HobWriter := TModflowHobWriter.Create(Self, etExport);
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

      LocalNameWriter.SaveNameFile(FileName);
    finally
      SetCurrentNameFileWriter(nil);
    end;
  finally
    UpdateCurrentModel(SelectedModel);
  end;
end;

procedure TCustomModel.ExportModpathModel(FileName: string;
      RunModel, NewBudgetFile: boolean; EmbeddedExport: boolean = False);
var
  StartLocations: TModpathStartingLocationsWriter;
  MainFileWriter: TModpathMainFileWriter;
  TimeFileWriter: TModpathTimeFileWriter;
  Responses: TModpathResponseFileWriter;
  NameFileWriter: TModpathNameFileWriter;
  BatchFileLocation: string;
  LargeBudgetFileResponse: string;
  BasicFileWriter : TModpathBasicFileWriter;
  SimFileWriter : TModpathSimFileWriter;
  LogFileName: string;
begin
  // Note: MODPATH can not read Unicode text files.

  if not TestModpathOK then
  begin
    Exit;
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

      case ModflowPackages.ModPath.MpathVersion of
        mp5:
          begin
            NameFileWriter := TModpathNameFileWriter.Create;
            try
              NameFileWriter.WriteFile(FileName, self, EmbeddedExport);
            finally
              NameFileWriter.Free;
            end;

            MainFileWriter := TModpathMainFileWriter.Create(Self, etExport);
            try
              MainFileWriter.WriteFile(FileName);
            finally
              MainFileWriter.Free;
            end;
            StartLocations := TModpathStartingLocationsWriter.Create(Self, etExport);
            try
              StartLocations.WriteFile(FileName);
            finally
              StartLocations.Free;
            end;
            if ModflowPackages.ModPath.ShouldCreateTimeFile then
            begin
              TimeFileWriter := TModpathTimeFileWriter.Create(Self, etExport);
              try
                TimeFileWriter.WriteFile(FileName);
              finally
                TimeFileWriter.Free;
              end;
            end;
            Responses := TModpathResponseFileWriter.Create(Self, etExport);
            try
              Responses.WriteFile(FileName, NewBudgetFile);
              LargeBudgetFileResponse := Responses.FLargeBudgetFileResponse;
            finally
              Responses.Free;
            end;

            BatchFileLocation := WriteModpathBatchFile(ProgramLocations, FileName,
              ChangeFileExt(FileName,'.mplst'), RunModel, LargeBudgetFileResponse,
              EmbeddedExport);

          end;
        mp6:
          begin
            NameFileWriter := TModpathNameFileWriter.Create;
            try
              NameFileWriter.WriteFileVersion6(FileName, self, EmbeddedExport);
            finally
              NameFileWriter.Free;
            end;

            BasicFileWriter := TModpathBasicFileWriter.Create(Self, etExport);
            try
              BasicFileWriter.WriteFile(FileName);
            finally
              BasicFileWriter.Free;
            end;
            StartLocations := TModpathStartingLocationsWriter.Create(Self, etExport);
            try
              StartLocations.WriteFileVersion6(FileName);
            finally
              StartLocations.Free;
            end;

            SimFileWriter := TModpathSimFileWriter.Create(Self, etExport);
            try
              SimFileWriter.WriteFile(FileName);
            finally
              SimFileWriter.Free;
            end;

            BatchFileLocation := WriteModPathBatchFileVersion6(ProgramLocations,
              FileName, ChangeFileExt(FileName,'.mplst'), RunModel,
              EmbeddedExport);
          end;
        else
          Assert(False);
      end;
      if RunModel then
      begin
        RunAProgram('"' + BatchFileLocation + '"');
      end;
    except on E: EFCreateError do
      begin
        Beep;
        MessageDlg(E.Message, mtError, [mbOK], 0);
      end;
    end;
    if (ModflowPackages.ModPath.MpathVersion = mp6) then
    begin
      LogFileName := IncludeTrailingPathDelimiter(ExtractFileDir(FileName))
        + 'MPATH6.LOG';
      AddFileToArchive(LogFileName);
    end;
  finally
    if frmProgressMM.Owner = nil then
    begin
      FreeAndNil(frmProgressMM);
    end;

//    ReclaimMemory;
  end;
end;

procedure TCustomModel.ExportMt3dmsModel(const FileName: string;
  RunModel, ShowWarning: Boolean);
var
  NumberOfSteps: integer;
  Mt3dmsBtnWriter: TMt3dmsBtnWriter;
  Mt3dmsAdvWriter: TMt3dmsAdvWriter;
  Mt3dmsDspWriter: TMt3dmsDspWriter;
  Mt3dmsSsmWriter: TMt3dmsSsmWriter;
  Mt3dmsRctWriter: TMt3dmsRctWriter;
  Mt3dmsGcgWriter: TMt3dmsGcgWriter;
  Mt3dmsTobWriter: TMt3dmsTobWriter;
  LocalNameWriter: TMt3dmsNameWriter;
  ListFileNames : TStringList;
  ListFileName: string;
  BatchFileLocation: string;
begin
  // Note: MT3DMS can not read Unicode text files.

  if frmProgressMM = nil then
  begin
    frmProgressMM := TfrmProgressMM.Create(nil);
  end;
  try
    frmFormulaErrors.DelayShowing := True;
    try
      frmProgressMM.Prefix := 'File ';
      frmProgressMM.Caption := 'Exporting MT3DMS input files';
      frmProgressMM.btnAbort.Visible := True;
      frmProgressMM.ShouldContinue := True;
      frmProgressMM.Show;

      NumberOfSteps := CountStepsInMt3dExport;

      frmProgressMM.pbProgress.Max := NumberOfSteps;
      frmProgressMM.pbProgress.Position := 0;

      if not PrepareModflowFullStressPeriods(ShowWarning) then
      begin
        Exit;
      end;
      if not CheckMt3dTimes(ShowWarning) then
      begin
        Exit;
      end;

      Assert(Assigned(NameFileWriter));
      LocalNameWriter := NameFileWriter as TMt3dmsNameWriter;
      SetCurrentNameFileWriter(LocalNameWriter);
      UpdateCurrentModel(self);
      try
        SetCurrentDir(ExtractFileDir(FileName));

        Mt3dmsBtnWriter := TMt3dmsBtnWriter.Create(Self, etExport);
        try
          Mt3dmsBtnWriter.WriteFile(FileName);
        finally
          Mt3dmsBtnWriter.Free;
        end;

        Mt3dmsAdvWriter := TMt3dmsAdvWriter.Create(Self, etExport);
        try
          Mt3dmsAdvWriter.WriteFile(FileName);
        finally
          Mt3dmsAdvWriter.Free;
        end;

        Mt3dmsDspWriter := TMt3dmsDspWriter.Create(Self, etExport);
        try
          Mt3dmsDspWriter.WriteFile(FileName);
        finally
          Mt3dmsDspWriter.Free;
        end;

        Mt3dmsSsmWriter := TMt3dmsSsmWriter.Create(Self, etExport);
        try
          Mt3dmsSsmWriter.WriteFile(FileName);
        finally
          Mt3dmsSsmWriter.Free;
        end;

        Mt3dmsRctWriter := TMt3dmsRctWriter.Create(Self, etExport);
        try
          Mt3dmsRctWriter.WriteFile(FileName);
        finally
          Mt3dmsRctWriter.Free;
        end;

        Mt3dmsGcgWriter := TMt3dmsGcgWriter.Create(Self, etExport);
        try
          Mt3dmsGcgWriter.WriteFile(FileName);
        finally
          Mt3dmsGcgWriter.Free;
        end;

        Mt3dmsTobWriter := TMt3dmsTobWriter.Create(Self, etExport);
        try
          Mt3dmsTobWriter.WriteFile(FileName, ObservationPurpose);
        finally
          Mt3dmsTobWriter.Free;
        end;

        LocalNameWriter.SaveNameFile(FileName);

        ListFileNames := TStringList.Create;
        try
          ListFileName := LocalNameWriter.ListFileName;
          ListFileNames.Add(ListFileName);

          BatchFileLocation := WriteMt3dmsBatchFile(
            ProgramLocations,
            ChangeFileExt(FileName, StrMtName), ListFileNames,
            ModflowOptions.OpenInTextEditor);
        finally
          ListFileNames.Free;
        end;

      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      if RunModel then
      begin
        RunAProgram('"' + BatchFileLocation + '"');
      end;

      finally
        UpdateCurrentModel(SelectedModel);
        SetCurrentNameFileWriter(nil);
      end;

    finally
      frmProgressMM.btnAbort.Visible := False;
      frmProgressMM.Hide;
      if frmProgressMM.Owner = nil then
      begin
        FreeAndNil(frmProgressMM);
      end;
      frmFormulaErrors.DelayShowing := False;
    end;
  except on E: EFCreateError do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TCustomModel.ExportSeparateLgrModel(const FileName: string;
  RunModel, ExportModpath, ExportZoneBudget, ShowWarning: boolean);
var
  IUPBHSV: Integer;
  IUPBFSV: Integer;
  NameFile: string;
  HeadFile: string;
  ANameFileWriter: TNameFileWriter;
  FlowFile: string;
  ListFileNames: TStringList;
  ListFileName: string;
  BatchFileLocation: string;
  FileDir: string;
begin
  if frmProgressMM = nil then
  begin
    frmProgressMM := TfrmProgressMM.Create(nil);
  end;
  try
    frmFormulaErrors.DelayShowing := True;
    try
      frmProgressMM.Prefix := 'File ';
      frmProgressMM.Caption := 'Exporting MODFLOW-LGR input files';
      frmProgressMM.btnAbort.Visible := True;
      frmProgressMM.ShouldContinue := True;
      frmProgressMM.Show;


      frmProgressMM.pbProgress.Max := CountStepsInExport;
      frmProgressMM.pbProgress.Position := 0;

      if not PrepareModflowFullStressPeriods(ShowWarning) then
      begin
        Exit;
      end;

      InitializeGages;
      if self is TChildModel then
      begin
        TChildModel(self).ParentModel.InitializeSfrWriter(etExport);
      end
      else
      begin
        InitializeSfrWriter(etExport);
      end;
      try
        IUPBHSV := UnitNumbers.UnitNumber(BFH_Heads);
        IUPBFSV := UnitNumbers.UnitNumber(BFH_Fluxes);
        NameFile := FixFileName(ExtractFileName(FileName));

        if self is TChildModel then
        begin
          FileDir := ExtractFilePath(FileName);
          NameFile := FileDir + TChildModel(self).Child_NameFile_Name(NameFile);
        end;

        HeadFile := ChangeFileExt(NameFile, '.bfh_head');
        FlowFile := ChangeFileExt(NameFile, '.bfh_flux');
        ANameFileWriter := NameFileWriter as TNameFileWriter;
        SetCurrentNameFileWriter(ANameFileWriter);
        if self is TChildModel then
        begin
          ANameFileWriter.WriteToNameFile('BFH', IUPBHSV, HeadFile, foInputAlreadyExists);
          ANameFileWriter.WriteToNameFile(StrDATA, IUPBFSV, FlowFile, foInputAlreadyExists);
        end
        else
        begin
          ANameFileWriter.WriteToNameFile(StrDATA, IUPBHSV, HeadFile, foInputAlreadyExists);
          ANameFileWriter.WriteToNameFile('BFH', IUPBFSV, FlowFile, foInputAlreadyExists);
        end;

        FPValFile.Clear;
        FTemplate.Clear;
        InternalExportModflowModel(NameFile, False);
      finally
        if self is TChildModel then
        begin
          TChildModel(self).ParentModel.FreeSfrWriter;
        end
        else
        begin
          FreeSfrWriter;
        end;
      end;

      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;




      ListFileNames := TStringList.Create;
      try

        ListFileName := (NameFileWriter as TNameFileWriter).ListFileName;
        ListFileNames.Add(ListFileName);

        BatchFileLocation := WriteModflowBatchFile(
          ProgramLocations,
          ChangeFileExt(NameFile, '.nam'), ListFileNames,
          ModflowOptions.OpenInTextEditor, BatchFileAdditionsBeforeModel,
          BatchFileAdditionsAfterModel, ExportModpath, ExportZoneBudget);

      finally
        ListFileNames.Free;
      end;

      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      if RunModel then
      begin
//        WinExec(PAnsiChar(AnsiString('"' + BatchFileLocation + '"')), SW_SHOW);
        RunAProgram('"' + BatchFileLocation + '"');
      end;

    finally
      frmProgressMM.btnAbort.Visible := False;
      frmProgressMM.Hide;
      if frmProgressMM.Owner = nil then
      begin
        FreeAndNil(frmProgressMM);
      end;
      frmFormulaErrors.DelayShowing := False;
    end;
  except on E: EFCreateError do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;end;

procedure TCustomModel.ExportZoneBudgetModel(FileName: string; RunModel, EmbeddedExport: boolean);
var
  NumberOfSteps: Integer;
  ZoneFileWriter: TZoneBudgetZoneFileWriter;
  ResponseFileWriter: TZoneBudgetResponseFileWriter;
  BatchFileLocation: string;
begin
  // Note: ZONEBUDGET can not read Unicode text files.

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

      ZoneFileWriter := TZoneBudgetZoneFileWriter.Create(self, etExport);
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

      ResponseFileWriter := TZoneBudgetResponseFileWriter.Create(self, etExport, EmbeddedExport);
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

      BatchFileLocation := WriteZoneBudgetBatchFile(self, FileName, RunModel, EmbeddedExport);

      if RunModel then
      begin
        RunAProgram('"' + BatchFileLocation + '"');
//        WinExec(PAnsiChar(AnsiString('"' + BatchFileLocation + '"')), SW_SHOW);
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
  ParameterUsed := (ModelSelection in [msModflow, msModflowLGR, msModflowNWT])
    and (ModflowPackages.LpfPackage.IsSelected
    or ModflowPackages.UpwPackage.IsSelected);
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

procedure TChildModelCollection.Assign(Source: TPersistent);
begin
  inherited;
  UpdateUnitNumbers;
end;

constructor TChildModelCollection.Create(Model: TBaseModel);
begin
  inherited Create(TChildModelItem, Model);
end;

procedure TChildModelCollection.UpdateUnitNumbers;
const
  // It is assumed here that  MODFLOW will never have more than 1000
  // files included in the name file.
  UnitNumberOffset = 1000;
var
  ItemIndex: Integer;
begin
  for ItemIndex := 0 to Count - 1 do
  begin
    Items[ItemIndex].ChildModel.UnitNumbers.AddedValue :=
      (ItemIndex + 1) * UnitNumberOffset;
  end;
end;

function TChildModelCollection.GetItem(Index: integer): TChildModelItem;
begin
  result := inherited Items[Index] as TChildModelItem;
end;

procedure TChildModelCollection.Loaded;
var
  ItemIndex: Integer;
begin
  for ItemIndex := 0 to Count - 1 do
  begin
    Items[ItemIndex].Loaded;
  end;
  UpdateUnitNumbers;
end;

procedure TChildModelCollection.SetItem(Index: integer;
  const Value: TChildModelItem);
begin
  inherited Items[Index] := Value;
end;

{ TChildModelItem }

procedure TChildModelItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.

  if Source is TChildModelItem then
  begin
    ChildModel.Assign(TChildModelItem(Source).ChildModel);
  end
  else if Source is TChildModelEdit then
  begin
    ChildModel.Assign(TChildModelEdit(Source));
  end
  else
  begin
    inherited;
  end;
end;

constructor TChildModelItem.Create(Collection: TCollection);
begin
  inherited;
  FChildModel := TChildModel.Create(Model as TCustomModel);
  if FChildModel.FParentModel <> nil then
  begin
    FChildModel.Assign(FChildModel.FParentModel);
  end;
  FChildModel.SetSubComponent(True);
end;

destructor TChildModelItem.Destroy;
begin
  FChildModel.Free;
  inherited;
end;

function TChildModelItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  AnotherChildItem: TChildModelItem;
  ChildEdit: TChildModelEdit;
begin
    // When editing this section, be sure to edit TChildModelEdit
    // and TChildModel too

  result := AnotherItem is TChildModelItem;
  if result then
  begin
    AnotherChildItem := TChildModelItem(AnotherItem);
    result := ChildModel = AnotherChildItem.ChildModel;
    if not result and (ChildModel <> nil)
      and (AnotherChildItem.ChildModel <> nil) then
    begin
      result :=
        (ChildModel.ModelName = AnotherChildItem.ChildModel.ModelName)
        and (ChildModel.ChildCellsPerParentCell = AnotherChildItem.ChildModel.ChildCellsPerParentCell)
        and (ChildModel.StartingHeadSource = AnotherChildItem.ChildModel.StartingHeadSource)
//        and (ChildModel.OneWayCoupling = AnotherChildItem.ChildModel.OneWayCoupling)
        and (ChildModel.CouplingMethod = AnotherChildItem.ChildModel.CouplingMethod)
        and (ChildModel.MaxIterations = AnotherChildItem.ChildModel.MaxIterations)
        and (ChildModel.LgrPrintChoice = AnotherChildItem.ChildModel.LgrPrintChoice)
        and (ChildModel.HeadRelaxationFactor = AnotherChildItem.ChildModel.HeadRelaxationFactor)
        and (ChildModel.FluxRelaxationFactor = AnotherChildItem.ChildModel.FluxRelaxationFactor)
        and (ChildModel.HeadClosureCriterion = AnotherChildItem.ChildModel.HeadClosureCriterion)
        and (ChildModel.FluxClosureCriterion = AnotherChildItem.ChildModel.FluxClosureCriterion)
        and ChildModel.Discretization.IsSame(AnotherChildItem.ChildModel.Discretization);
    end;
  end
  else if AnotherItem is TChildModelEdit then
  begin
    ChildEdit := TChildModelEdit(AnotherItem);
    result := ChildModel = ChildEdit.FChildModel;
    if result then
    begin
      result :=
        (ChildModel.ModelName = ChildEdit.ModelName)
        and (ChildModel.ChildCellsPerParentCell = ChildEdit.ChildCellsPerParentCell)
        and (ChildModel.StartingHeadSource = ChildEdit.StartingHeadSource)
//        and (ChildModel.OneWayCoupling = ChildEdit.OneWayCoupling)
        and (ChildModel.CouplingMethod = ChildEdit.CouplingMethod)
        and (ChildModel.MaxIterations = ChildEdit.MaxIterations)
        and (ChildModel.LgrPrintChoice = ChildEdit.LgrPrintChoice)
        and (ChildModel.HeadRelaxationFactor = ChildEdit.HeadRelaxationFactor)
        and (ChildModel.FluxRelaxationFactor = ChildEdit.FluxRelaxationFactor)
        and (ChildModel.HeadClosureCriterion = ChildEdit.HeadClosureCriterion)
        and (ChildModel.FluxClosureCriterion = ChildEdit.FluxClosureCriterion)
        and ChildModel.Discretization.IsSame(ChildEdit.Discretization)
    end;
  end;

end;

procedure TChildModelItem.Loaded;
begin
  ChildModel.Loaded;
end;

procedure TChildModelItem.SetChildModel(const Value: TChildModel);
begin
  FChildModel.Assign(Value);
end;

{ TChildModel }

procedure TChildModel.AdjustCellPosition(AValueCell: TValueCell);
var
  Column: Integer;
  Row: Integer;
  Layer: Integer;
begin
  Column := AValueCell.Column;
  Row := AValueCell.Row;
  Layer := AValueCell.Layer;
  AdjustCellPosition(Column, Row, Layer);
  AValueCell.Column := Column;
  AValueCell.Row := Row;
  AValueCell.Layer := Layer;
//
//  if AValueCell.Column = 0 then
//  begin
//    AValueCell.Column := 1;
//  end;
//  if AValueCell.Column = ModflowGrid.ColumnCount-1 then
//  begin
//    AValueCell.Column := ModflowGrid.ColumnCount-2;
//  end;
//  if AValueCell.Row = 0 then
//  begin
//    AValueCell.Row := 1;
//  end;
//  if AValueCell.Row = ModflowGrid.RowCount-1 then
//  begin
//    AValueCell.Row := ModflowGrid.RowCount-2;
//  end;
//  if AValueCell.Layer = ModflowGrid.LayerCount-1 then
//  begin
//    if (Discretization.BottomLayerGroup <> LayerStructure.Last)
//      or (Discretization.BottomLayerInUnit <> Discretization.BottomLayerGroup.LayerCount-1) then
//    begin
//      AValueCell.Layer := ModflowGrid.LayerCount-2
//    end;
//  end;
end;

procedure TChildModel.AdjustCellPosition(ACellAssignment: TCellAssignment);
var
  Column: Integer;
  Row: Integer;
  Layer: Integer;
begin
  Column := ACellAssignment.Column;
  Row := ACellAssignment.Row;
  Layer := ACellAssignment.Layer;
  AdjustCellPosition(Column, Row, Layer);
  ACellAssignment.Column := Column;
  ACellAssignment.Row := Row;
  ACellAssignment.Layer := Layer;
end;

procedure TChildModel.AdjustCellPosition(var Column, Row, Layer: integer);
begin
  if Column = 0 then
  begin
    Column := 1;
  end;
  if Column = ModflowGrid.ColumnCount-1 then
  begin
    Column := ModflowGrid.ColumnCount-2;
  end;
  if Row = 0 then
  begin
    Row := 1;
  end;
  if Row = ModflowGrid.RowCount-1 then
  begin
    Row := ModflowGrid.RowCount-2;
  end;
  if Layer = ModflowGrid.LayerCount-1 then
  begin
    if (Discretization.BottomLayerGroup <> LayerStructure.Last)
      or (Discretization.BottomLayerInUnit <> Discretization.BottomLayerGroup.LayerCount-1) then
    begin
      Layer := ModflowGrid.LayerCount-2
    end;
  end;
end;

procedure TChildModel.AdjustDataArray(ADataArray: TDataArray);
var
  AnnotationList: TStringList;
  ColIndex: Integer;
  LayerIndex: Integer;
  NewAnnotation: string;
  AnnotationPosition: Integer;
  OuterCellArea: Real;
  InnerCellArea: Real;
  OuterGridIndex: Integer;
  InnerGridIndex: Integer;
  RowIndex: Integer;
begin
  Assert(ADataArray.DataType in [rdtDouble, rdtBoolean]);
  Assert(ADataArray.ColumnCount >= 3);
  Assert(ADataArray.RowCount >= 3);
  AnnotationList := TStringList.Create;
  try
    AnnotationList.Sorted := True;
    for LayerIndex := 0 to ADataArray.LayerCount - 1 do
    begin
      for ColIndex := 0 to ADataArray.ColumnCount - 1 do
      begin
        OuterGridIndex := 0;
        InnerGridIndex := 1;
        if ADataArray.IsValue[LayerIndex, OuterGridIndex, ColIndex] then
        begin
          OuterCellArea := ModflowGrid.ColumnWidth[ColIndex]
            * ModflowGrid.RowWidth[OuterGridIndex];
          InnerCellArea := ModflowGrid.ColumnWidth[ColIndex]
            * ModflowGrid.RowWidth[InnerGridIndex];
          if ADataArray.IsValue[LayerIndex, InnerGridIndex, ColIndex] then
          begin
            NewAnnotation := 'Combined values for LGR: '
              + ADataArray.Annotation[LayerIndex, OuterGridIndex, ColIndex]
              + ', ' + ADataArray.Annotation[LayerIndex, InnerGridIndex, ColIndex];
            AnnotationPosition := AnnotationList.IndexOf(NewAnnotation);
            if AnnotationPosition >= 0 then
            begin
              NewAnnotation := AnnotationList[AnnotationPosition];
            end
            else
            begin
              AnnotationList.Add(NewAnnotation)
            end;
            case ADataArray.DataType of
              rdtDouble:
                ADataArray.RealData[LayerIndex, InnerGridIndex, ColIndex]
                  := ADataArray.RealData[LayerIndex, InnerGridIndex, ColIndex]
                  + ADataArray.RealData[LayerIndex, OuterGridIndex, ColIndex]
                  * OuterCellArea / InnerCellArea;
              rdtBoolean:
                ADataArray.BooleanData[LayerIndex, InnerGridIndex, ColIndex] :=
                  ADataArray.BooleanData[LayerIndex, InnerGridIndex, ColIndex]
                  or ADataArray.BooleanData[LayerIndex, OuterGridIndex, ColIndex];
              else Assert(False);
            end;
            ADataArray.Annotation[LayerIndex, InnerGridIndex, ColIndex]
              := NewAnnotation;
          end
          else
          begin
            case ADataArray.DataType of
              rdtDouble:
                ADataArray.RealData[LayerIndex, InnerGridIndex, ColIndex]
                  := ADataArray.RealData[LayerIndex, OuterGridIndex, ColIndex]
                  * OuterCellArea / InnerCellArea;
              rdtBoolean:
                ADataArray.BooleanData[LayerIndex, InnerGridIndex, ColIndex]
                  := ADataArray.BooleanData[LayerIndex, OuterGridIndex, ColIndex];
              else Assert(False);
            end;
            ADataArray.Annotation[LayerIndex, InnerGridIndex, ColIndex] :=
              ADataArray.Annotation[LayerIndex, OuterGridIndex, ColIndex];
          end;
          case ADataArray.DataType of
            rdtDouble:
              ADataArray.RealData[LayerIndex, OuterGridIndex, ColIndex] := 0;
            rdtBoolean:
              ADataArray.BooleanData[LayerIndex, OuterGridIndex, ColIndex] := False;
            else Assert(False);
          end;
        end;

        OuterGridIndex := ADataArray.RowCount-1;
        InnerGridIndex := ADataArray.RowCount-2;
        if ADataArray.IsValue[LayerIndex, OuterGridIndex, ColIndex] then
        begin
          OuterCellArea := ModflowGrid.ColumnWidth[ColIndex]
            * ModflowGrid.RowWidth[OuterGridIndex];
          InnerCellArea := ModflowGrid.ColumnWidth[ColIndex]
            * ModflowGrid.RowWidth[InnerGridIndex];
          if ADataArray.IsValue[LayerIndex, InnerGridIndex, ColIndex] then
          begin
            NewAnnotation := 'Combined values for LGR: '
              + ADataArray.Annotation[LayerIndex, OuterGridIndex, ColIndex]
              + ', ' + ADataArray.Annotation[LayerIndex, InnerGridIndex, ColIndex];
            AnnotationPosition := AnnotationList.IndexOf(NewAnnotation);
            if AnnotationPosition >= 0 then
            begin
              NewAnnotation := AnnotationList[AnnotationPosition];
            end
            else
            begin
              AnnotationList.Add(NewAnnotation)
            end;
            case ADataArray.DataType of
              rdtDouble:
                ADataArray.RealData[LayerIndex, InnerGridIndex, ColIndex]
                  := ADataArray.RealData[LayerIndex, InnerGridIndex, ColIndex]
                  + ADataArray.RealData[LayerIndex, OuterGridIndex, ColIndex]
                  * OuterCellArea / InnerCellArea;
              rdtBoolean:
                ADataArray.BooleanData[LayerIndex, InnerGridIndex, ColIndex]
                  := ADataArray.BooleanData[LayerIndex, InnerGridIndex, ColIndex]
                  or ADataArray.BooleanData[LayerIndex, OuterGridIndex, ColIndex]
              else Assert(False);

            end;
            ADataArray.Annotation[LayerIndex, InnerGridIndex, ColIndex]
              := NewAnnotation;
          end
          else
          begin
            case ADataArray.DataType of
              rdtDouble:
                ADataArray.RealData[LayerIndex, InnerGridIndex, ColIndex]
                  := ADataArray.RealData[LayerIndex, OuterGridIndex, ColIndex]
                  * OuterCellArea / InnerCellArea;
              rdtBoolean:
                ADataArray.BooleanData[LayerIndex, InnerGridIndex, ColIndex]
                  := ADataArray.BooleanData[LayerIndex, OuterGridIndex, ColIndex]
              else Assert(False);
            end;
            ADataArray.Annotation[LayerIndex, InnerGridIndex, ColIndex] :=
              ADataArray.Annotation[LayerIndex, OuterGridIndex, ColIndex];
          end;
          case ADataArray.DataType of
            rdtDouble:
              ADataArray.RealData[LayerIndex, OuterGridIndex, ColIndex] := 0;
            rdtBoolean:
              ADataArray.BooleanData[LayerIndex, OuterGridIndex, ColIndex] := False;
            else Assert(False);
          end;
        end;
      end;


      for RowIndex := 0 to ADataArray.RowCount - 1 do
      begin
        OuterGridIndex := 0;
        InnerGridIndex := 1;
        if ADataArray.IsValue[LayerIndex, RowIndex, OuterGridIndex] then
        begin
          OuterCellArea := ModflowGrid.ColumnWidth[OuterGridIndex]
            * ModflowGrid.RowWidth[RowIndex];
          InnerCellArea := ModflowGrid.ColumnWidth[InnerGridIndex]
            * ModflowGrid.RowWidth[RowIndex];
          if ADataArray.IsValue[LayerIndex, RowIndex, InnerGridIndex] then
          begin
            NewAnnotation := 'Combined values for LGR: '
              + ADataArray.Annotation[LayerIndex, RowIndex, OuterGridIndex]
              + ', ' + ADataArray.Annotation[LayerIndex, RowIndex, InnerGridIndex];
            AnnotationPosition := AnnotationList.IndexOf(NewAnnotation);
            if AnnotationPosition >= 0 then
            begin
              NewAnnotation := AnnotationList[AnnotationPosition];
            end
            else
            begin
              AnnotationList.Add(NewAnnotation)
            end;
            case ADataArray.DataType of
              rdtDouble:
                ADataArray.RealData[LayerIndex, RowIndex, InnerGridIndex]
                  := ADataArray.RealData[LayerIndex, RowIndex, InnerGridIndex]
                  + ADataArray.RealData[LayerIndex, RowIndex, OuterGridIndex]
                  * OuterCellArea / InnerCellArea;
              rdtBoolean:
                ADataArray.BooleanData[LayerIndex, RowIndex, InnerGridIndex]
                  := ADataArray.BooleanData[LayerIndex, RowIndex, InnerGridIndex]
                  or ADataArray.BooleanData[LayerIndex, RowIndex, OuterGridIndex]
              else Assert(False);
            end;
            ADataArray.Annotation[LayerIndex, RowIndex, InnerGridIndex]
              := NewAnnotation;
          end
          else
          begin
            case ADataArray.DataType of
              rdtDouble:
                ADataArray.RealData[LayerIndex, RowIndex, InnerGridIndex]
                  := ADataArray.RealData[LayerIndex, RowIndex, OuterGridIndex]
                  * OuterCellArea / InnerCellArea;
              rdtBoolean:
                ADataArray.BooleanData[LayerIndex, RowIndex, InnerGridIndex]
                  := ADataArray.BooleanData[LayerIndex, RowIndex, OuterGridIndex]
              else Assert(False);
            end;
            ADataArray.Annotation[LayerIndex, RowIndex, InnerGridIndex] :=
              ADataArray.Annotation[LayerIndex, RowIndex, OuterGridIndex];
          end;
            case ADataArray.DataType of
              rdtDouble:
                ADataArray.RealData[LayerIndex, RowIndex, OuterGridIndex] := 0;
              rdtBoolean:
                ADataArray.BooleanData[LayerIndex, RowIndex, OuterGridIndex] := False;
              else Assert(False);
            end;
        end;

        OuterGridIndex := ADataArray.ColumnCount-1;
        InnerGridIndex := ADataArray.ColumnCount-2;
        if ADataArray.IsValue[LayerIndex, RowIndex, OuterGridIndex] then
        begin
          OuterCellArea := ModflowGrid.ColumnWidth[OuterGridIndex]
            * ModflowGrid.RowWidth[RowIndex];
          InnerCellArea := ModflowGrid.ColumnWidth[InnerGridIndex]
            * ModflowGrid.RowWidth[RowIndex];
          if ADataArray.IsValue[LayerIndex, RowIndex, InnerGridIndex] then
          begin
            NewAnnotation := 'Combined values for LGR: '
              + ADataArray.Annotation[LayerIndex, RowIndex, OuterGridIndex]
              + ', ' + ADataArray.Annotation[LayerIndex, RowIndex, InnerGridIndex];
            AnnotationPosition := AnnotationList.IndexOf(NewAnnotation);
            if AnnotationPosition >= 0 then
            begin
              NewAnnotation := AnnotationList[AnnotationPosition];
            end
            else
            begin
              AnnotationList.Add(NewAnnotation)
            end;
            case ADataArray.DataType of
              rdtDouble:
                ADataArray.RealData[LayerIndex, RowIndex, InnerGridIndex]
                  := ADataArray.RealData[LayerIndex, RowIndex, InnerGridIndex]
                  + ADataArray.RealData[LayerIndex, RowIndex, OuterGridIndex]
                  * OuterCellArea / InnerCellArea;
              rdtBoolean:
                ADataArray.BooleanData[LayerIndex, RowIndex, InnerGridIndex]
                  := ADataArray.BooleanData[LayerIndex, RowIndex, InnerGridIndex]
                  or ADataArray.BooleanData[LayerIndex, RowIndex, OuterGridIndex];
              else Assert(False);
            end;
            ADataArray.Annotation[LayerIndex, RowIndex, InnerGridIndex]
              := NewAnnotation;
          end
          else
          begin
            case ADataArray.DataType of
              rdtDouble:
                ADataArray.RealData[LayerIndex, RowIndex, InnerGridIndex]
                  := ADataArray.RealData[LayerIndex, RowIndex, OuterGridIndex]
                  * OuterCellArea / InnerCellArea;
              rdtBoolean:
                ADataArray.BooleanData[LayerIndex, RowIndex, InnerGridIndex]
                  := ADataArray.BooleanData[LayerIndex, RowIndex, OuterGridIndex]
              else Assert(False);
            end;
            ADataArray.Annotation[LayerIndex, RowIndex, InnerGridIndex] :=
              ADataArray.Annotation[LayerIndex, RowIndex, OuterGridIndex];
          end;
          case ADataArray.DataType of
            rdtDouble:
              ADataArray.RealData[LayerIndex, RowIndex, OuterGridIndex] := 0;
            rdtBoolean:
              ADataArray.BooleanData[LayerIndex, RowIndex, OuterGridIndex] := False;
            else Assert(False);
          end;
        end;
      end
    end;
  finally
    AnnotationList.Free;
  end;
end;

procedure TChildModel.Assign(Source: TPersistent);
var
  SourceModel: TChildModel;
begin
  if Source is TChildModel then
  begin
    // When editing this section, be sure to edit TChildModelEdit
    // and TChildModelItem too
    SourceModel := TChildModel(Source);
    ModelName := SourceModel.ModelName;
    ChildCellsPerParentCell := SourceModel.ChildCellsPerParentCell;
    Discretization := SourceModel.Discretization;
    UpdateLayerCount;
    StartingHeadSource := SourceModel.StartingHeadSource;
//    OneWayCoupling := SourceModel.OneWayCoupling;
    CouplingMethod := SourceModel.CouplingMethod;
    MaxIterations := SourceModel.MaxIterations;
    LgrPrintChoice := SourceModel.LgrPrintChoice;
    HeadRelaxationFactor := SourceModel.HeadRelaxationFactor;
    FluxRelaxationFactor := SourceModel.FluxRelaxationFactor;
    HeadClosureCriterion := SourceModel.HeadClosureCriterion;
    FluxClosureCriterion := SourceModel.FluxClosureCriterion;
  end;
  inherited;
end;

function TChildDiscretizationCollection.GetAnItemByGroupAndLayer(
  LayerGroup: TLayerGroup; SubLayer: integer): TChildDiscretization;
var
  Index: Integer;
  Item: TChildDiscretization;
  PriorItem: TChildDiscretization;
begin
  Assert(LayerGroup.Index <> 0);
  result := nil;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TChildDiscretization;
    if (Item.LayerGroup = LayerGroup)
      and (Item.ParentLayerNumber = SubLayer) then
    begin
      result := Item;
      Exit;
    end;
  end;
  if result = nil then
  begin
    result := Add as TChildDiscretization;
    result.LayerGroup := LayerGroup;
    result.ParentLayerNumber := SubLayer;
    if SubLayer > 0 then
    begin
      PriorItem := GetAnItemByGroupAndLayer(LayerGroup, SubLayer-1);
      result.Discretization := PriorItem.Discretization;
    end;
  end;
end;

procedure TChildDiscretizationCollection.SortAndDeleteExtraItems;
var
  Index: Integer;
  Item: TChildDiscretization;
  InnerIndex: Integer;
begin
  Sort;
  for Index := Count - 1 downto 0 do
  begin
    Item := Items[Index];
    if Item.LayerGroup = BottomLayerGroup then
    begin
      for InnerIndex := Index downto 0 do
      begin
        Item := Items[InnerIndex];
        if Item.ParentLayerNumber > BottomLayerInUnit then
        begin
          Delete(InnerIndex);
        end
        else
        begin
          break;
        end;
      end;
      break;
    end
    else
    begin
      Delete(Index);
    end;
  end;
end;

procedure TChildDiscretizationCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Item <> nil then
  begin
    FChanged := True;
  end;
  if (UpdateCount = 0) and FChanged then
  begin
    if Model <> nil then
    begin
      (Model as TChildModel).ModflowGrid.NotifyGridChanged(self);
    end;
  end;
end;

procedure TChildDiscretizationCollection.WriteLAYCB(
  const DiscretizationWriter: TObject);
var
  DisWriter: TModflowDiscretizationWriter;
  LocalModel: TChildModel;
  GroupIndex: Integer;
  LAYCB: array of integer;
  AGroup: TLayerGroup;
  DisIndex: Integer;
  LayerIndex: integer;
  DisItem: TChildDiscretization;
  SubIndex: Integer;
  Index: Integer;
begin
  LocalModel := Model as TChildModel;
  SetLength(LAYCB, ModflowLayerCount);
  LayerIndex := 0;
  for GroupIndex := 1 to LocalModel.LayerStructure.Count - 1 do
  begin
    AGroup := LocalModel.LayerStructure[GroupIndex];
    if AGroup.Simulated then
    begin
      for DisIndex := 0 to AGroup.LayerCount - 1 do
      begin
        DisItem := GetAnItemByGroupAndLayer(AGroup,DisIndex);
        for SubIndex := 0 to DisItem.Discretization - 1 do
        begin
          if LayerIndex < Length(LAYCB) then
          begin
            LAYCB[LayerIndex] := 0;
          end;
          Inc(LayerIndex);
        end;
        if (AGroup = BottomLayerGroup)
          and (DisIndex = BottomLayerInUnit) then
        begin
          break;
        end;
      end;
    end
    else
    begin
      if LayerIndex-1 < Length(LAYCB) then
      begin
        LAYCB[LayerIndex-1] := 1;
      end;
    end;
    if (AGroup = BottomLayerGroup) then
    begin
      break;
    end;
  end;
  DisWriter := DiscretizationWriter as TModflowDiscretizationWriter;
  for Index := 0 to Length(LAYCB) - 1 do
  begin
    DisWriter.WriteInteger(LAYCB[Index]);
  end;
  DisWriter.WriteString(' # LAYCB');
  DisWriter.NewLine;
end;

function CompareChildDisItems(Item1, Item2: Pointer): Integer;
var
  Child1: TChildDiscretization;
  Child2: TChildDiscretization;
begin
  Child1 := Item1;
  Child2 := Item2;
  Assert(Child1.LayerGroup <> nil);
  Assert(Child2.LayerGroup <> nil);
  result := Child1.LayerGroup.Index - Child2.LayerGroup.Index;
  if result = 0 then
  begin
    result := Child1.ParentLayerNumber - Child2.ParentLayerNumber;
  end;
end;

procedure TChildDiscretizationCollection.Sort;
var
  ItemList: TList;
  Item: TChildDiscretization;
  Index: Integer;
begin
  ItemList := TList.Create;
  try
    ItemList.Capacity := Count;
    for Index := 0 to Count - 1 do
    begin
      ItemList.Add(Items[Index]);
    end;
    ItemList.Sort(CompareChildDisItems);
    for Index := 0 to ItemList.Count - 1 do
    begin
      Item := ItemList[Index];
      Item.Index := Index;
    end;
  finally
    ItemList.Free;
  end;
end;

function TChildDiscretizationCollection.GetBottomLayerGroup: TLayerGroup;
var
  Index: Integer;
  Group: TLayerGroup;
begin
  if (FBottomLayerGroup = nil) and (FBottomUnitName <> '') then
  begin
    for Index := 1 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
    begin
      Group := frmGoPhast.PhastModel.LayerStructure[Index];
      if Group.AquiferName = FBottomUnitName then
      begin
        FBottomLayerGroup := Group;
        break;
      end;
    end;
  end;
  result := FBottomLayerGroup;
end;

function TChildDiscretizationCollection.GetBottomUnitName: string;
begin
  if BottomLayerGroup <> nil then
  begin
    result := BottomLayerGroup.AquiferName;
  end
  else
  begin
    result := FBottomUnitName
  end;
end;

function TChildDiscretizationCollection.GetItem(
  Index: integer): TChildDiscretization;
begin
  result := inherited Items[Index] as TChildDiscretization;
end;

function TChildDiscretizationCollection.IsSame(
  AnOrderedCollection: TOrderedCollection): boolean;
var
  AnotherDis: TChildDiscretizationCollection;
begin
  result := AnOrderedCollection is TChildDiscretizationCollection;
  if result then
  begin
    AnotherDis := TChildDiscretizationCollection(AnOrderedCollection);
    result := inherited IsSame(AnOrderedCollection)
      and (BottomUnitName = AnotherDis.BottomUnitName)
      and (BottomLayerInUnit = AnotherDis.BottomLayerInUnit)
  end;
end;

procedure TChildDiscretizationCollection.Loaded;
var
  ItemIndex: Integer;
begin
  for ItemIndex := 0 to Count - 1 do
  begin
    Items[ItemIndex].Loaded;
  end;
  GetBottomLayerGroup;
end;

function TChildDiscretizationCollection.ModflowConfiningBedCount: integer;
var
  GroupIndex: Integer;
  LocalModel: TCustomModel;
  AGroup: TLayerGroup;
begin
  result := 0;
  LocalModel := Model as TCustomModel;
  for GroupIndex := 1 to LocalModel.LayerStructure.Count - 1 do
  begin
    AGroup := LocalModel.LayerStructure[GroupIndex];
    if AGroup = BottomLayerGroup then
    begin
      Exit;
    end;
    if not AGroup.Simulated then
    begin
      Inc(result);
    end;
  end;
end;

function TChildDiscretizationCollection.ModflowLayerCount: integer;
var
  GroupIndex: Integer;
  LocalModel: TCustomModel;
  AGroup: TLayerGroup;
  DisItem: TChildDiscretization;
  DisIndex: Integer;
begin
  result := 0;
  LocalModel := Model as TCustomModel;
  for GroupIndex := 1 to LocalModel.LayerStructure.Count - 1 do
  begin
    AGroup := LocalModel.LayerStructure[GroupIndex];
    if AGroup.Simulated then
    begin
      for DisIndex := 0 to AGroup.LayerCount - 1 do
      begin
        DisItem := GetAnItemByGroupAndLayer(AGroup, DisIndex);
        result := result + DisItem.Discretization;
        if (AGroup = BottomLayerGroup) and (DisIndex = BottomLayerInUnit) then
        begin
          if (GroupIndex <> LocalModel.LayerStructure.Count - 1)
            or (DisIndex <> AGroup.LayerCount - 1) then
          begin
            result := result - DisItem.Discretization div 2;
          end;
          Exit;
        end;
      end;
    end;
  end;
end;

function TChildModel.Chani: TOneDIntegerArray;
begin
  Result := ConvertIntegerParentArray(inherited Chani);
end;

function TChildModel.ChildColToParentCol(ACol: integer): integer;
begin
  if (ACol < 0) or (ACol >= Grid.ColumnCount) then
  begin
    result := -1;
  end
  else
  begin
    result := ((ACol + (ChildCellsPerParentCell div 2))
       div ChildCellsPerParentCell) + FFirstCol
  end;
end;

function TChildModel.ChildLayerToParentLayer(ALayer: integer): integer;
var
  CumulativeLayers: Integer;
  LayerGroupIndex: Integer;
  LayerGroup: TLayerGroup;
  DisIndex: Integer;
  DisItem: TChildDiscretization;
begin
  if (ALayer < 0) or (ALayer >= Grid.LayerCount) then
  begin
    result := -1;
  end
  else
  begin
    result := -1;
    CumulativeLayers := -1;

    for LayerGroupIndex := 1 to LayerStructure.Count - 1 do
    begin
      LayerGroup := LayerStructure[LayerGroupIndex];
      for DisIndex := 0 to LayerGroup.LayerCount - 1 do
      begin
        DisItem := Discretization.GetAnItemByGroupAndLayer(LayerGroup,DisIndex);
        CumulativeLayers := CumulativeLayers + DisItem.Discretization;
        Inc(result);
        if CumulativeLayers >= ALayer then
        begin
          Exit;
        end;
        if (Discretization.BottomLayerGroup = LayerGroup)
          and (Discretization.BottomLayerInUnit = DisIndex) then
        begin
          break;
        end;
      end;
      if (Discretization.BottomLayerGroup = LayerGroup) then
      begin
        Break;
      end;
    end;
    Assert(False);

    // The bottom bottom parent layer may have fewer layers
    // than indicated by Discretization[LayerIndex].Discretization.
    // That doesn't matter because cases where it might matter
    // are caught by ALayer >= Grid.LayerCount.
//    for LayerIndex := 0 to Discretization.Count - 1 do
//    begin
//      CumulativeLayers := CumulativeLayers
//        + Discretization[LayerIndex].Discretization;
//      if ALayer < CumulativeLayers  then
//      begin
//        result := LayerIndex;
//        Exit;
//      end;
//    end;
//    Assert(False);
  end;
end;

function TChildModel.ChildRowToParentRow(ARow: integer): integer;
begin
  if (ARow < 0) or (ARow >= Grid.RowCount) then
  begin
    result := -1;
  end
  else
  begin
    result := ((ARow + (ChildCellsPerParentCell div 2))
       div ChildCellsPerParentCell) + FFirstRow
  end;
end;

function TChildModel.Child_NameFile_Name(const Parent_NameFile_Name: string): string;
var
  Directory: string;
begin
  Directory := ExtractFileDir(Parent_NameFile_Name);
  result := ExtractFileName(Parent_NameFile_Name);
  result := ChangeFileExt(result, '');
  result := result + '_' + ModelName;
  result := FixFileName(result);
  result := ChangeFileExt(result, '.nam');
  result := IncludeTrailingPathDelimiter(Directory) + result;
end;

constructor TChildModel.Create(AnOwner: TComponent);
var
  DataArrayIndex: Integer;
  ParentDataArray: TDataArray;
  ChildDataArray: TDataArray;
  ChildArrays: TList;
  ParentArrays: TList;
  ParamIndex: Integer;
  AParam: TModflowTransientListParameter;
begin
  FCreating := True;
  inherited Create(nil);
  FStartingHeadSource := shsSelf;
  FMaxIterations := 20;
  FLgrPrintChoice := lpcListing;
  FFluxRelaxationFactor := 0.5;
  FHeadRelaxationFactor := 0.5;
  FHeadClosureCriterion := 5E-3;
  FFluxClosureCriterion := 5E-2;
  FFirstCol := -1;
  FLastCol := -1;
  FFirstRow := -1;
  FLastRow := -1;
  FParentModel := AnOwner as TCustomModel;
  FChildCellsPerParentCell := 3;
  FDiscretization := TChildDiscretizationCollection.Create(self);

  if (ParentModel  <> nil) then
  begin
    ChildArrays := TList.Create;
    ParentArrays := TList.Create;
    try
      for DataArrayIndex := 0 to ParentModel.DataArrayManager.DataSetCount - 1 do
      begin
        ParentDataArray := ParentModel.DataArrayManager.DataSets[DataArrayIndex];
        ChildDataArray := TDataArrayType(ParentDataArray.ClassType).Create(self);
        ChildDataArray.AssignProperties(ParentDataArray);
        DataArrayManager.AddDataSet(ChildDataArray);
        CreateVariables(ChildDataArray);
        ParentArrays.Add(ParentDataArray);
        ChildArrays.Add(ChildDataArray);
        ParentDataArray.TalksTo(ChildDataArray);
      end;
      ModelSelection := ParentModel.ModelSelection;
      for DataArrayIndex := 0 to ChildArrays.Count - 1 do
      begin
        ChildDataArray := ChildArrays[DataArrayIndex];
        ParentDataArray := ParentArrays[DataArrayIndex];
        ChildDataArray.Formula := ParentDataArray.Formula;
      end;

      for ParamIndex := 0 to ParentModel.ModflowTransientParameters.Count - 1 do
      begin
        AParam := ParentModel.ModflowTransientParameters[ParamIndex];
        AParam.NewChildModelCreated(self);
      end;

      ModflowGrid.OnSelectedColumnChange :=
        ParentModel.ModflowGrid.OnSelectedColumnChange;
      ModflowGrid.OnSelectedRowChange :=
        ParentModel.ModflowGrid.OnSelectedRowChange;
      ModflowGrid.OnSelectedLayerChange :=
        ParentModel.ModflowGrid.OnSelectedLayerChange;
    finally
      ParentArrays.Free;
      ChildArrays.Free;
    end;
  end;
  FCanUpdateGrid := True;
  FCreating := False;
end;

function TChildModel.DataSetLayerToModflowLayer(DataSetLayer: integer): integer;
var
  Index: Integer;
begin
  result := 0;
  for Index := 0 to DataSetLayer do
  begin
    if IsLayerSimulated(Index) then
    begin
      Inc(result);
    end;
  end;
end;

function TChildModel.DefaultModflowOutputFileName: string;
var
  Extension: string;
begin
  GetDefaultOutputFileExtension(Extension);
  if Extension = '' then
  begin
    result := '';
    Exit;
  end;
  result := ChangeFileExt(Child_NameFile_Name(ModelFileName), Extension);
  result := FixFileName(result);
end;

procedure TChildModel.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('HeadRelaxationFactor', ReadHeadRelaxationFactor, WriteHeadRelaxationFactor, FHeadRelaxationFactor = 0);
  Filer.DefineProperty('FluxRelaxationFactor', ReadFluxRelaxationFactor, WriteFluxRelaxationFactor, FFluxRelaxationFactor = 0);
  Filer.DefineProperty('HeadClosureCriterion', ReadHeadClosureCriterion, WriteHeadClosureCriterion, FHeadClosureCriterion = 0);
  Filer.DefineProperty('FluxClosureCriterion', ReadFluxClosureCriterion, WriteFluxClosureCriterion, FFluxClosureCriterion = 0);
end;

destructor TChildModel.Destroy;
var
  ParamIndex: Integer;
  AParam: TModflowTransientListParameter;
  ScreenObjectIndex: integer;
begin

  AllObserversStopTalking;
  DataArrayManager.UnlinkDeletedDataSets;
  if FParentModel <> nil then
  begin
    for ParamIndex := 0 to ParentModel.ModflowTransientParameters.Count - 1 do
    begin
      AParam := ParentModel.ModflowTransientParameters[ParamIndex];
      AParam.ChildModelBeingDestroyed(self);
    end;
    FParentModel.TopGridObserver.StopsTalkingTo(TopGridObserver);
    FParentModel.ThreeDGridObserver.StopsTalkingTo(ThreeDGridObserver);
    FParentModel.HufKxNotifier.StopsTalkingTo(HufKxNotifier);
    FParentModel.HufKyNotifier.StopsTalkingTo(HufKyNotifier);
    FParentModel.HufKzNotifier.StopsTalkingTo(HufKzNotifier);
    FParentModel.HufSsNotifier.StopsTalkingTo(HufSsNotifier);
    FParentModel.HufSyNotifier.StopsTalkingTo(HufSyNotifier);
    for ScreenObjectIndex := 0 to ScreenObjectCount - 1 do
    begin
      ScreenObjects[ScreenObjectIndex].RemoveModelLink(self);
    end;
  end;
  FClearing := True;
  try
    InternalClear;
  finally
    FClearing := False;
  end;
  HorizontalPositionScreenObject := nil;
  FreeHufNotifiers;
  FreeGridNotifiers;
  FModelName := '';
  FDiscretization.Free;

  inherited;
end;

function TChildModel.DMCOEF: TOneDRealArray;
begin
  Result := ConvertRealParentArray(inherited DMCOEF);
end;

function TChildModel.EdgeIndex: integer;
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
begin
  PhastModel := ParentModel as TPhastModel;
  result := -1;
  for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
  begin
    if PhastModel.ChildModels[ChildIndex].ChildModel = self then
    begin
      result := -ChildIndex -2;
      Exit;
    end;
  end;
  Assert(False);
end;

function TChildModel.FirstOverlappedLayer: integer;
var
  DisIndex: Integer;
  EndLayer: Integer;
  Group: TLayerGroup;
  GroupIndex: Integer;
  PriorItem: TChildDiscretization;
  Item: TChildDiscretization;
  NewItems: boolean;
begin
  result := 0;
  NewItems := False;
  for GroupIndex := 1 to LayerStructure.Count - 1 do
  begin
    Group := LayerStructure[GroupIndex];
    if Group.Simulated then
    begin
      if Group = Discretization.BottomLayerGroup then
      begin
        EndLayer := Discretization.BottomLayerInUnit;
      end
      else
      begin
        EndLayer := Group.LayerCount - 1;
      end;
      for DisIndex := 0 to EndLayer do
      begin
        Item := Discretization.GetAnItemByGroupAndLayer(Group, DisIndex);
        if Item = nil then
        begin
          Item := Discretization.Add as TChildDiscretization;
          Item.LayerGroup := Group;
          Item.ParentLayerNumber := DisIndex;
          if DisIndex > 0 then
          begin
            PriorItem := Discretization.GetAnItemByGroupAndLayer(Group, DisIndex - 1);
            Item.Discretization := PriorItem.Discretization;
          end;
          NewItems := True;
        end;
        if (Group = Discretization.BottomLayerGroup)
          and (DisIndex = EndLayer)
          and ((GroupIndex <> LayerStructure.Count - 1)
          or (EndLayer <> Group.LayerCount - 1)) then
        begin
//          Inc(result, (Item.Discretization div 2)+1);
        end
        else
        begin
          Inc(result, Item.Discretization);
        end;
      end;
      if Group = Discretization.BottomLayerGroup then
      begin
        break;
      end
    end
    else
    begin
      Inc(result);
    end;
  end;
  if NewItems then
  begin
    Discretization.SortAndDeleteExtraItems;
  end;
end;

function TChildModel.GetChemistryOptions: TChemistryOptions;
begin
  result := ParentModel.GetChemistryOptions;
end;

function TChildModel.GetContourFont: TFont;
begin
  result := ParentModel.GetContourFont;
end;

function TChildModel.GetDisplayName: string;
begin
  result := ModelName;
end;

function TChildModel.GetFormulaManager: TFormulaManager;
begin
  result := ParentModel.GetFormulaManager;
end;

function TChildModel.GetFreeSurface: boolean;
begin
  result := ParentModel.GetFreeSurface;
end;

function TChildModel.GetHufParameters: THufModflowParameters;
begin
  result := ParentModel.GetHufParameters;
end;

function TChildModel.GetImmobileComponents: TChemSpeciesCollection;
begin
  if ParentModel = nil then
  begin
    result := nil;
  end
  else
  begin
    result := ParentModel.GetImmobileComponents;
  end;
end;

function TChildModel.GetLayerGroupByLayer(const Layer: integer): TLayerGroup;
begin
  result := inherited GetLayerGroupByLayer(ChildLayerToParentLayer(Layer));
end;

function TChildModel.GetLayerStructure: TLayerStructure;
begin
  if ParentModel = nil then
  begin
    result := nil;
  end
  else
  begin
    result := ParentModel.GetLayerStructure;
  end;
end;

function TChildModel.GetMobileComponents: TMobileChemSpeciesCollection;
begin
  if ParentModel = nil then
  begin
    result := nil;
  end
  else
  begin
    result := ParentModel.GetMobileComponents;
  end;
end;

function TChildModel.GetModelSelection: TModelSelection;
begin
  if ParentModel = nil then
  begin
    result := msUndefined;
  end
  else
  begin
    result := ParentModel.GetModelSelection;
  end;
end;

function TChildModel.GetModflowFullStressPeriods: TModflowStressPeriods;
begin
  result := ParentModel.GetModflowFullStressPeriods;
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

function TChildModel.GetMt3dmsOutputControl: TMt3dmsOutputControl;
begin
  result := ParentModel.GetMt3dmsOutputControl;
end;

function TChildModel.GetMt3dmsTimes: TMt3dmsTimeCollection;
begin
  result := ParentModel.GetMt3dmsTimes
end;

function TChildModel.GetObservationPurpose: TObservationPurpose;
begin
  result := ParentModel.GetObservationPurpose;
end;

function TChildModel.GetProgramLocations: TProgramLocations;
begin
  result := ParentModel.GetProgramLocations;
end;

procedure TChildModel.GetRowColPositions(const StartPosition,
  EndPosition: integer; const ParentPositions: TOneDRealArray;
  out ChildPostions: TOneDRealArray);
var
  NewLength: Integer;
  ParentIndex: Integer;
  PositionIndex: Integer;
  ChildIndex: Integer;
  Start: Double;
  Delta: Double;
begin
  if (Length(ParentPositions) = 0) then
  begin
    SetLength(ChildPostions, 0);
    Exit;
  end;
  Assert(0 <= StartPosition);
  Assert(StartPosition < EndPosition);
  Assert(EndPosition+1 < Length(ParentPositions));
  NewLength := (EndPosition - StartPosition)*ChildCellsPerParentCell + 2;
  SetLength(ChildPostions, NewLength);
  PositionIndex := 0;

  Start := ParentPositions[StartPosition];
  Delta := ParentPositions[StartPosition+1] - Start;

  ChildPostions[PositionIndex] := (Start + ParentPositions[StartPosition+1])/2;
  Inc(PositionIndex);

  for ChildIndex := (ChildCellsPerParentCell div 2) + 1 to ChildCellsPerParentCell - 1 do
  begin
    ChildPostions[PositionIndex] := Start + ChildIndex*Delta/ChildCellsPerParentCell;
    Inc(PositionIndex);
  end;

  for ParentIndex := StartPosition+1 to EndPosition-1 do
  begin
    Start := ParentPositions[ParentIndex];
    Delta := ParentPositions[ParentIndex+1] - Start;
    for ChildIndex := 0 to ChildCellsPerParentCell - 1 do
    begin
      ChildPostions[PositionIndex] := Start + ChildIndex*Delta/ChildCellsPerParentCell;
      Inc(PositionIndex);
    end;
  end;

  Start := ParentPositions[EndPosition];
  Delta := ParentPositions[EndPosition+1] - Start;

  for ChildIndex := 0 to (ChildCellsPerParentCell div 2) do
  begin
    ChildPostions[PositionIndex] := Start + ChildIndex*Delta/ChildCellsPerParentCell;
    Inc(PositionIndex);
  end;

  ChildPostions[PositionIndex] := (Start + ParentPositions[EndPosition+1])/2;
  Inc(PositionIndex);

  Assert(PositionIndex = NewLength);
end;

function TChildModel.GetSaveBfhBoundaryConditions: boolean;
begin
  result := ParentModel.SaveBfhBoundaryConditions;
end;

function TChildModel.GetScreenObjectByName(AName: string): TScreenObject;
begin
  result := ParentModel.GetScreenObjectByName(AName);
end;

function TChildModel.GetScreenObjectCount: integer;
begin
  if ParentModel = nil then
  begin
    result := 0;
  end
  else
  begin
    result := ParentModel.GetScreenObjectCount;
  end;
end;

function TChildModel.GetScreenObjects(const Index: integer): TScreenObject;
begin
  result := ParentModel.GetScreenObjects(Index);
end;

function TChildModel.GetSelectedModel: TCustomModel;
begin
  result := ParentModel.SelectedModel;
end;

function TChildModel.GetShowContourLabels: boolean;
begin
  result := ParentModel.GetShowContourLabels;
end;

function TChildModel.GetSoluteTransport: boolean;
begin
  result := ParentModel.GetSoluteTransport
end;

function TChildModel.GetSomeSegmentsUpToDate: boolean;
begin
  result := FParentModel.SomeSegmentsUpToDate;
end;

function TChildModel.GetUseWaterTable: boolean;
begin
  result := ParentModel.GetUseWaterTable
end;

procedure TChildModel.Invalidate;
begin
  inherited;
  if ParentModel <> nil then
  begin
    ParentModel.Invalidate;
  end;
end;

function TChildModel.IsLayerSimulated(const LayerID: integer): boolean;
begin
  result := inherited IsLayerSimulated(ChildLayerToParentLayer(LayerID));
end;

procedure TChildDiscretizationCollection.SetBottomLayerGroup(const Value: TLayerGroup);
begin
  BeginUpdate;
  try
    if FBottomLayerGroup <> Value then
    begin
      FBottomLayerGroup := Value;
      FChanged := True;
      InvalidateModel;
    end;
    if BottomLayerGroup <> nil then
    begin
      FBottomUnitName := BottomLayerGroup.AquiferName;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TChildDiscretizationCollection.SetBottomLayerInUnit(const Value: integer);
begin
  if FBottomLayerInUnit <> Value then
  begin
    BeginUpdate;
    try
      FBottomLayerInUnit := Value;
      FChanged := True;
      InvalidateModel;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TChildDiscretizationCollection.SetBottomUnitName(const Value: string);
begin
  if FBottomUnitName <> Value then
  begin
    BeginUpdate;
    try
      FBottomUnitName := Value;
      FChanged := True;
      InvalidateModel;
      // upate FBottomLayerGroup;
      GetBottomLayerGroup;
    finally
      EndUpdate
    end;
  end;
end;

procedure TChildDiscretizationCollection.SetItem(Index: integer;
  const Value: TChildDiscretization);
begin
  inherited Items[Index] := Value;
end;

procedure TChildModel.SetCanUpdateGrid(const Value: Boolean);
begin
  FCanUpdateGrid := Value;
  if FCanUpdateGrid and FShouldUpdateGrid then
  begin
    UpdateGrid;
  end;
end;

procedure TChildModel.SetChemistryOptions(const Value: TChemistryOptions);
begin
  ParentModel.SetChemistryOptions(Value);
end;

procedure TChildModel.SetChildCellsPerParentCell(const Value: integer);
begin
  Assert(Odd(Value));
  if FChildCellsPerParentCell <> Value then
  begin
    FChildCellsPerParentCell := Value;
    UpdateGrid;
    Invalidate;
  end;
end;

procedure TChildModel.SetContourFont(const Value: TFont);
begin
  ParentModel.ContourFont := Value;
end;

procedure TChildModel.SetCouplingMethod(const Value: TCouplingMethod);
begin
  if FCouplingMethod <> Value then
  begin
    FCouplingMethod := Value;
    Invalidate;
  end;
end;

procedure TChildModel.SetDiscretization(
  const Value: TChildDiscretizationCollection);
begin
  FDiscretization.Assign(Value);
end;

procedure TChildModel.SetFluxClosureCriterion(const Value: double);
begin
  if FFluxClosureCriterion <> Value then
  begin
    FFluxClosureCriterion := Value;
    Invalidate;
  end;
end;

procedure TChildModel.SetFluxRelaxationFactor(const Value: double);
begin
  if FFluxRelaxationFactor <> Value then
  begin
    FFluxRelaxationFactor := Value;
    Invalidate;
  end;
end;

procedure TChildModel.SetFreeSurface(const Value: boolean);
begin
  ParentModel.SetFreeSurface(Value);
end;

procedure TChildModel.SetHeadClosureCriterion(const Value: double);
begin
  if FHeadClosureCriterion <> Value then
  begin
    FHeadClosureCriterion := Value;
    Invalidate;
  end;
end;

procedure TChildModel.SetHeadRelaxationFactor(const Value: double);
begin
  if FHeadRelaxationFactor <> Value then
  begin
    FHeadRelaxationFactor := Value;
    Invalidate;
  end;
end;

procedure TChildModel.SetHorizontalPositionScreenObject(
  const Value: TScreenObject);
begin
  if FHorizontalPositionScreenObject <> Value then
  begin
    if FHorizontalPositionScreenObject <> nil then
    begin
      FHorizontalPositionScreenObject.ChildModel := nil;
    end;
    FHorizontalPositionScreenObject := Value;
    if (FHorizontalPositionScreenObject <> nil)
      and (FHorizontalPositionScreenObject.ChildModel <> self) then
    begin
      FHorizontalPositionScreenObject.ChildModel := self;
    end;
    UpdateGrid;
    Invalidate;
  end;
end;

procedure TChildModel.SetHufParameters(const Value: THufModflowParameters);
begin
  ParentModel.SetHufParameters(Value);
end;

procedure TChildModel.SetImmobileComponents(
  const Value: TChemSpeciesCollection);
begin
  ParentModel.SetImmobileComponents(Value);
end;

procedure TChildModel.SetLayerStructure(const Value: TLayerStructure);
begin
  ParentModel.SetLayerStructure(Value);
end;

procedure TChildModel.SetLgrPrintChoice(const Value: TLgrPrintChoice);
begin
  if FLgrPrintChoice <> Value then
  begin
    FLgrPrintChoice := Value;
    Invalidate;
  end;
end;

procedure TChildModel.SetMaxIterations(const Value: integer);
begin
  if FMaxIterations <> Value then
  begin
    FMaxIterations := Value;
    Invalidate;
  end;
end;

procedure TChildModel.SetMobileComponents(
  const Value: TMobileChemSpeciesCollection);
begin
  ParentModel.SetMobileComponents(Value);
end;

procedure TChildModel.SetModelName(const Value: string);
begin
  if FModelName <> Value then
  begin
    FModelName := Value;
    Invalidate;
  end;
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

procedure TChildModel.SetMt3dmsOutputControl(const Value: TMt3dmsOutputControl);
begin
  ParentModel.SetMt3dmsOutputControl(Value);
end;

procedure TChildModel.SetMt3dmsTimes(const Value: TMt3dmsTimeCollection);
begin
  ParentModel.SetMt3dmsTimes(Value);
end;

procedure TChildModel.SetObservationPurpose(const Value: TObservationPurpose);
begin
  ParentModel.SetObservationPurpose(Value);
end;

procedure TChildModel.SetProgramLocations(const Value: TProgramLocations);
begin
  ParentModel.SetProgramLocations(Value);
end;

procedure TChildModel.SetSaveBfhBoundaryConditions(const Value: boolean);
begin
  ParentModel.SaveBfhBoundaryConditions := Value;
end;

procedure TChildModel.SetSelectedModel(const Value: TCustomModel);
begin
  ParentModel.SelectedModel := Value;
end;

procedure TChildModel.SetShowContourLabels(const Value: boolean);
begin
  ParentModel.ShowContourLabels := Value;
end;

procedure TChildModel.SetSoluteTransport(const Value: boolean);
begin
  ParentModel.SetSoluteTransport(Value);
end;

procedure TChildModel.SetSomeSegmentsUpToDate(const Value: boolean);
begin
  FParentModel.SomeSegmentsUpToDate := Value;
end;

procedure TChildModel.SetStartingHeadSource(const Value: TStartingHeadSource);
begin
  if FStartingHeadSource <> Value then
  begin
    FStartingHeadSource := Value;
    Invalidate;
  end;
end;

procedure TChildModel.SetUseWaterTable(const Value: boolean);
begin
  ParentModel.SetUseWaterTable(Value);
end;

function TChildModel.StoreHydrogeologicUnits: Boolean;
begin
  result := False;
end;

function TChildModel.TRPT: TOneDRealArray;
begin
  Result := ConvertRealParentArray(inherited TRPT);
end;

function TChildModel.TRPV: TOneDRealArray;
begin
  Result := ConvertRealParentArray(inherited TRPV);
end;

function TChildModel.Trpy: TOneDRealArray;
begin
  Result := ConvertRealParentArray(inherited Trpy);
end;

function TChildModel.ConvertRealParentArray(ParentArray: TOneDRealArray): TOneDRealArray;
var
  GroupIndex: Integer;
  ChildIndex: Integer;
  ParentIndex: Integer;
  ChildDisIndex: Integer;
  DisItem: TChildDiscretization;
  DiscIndex: Integer;
  AGroup: TLayerGroup;
begin
  SetLength(result, ModflowLayerCount);
  ParentIndex := 0;
  ChildIndex := 0;
  for GroupIndex := 1 to LayerStructure.Count - 1 do
  begin
    AGroup := LayerStructure[GroupIndex];
    if AGroup.Simulated then
    begin
      for DiscIndex := 0 to AGroup.LayerCount - 1 do
      begin
        DisItem := Discretization.GetAnItemByGroupAndLayer(AGroup, DiscIndex);
        for ChildDisIndex := 0 to DisItem.Discretization - 1 do
        begin
          if ChildIndex < length(result) then
          begin
            result[ChildIndex] := ParentArray[ParentIndex];
            Inc(ChildIndex);
          end;
        end;
        Inc(ParentIndex);
        if (AGroup = Discretization.BottomLayerGroup) and (DiscIndex = Discretization.BottomLayerInUnit) then
        begin
          break;
        end;
      end;
    end;
    if (AGroup = Discretization.BottomLayerGroup) then
    begin
      break;
    end;
  end;
end;

function TChildModel.ConvertIntegerParentArray( ParentArray: TOneDIntegerArray): TOneDIntegerArray;
var
  ChildDisIndex: Integer;
  DisItem: TChildDiscretization;
  DiscIndex: Integer;
  AGroup: TLayerGroup;
  GroupIndex: Integer;
  ChildIndex: Integer;
  ParentIndex: Integer;
begin
  SetLength(result, ModflowLayerCount);
  ParentIndex := 0;
  ChildIndex := 0;
  for GroupIndex := 1 to LayerStructure.Count - 1 do
  begin
    AGroup := LayerStructure[GroupIndex];
    if AGroup.Simulated then
    begin
      for DiscIndex := 0 to AGroup.LayerCount - 1 do
      begin
        DisItem := Discretization.GetAnItemByGroupAndLayer(AGroup, DiscIndex);
        for ChildDisIndex := 0 to DisItem.Discretization - 1 do
        begin
          if ChildIndex < length(result) then
          begin
            result[ChildIndex] := ParentArray[ParentIndex];
            Inc(ChildIndex);
          end;
        end;
        Inc(ParentIndex);
        if (AGroup = Discretization.BottomLayerGroup)
          and (DiscIndex = Discretization.BottomLayerInUnit) then
        begin
          break;
        end;
      end;
    end;
    if (AGroup = Discretization.BottomLayerGroup) then
    begin
      break;
    end;
  end;
end;

procedure TChildModel.UpdateDataSetConnections;
var
  Index: Integer;
  ParentDataArray: TDataArray;
  ChildDataArray: TDataArray;
begin
  for Index := 0 to ParentModel.DataArrayManager.DataSetCount - 1 do
  begin
    ParentDataArray := ParentModel.DataArrayManager[Index];
    ChildDataArray := DataArrayManager.GetDataSetByName(ParentDataArray.Name);
    ParentDataArray.TalksTo(ChildDataArray);
  end;
end;

procedure TChildModel.UpdateDisplayUseList(NewUseList: TStringList;
  ParamType: TParameterType; DataIndex: integer; const DisplayName: string);
begin
  FParentModel.UpdateDisplayUseList(NewUseList, ParamType, DataIndex, DisplayName);
end;

procedure TChildModel.UpdateLayerCount;
var
  LocalParent: TPhastModel;
begin
  if ParentModel <> nil then
  begin
    ModflowGrid.LayerCount := LayerCount;
    LocalParent := ParentModel as TPhastModel;
    LocalParent.InvalidateMapping;
    LocalParent.InvalidateSegments;
  end;
end;

procedure TChildModel.UpdateMt3dmsChemDataSets;
begin
  ParentModel.UpdateMt3dmsChemDataSets
end;

procedure TChildModel.WriteFluxClosureCriterion(Writer: TWriter);
begin
  Writer.WriteFloat(FFluxClosureCriterion);
end;

procedure TChildModel.WriteFluxRelaxationFactor(Writer: TWriter);
begin
  Writer.WriteFloat(FFluxRelaxationFactor);
end;

procedure TChildModel.WriteHeadClosureCriterion(Writer: TWriter);
begin
  Writer.WriteFloat(FHeadClosureCriterion);
end;

procedure TChildModel.WriteHeadRelaxationFactor(Writer: TWriter);
begin
  Writer.WriteFloat(FHeadRelaxationFactor);
end;

procedure TChildModel.WriteLAYCB(const DiscretizationWriter: TObject);
begin
  Discretization.WriteLAYCB(DiscretizationWriter);
end;

procedure TChildModel.UpdateGrid;
var
  SegIndex: Integer;
  SegmentList: TCellElementSegmentList;
  Segment: TCellElementSegment;
  NewColumnPositions: TOneDRealArray;
  NewRowPositions: TOneDRealArray;
  LocalFirstCol: Integer;
  LocalLastCol: Integer;
  LocalFirstRow: Integer;
  LocalLastRow: Integer;
begin
  if FParentModel = nil then
  begin
    Exit;
  end;
  if FParentModel.Clearing then
  begin
    Exit;
  end;
  if not FCanUpdateGrid then
  begin
    FShouldUpdateGrid := True;
    Exit;
  end;
  LocalFirstCol := FirstCol;
  LocalLastCol := LastCol;
  LocalFirstRow := FirstRow;
  LocalLastRow := LastRow;
  try
  ModflowGrid.GridAngle := (FParentModel as TCustomModel).ModflowGrid.GridAngle;
  FShouldUpdateGrid := False;
  FFirstCol := -1;
  FLastCol := -1;
  FFirstRow := -1;
  FLastRow := -1;
  if (HorizontalPositionScreenObject = nil)
    or (ParentModel.ModflowGrid.RowCount < 3)
    or (ParentModel.ModflowGrid.ColumnCount < 3)then
  begin
    ModflowGrid.RowCount := -1;
    ModflowGrid.ColumnCount := -1;
  end
  else
  begin
    SegmentList := HorizontalPositionScreenObject.Segments[ParentModel];
    if (SegmentList.Count >= 1) then
    begin
      Segment := SegmentList[0];
      FFirstCol :=  Segment.Col;
      FLastCol := FFirstCol;
      FFirstRow :=  Segment.Row;
      FLastRow := FFirstRow;
      for SegIndex := 1 to SegmentList.Count - 1 do
      begin
        Segment := SegmentList[SegIndex];
        if FFirstCol > Segment.Col then
        begin
          FFirstCol := Segment.Col;
        end
        else if FLastCol < Segment.Col then
        begin
          FLastCol := Segment.Col;
        end;
        if FFirstRow > Segment.Row then
        begin
          FFirstRow := Segment.Row;
        end
        else if FLastRow < Segment.Row then
        begin
          FLastRow := Segment.Row;
        end;
      end;
      if FFirstCol = 0 then
      begin
        FFirstCol := 1;
      end;
      if FFirstRow = 0 then
      begin
        FFirstRow := 1;
      end;
      if FLastRow = ParentModel.ModflowGrid.RowCount-1 then
      begin
        FLastRow := ParentModel.ModflowGrid.RowCount-2;
      end;
      if FLastCol = ParentModel.ModflowGrid.ColumnCount-1 then
      begin
        FLastCol := ParentModel.ModflowGrid.ColumnCount-2;
      end;

      GetRowColPositions(FFirstCol, FLastCol,
        ParentModel.ModflowGrid.ColumnPositions, NewColumnPositions);
      ModflowGrid.ColumnPositions := NewColumnPositions;
      GetRowColPositions(FFirstRow, FLastRow,
        ParentModel.ModflowGrid.RowPositions, NewRowPositions);
      ModflowGrid.RowPositions := NewRowPositions;
    end
    else
    begin
      ModflowGrid.RowCount := -1;
      ModflowGrid.ColumnCount := -1;
    end;
  end;
  UpdateLayerCount;
  finally
    if (LocalFirstCol <> FirstCol) or (LocalLastCol <> LastCol)
      or (LocalFirstRow <> FirstRow) or (LocalLastRow <> LastRow) then
    begin
      InvalidateScreenObjects;
    end;
  end;
end;

function TChildModel.Layavg: TOneDIntegerArray;
begin
  Result := ConvertIntegerParentArray(inherited Layavg);
end;

function TChildModel.LayerCount: integer;
var
  DisIndex: Integer;
  EndLayer: Integer;
  Group: TLayerGroup;
  GroupIndex: Integer;
  PriorItem: TChildDiscretization;
  Item: TChildDiscretization;
  NewItems: boolean;
begin
  result := 0;
  NewItems := False;
  for GroupIndex := 1 to LayerStructure.Count - 1 do
  begin
    Group := LayerStructure[GroupIndex];
    if Group.Simulated then
    begin
      if Group = Discretization.BottomLayerGroup then
      begin
        EndLayer := Discretization.BottomLayerInUnit;
      end
      else
      begin
        EndLayer := Group.LayerCount - 1;
      end;
      for DisIndex := 0 to EndLayer do
      begin
        Item := Discretization.GetAnItemByGroupAndLayer(Group, DisIndex);
        if Item = nil then
        begin
          Item := Discretization.Add as TChildDiscretization;
          Item.LayerGroup := Group;
          Item.ParentLayerNumber := DisIndex;
          if DisIndex > 0 then
          begin
            PriorItem := Discretization.GetAnItemByGroupAndLayer(Group, DisIndex - 1);
            Item.Discretization := PriorItem.Discretization;
          end;
          NewItems := True;
        end;
        if (Group = Discretization.BottomLayerGroup)
          and (DisIndex = EndLayer)
          and ((GroupIndex <> LayerStructure.Count - 1)
          or (EndLayer <> Group.LayerCount - 1)) then
        begin
          Inc(result, (Item.Discretization div 2)+1);
        end
        else
        begin
          Inc(result, Item.Discretization);
        end;
      end;
      if Group = Discretization.BottomLayerGroup then
      begin
        break;
      end
    end
    else
    begin
      Inc(result);
    end;
  end;
  if NewItems then
  begin
    Discretization.SortAndDeleteExtraItems;
  end;
end;

function TChildModel.LayerFractions(LayerGroup: TCustomLayerGroup): TDoubleDynArray;
var
  ParentLayerIndex: Integer;
  ArrayLength: Integer;
  Item: TChildDiscretization;
  LayerIndex: Integer;
  FracItem, HigherFracItem: TLayerFraction;
  Fraction: Real;
  ChildLayerIndex: Integer;
  HigherFraction: Real;
  Delta: Real;
begin
  Assert(Discretization.BottomLayerGroup.Index >= LayerGroup.Index);
  if (LayerGroup.Index = 0) then
  begin
    // The uppermost layer group represents the top of the model and never
    // has additional layers above it.
    //
    result := nil;
    Exit;
  end
  else
  begin
    ArrayLength := 0;
    if LayerGroup.Simulated then
    begin
      for ParentLayerIndex := 0 to LayerGroup.LayerCount - 1 do
      begin
        Item := Discretization.GetAnItemByGroupAndLayer(LayerGroup as TLayerGroup, ParentLayerIndex);
        Inc(ArrayLength, Item.Discretization);
      end;
    end
    else
    begin
      ArrayLength := 1;
    end;
    Dec(ArrayLength);
    SetLength(result, ArrayLength);
    LayerIndex := 0;
    HigherFracItem := nil;
    for ParentLayerIndex := 0 to LayerGroup.LayerCollection.Count do
    begin
      if ParentLayerIndex < LayerGroup.LayerCollection.Count then
      begin
        FracItem := LayerGroup.LayerCollection.Items[ParentLayerIndex] as TLayerFraction;
        Fraction := FracItem.Fraction;
      end
      else
      begin
        FracItem := nil;
        Fraction := 0;
      end;
      if HigherFracItem <> nil then
      begin
        HigherFraction := HigherFracItem.Fraction;
      end
      else
      begin
        HigherFraction := 1.;
      end;

      Delta := HigherFraction - Fraction;
      Item := Discretization.GetAnItemByGroupAndLayer(LayerGroup as TLayerGroup, ParentLayerIndex);
      for ChildLayerIndex := Item.Discretization - 1 downto 1 do
      begin
        result[LayerIndex] := Fraction
          + ChildLayerIndex/Item.Discretization * Delta;
        Inc(LayerIndex);
      end;
      if FracItem <> nil then
      begin
        result[LayerIndex] := Fraction;
        Inc(LayerIndex);
      end;
      HigherFracItem := FracItem;
    end;
  end;
  if not ((Discretization.BottomLayerGroup.Index > LayerGroup.Index)
    or ((LayerStructure[LayerStructure.Count-1] = Discretization.BottomLayerGroup)
    and (Discretization.BottomLayerInUnit = Discretization.BottomLayerGroup.LayerCount-1)))
//    and (Discretization.BottomLayerInUnit < Discretization.BottomLayerGroup.LayerCount-1)))
    then
  begin
    // The bottom of the child grid occurs in a layer in the parent grid
    // that is not the bottom most layer.  The child grid ends in the middle
    // of the layer of the parent grid.
    if ArrayLength > 0 then
    begin
      ArrayLength := 0;
      for ParentLayerIndex := 0 to LayerGroup.LayerCount - 1 do
      begin
        Item := Discretization.GetAnItemByGroupAndLayer(LayerGroup as TLayerGroup, ParentLayerIndex);
        if Discretization.BottomLayerInUnit = ParentLayerIndex then
        begin
          Inc(ArrayLength, Item.Discretization div 2);
          break;
        end
        else
        begin
          Inc(ArrayLength, Item.Discretization);
        end;
      end;
//      Item := Discretization.GetAnItemByGroupAndLayer(
//        Discretization.BottomLayerGroup, Discretization.BottomLayerInUnit);
//      ArrayLength := ArrayLength - Item.Discretization div 2;
      SetLength(result, ArrayLength);
    end;
  end

end;

function TChildModel.LayerGroupUsed(LayerGroup: TLayerGroup): boolean;
begin
  result := LayerGroup.Index <= Discretization.FBottomLayerGroup.Index;
end;

function TChildModel.Laytyp: TOneDIntegerArray;
var
  LayerIndex: Integer;
begin
  Result := ConvertIntegerParentArray(inherited Laytyp);
  if ModflowPackages.BcfPackage.IsSelected then
  begin
    // unconfined is only valid for the top layer.
    // convert unconfined to convertible.
    for LayerIndex := 1 to Length(result) - 1 do
    begin
      if (result[LayerIndex] mod 10) = 1 then
      begin
        result[LayerIndex] := result[LayerIndex] + 2;
      end;
    end;
  end;
end;

function TChildModel.Layvka: TOneDIntegerArray;
begin
  Result := ConvertIntegerParentArray(inherited Layvka);
end;

procedure TChildModel.Loaded;
begin
  inherited;
  Discretization.Loaded;
  ModelSelection := ParentModel.ModelSelection;
  UpdateGrid;
end;

function TChildModel.MaxPosition(
  ViewDirection: TViewDirection): integer;
begin
  result := 0;
  case ViewDirection of
    vdTop: result := ModflowGrid.LayerCount;
    vdFront: result := ModflowGrid.RowCount;
    vdSide: result := ModflowGrid.ColumnCount;
    else Assert(False);
  end;
end;

function TChildModel.ModflowConfiningBedCount: integer;
begin
  result := Discretization.ModflowConfiningBedCount;
end;

function TChildModel.ModflowLayerBottomDescription(
  const LayerID: integer): string;
begin
  result := inherited ModflowLayerBottomDescription(
    ChildLayerToParentLayer(LayerID))
    + ' Child layer ' + IntToStr(LayerId+1);
end;

function TChildModel.ModflowLayerCount: integer;
begin
  result := Discretization.ModflowLayerCount;
end;

function TChildModel.ModflowLayerToDataSetLayer(ModflowLayer: integer): integer;
var
  GroupIndex: Integer;
  LayerGroup: TLayerGroup;
  ParentLayerIndex: Integer;
  DisItem: TChildDiscretization;
  MFLayer: Integer;
  ChildDisIndex: Integer;
begin
  result := 0;
  MFLayer := 0;
  for GroupIndex := 1 to LayerStructure.Count - 1 do
  begin
    LayerGroup := LayerStructure[GroupIndex];
    if LayerGroup.Simulated then
    begin
      for ParentLayerIndex := 0 to LayerGroup.LayerCount - 1 do
      begin
        DisItem := Discretization.
          GetAnItemByGroupAndLayer(LayerGroup, ParentLayerIndex);
        for ChildDisIndex := 0 to DisItem.Discretization - 1 do
        begin
          Inc(MFLayer);
          if MFLayer = ModflowLayer then
          begin
            Exit;
          end;
          Inc(result);
        end;
      end;
    end
    else
    begin
      Inc(result);
    end;
  end;
end;

function TChildModel.ParentPositionToChildPositions(
  ViewDirection: TViewDirection; APosition: integer): TGridRange;
begin
  case ViewDirection of
    vdTop: result := ParentLayerToChildLayers(APosition);
    vdFront: result := ParentRowToChildRows(APosition);
    vdSide: result := ParentColToChildCols(APosition);
  end;
end;

function TChildModel.ParentColToChildCols(ACol: integer): TGridRange;
begin
  if (FFirstCol < 0) or (FLastCol < 0)
    or (ACol < FFirstCol) then
  begin
    result.First := -1;
    result.Last := -1;
  end
  else if (ACol > FLastCol) then
  begin
    result.First := ModflowGrid.ColumnCount;
    result.Last := result.First;
  end
  else if ACol = FFirstCol then
  begin
    result.First := 0;
    result.Last := (ChildCellsPerParentCell div 2);
  end
  else if ACol = FLastCol then
  begin
    result.Last := Grid.ColumnCount-1;
    result.First := result.Last - (ChildCellsPerParentCell div 2);
  end
  else
  begin
    result.Last := (ACol-FFirstCol+1)*ChildCellsPerParentCell
      - (ChildCellsPerParentCell div 2) - 1;
    result.First  := result.Last - ChildCellsPerParentCell + 1;
  end;

end;

function TChildModel.ParentLayerToChildLayers(ALayer: integer): TGridRange;
var
  LayerGroupIndex: Integer;
  LayerGroup: TLayerGroup;
  ParentLayerIndex: Integer;
  DisItem: TChildDiscretization;
  CumulativeParentLayers: Integer;
  LastLayer: Integer;
begin
  result.First := -1;
  result.Last := -1;
  if (ALayer >= ModflowGrid.LayerCount) and (ModflowGrid.LayerCount > 0) then
  begin
    result.First := ModflowGrid.LayerCount;
    result.Last := result.First;
    Exit;
  end;
  if ALayer < 0 then
  begin
    Exit;
  end;
  CumulativeParentLayers := 0;
  LastLayer := -1;
  for LayerGroupIndex := 1 to FParentModel.LayerStructure.Count - 1 do
  begin
    LayerGroup := FParentModel.LayerStructure[LayerGroupIndex];
    for ParentLayerIndex := 0 to LayerGroup.LayerCount - 1 do
    begin
      if LayerGroup.Simulated then
      begin
        DisItem := Discretization.
          GetAnItemByGroupAndLayer(LayerGroup,ParentLayerIndex);
        LastLayer := LastLayer + DisItem.Discretization;
      end
      else
      begin
        DisItem := nil;
        LastLayer := LastLayer + 1;
      end;
      if CumulativeParentLayers = ALayer then
      begin
        result.Last := Min(LastLayer, ModflowGrid.LayerCount-1);
        if LayerGroup.Simulated then
        begin
          result.First := LastLayer - DisItem.Discretization + 1;
        end
        else
        begin
          result.First := LastLayer;
        end;
        Exit;
      end;
      Inc(CumulativeParentLayers);
    end;
  end;
end;

function TChildModel.ParentRowToChildRows(ARow: integer): TGridRange;
begin
  if (FFirstRow < 0) or (FLastRow < 0)
    or (ARow < FFirstRow) then
  begin
    result.First := -1;
    result.Last := -1;
  end
  else if (ARow > FLastRow) then
  begin
    result.First := ModflowGrid.RowCount;
    result.Last := result.First;
  end
  else if ARow = FFirstRow then
  begin
    result.First := 0;
    result.Last := (ChildCellsPerParentCell div 2);
  end
  else if ARow = FLastRow then
  begin
    result.Last := Grid.RowCount-1;
    result.First := result.Last - (ChildCellsPerParentCell div 2);
  end
  else
  begin
    result.Last := (ARow-FFirstRow+1)*ChildCellsPerParentCell
      - (ChildCellsPerParentCell div 2) - 1;
    result.First  := result.Last - ChildCellsPerParentCell + 1;
  end;
end;

procedure TChildModel.ReadFluxClosureCriterion(Reader: TReader);
begin
  FFluxClosureCriterion := Reader.ReadFloat;
end;

procedure TChildModel.ReadFluxRelaxationFactor(Reader: TReader);
begin
  FFluxRelaxationFactor := Reader.ReadFloat;
end;

procedure TChildModel.ReadHeadClosureCriterion(Reader: TReader);
begin
  FFluxRelaxationFactor := Reader.ReadFloat;
end;

procedure TChildModel.ReadHeadRelaxationFactor(Reader: TReader);
begin
  FHeadRelaxationFactor := Reader.ReadFloat;
end;

{ TChildDiscretization }

procedure TChildDiscretization.Assign(Source: TPersistent);
var
  SourceChildDiscretization: TChildDiscretization;
begin
  // if Assign is updated, update IsSame too.
  if Source is TChildDiscretization then
  begin
    SourceChildDiscretization := TChildDiscretization(Source);
    LayerGroup := SourceChildDiscretization.LayerGroup;
    LayerGroupName := SourceChildDiscretization.LayerGroupName;
    ParentLayerNumber := SourceChildDiscretization.ParentLayerNumber;
    Discretization := SourceChildDiscretization.Discretization;
  end
  else
  begin
    inherited;
  end;
end;

constructor TChildDiscretization.Create(Collection: TCollection);
begin
  inherited;
  FDiscretization := 1;
end;

function TChildDiscretization.GetDiscretization: integer;
begin
  if LayerGroup.Simulated then
  begin
    result := FDiscretization
  end
  else
  begin
    result := 1
  end;
end;

function TChildDiscretization.GetLayerGroup: TLayerGroup;
var
  Group: TLayerGroup;
  Index: integer;
begin
  if (FLayerGroup = nil) and (FLayerGroupName <> '') then
  begin
    for Index := 0 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
    begin
      Group := frmGoPhast.PhastModel.LayerStructure[Index];
      if Group.AquiferName = FLayerGroupName then
      begin
        FLayerGroup := Group;
        break;
      end;
    end;
  end;
  result := FLayerGroup;
end;

function TChildDiscretization.GetLayerGroupName: string;
begin
  if FLayerGroup <> nil then
  begin
    result := FLayerGroup.AquiferName;
  end
  else
  begin
    result := FLayerGroupName;
  end;
end;

function TChildDiscretization.IsSame(AnotherItem: TOrderedItem): boolean;
var
  AnotherChildDis: TChildDiscretization;
begin
  result := AnotherItem is TChildDiscretization;
  if result then
  begin
    AnotherChildDis := TChildDiscretization(AnotherItem);
    result := (LayerGroupName = AnotherChildDis.LayerGroupName)
      and (ParentLayerNumber = AnotherChildDis.ParentLayerNumber)
      and (Discretization = AnotherChildDis.Discretization);
  end;
end;

procedure TChildDiscretization.Loaded;
begin
  GetLayerGroup;
end;

procedure TChildDiscretization.SetDiscretization(const Value: integer);
begin
  SetIntegerProperty(FDiscretization, Value);
end;

procedure TChildDiscretization.SetLayerGroup(const Value: TLayerGroup);
begin
  if FLayerGroup <> Value then
  begin
    FLayerGroup := Value;
    InvalidateModel;
  end;
  if FLayerGroup <> nil then
  begin
    FLayerGroupName := FLayerGroup.AquiferName;
  end;
end;

procedure TChildDiscretization.SetLayerGroupName(const Value: string);
begin
  SetCaseInsensitiveStringProperty(FLayerGroupName, Value);
  // update FLayerGroup.
  GetLayerGroup;
end;

procedure TChildDiscretization.SetParentLayerNumber(const Value: integer);
begin
  SetIntegerProperty(FParentLayerNumber, Value);
end;

{ TChildDiscretizationCollection }

procedure TChildDiscretizationCollection.Assign(Source: TPersistent);
var
  SourceCollection: TChildDiscretizationCollection;
begin
  inherited;
  // if Assign is updated, update IsSame too.
  if Source is TChildDiscretizationCollection then
  begin
    BeginUpdate;
    try
      SourceCollection := TChildDiscretizationCollection(Source);
      BottomUnitName := SourceCollection.BottomUnitName;
      BottomLayerInUnit := SourceCollection.BottomLayerInUnit;
      BottomLayerGroup := SourceCollection.BottomLayerGroup;
      SortAndDeleteExtraItems;
    finally
      EndUpdate;
    end;
  end;
end;

function TChildDiscretizationCollection.BottomLayerIndex: integer;
var
  ChildModel: TChildModel;
  PhastModel: TPhastModel;
  GroupIndex: Integer;
  LayerGroup: TLayerGroup;
begin
  Assert(Model <> nil);
  ChildModel := Model as TChildModel;
  PhastModel := ChildModel.ParentModel as TPhastModel;
  result := -1;
  for GroupIndex := 1 to PhastModel.LayerStructure.Count - 1 do
  begin
    LayerGroup := PhastModel.LayerStructure[GroupIndex];
    if LayerGroup = ChildModel.Discretization.BottomLayerGroup then
    begin
      Inc(result, ChildModel.Discretization.BottomLayerInUnit+1);
      Exit;
    end
    else
    begin
      Inc(result, LayerGroup.LayerCount);
    end;
  end;
end;

function TChildDiscretizationCollection.BottomModflowParentLayerNumber: integer;
var
  ChildModel: TChildModel;
  PhastModel: TPhastModel;
  GroupIndex: Integer;
  LayerGroup: TLayerGroup;
begin
  Assert(Model <> nil);
  ChildModel := Model as TChildModel;
  PhastModel := ChildModel.ParentModel as TPhastModel;
  result := 0;
  for GroupIndex := 1 to PhastModel.LayerStructure.Count - 1 do
  begin
    LayerGroup := PhastModel.LayerStructure[GroupIndex];
    if LayerGroup.Simulated then
    begin
      if LayerGroup = ChildModel.Discretization.BottomLayerGroup then
      begin
        Inc(result, ChildModel.Discretization.BottomLayerInUnit + 1);
        Exit;
      end
      else
      begin
        Inc(result, LayerGroup.LayerCount);
      end;
    end;
  end;
end;

constructor TChildDiscretizationCollection.Create(Model: TBaseModel);
begin
  inherited Create(TChildDiscretization, Model);
  FBottomLayerInUnit := 0;
end;

{ TChildModelEdit }

procedure TChildModelEdit.Assign(Source: TPersistent);
  procedure AssignSourceModel(SourceModel: TChildModel);
  begin
    ModelName := SourceModel.ModelName;
    Discretization := SourceModel.Discretization;
    ChildCellsPerParentCell := SourceModel.ChildCellsPerParentCell;
    StartingHeadSource := SourceModel.StartingHeadSource;
//    OneWayCoupling := SourceModel.OneWayCoupling;
    CouplingMethod := SourceModel.CouplingMethod;
    MaxIterations := SourceModel.MaxIterations;
    LgrPrintChoice := SourceModel.LgrPrintChoice;
    HeadRelaxationFactor := SourceModel.HeadRelaxationFactor;
    FluxRelaxationFactor := SourceModel.FluxRelaxationFactor;
    HeadClosureCriterion := SourceModel.HeadClosureCriterion;
    FluxClosureCriterion := SourceModel.FluxClosureCriterion;
    FChildModel := SourceModel;
  end;
var
  SourceItem: TChildModelEdit;
begin
  // if Assign is updated, update IsSame too.

    // When editing this section, be sure to edit TChildModel
    // and TChildModelItem too
  if Source is TChildModelEdit then
  begin
    SourceItem := TChildModelEdit(Source);
    ModelName := SourceItem.ModelName;
    Discretization := SourceItem.Discretization;
    ChildCellsPerParentCell := SourceItem.ChildCellsPerParentCell;
    StartingHeadSource := SourceItem.StartingHeadSource;
//    OneWayCoupling := SourceItem.OneWayCoupling;
    CouplingMethod := SourceItem.CouplingMethod;
    MaxIterations := SourceItem.MaxIterations;
    LgrPrintChoice := SourceItem.LgrPrintChoice;
    HeadRelaxationFactor := SourceItem.HeadRelaxationFactor;
    FluxRelaxationFactor := SourceItem.FluxRelaxationFactor;
    HeadClosureCriterion := SourceItem.HeadClosureCriterion;
    FluxClosureCriterion := SourceItem.FluxClosureCriterion;
    FChildModel := SourceItem.FChildModel;
  end
  else if Source is TChildModel then
  begin
    AssignSourceModel(TChildModel(Source));
  end
  else if Source is TChildModelItem then
  begin
    AssignSourceModel(TChildModelItem(Source).ChildModel);
  end
  else
  begin
    inherited;
  end;
end;

procedure TChildModelEdit.AssignTo(Dest: TPersistent);
var
  DestModel: TChildModel;
begin
    // When editing this section, be sure to edit TChildModel
    // and TChildModelItem too
  if Dest is TChildModel then
  begin
    DestModel := TChildModel(Dest);
    DestModel.ModelName := ModelName;
    DestModel.Discretization := Discretization;
    DestModel.ChildCellsPerParentCell := ChildCellsPerParentCell;
    DestModel.StartingHeadSource := StartingHeadSource;
//    DestModel.OneWayCoupling := OneWayCoupling;
    DestModel.CouplingMethod := CouplingMethod;
    DestModel.MaxIterations := MaxIterations;
    DestModel.LgrPrintChoice := LgrPrintChoice;
    DestModel.HeadRelaxationFactor := HeadRelaxationFactor;
    DestModel.FluxRelaxationFactor := FluxRelaxationFactor;
    DestModel.HeadClosureCriterion := HeadClosureCriterion;
    DestModel.FluxClosureCriterion := FluxClosureCriterion;
    DestModel.UpdateGrid;
  end
  else
  begin
    inherited;
  end;
end;

constructor TChildModelEdit.Create(Collection: TCollection);
begin
  inherited;
  FChildCellsPerParentCell := 3;
  FStartingHeadSource := shsSelf;
  FMaxIterations := 20;
  FLgrPrintChoice := lpcListing;
  FFluxRelaxationFactor := 0.5;
  FHeadRelaxationFactor := 0.5;
  FHeadClosureCriterion := 5E-3;
  FFluxClosureCriterion := 5E-2;
  FDiscretization := TChildDiscretizationCollection.Create(nil);
end;

destructor TChildModelEdit.Destroy;
begin
  FDiscretization.Free;
  inherited;
end;

function TChildModelEdit.IsSame(AnotherItem: TOrderedItem): boolean;
var
  AnotherEdit: TChildModelEdit;
  ChildModel: TChildModel;
begin
    // When editing this section, be sure to edit TChildModel
    // and TChildModelItem too

  result := AnotherItem is TChildModelEdit;
  if result then
  begin
    AnotherEdit := TChildModelEdit(AnotherItem);
    result := (ModelName = AnotherEdit.ModelName)
      and (ChildCellsPerParentCell = AnotherEdit.ChildCellsPerParentCell)
      and Discretization.IsSame(AnotherEdit.Discretization)
      and (StartingHeadSource = AnotherEdit.StartingHeadSource)
//      and (OneWayCoupling = AnotherEdit.OneWayCoupling)
      and (CouplingMethod = AnotherEdit.CouplingMethod)
      and (MaxIterations = AnotherEdit.MaxIterations)
      and (LgrPrintChoice = AnotherEdit.LgrPrintChoice)
      and (HeadRelaxationFactor = AnotherEdit.HeadRelaxationFactor)
      and (FluxRelaxationFactor = AnotherEdit.FluxRelaxationFactor)
      and (HeadClosureCriterion = AnotherEdit.HeadClosureCriterion)
      and (FluxClosureCriterion = AnotherEdit.FluxClosureCriterion)
  end
  else
  begin
    result := AnotherItem is TChildModelItem;
    if result then
    begin
      ChildModel := TChildModelItem(AnotherItem).ChildModel;
      result := (ModelName = ChildModel.ModelName)
        and (ChildCellsPerParentCell = ChildModel.ChildCellsPerParentCell)
        and Discretization.IsSame(ChildModel.Discretization)
        and (StartingHeadSource = ChildModel.StartingHeadSource)
//        and (OneWayCoupling = ChildModel.OneWayCoupling)
        and (CouplingMethod = ChildModel.CouplingMethod)
        and (MaxIterations = ChildModel.MaxIterations)
        and (LgrPrintChoice = ChildModel.LgrPrintChoice)
        and (HeadRelaxationFactor = ChildModel.HeadRelaxationFactor)
        and (FluxRelaxationFactor = ChildModel.FluxRelaxationFactor)
        and (HeadClosureCriterion = ChildModel.HeadClosureCriterion)
        and (FluxClosureCriterion = ChildModel.FluxClosureCriterion)
    end;
  end;
end;

procedure TChildModelEdit.SetChildCellsPerParentCell(const Value: integer);
begin
  FChildCellsPerParentCell := Value;
end;

procedure TChildModelEdit.SetCouplingMethod(const Value: TCouplingMethod);
begin
  FCouplingMethod := Value;
end;

procedure TChildModelEdit.SetDiscretization(
  const Value: TChildDiscretizationCollection);
begin
  FDiscretization.Assign(Value);
end;

procedure TChildModelEdit.SetFluxClosureCriterion(const Value: double);
begin
  FFluxClosureCriterion := Value;
end;

procedure TChildModelEdit.SetFluxRelaxationFactor(const Value: double);
begin
  FFluxRelaxationFactor := Value;
end;

procedure TChildModelEdit.SetHeadClosureCriterion(const Value: double);
begin
  FHeadClosureCriterion := Value;
end;

procedure TChildModelEdit.SetHeadRelaxationFactor(const Value: double);
begin
  FHeadRelaxationFactor := Value;
end;

procedure TChildModelEdit.SetLgrPrintChoice(const Value: TLgrPrintChoice);
begin
  FLgrPrintChoice := Value;
end;

procedure TChildModelEdit.SetMaxIterations(const Value: integer);
begin
  FMaxIterations := Value;
end;

procedure TChildModelEdit.SetModelName(const Value: string);
begin
  FModelName := Value;
end;

procedure TChildModelEdit.SetStartingHeadSource(
  const Value: TStartingHeadSource);
begin
  FStartingHeadSource := Value;
end;

{ TChildModelEditCollection }

constructor TChildModelEditCollection.Create;
begin
  inherited Create(TChildModelEdit, nil);
end;

{ TFontChangeNotifyier }

procedure TFontChangeNotifyier.Changed;
begin
  FModel.Invalidate;
end;

constructor TFontChangeNotifyier.Create(AModel: TCustomModel; AFont: TFont);
begin
  inherited Create(AFont);
  FModel := AModel;
end;

procedure TCustomModel.SetPathLine(const Value: TPathLineReader);
begin
  if FPathLine = nil then
  begin
    FPathLine := TPathLineReader.Create(self);
  end;
  FPathLine.Assign(Value);
end;

function TCustomModel.GetPathLine: TPathLineReader;
begin
  if (FPathLine = nil) then
  begin
    FPathLine := TPathLineReader.Create(self);
  end;
  result := FPathLine;
end;

function TCustomModel.StorePathLine: boolean;
begin
  result := (FPathLine <> nil) and (FPathLine.FileName <> '');
end;

function TCustomModel.StoreTimeSeries: boolean;
begin
  result := (FTimeSeries <> nil) and (FTimeSeries.FileName <> '');
end;

procedure TCustomModel.SetTimeSeries(const Value: TTimeSeriesReader);
begin
  if FTimeSeries = nil then
  begin
    FTimeSeries := TTimeSeriesReader.Create(self);
  end;
  FTimeSeries.Assign(Value);
end;

function TCustomModel.GetTimeSeries: TTimeSeriesReader;
begin
  if (FTimeSeries = nil) then
  begin
    FTimeSeries := TTimeSeriesReader.Create(self);
  end;
  result := FTimeSeries;
end;

function TCustomModel.GetTopDataSet: TDataArray;
begin
  result := nil;
  case ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast, msModflow, msModflowLGR, msModflowNWT:
      begin
        result := Grid.TopDataSet;
      end;
    {$IFDEF SUTRA}
    msSutra:
      begin
        if (Mesh <> nil) then
        begin
          result := Mesh.TopDataSet;
        end;
      end;
    {$ENDIF}
    else
       Assert(False);
  end;
end;

function TCustomModel.StoreEndPoints: boolean;
begin
  result := (FEndPoints <> nil) and (FEndPoints.FileName <> '');
end;

function TCustomModel.GetEndPoints: TEndPointReader;
begin
  if (FEndPoints = nil) then
  begin
    FEndPoints := TEndPointReader.Create(self);
  end;
  result := FEndPoints;
end;

function TCustomModel.GetFrontDataSet: TDataArray;
begin
  result := nil;
  case ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast, msModflow, msModflowLGR, msModflowNWT:
      begin
        result := Grid.FrontDataSet;
      end;
    {$IFDEF SUTRA}
    msSutra:
      begin
        // do nothing
      end;
    {$ENDIF}
    else
       Assert(False);
  end;
end;

procedure TCustomModel.SetEndPoints(const Value: TEndPointReader);
begin
  if FEndPoints = nil then
  begin
    FEndPoints := TEndPointReader.Create(self);
  end;
  FEndPoints.Assign(Value);
end;

initialization

  RegisterClass(TPhastModel);
  RegisterClass(TChildModel);

end.



