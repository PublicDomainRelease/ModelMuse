unit Modflow2005ImporterUnit;

interface

// Windows is used so that AnsiCompareText will be inlined. 
uses Windows, SubscriptionUnit, SysUtils, Classes, Contnrs, JclSysUtils,
  ModflowPackageSelectionUnit, Dialogs;

{
Import the MODFLOW-2005 model whose name file is ListFileName.
XOrigin, YOrigin are the X and Y coordinates of the corner of the model at
Row 1, Column1. GridAngle is the angle of the grid and textHandler
is a procedure passed in to handle data read by @name.

This procedure is implemented using classes in the implementation section.
The most important of these is TModflow2005Importer.  In
TModflow2005Importer.Create, instances of other classes are associated
with the symbols used to identify the packages in MODFLOW.
Other important base classes include TPackageImporter, TListImporter,
TArrayImporter, and TObjectArray.
}
procedure ImportModflow2005(const ListFileName: string;
  XOrigin, YOrigin, GridAngle: double; textHandler: TTextHandler);

implementation

uses ModflowGridUnit, AbstractGridUnit, ScreenObjectUnit, GoPhastTypes,
  frmGoPhastUnit, PhastModelUnit, FastGEO, UndoItems, ValueArrayStorageUnit,
  DataSetUnit, Math, GIS_Functions, InterpolationUnit, RbwParser,
  ModflowTimeUnit, LayerStructureUnit, ModflowConstantHeadBoundaryUnit,
  ModflowParameterUnit, OrderedCollectionUnit, IntListUnit,
  ModflowTransientListParameterUnit, ModflowBoundaryUnit, ModflowWellUnit,
  ModflowGhbUnit, ModflowDrnUnit, ModflowRivUnit, ModflowRchUnit, ContourUnit,
  QuadTreeClass, frmImportModflowUnit, ModflowEvtUnit, RealListUnit,
  ModflowSfrUnit, ModflowSfrReachUnit, ModflowSfrParamIcalcUnit,
  ModflowSfrTable, ModflowSfrFlows, ModflowSfrChannelUnit,
  ModflowSfrEquationUnit, ModflowSfrSegment, ModflowSfrUnsatSegment, 
  ModflowHobUnit, ModflowHfbUnit, ModflowLakUnit, ModflowDrtUnit, 
  ModflowEtsUnit, ModflowResUnit, ModflowUzfUnit, ModflowGageUnit, 
  HufDefinition, FluxObservationUnit, ModflowMnw2Unit, ModflowSubsidenceDefUnit;

resourcestring
  StrHydCondAlongRows = 'HYD. COND. ALONG ROWS';
  StrHorizAnisotropy = 'HORIZ. ANI. (COL./ROW)';
  StrVerticalHydCond = 'VERTICAL HYD. COND.';
  StrHorizToVertAnis = 'HORIZ. TO VERTICAL ANI.';
  StrQuasi3DVertHydCond = 'QUASI3D VERT. HYD. COND.';
  StrSpecificStorage = 'SPECIFIC STORAGE';
  StrSpecificYield = 'SPECIFIC YIELD';
  StrWetDry = 'WETDRY PARAMETER';
  StrStorageCoef = 'STORAGE COEFFICIENT';
  StrAll = 'ALL';
  StrNone = 'NONE';
  StrImportedRechargeSt = 'Imported_Recharge_StressPeriod';
  StrImportedRecharge = 'ImportedRecharge';
  StrImportedRechargeLa = 'Imported_Recharge_Layer_StressPeriod';
  StrImportedRechargeEl = 'Imported_Recharge_Elevation';
  StrImportedEvtEl = 'Imported_Evt_Elevation';
  StrImportedEvtLa = 'Imported_Evt_Layer_StressPeriod';
  StrImportedEvtSt = 'Imported_EvtRate_StressPeriod';
  StrImportedEvtSurfSt = 'Imported_EvtSurface_StressPeriod';
  StrImportedEvtExtinctDepthSt = 'Imported_EvtExtinctionDepth_StressPeriod';
  StrLAKEIDARRAY = 'LAKE ID ARRAY';
  StrLAKEBEDLEAKANCEARR = 'LAKEBED LEAKANCE ARRAY';
  StrImportedLakeBottom = 'Imported_Lake_Bottom';
  StrETLAYERINDEXIETS = 'ET LAYER INDEX (IETS)';
  StrETSURFACEETSS = 'ET SURFACE (ETSS)';
  StrEVAPOTRANSRATEET = 'EVAPOTRANS. RATE (ETSR)';
  StrEXTINCTIONDEPTHET = 'EXTINCTION DEPTH (ETSX)';
  StrImportedETSFractionalRate = 'Imported_ETS_FractionalRate_';
  StrImportedETSFractionalDepth = 'Imported_ETS_FractionalDepth_';
  StrETSImportedFractionalDepth = 'ETS_Imported_FractionalDepth_';
  StrETSImportedFractionalRate = 'ETS_Imported_FractionalRate_';
  StrTOPELEVATN = 'TOP ELEVATN: ';
  StrTHICKNESS = 'THICKNESS: ';
  StrCOLUMNTOROWANISOT = 'COLUMN TO ROW ANISOTROPY';
  StrPRIMARYSTORAGECOEF = 'PRIMARY STORAGE COEF';
  StrTRANSMISALONGROWS = 'TRANSMIS. ALONG ROWS';
  StrHYDCONDALONGROW = 'HYD. COND. ALONG ROWS';
  StrVERTHYDCONDTHICK = 'VERT HYD COND /THICKNESS';
  StrSECONDARYSTORAGECO = 'SECONDARY STORAGE COEF';
  StrWETDRYPARAMETER = 'WETDRY PARAMETER';
  StrNUMBEROFBEDSINSY = 'NUMBER OF BEDS IN SYSTEM';
  StrPRECONSOLIDATIONHEA = 'PRECONSOLIDATION HEAD';
  StrELASTICINTERBEDSTO = 'ELASTIC INTERBED STORAGE';
  StrVIRGININTERBEDSTOR = 'VIRGIN INTERBED STORAGE';
  StrSTARTINGCOMPACTION = 'STARTING COMPACTION';
  StrDELAYSTARTINGHEAD = 'DELAY STARTING HEAD';
  StrDELAYPRECOLSOLHEA = 'DELAY PRECOLSOL. HEAD';
  StrDELAYINITIALCOMPAC = 'DELAY INITIAL COMPACTION';
  StrDELAYINTERBEDTHICK = 'DELAY INTERBED THICKNESS';

const
  StrConstant1DRealArray = 'CONSTANT 1D REAL ARRAY:';
  StrVariable1DRealArray = 'VARIABLE 1D REAL ARRAY:';
  StrConstant2DRealArray = 'CONSTANT 2D REAL ARRAY:';
  StrConstant2DRealArrayForLayer = 'CONSTANT 2D REAL ARRAY FOR A LAYER:';
  StrVariable2DRealArray = 'VARIABLE 2D REAL ARRAY:';
  StrVariable2DRealArrayForLayer = 'VARIABLE 2D REAL ARRAY FOR A LAYER:';
  StrVariable2DRealArrayForCrossSection
    = 'VARIABLE 2D REAL ARRAY FOR CROSS SECTION:';
  StrConstant2DIntegerArrayForLayer = 'CONSTANT 2D INTEGER ARRAY FOR A LAYER:';
  StrConstant2DIntegerArray = 'CONSTANT 2D INTEGER ARRAY:';
  StrVariable2DIntegerArrayForLayer = 'VARIABLE 2D INTEGER ARRAY FOR A LAYER:';
  StrVariable2DIntegerArrayForCrossSection
    = 'VARIABLE 2D INTEGER ARRAY FOR CROSS SECTION:';
  StrVariable2DIntegerArray = 'VARIABLE 2D INTEGER ARRAY:';
  StrInitialHead = 'INITIAL HEAD';
  BoundaryArray = 'BOUNDARY ARRAY';

Type
  TDoubleArray = array of double;
  T2DDoubleArray = array of array of double;
  T2DIntArray = array of array of integer;
  TSurfacePointArray = array of array of TPoint2D;
  T3DDoubleArray = array of T2DDoubleArray;
  T3DIntArray = array of T2DIntArray;

  TMultZoneImporter = class;

  TImportedStressPeriod = record
    PERLEN: double;
    NSTP: integer;
    TSMULT: double;
    ISSFLG: integer;
  end;

  TRealConstantRecord = record
    IsConstant: boolean;
    RealValue: double;
  end;

  TRealConstantRecordArray = array of TRealConstantRecord;

  TIntegerConstantRecord = record
    IsConstant: boolean;
    IntegerValue: integer;
  end;

  TIntegerConstantArray = array of TIntegerConstantRecord;

  TClusterRecord = record
    Layer: integer;
    MultiplierName: string;
    ZoneName: string;
    ZoneValues: array of integer;
  end;
  
  TClusterRecordArray = array of TClusterRecord;

  TInstanceRecord = record
    InstanceName: string;
    Clusters: TClusterRecordArray;
  end;

  TArrayParameterRecord = record
    PARNAM: string;
    PARTYP: string;
    Parval: Double;
    Instances: array of TInstanceRecord;
  end;

  TGageImporter = class;
  TChdObsImporter = class;
  TPvalImporter = class;

  TModflow2005Importer = class(TObject)
  strict private
    FCenterPoints: TSurfacePointArray;
  private
    FListFileName: string;
    FPackageIdentifiers: TStringList;
    FFile: TextFile;
    FPointsComputed: Boolean;
    FGageImporter: TGageImporter;
    FChobImporter: TChdObsImporter;
    FPvalImporter: TPvalImporter;
    function GetCenterPoints: TSurfacePointArray;
    property CenterPoints: TSurfacePointArray read GetCenterPoints;
  public
    textHandler: TTextHandler;
    Constructor Create(const ListFileName: string;
      XOrigin, YOrigin, GridAngle: double);
    Destructor Destroy; override;
    procedure ImportModel;
  end;

  TPackageImporter = class(TObject)
  private
    FImportedPackage: boolean;
    procedure AssignConstant2DIntArray(Value: integer; Array2D: T2DIntArray);
    function CreateScreenObject(const Name: string): TScreenObject;
  protected
    FComments: TStringList;
    FImporter: TModflow2005Importer;
    FPackageIdentifier: string;
    FModel: TPhastModel;
    FGrid: TModflowGrid;
    // @name is passed a label indicating the data that has been read from
    // the @link(FImporter) file and reads that data from the file and stores
    // it.
    procedure ReadData(const ALabel: string); virtual;
    // @name takes the data it has stored and maodifies @link(FModel) to
    // include that data.  This often means that @link(TDataArray)s and
    // @link(TScreenObject)s are created.
    procedure HandlePackage; virtual;
    procedure Read1DRealArray(var DoubleArray: TOneDRealArray);
    procedure Read2DRealArray(var DoubleArray: T2DDoubleArray);
    procedure AssignConstant2DArray(Value: Double; Array2D: T2DDoubleArray);
    procedure AssignConstant1DArray(AnArray: TOneDRealArray; Value: Double);
    // @name creates a @link(TScreenObject) that has a vertex at the center
    // of the each cell as seen from the top view of the model.  Each vertex
    // is a new section. When adding verticies, the loop over columns is
    // inside the loop over rows.
    procedure CreateOrRetrieveCellCenterScreenObject(var ScreenObject: TScreenObject);
    procedure AssignRealValuesToCellCenters(DataArray: TDataArray;
      ScreenObject: TScreenObject; ImportedData: T2DDoubleArray);
    procedure AssignIntegerValuesToCellCenters(DataArray: TDataArray;
      ScreenObject: TScreenObject; ImportedData: T2DIntArray);
    procedure AssignBooleanValuesToCellCenters(DataArray: TDataArray;
      ScreenObject: TScreenObject; ImportedData: T2DIntArray);
    procedure ReadVariable2DIntArray(IntArray: T2DIntArray);
    procedure CreateDataArrayAndAssignValues(ScreenObject: TScreenObject;
      const DataArrayName: string; ImportedValues: T2DDoubleArray);
    procedure CheckRealConstArray(out ConstantValue: Double;
      out IsConstant: Boolean; const ArrayToCheck: TRealConstantRecordArray);
    function FixArrayName(const ArrayName: string): string;
    procedure AssignImportedValues(ImportedValues: TValueArrayItem;
      ImportedData: T2DDoubleArray); overload;
    procedure AssignImportedValues(ImportedValues: TValueArrayItem;
      ImportedData: TDoubleArray); overload;
    function UniformArray(ImportedData: TDoubleArray): boolean;
  public
    Constructor Create(Importer: TModflow2005Importer;
      const PackageIdentifier: string);
    Destructor Destroy; override;
    procedure ImportPackage(out NextPackageIndex: integer);
    property ImportedPackage: boolean read FImportedPackage;
  end;

  TNamImporter = class(TPackageImporter)
  private
    FUnitNumbers: TIntegerList;
    FFileNames: TStringList;
  protected
    procedure ReadData(const ALabel: string); override;
  public
    Constructor Create(Importer: TModflow2005Importer);
    Destructor Destroy; override;
  end;

  TDisImporter = class(TPackageImporter)
  strict private
    FOriginPoint: TPoint2D;
    FGridAngle: Double;
    NLAY: integer;
    NROW: integer;
    NCOL: integer;
    NPER: integer;
    ITMUNI: integer;
    LENUNI: integer;
    LAYCBD: array of integer;
    DELR: TOneDRealArray;
    DELC: TOneDRealArray;
    FElevations: array of T2DDoubleArray;
    FConstantElevations : array of TRealConstantRecord;
    FStressPeriods: array of TImportedStressPeriod;
    procedure ReadDataSet1;
    procedure ReadDataSet2;
    procedure ReadConstantDelrDelc;
    procedure ReadVariableDelrDelc;
    function GetLayerBottomIndex(Layer: Integer): integer;
    function GetConfiningBedBottomIndex(Layer: Integer): integer;
    procedure ReadConstantTopElev;
    procedure ReadConstantBottomElev;
    procedure ReadVariableTopElev;
    procedure ReadVariableBottomElev;
    procedure ImportStressPeriods;
    procedure ImportElevations;
    procedure ImportRowsAndColumns;
    procedure ReadStressPeriod;
    procedure InitializeView;
    procedure ReleaseMemory;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer;
      XOrigin, YOrigin, GridAngle: double);
  end;

  TMultZoneImporter = class(TPackageImporter)
  strict private
    FZoneArrays: array of T2DIntArray;
    FConstantZoneArrays : array of TIntegerConstantRecord;
    FZoneNames: TStringList;
    FMultArrays: array of T2DDoubleArray;
    FConstantMultArrays : array of TRealConstantRecord;
    FMultNames: TStringList;
    FCurrentMultName: string;
    FCurrentZoneName: string;
    procedure ReadNumberOfZones;
    procedure ReadNumberOfMultipliers;
    procedure ReadMultiplierName;
    procedure ReadConstantMultiplier;
    procedure ReadVariableMultiplier;
    procedure ReadMultiplierDefinedByFunction;
    procedure ReadZoneName;
    procedure ReadConstantZoneArray;
    procedure ReadVariableZoneArray;
    procedure ImportZones;
    procedure ImportMultipliers;
//    procedure ReleaseMemory;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Function IndexOfZone(Name: string): integer;
    function ConstantZone(Index: integer; Out ZoneValue: integer): boolean;
    function ZoneArray(Index: integer): T2DIntArray;
    Constructor Create(Importer: TModflow2005Importer);
    destructor Destroy; override;
  end;

  TArrayImporter = class(TPackageImporter)
  protected
    procedure InitializeConstArray(ConstArray: TRealConstantRecordArray);
    procedure InitializeConstIntArray(ConstArray: TIntegerConstantArray);
    procedure ImportDataSet(ImportName: string; ImportArrayName: string;
      const ThreeDConstRealArray: TRealConstantRecordArray;
      const ThreeDRealArray: T3DDoubleArray;
      const LayerFormulaSuffix: string = '');
  end;

  TArrayParameterImporter = class (TArrayImporter)
  protected
    FNextParameterIndex: integer;
    FParameters: array of TArrayParameterRecord;
    procedure ReadArrayParameter;
    function ReadInstance: boolean; virtual;
  end;

  TLpfImporter = class(TArrayParameterImporter)
  private
    FIsSelected: boolean;
    ILPFCB: integer;
    HDRY: double;
    FStorageCoefficientChoice: Boolean;
    FComputeVkUsingCellThickness: Boolean;
    FComputeThicknessUsingStartingHead: Boolean;
    FNoVerticalFlowCorrection: Boolean;
    LAYTYP: array of integer;
    LAYAVG: array of integer;
    CHANI: array of double;
    LAYVKA: array of integer;
    LAYWET: array of integer;
    WETFCT: double;
    IWETIT: integer;
    IHDWET: integer;
    FHk: T3DDoubleArray;
    FHorizontalAnisotropy: T3DDoubleArray;
    FVerticalK: T3DDoubleArray;
    FHorizontalToVerticalAnisotropy: T3DDoubleArray;
    FQuasiVerticalK: T3DDoubleArray;
    FSpecificStorage: T3DDoubleArray;
    FSpecificYield: T3DDoubleArray;
    FWetDry: T3DDoubleArray;
    FStorageCoefficient: T3DDoubleArray;
    FHkConst: TRealConstantRecordArray;
    FHorizontalAnisotropyConst: TRealConstantRecordArray;
    FVerticalKConst: TRealConstantRecordArray;
    FHorizontalToVerticalAnisotropyConst: TRealConstantRecordArray;
    FQuasiVerticalKConst: TRealConstantRecordArray;
    FSpecificStorageConst: TRealConstantRecordArray;
    FSpecificYieldConst: TRealConstantRecordArray;
    FWetDryConst: TRealConstantRecordArray;
    FStorageCoefficientConst: TRealConstantRecordArray;
    procedure ReadDataSet1;
    procedure ReadDataSet1Options;
    procedure ReadDataSets2to6;
    procedure ReadDataSet7;
    procedure ReadDataSets10to16Variable;
    procedure ReadDataSets10to16Constant;
    procedure ImportDataSet1(LpfPackage: TLpfSelection);
    procedure ImportDataSet2;
    procedure ImportDataSet3;
    procedure ImportHorizontalAnisotropy;
    procedure ImportDataSet5;
    procedure ImportDataSet6;
    procedure ImportDataSet7;
    procedure ImportLpfParameters;
    procedure ImportHorizontalHydraulicConductivity;
    procedure ImportVerticalHydraulicConductivity;
    procedure ImportSpecificStorage;
    procedure ImportSpecificYield;
    procedure ImportWetDry;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer);
  end;

  TBcfImporter = class(TArrayImporter)
  private
    FIsSelected: Boolean;
    HDRY: double;
    WETFCT: double;
    IWDFLG: integer;
    IWETIT: integer;
    IHDWET: integer;
    NLAY: integer;
    LAYCON: TOneDIntegerArray;
    TRPY: TOneDRealArray;
    FConfinedStorage: T3DDoubleArray;
    FConfinedStorage_Const: TRealConstantRecordArray;
    FTran_Const: TRealConstantRecordArray;
    FTran: T3DDoubleArray;
    FHY_Const: TRealConstantRecordArray;
    FHY: T3DDoubleArray;
    FVcont_Const: TRealConstantRecordArray;
    FVcont: T3DDoubleArray;
    FSpecificYield_Const: TRealConstantRecordArray;
    FSpecificYield: T3DDoubleArray;
    FWetDry_Const: TRealConstantRecordArray;
    FWetDry: T3DDoubleArray;
//    FModelLayer: Integer;
    FTransientModel: Boolean;
    procedure ReadDataSet1;
    procedure ReadDataSet2;
    procedure InitializeContArray(var ConstArray: TRealConstantRecordArray);
    procedure Initialize3DArray(var ThreeDArray: T3DDoubleArray);
    procedure ReadVariableAnisotropy;
    procedure ReadConstantAnisotropy;
    procedure ReadVariableArrays;
    procedure ReadConstantArrays;
    procedure ImportDataSet1;
    procedure ImportDataSets2And3;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer);
  end;

  THufRecord = record
    HUFNAME: string;
    HGUHANI: double;
    HGUVANI: double;
    PrintHK: integer;
    PrintHANI: integer;
    PrintVK: integer;
    PrintSS: integer;
    PrintSY: integer;
    PrintCode: integer;
  end;

  TKdepImporter = class;
  TLvdaImporter = class;

  TCustomHufImporter = class(TArrayParameterImporter)
  protected
    procedure AssignHufParametersZoneAndMultiplier;
  end;

  THufImporter = class(TCustomHufImporter)
  strict private
    HDRY: double;
    FHydrogeologicUnits: array of THufRecord;
    FNextHufIndexIndex: Integer;
    LTHUF: array of integer;
    LAYWT: array of double;
    WETFCT: double;
    IWETIT: Integer;
    IHDWET: Integer;
    NHUF: integer;
    NPHUF: integer;
    IOHUFHDS: integer;
    IOHUFFLWS: integer;
    IsConstTopHuf: Boolean;
    Top: T3DDoubleArray;
    IsConstThicknessHuf: Boolean;
    Thickness: T3DDoubleArray;
    FWetDry: T3DDoubleArray;
    FConstantTopElevations: array of TRealConstantRecord;
    FConstantThicknesses: array of TRealConstantRecord;
    HufPackage: THufPackageSelection;
    FWetDryConst: TRealConstantRecordArray;
    FIsSelected: Boolean;
  private
    FReadLthuf: Boolean;
    FReadKdep: boolean;
    FReadLvda: boolean;
    FKdepImporter: TKdepImporter;
    FLvdaImporter: TLvdaImporter;
    procedure ReadDataSet1;
    procedure ReadDataSet2;
    procedure ReadDataSet3;
    procedure ReadDataSet4;
    procedure ReadVariableWetDry;
    procedure ReadConstantWetDry;
    procedure ReadDataSet6;
    procedure ReadVariableHguGeometry;
    procedure ReadConstantHguGeometry;
    procedure ReadDataSet9;
    procedure ReadDataSet12;
    procedure ImportDataSet1;
    procedure ImportDataSet2;
    procedure ImportDataSet3;
    procedure ImportDataSet4;
    procedure ImportWetDry;
    procedure ImportHydrogeologicUnits;
    procedure ImportGeometry;
    function ReadInstance: boolean; override;
    procedure AssignSteadyParametersZoneAndMultiplier;
    procedure CreateParameters(HydrogeologicUnits: THydrogeologicUnits);
    procedure ReleaseMemory;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer);
  end;

  TKdepImporter = class(TCustomHufImporter)
  private
    FHufImporter: THufImporter;
    NPKDEP: integer;
    IFKDEP: integer;
    FGroundSurface: T2DDoubleArray;
    FConstantGroundSurface: double;
    FIsConstantGroundSurface: Boolean;
    FHufPackage: THufPackageSelection;
    procedure ReadDataSet1;
    procedure ReadVariableGroundSurface;
    procedure ReadConstantGroundSurface;
    procedure ImportReferenceChoice;
    procedure ImportReferenceLayer;
    procedure CreateParameters;
  protected
    function ReadInstance: boolean; override;
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer;
      HufImporter: THufImporter);
  end;

  TLvdaImporter = class(TArrayParameterImporter)
  private
    FHufImporter: THufImporter;
    NPLVDA: integer;
    procedure ReadDataSet1;
    procedure CreateParameters;
  protected
    function ReadInstance: boolean; override;
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer;
      HufImporter: THufImporter);
  end;

  TSolverImporter = class (TPackageImporter)
  protected
    procedure DeselectAllSolvers;
    procedure HandlePackage; override;
  end;

  TPcgImporter = class (TSolverImporter)
  private
    MXITER: integer;
    ITER1: integer;
    HCLOSEPCG: double;
    RCLOSEPCG: double;
    RELAXPCG: double;
    NBPOL: integer;
    IPRPCG: integer;
    MUTPCG: integer;
    DAMPPCG: double;
    DAMPPCGT: double;
    NPCOND: integer;
    procedure ReadDataSet1;
    procedure ReadDataSet2;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer);
  end;

  TGmgImporter = class(TSolverImporter)
  private
    RCLOSE: double;
    IITER: integer;
    HCLOSE: double;
    MXITER: integer;
    DAMP: double;
    IADAMP: integer;
    IOUTGMG: integer;
    IUNITMHC: integer;
    ISM: integer;
    ISC: integer;
    DUP: double;
    DLOW: double;
    CHGLIMIT: double;
    RELAX: double;
    FGmgPackage: TGmgPackageSelection;
    procedure ReadDataSet1;
    procedure ReadDataSet2;
    procedure ReadPartialDataSet3;
    procedure ReadFullDataSet3;
    procedure ReadDataSet4;
    procedure ImportDataSet1;
    procedure ImportDataset2;
    procedure ImportDataSet3;
    procedure ImportDataSet4;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer);
  end;

  TSipImporter = class(TSolverImporter)
  private
    MXITER: integer;
    NPARM: integer;
    ACCL: double;
    HCLOSE: double;
    IPCALC: integer;
    WSEED: double;
    IPRSIP: integer;
    FSipPackage: TSIPPackageSelection;
    procedure ReadDataSet1;
    procedure ReadDataSet2;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer);
  end;

  TDe4Importer = class(TSolverImporter)
  private
    ITMX: integer;
    MXUP: integer;
    MXLOW: integer;
    MXBW: integer;
    IFREQ: integer;
    MUTD4: integer;
    ACCL: double;
    HCLOSE: double;
    IPRD4: integer;
    FDe4Package: TDE4PackageSelection;
    procedure ReadDataSet1;
    procedure ReadDataSet2;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer);
  end;

  TArrayMember = class(TObject)
    Constructor Create; virtual;
  end;

  TArrayMemberClass = class of TArrayMember;

  TObjectArray = class(TArrayMember)
  strict private
    FObjects: array of TArrayMember;
  private
    function GetArrayLength: integer;
    function GetObject(Index: integer): TArrayMember;
    procedure SetArrayLength(const Value: integer);
  strict protected
    property Objects[Index: integer]: TArrayMember
      read GetObject;
  protected
    function ArrayMemberClass: TArrayMemberClass; virtual; abstract;
  public
    property ArrayLength: integer read GetArrayLength
      write SetArrayLength;
    Destructor Destroy; override;
  end;

  TLocation = class(TArrayMember)
    Layer: integer;
    Row: integer;
    Column: integer;
    Used: Boolean;
    AuxilliaryVariables: array of double;
    FObservationGroups: TList;
    FObservationCells: TList;
    Constructor Create; override;
    Destructor Destroy; override;
    function SameObservations(ALocation: TLocation): boolean;
  end;

  TListInstanceObject = class(TObjectArray)
  private
    function GetLocations(Index: integer): TLocation;
  public
    Name: string;
    property Locations[Index: integer]: TLocation read GetLocations; default;
  end;

  TClusterObject = class(TArrayMember)
    MultiplierArray: string;
    ZoneArray: string;
    Zones: array of integer;
    function SimilarZone(Cluster: TClusterObject): boolean;
  end;

  TArrayInstanceObject = class(TObjectArray)
  private
    function GetCluster(Index: integer): TClusterObject;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  published
  public
    Name: string;
    property Clusters[Index: integer]: TClusterObject read GetCluster; default;
  end;

  TListParameterObject = class(TObjectArray)
  private
    function GetInstance(Index: integer): TListInstanceObject;
  public
    PARNAM: string;
    ModifiedParamName: string;
    PARTYP: string;
    Parval: Double;
    property Instances[Index: integer]: TListInstanceObject
      read GetInstance; default;
    function GetInstanceByName(const InstanceName: string): TListInstanceObject;
  end;

  TArrayParameterObject = class(TObjectArray)
  private
    function GetInstance(Index: integer): TArrayInstanceObject;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    PARNAM: string;
    ModifiedParamName: string;
    PARTYP: string;
    Parval: Double;
    property Instances[Index: integer]: TArrayInstanceObject
      read GetInstance; default;
    function GetInstanceByName(
      const InstanceName: string): TArrayInstanceObject;
  end;

  TListParamArray = class(TObjectArray)
  private
    function GetParams(Index: integer): TListParameterObject;
  public
    property Params[Index: integer]: TListParameterObject
      read GetParams; default;
    function GetParamByName(const ParamName: string): TListParameterObject;
  end;

  TArrayParamArray = class(TObjectArray)
  private
    function GetParams(Index: integer): TArrayParameterObject;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property Params[Index: integer]: TArrayParameterObject
      read GetParams; default;
  end;

  TArrayStressPeriod = class(TArrayMember)
    Reuse: boolean;
    Parameters: array of string;
    Instances: array of string;
  end;

  TListStressPeriod = class(TObjectArray)
  private
    Reuse: boolean;
    Parameters: array of string;
    Instances: array of string;
    function GetBoundary(Index: integer): TLocation;
  public
    property Boundaries[Index: integer]: TLocation
      read GetBoundary; default;
    function IndexOfParameter(const ParameterName: string): integer;
  end;

  TArrayStressPeriodArray = class(TObjectArray)
  private
    function GetStressPeriod(Index: integer): TArrayStressPeriod;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property StressPeriods[Index: integer]: TArrayStressPeriod
      read GetStressPeriod; default;
  end;

  TListStressPeriodArray = class(TObjectArray)
  private
    function GetStressPeriod(Index: integer): TListStressPeriod;
  public
    property StressPeriods[Index: integer]: TListStressPeriod
      read GetStressPeriod; default;
  end;

  TChdInstanceObject = class(TListInstanceObject)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TChdParameterObject = class(TListParameterObject)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TChdParamArray = class(TListParamArray)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TChdStressPeriod = class(TListStressPeriod)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TChdStressPeriodArray = class(TListStressPeriodArray)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TCustomFlowObservationImporter = class;

  TListImporter = class(TPackageImporter)
  private
    FParameters: TListParamArray;
    NP: integer;
    FAuxillaryVariables: TStringList;
    FNoPrint: Boolean;
    CurrentParameter: integer;
    CurrentInstance: integer;
    FCurrentStressPeriod: integer;
    ITMP: integer;
    NLST: integer;
    FStressPeriods: TListStressPeriodArray;
    FBoundaryIndex: Integer;
    FObsImporter: TCustomFlowObservationImporter;
    // @name creates a @link(TScreenObject) and adds it to the model.
    // The name of the object will incorporate the stress period number if
    // StressPeriodIndex is greater than or equal to 0.
    // List contains @link(TLocation)s which define where the verticies of
    // the @link(TScreenObject).
    // LayerIndex is used to help determine the @link(TScreenObject.Name)
    // as well as the elevation formula.
    function CreateScreenObject(List: TList;
      var ScreenObjectIndex: Integer; LayerIndex,
      StressPeriodIndex: integer): TScreenObject; virtual;
    procedure SetItemValues(Item: TCustomModflowBoundaryItem;
      Boundaries: TList; EndTime: Double; StartTime: Double;
      ScreenObject: TScreenObject; const ParamName: string); virtual; abstract;
    function GetBoundary(ScreenObject: TScreenObject): TModflowParamBoundary;
      virtual; abstract;
    procedure ImportNonParameterBoundaries(var ScreenObjectIndex: Integer);
    procedure ImportParameterBoundaries(ScreenObjectIndex: Integer);
    function ScreenObjectNameRoot: string; virtual; abstract;
    procedure InitializeCurrentStressPeriod;
    procedure InitializeStressPeriods;
    procedure ReadAuxilliaryVariableName;
    procedure ReadParameterName;
    procedure ReadParameterValueAndLocationCount; virtual;
    procedure ReadNumberOfInstances; virtual;
    procedure ReadInstanceName;
    procedure ReadFirstStressPeriodDataSet5WithParameters;
    procedure ReadFirstStressPeriodDataSet5WithoutParameters;
    procedure ReadNewStressPeriodDataSet5WithParameters;
    procedure ReadNewStressPeriodDataSet5WithoutParameters;
    procedure ReadParamNameForStressPeriod;
    procedure ReadInstanceNameForStressPeriod;
    procedure ReadDataSet1;
    procedure AssignObsGroupsToCells(CellList: TList);
  protected
    procedure AssignObservations; virtual;
    procedure AssignObservationFactors(ScreenObject: TScreenObject;
      const ParamName: string; Boundaries: TList);
    procedure FillListOfCellsForStressPeriod(StressPeriodIndex: Integer;
      CellList: TList);
    function ParameterType: TParameterType; virtual; abstract;
    procedure ReleaseMemory;
  public
    Constructor Create(Importer: TModflow2005Importer;
      const PackageIdentifier: string);
    Destructor Destroy; override;
  end;

  TFlowObservationLocation = class(TArrayMember)
  private
    LAYER: integer;
    ROW: integer;
    COLUMN: integer;
    FACTOR: double;
  end;

  TFlowObsLocArray = class(TObjectArray)
  private
    function GetItem(Index: integer): TFlowObservationLocation;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property Items[Index: integer]: TFlowObservationLocation
      read GetItem; default;
  end;

  TObservationTime = class(TArrayMember)
    OBSNAM: string;
    IREFSP: integer;
    TOFFSET: double;
    FLWOBS: double;
  end;

  TObsTimeArray = class(TObjectArray)
  private
    function GetItem(Index: integer): TObservationTime;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property Items[Index: integer]: TObservationTime
      read GetItem; default;
  end;

  TObservationGroup = class(TArrayMember)
  private
    FTimes: TObsTimeArray;
    FCells: TFlowObsLocArray;
    FGroup: TFluxObservationGroup;
    Constructor Create; override;
    Destructor Destroy; override;
  end;

  TObservations = class(TObjectArray)
  private
    function GetItem(Index: integer): TObservationGroup;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property Items[Index: integer]: TObservationGroup
      read GetItem; default;
  end;

  TCustomFlowObservationImporter = class(TPackageImporter)
  private
    NQ: integer;
    IUOBSV: integer;
    TOMULT: double;
    FObservations: TObservations;
    FCurrentGroupIndex: Integer;
    FCurrentGroup: TObservationGroup;
    FCurrentTimeIndex: Integer;
    FCurrentLocationIndex: Integer;
    FIsActive: Boolean;
    NQCL: integer;
    procedure ReadDataSet1;
    procedure ReadDataSet2;
    procedure ReadDataSet3;
    procedure ReadDataSet4;
    procedure ReadDataSet5;
    procedure CreateObservationGroups;
  protected
    FDataSet1Label: string;
    FDataSet2Label: string;
    FDataSet3Label: string;
    FDataSet4Label: string;
    FDataSet5Label: string;
    FObsPrefix: string;
    FFlowObsGroups: TFluxObservationGroups;
    procedure ReadData(const ALabel: string); override;
  public
    Constructor Create(Importer: TModflow2005Importer;
      const PackageIdentifier: string);
    Destructor Destroy; override;
  end;

  TChdLocationObject = class(TLocation)
    StartFactor: double;
    EndFactor: double;
  end;

  TCustomChdImporter = class(TListImporter)
  protected
    function GetBoundary(ScreenObject: TScreenObject): TModflowParamBoundary;
      override;
    function ParameterType: TParameterType; override;
    procedure SetItemValues(Item: TCustomModflowBoundaryItem;
      Boundaries: TList; EndTime: Double; StartTime: Double;
      ScreenObject: TScreenObject; const ParamName: string);override;
  end;

  TBasChdObjects = class(TObject)
  private
    FScreenObject: TScreenObject;
    FList: TList;
  public
    Constructor Create;
    Destructor Destroy; override;
  end;

  TBasImporter = class(TCustomChdImporter)
  strict private
    FHeading1: string;
    FHeading2: string;
    FChtoch: Boolean;
    FPrintTime: Boolean;
    FHNoFlo: double;
    FCrossSection: Boolean;
    FIbound: array of T2DIntArray;
    FConstantIbound : array of TIntegerConstantRecord;
    FStrt: array of T2DDoubleArray;
    FConstantStrt : TRealConstantRecordArray;
    FSpecifiedHeadsList: TList;
    procedure ImportInitialHead;
    procedure ImportActiveCells;
    procedure ReadHeading(var AHeading: string);
    procedure ReadDataSet1;
    procedure ReadVariableInitialHeadForLayer;
    procedure ReadVariableInitialHeadForCrossSection;
    procedure ReadConstantInitialHeadForLayer;
    procedure ReadConstantInitialHeadForCrossSection;
    procedure ReadVariableIboundForCrossSection;
    procedure ReadVariableIboundForLayer;
    procedure ReadConstantIboundForCrossSection;
    procedure ReadConstantIboundForLayer;
    procedure ReleaseMemory;
  private
    procedure ImportSpecifiedHeads;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
    function ScreenObjectNameRoot: string; override;
  public
    Constructor Create(Importer: TModflow2005Importer);
    Destructor Destroy; override;
  end;

  TChdImporter = class(TCustomChdImporter)
  private
    MXACTC: integer;
    FChdPackage: TModflowPackageSelection;
    FReadData: Boolean;
    function CreateScreenObject(List: TList;
      var ScreenObjectIndex: Integer; LayerIndex,
      StressPeriodIndex: integer): TScreenObject; override;
    procedure ReadDataSet2;
    procedure ReadParameterType;
    procedure ReadParameterLocations;
    procedure ReadNonParameterLocations;
    function ScreenObjectNameRoot: string; override;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer);
    Destructor Destroy; override;
  end;

  TChdObsImporter = class(TCustomFlowObservationImporter)
  private
    FChdImporter: TChdImporter;
    FBasImporter: TBasImporter;
  protected
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer;
      ChdImporter: TChdImporter; BasImporter: TBasImporter);
  end;


  TWelLocationObject = class(TLocation)
    PumpingRate: double;
  end;

  TWelInstanceObject = class(TListInstanceObject)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TWelParameterObject = class(TListParameterObject)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TWelParamArray = class(TListParamArray)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TWelStressPeriod = class(TListStressPeriod)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TWelStressPeriodArray = class(TListStressPeriodArray)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TWelImporter = class(TListImporter)
  private
    MXACTW: integer;
    IWELCB: integer;
    procedure ReadDataSet2;
    procedure ReadParameterType;
    procedure ReadParameterLocations;
    procedure ReadNonParameterLocations;
    function CreateScreenObject(List: TList;
      var ScreenObjectIndex: Integer; LayerIndex,
      StressPeriodIndex: integer): TScreenObject; override;
    procedure SetItemValues(Item: TCustomModflowBoundaryItem;
      Boundaries: TList; EndTime: Double; StartTime: Double;
      ScreenObject: TScreenObject; const ParamName: string); override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowParamBoundary;
      override;
    function ScreenObjectNameRoot: string; override;
    function ParameterType: TParameterType; override;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer);
    Destructor Destroy; override;
  end;

  TGhbLocationObject = class(TLocation)
    BHead: double;
    Conductance: double;
  end;

  TGhbInstanceObject = class(TListInstanceObject)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TGhbParameterObject = class(TListParameterObject)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TGhbParamArray = class(TListParamArray)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TGhbStressPeriod = class(TListStressPeriod)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TGhbStressPeriodArray = class(TListStressPeriodArray)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TGhbImporter = class(TListImporter)
  private
    MXACTB: integer;
    IGHBCB: integer;
    procedure ReadDataSet2;
    procedure ReadParameterType;
    procedure ReadParameterLocations;
    procedure ReadNonParameterLocations;
    function CreateScreenObject(List: TList;
      var ScreenObjectIndex: Integer; LayerIndex,
      StressPeriodIndex: integer): TScreenObject; override;
    procedure SetItemValues(Item: TCustomModflowBoundaryItem;
      Boundaries: TList; EndTime: Double; StartTime: Double;
      ScreenObject: TScreenObject; const ParamName: string); override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowParamBoundary;
      override;
    function ScreenObjectNameRoot: string; override;
    function ParameterType: TParameterType; override;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer);
    Destructor Destroy; override;
  end;

  TGhbObsImporter = class(TCustomFlowObservationImporter)
  private
    FGhbImporter: TGhbImporter;
  protected
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer;
      GhbImporter: TGhbImporter);
  end;

  TDrnLocationObject = class(TLocation)
    Elevation: double;
    Conductance: double;
  end;

  TDrnInstanceObject = class(TListInstanceObject)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TDrnParameterObject = class(TListParameterObject)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TDrnParamArray = class(TListParamArray)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TDrnStressPeriod = class(TListStressPeriod)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TDrnStressPeriodArray = class(TListStressPeriodArray)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TDrnObsImporter = class;

  TDrnImporter = class(TListImporter)
  private
    MXACTD: integer;
    IDRNCB: integer;
    procedure ReadDataSet2;
    procedure ReadParameterType;
    procedure ReadParameterLocations;
    procedure ReadNonParameterLocations;
    function CreateScreenObject(List: TList;
      var ScreenObjectIndex: Integer; LayerIndex,
      StressPeriodIndex: integer): TScreenObject; override;
    procedure SetItemValues(Item: TCustomModflowBoundaryItem;
      Boundaries: TList; EndTime: Double; StartTime: Double;
      ScreenObject: TScreenObject; const ParamName: string); override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowParamBoundary;
      override;
    function ScreenObjectNameRoot: string; override;
    function ParameterType: TParameterType; override;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer);
    Destructor Destroy; override;
  end;

  TDrnObsImporter = class(TCustomFlowObservationImporter)
  private
    FDrnImporter: TDrnImporter;
  protected
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer;
      DrnImporter: TDrnImporter);
  end;

  TDrtLocationObject = class(TDrnLocationObject)
    LayR: integer;
    RowR: integer;
    ColR: integer;
    Rfprop: double;
  end;

  TDrtInstanceObject = class(TListInstanceObject)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TDrtParameterObject = class(TListParameterObject)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TDrtParamArray = class(TListParamArray)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TDrtStressPeriod = class(TListStressPeriod)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TDrtStressPeriodArray = class(TListStressPeriodArray)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TDrtImporter = class(TListImporter)
  private
    ReturnFlow: boolean;
    FCurrentDrtBoundary: TDrtLocationObject;
    procedure ReadParameterType;
    function CreateScreenObject(List: TList;
      var ScreenObjectIndex: Integer; LayerIndex,
      StressPeriodIndex: integer): TScreenObject; override;
    procedure SetItemValues(Item: TCustomModflowBoundaryItem;
      Boundaries: TList; EndTime: Double; StartTime: Double;
      ScreenObject: TScreenObject; const ParamName: string); override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowParamBoundary;
      override;
    function ScreenObjectNameRoot: string; override;
    function ParameterType: TParameterType; override;
    procedure ReadDataSet1DRT;
    procedure ReadParameterLocationsWithoutReturnFlow;
    procedure ReadParameterLocationsWithReturnFlow;
    procedure ReadNonParameterLocationsWithoutReturnFlow;
    procedure ReadNonParameterLocationsWithReturnFlow;
    procedure ReadAuxilliaryVariables;
    procedure ImportNonParameterBoundaries(var ScreenObjectIndex: Integer);
    procedure ImportParameterBoundaries(ScreenObjectIndex: Integer);
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer);
    Destructor Destroy; override;
  end;

  TRivLocationObject = class(TLocation)
    Stage: double;
    Conductance: double;
    Bottom: double;
  end;

  TRivInstanceObject = class(TListInstanceObject)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TRivParameterObject = class(TListParameterObject)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TRivParamArray = class(TListParamArray)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TRivStressPeriod = class(TListStressPeriod)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TRivStressPeriodArray = class(TListStressPeriodArray)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TRivImporter = class(TListImporter)
  private
    MXACTR: integer;
    IRIVCB: integer;
    procedure ReadDataSet2;
    procedure ReadParameterType;
    procedure ReadParameterLocations;
    procedure ReadNonParameterLocations;
    function CreateScreenObject(List: TList;
      var ScreenObjectIndex: Integer; LayerIndex,
      StressPeriodIndex: integer): TScreenObject; override;
    procedure SetItemValues(Item: TCustomModflowBoundaryItem;
      Boundaries: TList; EndTime: Double; StartTime: Double;
      ScreenObject: TScreenObject; const ParamName: string); override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowParamBoundary;
      override;
    function ScreenObjectNameRoot: string; override;
    function ParameterType: TParameterType; override;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer);
    Destructor Destroy; override;
  end;

  TRivObsImporter = class(TCustomFlowObservationImporter)
  private
    FRivImporter: TRivImporter;
  protected
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer;
      RivImporter: TRivImporter);
  end;


  TPointList = class(TObject)
  strict private
    FPoints: array of TPoint2D;
    FCount: integer;
    procedure Grow;
    function GetCapacity: integer;
    function GetPoint(Index: integer): TPoint2D;
    procedure SetCapacity(const Value: integer);
    procedure SetPoint(Index: integer; const Value: TPoint2D);
  public
    function Add(Point: TPoint2D): integer;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read FCount;
    property Points[Index: integer]: TPoint2D read GetPoint write SetPoint;
    procedure Insert(Position: integer; Point: TPoint2D);
  end;

  TTransientArrayImporter = class(TPackageImporter)
  private
    NP: integer;
    CurrentParameter: Integer;
    CurrentInstance: integer;
    ZoneCount: integer;
    FCurrentStressPeriod: integer;
    FParams: TArrayParamArray;
    NCLU: integer;
    CurrentCluster: Integer;
    FStressPeriods: TArrayStressPeriodArray;
    FZoneImporter: TMultZoneImporter;
    FQuadTree: TRbwQuadTree;
    FPointLists: TList;
    FEpsilon: Real;
    FRequiredType: string;
    FReuseLayerIndicator: array of boolean;
    FConstantLayerIndicators: TIntegerConstantArray;
    FVariableLayerIndicators: T3DIntArray;
    procedure ReadNumberOfParameters;
    procedure ReadParameterName;
    procedure ReadParameterType(RequiredType: string);
    procedure ReadParameterValue;
    procedure ReadNumberOfClusters;
    procedure ReadNumberOfInstances;
    procedure ReadInstanceName;
    procedure ReadMultiplierAndZoneArrayNames;
    procedure ReadNumberOfZones;
    procedure ReadZoneValues;
    procedure ReadParameterForStressPeriod;
    procedure ReadInstanceForStressPeriod;
    procedure InitializeStressPeriods;
    procedure ImportSharedData(const ALabel: string; out Handled: boolean);
    procedure InitializeCurrentStressPeriod(ReadParamArray: Integer);
    procedure InitializeReuseLayerIndicator;
    procedure ReadRealConstantArrayItem(Value: Double;
      var RealConstantArray: TRealConstantRecordArray);
    procedure ReadRealVariableArray(var VariableArray: T3DDoubleArray);
    procedure ReadConstantIntArray(IntValue: Integer;
      var ConstantIntArray: TIntegerConstantArray);
    procedure ReadVariableIntArray(var VariableIntArray: T3DIntArray);
    procedure EvaluateTimeVaryingLayers(
      Package: TCustomTransientLayerPackageSelection);
    procedure CreateAssignedLayerDataSet(
      Package: TCustomTransientLayerPackageSelection; DataSetRoot: string;
      ScreenObjectName: string; var AssignedLayerDataSet: TDataArray);
    procedure CreateDataSet(StressPeriodIndex: Integer; const Root: string;
      DataType: TRbwDataType; var DataSet: TDataArray);
    function GetStressPeriodString(StressPeriodIndex: Integer): string;
    procedure CreateTransientParam(Param: TArrayParameterObject);
    function FindSimilarClusterAndAddScreenObject(ACluster: TClusterObject;
      ClusterList, ScreenObjectList: TList): boolean;
    function CreateScreenObjectAroundGrid(const Name: string): TScreenObject;
    procedure CreateScreenObjectFromCluster(ClusterList,
      ScreenObjectList: TList; var ObjectIndex: Integer;
      Cluster: TClusterObject; const ScreenObjectRoot,
      ImportedElevFormula: string;
      Package: TCustomTransientLayerPackageSelection);
    procedure CreateScreenObjectsFromClusters(Param: TArrayParameterObject;
      var ObjectIndex: Integer; ScreenObjectList, ClusterList: TList;
      const ScreenObjectRoot, ImportedElevFormula: string;
      Package: TCustomTransientLayerPackageSelection);
    Function CreateScreenObjectAroundZones(ZoneArray: T2DIntArray;
      Cluster: TClusterObject; const Name: string): TScreenObject;
    procedure InitializeQuadTree;
    procedure ImportSegments(Sender: TObject; const Segments: TLine2DArray);
    procedure CreateTimeVaryingAssignedLayerDataSet(StressPeriodIndex: Integer;
      const DataSetRoot, ScreenObjectRoot: string;
      Package: TCustomTransientLayerPackageSelection);
    procedure GetParamInstanceForCurrentStressPeriod(
      var Instance: TArrayInstanceObject;
      Param: TArrayParameterObject; StressPeriod: TArrayStressPeriod);
    procedure CreateBoundary(ScreenObject: TScreenObject); virtual; abstract;
    procedure CreateScreenObjectsAroundValues(Values: T2DDoubleArray;
      const Root: string; DataArray: TDataArray;
      ValueList: TRealList); overload;
    procedure CreateScreenObjectsAroundValues(Values: T2DIntArray;
      const Root: string; DataArray: TDataArray;
      ValueList: TIntegerList); overload;
  public
    Constructor Create(Importer: TModflow2005Importer;
      const PackageIdentifier: string; ZoneImporter: TMultZoneImporter);
    Destructor Destroy; override;
  end;

  TRchImporter = class(TTransientArrayImporter)
  private
    NRCHOP: integer;
    FConstantRecharge: TRealConstantRecordArray;
    FVariableRecharge: T3DDoubleArray;
    FReuseRecharge: array of boolean;
    FRchPackage: TRchPackageSelection;
    procedure InitializeReuseRecharge;
    procedure SetRechargeOption;
    procedure ReuseRchStressPeriodWithParameters(StressPeriodIndex: Integer;
      Param: TArrayParameterObject; ScreenObjectList: TList;
      const RechargeLayerName: string);
    procedure AssignTimeVaryingLayer(ScreenObject: TScreenObject;
      RechargeLayerName: string; StressPeriodIndex: Integer);
    procedure AssignParamRechargeRate(ScreenObject: TScreenObject;
      StressPeriodIndex: Integer; Cluster: TClusterObject;
      Param: TArrayParameterObject);
    procedure CreateRechargeRateDataSet(StressPeriodIndex: Integer);
    procedure AssignRchLayerNonParam(NewItemNeeded: Boolean;
      var LayerItem: TRchLayerItem; RchBoundary: TRchBoundary;
      RechargeLayerName: string; StressPeriodIndex: Integer);
    procedure AssignRechRateNonParam(RechargeName: string;
      NewItemNeeded: Boolean; var RchItem: TRchItem; RchBoundary: TRchBoundary;
      StressPeriodIndex: Integer);
    procedure CreateBoundary(ScreenObject: TScreenObject); override;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer;
      ZoneImporter: TMultZoneImporter);
  end;

  TCustomETImporter = class(TTransientArrayImporter)
  protected
    FReuseEtSurface: array of boolean;
    FReuseEtExtinctionDepth: array of boolean;
    FReuseEtRate: array of boolean;
    FConstantEtSurface: TRealConstantRecordArray;
    FVariableEtSurface: T3DDoubleArray;
    FConstantEtRate: TRealConstantRecordArray;
    FVariableEtRate: T3DDoubleArray;
    FConstantExtinctionDepth: TRealConstantRecordArray;
    FVariableExtinctionDepth: T3DDoubleArray;
    Package: TCustomTransientLayerPackageSelection;
    procedure InitializeReuseEtSurface;
    procedure InitializeReuseExtinctionDepth;
    procedure InitializeReuseEtRate;
    class function ImportedEtSurfaceName: string; virtual; abstract;
    class function ImportedEtSurfaceSP: string; virtual; abstract;
    class function EtExtinctionDepth_Name: string; virtual; abstract;
    class function EtExtinctionDepth_SP: string; virtual; abstract;
    procedure CreateEtSurfaceDataSet(StressPeriodIndex: Integer);
    procedure CreateEtExtinctionDepthDataSet(StressPeriodIndex: Integer);
    function GetBoundary(ScreenObject: TScreenObject): TModflowParamBoundary;
      virtual; abstract;
    procedure ReuseEtStressPeriodWithParameters(StressPeriodIndex: Integer;
      Param: TArrayParameterObject; ScreenObjectList: TList;
      const EvtLayerName, EvtSurfaceName, EvtDepthName: string);
    procedure AssignTimeVaryingLayer(ScreenObject: TScreenObject;
      EvtLayerName: string; StressPeriodIndex: Integer); virtual; abstract;
    procedure AssignSurfaceAndDepth(ScreenObject: TScreenObject;
      EvtSurfaceName, EvtDepthName: string; StressPeriodIndex: Integer);
      virtual; abstract;
    procedure AssignParamEvtRate(ScreenObject: TScreenObject;
      StressPeriodIndex: Integer; Cluster: TClusterObject;
      Param: TArrayParameterObject);
    class function ImportedEtRateName: string; virtual; abstract;
    class function ImportedEtValuesName: string; virtual; abstract;
    procedure CreateEvtRateDataSet(StressPeriodIndex: Integer);
    procedure AssignEtRateNonParam(EvtName: string;
      NewItemNeeded: Boolean; var EvtItem: TEvtItem;
      EvtBoundary: TModflowParamBoundary; StressPeriodIndex: Integer);
  end;

  TEvtImporter = class(TCustomETImporter)
  private
    NEVTOP: integer;
    FEvtPackage: TEvtPackageSelection;
    procedure SetEvtOption;
    procedure AssignEvtLayerNonParam(NewItemNeeded: Boolean;
      var LayerItem: TEvtLayerItem; EvtBoundary: TEvtBoundary;
      EvtLayerName: string; StressPeriodIndex: Integer);
    procedure AssignEvtSurfaceNonParam(EvtSurfaceName, EvtExtinctName: string;
      NewItemNeeded: Boolean; var EvtItem: TEvtSurfDepthItem;
      EvtBoundary: TEvtBoundary; StressPeriodIndex: Integer);
    procedure AssignSurfaceAndDepth(ScreenObject: TScreenObject;
      EvtSurfaceName, EvtDepthName: string; StressPeriodIndex: Integer); override;
    procedure CreateBoundary(ScreenObject: TScreenObject); override;
    class function ImportedEtSurfaceName: string; override;
    class function ImportedEtSurfaceSP: string; override;
    class function EtExtinctionDepth_Name: string; override;
    class function EtExtinctionDepth_SP: string; override;
    function GetBoundary(ScreenObject: TScreenObject):
      TModflowParamBoundary; override;
    procedure AssignTimeVaryingLayer(ScreenObject: TScreenObject;
      EvtLayerName: string; StressPeriodIndex: Integer); override;
    class function ImportedEtRateName: string; override;
    class function ImportedEtValuesName: string; override;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer;
      ZoneImporter: TMultZoneImporter);
  end;

  TEtsImporter = class(TCustomETImporter)
  private
    FCurrentSegment: Integer;
    FReuseSegmentDefinition: array of boolean;
    FConstantDepthProportions: array of TRealConstantRecordArray;
    FVariableDepthProportions: array of T3DDoubleArray;
    FConstantRateProportions: array of TRealConstantRecordArray;
    FVariableRateProportions: array of T3DDoubleArray;
    FEtsPackage: TEtsPackageSelection;
    NETSOP: integer;
    NETSEG: integer;
    procedure InitializeReuseSegmentDefinition;
    procedure CreateBoundary(ScreenObject: TScreenObject); override;
    procedure SetEtsOption;
    class function ImportedEtSurfaceName: string; override;
    class function ImportedEtSurfaceSP: string; override;
    class function EtExtinctionDepth_Name: string; override;
    class function EtExtinctionDepth_SP: string; override;
    function GetBoundary(ScreenObject: TScreenObject):
      TModflowParamBoundary; override;
    procedure AssignTimeVaryingLayer(ScreenObject: TScreenObject;
      EvtLayerName: string; StressPeriodIndex: Integer); override;
    procedure AssignSurfaceAndDepth(ScreenObject: TScreenObject;
      EvtSurfaceName, EvtDepthName: string;
      StressPeriodIndex: Integer); override;
    class function ImportedEtRateName: string; override;
    class function ImportedEtValuesName: string; override;
    procedure AssignEtsLayerNonParam(NewItemNeeded: Boolean;
      var LayerItem: TEvtLayerItem; EvtBoundary: TEtsBoundary;
      EvtLayerName: string; StressPeriodIndex: Integer);
    procedure AssignEtsSurfaceNonParam(EvtSurfaceName, EvtExtinctName: string;
      NewItemNeeded: Boolean; var EvtItem: TEtsSurfDepthItem;
      EvtBoundary: TEtsBoundary; StressPeriodIndex: Integer);
    function SegIndexToStr(SegIndex: integer): string;
    procedure CreateDepthFractionDataSet(SegmentIndex,
      StressPeriodIndex: Integer);
    procedure CreateRateFractionDataSet(SegmentIndex,
      StressPeriodIndex: Integer);
    procedure AssignSegments(LayerItem: TEtsSurfDepthItem;
      StressPeriodIndex: Integer);
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer;
      ZoneImporter: TMultZoneImporter);
  end;

  TSfrLocationObject = class(TLocation)
    SegmentNumber: integer;
    ReachNumber: integer;
    RCHLEN: double;
    STRTOP: double;
    SLOPE: double;
    STRTHICK: double;
    STRHC1: double;
    THTS: double;
    THTI: double;
    EPS: double;
    UHC: double;
  end;

  TFlowTableItem = class(TArrayMember)
    FLOWTAB: double;
    DPTHTAB: double;
    WDTHTAB: double;
  end;

  TFlowTableArray = class(TObjectArray)
  private
    function GetItem(Index: integer): TFlowTableItem;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property Items[Index: integer]: TFlowTableItem read GetItem;  default;
  end;

  TSegment = class(TArrayMember)
    NSEG: integer;
    ICALC: integer;
    OUTSEG: integer;
    IUPSEG: integer;
    IPRIOR: integer;
    NSTRPTS: integer;
    FLOW: double;
    RUNOFF: double;
    ETSW: double;
    PPTSW: double;
    ROUGHCH: double;
    ROUGHBK: double;
    CDPTH: double;
    FDPTH: double;
    AWDTH: double;
    BWDTH: double;
    HCOND1: double;
    THICKM1: double;
    ELEVUP: double;
    WIDTH1: double;
    DEPTH1: double;
    THTS1: double;
    THTI1: double;
    EPS1: double;
    UHC1: double;
    HCOND2: double;
    THICKM2: double;
    ELEVDN: double;
    WIDTH2: double;
    DEPTH2: double;
    THTS2: double;
    THTI2: double;
    EPS2: double;
    UHC2: double;
    TableX: array[0..7] of double;
    TableZ: array[0..7] of double;
    FFlowTable: TFlowTableArray;
    Constructor Create; override;
    Destructor Destroy; override;
  end;

  TSegmentArray = class(TListStressPeriod)
  private
    function GetSegments(Index: integer): TSegment;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property Segments[Index: integer]: TSegment read GetSegments;  default;
  end;

  TStressPeriodSegments = class(TObjectArray)
  private
    function GetSegmentArray(Index: integer): TSegmentArray;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property SegmentArrays[Index: integer]: TSegmentArray
      read GetSegmentArray;  default;
  end;

  TSfrInstanceObject = class(TListInstanceObject)
  private
    FSegments: TSegmentArray;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property Segments: TSegmentArray read FSegments;
    Constructor Create; override;
    Destructor Destroy; override;
    function GetSegByNumber(NSEG: integer): TSegment;
  end;

  TSfrParameterObject = class(TListParameterObject)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TSfrParamArray = class(TListParamArray)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TSfReaches = class(TObjectArray)
  private
    function GetReach(Index: integer): TSfrLocationObject;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  published
  public
    property Reaches[Index: integer]: TSfrLocationObject read GetReach; default;
  end;

  TSfrStressPeriodArray = class(TListStressPeriodArray)
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  TSfrImporter = class(TListImporter)
  private
    NSTRM: integer;
    DLEAK: double;
    ISFROPT: Integer;
    ISUZN: integer;
    NSFRSETS: integer;
    NSS: integer;
    ISTCB1: integer;
    ISTCB2: integer;
    NSTRAIL: integer;
    NPARSEG: integer;
    SfrCONST: double;
    FCurrentReachIndex: integer;
    FReaches: TSfReaches;
    FCurrentReach: TSfrLocationObject;
    FCurrentSegments: TSegmentArray;
    FCurrentSegmentIndex: integer;
    FCurrentSegment: TSegment;
    FSfrPackage: TSfrPackageSelection;
    FSfrBoundary: TSfrBoundary;
    FStressPeriodSegments: TStressPeriodSegments;
    SegCount: integer;
    FParamUsed: Boolean;
    IRTFLG: integer;
    NUMTIM: integer;
    WEIGHT: double;
    FLWTOL: double;
    procedure ReadBasicData;
    procedure ReadIsfropt;
    procedure ReadUnsatParameters;
    procedure ReadParameterType;
    procedure ReadSegmentStart(IsParameter: boolean);
    procedure ReadSegment4b6a(const ALabel: string; IsParameter: boolean);
    procedure ReadSegment4c6b(const ALabel: string);
    procedure ReadSegment4d6c(const ALabel: string);
    procedure ReadTableXValues;
    procedure ReadTableZValues;
    procedure ReadFlowTableFlows;
    procedure ReadFlowTableDepths;
    procedure ReadFlowTableWidths;
    procedure ReadFirstDataSet5WithoutParameters;
    procedure ReadFirstDataSet5WithParameters;
    procedure ReadDataSet5WithoutParameters;
    procedure ReadDataSet5WithParameters;
    procedure ReadParameterValueAndLocationCount; override;
    procedure ReadNumberOfInstances; override;
    function CreateStream(List: TList;
      var ScreenObjectIndex: integer): TScreenObject;
    procedure AssignReachValues(List: TList; ScreenObject: TScreenObject);
    procedure AssignStartAndEndTimes;
    procedure InitializeCurrentStressPeriod;
    procedure AssignParamIcalcValues(IcalcIndex: Integer; Segment: TSegment);
    procedure AssignFlowTableValues(IcalcIndex: Integer; Segment: TSegment);
    procedure AssignSegmentFlowValues(IcalcIndex: Integer; Segment: TSegment);
    procedure AssignChannelValues(IcalcIndex: Integer;
      StressPeriodIndex: Integer; Segment: TSegment);
    procedure AssignEquationValues(IcalcIndex: Integer; Segment: TSegment);
    procedure AssignUpstreamValues(IcalcIndex: Integer;
      StressPeriodIndex: Integer; Segment: TSegment);
    procedure AssignDownstreamValues(Segment: TSegment;
      StressPeriodIndex: Integer; IcalcIndex: Integer);
    procedure AssignUnsatValues(Segment: TSegment; StressPeriodIndex: Integer;
      IcalcIndex: Integer);
    procedure SetItemValues(Item: TCustomModflowBoundaryItem;
      Boundaries: TList; EndTime: Double; StartTime: Double;
      ScreenObject: TScreenObject; const ParamName: string); override;
    procedure AssignSegmentProperties;
    procedure CreateParamInstances(ParameterSegments: TIntegerList);
    function DefaultInstanceName(StressPeriodIndex: integer): string;
    procedure AssignParameterSegmentProperties(PriorSegNumber: Integer);
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowParamBoundary;
      override;
    function ScreenObjectNameRoot: string; override;
    function ParameterType: TParameterType; override;
  public
    Constructor Create(Importer: TModflow2005Importer);
    Destructor Destroy; override;
  end;

  TMultLayerFactor = class(TArrayMember)
    Layer: integer;
    Proportion: double;
  end;

  TMultiLayerArray = class(TObjectArray)
  private
    function GetMultLayerFactor(Index: integer): TMultLayerFactor;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property MultLayerFactors[Index: integer]: TMultLayerFactor
      read GetMultLayerFactor; default;
  end;

  TMultiTimeObs = class(TArrayMember)
    Name: string;
    RefStressPeriod: integer;
    TimeOffset: double;
    HeadObservation: double;
  end;

  TMultiTimeObsArray = class(TObjectArray)
  private
    function GetMultiTimeObs(Index: integer): TMultiTimeObs;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property MultTimeObs[Index: integer]: TMultiTimeObs
      read GetMultiTimeObs; default;
  end;

  THeadObservation = class(TArrayMember)
    OBSNAM: string;
    Layer: integer;
    Row: integer;
    Column: integer;
    RowOffset: double;
    ColumnOffset: double;
    ITT: integer;
    FMultiLayers: TMultiLayerArray;
    FObsTimes: TMultiTimeObsArray;
    Constructor Create; override;
    Destructor Destroy; override;
  end;

  THeadObsevationArray = class(TObjectArray)
  private
    function GetHeadObservation(Index: integer): THeadObservation;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property HeadObservations[Index: integer]: THeadObservation
      read GetHeadObservation; default;
  end;

  THobImporter = class(TPackageImporter)
  private
    HOBDRY: double;
    TOMULTH: double;
    FHeadObservations: THeadObsevationArray;
    FHobPackage: THobPackageSelection;
    FCurrentObsIndex: Integer;
    FCurrentTimeIndex: integer;
    FCurrentObs: THeadObservation;
    FCurrentObsTime: TMultiTimeObs;
    procedure ReadDataSet1;
    procedure ReadDataSet2;
    procedure ReadDataSet3ObsName;
    procedure ReadDataSet3Observation;
    procedure ReadDataSet4;
    procedure ReadDataSet5;
    procedure ReadDataSet6ObsName;
    procedure ReadDataSet6Values;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer);
    Destructor Destroy; override;
  end;

  THfbLocation = class(TLocation)
    Row2: integer;
    Col2: integer;
    Factor: double;
  end;

  THfbLocations = class(TListInstanceObject)
  private
    function GetBarriers(Index: integer): THfbLocation;
  protected
    // @name returns @link(THfbLocation);
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property Barriers[Index: integer]: THfbLocation read GetBarriers; default;
  end;

  THfbParam = class(TListParameterObject)
  protected
    // @name returns @link(THfbLocations);
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  THfbParamArray = class(TListParamArray)
  protected
    // @name returns @link(THfbParam);
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  THfbStressPeriod = class(TListStressPeriod)
  private
    function GetBarrierList(Index: integer): THfbLocations;
  protected
    // @name returns @link(THfbLocations);
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property BarrierList[Index: integer]: THfbLocations
      read GetBarrierList; default;
  end;

  THfbStressPeriodArray = class(TListStressPeriodArray)
  protected
    // @name returns @link(THfbStressPeriod);
    function ArrayMemberClass: TArrayMemberClass; override;
  end;

  THfbImporter = class(TListImporter)
  private
    NoPrintOption: boolean;
    NACTHFB: integer;
    ParameterNames: array of string;
    FParam: THfbParam;
    FCurrentLocation: integer;
    FLocations: THfbLocations;
    NHFBNP: integer;
    FHfbPackage: TModflowPackageSelection;
    procedure ReadParameterType;
    procedure DefineBarrier(Location: THfbLocation;
      const ParamName: string; var Count: integer);
    function ScreenObjectNameRoot: string; override;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
    procedure SetItemValues(Item: TCustomModflowBoundaryItem;
      Boundaries: TList; EndTime: Double; StartTime: Double;
      ScreenObject: TScreenObject; const ParamName: string); override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowParamBoundary;
      override;
    function ParameterType: TParameterType; override;
  public
    Constructor Create(Importer: TModflow2005Importer);
    Destructor Destroy; override;
  end;

  TLakeValues = class(TArrayMember)
    SSMN: double;
    SSMX: double;
    PRCPLK: double;
    EVAPLK: double;
    RNF: double;
    WTHDRW: double;
  end;

  TLakeValueArray = class(TObjectArray)
  private
    function GetLakeValues(Index: integer): TLakeValues;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    Reuse: boolean;
    property LakeValues[Index: integer]: TLakeValues
      read GetLakeValues; default;
  end;

  TLakeStressPeriodValues = class(TObjectArray)
  private
    function GetLakeValueArray(Index: integer): TLakeValueArray;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property LakeValueArray[Index: integer]: TLakeValueArray
      read GetLakeValueArray; default;
  end;

  TLakImporter = class(TTransientArrayImporter)
  private
    NLAKES: integer;
    THETA: double;
    NSSITR: integer;
    SSCNCR: double;
    SURFDEPTH: double;
    Stages: array of double;
    MinStages: array of double;
    MaxStages: array of double;
    NSOL: integer;
    LKARR: array of T2DIntArray;
    FLakeOutline: T2DIntArray;
    FLakeBottom: T2DDoubleArray;
    BDLKNC: array of T2DDoubleArray;
    FConstantLkarr : TIntegerConstantArray;
    FConstantBdlknc : TRealConstantRecordArray;
    NSLMS: integer;
    IC: integer;
    FLakPackage: TLakePackageSelection;
    FCurrentLakeDefinition: integer;
    SubLakes: array of array of integer;
    Sills: array of array of double;
    FLakeStressPeriodValues: TLakeStressPeriodValues;
    FCurrentLakeValues: Integer;
    FCurrentStressPeriodValues: TLakeValueArray;
    FPriorStressPeriodValues: TLakeValueArray;
    procedure ReadDataSet9bNoAugmentation;
    procedure ReadDataSet9bWithAugmentation;
    procedure ReadDataSet9aNoLimits;
    procedure ReadDataSet9aWithLimits;
    procedure ReadSills;
    procedure ReadSublakes;
    procedure ReadNumberOfSublakes;
    procedure ReadConstantLakebedLeakance;
    procedure ReadVariableLakebedLeakance;
    procedure ReadConstantLakeID;
    procedure ReadVariableLakeID;
    procedure ReadDataSet4;
    procedure ReadLakeSolutes;
    procedure ReadNumberOfSolutes;
    procedure ReadLakeStage;
    procedure ReadLakeStageAndLimits;
    procedure ReadTransientControls;
    procedure ReadTheta;
    procedure ReadDataSet1;
    procedure DefineLakeOutlines;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
    procedure CreateBoundary(ScreenObject: TScreenObject); override;
  public
    Constructor Create(Importer: TModflow2005Importer);
    Destructor Destroy; override;
  end;

  TResStage = class(TArrayMember)
    Ststage: double;
    Endstage: double;
  end;

  TResStressPeriod = class(TObjectArray)
  private
    function GetStages(Index: integer): TResStage;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property Stages[Index: integer]: TResStage read GetStages; default;
  end;

  TResStressPeriods = class(TObjectArray)
  private
    function GetStressPeriods(Index: integer): TResStressPeriod;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property StressPeriods[Index: integer]: TResStressPeriod
      read GetStressPeriods; default;
  end;

  TResImporter = class(TTransientArrayImporter)
  private
    NRES: integer;
    NRESOP: integer;
    NPTS: integer;
    ConstantLandSurface: Boolean;
    ConstantVertK: Boolean;
    ConstantBedThick: Boolean;
    LandSurface: T2DDoubleArray;
    VertK: T2DDoubleArray;
    BedThickness: T2DDoubleArray;
    ConstantResLocation: Boolean;
    ResLocation: T2DIntArray;
    ConstantResLayer: Boolean;
    ResLayer: T2DIntArray;
    FStressPeriods: TResStressPeriods;
    FResPackage: TResPackageSelection;
    IRESPT: integer;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
    procedure CreateBoundary(ScreenObject: TScreenObject); override;
  public
    Constructor Create(Importer: TModflow2005Importer);
    Destructor Destroy; override;
  end;

  TUzfGage = class(TArrayMember)
    IUZROW: integer;
    IUZCOL: integer;
    IFTUNIT: integer;
    IUZOPT: integer;
  end;

  TUzfGageArray = class(TObjectArray)
  private
    function GetGage(Index: integer): TUzfGage;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property Gages[Index: integer]: TUzfGage read GetGage; default;
  end;

  TUzfImporter = class(TTransientArrayImporter)
  private
    FEtStressPeriods: TArrayStressPeriodArray;
    FEtExtinctDepthStressPeriods: TArrayStressPeriodArray;
    FEtExtinctWaterContentStressPeriods: TArrayStressPeriodArray;
    NUZTOP: integer;
    IUZFOPT: integer;
    IRUNFLG: integer;
    IETFLG: integer;
    NTRAIL2: integer;
    NSETS2: integer;
    NUZGAG: integer;
    SURFDEP: double;
    FGages: TUzfGageArray;
    FCurrentGage: integer;
    FConstantInfiltration: TRealConstantRecordArray;
    FVariableInfiltration: T3DDoubleArray;
    FConstantET: TRealConstantRecordArray;
    FVariableET: T3DDoubleArray;
    FConstantExtinctDepth: TRealConstantRecordArray;
    FVariableExtinctDepth: T3DDoubleArray;
    FConstantExtinctWaterContent: TRealConstantRecordArray;
    FVariableExtinctWaterContent: T3DDoubleArray;
    IsConstIUZFBND: boolean;
    ConstIUZFBND: integer;
    IUZFBND: T2DIntArray;
    IsConstIRUNBND: boolean;
    ConstIRUNBND: integer;
    IRUNBND: T2DIntArray;
    IsConstVks: boolean;
    ConstVks: double;
    VKS: T2DDoubleArray;
    IsConstEps: boolean;
    ConstEps: double;
    EPS: T2DDoubleArray;
    IsConstThts: boolean;
    ConstThts: double;
    THTS: T2DDoubleArray;
    IsConstThti: boolean;
    ConstThti: double;
    THTI: T2DDoubleArray;
    FUzfPackage: TUzfPackageSelection;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
    procedure CreateBoundary(ScreenObject: TScreenObject); override;
  public
    Constructor Create(Importer: TModflow2005Importer);
    Destructor Destroy; override;
  end;

  TGageType = (gtLake, gtStream);

  TGage = class(TArrayMember)
    GageType: TGageType;
    LakeNumber: integer;
    OUTTYPE: integer;
    GAGESEG: integer;
    GAGERCH: integer;
  end;

  TGageArray = class(TObjectArray)
  private
    function GetGage(Index: integer): TGage;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property Gages[Index: integer]: TGage read GetGage; default;
  end;

  TGageImporter = class(TPackageImporter)
  private
    FGages: TGageArray;
    FCurrentGage: integer;
    FLakImporter: TLakImporter;
    FSfrImporter: TSfrImporter;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
    Constructor Create(Importer: TModflow2005Importer;
      LakImporter: TLakImporter; SfrImporter: TSfrImporter);
    Destructor Destroy; override;
  end;

  TPvalParam = class(TArrayMember)
    PARNAM: string;
    Value: double;
  end;

  TPvalParamArray = class(TObjectArray)
  private
    function GetParam(Index: integer): TPvalParam;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property Params[Index: integer]: TPvalParam read GetParam; default;
  end;

  TPvalImporter = class(TPackageImporter)
  private
    FPvalParams: TPvalParamArray;
    FCurrentParam: integer;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer);
    Destructor Destroy; override;
  end;

  TMnwiItem = class(TArrayMember)
    WellID: string;
    QNDflag: double;
    QBHflag: double;
  end;

  TMnwiArray = class(TObjectArray)
  private
    function GetItem(Index: integer): TMnwiItem;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    function GetWellByName(const AWellId: string): TMnwiItem;
    property Items[Index: integer]: TMnwiItem read GetItem; default;
  end;

  TMnw2Importer = class;

  TMnwiImporter = class(TPackageImporter)
  private
    FMnwiArray: TMnwiArray;
    Wel1flag: integer;
    QSUMflag: integer;
    BYNDflag: integer;
    FCurrentWell: integer;
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    function GetWellByName(const AWellId: string): TMnwiItem;
    Constructor Create(Importer: TModflow2005Importer;
      MnwImporter: TMnw2Importer);
    Destructor Destroy; override;
  end;

  TMnw2PumpTableItem = class(TArrayMember)
    LIFTn: double;
    Qn: double;
  end;

  TMnw2PumpTableArray = class(TObjectArray)
  private
    function GetItem(Index: integer): TMnw2PumpTableItem;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property Items[Index: integer]: TMnw2PumpTableItem read GetItem; default;
  end;

  TMnw2WellItem = class(TArrayMember)
  public
    Rw: double;
    Rskin: double;
    Kskin: double;
    B: double;
    C: double;
    P: double;
    CWC: double;
    IR: integer;
    IC: integer;
  end;

  TMnw2WellCell = class(TMnw2WellItem)
  public
    IL: integer;
    PP: double;
  end;

  TMnw2CellArray = class(TObjectArray)
  private
    function GetCell(Index: integer): TMnw2WellCell;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property Cells[Index: integer]: TMnw2WellCell read GetCell; default;
  end;

  TMnw2WellScreen = class(TMnw2WellItem)
  public
    Ztop: double;
    Zbotm: double;
  end;

  TMnw2ScreenArray = class(TObjectArray)
  private
    function GetScreen(Index: integer): TMnw2WellScreen;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property Screens[Index: integer]: TMnw2WellScreen read GetScreen; default;
  end;

  TWellState = (wsInactive, wsActive, wsReuse);

  TMnw2WellStressPeriod = class(TMnw2WellItem)
  public
    WellState: TWellState;
    Qdes: double;
    CapMult: double;
    Hlim: double;
    QCUT: integer;
    Qfrcmn: double;
    Qfrcmx: double;
  end;

  TMnw2StressPeriodArray = class(TObjectArray)
  private
    function GetStressPeriod(Index: integer): TMnw2WellStressPeriod;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property StressPeriods[Index: integer]: TMnw2WellStressPeriod
      read GetStressPeriod; default;
  end;

  TMnw2Well = class(TArrayMember)
  private
    FNNodes: integer;
    FCells: TMnw2CellArray;
    FScreens: TMnw2ScreenArray;
    FCurrentCell: integer;
    FCurrentScreen: integer;
    FPumpTable : TMnw2PumpTableArray;
    FPUMPCAP: integer;
    FCurrentPumpItem: integer;
    FStressPeriods: TMnw2StressPeriodArray;
    procedure SetNNodes(const Value: integer);
    procedure SetRw(const Value: double);
    procedure SetKskin(const Value: double);
    procedure SetRskin(const Value: double);
    procedure SetB(const Value: double);
    procedure SetC(const Value: double);
    procedure SetP(const Value: double);
    procedure SetCWC(const Value: double);
    procedure SetPUMPCAP(const Value: integer);
  public
    WellId: string;
    LossType: TMnwLossType;
    PUMPLOC: integer;
    Qlimit: integer;
    PPFLAG: integer;
    PUMPLAY: integer;
    PUMPROW: integer;
    PUMPCOL: integer;
    Zpump: double;
    Hlim: double;
    QCUT: integer;
    Qfrcmn: double;
    Qfrcmx: double;
    Hlift: double;
    LIFTq0: double;
    // LIFTqmax
    LIFTqdes: double;
    HWtol: double;
    Constructor Create; override;
    Destructor Destroy; override;
    property PUMPCAP: integer read FPUMPCAP write SetPUMPCAP;
    property NNodes: integer read FNNodes write SetNNodes;
    property Rw: double write SetRw;
    property Rskin: double write SetRskin;
    property Kskin: double write SetKskin;
    property B: double write SetB;
    property C: double write SetC;
    property P: double write SetP;
    property CWC: double write SetCWC;
  end;

  TMnw2WellArray = class(TObjectArray)
  private
    FCurrentWell: TMnw2Well;
    function GetWell(Index: integer): TMnw2Well;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    function GetWellByName(const AWellId: string): TMnw2Well;
    property Wells[Index: integer]: TMnw2Well read GetWell; default;
  end;

  TMnw2Importer = class(TPackageImporter)
  private
    MNWPRNT: integer;
    FWells: TMnw2WellArray;
    FCurrentWell: Integer;
    FCurrentStressPeriod: integer;
    FMnw2Package: TMultinodeWellSelection;
    FMnwiImporter: TMnwiImporter;
    procedure ReadDataSet1;
    procedure ReadAuxiliary;
    procedure ReadWellId;
    procedure ReadNumberOfNodes;
    procedure ReadLossType;
    procedure ReadFlags;
    procedure ReadThiemData;
    procedure ReadSkinData;
    procedure ReadEquationData;
    procedure ReadSpecifyCwcData;
    procedure ReadACell(const ALabel: string);
    procedure ReadAWellScreen(const ALabel: string);
    procedure ReadPumpCell;
    procedure ReadPumpZ;
    procedure ReadFullPumpLimits;
    procedure ReadPartialPumpLimits;
    procedure ReadLiftTableLimits;
    procedure ReadLiftTableItem;
    procedure ReadItmp;
    procedure ReadWellForCurrentStressPeriod;
    procedure AssignCells(AScreenObject: TScreenObject; AWell: TMnw2Well);
    procedure AssignScreens(AScreenObject: TScreenObject; AWell: TMnw2Well);
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Procedure AddStressPeriod(ITMP: integer);
    Constructor Create(Importer: TModflow2005Importer);
    Destructor Destroy; override;
  end;

  TSubLayerAssignment = class(TArrayMember)
    Layer: integer;
  end;

  TSubLayerAssignments = class(TObjectArray)
  private
    function GetLayer(Index: integer): TSubLayerAssignment;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property Layers[Index: integer]: TSubLayerAssignment
      read GetLayer; default;
  end;

  TMaterialZone = class(TArrayMember)
    VerticalHydraulicConductivity: double;
    ElasticSpecificStorage: double;
    InelasticSpecificStorage: double;
  end;

  TMaterialZones = class(TObjectArray)
  private
    function GetZone(Index: integer): TMaterialZone;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property Zone[Index: integer]: TMaterialZone read GetZone; default;
  end;

  TSubOC = class(TArrayMember)
    ISP1: integer;
    ISP2: integer;
    ITS1: integer;
    ITS2: integer;
    Ifl: array [0..12] of integer;
  end;

  TSubOutputs = class(TObjectArray)
  private
    function GetOC(Index: integer): TSubOC;
  protected
    function ArrayMemberClass: TArrayMemberClass; override;
  public
    property OC[Index: integer]: TSubOC read GetOC; default;
  end;

  TSubImporter = class(TArrayImporter)
  private
    NN: integer;
    ITMIN: integer;
    AC1: double;
    AC2: double;
    IDSAVE: integer;
    IDREST: integer;
    FDelayBeds: TSubLayerAssignments;
    FNoDelayBeds: TSubLayerAssignments;
    FRnbIndex: integer;
    FNoDelayIndex: integer;
    FDelayIndex: integer;
    FConstRNB: TRealConstantRecordArray;
    FRNB: T3DDoubleArray;
    FConstHC: TRealConstantRecordArray;
    FHC: T3DDoubleArray;
    FConstSfe: TRealConstantRecordArray;
    FSfe: T3DDoubleArray;
    FConstSfv: TRealConstantRecordArray;
    FSfv: T3DDoubleArray;
    FConstCom: TRealConstantRecordArray;
    FCom: T3DDoubleArray;
    FConstDstart: TRealConstantRecordArray;
    FDstart: T3DDoubleArray;
    FConstDHC: TRealConstantRecordArray;
    FDHC: T3DDoubleArray;
    FConstDCOM: TRealConstantRecordArray;
    FDCOM: T3DDoubleArray;
    FConstDZ: TRealConstantRecordArray;
    FDZ: T3DDoubleArray;
    FConstNZ: TIntegerConstantArray;
    FNZ: T3DIntArray;
    FMaterialZones: TMaterialZones;
    Ifm1: integer;
    Ifm2: integer;
    Ifm3: integer;
    Ifm4: integer;
    Ifm5: integer;
    Ifm6: integer;
    FSubOutputs: TSubOutputs;
    FNamImporter: TNamImporter;
    procedure ReadDataSet1;
    procedure ReadDataSet2;
    procedure ReadDataSet3;
    procedure ReadConstRealArrayForLayer;
    procedure ReadVariableRealArrayForLayer;
    procedure ReadDataSet9;
    procedure ReadConstIntArrayForLayer;
    procedure ReadVariableIntArrayForLayer;
    procedure ReadDataSet15;
    procedure ImportDataSet1(SubPackage: TSubPackageSelection);
    procedure ImportPrintFormat(SubPackage: TSubPackageSelection);
    procedure ImportPrintChoices(SubPackage: TSubPackageSelection);
    procedure ImportPreconsolidatonHead(Index: Integer;
      NoDelayItem: TSubNoDelayBedLayerItem; var ScreenObject: TScreenObject);
    procedure ImportSfe(Index: Integer; NoDelayItem: TSubNoDelayBedLayerItem;
      var ScreenObject: TScreenObject);
    procedure ImportSfv(Index: Integer; NoDelayItem: TSubNoDelayBedLayerItem;
      var ScreenObject: TScreenObject);
    procedure ImportCom(Index: Integer; NoDelayItem: TSubNoDelayBedLayerItem;
      var ScreenObject: TScreenObject);
    procedure ImportRNB(DelayItem: TSubDelayBedLayerItem; Index: Integer;
      var ScreenObject: TScreenObject);
    procedure ImportDStart(DelayItem: TSubDelayBedLayerItem; Index: Integer;
      var ScreenObject: TScreenObject);
    procedure ImportDHC(DelayItem: TSubDelayBedLayerItem; Index: Integer;
      var ScreenObject: TScreenObject);
    procedure ImportDCOM(DelayItem: TSubDelayBedLayerItem; Index: Integer;
      var ScreenObject: TScreenObject);
    procedure ImportDZ(DelayItem: TSubDelayBedLayerItem; Index: Integer;
      var ScreenObject: TScreenObject);
    procedure ImportMaterialZone(DelayItem: TSubDelayBedLayerItem;
      Index: Integer; var ScreenObject: TScreenObject);
  protected
    procedure ReadData(const ALabel: string); override;
    procedure HandlePackage; override;
  public
    Constructor Create(Importer: TModflow2005Importer;
      NamImporter: TNamImporter);
    Destructor Destroy; override;
  end;

var
  GlobalCellCenterScreenObject: TScreenObject = nil;

procedure ImportModflow2005(const ListFileName: string;
  XOrigin, YOrigin, GridAngle: double; textHandler: TTextHandler);
var
  Importer: TModflow2005Importer;
begin
  Assert(Assigned(textHandler));
  frmGoPhast.CanDraw:= False;
  Importer := TModflow2005Importer.Create(ListFileName,
    XOrigin, YOrigin, GridAngle);
  try
    Importer.textHandler := textHandler;
    Importer.ImportModel;
  finally
    Importer.Free;
    frmGoPhast.CanDraw := True;
  end;
end;

{ TModflow2005Importer }

constructor TModflow2005Importer.Create(const ListFileName: string;
  XOrigin, YOrigin, GridAngle: double);
var
  MZImporter: TMultZoneImporter;
  LakImporter: TLakImporter;
  SfrImporter: TSfrImporter;
  HufImporter: THufImporter;
  Drn: TDrnImporter;
  GHB: TGhbImporter;
  Riv: TRivImporter;
  Bas: TBasImporter;
  Chd: TChdImporter;
  Mnw2: TMnw2Importer;
  Nam: TNamImporter;
begin
  inherited Create;
  FPointsComputed := False;
  Assert(FileExists(ListFileName));
  FListFileName := ListFileName;

  FPackageIdentifiers:= TStringList.Create;
  Nam := TNamImporter.Create(self);
  FPackageIdentifiers.AddObject('NAM:', Nam);
  FPackageIdentifiers.AddObject('DIS:', TDisImporter.Create(self,
    XOrigin, YOrigin, GridAngle));
  Bas := TBasImporter.Create(self);
  FPackageIdentifiers.AddObject('BAS:', Bas);

  MZImporter := TMultZoneImporter.Create(self);
  
  FPackageIdentifiers.AddObject('ZONE_MULT:', MZImporter);
  FPvalImporter := TPvalImporter.Create(self);
  FPackageIdentifiers.AddObject('PVAL:', FPvalImporter);
  FPackageIdentifiers.AddObject('BCF:', TBcfImporter.Create(self));
  FPackageIdentifiers.AddObject('LPF:', TLpfImporter.Create(self));
  HufImporter := THufImporter.Create(self);
  FPackageIdentifiers.AddObject('HUF2:', HufImporter);
  FPackageIdentifiers.AddObject('KDEP:',
    TKdepImporter.Create(self, HufImporter));
  FPackageIdentifiers.AddObject('LVDA:',
    TLvdaImporter.Create(self, HufImporter));
  FPackageIdentifiers.AddObject('HFB:', THfbImporter.Create(self));
  FPackageIdentifiers.AddObject('UZF:', TUzfImporter.Create(self));
  Chd := TChdImporter.Create(self);
  FPackageIdentifiers.AddObject('CHD:', Chd);
  FPackageIdentifiers.AddObject('FHB:', nil);
  FPackageIdentifiers.AddObject('RCH:', TRchImporter.Create(self, MZImporter));
  FPackageIdentifiers.AddObject('WEL:', TWelImporter.Create(self));
  Drn := TDrnImporter.Create(self);
  FPackageIdentifiers.AddObject('DRN:', Drn);
  FPackageIdentifiers.AddObject('DRT:', TDrtImporter.Create(self));
  FPackageIdentifiers.AddObject('ETS:', TEtsImporter.Create(self, MZImporter));
  FPackageIdentifiers.AddObject('EVT:', TEvtImporter.Create(self, MZImporter));
  GHB := TGhbImporter.Create(self);
  FPackageIdentifiers.AddObject('GHB:', GHB);
  LakImporter:= TLakImporter.Create(self);
  FPackageIdentifiers.AddObject('LAK:', LakImporter);
  FPackageIdentifiers.AddObject('MNW:', nil);
  Mnw2 := TMnw2Importer.Create(self);
  FPackageIdentifiers.AddObject('MNW2:', Mnw2);
  FPackageIdentifiers.AddObject('MNWI:', TMnwiImporter.Create(self, Mnw2));
  FPackageIdentifiers.AddObject('RES:', TResImporter.Create(self));
  Riv := TRivImporter.Create(self);
  FPackageIdentifiers.AddObject('RIV:', Riv);
  SfrImporter := TSfrImporter.Create(self);
  FPackageIdentifiers.AddObject('SFR:', SfrImporter);
  FPackageIdentifiers.AddObject('STR:', nil);
  FPackageIdentifiers.AddObject('DE4:', TDe4Importer.Create(self));
  FPackageIdentifiers.AddObject('GMG:', TGmgImporter.Create(self));
  FPackageIdentifiers.AddObject('SIP:', TSipImporter.Create(self));
  FPackageIdentifiers.AddObject('PCG:', TPcgImporter.Create(self));
  FGageImporter := TGageImporter.Create(self, LakImporter, SfrImporter);
  FPackageIdentifiers.AddObject('GAG:', FGageImporter);
  FPackageIdentifiers.AddObject('OC:', nil);
  FPackageIdentifiers.AddObject('IBS:', nil);
  FPackageIdentifiers.AddObject('SUB:', TSubImporter.Create(self, Nam));
  FPackageIdentifiers.AddObject('SWT:', nil);
  FPackageIdentifiers.AddObject('HOB:', THobImporter.Create(self));
  FChobImporter := TChdObsImporter.Create(self, Chd, Bas);
  FPackageIdentifiers.AddObject('CHOB:', FChobImporter);
  FPackageIdentifiers.AddObject('DROB:', TDrnObsImporter.Create(self, Drn));
  FPackageIdentifiers.AddObject('GBOB:', TGhbObsImporter.Create(self, GHB));
  FPackageIdentifiers.AddObject('RVOB:', TRivObsImporter.Create(self, Riv));
  FPackageIdentifiers.AddObject('LMT6:', nil);
  FPackageIdentifiers.AddObject('HYD:', nil);
end;

destructor TModflow2005Importer.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to FPackageIdentifiers.Count - 1 do
  begin
    FPackageIdentifiers.Objects[Index].Free;
  end;
  FPackageIdentifiers.Free;
  inherited;
end;

function TModflow2005Importer.GetCenterPoints: TSurfacePointArray;
var
  Grid: TModflowGrid;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  if not FPointsComputed then
  begin
    Grid := frmGoPhast.PhastModel.ModflowGrid;
    SetLength(FCenterPoints, Grid.RowCount, Grid.ColumnCount);
    for RowIndex := 0 to Grid.RowCount - 1 do
    begin
      for ColIndex := 0 to Grid.ColumnCount - 1 do
      begin
        FCenterPoints[RowIndex,ColIndex] :=
          Grid.TwoDElementCenter(ColIndex,RowIndex);
      end;
    end;
    FPointsComputed := True;
  end;
  result := FCenterPoints;
end;

procedure TModflow2005Importer.ImportModel;
var
  ALine: string;
  PackageIndex: integer;
  PackageImporter: TPackageImporter;
  KPER: integer;
  StressPeriodString: string;
begin
  StressPeriodString := '';
  Assert(FileExists(FListFileName));
  AssignFile(FFile,FListFileName);
  try
    Reset(FFile);
    While not Eof(FFile) do
    begin
      ReadLn(FFile, ALine);
      ALine := Trim(ALine);
      if ALine = 'DIS:' then
      begin
        texthandler('Converting DIS input file');
      end
      else if ALine = 'BAS:' then
      begin
        texthandler('Converting BAS input file');
      end
      else if ALine = 'LPF:' then
      begin
        texthandler('Converting LPF input file');
      end
      else if ALine = 'HUF:' then
      begin
        texthandler('Converting HUF2 input file');
      end
      else if ALine = 'BCF:' then
      begin
        texthandler('Converting BCF input file');
      end
      else if ALine = 'OC:' then
      begin
        texthandler('Converting OC input file');
      end;

      if ALine = 'KPER:' then
      begin
        ReadLn(FFile, KPER);
        texthandler('Converting Stress Period ' + IntToStr(KPER));
        StressPeriodString := ' Stress Period ' + IntToStr(KPER);
      end
      else
      begin
        if (Length(ALine) > 0) and (ALine[Length(ALine)] = ':') then
        begin
          PackageIndex := FPackageIdentifiers.IndexOf(ALine);
          if PackageIndex >= 0 then
          begin
            PackageImporter := FPackageIdentifiers.Objects[PackageIndex]
              as TPackageImporter;
            while PackageImporter <> nil do
            begin
              texthandler('Converting '
                + FPackageIdentifiers[PackageIndex] + StressPeriodString);
              PackageImporter.ImportPackage(PackageIndex);
              if PackageIndex >= 0 then
              begin
                texthandler('Converting '
                  + FPackageIdentifiers[PackageIndex] + StressPeriodString);
                PackageImporter := FPackageIdentifiers.Objects[PackageIndex]
                  as TPackageImporter;
              end
              else
              begin
                PackageImporter := nil;
              end;
            end;
          end;
        end;
      end;
    end;
    FChobImporter.HandlePackage;
    FGageImporter.HandlePackage;
    FPvalImporter.HandlePackage;
  finally
    CloseFile(FFile);
    GlobalCellCenterScreenObject := nil;
  end;
  frmGoPhast.PhastModel.FormulaManager.Pack;
end;

{ TPackageImporter }

constructor TPackageImporter.Create(Importer: TModflow2005Importer;
  const PackageIdentifier: string);
begin
  FImporter := Importer;
  FPackageIdentifier := PackageIdentifier;
  FComments:= TStringList.Create;
  FModel := frmGoPhast.PhastModel;
  FGrid := FModel.ModflowGrid;
end;

destructor TPackageImporter.Destroy;
begin
  FComments.Free;
  inherited;
end;

function TPackageImporter.FixArrayName(const ArrayName: string): string;
var
  Index: Integer;
begin
  result := ArrayName;
  if (Length(result) > 0) then
  begin
    if not (result[1] in ['A'..'Z', 'a'..'z', '_']) then
    begin
      result := '_' + result;
    end;
    for Index := 2 to Length(result) do
    begin
      if not (result[Index] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
      begin
        result[Index] := '_';
      end;
    end;
  end;
end;

procedure TPackageImporter.HandlePackage;
var
  ID: string;
begin
  ID := FPackageIdentifier;
  Assert(Length(ID) > 0);
  if ID[Length(ID)] = ':' then
  begin
    ID := Copy(ID, 1, Length(ID)-1);
  end;
  FImporter.TextHandler('Importing ' + ID + ' Package');
  FImportedPackage := True;
end;

procedure TPackageImporter.ReadData(const ALabel: string);
begin

end;

procedure TPackageImporter.ImportPackage(out NextPackageIndex: integer);
var
  ALine: string;
  KPER: integer;
  FileName: string;
begin
  While not Eof(FImporter.FFile) do
  begin
    ReadLn(FImporter.FFile, ALine);
    ALine := Trim(ALine);
    if ALine = FPackageIdentifier then
    begin
      Continue;
    end;
    if (Length(ALine) > 0) and (ALine[Length(ALine)] = ':') then
    begin
      NextPackageIndex := FImporter.FPackageIdentifiers.IndexOf(ALine);
      if NextPackageIndex >= 0 then
      begin
        HandlePackage;
        Exit;
      end;
      if ALine = 'COMMENT:' then
      begin
        ReadLn(FImporter.FFile, ALine);
        ALine := Trim(ALine);
        if (Length(ALine) > 0) and (ALine[1] = '#') then
        begin
          ALine := Trim(Copy(ALine, 2, MAXINT));
        end;
        if (Length(ALine) > 0) then
        begin
          FComments.Add(ALine);
        end;
      end
      else if ALine = 'KPER:' then
      begin
        ReadLn(FImporter.FFile, KPER);
        FImporter.texthandler('Converting Stress Period ' + IntToStr(KPER));
      end
      else if Pos('OPENING FILE ON UNIT', ALine) = 1 then
      begin
        ReadLn(FImporter.FFile, FileName);
        FileName := Trim(FileName);
      end
      else
      begin
        ReadData(ALine);
      end;
    end;
  end;
  NextPackageIndex := -1;
  if not ImportedPackage then
  begin
    HandlePackage;
  end;
  Exit;
end;

procedure TPackageImporter.Read1DRealArray(var DoubleArray: TOneDRealArray);
var
  Index: Integer;
begin
  for Index := 0 to Length(DoubleArray) - 1 do
  begin
    Read(FImporter.FFile,DoubleArray[Index]);
  end;
  ReadLn(FImporter.FFile);
end;

procedure TPackageImporter.Read2DRealArray(var DoubleArray: T2DDoubleArray);
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  for RowIndex := 0 to Length(DoubleArray) - 1 do
  begin
    for ColIndex := 0 to Length(DoubleArray[RowIndex]) - 1 do
    begin
      Read(FImporter.FFile,DoubleArray[RowIndex, ColIndex]);
    end;
    Readln(FImporter.FFile);
  end;
end;

{ TDisImporter }

constructor TDisImporter.Create(Importer: TModflow2005Importer;
  XOrigin, YOrigin, GridAngle: double);
begin
  inherited Create(Importer, 'DIS:');
  FOriginPoint.x := XOrigin;
  FOriginPoint.y := YOrigin;
  FGridAngle := GridAngle;
end;

procedure TDisImporter.InitializeView;
var
  YWidth: Real;
  MaxZ: Real;
  RowIndex: Integer;
  ColIndex: Integer;
  MaxLayer: Integer;
  MinZ: Real;
  ZHeight: Real;
  XWidth: Real;
begin
  XWidth := FGrid.ColumnPosition[FGrid.ColumnCount] - FGrid.ColumnPosition[0];
  YWidth := FGrid.RowPosition[0] - FGrid.RowPosition[FGrid.RowCount];

  if FConstantElevations[0].IsConstant then
  begin
    MaxZ := FConstantElevations[0].RealValue;
  end
  else
  begin
    MaxZ := FElevations[0, 0, 0];
    for RowIndex := 0 to Length(FElevations[0]) - 1 do
    begin
      for ColIndex := 0 to Length(FElevations[0, 0]) - 1 do
      begin
        if MaxZ < FElevations[0, RowIndex, ColIndex] then
        begin
          MaxZ := FElevations[0, RowIndex, ColIndex];
        end;
      end;
    end;
  end;

  MaxLayer := Length(FElevations) - 1;
  if FConstantElevations[MaxLayer].IsConstant then
  begin
    MinZ := FConstantElevations[MaxLayer].RealValue;
  end
  else
  begin
    MinZ := FElevations[MaxLayer, 0, 0];
    for RowIndex := 0 to Length(FElevations[0]) - 1 do
    begin
      for ColIndex := 0 to Length(FElevations[0, 0]) - 1 do
      begin
        if MinZ > FElevations[MaxLayer, RowIndex, ColIndex] then
        begin
          MinZ := FElevations[MaxLayer, RowIndex, ColIndex];
        end;
      end;
    end;
  end;

  ZHeight := MaxZ - MinZ;
  frmGoPhast.PhastModel.Exaggeration := frmGoPhast.DefaultVE;
  frmGoPhast.InitializeView(XWidth, YWidth, ZHeight);
end;

procedure TDisImporter.ReadStressPeriod;
var
  N: Integer;
begin
  Read(FImporter.FFile, N);
  Read(FImporter.FFile, FStressPeriods[N - 1].PERLEN);
  Read(FImporter.FFile, FStressPeriods[N - 1].NSTP);
  Read(FImporter.FFile, FStressPeriods[N - 1].TSMULT);
  Read(FImporter.FFile, FStressPeriods[N - 1].ISSFLG);
  ReadLn(FImporter.FFile);
end;

procedure TDisImporter.ImportRowsAndColumns;
var
  RowPositions: TOneDRealArray;
  Index: Integer;
  ColumnPositions: TOneDRealArray;
  Grid: TModflowGrid;
  RotatedOrigin: TPoint2D;
begin
  Grid := frmGoPhast.PhastModel.ModflowGrid;
  RotatedOrigin := Grid.
    RotateFromRealWorldCoordinatesToGridCoordinates(FOriginPoint);

  SetLength(ColumnPositions, NCOL + 1);
  ColumnPositions[0] := RotatedOrigin.x;
  for Index := 0 to NCOL - 1 do
  begin
    ColumnPositions[Index + 1] := ColumnPositions[Index] + DELR[Index];
  end;
  Grid.ColumnPositions := ColumnPositions;
  SetLength(RowPositions, NROW + 1);
  RowPositions[0] := RotatedOrigin.Y;
  for Index := 0 to NROW - 1 do
  begin
    RowPositions[Index + 1] := RowPositions[Index] - DELC[Index];
  end;
  Grid.RowPositions := RowPositions;
end;

procedure TDisImporter.ImportElevations;
var
  GroupIndex: Integer;
  NewLayerStructure: TLayerStructure;
  ScreenObject: TScreenObject;
  DataArray: TDataArray;
  DataArrayName: string;
  LayerIndex: Integer;
begin
  ScreenObject := nil;
  NewLayerStructure := TLayerStructure.Create(nil);
  try
    NewLayerStructure.Assign(FModel.LayerStructure);
    for GroupIndex := 1 to NewLayerStructure.Count - 1 do
    begin
      NewLayerStructure.LayerGroups[GroupIndex].AquiferName :=
        'Layer ' + IntToStr(GroupIndex);
    end;
    while NewLayerStructure.Count > Length(FElevations) do
    begin
      NewLayerStructure.Delete(NewLayerStructure.Count - 1);
    end;
    while NewLayerStructure.Count < Length(FElevations) do
    begin
      NewLayerStructure.Add;
    end;
    NewLayerStructure.LayerGroups[0].AquiferName := StrModelTop;
    GroupIndex := 1;
    for LayerIndex := 0 to Length(LAYCBD) - 1 do
    begin
      NewLayerStructure.LayerGroups[GroupIndex].Simulated := True;
      NewLayerStructure.LayerGroups[GroupIndex].AquiferName :=
        'Layer ' + IntToStr(LayerIndex + 1);
      if LAYCBD[LayerIndex] <> 0 then
      begin
        Inc(GroupIndex);
        NewLayerStructure.LayerGroups[GroupIndex].Simulated := False;
        NewLayerStructure.LayerGroups[GroupIndex].AquiferName :=
          'Confining Bed ' + IntToStr(LayerIndex + 1);
      end;
      Inc(GroupIndex);
    end;
    FModel.LayerStructure.Assign(NewLayerStructure);
  finally
    NewLayerStructure.Free;
  end;
  for GroupIndex := 0 to FModel.LayerStructure.Count - 1 do
  begin
    DataArrayName := FModel.LayerStructure.LayerGroups[GroupIndex].
      DataArrayName;
    DataArray := FModel.GetDataSetByName(DataArrayName);
    Assert(DataArray <> nil);
    if FConstantElevations[GroupIndex].IsConstant then
    begin
      DataArray.Formula := FloatToStr(FConstantElevations[GroupIndex].
        RealValue);
    end
    else
    begin
      if ScreenObject = nil then
      begin
        CreateOrRetrieveCellCenterScreenObject(ScreenObject);
      end;
      AssignRealValuesToCellCenters(DataArray, ScreenObject,
        FElevations[GroupIndex]);
    end;
  end;
  FGrid.UpdateCellElevations;
end;

procedure TDisImporter.ImportStressPeriods;
var
  Delta: Double;
  NewPeriod: TModflowStressPeriod;
  ImportedSP: TImportedStressPeriod;
  StressPeriodIndex: Integer;
  StartTime: Double;
  Changed: Boolean;
begin
  FModel.ModflowStressPeriods.Clear;
  StartTime := 0;
  for StressPeriodIndex := 0 to Length(FStressPeriods) - 1 do
  begin
    ImportedSP := FStressPeriods[StressPeriodIndex];
    NewPeriod := FModel.ModflowStressPeriods.Add as TModflowStressPeriod;
    NewPeriod.StartTime := StartTime;
    NewPeriod.PeriodLength := ImportedSP.PERLEN;
    if (StressPeriodIndex = 0) and (ImportedSP.PERLEN = 0) then
    begin
      NewPeriod.StartTime := -1;
      NewPeriod.PeriodLength := 1;
    end;
    StartTime := StartTime + ImportedSP.PERLEN;
    NewPeriod.EndTime := StartTime;
    NewPeriod.TimeStepMultiplier := ImportedSP.TSMULT;
    if ImportedSP.ISSFLG = 0 then
    begin
      NewPeriod.StressPeriodType := sptTransient;
    end
    else
    begin
      NewPeriod.StressPeriodType := sptSteadyState;
    end;
    if ImportedSP.NSTP > 1 then
    begin
      if ImportedSP.TSMULT = 1 then
      begin
        NewPeriod.MaxLengthOfFirstTimeStep :=
          ImportedSP.PERLEN / ImportedSP.NSTP;
      end
      else
      begin
        NewPeriod.MaxLengthOfFirstTimeStep :=
          ImportedSP.PERLEN * (ImportedSP.TSMULT - 1)
          / (IntPower(ImportedSP.TSMULT, ImportedSP.NSTP) - 1);
      end;
    end
    else
    begin
      NewPeriod.MaxLengthOfFirstTimeStep := NewPeriod.PeriodLength;
    end;
    Changed := True;
    Delta := 0.01;
    while Changed do
    begin
      Changed := False;
      Delta := Delta / 10;
      while (NewPeriod.NumberOfSteps > ImportedSP.NSTP) do
      begin
        NewPeriod.MaxLengthOfFirstTimeStep :=
          NewPeriod.MaxLengthOfFirstTimeStep * (1 + Delta);
        Changed := True;
      end;
      while (NewPeriod.NumberOfSteps < ImportedSP.NSTP) do
      begin
        NewPeriod.MaxLengthOfFirstTimeStep :=
          NewPeriod.MaxLengthOfFirstTimeStep * (1 - Delta);
        Changed := True;
      end;
    end;
  end;
end;

procedure TDisImporter.ReadVariableBottomElev;
var
  LayerIndex: Integer;
  ALine: string;
  Layer: Integer;
begin
  ReadLn(FImporter.FFile, ALine);
  ALine := Trim(ALine);
  ReadLn(FImporter.FFile, Layer);
  if ALine = 'MODEL LAYER BOTTOM EL.' then
  begin
    LayerIndex := GetLayerBottomIndex(Layer);
  end
  else if ALine = 'BOT. EL. OF QUASI-3D BED' then
  begin
    LayerIndex := GetConfiningBedBottomIndex(Layer);
  end
  else
  begin
    LayerIndex := -1;
    Assert(False);
  end;
  Read2DRealArray(FElevations[LayerIndex]);
end;

procedure TDisImporter.ReadVariableTopElev;
var
  ALine: string;
begin
  ReadLn(FImporter.FFile, ALine);
  ALine := Trim(ALine);
  if ALine = 'TOP ELEVATION OF LAYER 1' then
  begin
    Read2DRealArray(FElevations[0]);
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TDisImporter.ReleaseMemory;
begin
  SetLength(DELR, 0);
  SetLength(DELC, 0);
  SetLength(FElevations, 0, 0, 0);
  SetLength(FConstantElevations, 0);
  SetLength(FStressPeriods, 0);
end;

procedure TDisImporter.ReadConstantBottomElev;
var
  LayerIndex: Integer;
  ALine: string;
  Value: Double;
  Layer: Integer;
begin
  ReadLn(FImporter.FFile, ALine);
  ALine := Trim(ALine);
  ReadLn(FImporter.FFile, Layer);
  Readln(FImporter.FFile, Value);
  if ALine = 'MODEL LAYER BOTTOM EL.' then
  begin
    LayerIndex := GetLayerBottomIndex(Layer);
  end
  else if ALine = 'BOT. EL. OF QUASI-3D BED' then
  begin
    LayerIndex := GetConfiningBedBottomIndex(Layer);
  end
  else
  begin
    LayerIndex := -1;
    Assert(False);
  end;
  FConstantElevations[LayerIndex].IsConstant := True;
  FConstantElevations[LayerIndex].RealValue := Value;
end;

procedure TDisImporter.ReadConstantTopElev;
var
  Value: Double;
  ALine: string;
begin
  ReadLn(FImporter.FFile, ALine);
  ALine := Trim(ALine);
  Readln(FImporter.FFile, Value);
  if ALine = 'TOP ELEVATION OF LAYER 1' then
  begin
    FConstantElevations[0].IsConstant := True;
    FConstantElevations[0].RealValue := Value;
  end
  else
  begin
    Assert(False);
  end;
end;

function TDisImporter.GetConfiningBedBottomIndex(Layer: Integer): integer;
var
  Index: Integer;
begin
  result := Layer;
  for Index := 0 to Layer - 1 do
  begin
    if LAYCBD[Index] <> 0 then
    begin
      Inc(result);
    end;
  end;
end;

function TDisImporter.GetLayerBottomIndex(Layer: Integer): integer;
var
  Index: Integer;
begin
  result := Layer;
  for Index := 0 to Layer - 2 do
  begin
    if LAYCBD[Index] <> 0 then
    begin
      Inc(result);
    end;
  end;
end;

procedure TDisImporter.HandlePackage;
begin
  inherited;
  FModel.ModelSelection := msModflow;
  FModel.CreateInitialDataSets;

  FModel.ModflowOptions.Description.AddStrings(FComments);

  FGrid.GridAngle := FGridAngle;

  FModel.ModflowOptions.LengthUnit := LENUNI;
  FModel.ModflowOptions.TimeUnit := ITMUNI;

  ImportRowsAndColumns;
  ImportElevations;
  ImportStressPeriods;
  InitializeView;
  ReleaseMemory;
end;

procedure TDisImporter.ReadVariableDelrDelc;
var
  ALine: string;
begin
  ReadLn(FImporter.FFile, ALine);
  ALine := Trim(ALine);
  if ALine = 'DELR' then
  begin
    Read1DRealArray(DELR);
  end
  else if ALine = 'DELC' then
  begin
    Read1DRealArray(DELC);
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TDisImporter.ReadConstantDelrDelc;
var
  Value: Double;
  ALine: string;
begin
  ReadLn(FImporter.FFile, ALine);
  Read(FImporter.FFile, Value);
  if Trim(ALine) = 'DELR' then
  begin
    AssignConstant1DArray(DELR, Value);
  end
  else if Trim(ALine) = 'DELC' then
  begin
    AssignConstant1DArray(DELC, Value);
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TDisImporter.ReadDataSet2;
var
  Index: Integer;
  NBOTM: integer;
begin
  Assert(Length(LAYCBD) = NLAY);
  for Index := 0 to NLAY - 1 do
  begin
    Read(FImporter.FFile, LAYCBD[Index]);
  end;
  NBOTM := NLAY + 1;
  for Index := 0 to NLAY - 1 do
  begin
    if LAYCBD[Index] <> 0 then
    begin
      Inc(NBOTM);
    end;
  end;
  SetLength(FElevations, NBOTM, NROW, NCOL);
  SetLength(FConstantElevations, NBOTM);
  for Index := 0 to NBOTM - 1 do
  begin
    FConstantElevations[Index].IsConstant := False;
  end;
end;

procedure TDisImporter.ReadDataSet1;
begin
  Read(FImporter.FFile, NLAY);
  Read(FImporter.FFile, NROW);
  Read(FImporter.FFile, NCOL);
  Read(FImporter.FFile, NPER);
  Read(FImporter.FFile, ITMUNI);
  Read(FImporter.FFile, LENUNI);
  ReadLn(FImporter.FFile);
  SetLength(LAYCBD, NLAY);
  SetLength(DELR, NCOL);
  SetLength(DELC, NROW);
  SetLength(FStressPeriods, NPER);
end;

procedure TDisImporter.ReadData(const ALabel: string);
begin
  inherited;
  if ALabel = 'NLAY, NROW, NCOL, NPER, ITMUNI, LENUNI:' then
  begin
    ReadDataSet1;
  end
  else if ALabel = 'LAYCBD(K):' then
  begin
    ReadDataSet2;
  end
  else if ALabel = StrConstant1DRealArray then
  begin
    ReadConstantDelrDelc;
  end
  else if ALabel = StrVariable1DRealArray then
  begin
    ReadVariableDelrDelc;
  end
  else if ALabel = StrConstant2DRealArray then
  begin
    ReadConstantTopElev;
  end
  else if ALabel = StrConstant2DRealArrayForLayer then
  begin
    ReadConstantBottomElev;
  end
  else if ALabel = StrVariable2DRealArray then
  begin
    ReadVariableTopElev;
  end
  else if ALabel = StrVariable2DRealArrayForLayer then
  begin
    ReadVariableBottomElev;
  end
  else if ALabel = StrVariable2DRealArrayForCrossSection then
  begin
    Assert(False);
  end
  else if ALabel = 'N,PERLEN(N),NSTP(N),TSMULT(N),ISSFLG(N):' then
  begin
    ReadStressPeriod;
  end
  else
  begin
    Assert(False);
  end
end;

procedure TPackageImporter.AssignConstant2DArray(Value: Double;
  Array2D: T2DDoubleArray);
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  for RowIndex := 0 to Length(Array2D)-1 do
  begin
    for ColIndex := 0 to Length(Array2D[RowIndex]) - 1 do
    begin
      Array2D[RowIndex, ColIndex] := Value;
    end;
  end;
end;

procedure TPackageImporter.AssignConstant2DIntArray(Value: integer;
  Array2D: T2DIntArray);
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  for RowIndex := 0 to Length(Array2D)-1 do
  begin
    for ColIndex := 0 to Length(Array2D[RowIndex]) - 1 do
    begin
      Array2D[RowIndex, ColIndex] := Value;
    end;
  end;
end;

procedure TPackageImporter.AssignImportedValues(ImportedValues: TValueArrayItem;
  ImportedData: TDoubleArray);
var
  Index: Integer;
begin
  ImportedValues.Values.DataType := rdtDouble;
  ImportedValues.Values.Count := Length(ImportedData);;
  for Index := 0 to Length(ImportedData) - 1 do
  begin
    ImportedValues.Values.RealValues[Index] :=
      ImportedData[Index];
  end;
end;

procedure TPackageImporter.AssignIntegerValuesToCellCenters(
  DataArray: TDataArray; ScreenObject: TScreenObject;
  ImportedData: T2DIntArray);
var
  PointIndex: Integer;
  ImportedValues: TValueArrayItem;
  DataSetIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Interpolator: TNearestPoint2DInterpolator;
begin
  Assert(DataArray.Orientation = dsoTop);
  if DataArray.TwoDInterpolator = nil then
  begin
    Interpolator := TNearestPoint2DInterpolator.Create(nil);
    try
      DataArray.TwoDInterpolator := Interpolator;
    finally
      Interpolator.Free;
    end;
  end;
  DataSetIndex := ScreenObject.AddDataSet(DataArray);
  ScreenObject.DataSetFormulas[DataSetIndex] := rsObjectImportedValuesI;
  ScreenObject.ImportedValues.Add;
  ImportedValues := ScreenObject.ImportedValues.Items[
    ScreenObject.ImportedValues.Count-1];
  ImportedValues.Values.DataType := rdtInteger;
  ImportedValues.Values.Count := FGrid.RowCount * FGrid.ColumnCount;
  ImportedValues.Name := DataArray.Name;
  PointIndex := 0;
  for RowIndex := 0 to FGrid.RowCount - 1 do
  begin
    for ColIndex := 0 to FGrid.ColumnCount - 1 do
    begin
      ImportedValues.Values.IntValues[PointIndex] :=
        ImportedData[RowIndex, ColIndex];
      Inc(PointIndex);
    end;
  end;
end;

procedure TPackageImporter.AssignBooleanValuesToCellCenters(
  DataArray: TDataArray; ScreenObject: TScreenObject;
  ImportedData: T2DIntArray);
var
  PointIndex: Integer;
  ImportedValues: TValueArrayItem;
  DataSetIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Interpolator: TNearestPoint2DInterpolator;
begin
  Assert(DataArray.Orientation = dsoTop);
  if DataArray.TwoDInterpolator = nil then
  begin
    Interpolator := TNearestPoint2DInterpolator.Create(nil);
    try
      DataArray.TwoDInterpolator := Interpolator;
    finally
      Interpolator.Free;
    end;
  end;
  DataSetIndex := ScreenObject.AddDataSet(DataArray);
  ScreenObject.DataSetFormulas[DataSetIndex] := rsObjectImportedValuesB;
  ScreenObject.ImportedValues.Add;
  ImportedValues := ScreenObject.ImportedValues.Items[
    ScreenObject.ImportedValues.Count-1];
  ImportedValues.Values.DataType := rdtBoolean;
  ImportedValues.Values.Count := FGrid.RowCount * FGrid.ColumnCount;
  ImportedValues.Name := DataArray.Name;
  PointIndex := 0;
  for RowIndex := 0 to FGrid.RowCount - 1 do
  begin
    for ColIndex := 0 to FGrid.ColumnCount - 1 do
    begin
      ImportedValues.Values.BooleanValues[PointIndex] :=
        ImportedData[RowIndex, ColIndex] <> 0;
      Inc(PointIndex);
    end;
  end;
end;

procedure TPackageImporter.AssignConstant1DArray(AnArray: TOneDRealArray;
  Value: Double);
var
  Index: Integer;
begin
  for Index := 0 to Length(AnArray) - 1 do
  begin
    AnArray[Index] := Value;
  end;
end;

{ TBasImporter }

constructor TBasImporter.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'BAS:');
  FSpecifiedHeadsList := TObjectList.Create;
end;

procedure TBasImporter.ReadConstantIboundForLayer;
var
  ColIndex: Integer;
  RowIndex: Integer;
  IntegerConstant: Integer;
  Layer: Integer;
  ID: string;
begin
  ReadLn(FImporter.FFile, ID);
  Assert(Trim(ID) = BoundaryArray);
  ReadLn(FImporter.FFile, Layer);
  Dec(Layer);
  ReadLn(FImporter.FFile, IntegerConstant);
  for RowIndex := 0 to Length(FIbound[Layer]) - 1 do
  begin
    for ColIndex := 0 to Length(FIbound[Layer, RowIndex]) - 1 do
    begin
      FIbound[Layer, RowIndex, ColIndex] := IntegerConstant;
    end;
  end;
  FConstantIbound[Layer].IsConstant := True;
  FConstantIbound[Layer].IntegerValue := IntegerConstant;
end;

procedure TBasImporter.ReadConstantIboundForCrossSection;
var
  ColIndex: Integer;
  LayerIndex: Integer;
  IntegerConstant: Integer;
  ID: string;
begin
  ReadLn(FImporter.FFile, ID);
  Assert(Trim(ID) = BoundaryArray);
  ReadLn(FImporter.FFile, IntegerConstant);
  Assert(FCrossSection);
  for LayerIndex := 0 to Length(FIbound) - 1 do
  begin
    Assert(Length(FIbound[LayerIndex]) = 1);
    for ColIndex := 0 to Length(FIbound[LayerIndex, 0]) - 1 do
    begin
      FIbound[LayerIndex, 0, ColIndex] := IntegerConstant;
      FConstantIbound[LayerIndex].IsConstant := True;
      FConstantIbound[LayerIndex].IntegerValue := IntegerConstant;
    end;
  end;
end;

procedure TBasImporter.ReadVariableIboundForLayer;
var
  Layer: Integer;
  ID: string;
  IntArray: T2DIntArray;
begin
  ReadLn(FImporter.FFile, ID);
  Assert(Trim(ID) = BoundaryArray);
  ReadLn(FImporter.FFile, Layer);
  Dec(Layer);
  IntArray := FIbound[Layer];
  ReadVariable2DIntArray(IntArray);
end;

procedure TBasImporter.ReadVariableIboundForCrossSection;
var
  ColIndex: Integer;
  LayerIndex: Integer;
  ID: string;
begin
  ReadLn(FImporter.FFile, ID);
  Assert(Trim(ID) = BoundaryArray);
  Assert(FCrossSection);
  for LayerIndex := 0 to Length(FIbound) - 1 do
  begin
    Assert(Length(FIbound[LayerIndex]) = 1);
    for ColIndex := 0 to Length(FIbound[LayerIndex, 0]) - 1 do
    begin
      Read(FImporter.FFile, FIbound[LayerIndex, 0, ColIndex]);
    end;
    ReadLn(FImporter.FFile);
  end;
end;

procedure TBasImporter.ReadConstantInitialHeadForCrossSection;
var
  ID: string;
  Value: Double;
  LayerIndex: Integer;
begin
  ReadLn(FImporter.FFile, ID);
  ID := Trim(ID);
  Readln(FImporter.FFile, Value);
  Assert(ID = StrInitialHead);
  Assert(FCrossSection);
  for LayerIndex := 0 to Length(FStrt) - 1 do
  begin
    Assert(Length(FStrt[0]) = 1);
    AssignConstant2DArray(Value, FStrt[LayerIndex]);
    FConstantStrt[LayerIndex].IsConstant := True;
    FConstantStrt[LayerIndex].RealValue := Value;
  end;
end;

procedure TBasImporter.ReadConstantInitialHeadForLayer;
var
  Value: Double;
  LayerIndex: Integer;
  ID: string;
  Layer: Integer;
begin
  ReadLn(FImporter.FFile, ID);
  ID := Trim(ID);
  ReadLn(FImporter.FFile, Layer);
  Readln(FImporter.FFile, Value);
  Assert(ID = StrInitialHead);
  LayerIndex := Layer - 1;
  AssignConstant2DArray(Value, FStrt[LayerIndex]);
  FConstantStrt[LayerIndex].IsConstant := True;
  FConstantStrt[LayerIndex].RealValue := Value;
end;

procedure TBasImporter.ReadVariableInitialHeadForCrossSection;
var
  ID: string;
  LayerIndex: Integer;
  ColIndex: Integer;
begin
  ReadLn(FImporter.FFile, ID);
  ID := Trim(ID);
  Assert(ID = StrInitialHead);
  Assert(FCrossSection);
  for LayerIndex := 0 to Length(FStrt) - 1 do
  begin
    Assert(Length(FStrt[LayerIndex]) = 1);
    for ColIndex := 0 to Length(FStrt[LayerIndex, 0]) - 1 do
    begin
      Read(FImporter.FFile, FStrt[LayerIndex, 0, ColIndex]);
    end;
    ReadLn(FImporter.FFile);
  end;
end;

procedure TBasImporter.ReadVariableInitialHeadForLayer;
var
  ID: string;
  Layer: Integer;
  LayerIndex: Integer;
begin
  ReadLn(FImporter.FFile, ID);
  ID := Trim(ID);
  ReadLn(FImporter.FFile, Layer);
  Assert(ID = StrInitialHead);
  LayerIndex := Layer - 1;
  Read2DRealArray(FStrt[LayerIndex]);
end;

procedure TBasImporter.ReleaseMemory;
begin
  inherited;
  SetLength(FIbound, 0, 0, 0);
  SetLength(FConstantIbound, 0);
  SetLength(FStrt, 0, 0, 0);
  SetLength(FConstantStrt, 0);
end;

function TBasImporter.ScreenObjectNameRoot: string;
begin
  result := 'Imported_Specified_Head_';
end;

procedure TBasImporter.ReadDataSet1;
var
  IPRTIM: Integer;
  IFREFM: Integer;
  ICHFLG: Integer;
  IXSEC: Integer;
begin
  Read(FImporter.FFile, IXSEC);
  Read(FImporter.FFile, ICHFLG);
  // IFREFM is not used but you need to read it to get to IPRTIM.
  Read(FImporter.FFile, IFREFM);
  Read(FImporter.FFile, IPRTIM);

  FChtoch := (ICHFLG <> 0);
  FPrintTime := (IPRTIM <> 0);
  FCrossSection := (IXSEC <> 0);
  ReadLn(FImporter.FFile);
end;

procedure TBasImporter.ReadHeading(var AHeading: string);
begin
  ReadLn(FImporter.FFile, AHeading);
  AHeading := Trim(AHeading);
  if Length(AHeading) > 0 then
  begin
    Assert(AHeading[1] = '#');
    AHeading := Trim(Copy(AHeading, 2, MAXINT));
  end;
end;

procedure TBasImporter.ImportActiveCells;
var
  LayerIndex: Integer;
  Interpolator: TNearestPoint2DInterpolator;
  DataArray: TDataArray;
  DataArrayName: string;
  ScreenObject: TScreenObject;
  Group: TLayerGroup;
  GroupIndex: Integer;
  ActiveFormula: string;
  IsConstant: Boolean;
  ConstantValue: Integer;
  Index: Integer;

begin
  ScreenObject := nil;
  IsConstant := True;
  ConstantValue := FConstantIbound[0].IntegerValue;
  for Index := 0 to Length(FConstantIbound) - 1 do
  begin
    IsConstant := FConstantIbound[Index].IsConstant
      and (FConstantIbound[Index].IntegerValue = ConstantValue);
    if not IsConstant then
    begin
      break;
    end;
  end;

  if IsConstant then
  begin
    DataArray := FModel.GetDataSetByName(rsActive);
    if ConstantValue = 0 then
    begin
      DataArray.Formula := 'False';
    end
    else
    begin
      DataArray.Formula := 'True';
    end;
  end
  else
  begin
    LayerIndex := -1;
    if FModel.LayerStructure.Count > 2 then
    begin
      ActiveFormula := 'CaseB(Layer, ';
    end
    else
    begin
      ActiveFormula := '';
    end;
    for GroupIndex := 1 to FModel.LayerStructure.Count - 1 do
    begin
      Group := FModel.LayerStructure.LayerGroups[GroupIndex];
      if Group.Simulated then
      begin
        Inc(LayerIndex);
        if FConstantIbound[LayerIndex].IsConstant then
        begin
          if FConstantIbound[LayerIndex].IntegerValue = 0 then
          begin
            ActiveFormula := ActiveFormula + 'False';
          end
          else
          begin
            ActiveFormula := ActiveFormula + 'True';
          end;
        end
        else
        begin
          if ScreenObject = nil then
          begin
            CreateOrRetrieveCellCenterScreenObject(ScreenObject);
          end;
          DataArrayName := 'Imported_Active_Layer_' + IntToStr(GroupIndex);
          DataArray := FModel.GetDataSetByName(DataArrayName);
          if DataArray = nil then
          begin
            DataArray := FModel.CreateNewDataArray(TDataArray, DataArrayName,
              'True', [], rdtBoolean, eaBlocks, dsoTop, '');

            DataArray.UpdateDimensions(FGrid.LayerCount,
              FGrid.RowCount,FGrid.ColumnCount);

            Interpolator := TNearestPoint2DInterpolator.Create(nil);
            try
              DataArray.TwoDInterpolator := Interpolator;
            finally
              Interpolator.Free;
            end;
          end;
          AssignBooleanValuesToCellCenters(
            DataArray, ScreenObject, FIbound[LayerIndex]);
          ActiveFormula := ActiveFormula + DataArrayName;
        end;
      end
      else
      begin
        ActiveFormula := ActiveFormula + 'True';
      end;
      if GroupIndex < FModel.LayerStructure.Count - 1 then
      begin
        ActiveFormula := ActiveFormula + ', ';
      end;
    end;
    if FModel.LayerStructure.Count > 2 then
    begin
      ActiveFormula := ActiveFormula + ')';
    end;
    DataArray := FModel.GetDataSetByName(rsActive);
    DataArray.Formula := ActiveFormula;
  end;
end;

procedure TBasImporter.ImportSpecifiedHeads;
var
  Item: TChdItem;
  GroupIndex: Integer;
  ModelStartTime: Double;
  ModelEndTime: Double;
  RowIndex: Integer;
  ColIndex: Integer;
  LayerIndex: Integer;
  Group: TLayerGroup;
  ScreenObject: TScreenObject;
  ObjectIndex: integer;
  Boundary: TChdLocationObject;
  InstanceCount: integer;
  SpecifiedHeadLocations: TDataArray;
  Position: Integer;
  List: TList;
  Storage: TBasChdObjects;
  ListIndex: Integer;
  InnerListIndex: Integer;
  AnotherBoundary: TChdLocationObject;
begin
  InstanceCount := 0;
  SpecifiedHeadLocations := nil;

  ModelStartTime := FModel.ModflowStressPeriods[0].StartTime;
  ModelEndTime := FModel.ModflowStressPeriods[
    FModel.ModflowStressPeriods.Count - 1].EndTime;
  LayerIndex := -1;
  ObjectIndex := 0;

  List := TList.Create;
  try
    for GroupIndex := 1 to FModel.LayerStructure.Count - 1 do
    begin
      Group := FModel.LayerStructure.LayerGroups[GroupIndex];
      if Group.Simulated then
      begin
        List.Clear;
        Inc(LayerIndex);
        for RowIndex := 0 to FGrid.RowCount - 1 do
        begin
          for ColIndex := 0 to FGrid.ColumnCount - 1 do
          begin
            if FIbound[LayerIndex, RowIndex, ColIndex] < 0 then
            begin
              Boundary:= TChdLocationObject.Create;
              List.Add(Boundary);
              Boundary.Layer := LayerIndex+1;
              Boundary.Row := RowIndex+1;
              Boundary.Column := ColIndex+1;
              Boundary.StartFactor := FStrt[LayerIndex, RowIndex, ColIndex];
              Boundary.EndFactor := Boundary.StartFactor;
            end;
          end;
        end;
        if List.Count > 0 then
        begin
          AssignObsGroupsToCells(List);

          if not FModel.ModflowPackages.ChdBoundary.IsSelected then
          begin
            FModel.ModflowPackages.ChdBoundary.IsSelected := True;
            FModel.CreateInitialDataSets;
            SpecifiedHeadLocations := FModel.GetDataSetByName(
              rsModflowSpecifiedHead);
            Assert(SpecifiedHeadLocations <> nil);
          end;

          for ListIndex := 0 to List.Count - 1 do
          begin
            Boundary := List[ListIndex];
            if Boundary.Used then
            begin
              Continue;
            end;
            Storage:= TBasChdObjects.Create;
            FSpecifiedHeadsList.Add(Storage);
            Storage.FList.Add(Boundary);
            Boundary.Used := True;
            for InnerListIndex := ListIndex + 1 to List.Count - 1 do
            begin
              AnotherBoundary := List[InnerListIndex];
              if Boundary.SameObservations(AnotherBoundary) then
              begin
                AnotherBoundary.Used := True;
                Storage.FList.Add(AnotherBoundary);
              end;
            end;

            ScreenObject := CreateScreenObject(Storage.FList, ObjectIndex,
              LayerIndex+1, -1);
            ScreenObject.CreateChdBoundary;
            Item := ScreenObject.ModflowChdBoundary.Values.Add as TChdItem;

            Inc(InstanceCount);
            SetItemValues(Item, Storage.FList, ModelEndTime,
              ModelStartTime, ScreenObject, IntToStr(InstanceCount));

            Assert(SpecifiedHeadLocations <> nil);
            Position := ScreenObject.AddDataSet(SpecifiedHeadLocations);
            ScreenObject.DataSetFormulas[Position] := 'True';
            Storage.FScreenObject := ScreenObject;
          end;
        end;
      end;
    end;
  finally
    List.Free;
  end;
  ReleaseMemory;
end;

procedure TBasImporter.ImportInitialHead;
var
  DataArray: TDataArray;
  DataArrayName: string;
  Group: TLayerGroup;
  GroupIndex: Integer;
  InitialHeadFormula: string;
  LayerIndex: Integer;
  ScreenObject: TScreenObject;
  IsConstant: Boolean;
  ConstantValue: Double;
begin
  ScreenObject := nil;
  CheckRealConstArray(ConstantValue, IsConstant, FConstantStrt);
  if IsConstant then
  begin
    DataArray := FModel.GetDataSetByName(rsModflow_Initial_Head);
    DataArray.Formula := FloatToStr(ConstantValue);
  end
  else
  begin
    LayerIndex := -1;
    if FModel.LayerStructure.Count > 2 then
    begin
      InitialHeadFormula := 'CaseR(Layer, ';
    end
    else
    begin
      InitialHeadFormula := '';
    end;
    for GroupIndex := 1 to FModel.LayerStructure.Count - 1 do
    begin
      Group := FModel.LayerStructure.LayerGroups[GroupIndex];
      if Group.Simulated then
      begin
        Inc(LayerIndex);
        if FConstantStrt[LayerIndex].IsConstant then
        begin
          InitialHeadFormula := InitialHeadFormula
            + FloatToStr(FConstantStrt[LayerIndex].RealValue);
        end
        else
        begin
          if ScreenObject = nil then
          begin
            CreateOrRetrieveCellCenterScreenObject(ScreenObject);
//            ScreenObject.Name := 'Imported_Initial_Head_Object_'
//              + IntToStr(GroupIndex);
          end;
          DataArrayName := 'Imported_Initial_Head_Layer_'
            + IntToStr(GroupIndex);

          CreateDataArrayAndAssignValues(ScreenObject, DataArrayName,
            FStrt[LayerIndex]);

          InitialHeadFormula := InitialHeadFormula + DataArrayName;
        end;
      end
      else
      begin
        InitialHeadFormula := InitialHeadFormula + '0';
      end;
      if GroupIndex < FModel.LayerStructure.Count - 1 then
      begin
        InitialHeadFormula := InitialHeadFormula + ', ';
      end;
    end;
    if FModel.LayerStructure.Count > 2 then
    begin
      InitialHeadFormula := InitialHeadFormula + ')';
    end;
    DataArray := FModel.GetDataSetByName(rsModflow_Initial_Head);
    DataArray.Formula := InitialHeadFormula;
  end;
end;

destructor TBasImporter.Destroy;
begin
  FSpecifiedHeadsList.Free;
  inherited;
end;

procedure TBasImporter.HandlePackage;
begin
  inherited;
  if FHeading2 <> '' then
  begin
    FModel.ModflowOptions.Description.Insert(0, FHeading2);
  end;
  if FHeading1 <> '' then
  begin
    FModel.ModflowOptions.Description.Insert(0, FHeading1);
  end;
  FModel.ModflowOptions.ComputeFluxesBetweenConstantHeadCells := FChtoch;
  FModel.ModflowOptions.PrintTime := FPrintTime;
  FModel.ModflowOptions.HNoFlow := FHNoFlo;

  ImportInitialHead;
  ImportActiveCells;

  // Specified heads are imported by TChdObsImporter (even if the
  // the CHOB package is not used.
//  ImportSpecifiedHeads;

  FModel.CreateInitialDataSets;
end;

procedure TBasImporter.ReadData(const ALabel: string);
var
  NLAY: integer;
  NROW: Integer;
  NCOL: Integer;
  Index: Integer;
begin
  inherited;

  if (FIbound = nil) or (FStrt = nil) then
  begin
    NLAY := FModel.LayerStructure.ModflowLayerCount;
    NROW := FGrid.RowCount;
    NCOL := FGrid.ColumnCount;
    if FIbound = nil then
    begin
      SetLength(FIbound, NLAY, NROW, NCOL);
      SetLength(FConstantIbound, NLAY);
      for Index := 0 to NLAY - 1 do
      begin
        FConstantIbound[Index].IsConstant := False;
      end;
    end;
    if FStrt = nil then
    begin
      SetLength(FStrt, NLAY, NROW, NCOL);
      SetLength(FConstantStrt, NLAY);
      for Index := 0 to NLAY - 1 do
      begin
        FConstantStrt[Index].IsConstant := False;
      end;
    end;
  end;

  if ALabel = 'HEADNG(1):' then
  begin
    ReadHeading(FHeading1);
  end
  else if ALabel = 'HEADNG(2):' then
  begin
    ReadHeading(FHeading2);
  end
  else if ALabel = 'IXSEC, ICHFLG, IFREFM, IPRTIM:' then
  begin
    ReadDataSet1;
  end
  else if ALabel = StrConstant2DIntegerArrayForLayer then
  begin
    ReadConstantIboundForLayer;
  end
  else if ALabel = StrConstant2DIntegerArray then
  begin
    ReadConstantIboundForCrossSection;
  end
  else if ALabel = StrVariable2DIntegerArrayForLayer then
  begin
    ReadVariableIboundForLayer;
  end
  else if ALabel = StrVariable2DIntegerArrayForCrossSection then
  begin
    ReadVariableIboundForCrossSection;
  end
  else if ALabel = 'HNOFLO:' then
  begin
    ReadLn(FImporter.FFile, FHNoFlo);
  end
  else if ALabel = StrConstant2DRealArray then
  begin
    ReadConstantInitialHeadForCrossSection;
  end
  else if ALabel = StrConstant2DRealArrayForLayer then
  begin
    ReadConstantInitialHeadForLayer;
  end
  else if ALabel = StrVariable2DRealArray then
  begin
    Assert(False);
//    ReadVariableInitialHeadForCrossSection;
 end
  else if ALabel = StrVariable2DRealArrayForLayer then
  begin
    ReadVariableInitialHeadForLayer;
  end
  else if ALabel = StrVariable2DRealArrayForCrossSection then
  begin
    ReadVariableInitialHeadForCrossSection;
//    Assert(False);
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TPackageImporter.CreateOrRetrieveCellCenterScreenObject(
  var ScreenObject: TScreenObject);
var
  UndoCreateScreenObject: TCustomUndo;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  if GlobalCellCenterScreenObject = nil then
  begin
    GlobalCellCenterScreenObject := TScreenObject.CreateWithViewDirection(
      FModel, vdTop, UndoCreateScreenObject, False);

    FModel.AddScreenObject(GlobalCellCenterScreenObject);
    GlobalCellCenterScreenObject.ElevationCount := ecZero;
    GlobalCellCenterScreenObject.SetValuesByInterpolation := True;
    GlobalCellCenterScreenObject.EvaluatedAt := eaBlocks;
    GlobalCellCenterScreenObject.Visible := False;
    GlobalCellCenterScreenObject.Capacity := FGrid.RowCount * FGrid.ColumnCount;
    for RowIndex := 0 to FGrid.RowCount - 1 do
    begin
      for ColIndex := 0 to FGrid.ColumnCount - 1 do
      begin
        GlobalCellCenterScreenObject.AddPoint(FImporter.CenterPoints[
          RowIndex, ColIndex], True);
      end;
    end;
    GlobalCellCenterScreenObject.Name := 'Imported_Arrays';
  end;
  ScreenObject := GlobalCellCenterScreenObject;
end;

procedure TPackageImporter.AssignRealValuesToCellCenters(DataArray: TDataArray;
  ScreenObject: TScreenObject; ImportedData: T2DDoubleArray);
var
  PointIndex: Integer;
  ImportedValues: TValueArrayItem;
  DataSetIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Interpolator: TNearestPoint2DInterpolator;
begin
  Assert(DataArray.Orientation = dsoTop);
  if DataArray.TwoDInterpolator = nil then
  begin
    Interpolator := TNearestPoint2DInterpolator.Create(nil);
    try
      DataArray.TwoDInterpolator := Interpolator;
    finally
      Interpolator.Free;
    end;
  end;
  DataSetIndex := ScreenObject.AddDataSet(DataArray);
  ScreenObject.DataSetFormulas[DataSetIndex] := rsObjectImportedValuesR
    + '("' + DataArray.Name + '")';
  ScreenObject.ImportedValues.Add;
  ImportedValues := ScreenObject.ImportedValues.Items[
    ScreenObject.ImportedValues.Count-1];
  ImportedValues.Values.DataType := rdtDouble;
  ImportedValues.Values.Count := FGrid.RowCount * FGrid.ColumnCount;
  ImportedValues.Name := DataArray.Name;
  PointIndex := 0;
  for RowIndex := 0 to FGrid.RowCount - 1 do
  begin
    for ColIndex := 0 to FGrid.ColumnCount - 1 do
    begin
      ImportedValues.Values.RealValues[PointIndex] :=
        ImportedData[RowIndex, ColIndex];
      Inc(PointIndex);
    end;
  end;
end;

{ TMultZoneImporter }

function TMultZoneImporter.ConstantZone(Index: integer;
  out ZoneValue: integer): boolean;
begin
  ZoneValue := 0;
  result := Length(FConstantZoneArrays) > 0;
  if result then
  begin
    result := FConstantZoneArrays[Index].IsConstant;
    ZoneValue := FConstantZoneArrays[Index].IntegerValue;
  end;
end;

constructor TMultZoneImporter.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'ZONE_MULT:');
  FModel := frmGoPhast.PhastModel;
  FGrid := FModel.ModflowGrid;
  FZoneNames := TStringList.Create;
  FMultNames := TStringList.Create;
  FZoneNames.CaseSensitive := False;
end;

destructor TMultZoneImporter.Destroy;
begin
  FMultNames.Free;
  FZoneNames.Free;
  inherited;
end;

procedure TMultZoneImporter.ImportZones;
var
  ZoneIndex: Integer;
  DataArrayName: string;
  DataArray: TDataArray;
  ScreenObject: TScreenObject;
begin
  ScreenObject := nil;
  Assert(FZoneNames.Count = Length(FZoneArrays));
  Assert(FZoneNames.Count = Length(FConstantZoneArrays));
  for ZoneIndex := 0 to FZoneNames.Count - 1 do
  begin
    DataArrayName := FixArrayName(FZoneNames[ZoneIndex]);
    DataArray := FModel.GetDataSetByName(DataArrayName);
    if DataArray = nil then
    begin
      DataArray := FModel.CreateNewDataArray(TDataArray,
        DataArrayName, '0', [], rdtInteger, eaBlocks, dsoTop, '');

      DataArray.UpdateDimensions(FGrid.LayerCount,
        FGrid.RowCount, FGrid.ColumnCount);
    end;
    if FConstantZoneArrays[ZoneIndex].IsConstant then
    begin
      DataArray.Formula := IntToStr(
        FConstantZoneArrays[ZoneIndex].IntegerValue);
    end
    else
    begin
      DataArray.Formula := '0';
      if ScreenObject = nil then
      begin
        CreateOrRetrieveCellCenterScreenObject(ScreenObject);
      end;
      AssignIntegerValuesToCellCenters(DataArray, ScreenObject,
        FZoneArrays[ZoneIndex]);
    end;
  end;
end;

function TMultZoneImporter.IndexOfZone(Name: string): integer;
begin
  result := FZoneNames.IndexOf(Name);
end;

procedure TMultZoneImporter.ImportMultipliers;
var
  MultIndex: Integer;
  DataArrayName: string;
  DataArray: TDataArray;
  ScreenObject: TScreenObject;
begin
  ScreenObject := nil;
  Assert(FMultNames.Count = Length(FMultArrays));
  Assert(FMultNames.Count = Length(FConstantMultArrays));
  for MultIndex := 0 to FMultNames.Count - 1 do
  begin
    DataArrayName := FixArrayName(FMultNames[MultIndex]);
    DataArray := FModel.GetDataSetByName(DataArrayName);
    if DataArray = nil then
    begin
      DataArray := FModel.CreateNewDataArray(TDataArray, DataArrayName, '0',
        [], rdtDouble, eaBlocks, dsoTop, '');

      DataArray.UpdateDimensions(FGrid.LayerCount,
        FGrid.RowCount, FGrid.ColumnCount);
    end;
    if FConstantMultArrays[MultIndex].IsConstant then
    begin
      DataArray.Formula := FloatToStr(FConstantMultArrays[MultIndex].RealValue);
    end
    else
    begin
      DataArray.Formula := '0';
      if ScreenObject = nil then
      begin
        CreateOrRetrieveCellCenterScreenObject(ScreenObject);
      end;
      AssignRealValuesToCellCenters(DataArray, ScreenObject,
        FMultArrays[MultIndex]);
    end;
  end;
end;

procedure TMultZoneImporter.HandlePackage;
begin
  inherited;
  ImportZones;
  ImportMultipliers;
//  ReleaseMemory;
end;

procedure TMultZoneImporter.ReadVariableZoneArray;
var
  IntArray: T2DIntArray;
  ID: string;
begin
  ReadLn(FImporter.FFile, ID);
  ID := Trim(ID);
  if (Pos(FCurrentZoneName, ID) > 0) and (Pos('ZONE ARRAY:', ID) > 0) then
  begin
    IntArray := FZoneArrays[FZoneNames.Count - 1];
    ReadVariable2DIntArray(IntArray);
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TMultZoneImporter.ReadConstantZoneArray;
var
  Value: Integer;
  ALine: string;
begin
  ReadLn(FImporter.FFile, ALine);
  ALine := Trim(ALine);
  Readln(FImporter.FFile, Value);
  if (Pos(FCurrentZoneName, ALine) > 0)
    and (Pos('ZONE ARRAY:', ALine) > 0) then
  begin
    FConstantZoneArrays[FZoneNames.Count - 1].IsConstant := True;
    FConstantZoneArrays[FZoneNames.Count - 1].IntegerValue := Value;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TMultZoneImporter.ReadZoneName;
var
  NZ: Integer;
  ZONNAM: string;
begin
  Readln(FImporter.FFile, NZ);
  Readln(FImporter.FFile, ZONNAM);
  Dec(NZ);
  Assert(NZ = FZoneNames.Count);
  ZONNAM := Trim(ZONNAM);
  FCurrentZoneName := ZONNAM;
  ZONNAM := FixArrayName(ZONNAM);
  FZoneNames.Add(ZONNAM);
end;

//procedure TMultZoneImporter.ReleaseMemory;
//begin
//  SetLength(FZoneArrays, 0, 0, 0);
//  SetLength(FConstantZoneArrays, 0);
//  SetLength(FMultArrays, 0, 0, 0);
//  SetLength(FConstantMultArrays, 0);
//end;

function TMultZoneImporter.ZoneArray(Index: integer): T2DIntArray;
begin
  result := FZoneArrays[Index];
end;

procedure TMultZoneImporter.ReadMultiplierDefinedByFunction;
begin
  Read2DRealArray(FMultArrays[FMultNames.Count - 1]);
end;

procedure TMultZoneImporter.ReadVariableMultiplier;
var
  ALine: string;
begin
  ReadLn(FImporter.FFile, ALine);
  ALine := Trim(ALine);
  if (Pos(FCurrentMultName, ALine) > 0)
    and (Pos('MULT. ARRAY:', ALine) > 0) then
  begin
    Read2DRealArray(FMultArrays[FMultNames.Count - 1]);
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TMultZoneImporter.ReadConstantMultiplier;
var
  ALine: string;
  Value: Double;
begin
  ReadLn(FImporter.FFile, ALine);
  ALine := Trim(ALine);
  Readln(FImporter.FFile, Value);
  if (Pos(FCurrentMultName, ALine) > 0)
    and (Pos('MULT. ARRAY:', ALine) > 0) then
  begin
    FConstantMultArrays[FMultNames.Count - 1].IsConstant := True;
    FConstantMultArrays[FMultNames.Count - 1].RealValue := Value;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TMultZoneImporter.ReadMultiplierName;
var
  M: Integer;
  MLTNAM: string;
begin
  Readln(FImporter.FFile, M);
  Readln(FImporter.FFile, MLTNAM);
  Dec(M);
  Assert(M = FMultNames.Count);
  MLTNAM := Trim(MLTNAM);
  FCurrentMultName := MLTNAM;
  MLTNAM := FixArrayName(MLTNAM);
  FMultNames.Add(MLTNAM);
end;

procedure TMultZoneImporter.ReadNumberOfMultipliers;
var
  NMLTAR: Integer;
begin
  ReadLn(FImporter.FFile, NMLTAR);
  SetLength(FMultArrays, NMLTAR, FGrid.RowCount, FGrid.ColumnCount);
  SetLength(FConstantMultArrays, NMLTAR);
  FMultNames.Capacity := NMLTAR;
end;

procedure TMultZoneImporter.ReadNumberOfZones;
var
  NZONAR: Integer;
begin
  ReadLn(FImporter.FFile, NZONAR);
  SetLength(FZoneArrays, NZONAR, FGrid.RowCount, FGrid.ColumnCount);
  SetLength(FConstantZoneArrays, NZONAR);
  FZoneNames.Capacity := NZONAR;
end;

procedure TMultZoneImporter.ReadData(const ALabel: string);
begin
  inherited;
  if ALabel = 'NZONAR:' then
  begin
    ReadNumberOfZones;
  end
  else if ALabel = 'NMLTAR:' then
  begin
    ReadNumberOfMultipliers;
  end
  else if ALabel = 'M, MLTNAM(M):' then
  begin
    ReadMultiplierName;
  end
  else if ALabel = StrConstant2DRealArray then
  begin
    ReadConstantMultiplier;
  end
  else if ALabel = StrVariable2DRealArray then
  begin
    ReadVariableMultiplier;
  end
  else if ALabel = 'DEFINED BY FUNCTION:' then
  begin
    ReadMultiplierDefinedByFunction;
  end
  else if ALabel = 'NZ, ZONNAM(NZ):' then
  begin
    ReadZoneName;
  end
  else if ALabel = StrConstant2DIntegerArray then
  begin
    ReadConstantZoneArray;
  end
  else if ALabel = StrVariable2DIntegerArray then
  begin
    ReadVariableZoneArray;
  end
  else
  begin
//    raise Exception.Create(ALabel);
    Assert(False);
  end;
end;

procedure TPackageImporter.ReadVariable2DIntArray(IntArray: T2DIntArray);
var
  ColIndex: Integer;
  RowIndex: Integer;
begin
  for RowIndex := 0 to Length(IntArray) - 1 do
  begin
    for ColIndex := 0 to Length(IntArray[RowIndex]) - 1 do
    begin
      Read(FImporter.FFile, IntArray[RowIndex, ColIndex]);
    end;
    ReadLn(FImporter.FFile);
  end;
end;

function TPackageImporter.UniformArray(ImportedData: TDoubleArray): boolean;
var
  AValue: Double;
  Index: Integer;
begin
  result := True;
  AValue := ImportedData[0];
  for Index := 1 to Length(ImportedData) - 1 do
  begin
    if AValue <> ImportedData[Index] then
    begin
      result := False;
      Exit;
    end;
  end;
end;

{ TLpfImporter }

constructor TLpfImporter.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'LPF:');
  FIsSelected := False;
  FNextParameterIndex := -1;
end;

procedure TArrayImporter.ImportDataSet(ImportName: string;
  ImportArrayName: string; const ThreeDConstRealArray: TRealConstantRecordArray;
  const ThreeDRealArray: T3DDoubleArray; const LayerFormulaSuffix: string = '');
var
  DataArrayName: string;
  ScreenObject: TScreenObject;
  Group: TLayerGroup;
  GroupIndex: Integer;
  DataSetFormula: string;
  LayerIndex: Integer;
  DataArray: TDataArray;
  IsConstant: Boolean;
  ConstantValue: Double;
begin
  ScreenObject := nil;
  if (ThreeDRealArray <> nil) or (ThreeDConstRealArray <> nil) then
  begin
    CheckRealConstArray(ConstantValue, IsConstant, ThreeDConstRealArray);
    if IsConstant then
    begin
      DataArray := FModel.GetDataSetByName(ImportArrayName);
      DataArray.Formula := FloatToStr(ConstantValue);
    end
    else
    begin
      LayerIndex := -1;
      if FModel.LayerStructure.Count > 2 then
      begin
        DataSetFormula := 'CaseR(Layer, ';
      end
      else
      begin
        DataSetFormula := '';
      end;
      for GroupIndex := 1 to FModel.LayerStructure.Count - 1 do
      begin
        Group := FModel.LayerStructure.LayerGroups[GroupIndex];
        if Group.Simulated then
        begin
          Inc(LayerIndex);
        end;
        Assert(LayerIndex >= 0);
        if (ThreeDConstRealArray <> nil)
          and ThreeDConstRealArray[LayerIndex].IsConstant then
        begin
          DataSetFormula := DataSetFormula
            + FloatToStr(ThreeDConstRealArray[LayerIndex].RealValue);
        end
        else
        begin
          if Group.Simulated and (LayerIndex < Length(ThreeDRealArray)) then
          begin
            if ThreeDRealArray[LayerIndex] = nil then
            begin
              DataArrayName := '0.';
            end
            else
            begin
              if ScreenObject = nil then
              begin
                CreateOrRetrieveCellCenterScreenObject(ScreenObject);
              end;
              DataArrayName := 'Imported_' + ImportName
                + '_Layer_' + IntToStr(GroupIndex);
              CreateDataArrayAndAssignValues(ScreenObject, DataArrayName,
                ThreeDRealArray[LayerIndex]);
            end;
            if LayerFormulaSuffix <> '' then
            begin
              DataSetFormula := DataSetFormula + '(';
            end;
            DataSetFormula := DataSetFormula + DataArrayName;
            if LayerFormulaSuffix <> '' then
            begin
              DataSetFormula := DataSetFormula + LayerFormulaSuffix + ')';
            end;
          end
          else
          begin
            DataSetFormula := DataSetFormula + '0';
          end;
        end;
        if GroupIndex < FModel.LayerStructure.Count - 1 then
        begin
          DataSetFormula := DataSetFormula + ', ';
        end;
      end;
      if FModel.LayerStructure.Count > 2 then
      begin
        DataSetFormula := DataSetFormula + ')';
      end;
      DataArray := FModel.GetDataSetByName(ImportArrayName);
      DataArray.Formula := DataSetFormula;
    end;
  end;
end;

procedure TLpfImporter.ImportWetDry;
begin
  ImportDataSet('WetDry', rsWetDry, FWetDryConst, FWetDry);
end;

procedure TLpfImporter.ImportVerticalHydraulicConductivity;
var
  ConstantValueK: Double;
  IsConstantK: Boolean;
  ConstantValueQuasiK: Double;
  IsConstantQuasiK: Boolean;
  IsConstant: Boolean;
  DataArray: TDataArray;
  LayerIndex: Integer;
  DataSetFormulaKz: string;
  GroupIndex: Integer;
  Group: TLayerGroup;
  ScreenObject: TScreenObject;
  DataArrayName: string;
  Index: Integer;
  AnisotropyFormulaKz: string;
  UseAnisotropy: Boolean;
  UseAnisotropyInLayer: Boolean;
begin
  ScreenObject := nil;
  if (FVerticalK <> nil) or (FVerticalKConst <> nil)
    or (FQuasiVerticalK <> nil) or (FQuasiVerticalKConst <> nil) then
  begin
    CheckRealConstArray(ConstantValueK, IsConstantK, FVerticalKConst);
    if FQuasiVerticalKConst = nil then
    begin
      IsConstant := IsConstantK;
    end
    else
    begin
      CheckRealConstArray(ConstantValueQuasiK, IsConstantQuasiK,
        FQuasiVerticalKConst);
      IsConstant := IsConstantK and IsConstantQuasiK
        and (ConstantValueK = ConstantValueQuasiK);
    end;
    if IsConstant then
    begin
      for Index := 0 to Length(LAYVKA) - 1 do
      begin
        IsConstant := LAYVKA[Index] = 0;
        if not IsConstant then
        begin
          break;
        end;
      end;
    end;
    if IsConstant then
    begin
      DataArray := FModel.GetDataSetByName(rsKz);
      DataArray.Formula := FloatToStr(ConstantValueK);
    end
    else
    begin
      LayerIndex := -1;
      UseAnisotropy := False;
      if FModel.LayerStructure.Count > 2 then
      begin
        DataSetFormulaKz := 'CaseR(Layer, ';
        AnisotropyFormulaKz := 'CaseR(Layer, ';
      end
      else
      begin
        DataSetFormulaKz := '';
        AnisotropyFormulaKz := '';
      end;
      for GroupIndex := 1 to FModel.LayerStructure.Count - 1 do
      begin
        Group := FModel.LayerStructure.LayerGroups[GroupIndex];
        if Group.Simulated then
        begin
          Inc(LayerIndex);
        end;
        Assert(LayerIndex >= 0);
        if Group.Simulated then
        begin
          UseAnisotropyInLayer := LAYVKA[LayerIndex] <> 0;
          if UseAnisotropyInLayer then
          begin
            UseAnisotropy := True;
          end;
          if not UseAnisotropyInLayer and
            (FVerticalKConst <> nil)
            and FVerticalKConst[LayerIndex].IsConstant
            then
          begin
            AnisotropyFormulaKz := AnisotropyFormulaKz + '0';
            DataSetFormulaKz := DataSetFormulaKz
              + FloatToStr(FVerticalKConst[LayerIndex].RealValue);
          end
          else if UseAnisotropyInLayer
            and ((FHorizontalToVerticalAnisotropyConst <> nil)
            and (FHorizontalToVerticalAnisotropyConst[
            LayerIndex].IsConstant)) then
          begin
              DataSetFormulaKz := DataSetFormulaKz + '0';
              AnisotropyFormulaKz := AnisotropyFormulaKz
                + FloatToStr(FHorizontalToVerticalAnisotropyConst[
                LayerIndex].RealValue);

          end
          else
          begin
            if UseAnisotropyInLayer then
            begin
              if (FHorizontalToVerticalAnisotropy = nil)
                or (FHorizontalToVerticalAnisotropy[LayerIndex] = nil) then
              begin
                DataSetFormulaKz := DataSetFormulaKz + '0';
                AnisotropyFormulaKz := AnisotropyFormulaKz + '0';
              end
              else
              begin
                if ScreenObject = nil then
                begin
                  CreateOrRetrieveCellCenterScreenObject(ScreenObject);
                end;
                DataArrayName :=
                  'Imported_Vertical_Anisotropy_Layer_' + IntToStr(GroupIndex);
                CreateDataArrayAndAssignValues(ScreenObject, DataArrayName,
                  FHorizontalToVerticalAnisotropy[LayerIndex]);
                DataSetFormulaKz := DataSetFormulaKz + '0';
                AnisotropyFormulaKz := AnisotropyFormulaKz + DataArrayName;
              end;
            end
            else
            begin
              if (FVerticalK = nil)
                or (FVerticalK[LayerIndex] = nil) then
              begin
                DataSetFormulaKz := DataSetFormulaKz + '0';
                AnisotropyFormulaKz := AnisotropyFormulaKz + '0';
              end
              else
              begin
                if ScreenObject = nil then
                begin
                  CreateOrRetrieveCellCenterScreenObject(ScreenObject);
                end;
                Assert(FVerticalK <> nil);
                DataArrayName := 'Imported_Kz_Layer_' + IntToStr(GroupIndex);
                CreateDataArrayAndAssignValues(ScreenObject, DataArrayName,
                  FVerticalK[LayerIndex]);
                AnisotropyFormulaKz := AnisotropyFormulaKz + '0';
                DataSetFormulaKz := DataSetFormulaKz + DataArrayName;
              end;
            end;
          end;
        end
        else
        begin
          if (FQuasiVerticalKConst <> nil)
            and FQuasiVerticalKConst[LayerIndex].IsConstant then
          begin
            AnisotropyFormulaKz := AnisotropyFormulaKz + '0';
            DataSetFormulaKz := DataSetFormulaKz
              + FloatToStr(FQuasiVerticalKConst[LayerIndex].RealValue);
          end
          else
          begin
            if (FQuasiVerticalK = nil)
              or (FQuasiVerticalK[LayerIndex] = nil) then
            begin
              DataSetFormulaKz := DataSetFormulaKz + '0';
              AnisotropyFormulaKz := AnisotropyFormulaKz + '0';
            end
            else
            begin
              if ScreenObject = nil then
              begin
                CreateOrRetrieveCellCenterScreenObject(ScreenObject);
              end;
              DataArrayName := 'Imported_Kz_Layer_' + IntToStr(GroupIndex);
              CreateDataArrayAndAssignValues(ScreenObject, DataArrayName,
                FQuasiVerticalK[LayerIndex]);
              DataSetFormulaKz := DataSetFormulaKz + DataArrayName;
              AnisotropyFormulaKz := AnisotropyFormulaKz + '0';
            end;
          end;
        end;
        if GroupIndex < FModel.LayerStructure.Count - 1 then
        begin
          DataSetFormulaKz := DataSetFormulaKz + ', ';
          AnisotropyFormulaKz := AnisotropyFormulaKz + ', ';
        end;
      end;
      if FModel.LayerStructure.Count > 2 then
      begin
        DataSetFormulaKz := DataSetFormulaKz + ')';
        AnisotropyFormulaKz := AnisotropyFormulaKz + ')';
      end;
      DataArray := FModel.GetDataSetByName(rsKz);
      DataArray.Formula := DataSetFormulaKz;
      DataArray := FModel.GetDataSetByName(rsModflow_CBKz);
      if DataArray <> nil then
      begin
        DataArray.Formula := DataSetFormulaKz;
      end;
      if UseAnisotropy then
      begin
        DataArray := FModel.GetDataSetByName(rsVerticalAnisotropy);
        DataArray.Formula := AnisotropyFormulaKz;
      end;
    end;
  end;
end;

procedure TLpfImporter.ImportHorizontalHydraulicConductivity;
begin
  ImportDataSet('Kx', rsKx, FHkConst, FHk);
end;

procedure TLpfImporter.ImportSpecificStorage;
var
  Suffix: string;
begin
  Suffix := '';
  if FStorageCoefficientChoice then
  begin
    Suffix := ' / ' + StrLayerHeight;
  end;
  ImportDataSet('Specific_Storage', rsSpecific_Storage,
    FSpecificStorageConst, FSpecificStorage, Suffix);
end;

procedure TLpfImporter.ImportSpecificYield;
begin
  ImportDataSet('Specific_Yield', rsSpecificYield,
    FSpecificYieldConst, FSpecificYield);
end;

procedure TLpfImporter.ImportLpfParameters;
var
  ClusterIndex: Integer;
  Cluster: TClusterRecord;
  MultName: string;
  MultDataArray: TDataArray;
  ZoneName: string;
  ZoneDataArray: TDataArray;
  MultFunctionList: TStringList;
  ZoneFunctionList: TStringList;
  LayerIndex: Integer;
  ZoneFunction: string;
  ZoneIndex: Integer;
  MultFunction: string;
  MultIndex: Integer;
  Index: Integer;
  Param: TModflowSteadyParameter;
  Instance: TInstanceRecord;
  MultUsed: Boolean;
  ZoneUsed: Boolean;
  IntList: TIntegerList;
begin
  for Index := 0 to Length(FParameters) - 1 do
  begin
    Param := FModel.ModflowSteadyParameters.Add as TModflowSteadyParameter;
    Param.ParameterName := FParameters[Index].PARNAM;
    Param.Value := FParameters[Index].Parval;
    if FParameters[Index].PARTYP = 'HK' then
    begin
      Param.ParameterType := ptLPF_HK;
    end
    else if FParameters[Index].PARTYP = 'HANI' then
    begin
      Param.ParameterType := ptLPF_HANI;
    end
    else if FParameters[Index].PARTYP = 'VK' then
    begin
      Param.ParameterType := ptLPF_VK;
    end
    else if FParameters[Index].PARTYP = 'VANI' then
    begin
      Param.ParameterType := ptLPF_VANI;
    end
    else if FParameters[Index].PARTYP = 'SS' then
    begin
      Param.ParameterType := ptLPF_SS;
    end
    else if FParameters[Index].PARTYP = 'SY' then
    begin
      Param.ParameterType := ptLPF_SY;
    end
    else if FParameters[Index].PARTYP = 'VKCB' then
    begin
      Param.ParameterType := ptLPF_VKCB;
    end
    else
    begin
      Assert(False);
    end;
    Assert(Length(FParameters[Index].Instances) = 1);
    Instance := FParameters[Index].Instances[0];
    MultUsed := False;
    ZoneUsed := False;
    IntList := TIntegerList.Create;
    try
      IntList.Sorted := True;
      for ClusterIndex := 0 to Length(Instance.Clusters) - 1 do
      begin
        Cluster := Instance.Clusters[ClusterIndex];
        IntList.AddUnique(Cluster.Layer);
        if not SameText(Cluster.MultiplierName, StrNone) then
        begin
          MultUsed := True;
        end;
        if not SameText(Cluster.ZoneName, StrAll) then
        begin
          ZoneUsed := True;
        end;
      end;
      if Param.ParameterType = ptLPF_VKCB then
      begin
        Param.UseMultiplier := MultUsed or
          (IntList.Count < FModel.LayerStructure.ModflowConfiningBedCount);
      end
      else
      begin
        Param.UseMultiplier := MultUsed or
          (IntList.Count < FModel.LayerStructure.ModflowLayerCount);
      end;
      if Param.UseMultiplier then
      begin
        MultName := Param.MultiplierName;
        MultDataArray := FModel.GetDataSetByName(MultName);
      end
      else
      begin
        MultDataArray := nil;
      end;
      if Param.ParameterType = ptLPF_VKCB then
      begin
        Param.UseZone := ZoneUsed or
          (IntList.Count < FModel.LayerStructure.ModflowConfiningBedCount);
      end
      else
      begin
        Param.UseZone := ZoneUsed or
          (IntList.Count < FModel.LayerStructure.ModflowLayerCount);
      end;
      if Param.UseZone then
      begin
        ZoneName := Param.ZoneName;
        ZoneDataArray := FModel.GetDataSetByName(ZoneName);
      end
      else
      begin
        ZoneDataArray := nil;
      end;
    finally
      IntList.Free;
    end;
    MultFunctionList := TStringList.Create;
    ZoneFunctionList := TStringList.Create;
    try
      for LayerIndex := 0 to FGrid.LayerCount - 1 do
      begin
        MultFunctionList.Add('0');
        ZoneFunctionList.Add('False');
      end;
      for ClusterIndex := 0 to Length(Instance.Clusters) - 1 do
      begin
        Cluster := Instance.Clusters[ClusterIndex];
        LayerIndex := FModel.LayerStructure.
          ModflowLayerToDataSetLayer(Cluster.Layer);
        if Param.ParameterType = ptLPF_VKCB then
        begin
          Inc(LayerIndex);
        end;
        if SameText(Cluster.MultiplierName, StrNone) then
        begin
          MultFunctionList[LayerIndex] := '1';
        end
        else
        begin
          MultFunctionList[LayerIndex] := FixArrayName(Cluster.MultiplierName);
        end;
        if SameText(Cluster.ZoneName, StrAll) then
        begin
          ZoneFunctionList[LayerIndex] := 'True';
        end
        else
        begin
          ZoneFunction := '';
          for ZoneIndex := 0 to Length(Cluster.ZoneValues) - 1 do
          begin
            ZoneFunction := ZoneFunction + '(' + FixArrayName(Cluster.ZoneName)
              + ' = ' + IntToStr(Cluster.ZoneValues[ZoneIndex]) + ')';
            if ZoneIndex < Length(Cluster.ZoneValues) - 1 then
            begin
              ZoneFunction := ZoneFunction + ' or ';
            end;
          end;
          ZoneFunctionList[LayerIndex] := ZoneFunction;
        end;
      end;
      if MultDataArray <> nil then
      begin
        MultFunction := 'CaseR(Layer, ';
        for MultIndex := 0 to MultFunctionList.Count - 1 do
        begin
          MultFunction := MultFunction + MultFunctionList[MultIndex];
          if MultIndex < MultFunctionList.Count - 1 then
          begin
            MultFunction := MultFunction + ', ';
          end;
        end;
        MultFunction := MultFunction + ')';
        MultDataArray.Formula := MultFunction;
      end;
      if ZoneDataArray <> nil then
      begin
        if ZoneFunctionList.Count = 1 then
        begin
          ZoneFunction :=ZoneFunctionList[0];
        end
        else
        begin
          ZoneFunction := 'CaseB(Layer, ';
          for ZoneIndex := 0 to ZoneFunctionList.Count - 1 do
          begin
            ZoneFunction := ZoneFunction + ZoneFunctionList[ZoneIndex];
            if ZoneIndex < ZoneFunctionList.Count - 1 then
            begin
              ZoneFunction := ZoneFunction + ', ';
            end;
          end;
          ZoneFunction := ZoneFunction + ')';
        end;
        ZoneDataArray.Formula := ZoneFunction;
      end;
    finally
      MultFunctionList.Free;
      ZoneFunctionList.Free;
    end;
  end;
end;

procedure TLpfImporter.ImportDataSet7;
begin
  if FModel.ModflowWettingOptions.WettingActive then
  begin
    FModel.ModflowWettingOptions.WettingFactor := WETFCT;
    FModel.ModflowWettingOptions.WettingIterations := IWETIT;
    FModel.ModflowWettingOptions.WettingEquation := IHDWET;
  end;
end;

procedure TLpfImporter.ImportDataSet6;
var
  LayerGroup: TLayerGroup;
  Index: Integer;
  LayerIndex: Integer;
begin
  // Data set 6.
  LayerIndex := -1;
  for Index := 1 to FModel.LayerStructure.Count - 1 do
  begin
    LayerGroup := FModel.LayerStructure.LayerGroups[Index];
    if LayerGroup.Simulated then
    begin
      Inc(LayerIndex);
      if LAYWET[LayerIndex] <> 0 then
      begin
        FModel.ModflowWettingOptions.WettingActive := True;
        break;
      end;
    end;
  end;
end;

procedure TLpfImporter.ImportDataSet5;
var
  LayerGroup: TLayerGroup;
  Index: Integer;
  LayerIndex: Integer;
begin
  // Data set 5.
  LayerIndex := -1;
  for Index := 1 to FModel.LayerStructure.Count - 1 do
  begin
    LayerGroup := FModel.LayerStructure.LayerGroups[Index];
    if LayerGroup.Simulated then
    begin
      Inc(LayerIndex);
    end;
    Assert(LayerIndex >= 0);
    LayerGroup.VerticalHydraulicConductivityMethod := LAYVKA[LayerIndex];
  end;
end;

procedure TLpfImporter.ImportHorizontalAnisotropy;
var
  DataArrayName: string;
  ScreenObject: TScreenObject;
  Value: Double;
  LayerGroup: TLayerGroup;
  GroupIndex: Integer;
  LayerIndex: Integer;
  HorizontalAnisotropyFormula: string;
  DataArray: TDataArray;
  Index: Integer;
  IsConstant: Boolean;
  ConstantValue: Double;
begin
  ScreenObject := nil;
  // Data sets 4 and 11.
  ConstantValue := CHANI[0];
  IsConstant := ConstantValue > 0;
  if IsConstant then
  begin
    for Index := 1 to Length(CHANI) - 1 do
    begin
      IsConstant := (CHANI[Index] = ConstantValue);
      if not IsConstant then
      begin
        break;
      end;
    end;
  end;
  if IsConstant then
  begin
    HorizontalAnisotropyFormula := FloatToStr(ConstantValue);
  end
  else
  begin
    if FModel.LayerStructure.Count > 2 then
    begin
      HorizontalAnisotropyFormula := 'CaseR(Layer, ';
    end
    else
    begin
      HorizontalAnisotropyFormula := '';
    end;
    LayerIndex := -1;
    for GroupIndex := 1 to FModel.LayerStructure.Count - 1 do
    begin
      LayerGroup := FModel.LayerStructure.LayerGroups[GroupIndex];
      if LayerGroup.Simulated then
      begin
        Inc(LayerIndex);
      end;
      Assert(LayerIndex >= 0);
      Value := CHANI[LayerIndex];
      if Value > 0 then
      begin
        HorizontalAnisotropyFormula :=
          HorizontalAnisotropyFormula + FloatToStr(Value);
      end
      else if (FHorizontalAnisotropyConst <> nil)
        and FHorizontalAnisotropyConst[LayerIndex].IsConstant then
      begin
        HorizontalAnisotropyFormula := HorizontalAnisotropyFormula
          + FloatToStr(FHorizontalAnisotropyConst[LayerIndex].RealValue);
      end
      else if (FHorizontalAnisotropy <> nil)
        and (FHorizontalAnisotropy[LayerIndex] <> nil) then
      begin
        if ScreenObject = nil then
        begin
          CreateOrRetrieveCellCenterScreenObject(ScreenObject);
        end;
        DataArrayName := 'Imported_Horizontal_Anisotropy_Layer_'
          + IntToStr(GroupIndex);
        CreateDataArrayAndAssignValues(ScreenObject,
          DataArrayName, FHorizontalAnisotropy[LayerIndex]);
        HorizontalAnisotropyFormula :=
          HorizontalAnisotropyFormula + DataArrayName;
      end
      else
      begin
        HorizontalAnisotropyFormula := HorizontalAnisotropyFormula + '1';
      end;
      if GroupIndex < FModel.LayerStructure.Count - 1 then
      begin
        HorizontalAnisotropyFormula := HorizontalAnisotropyFormula + ', ';
      end;
    end;
    if FModel.LayerStructure.Count > 2 then
    begin
      HorizontalAnisotropyFormula := HorizontalAnisotropyFormula + ')';
    end
  end;
  DataArray := FModel.GetDataSetByName(rsHorizontalAnisotropy);
  DataArray.Formula := HorizontalAnisotropyFormula;
end;

procedure TLpfImporter.ImportDataSet3;
var
  LayerIndex: Integer;
  Index: Integer;
  LayerGroup: TLayerGroup;
begin
  // Data set 3.
  LayerIndex := -1;
  for Index := 1 to FModel.LayerStructure.Count - 1 do
  begin
    LayerGroup := FModel.LayerStructure.LayerGroups[Index];
    if LayerGroup.Simulated then
    begin
      Inc(LayerIndex);
    end;
    Assert(LayerIndex >= 0);
    LayerGroup.InterblockTransmissivityMethod := LAYAVG[LayerIndex];
  end;
end;

procedure TLpfImporter.ImportDataSet2;
var
  LayerIndex: Integer;
  Index: Integer;
  LayerGroup: TLayerGroup;
begin
  // Data set 2.
  LayerIndex := -1;
  for Index := 1 to FModel.LayerStructure.Count - 1 do
  begin
    LayerGroup := FModel.LayerStructure.LayerGroups[Index];
    if LayerGroup.Simulated then
    begin
      Inc(LayerIndex);
    end;
    if LayerIndex >= 0 then
    begin
      if LAYTYP[LayerIndex] > 0 then
      begin
        LayerGroup.AquiferType := 1;
      end
      else if LAYTYP[LayerIndex] = 0 then
      begin
        LayerGroup.AquiferType := 0;
      end
      else
      begin
        if FComputeThicknessUsingStartingHead then
        begin
          LayerGroup.AquiferType := 0;
        end
        else
        begin
          LayerGroup.AquiferType := 1;
        end;
      end;
    end;
  end;
  FModel.CreateInitialDataSets;
end;

procedure TLpfImporter.ImportDataSet1(LpfPackage: TLpfSelection);
begin
  // Data set 1
  FModel.ModflowOptions.HDry := HDRY;
  LpfPackage.UseConstantCV := FComputeVkUsingCellThickness;
  LpfPackage.UseSaturatedThickness := FComputeThicknessUsingStartingHead;
  LpfPackage.UseCvCorrection := not FNoVerticalFlowCorrection;
end;

procedure TArrayImporter.InitializeConstArray(
  ConstArray: TRealConstantRecordArray);
var
  Index: Integer;
begin
  for Index := 0 to Length(ConstArray) - 1 do
  begin
    ConstArray[Index].IsConstant := False;
    ConstArray[Index].RealValue := 0;
  end;
end;

procedure TArrayImporter.InitializeConstIntArray(
  ConstArray: TIntegerConstantArray);
var
  Index: Integer;
begin
  for Index := 0 to Length(ConstArray) - 1 do
  begin
    ConstArray[Index].IsConstant := False;
    ConstArray[Index].IntegerValue := 0;
  end;
end;

procedure TLpfImporter.ReadDataSets10to16Constant;
var
  ConstArray: TRealConstantRecordArray;
  Layer: Integer;
  ID: string;
  Value: double;
begin
  ReadLn(FImporter.FFile, ID);
  ID := Trim(ID);
  ReadLn(FImporter.FFile, Layer);
  Readln(FImporter.FFile, Value);
  Dec(Layer);
  if ID = StrHydCondAlongRows then
  begin
    if FHkConst = nil then
    begin
      SetLength(FHkConst, FModel.LayerStructure.ModflowLayerCount);
      InitializeConstArray(FHkConst);
    end;
    ConstArray := FHkConst;
  end
  else if ID = StrHorizAnisotropy then
  begin
    if FHorizontalAnisotropyConst = nil then
    begin
      SetLength(FHorizontalAnisotropyConst,
        FModel.LayerStructure.ModflowLayerCount);
      InitializeConstArray(FHorizontalAnisotropyConst);
    end;
    ConstArray := FHorizontalAnisotropyConst;
  end
  else if ID = StrVerticalHydCond then
  begin
    if FVerticalKConst = nil then
    begin
      SetLength(FVerticalKConst, FModel.LayerStructure.ModflowLayerCount);
      InitializeConstArray(FVerticalKConst);
    end;
    ConstArray := FVerticalKConst;
  end
  else if ID = StrHorizToVertAnis then
  begin
    if FHorizontalToVerticalAnisotropyConst = nil then
    begin
      SetLength(FHorizontalToVerticalAnisotropyConst,
        FModel.LayerStructure.ModflowLayerCount);
      InitializeConstArray(FHorizontalToVerticalAnisotropyConst);
    end;
    ConstArray := FHorizontalToVerticalAnisotropyConst;
  end
  else if ID = StrQuasi3DVertHydCond then
  begin
    if FQuasiVerticalKConst = nil then
    begin
      SetLength(FQuasiVerticalKConst, FModel.LayerStructure.ModflowLayerCount);
      InitializeConstArray(FQuasiVerticalKConst);
    end;
    ConstArray := FQuasiVerticalKConst;
  end
  else if ID = StrSpecificStorage then
  begin
    if FSpecificStorageConst = nil then
    begin
      SetLength(FSpecificStorageConst, FModel.LayerStructure.ModflowLayerCount);
      InitializeConstArray(FSpecificStorageConst);
    end;
    ConstArray := FSpecificStorageConst;
  end
  else if ID = StrSpecificYield then
  begin
    if FSpecificYieldConst = nil then
    begin
      SetLength(FSpecificYieldConst, FModel.LayerStructure.ModflowLayerCount);
      InitializeConstArray(FSpecificYieldConst);
    end;
    ConstArray := FSpecificYieldConst;
  end
  else if ID = StrWetDry then
  begin
    if FWetDryConst = nil then
    begin
      SetLength(FWetDryConst, FModel.LayerStructure.ModflowLayerCount);
      InitializeConstArray(FWetDryConst);
    end;
    ConstArray := FWetDryConst;
  end
  else if ID = StrStorageCoef then
  begin
    if FStorageCoefficientConst = nil then
    begin
      SetLength(FStorageCoefficientConst,
        FModel.LayerStructure.ModflowLayerCount);
      InitializeConstArray(FStorageCoefficientConst);
    end;
    ConstArray := FStorageCoefficientConst;
  end
  else
  begin
    Assert(False);
  end;
  ConstArray[Layer].IsConstant := True;
  ConstArray[Layer].RealValue := Value;
end;

procedure TLpfImporter.ReadDataSets10to16Variable;
var
  ThreeDArray: T3DDoubleArray;
  Layer: Integer;
  ID: string;
begin
  ReadLn(FImporter.FFile, ID);
  ID := Trim(ID);
  ReadLn(FImporter.FFile, Layer);
  Dec(Layer);
  if ID = StrHydCondAlongRows then
  begin
    if FHk = nil then
    begin
      SetLength(FHk, FModel.LayerStructure.ModflowLayerCount);
    end;
    ThreeDArray := FHk;
  end
  else if ID = StrHorizAnisotropy then
  begin
    if FHorizontalAnisotropy = nil then
    begin
      SetLength(FHorizontalAnisotropy,
        FModel.LayerStructure.ModflowLayerCount);
    end;
    ThreeDArray := FHorizontalAnisotropy;
  end
  else if ID = StrVerticalHydCond then
  begin
    if FVerticalK = nil then
    begin
      SetLength(FVerticalK, FModel.LayerStructure.ModflowLayerCount);
    end;
    ThreeDArray := FVerticalK;
  end
  else if ID = StrHorizToVertAnis then
  begin
    if FHorizontalToVerticalAnisotropy = nil then
    begin
      SetLength(FHorizontalToVerticalAnisotropy,
        FModel.LayerStructure.ModflowLayerCount);
    end;
    ThreeDArray := FHorizontalToVerticalAnisotropy;
  end
  else if ID = StrQuasi3DVertHydCond then
  begin
    if FQuasiVerticalK = nil then
    begin
      SetLength(FQuasiVerticalK, FModel.LayerStructure.ModflowLayerCount);
    end;
    ThreeDArray := FQuasiVerticalK;
  end
  else if ID = StrSpecificStorage then
  begin
    if FSpecificStorage = nil then
    begin
      SetLength(FSpecificStorage, FModel.LayerStructure.ModflowLayerCount);
    end;
    ThreeDArray := FSpecificStorage;
  end
  else if ID = StrSpecificYield then
  begin
    if FSpecificYield = nil then
    begin
      SetLength(FSpecificYield, FModel.LayerStructure.ModflowLayerCount);
    end;
    ThreeDArray := FSpecificYield;
  end
  else if ID = StrWetDry then
  begin
    if FWetDry = nil then
    begin
      SetLength(FWetDry, FModel.LayerStructure.ModflowLayerCount);
    end;
    ThreeDArray := FWetDry;
  end
  else if ID = StrStorageCoef then
  begin
    if FStorageCoefficient = nil then
    begin
      SetLength(FStorageCoefficient, FModel.LayerStructure.ModflowLayerCount);
    end;
    ThreeDArray := FStorageCoefficient;
  end
  else
  begin
    Assert(False);
  end;
  if ThreeDArray[Layer] = nil then
  begin
    SetLength(ThreeDArray[Layer], FGrid.RowCount, FGrid.ColumnCount);
  end;
  Read2DRealArray(ThreeDArray[Layer]);
end;

procedure TLpfImporter.ReadDataSet7;
begin
  Read(FImporter.FFile, WETFCT);
  Read(FImporter.FFile, IWETIT);
  Read(FImporter.FFile, IHDWET);
  Readln(FImporter.FFile);
end;

procedure TLpfImporter.ReadDataSets2to6;
var
  NLAY: Integer;
  K: Integer;
begin
  if (LAYTYP = nil) or (LAYAVG = nil) or (CHANI = nil)
    or (LAYVKA = nil) or (LAYWET = nil) then
  begin
    NLAY := FModel.LayerStructure.ModflowLayerCount;
    if (LAYTYP = nil) then
    begin
      SetLength(LAYTYP, NLAY);
    end;
    if (LAYAVG = nil) then
    begin
      SetLength(LAYAVG, NLAY);
    end;
    if (CHANI = nil) then
    begin
      SetLength(CHANI, NLAY);
    end;
    if (LAYVKA = nil) then
    begin
      SetLength(LAYVKA, NLAY);
    end;
    if (LAYWET = nil) then
    begin
      SetLength(LAYWET, NLAY);
    end;
  end;
  Read(FImporter.FFile, K);
  Dec(K);
  Read(FImporter.FFile, LAYTYP[K]);
  Read(FImporter.FFile, LAYAVG[K]);
  Read(FImporter.FFile, CHANI[K]);
  Read(FImporter.FFile, LAYVKA[K]);
  Read(FImporter.FFile, LAYWET[K]);
  ReadLn(FImporter.FFile);
end;

procedure TLpfImporter.ReadDataSet1Options;
var
  ITHFLG: Integer;
  ICONCV: Integer;
  ISFAC: Integer;
  NOCVCO: Integer;
begin
  Read(FImporter.FFile, ISFAC);
  Read(FImporter.FFile, ICONCV);
  Read(FImporter.FFile, ITHFLG);
  Read(FImporter.FFile, NOCVCO);
  Readln(FImporter.FFile);
  FStorageCoefficientChoice := ISFAC <> 0;
  FComputeVkUsingCellThickness := ICONCV <> 0;
  FComputeThicknessUsingStartingHead := ITHFLG <> 0;
  FNoVerticalFlowCorrection := NOCVCO <> 0;
end;

procedure TLpfImporter.ReadDataSet1;
var
  NPLPF: Integer;
begin
  FIsSelected := True;
  Read(FImporter.FFile, ILPFCB);
  Read(FImporter.FFile, HDRY);
  Read(FImporter.FFile, NPLPF);
  Readln(FImporter.FFile);
  SetLength(FParameters, NPLPF);
  FNextParameterIndex := 0;
end;

procedure TLpfImporter.HandlePackage;
var
  LpfPackage: TLpfSelection;
begin
  inherited;

  LpfPackage := FModel.ModflowPackages.LpfPackage;
  LpfPackage.IsSelected := FIsSelected;
  if FIsSelected then
  begin
    FModel.ModflowPackages.HufPackage.IsSelected := False;
    FModel.ModflowPackages.BcfPackage.IsSelected := False;
    LpfPackage.Comments := FComments;
    ImportDataSet1(LpfPackage);
    ImportDataSet2;
    ImportDataSet3;
    ImportHorizontalAnisotropy;
    ImportDataSet5;
    ImportDataSet6;
    ImportDataSet7;
    ImportLpfParameters;
    FModel.CreateInitialDataSets;

    ImportHorizontalHydraulicConductivity;
    ImportVerticalHydraulicConductivity;
    ImportSpecificStorage;
    ImportSpecificYield;
    ImportWetDry;

    FModel.UpdateDataArrayParameterUsed;

  end;
end;

procedure TLpfImporter.ReadData(const ALabel: string);
begin
  inherited;
  if ALabel = 'ILPFCB, HDRY, NPLPF:' then
  begin
    ReadDataSet1;
  end
  else if ALabel = 'ISFAC, ICONCV, ITHFLG, NOCVCO:' then
  begin
    ReadDataSet1Options;
  end
  else if ALabel = 'K,LAYTYP(K),LAYAVG(K),CHANI(K),LAYVKA(K),LAYWET(K):' then
  begin
    ReadDataSets2to6;
  end
  else if ALabel = 'WETFCT,IWETIT,IHDWET:' then
  begin
    ReadDataSet7;
  end
  else if ALabel = 'PARNAM:' then
  begin
    ReadArrayParameter;
  end
  else if ALabel = StrVariable2DRealArrayForLayer then
  begin
    ReadDataSets10to16Variable;
  end
  else if ALabel = StrConstant2DRealArrayForLayer then
  begin
    ReadDataSets10to16Constant;
  end
  else
  begin
    Assert(False);
  end
end;


procedure TArrayParameterImporter.ReadArrayParameter;
var
  Index: Integer;
  NumberOfZones: Integer;
  ZoneName: string;
  MultiplierName: string;
  Layer: Integer;
  InstanceIndex: Integer;
  Limit: Integer;
  NUMINST: Integer;
  NCLU: Integer;
  PARTYP: string;
  ID: string;
  ClusterIndex: Integer;
  PARNAM: string;
begin
  Readln(FImporter.FFile, PARNAM);
  FParameters[FNextParameterIndex].PARNAM := Trim(PARNAM);

  ReadLn(FImporter.FFile, ID);
  ID := Trim(ID);
  Assert((ID = 'PTYP:') or (ID = 'PARTYP:'));
  ReadLn(FImporter.FFile, PARTYP);
  FParameters[FNextParameterIndex].PARTYP := Trim(UpperCase(PARTYP));

  ReadLn(FImporter.FFile, ID);
  ID := Trim(ID);
  Assert(ID = 'Parval:');
  ReadLn(FImporter.FFile, FParameters[FNextParameterIndex].Parval);

  ReadLn(FImporter.FFile, ID);
  ID := Trim(ID);
  Assert(ID = 'NCLU:');
  ReadLn(FImporter.FFile, NCLU);

  if ReadInstance then
  begin
    ReadLn(FImporter.FFile, ID);
    ID := Trim(ID);
    Assert(ID = 'NUMINST:');
    ReadLn(FImporter.FFile, NUMINST);
  end
  else
  begin
    NUMINST := 0;
  end;

  if NUMINST > 0 then
  begin
    Limit := NUMINST;
  end
  else
  begin
    Limit := 1;
  end;
  SetLength(FParameters[FNextParameterIndex].Instances,Limit);

  for InstanceIndex := 0 to Limit-1 do
  begin
    SetLength(FParameters[FNextParameterIndex].
      Instances[InstanceIndex].Clusters, NCLU);
    if NUMINST > 0 then
    begin
      ReadLn(FImporter.FFile, ID);
      ID := Trim(ID);
      Assert(ID = 'INAME(ILOC):');
      ReadLn(FImporter.FFile, FParameters[FNextParameterIndex].
        Instances[InstanceIndex].InstanceName);
    end
    else
    begin
      FParameters[FNextParameterIndex].
        Instances[InstanceIndex].InstanceName := '';
    end;
    for ClusterIndex := 0 to NCLU - 1 do
    begin
      ReadLn(FImporter.FFile, ID);
      ID := Trim(ID);
      Layer := -1;
      if ID = 'MULTIPLIER ARRAY AND ZONE ARRAY FOR A LAYER:' then
      begin
        ReadLn(FImporter.FFile, Layer);
      end
      else if ID = 'MULTIPLIER ARRAY AND ZONE ARRAY:' then
      begin
        Layer := 0;
      end
      else if ID = 'HGUNAM:' then
      begin
        ReadLn(FImporter.FFile, Layer);
      end
      else if ID = 'LAYER:' then
      begin
        ReadLn(FImporter.FFile, Layer);
      end
      else
      begin
        Assert(False);
      end;
      FParameters[FNextParameterIndex].
        Instances[InstanceIndex].Clusters[ClusterIndex].Layer := Layer;

      ReadLn(FImporter.FFile, MultiplierName);
      MultiplierName := FixArrayName(Trim(MultiplierName));
      FParameters[FNextParameterIndex].Instances[InstanceIndex].
        Clusters[ClusterIndex].MultiplierName := MultiplierName;

      ReadLn(FImporter.FFile, ZoneName);
      ZoneName := FixArrayName(Trim(ZoneName));
      FParameters[FNextParameterIndex].Instances[InstanceIndex].
        Clusters[ClusterIndex].ZoneName := ZoneName;

      if UpperCase(ZoneName) <> 'ALL' then
      begin
        ReadLn(FImporter.FFile, ID);
        ID := Trim(ID);
        Assert(ID = 'NUMBER OF ZONES:');
        ReadLn(FImporter.FFile, NumberOfZones);
        SetLength(FParameters[FNextParameterIndex].Instances[InstanceIndex].
          Clusters[ClusterIndex].ZoneValues, NumberOfZones);

        ReadLn(FImporter.FFile, ID);
        ID := Trim(ID);
        Assert(ID = 'ZONE VALUES:');
        for Index := 0 to NumberOfZones - 1 do
        begin
          Read(FImporter.FFile, FParameters[FNextParameterIndex].
            Instances[InstanceIndex].Clusters[ClusterIndex].ZoneValues[Index]);
        end;
        Readln(FImporter.FFile);
      end;
    end;
  end;
  Inc(FNextParameterIndex);
end;

function TArrayParameterImporter.ReadInstance: boolean;
begin
  result := True;
end;

procedure TPackageImporter.CreateDataArrayAndAssignValues(
  ScreenObject: TScreenObject; const DataArrayName: string;
  ImportedValues: T2DDoubleArray);
var
  DataArray: TDataArray; 
  Interpolator: TNearestPoint2DInterpolator;
begin
  DataArray := FModel.GetDataSetByName(DataArrayName);
  if DataArray = nil then
  begin
    DataArray := FModel.CreateNewDataArray(TDataArray, DataArrayName, '0',
      [], rdtDouble, eaBlocks, dsoTop, '');

    DataArray.UpdateDimensions(FGrid.LayerCount, FGrid.RowCount,
      FGrid.ColumnCount);

    Interpolator := TNearestPoint2DInterpolator.Create(nil);
    try
      DataArray.TwoDInterpolator := Interpolator;
    finally
      Interpolator.Free;
    end;
  end;
  AssignRealValuesToCellCenters(DataArray, ScreenObject, ImportedValues);
end;

procedure TPackageImporter.CheckRealConstArray(out ConstantValue: Double;
      out IsConstant: Boolean; const ArrayToCheck: TRealConstantRecordArray);
var
  Index: Integer;
begin
  IsConstant := ArrayToCheck <> nil;
  if IsConstant then
  begin
    ConstantValue := ArrayToCheck[0].RealValue;
    for Index := 0 to Length(ArrayToCheck) - 1 do
    begin
      IsConstant := ArrayToCheck[Index].IsConstant
        and (ArrayToCheck[Index].RealValue = ConstantValue);
      if not IsConstant then
      begin
        break;
      end;
    end;
  end
  else
  begin
    ConstantValue := 0;
  end;
end;

{ TPcgImporter }

constructor TPcgImporter.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'PCG:');
end;

procedure TPcgImporter.HandlePackage;
var
  PcgPackage: TPcgSelection;
begin
  inherited;

  PcgPackage := FModel.ModflowPackages.PcgPackage;
  PcgPackage.IsSelected := True;
  PcgPackage.Comments := FComments;

  PcgPackage.MXITER := MXITER;
  PcgPackage.ITER1 := ITER1;
  PcgPackage.MXITER := MXITER;

  PcgPackage.HCLOSE.Value := HCLOSEPCG;
  PcgPackage.RCLOSE.Value := RCLOSEPCG;
  PcgPackage.RELAX.Value := RELAXPCG;
  if NBPOL = 2 then
  begin
    PcgPackage.NBPOL := peeDontEstimate;
  end
  else
  begin
    PcgPackage.NBPOL := peeEstimate;
  end;
  PcgPackage.IPRPCG := IPRPCG;
  case MUTPCG of
    0:PcgPackage.MUTPCG := ppsAll;
    1:PcgPackage.MUTPCG := ppsIterations;
    2:PcgPackage.MUTPCG := ppsNone;
    3:PcgPackage.MUTPCG := ppsFail;
    else PcgPackage.MUTPCG := ppsAll;
  end;
  PcgPackage.DAMPPCG.Value := DAMPPCG;
  PcgPackage.DAMPPCGT.Value := DAMPPCGT;
end;

procedure TPcgImporter.ReadDataSet2;
begin
  Read(FImporter.FFile, HCLOSEPCG);
  Read(FImporter.FFile, RCLOSEPCG);
  Read(FImporter.FFile, RELAXPCG);
  Read(FImporter.FFile, NBPOL);
  Read(FImporter.FFile, IPRPCG);
  Read(FImporter.FFile, MUTPCG);
  Read(FImporter.FFile, DAMPPCG);
  Read(FImporter.FFile, DAMPPCGT);
  ReadLn(FImporter.FFile);
end;

procedure TPcgImporter.ReadDataSet1;
begin
  Read(FImporter.FFile, MXITER);
  Read(FImporter.FFile, ITER1);
  Read(FImporter.FFile, NPCOND);
  ReadLn(FImporter.FFile);
end;

procedure TPcgImporter.ReadData(const ALabel: string);
begin
  inherited;
  if ALabel = 'MXITER, ITER1, NPCOND:' then
  begin
    ReadDataSet1;
  end
  else
  begin if ALabel =
    'HCLOSEPCG,RCLOSEPCG,RELAXPCG,NBPOL,IPRPCG,MUTPCG,DAMPPCG:' then
  begin
      ReadDataSet2;
  end
  else
    Assert(False);
  end;
end;

{ TGmgImporter }

constructor TGmgImporter.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'GMG:');
end;

procedure TGmgImporter.HandlePackage;
begin
  inherited;
  FGmgPackage := FModel.ModflowPackages.GmgPackage;
  FGmgPackage.IsSelected := True;
  FGmgPackage.Comments := FComments;
  ImportDataSet1;
  ImportDataset2;
  ImportDataSet3;
  ImportDataSet4;
end;

procedure TGmgImporter.ImportDataSet4;
begin
  // Data set 4
  if ISC = 4 then
  begin
    FGmgPackage.RELAX.Value := RELAX;
  end;
end;

procedure TGmgImporter.ImportDataSet3;
begin
  // Data set 3
  FGmgPackage.ISM := ISM;
  FGmgPackage.ISC := ISC;
  if IADAMP = 2 then
  begin
    FGmgPackage.DUP.Value := DUP;
    FGmgPackage.DLOW.Value := DLOW;
    FGmgPackage.CHGLIMIT.Value := CHGLIMIT;
  end;
end;

procedure TGmgImporter.ImportDataset2;
begin
  // Data set 2
  FGmgPackage.DAMP.Value := DAMP;
  FGmgPackage.IADAMP := IADAMP;
  FGmgPackage.IOUTGMG := IOUTGMG;
  FGmgPackage.IUNITMHC := IUNITMHC > 0;
end;

procedure TGmgImporter.ImportDataSet1;
begin
  // Data set 1
  FGmgPackage.RCLOSE.Value := RCLOSE;
  FGmgPackage.IITER := IITER;
  FGmgPackage.HCLOSE.Value := HCLOSE;
  FGmgPackage.MXITER := MXITER;
end;

procedure TGmgImporter.ReadDataSet4;
begin
  Read(FImporter.FFile, RELAX);
  ReadLn(FImporter.FFile);
end;

procedure TGmgImporter.ReadFullDataSet3;
begin
  Read(FImporter.FFile, ISM);
  Read(FImporter.FFile, ISC);
  Read(FImporter.FFile, DUP);
  Read(FImporter.FFile, DLOW);
  Read(FImporter.FFile, CHGLIMIT);
  ReadLn(FImporter.FFile);
end;

procedure TGmgImporter.ReadPartialDataSet3;
begin
  Read(FImporter.FFile, ISM);
  Read(FImporter.FFile, ISC);
  ReadLn(FImporter.FFile);
end;

procedure TGmgImporter.ReadDataSet2;
begin
  Read(FImporter.FFile, DAMP);
  Read(FImporter.FFile, IADAMP);
  Read(FImporter.FFile, IOUTGMG);
  Read(FImporter.FFile, IUNITMHC);
  ReadLn(FImporter.FFile);
end;

procedure TGmgImporter.ReadDataSet1;
begin
  Read(FImporter.FFile, RCLOSE);
  Read(FImporter.FFile, IITER);
  Read(FImporter.FFile, HCLOSE);
  Read(FImporter.FFile, MXITER);
  ReadLn(FImporter.FFile);
end;

procedure TGmgImporter.ReadData(const ALabel: string);
begin
  inherited;
  if ALabel = 'RCLOSEGMG,IITER,HCLOSEGMG,MXITER:' then
  begin
    ReadDataSet1;
  end
  else if ALabel = 'DAMP, IADAMP, IOUTGMG, IUNITMHC:' then
  begin
    ReadDataSet2;
  end
  else if ALabel = 'ISM,ISC:' then
  begin
    ReadPartialDataSet3;
  end
  else if ALabel = 'ISM,ISC,DUP,DLOW,CHGLIMIT:' then
  begin
    ReadFullDataSet3;
  end
  else if ALabel = 'RELAX:' then
  begin
    ReadDataSet4;
  end
  else
  begin
    Assert(False);
  end;
end;

{ TSipImporter }

constructor TSipImporter.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'SIP:');
end;

procedure TSipImporter.ReadDataSet2;
begin
  Read(FImporter.FFile, ACCL);
  Read(FImporter.FFile, HCLOSE);
  Read(FImporter.FFile, IPCALC);
  Read(FImporter.FFile, WSEED);
  Read(FImporter.FFile, IPRSIP);
  ReadLn(FImporter.FFile);
end;

procedure TSipImporter.ReadDataSet1;
begin
  Read(FImporter.FFile, MXITER);
  Read(FImporter.FFile, NPARM);
  ReadLn(FImporter.FFile);
end;

procedure TSipImporter.HandlePackage;
begin
  inherited;
  FSipPackage := FModel.ModflowPackages.SipPackage;
  FSipPackage.IsSelected := True;
  FSipPackage.Comments := FComments;

  // data set 1
  FSipPackage.MXITER := MXITER;
  FSipPackage.NPARM := NPARM;

  // data set 2
  FSipPackage.ACCL.Value := ACCL;
  FSipPackage.HCLOSE.Value := HCLOSE;
  FSipPackage.IPCALC := IPCALC;
  FSipPackage.WSEED.Value := WSEED;
  FSipPackage.IPRSIP := IPRSIP;
end;

procedure TSipImporter.ReadData(const ALabel: string);
begin
  inherited;
  if ALabel = 'MXITER,NPARM:' then
  begin
    ReadDataSet1;
  end
  else if ALabel = 'ACCL,HCLOSE,IPCALC,WSEED,IPRSIP:' then
  begin
    ReadDataSet2;
  end
  else
  begin
    Assert(False);
  end;
end;

{ TDe4Importer }

constructor TDe4Importer.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'DE4:');
end;

procedure TDe4Importer.ReadDataSet2;
begin
  Read(FImporter.FFile, IFREQ);
  Read(FImporter.FFile, MUTD4);
  Read(FImporter.FFile, ACCL);
  Read(FImporter.FFile, HCLOSE);
  Read(FImporter.FFile, IPRD4);
  ReadLn(FImporter.FFile);
  if IFREQ < 1 then
  begin
    IFREQ := 1;
  end
  else if IFREQ > 3 then
  begin
    IFREQ := 3;
  end;
  if MUTD4 < 0 then
  begin
    MUTD4 := 0;
  end
  else if MUTD4 > 2 then
  begin
    MUTD4 := 2;
  end;
end;

procedure TDe4Importer.ReadDataSet1;
begin
  Read(FImporter.FFile, ITMX);
  Read(FImporter.FFile, MXUP);
  Read(FImporter.FFile, MXLOW);
  Read(FImporter.FFile, MXBW);
  ReadLn(FImporter.FFile);
end;

procedure TDe4Importer.HandlePackage;
begin
  inherited;
  FDe4Package := FModel.ModflowPackages.De4Package;
  FDe4Package.IsSelected := True;
  FDe4Package.Comments := FComments;

  // data set 1.
  FDe4Package.ITMX := ITMX;
  FDe4Package.MXUP := MXUP;
  FDe4Package.MXLOW := MXLOW;
  FDe4Package.MXBW := MXBW;

  // data set 2.
  FDe4Package.IFREQ := IFREQ;
  FDe4Package.MUTD4 := MUTD4;
  FDe4Package.ACCL.Value := ACCL;
  FDe4Package.HCLOSE.Value := HCLOSE;
  FDe4Package.IPRD4 := IPRD4;
end;

procedure TDe4Importer.ReadData(const ALabel: string);
begin
  inherited;
  if ALabel = 'ITMX, MXUP, MXLOW, MXBW:' then
  begin
    ReadDataSet1;
  end
  else if ALabel = 'IFREQ,MUTD4,ACCLDE4,HCLOSEDE4,IPRD4:' then
  begin
    ReadDataSet2;
  end
  else
  begin
    Assert(False);
  end;
end;

{ TChdImporter }

constructor TChdImporter.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'CHD:');
  FStressPeriods:= TChdStressPeriodArray.Create;
  FParameters:= TChdParamArray.Create;
  FReadData := False;
end;

function TChdImporter.CreateScreenObject(List: TList;
  var ScreenObjectIndex: Integer; LayerIndex,
  StressPeriodIndex: integer): TScreenObject;
var
  SpecifiedHeadLocations: TDataArray;
  Position: Integer;
begin
  result := inherited CreateScreenObject(List,
    ScreenObjectIndex, LayerIndex, StressPeriodIndex);
  result.CreateChdBoundary;
  SpecifiedHeadLocations := FModel.GetDataSetByName(rsModflowSpecifiedHead);
  Assert(SpecifiedHeadLocations <> nil);
  Position := result.AddDataSet(SpecifiedHeadLocations);
  result.DataSetFormulas[Position] := 'True';
end;

destructor TChdImporter.Destroy;
begin
  FStressPeriods.Free;
  FParameters.Free;
  inherited;
end;

function TCustomChdImporter.GetBoundary(
  ScreenObject: TScreenObject): TModflowParamBoundary;
begin
  // Assign values to the TScreenObject
  // for the current stress period(s).
  result := ScreenObject.ModflowChdBoundary;
end;

procedure TChdImporter.ReadNonParameterLocations;
var
  BoundaryIndex: Integer;
  Index: Integer;
  LAYER: integer;
  Row: integer;
  Column: integer;
  StartFactor: double;
  EndFactor: double;
  AuxVar: double;
  ChdBoundary: TChdLocationObject;
begin
  for BoundaryIndex := 0 to ITMP - 1 do
  begin
    ChdBoundary := FStressPeriods[FCurrentStressPeriod].
      Boundaries[CurrentInstance] as TChdLocationObject;
    Read(FImporter.FFile, LAYER);
    ChdBoundary.Layer := LAYER;
    Read(FImporter.FFile, Row);
    ChdBoundary.Row := Row;
    Read(FImporter.FFile, Column);
    ChdBoundary.Column := Column;
    Read(FImporter.FFile, StartFactor);
    ChdBoundary.StartFactor := StartFactor;
    Read(FImporter.FFile, EndFactor);
    ChdBoundary.EndFactor := EndFactor;
    for Index := 0 to FAuxillaryVariables.Count - 1 do
    begin
      Read(FImporter.FFile, AuxVar);
      ChdBoundary.AuxilliaryVariables[Index] := AuxVar;
    end;
    Inc(CurrentInstance);
  end;
end;

procedure TChdImporter.ReadParameterLocations;
var
  Index: Integer;
  BoundaryIndex: Integer;
  LAYER: integer;
  Row: integer;
  Column: integer;
  StartFactor: double;
  EndFactor: double;
  AuxVar: double;
  ChdLocation: TChdLocationObject;
begin
  for BoundaryIndex := 0 to NLST - 1 do
  begin
    ChdLocation := FParameters[CurrentParameter].Instances[CurrentInstance].
      Locations[BoundaryIndex] as TChdLocationObject;
    Read(FImporter.FFile, LAYER);
    ChdLocation.Layer := LAYER;
    Read(FImporter.FFile, Row);
    ChdLocation.Row := Row;
    Read(FImporter.FFile, Column);
    ChdLocation.Column := Column;
    Read(FImporter.FFile, StartFactor);
    ChdLocation.StartFactor := StartFactor;
    Read(FImporter.FFile, EndFactor);
    ChdLocation.EndFactor := EndFactor;
    for Index := 0 to FAuxillaryVariables.Count - 1 do
    begin
      Read(FImporter.FFile, AuxVar);
      ChdLocation.AuxilliaryVariables[Index] := AuxVar;
    end;
    ReadLn(FImporter.FFile);
  end;
  Inc(CurrentInstance);
end;

procedure TChdImporter.ReadParameterType;
var
  PARTYP: string;
begin
  Readln(FImporter.FFile, PARTYP);
  PARTYP := Trim(PARTYP);
  PARTYP := UpperCase(PARTYP);
  Assert(PARTYP = 'CHD');
  FParameters[CurrentParameter].PARTYP := PARTYP;
end;

procedure TChdImporter.ReadDataSet2;
begin
  FReadData := True;
  Readln(FImporter.FFile, MXACTC);
end;

function TChdImporter.ScreenObjectNameRoot: string;
begin
  result := 'Imported_CHD_';
end;

procedure TChdImporter.HandlePackage;
var
  ScreenObjectIndex: integer;
begin
  if (FCurrentStressPeriod < 0) or
    (FCurrentStressPeriod < FStressPeriods.ArrayLength-1) then
  begin
    Exit;
  end;
  inherited;
  FChdPackage := FModel.ModflowPackages.ChdBoundary;
  FChdPackage.IsSelected := True;
  FChdPackage.Comments := FComments;
  FModel.CreateInitialDataSets;
  AssignObservations;

  ScreenObjectIndex := 0;
  ImportNonParameterBoundaries(ScreenObjectIndex);
  ImportParameterBoundaries(ScreenObjectIndex);
  if FObsImporter <> nil then
  begin
    FObsImporter.HandlePackage;
  end;
  ReleaseMemory;
end;

procedure TChdImporter.ReadData(const ALabel: string);
begin
  inherited;
  if FCurrentStressPeriod = -1 then
  begin
    if ALabel = 'MXACTC:' then
    begin
      ReadDataSet2;
    end
    else if ALabel = 'NP,MXL:' then
    begin
      ReadDataSet1;
    end
    else if ALabel = 'CHDAUX(NAUX):' then
    begin
      ReadAuxilliaryVariableName;
    end
    else if ALabel = 'NOPRINT:' then
    begin
      FNoPrint := True;
    end
    else if ALabel = 'PARNAM:' then
    begin
      ReadParameterName;
    end
    else if ALabel = 'PARTYP:' then
    begin
      ReadParameterType;
    end
    else if ALabel = 'Parval,NLST:' then
    begin
      ReadParameterValueAndLocationCount;
    end
    else if ALabel = 'INSTANCES:' then
    begin
      // do nothing
    end
    else if ALabel = 'NUMINST:' then
    begin
      ReadNumberOfInstances;
    end
    else if ALabel = 'INAME(ILOC):' then
    begin
      ReadInstanceName;
    end
    else if ALabel = 'LAYER,ROW,COL,START_FACTOR,END_FACTOR,CHDAUX:' then
    begin
      ReadParameterLocations;
    end
    else if ALabel = 'ITMP,NP:' then
    begin
      ReadFirstStressPeriodDataSet5WithParameters;
    end
    else if ALabel = 'ITMP:' then
    begin
      ReadFirstStressPeriodDataSet5WithoutParameters;
    end
    else
    begin
      Assert(False);
    end;
  end
  else
  begin
    if ALabel = 'ITMP,NP:' then
    begin
      ReadNewStressPeriodDataSet5WithParameters;
    end
    else if ALabel = 'ITMP:' then
    begin
      ReadNewStressPeriodDataSet5WithoutParameters;
    end
    else if ALabel = 'LAYER,ROW,COL,START_FACTOR,END_FACTOR,CHDAUX:' then
    begin
      ReadNonParameterLocations;
    end
    else if ALabel = 'Pname:' then
    begin
      ReadParamNameForStressPeriod;
    end
    else if ALabel = 'Iname:' then
    begin
      ReadInstanceNameForStressPeriod;
    end
    else
    begin
      Assert(False);
    end;
  end;
end;

function TCustomChdImporter.ParameterType: TParameterType;
begin
  result := ptCHD;
end;

{ TWelImporter }

constructor TWelImporter.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'WEL:');
  FParameters := TWelParamArray.Create;
  FStressPeriods := TWelStressPeriodArray.Create;
end;

function TWelImporter.CreateScreenObject(List: TList;
  var ScreenObjectIndex: Integer; LayerIndex, StressPeriodIndex: integer): TScreenObject;
begin
  result := inherited CreateScreenObject(List, ScreenObjectIndex, LayerIndex, StressPeriodIndex);
  result.CreateWelBoundary;
  result.ModflowWellBoundary.FormulaInterpretation := fiDirect;
end;

destructor TWelImporter.Destroy;
begin
  FParameters.Free;
  FStressPeriods.Free;
  inherited;
end;

function TWelImporter.GetBoundary(
  ScreenObject: TScreenObject): TModflowParamBoundary;
begin
  result := ScreenObject.ModflowWellBoundary;
end;

procedure TWelImporter.HandlePackage;
var
  ScreenObjectIndex: integer;
  WelPackage: TModflowPackageSelection;
begin
  if (FCurrentStressPeriod < 0) or
    (FCurrentStressPeriod < FStressPeriods.ArrayLength -1) then
  begin
    Exit;
  end;
  inherited;
  WelPackage := FModel.ModflowPackages.WelPackage;
  WelPackage.IsSelected := True;
  WelPackage.Comments := FComments;

  ScreenObjectIndex := 0;
  ImportNonParameterBoundaries(ScreenObjectIndex);
  ImportParameterBoundaries(ScreenObjectIndex);
  ReleaseMemory;
end;

function TWelImporter.ScreenObjectNameRoot: string;
begin
  result := 'Imported_Wel_';
end;

procedure TWelImporter.SetItemValues(Item: TCustomModflowBoundaryItem;
  Boundaries: TList; EndTime, StartTime: Double; ScreenObject: TScreenObject;
  const ParamName: string);
var
  WelBoundary: TWelLocationObject;
  WelItem: TWellItem;
  ValueItem: TValueArrayItem;
  Index: Integer;
  PumpingValues: TValueArrayStorage;
  PumpName: string;
begin
  ValueItem := ScreenObject.ImportedValues.Add as TValueArrayItem;
  PumpName := 'WelPump' + ParamName;;
  ValueItem.Name := PumpName;
  PumpingValues := ValueItem.Values;
  PumpingValues.DataType := rdtDouble;
  PumpingValues.Count := Boundaries.Count;

  WelItem := Item as TWellItem;
  WelItem.StartTime := StartTime;
  WelItem.EndTime := EndTime;
  for Index := 0 to Boundaries.Count - 1 do
  begin
    WelBoundary := Boundaries[Index];
    PumpingValues.RealValues[Index] := WelBoundary.PumpingRate;
  end;
  WelItem.PumpingRate := rsObjectImportedValuesR + '("' + PumpName + '")';
end;

procedure TWelImporter.ReadNonParameterLocations;
var
  Index: Integer;
  AuxVar: Double;
  BoundaryIndex: Integer;
  LAYER: Integer;
  Row: Integer;
  Column: Integer;
  PumpingRate: Double;
  WelBoundary: TWelLocationObject;
begin
  for BoundaryIndex := 0 to ITMP - 1 do
  begin
    WelBoundary := FStressPeriods[FCurrentStressPeriod].
      Boundaries[CurrentInstance] as TWelLocationObject;
    Read(FImporter.FFile, LAYER);
    WelBoundary.Layer := LAYER;
    Read(FImporter.FFile, Row);
    WelBoundary.Row := Row;
    Read(FImporter.FFile, Column);
    WelBoundary.Column := Column;
    Read(FImporter.FFile, PumpingRate);
    WelBoundary.PumpingRate := PumpingRate;
    for Index := 0 to FAuxillaryVariables.Count - 1 do
    begin
      Read(FImporter.FFile, AuxVar);
      WelBoundary.AuxilliaryVariables[Index] := AuxVar;
    end;
    Inc(CurrentInstance);
  end;
end;

procedure TWelImporter.ReadParameterLocations;
var
  AuxVar: Double;
  BoundaryIndex: Integer;
  LAYER: Integer;
  Row: Integer;
  Column: Integer;
  PumpingRate: Double;
  Index: Integer;
  WellBoundary : TWelLocationObject;
begin
  for BoundaryIndex := 0 to NLST - 1 do
  begin
    WellBoundary := FParameters[CurrentParameter].Instances[CurrentInstance].
      Locations[BoundaryIndex] as TWelLocationObject;
    Read(FImporter.FFile, LAYER);
    WellBoundary.Layer := LAYER;
    Read(FImporter.FFile, Row);
    WellBoundary.Row := Row;
    Read(FImporter.FFile, Column);
    WellBoundary.Column := Column;
    Read(FImporter.FFile, PumpingRate);
    WellBoundary.PumpingRate := PumpingRate;
    for Index := 0 to FAuxillaryVariables.Count - 1 do
    begin
      Read(FImporter.FFile, AuxVar);
      WellBoundary.AuxilliaryVariables[Index] := AuxVar;
    end;
    ReadLn(FImporter.FFile);
  end;
  Inc(CurrentInstance);
end;

procedure TWelImporter.ReadParameterType;
var
  PARTYP: string;
begin
  Readln(FImporter.FFile, PARTYP);
  PARTYP := Trim(PARTYP);
  PARTYP := UpperCase(PARTYP);
  Assert(PARTYP = 'Q');
  FParameters[CurrentParameter].PARTYP := PARTYP;
end;

procedure TWelImporter.ReadDataSet2;
begin
  Read(FImporter.FFile, MXACTW);
  Read(FImporter.FFile, IWELCB);
  Readln(FImporter.FFile);
end;

procedure TWelImporter.ReadData(const ALabel: string);
begin
  inherited;
  if FCurrentStressPeriod = -1 then
  begin
    if ALabel = 'NP,MXL:' then
    begin
      ReadDataSet1;
    end
    else if ALabel = 'MXACTW, IWELCB:' then
    begin
      ReadDataSet2;
    end
    else if ALabel = 'WELAUX(NAUX):' then
    begin
      ReadAuxilliaryVariableName;
    end
    else if ALabel = 'NOPRINT:' then
    begin
      FNoPrint := True;
    end
    else if ALabel = 'PARNAM:' then
    begin
      ReadParameterName;
    end
    else if ALabel = 'PARTYP:' then
    begin
      ReadParameterType;
    end
    else if ALabel = 'Parval,NLST:' then
    begin
      ReadParameterValueAndLocationCount;
    end
    else if ALabel = 'INSTANCES:' then
    begin
      // do nothing
    end
    else if ALabel = 'NUMINST:' then
    begin
      ReadNumberOfInstances;
    end
    else if ALabel = 'INAME(ILOC):' then
    begin
      ReadInstanceName;
    end
    else if ALabel = 'Layer Row Column Qfact [xyz]:' then
    begin
      ReadParameterLocations;
    end
    else if ALabel = 'ITMP,NP:' then
    begin
      ReadFirstStressPeriodDataSet5WithParameters;
    end
    else if ALabel = 'ITMP:' then
    begin
      ReadFirstStressPeriodDataSet5WithoutParameters;
    end
    else
    begin
      Assert(False);
    end;
  end
  else
  begin
    if ALabel = 'ITMP,NP:' then
    begin
      ReadNewStressPeriodDataSet5WithParameters;
    end
    else if ALabel = 'ITMP:' then
    begin
      ReadNewStressPeriodDataSet5WithoutParameters;
    end
    else if ALabel = 'Layer Row Column Q [xyz]:' then
    begin
      ReadNonParameterLocations;
    end
    else if ALabel = 'Pname:' then
    begin
      ReadParamNameForStressPeriod;
    end
    else if ALabel = 'Iname:' then
    begin
      ReadInstanceNameForStressPeriod;
    end
    else
    begin
      Assert(False);
    end;
  end;
end;

function TWelImporter.ParameterType: TParameterType;
begin
  result := ptQ;
end;

{ TWelInstanceRecord }

function TWelInstanceObject.ArrayMemberClass: TArrayMemberClass;
begin
  result := TWelLocationObject;
end;

{ TWelParameterRecord }

function TWelParameterObject.ArrayMemberClass: TArrayMemberClass;
begin
  result := TWelInstanceObject;
end;

{ TWelStressPeriod }

function TWelStressPeriod.ArrayMemberClass: TArrayMemberClass;
begin
  result := TWelLocationObject;
end;

{ TWellParamArray }

function TWelParamArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TWelParameterObject;
end;

{ TWelStressPeriodArray }

function TWelStressPeriodArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TWelStressPeriod;
end;

{ TArrayMember }

constructor TArrayMember.Create;
begin
  inherited;
end;

{ TObjectArray }

destructor TObjectArray.Destroy;
begin
  ArrayLength := 0;
  inherited;
end;

function TObjectArray.GetArrayLength: integer;
begin
  result := Length(FObjects);
end;

function TObjectArray.GetObject(Index: integer): TArrayMember;
begin
  result := FObjects[Index];
end;

procedure TObjectArray.SetArrayLength(const Value: integer);
var
  OldLength: integer;
  Index: Integer;
begin
  OldLength := ArrayLength;
  if Value <> OldLength then
  begin
    if Value > OldLength then
    begin
      SetLength(FObjects, Value);
      for Index := OldLength to Value - 1 do
      begin
        FObjects[Index] := ArrayMemberClass.Create;
      end;
    end
    else
    begin
      for Index := OldLength -1 downto Value do
      begin
        FObjects[Index].Free;
      end;
      SetLength(FObjects, Value);
    end;
  end;
end;

{ TChdInstanceObject }

function TChdInstanceObject.ArrayMemberClass: TArrayMemberClass;
begin
  result := TChdLocationObject;
end;

{ TChdParameterObject }

function TChdParameterObject.ArrayMemberClass: TArrayMemberClass;
begin
  result := TChdInstanceObject;
end;

{ TChdParamArray }

function TChdParamArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TChdParameterObject;
end;

{ TChdStressPeriod }

function TChdStressPeriod.ArrayMemberClass: TArrayMemberClass;
begin
  result := TChdLocationObject;
end;

{ TChdStressPeriodArray }

function TChdStressPeriodArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TChdStressPeriod;
end;

function TListImporter.CreateScreenObject(List: TList;
  var ScreenObjectIndex: Integer; LayerIndex,
  StressPeriodIndex: integer): TScreenObject;
var
  UndoCreateScreenObject: TCustomUndo;
  Index: Integer;
  Boundary: TLocation;
  ImportedElevations: TValueArrayStorage;
  StressPeriodString: string;
  MaxStressPeriodString: string;
  LayerString: string;
  MaxLayerString: string;
begin
  if StressPeriodIndex < 0 then
  begin
    StressPeriodString := '';
  end
  else
  begin
    StressPeriodString := IntToStr(StressPeriodIndex+1);
    MaxStressPeriodString := IntToStr(FModel.ModflowStressPeriods.Count);
    while Length(StressPeriodString) < Length(MaxStressPeriodString) do
    begin
      StressPeriodString := '0' + StressPeriodString;
    end;
    StressPeriodString := 'StressPeriod' + StressPeriodString;
  end;


  LayerString := IntToStr(FModel.LayerStructure.
    ModflowLayerToDataSetLayer(LayerIndex)+1);
  MaxLayerString := IntToStr(FGrid.LayerCount);
  while Length(LayerString) < Length(MaxLayerString) do
  begin
    LayerString := '0' + LayerString;
  end;

  result := TScreenObject.CreateWithViewDirection(FModel, vdTop,
    UndoCreateScreenObject, False);
  Inc(ScreenObjectIndex);
  result.Name := ScreenObjectNameRoot + StressPeriodString
    + '_Layer' + LayerString + '_'
    + IntToStr(ScreenObjectIndex);
  FModel.AddScreenObject(result);
  result.ElevationCount := ecOne;
  result.SetValuesOfIntersectedCells := True;
  result.EvaluatedAt := eaBlocks;
  result.Visible := False;
  result.Capacity := List.Count;
  ImportedElevations := result.ImportedSectionElevations;
  ImportedElevations.DataType := rdtDouble;
  ImportedElevations.Count := List.Count;
  for Index := 0 to List.Count - 1 do
  begin
    Boundary := List[Index];
    result.AddPoint(FImporter.CenterPoints[Boundary.Row - 1,
      Boundary.Column - 1], True);
    ImportedElevations.RealValues[Index] :=
      FGrid.LayerCenter(Boundary.Column - 1, Boundary.Row - 1,
      FModel.LayerStructure.ModflowLayerToDataSetLayer(Boundary.Layer));
  end;
  result.ElevationFormula := rsObjectImportedValuesR
    + '("' + StrImportedElevations + '")';
end;

destructor TListImporter.Destroy;
begin
  FAuxillaryVariables.Free;
  inherited;
end;

procedure TListImporter.AssignObsGroupsToCells(CellList: TList);
var
  CellIndex: Integer;
  Cell: TLocation;
  StartPosition: Integer;
  ObsGroupIndex: Integer;
  ObsGroup: TObservationGroup;
  ObsCellIndex: Integer;
  ObsCell: TFlowObservationLocation;
  Found: Boolean;
begin
  StartPosition := 0;
  for ObsGroupIndex := 0 to FObsImporter.FObservations.ArrayLength - 1 do
  begin
    ObsGroup := FObsImporter.FObservations[ObsGroupIndex];
    for ObsCellIndex := 0 to ObsGroup.FCells.ArrayLength - 1 do
    begin
      ObsCell := ObsGroup.FCells[ObsCellIndex];
      Found := False;
      for CellIndex := StartPosition to CellList.Count - 1 do
      begin
        Cell := CellList[CellIndex];
        if (Cell.Layer = ObsCell.LAYER)
          and (Cell.Row = ObsCell.ROW)
          and (Cell.Column = ObsCell.COLUMN) then
        begin
          Found := True;
          StartPosition := CellIndex + 1;
          Cell.FObservationGroups.Add(ObsGroup);
          Cell.FObservationCells.Add(ObsCell);
          break;
        end;
      end;
      if not Found then
      begin
        for CellIndex := 0 to StartPosition - 1 do
        begin
          Cell := CellList[CellIndex];
          if (Cell.Layer = ObsCell.LAYER)
            and (Cell.Row = ObsCell.ROW)
            and (Cell.Column = ObsCell.COLUMN) then
          begin
            Found := True;
            StartPosition := CellIndex + 1;
            Cell.FObservationGroups.Add(ObsGroup);
            Cell.FObservationCells.Add(ObsCell);
            break;
          end;
        end;
      end;
    end;
  end;
end;

constructor TListImporter.Create(Importer: TModflow2005Importer;
  const PackageIdentifier: string);
begin
  inherited Create(Importer, PackageIdentifier);
  CurrentParameter := -1;
  FCurrentStressPeriod := -1;
  FAuxillaryVariables := TStringList.Create;
end;

{ TInstanceObject }

function TListInstanceObject.GetLocations(Index: integer): TLocation;
begin
  result := TLocation(Objects[Index]);
end;

{ TParameterObject }

function TListParameterObject.GetInstance(Index: integer): TListInstanceObject;
begin
  result := TListInstanceObject(Objects[Index]);
end;

{ TParamArray }

function TListParamArray.GetParamByName(
  const ParamName: string): TListParameterObject;
var
  Index: Integer;
  Param: TListParameterObject;
begin
  result := nil;
  for Index := 0 to ArrayLength - 1 do
  begin
    Param := Params[Index];
    if CompareText(Param.PARNAM, ParamName) = 0 then
    begin
      result := Param;
      Exit;
    end;
  end;
end;

function TListParamArray.GetParams(Index: integer): TListParameterObject;
begin
  result := TListParameterObject(Objects[Index]);
end;

{ TStressPeriod }

function TListStressPeriod.GetBoundary(Index: integer): TLocation;
begin
  result := TLocation(Objects[Index]);
end;

{ TStressPeriodArray }

function TListStressPeriodArray.GetStressPeriod(Index: integer): TListStressPeriod;
begin
  result := (Objects[Index]) as TListStressPeriod;
end;

procedure TListImporter.ImportNonParameterBoundaries(
  var ScreenObjectIndex: Integer);
var
  Item: TCustomModflowBoundaryItem;
  SO_Boundary: TModflowParamBoundary;
  ScreenObject: TScreenObject;
  Boundary, AnotherBoundary: TLocation;
  BoundaryIndex: Integer;
  SP: TListStressPeriod;
  InnerIndex: Integer;
  StressPeriod: TListStressPeriod;
  EndTime: Double;
  StartTime: Double;
  StressPeriodIndex: Integer;
  UsedLocations: array of array of boolean;
  LocationsToUse: TList;
  InnerBoundaryIndex: Integer;
  InstanceCount: integer;
  procedure InitializeTestArray;
  var
    RowIndex: Integer;
    ColIndex: Integer;
  begin
    for RowIndex := 0 to FGrid.RowCount - 1 do
    begin
      for ColIndex := 0 to FGrid.ColumnCount - 1 do
      begin
        UsedLocations[RowIndex,ColIndex] := false;
      end;
    end;
  end;
begin
  InstanceCount := 0;
  SetLength(UsedLocations, FGrid.RowCount, FGrid.ColumnCount);
  LocationsToUse := TList.Create;
  try
    for StressPeriodIndex := 0 to FStressPeriods.ArrayLength - 1 do
    begin
      // initialize the start and end times for when the boundary will be
      // applied.
      StartTime := FModel.ModflowStressPeriods[StressPeriodIndex].StartTime;
      EndTime := FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
      StressPeriod := FStressPeriods[StressPeriodIndex];
      if StressPeriod.Reuse then
      begin
        Continue;
      end
      else
      begin
        // Update the endtime if the boundaries from the current
        // stress period will be reused in subsequent stress periods.
        for InnerIndex := StressPeriodIndex + 1 to
          FStressPeriods.ArrayLength - 1 do
        begin
          SP := FStressPeriods[InnerIndex];
          if SP.Reuse then
          begin
            EndTime := FModel.ModflowStressPeriods[InnerIndex].EndTime;
          end
          else
          begin
            break;
          end;
        end;

        for BoundaryIndex := 0 to StressPeriod.ArrayLength - 1 do
        begin
          Boundary := StressPeriod.Boundaries[BoundaryIndex];
          if Boundary.Used then
          begin
            Continue;
          end;
          InitializeTestArray;
          LocationsToUse.Clear;
          LocationsToUse.Add(Boundary);
          UsedLocations[Boundary.Row-1, Boundary.Column-1] := True;
          Boundary.Used := True;
          for InnerBoundaryIndex := BoundaryIndex+1
            to StressPeriod.ArrayLength - 1 do
          begin
            AnotherBoundary := StressPeriod.Boundaries[InnerBoundaryIndex];
            if not AnotherBoundary.Used
              and (Boundary.Layer = AnotherBoundary.Layer)
              and not UsedLocations[AnotherBoundary.Row-1,
              AnotherBoundary.Column-1]
              and Boundary.SameObservations(AnotherBoundary) then
            begin
              LocationsToUse.Add(AnotherBoundary);
              UsedLocations[AnotherBoundary.Row-1,
                AnotherBoundary.Column-1] := True;
              AnotherBoundary.Used := True;
            end;
          end;
          ScreenObject := CreateScreenObject(LocationsToUse, ScreenObjectIndex,
            Boundary.Layer, StressPeriodIndex);

          // Assign values to the TScreenObject
          // for the current stress period(s).
          SO_Boundary := GetBoundary(ScreenObject);
          Item := SO_Boundary.Values.Add as TCustomModflowBoundaryItem;
          Inc(InstanceCount);
          SetItemValues(Item, LocationsToUse, EndTime, StartTime, ScreenObject,
            IntToStr(InstanceCount));
        end;
      end;
    end;
  finally
    LocationsToUse.Free;
  end;
end;

procedure TListImporter.ImportParameterBoundaries(ScreenObjectIndex: Integer);
var
  Item: TCustomModflowBoundaryItem;
  EndTime: Double;
  LocationIndex: Integer;
  InnerParamIndex: Integer;
  StressPeriod: TListStressPeriod;
  StressPeriodIndex: Integer;
  Instance: TListInstanceObject;
  InstanceIndex: Integer;
  InstanceCount: Integer;
  Instances: TIntegerList;
  UsedStressPeriods: TIntegerList;
  NewParameter: TModflowTransientListParameter;
  Parameter: TListParameterObject;
  ParameterIndex: Integer;
  Parameters: TModflowTransientListParameters;
  StartTime: Double;
  StressPeriodI: Integer;
  Param: TCustomMF_BoundColl;
  ParamItem: TModflowParamItem;
  SO_Boundary: TModflowParamBoundary;
  ScreenObject: TScreenObject;
  Boundary: TLocation;
  UsedLocations: array of array of boolean;
  LocationsToUse: TList;
  InnerBoundaryIndex: Integer;
  AnotherBoundary: TLocation;
  ACount: integer;
  RowIndex: Integer;
  ColIndex: Integer;
  procedure InitializeTestArray;
  var
    RowIndex: Integer;
    ColIndex: Integer;
  begin
    for RowIndex := 0 to FGrid.RowCount - 1 do
    begin
      for ColIndex := 0 to FGrid.ColumnCount - 1 do
      begin
        UsedLocations[RowIndex,ColIndex] := false;
      end;
    end;
  end;
begin
  ACount := 0;
  // If multiple boundaries are in the same cell, they must
  // be separate objects.
  SetLength(UsedLocations, FGrid.RowCount, FGrid.ColumnCount);
  // Create parameters in the model.
  Parameters := TModflowTransientListParameters.Create(nil);
  try
    Parameters.Assign(FModel.ModflowTransientParameters);
    for ParameterIndex := 0 to FParameters.ArrayLength - 1 do
    begin
      Parameter := FParameters[ParameterIndex];
      NewParameter := Parameters.Add as TModflowTransientListParameter;
      NewParameter.ParameterName := Parameter.PARNAM;
      Parameter.ModifiedParamName := NewParameter.ParameterName;
      NewParameter.ParameterType := ParameterType;
      NewParameter.Value := Parameter.Parval;
    end;
    FModel.ModflowTransientParameters := Parameters;
  finally
    Parameters.Free;
  end;

  LocationsToUse := TList.Create;
  // Create TScreenObjects to represent the boundaries.
  // UsedStressPeriods will list the stress periods
  // in which a parameter is used.
  UsedStressPeriods := TIntegerList.Create;
  // Instances will indicate which instance of a parameter will be used
  // in any particular stress period.
  Instances := TIntegerList.Create;
  try
    for ParameterIndex := 0 to FParameters.ArrayLength - 1 do
    begin
      Parameter := FParameters[ParameterIndex];
      InstanceCount := Parameter.ArrayLength;
      UsedStressPeriods.Clear;
      Instances.Clear;
      if InstanceCount > 1 then
      begin
        for InstanceIndex := 0 to InstanceCount - 1 do
        begin
          Instance := Parameter.Instances[InstanceIndex];
          Assert(Instance.Name <> '');
        end;
      end;
      // Identify the stress periods in which the parameter is used
      // and the instance used in each stress period.
      for StressPeriodIndex := 0 to FStressPeriods.ArrayLength - 1 do
      begin
        StressPeriod := FStressPeriods[StressPeriodIndex];
        for InnerParamIndex := 0 to Length(StressPeriod.Parameters) - 1 do
        begin
          if SameText(Parameter.PARNAM,
            StressPeriod.Parameters[InnerParamIndex]) then
          begin
            if InstanceCount = 1 then
            begin
              UsedStressPeriods.Add(StressPeriodIndex);
              Instances.Add(0);
            end
            else
            begin
              for InstanceIndex := 0 to InstanceCount - 1 do
              begin
                Instance := Parameter.Instances[InstanceIndex];
                if SameText(Instance.Name,
                  StressPeriod.Instances[InnerParamIndex]) then
                begin
                  UsedStressPeriods.Add(StressPeriodIndex);
                  Instances.Add(InstanceIndex);
                end;
              end;
            end;
          end;
        end;
      end;
      // Create the TScreenObjects to represent the boundaries.
      for InstanceIndex := 0 to InstanceCount - 1 do
      begin
        Instance := Parameter.Instances[InstanceIndex];
        for LocationIndex := 0 to Instance.ArrayLength - 1 do
        begin
          Boundary := Instance.Locations[LocationIndex];
          if Boundary.Used then
          begin
            Continue;
          end;
          InitializeTestArray;
          LocationsToUse.Clear;
          LocationsToUse.Add(Boundary);
          UsedLocations[Boundary.Row-1, Boundary.Column-1] := True;
          Boundary.Used := True;
          for InnerBoundaryIndex := LocationIndex+1
            to Instance.ArrayLength - 1 do
          begin
            AnotherBoundary := Instance.Locations[InnerBoundaryIndex];
            if not AnotherBoundary.Used
              and (Boundary.Layer = AnotherBoundary.Layer)
              and not UsedLocations[AnotherBoundary.Row-1,
              AnotherBoundary.Column-1]
              and Boundary.SameObservations(AnotherBoundary)
              then
            begin
              LocationsToUse.Add(AnotherBoundary);
              UsedLocations[AnotherBoundary.Row-1,
                AnotherBoundary.Column-1] := True;
              AnotherBoundary.Used := True;
            end;
          end;
          ScreenObject := CreateScreenObject(LocationsToUse, ScreenObjectIndex,
            Boundary.Layer, InstanceIndex);
          SO_Boundary := GetBoundary(ScreenObject);
          ParamItem := SO_Boundary.Parameters.Add;
          Param := ParamItem.Param;
          Param.ParamName := Parameter.ModifiedParamName;
          // Loop over the stress periods in which this parameter
          // is used.
          for StressPeriodI := 0 to UsedStressPeriods.Count - 1 do
          begin
            // If this instance is used in the current stress period,
            // assign new values.
            if Instances[StressPeriodI] = InstanceIndex then
            begin
              StressPeriodIndex := UsedStressPeriods[StressPeriodI];
              StartTime := FModel.ModflowStressPeriods[
                StressPeriodIndex].StartTime;
              EndTime := FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
              Item := Param.Add as TCustomModflowBoundaryItem;
              Inc(ACount);
              SetItemValues(Item, LocationsToUse, EndTime, StartTime,
                ScreenObject, IntToStr(ACount));
            end;
          end;
        end;
      end;
    end;
  finally
    UsedStressPeriods.Free;
    Instances.Free;
    LocationsToUse.Free;
  end;
end;

procedure TListImporter.InitializeCurrentStressPeriod;
var
  Index: Integer;
begin
  if ITMP < 0 then
  begin
    FStressPeriods[FCurrentStressPeriod].Reuse := True;
    FStressPeriods[FCurrentStressPeriod].ArrayLength := 0;
  end
  else
  begin
    FStressPeriods[FCurrentStressPeriod].Reuse := False;
    FStressPeriods[FCurrentStressPeriod].ArrayLength := ITMP;
    for Index := 0 to ITMP - 1 do
    begin
      SetLength(FStressPeriods[FCurrentStressPeriod].Boundaries[Index].
        AuxilliaryVariables, FAuxillaryVariables.Count);
    end;
  end;
  SetLength(FStressPeriods[FCurrentStressPeriod].Parameters, NP);
  SetLength(FStressPeriods[FCurrentStressPeriod].Instances, NP);
  CurrentInstance := 0;
  CurrentParameter := 0;
end;

procedure TListImporter.InitializeStressPeriods;
begin
  FStressPeriods.ArrayLength :=
    FModel.ModflowStressPeriods.Count;
end;

procedure TListImporter.ReadAuxilliaryVariableName;
var
  AuxName: string;
begin
  Readln(FImporter.FFile, AuxName);
  AuxName := Trim(AuxName);
  FAuxillaryVariables.Add(AuxName);
end;

procedure TListImporter.ReadParameterName;
var
  PNAME: string;
begin
  Readln(FImporter.FFile, PNAME);
  PNAME := Trim(PNAME);
  Inc(CurrentParameter);
  FParameters[CurrentParameter].PARNAM := PNAME;
  FParameters[CurrentParameter].ArrayLength := 1;
  FParameters[CurrentParameter].Instances[0].ArrayLength := 1;
  SetLength(FParameters[CurrentParameter].Instances[0].Locations[0].
    AuxilliaryVariables, FAuxillaryVariables.Count);
  CurrentInstance := 0;
end;

procedure TListImporter.ReadParameterValueAndLocationCount;
var
  InnerIndex: Integer;
  Index: Integer;
  Parval: Double;
begin
  Read(FImporter.FFile, Parval);
  Read(FImporter.FFile, NLST);
  Readln(FImporter.FFile);
  FParameters[CurrentParameter].Parval := Parval;
  for Index := 0 to FParameters[CurrentParameter].ArrayLength - 1 do
  begin
    FParameters[CurrentParameter].Instances[Index].ArrayLength := NLST;
    for InnerIndex := 0 to NLST - 1 do
    begin
      SetLength(FParameters[CurrentParameter].Instances[Index].
        Locations[InnerIndex].AuxilliaryVariables, FAuxillaryVariables.Count);
    end;
  end;
  FBoundaryIndex := -1;
end;

procedure TListImporter.ReadNumberOfInstances;
var
  NUMINST: Integer;
  Index: Integer;
  InnerIndex: Integer;
  Param: TListParameterObject;
  Instance: TListInstanceObject;
begin
  Read(FImporter.FFile, NUMINST);
  Readln(FImporter.FFile);
  Param := FParameters[CurrentParameter];
  Param.ArrayLength := NUMINST;
  for Index := 0 to NUMINST - 1 do
  begin
    Instance := Param.Instances[Index];
    Instance.ArrayLength := NLST;
    for InnerIndex := 0 to NLST - 1 do
    begin
      SetLength(Instance.Locations[InnerIndex].AuxilliaryVariables,
        FAuxillaryVariables.Count);
    end;
  end;
end;

procedure TListImporter.ReadInstanceName;
var
  InstanceName: string;
begin
  Readln(FImporter.FFile, InstanceName);
  InstanceName := Trim(InstanceName);
  FParameters[CurrentParameter].Instances[CurrentInstance].Name := InstanceName;
end;

procedure TListImporter.ReadFirstStressPeriodDataSet5WithParameters;
begin
  Read(FImporter.FFile, ITMP);
  Read(FImporter.FFile, NP);
  Readln(FImporter.FFile);
  Inc(FCurrentStressPeriod);
  InitializeStressPeriods;
  InitializeCurrentStressPeriod;
end;

procedure TListImporter.ReadFirstStressPeriodDataSet5WithoutParameters;
begin
  ReadLn(FImporter.FFile, ITMP);
  NP := 0;
  Inc(FCurrentStressPeriod);
  InitializeStressPeriods;
  InitializeCurrentStressPeriod;
end;

procedure TListImporter.ReadNewStressPeriodDataSet5WithParameters;
begin
  Read(FImporter.FFile, ITMP);
  Read(FImporter.FFile, NP);
  Readln(FImporter.FFile);
  Inc(FCurrentStressPeriod);
  InitializeCurrentStressPeriod;
end;

procedure TListImporter.ReadNewStressPeriodDataSet5WithoutParameters;
begin
  ReadLn(FImporter.FFile, ITMP);
  NP := 0;
  Inc(FCurrentStressPeriod);
  InitializeCurrentStressPeriod;
end;

procedure TListImporter.ReadParamNameForStressPeriod;
var
  ParameterName: string;
begin
  Readln(FImporter.FFile, ParameterName);
  ParameterName := Trim(ParameterName);
  if Length(ParameterName) > 10 then
  begin
    SetLength(ParameterName, 10);
  end;
  FStressPeriods[FCurrentStressPeriod].Parameters[CurrentParameter] :=
    ParameterName;
  Inc(CurrentParameter);
end;

procedure TListImporter.ReleaseMemory;
begin
  if FParameters <> nil then
  begin
    FParameters.ArrayLength := 0;
  end;
  if FStressPeriods <> nil then
  begin
    FStressPeriods.ArrayLength := 0;
  end;
end;

procedure TListImporter.ReadInstanceNameForStressPeriod;
var
  InstanceName: string;
begin
  Readln(FImporter.FFile, InstanceName);
  InstanceName := Trim(InstanceName);
  FStressPeriods[FCurrentStressPeriod].Instances[CurrentParameter - 1] :=
    InstanceName;
end;

{ TGhbInstanceObject }

function TGhbInstanceObject.ArrayMemberClass: TArrayMemberClass;
begin
  result := TGhbLocationObject;
end;

{ TGhbParameterObject }

function TGhbParameterObject.ArrayMemberClass: TArrayMemberClass;
begin
  result := TGhbInstanceObject;
end;

{ TGhbParamArray }

function TGhbParamArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TGhbParameterObject;
end;

{ TGhbStressPeriod }

function TGhbStressPeriod.ArrayMemberClass: TArrayMemberClass;
begin
  result := TGhbLocationObject;
end;

{ TGhbStressPeriodArray }

function TGhbStressPeriodArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TGhbStressPeriod;
end;

procedure TListImporter.ReadDataSet1;
var
  MXL: integer;
begin

  Read(FImporter.FFile, NP);
  Read(FImporter.FFile, MXL);
  Readln(FImporter.FFile);
  FParameters.ArrayLength := NP;
end;

{ TGhbImporter }

constructor TGhbImporter.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'GHB:');
  FStressPeriods:= TGhbStressPeriodArray.Create;
  FParameters:= TGhbParamArray.Create;
end;

function TGhbImporter.CreateScreenObject(List: TList;
  var ScreenObjectIndex: Integer; LayerIndex,
  StressPeriodIndex: integer): TScreenObject;
begin
  result := inherited CreateScreenObject(List, ScreenObjectIndex, LayerIndex,
    StressPeriodIndex);
  result.CreateGhbBoundary;
  result.ModflowGhbBoundary.FormulaInterpretation := fiDirect;
end;

destructor TGhbImporter.Destroy;
begin
  FStressPeriods.Free;
  FParameters.Free;
  inherited;
end;

function TGhbImporter.GetBoundary(
  ScreenObject: TScreenObject): TModflowParamBoundary;
begin
  result := ScreenObject.ModflowGhbBoundary;
end;

procedure TGhbImporter.HandlePackage;
var
  ScreenObjectIndex: integer;
  GhbPackage: TModflowPackageSelection;
begin
  if (FCurrentStressPeriod < 0) or
    (FCurrentStressPeriod < FStressPeriods.ArrayLength -1) then
  begin
    Exit;
  end;
  inherited;
  GhbPackage := FModel.ModflowPackages.GhbBoundary;
  GhbPackage.IsSelected := True;
  GhbPackage.Comments := FComments;
  AssignObservations;

  ScreenObjectIndex := 0;
  ImportNonParameterBoundaries(ScreenObjectIndex);
  ImportParameterBoundaries(ScreenObjectIndex);
  if FObsImporter <> nil then
  begin
    FObsImporter.HandlePackage;
  end;
  ReleaseMemory;
end;

procedure TGhbImporter.ReadData(const ALabel: string);
begin
  inherited;
  if FCurrentStressPeriod = -1 then
  begin
    if ALabel = 'NP,MXL:' then
    begin
      ReadDataSet1;
    end
    else if ALabel = 'MXACTB,IGHBCB:' then
    begin
      ReadDataSet2;
    end
    else if ALabel = 'GHBAUX(NAUX):' then
    begin
      ReadAuxilliaryVariableName;
    end
    else if ALabel = 'NOPRINT:' then
    begin
      FNoPrint := True;
    end
    else if ALabel = 'PARNAM:' then
    begin
      ReadParameterName;
    end
    else if ALabel = 'PARTYP:' then
    begin
      ReadParameterType;
    end
    else if ALabel = 'Parval,NLST:' then
    begin
      ReadParameterValueAndLocationCount;
    end
    else if ALabel = 'INSTANCES:' then
    begin
      // do nothing
    end
    else if ALabel = 'NUMINST:' then
    begin
      ReadNumberOfInstances;
    end
    else if ALabel = 'INAME(ILOC):' then
    begin
      ReadInstanceName;
    end
    else if ALabel = 'Layer Row Column Bhead  Cond [xyz]:' then
    begin
      ReadParameterLocations;
    end
    else if ALabel = 'ITMP,NP:' then
    begin
      ReadFirstStressPeriodDataSet5WithParameters;
    end
    else if ALabel = 'ITMP:' then
    begin
      ReadFirstStressPeriodDataSet5WithoutParameters;
    end
    else
    begin
      Assert(False);
    end;
  end
  else
  begin
    if ALabel = 'ITMP,NP:' then
    begin
      ReadNewStressPeriodDataSet5WithParameters;
    end
    else if ALabel = 'ITMP:' then
    begin
      ReadNewStressPeriodDataSet5WithoutParameters;
    end
    else if ALabel = 'Layer Row Column Bhead  Cond [xyz]:' then
    begin
      ReadNonParameterLocations;
    end
    else if ALabel = 'Pname:' then
    begin
      ReadParamNameForStressPeriod;
    end
    else if ALabel = 'Iname:' then
    begin
      ReadInstanceNameForStressPeriod;
    end
    else
    begin
      Assert(False);
    end;
  end;
end;

function TGhbImporter.ParameterType: TParameterType;
begin
  result := ptGHB;
end;

procedure TGhbImporter.ReadDataSet2;
begin
  Read(FImporter.FFile, MXACTB);
  Read(FImporter.FFile, IGHBCB);
  Readln(FImporter.FFile);
end;

procedure TGhbImporter.ReadNonParameterLocations;
var
  Index: Integer;
  AuxVar: Double;
  BoundaryIndex: Integer;
  LAYER: Integer;
  Row: Integer;
  Column: Integer;
  BHead: Double;
  GhbBoundary: TGhbLocationObject;
  Conductance: double;
begin
  for BoundaryIndex := 0 to ITMP - 1 do
  begin
    GhbBoundary := FStressPeriods[FCurrentStressPeriod].
      Boundaries[CurrentInstance] as TGhbLocationObject;
    Read(FImporter.FFile, LAYER);
    GhbBoundary.Layer := LAYER;
    Read(FImporter.FFile, Row);
    GhbBoundary.Row := Row;
    Read(FImporter.FFile, Column);
    GhbBoundary.Column := Column;
    Read(FImporter.FFile, BHead);
    GhbBoundary.BHead := BHead;
    Read(FImporter.FFile, Conductance);
    GhbBoundary.Conductance := Conductance;
    for Index := 0 to FAuxillaryVariables.Count - 1 do
    begin
      Read(FImporter.FFile, AuxVar);
      GhbBoundary.AuxilliaryVariables[Index] := AuxVar;
    end;
    Inc(CurrentInstance);
  end;
end;

procedure TGhbImporter.ReadParameterLocations;
var
  AuxVar: Double;
  BoundaryIndex: Integer;
  LAYER: Integer;
  Row: Integer;
  Column: Integer;
  BHead: Double;
  Index: Integer;
  GhbBoundary : TGhbLocationObject;
  Conductance: double;
begin
  for BoundaryIndex := 0 to NLST - 1 do
  begin
    GhbBoundary := FParameters[CurrentParameter].Instances[CurrentInstance].
      Locations[BoundaryIndex] as TGhbLocationObject;
    Read(FImporter.FFile, LAYER);
    GhbBoundary.Layer := LAYER;
    Read(FImporter.FFile, Row);
    GhbBoundary.Row := Row;
    Read(FImporter.FFile, Column);
    GhbBoundary.Column := Column;
    Read(FImporter.FFile, BHead);
    GhbBoundary.BHead := BHead;
    Read(FImporter.FFile, Conductance);
    GhbBoundary.Conductance := Conductance;
    for Index := 0 to FAuxillaryVariables.Count - 1 do
    begin
      Read(FImporter.FFile, AuxVar);
      GhbBoundary.AuxilliaryVariables[Index] := AuxVar;
    end;
    ReadLn(FImporter.FFile);
  end;
  Inc(CurrentInstance);
end;

procedure TGhbImporter.ReadParameterType;
var
  PARTYP: string;
begin
  Readln(FImporter.FFile, PARTYP);
  PARTYP := Trim(PARTYP);
  PARTYP := UpperCase(PARTYP);
  Assert(PARTYP = 'GHB');
  FParameters[CurrentParameter].PARTYP := PARTYP;
end;

function TGhbImporter.ScreenObjectNameRoot: string;
begin
  result := 'Imported_GHB_'
end;

procedure TGhbImporter.SetItemValues(Item: TCustomModflowBoundaryItem;
  Boundaries: TList; EndTime, StartTime: Double; ScreenObject: TScreenObject;
  const ParamName: string);
var
  GhbBoundary: TGhbLocationObject;
  GhbItem: TGhbItem;
  ValueItem: TValueArrayItem;
  Index: Integer;
  GhbHeadValues: TValueArrayStorage;
  GhbConductanceValues: TValueArrayStorage;
  HeadName: string;
  ConductanceName: string;
begin
  ValueItem := ScreenObject.ImportedValues.Add as TValueArrayItem;
  HeadName := 'GhbHead' + ParamName;
  ValueItem.Name := HeadName;
  GhbHeadValues := ValueItem.Values;
  GhbHeadValues.DataType := rdtDouble;
  GhbHeadValues.Count := Boundaries.Count;

  ValueItem := ScreenObject.ImportedValues.Add as TValueArrayItem;
  ConductanceName := 'GhbConductance' + ParamName;
  ValueItem.Name := ConductanceName;
  GhbConductanceValues := ValueItem.Values;
  GhbConductanceValues.DataType := rdtDouble;
  GhbConductanceValues.Count := Boundaries.Count;

  GhbItem := Item as TGhbItem;
  GhbItem.StartTime := StartTime;
  GhbItem.EndTime := EndTime;
  for Index := 0 to Boundaries.Count - 1 do
  begin
    GhbBoundary := Boundaries[Index];
    GhbHeadValues.RealValues[Index] := GhbBoundary.BHead;
    GhbConductanceValues.RealValues[Index] := GhbBoundary.Conductance;
  end;
  GhbItem.BoundaryHead := rsObjectImportedValuesR + '("' + HeadName + '")';
  GhbItem.Conductance := rsObjectImportedValuesR + '("' + ConductanceName
    + '")';
  AssignObservationFactors(ScreenObject, ParamName, Boundaries);
end;

{ TDrnInstanceObject }

function TDrnInstanceObject.ArrayMemberClass: TArrayMemberClass;
begin
  result := TDrnLocationObject;
end;

{ TDrnParameterObject }

function TDrnParameterObject.ArrayMemberClass: TArrayMemberClass;
begin
  result := TDrnInstanceObject;
end;

{ TDrnParamArray }

function TDrnParamArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TDrnParameterObject;
end;

{ TDrnStressPeriod }

function TDrnStressPeriod.ArrayMemberClass: TArrayMemberClass;
begin
  result := TDrnLocationObject;
end;

{ TDrnStressPeriodArray }

function TDrnStressPeriodArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TDrnStressPeriod;
end;

{ TDrnImporter }

procedure TListImporter.AssignObservations;
var
  StressPeriodIndex: Integer;
  CellList: TList;
begin
  // If observations are used, add the observation group
  // and observation location to each cell that
  // is part of the observation.
  if FObsImporter <> nil then
  begin
    FObsImporter.CreateObservationGroups;
    CellList := TList.Create;
    try
      for StressPeriodIndex := 0 to FStressPeriods.ArrayLength - 1 do
      begin
        FillListOfCellsForStressPeriod(StressPeriodIndex, CellList);
        AssignObsGroupsToCells(CellList);
      end;
    finally
      CellList.Free;
    end;
  end;
end;

constructor TDrnImporter.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'DRN:');
  FObsImporter := nil;
  FStressPeriods:= TDrnStressPeriodArray.Create;
  FParameters:= TDrnParamArray.Create;
end;

function TDrnImporter.CreateScreenObject(List: TList;
  var ScreenObjectIndex: Integer; LayerIndex,
  StressPeriodIndex: integer): TScreenObject;
begin
  result := inherited CreateScreenObject(List,
    ScreenObjectIndex, LayerIndex, StressPeriodIndex);
  result.CreateDrnBoundary;
  result.ModflowDrnBoundary.FormulaInterpretation := fiDirect;
end;

destructor TDrnImporter.Destroy;
begin
  FStressPeriods.Free;
  FParameters.Free;
  inherited;
end;

procedure TListImporter.AssignObservationFactors(ScreenObject: TScreenObject;
  const ParamName: string; Boundaries: TList);
var
  Location: TLocation;
  ObservationFactors: TList;
  ObservationNames: TStringList;
  ObservationIndex: Integer;
  Values: TValueArrayStorage;
  FactorIndex: Integer;
  ObsCell: TFlowObservationLocation;
  ObsGroup: TObservationGroup;
  Group: TFluxObservationGroup;
  Position: Integer;
  Factor: TObservationFactor;
  Index: Integer;
  ValueItem: TValueArrayItem;
begin
  Assert(Boundaries.Count > 0);
  Location := Boundaries[0];
  if Location.FObservationGroups.Count = 0 then
  begin
    Exit;
  end;
  ObservationFactors := TList.Create;
  ObservationNames := TStringList.Create;
  try
    for ObservationIndex := 0 to Location.FObservationGroups.Count - 1 do
    begin
      ValueItem := ScreenObject.ImportedValues.Add as TValueArrayItem;
      ValueItem.Name := 'ObservationFactor_' + ParamName
        + '_' + IntToStr(ObservationIndex + 1);
      ObservationNames.Add(ValueItem.Name);
      Values := ValueItem.Values;
      Values.DataType := rdtDouble;
      Values.Count := Boundaries.Count;
      ObservationFactors.Add(Values);
    end;
    for Index := 0 to Boundaries.Count - 1 do
    begin
      Location := Boundaries[Index];
      for FactorIndex := 0 to ObservationFactors.Count - 1 do
      begin
        Values := ObservationFactors[FactorIndex];
        ObsCell := Location.FObservationCells[FactorIndex];
        Values.RealValues[Index] := ObsCell.FACTOR;
      end;
    end;
    Location := Boundaries[0];
    for ObservationIndex := 0 to Location.FObservationGroups.Count - 1 do
    begin
      ObsGroup := Location.FObservationGroups[ObservationIndex];
      Group := ObsGroup.FGroup;
      Position := Group.AddObject(ScreenObject);
      Factor := Group.ObservationFactors[Position];
      Factor.Factor := rsObjectImportedValuesR + '("'
        + ObservationNames[ObservationIndex] + '")';
    end;
  finally
    ObservationNames.Free;
    ObservationFactors.Free;
  end;
end;

procedure TListImporter.FillListOfCellsForStressPeriod(
  StressPeriodIndex: Integer; CellList: TList);
var
  Instance: TListInstanceObject;
  InstanceCount: Integer;
  ParPosition: Integer;
  Param: TListParameterObject;
  ParamIndex: Integer;
  BoundaryCell: TLocation;
  CellIndex: Integer;
  StressPeriodData: TListStressPeriod;
begin
  CellList.Clear;
  StressPeriodData := FStressPeriods[StressPeriodIndex];
  for CellIndex := 0 to StressPeriodData.ArrayLength - 1 do
  begin
    BoundaryCell := StressPeriodData[CellIndex];
    CellList.Add(BoundaryCell);
  end;
  for ParamIndex := 0 to FParameters.ArrayLength - 1 do
  begin
    Param := FParameters[ParamIndex];
    ParPosition := StressPeriodData.IndexOfParameter(Param.PARNAM);
    if ParPosition >= 0 then
    begin
      InstanceCount := Param.ArrayLength;
      if InstanceCount > 1 then
      begin
        Instance := Param.GetInstanceByName(
          StressPeriodData.Instances[ParPosition]);
      end
      else
      begin
        Instance := Param[0];
      end;
      for CellIndex := 0 to Instance.ArrayLength - 1 do
      begin
        BoundaryCell := Instance[CellIndex];
        CellList.Add(BoundaryCell);
      end;
    end;
  end;
end;

function TDrnImporter.GetBoundary(
  ScreenObject: TScreenObject): TModflowParamBoundary;
begin
  result := ScreenObject.ModflowDrnBoundary;
end;

procedure TDrnImporter.HandlePackage;
var
  ScreenObjectIndex: integer;
  DrnPackage: TModflowPackageSelection;
begin
  if (FCurrentStressPeriod < 0) or
    (FCurrentStressPeriod < FStressPeriods.ArrayLength -1) then
  begin
    Exit;
  end;
  inherited;
  DrnPackage := FModel.ModflowPackages.DrnPackage;
  DrnPackage.IsSelected := True;
  DrnPackage.Comments := FComments;
  AssignObservations;

  ScreenObjectIndex := 0;
  ImportNonParameterBoundaries(ScreenObjectIndex);
  ImportParameterBoundaries(ScreenObjectIndex);
  if FObsImporter <> nil then
  begin
    FObsImporter.HandlePackage;
  end;
  ReleaseMemory;
end;

procedure TDrnImporter.ReadData(const ALabel: string);
begin
  inherited;
  if FCurrentStressPeriod = -1 then
  begin
    if ALabel = 'NP,MXL:' then
    begin
      ReadDataSet1;
    end
    else if ALabel = 'MXACTD,IDRNCB:' then
    begin
      ReadDataSet2;
    end
    else if ALabel = 'DRNAUX(NAUX):' then
    begin
      ReadAuxilliaryVariableName;
    end
    else if ALabel = 'NOPRINT:' then
    begin
      FNoPrint := True;
    end
    else if ALabel = 'PARNAM:' then
    begin
      ReadParameterName;
    end
    else if ALabel = 'PARTYP:' then
    begin
      ReadParameterType;
    end
    else if ALabel = 'Parval,NLST:' then
    begin
      ReadParameterValueAndLocationCount;
    end
    else if ALabel = 'INSTANCES:' then
    begin
      // do nothing
    end
    else if ALabel = 'NUMINST:' then
    begin
      ReadNumberOfInstances;
    end
    else if ALabel = 'INAME(ILOC):' then
    begin
      ReadInstanceName;
    end
    else if ALabel = 'Layer Row Column Elevation  Cond [xyz]:' then
    begin
      ReadParameterLocations;
    end
    else if ALabel = 'ITMP,NP:' then
    begin
      ReadFirstStressPeriodDataSet5WithParameters;
    end
    else if ALabel = 'ITMP:' then
    begin
      ReadFirstStressPeriodDataSet5WithoutParameters;
    end
    else
    begin
      Assert(False);
    end;
  end
  else
  begin
    if ALabel = 'ITMP,NP:' then
    begin
      ReadNewStressPeriodDataSet5WithParameters;
    end
    else if ALabel = 'ITMP:' then
    begin
      ReadNewStressPeriodDataSet5WithoutParameters;
    end
    else if ALabel = 'Layer Row Column Elevation  Cond [xyz]:' then
    begin
      ReadNonParameterLocations;
    end
    else if ALabel = 'Pname:' then
    begin
      ReadParamNameForStressPeriod;
    end
    else if ALabel = 'Iname:' then
    begin
      ReadInstanceNameForStressPeriod;
    end
    else
    begin
      Assert(False);
    end;
  end;
end;

function TDrnImporter.ParameterType: TParameterType;
begin
  result := ptDRN;
end;

procedure TDrnImporter.ReadDataSet2;
begin
  Read(FImporter.FFile, MXACTD);
  Read(FImporter.FFile, IDRNCB);
  Readln(FImporter.FFile);
end;

procedure TDrnImporter.ReadNonParameterLocations;
var
  Index: Integer;
  AuxVar: Double;
  BoundaryIndex: Integer;
  LAYER: Integer;
  Row: Integer;
  Column: Integer;
  DrnBoundary: TDrnLocationObject;
  Conductance: double;
  Elevation: double;
begin
  for BoundaryIndex := 0 to ITMP - 1 do
  begin
    DrnBoundary := FStressPeriods[FCurrentStressPeriod].
      Boundaries[CurrentInstance] as TDrnLocationObject;
    Read(FImporter.FFile, LAYER);
    DrnBoundary.Layer := LAYER;
    Read(FImporter.FFile, Row);
    DrnBoundary.Row := Row;
    Read(FImporter.FFile, Column);
    DrnBoundary.Column := Column;
    Read(FImporter.FFile, Elevation);
    DrnBoundary.Elevation := Elevation;
    Read(FImporter.FFile, Conductance);
    DrnBoundary.Conductance := Conductance;
    for Index := 0 to FAuxillaryVariables.Count - 1 do
    begin
      Read(FImporter.FFile, AuxVar);
      DrnBoundary.AuxilliaryVariables[Index] := AuxVar;
    end;
    Inc(CurrentInstance);
  end;
end;

procedure TDrnImporter.ReadParameterLocations;
var
  AuxVar: Double;
  BoundaryIndex: Integer;
  LAYER: Integer;
  Row: Integer;
  Column: Integer;
  Index: Integer;
  DrnBoundary : TDrnLocationObject;
  Conductance: double;
  Elevation: double;
begin
  for BoundaryIndex := 0 to NLST - 1 do
  begin
    DrnBoundary := FParameters[CurrentParameter].Instances[CurrentInstance].
      Locations[BoundaryIndex] as TDrnLocationObject;
    Read(FImporter.FFile, LAYER);
    DrnBoundary.Layer := LAYER;
    Read(FImporter.FFile, Row);
    DrnBoundary.Row := Row;
    Read(FImporter.FFile, Column);
    DrnBoundary.Column := Column;
    Read(FImporter.FFile, Elevation);
    DrnBoundary.Elevation := Elevation;
    Read(FImporter.FFile, Conductance);
    DrnBoundary.Conductance := Conductance;
    for Index := 0 to FAuxillaryVariables.Count - 1 do
    begin
      Read(FImporter.FFile, AuxVar);
      DrnBoundary.AuxilliaryVariables[Index] := AuxVar;
    end;
    ReadLn(FImporter.FFile);
  end;
  Inc(CurrentInstance);
end;

procedure TDrnImporter.ReadParameterType;
var
  PARTYP: string;
begin
  Readln(FImporter.FFile, PARTYP);
  PARTYP := Trim(PARTYP);
  PARTYP := UpperCase(PARTYP);
  Assert(PARTYP = 'DRN');
  FParameters[CurrentParameter].PARTYP := PARTYP;
end;

function TDrnImporter.ScreenObjectNameRoot: string;
begin
  result := 'Imported_DRN_'
end;

procedure TDrnImporter.SetItemValues(Item: TCustomModflowBoundaryItem;
  Boundaries: TList; EndTime, StartTime: Double; ScreenObject: TScreenObject;
  const ParamName: string);
var
  DrnBoundary: TDrnLocationObject;
  DrnItem: TDrnItem;
  ValueItem: TValueArrayItem;
  Index: Integer;
  DrnHeadValues: TValueArrayStorage;
  DrnConductanceValues: TValueArrayStorage;
  DrnElevName: string;
  ConductanceName: string;
begin
  ValueItem := ScreenObject.ImportedValues.Add as TValueArrayItem;
  DrnElevName := 'DrnElevation' + ParamName;
  ValueItem.Name := DrnElevName;
  DrnHeadValues := ValueItem.Values;
  DrnHeadValues.DataType := rdtDouble;
  DrnHeadValues.Count := Boundaries.Count;

  ValueItem := ScreenObject.ImportedValues.Add as TValueArrayItem;
  ConductanceName := 'DrnConductance' + ParamName;
  ValueItem.Name := ConductanceName;
  DrnConductanceValues := ValueItem.Values;
  DrnConductanceValues.DataType := rdtDouble;
  DrnConductanceValues.Count := Boundaries.Count;

  DrnItem := Item as TDrnItem;
  DrnItem.StartTime := StartTime;
  DrnItem.EndTime := EndTime;
  for Index := 0 to Boundaries.Count - 1 do
  begin
    DrnBoundary := Boundaries[Index];
    DrnHeadValues.RealValues[Index] := DrnBoundary.Elevation;
    DrnConductanceValues.RealValues[Index] := DrnBoundary.Conductance;
  end;
  DrnItem.Elevation := rsObjectImportedValuesR + '("' + DrnElevName + '")';
  DrnItem.Conductance := rsObjectImportedValuesR
    + '("' + ConductanceName + '")';

  AssignObservationFactors(ScreenObject, ParamName, Boundaries);
end;

{ TRivInstanceObject }

function TRivInstanceObject.ArrayMemberClass: TArrayMemberClass;
begin
  result := TRivLocationObject;
end;

{ TRivParameterObject }

function TRivParameterObject.ArrayMemberClass: TArrayMemberClass;
begin
  result := TRivInstanceObject;
end;

{ TRivParamArray }

function TRivParamArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TRivParameterObject;
end;

{ TRivStressPeriod }

function TRivStressPeriod.ArrayMemberClass: TArrayMemberClass;
begin
  result := TRivLocationObject;
end;

{ TRivStressPeriodArray }

function TRivStressPeriodArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TRivStressPeriod;
end;

{ TRivImporter }

constructor TRivImporter.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'RIV:');
  FStressPeriods:= TRivStressPeriodArray.Create;
  FParameters:= TRivParamArray.Create;
end;

function TRivImporter.CreateScreenObject(List: TList;
  var ScreenObjectIndex: Integer; LayerIndex,
  StressPeriodIndex: integer): TScreenObject;
begin
  result := inherited CreateScreenObject(List,
    ScreenObjectIndex, LayerIndex, StressPeriodIndex);
  result.CreateRivBoundary;
  result.ModflowRivBoundary.FormulaInterpretation := fiDirect;
end;

destructor TRivImporter.Destroy;
begin
  FStressPeriods.Free;
  FParameters.Free;
  inherited;
end;

function TRivImporter.GetBoundary(
  ScreenObject: TScreenObject): TModflowParamBoundary;
begin
  result := ScreenObject.ModflowRivBoundary
end;

procedure TRivImporter.HandlePackage;
var
  ScreenObjectIndex: integer;
  RivPackage: TModflowPackageSelection;
begin
  if (FCurrentStressPeriod < 0) or
    (FCurrentStressPeriod < FStressPeriods.ArrayLength -1) then
  begin
    Exit;
  end;
  inherited;
  RivPackage := FModel.ModflowPackages.RivPackage;
  RivPackage.IsSelected := True;
  RivPackage.Comments := FComments;
  AssignObservations;

  ScreenObjectIndex := 0;
  ImportNonParameterBoundaries(ScreenObjectIndex);
  ImportParameterBoundaries(ScreenObjectIndex);
  if FObsImporter <> nil then
  begin
    FObsImporter.HandlePackage;
  end;
  ReleaseMemory;
end;

procedure TRivImporter.ReadData(const ALabel: string);
begin
  inherited;
  if FCurrentStressPeriod = -1 then
  begin
    if ALabel = 'NP,MXL:' then
    begin
      ReadDataSet1;
    end
    else if ALabel = 'MXACTR,IRIVCB:' then
    begin
      ReadDataSet2;
    end
    else if ALabel = 'RIVAUX(NAUX):' then
    begin
      ReadAuxilliaryVariableName;
    end
    else if ALabel = 'NOPRINT:' then
    begin
      FNoPrint := True;
    end
    else if ALabel = 'PARNAM:' then
    begin
      ReadParameterName;
    end
    else if ALabel = 'PARTYP:' then
    begin
      ReadParameterType;
    end
    else if ALabel = 'Parval,NLST:' then
    begin
      ReadParameterValueAndLocationCount;
    end
    else if ALabel = 'INSTANCES:' then
    begin
      // do nothing
    end
    else if ALabel = 'NUMINST:' then
    begin
      ReadNumberOfInstances;
    end
    else if ALabel = 'INAME(ILOC):' then
    begin
      ReadInstanceName;
    end
    else if ALabel = 'Layer Row Column Stage  Cond Rbot [xyz]:' then
    begin
      ReadParameterLocations;
    end
    else if ALabel = 'ITMP,NP:' then
    begin
      ReadFirstStressPeriodDataSet5WithParameters;
    end
    else if ALabel = 'ITMP:' then
    begin
      ReadFirstStressPeriodDataSet5WithoutParameters;
    end
    else
    begin
      Assert(False);
    end;
  end
  else
  begin
    if ALabel = 'ITMP,NP:' then
    begin
      ReadNewStressPeriodDataSet5WithParameters;
    end
    else if ALabel = 'ITMP:' then
    begin
      ReadNewStressPeriodDataSet5WithoutParameters;
    end
    else if ALabel = 'Layer Row Column Stage  Cond Rbot [xyz]:' then
    begin
      ReadNonParameterLocations;
    end
    else if ALabel = 'Pname:' then
    begin
      ReadParamNameForStressPeriod;
    end
    else if ALabel = 'Iname:' then
    begin
      ReadInstanceNameForStressPeriod;
    end
    else
    begin
      Assert(False);
    end;
  end;
end;

function TRivImporter.ParameterType: TParameterType;
begin
  result := ptRIV;
end;

procedure TRivImporter.ReadDataSet2;
begin
  Read(FImporter.FFile, MXACTR);
  Read(FImporter.FFile, IRIVCB);
  Readln(FImporter.FFile);
end;

procedure TRivImporter.ReadNonParameterLocations;
var
  Index: Integer;
  AuxVar: Double;
  BoundaryIndex: Integer;
  LAYER: Integer;
  Row: Integer;
  Column: Integer;
  RivBoundary: TRivLocationObject;
  Conductance: double;
  Stage: double;
  Bottom: double;
begin
  for BoundaryIndex := 0 to ITMP - 1 do
  begin
    RivBoundary := FStressPeriods[FCurrentStressPeriod].
      Boundaries[CurrentInstance] as TRivLocationObject;
    Read(FImporter.FFile, LAYER);
    RivBoundary.Layer := LAYER;
    Read(FImporter.FFile, Row);
    RivBoundary.Row := Row;
    Read(FImporter.FFile, Column);
    RivBoundary.Column := Column;
    Read(FImporter.FFile, Stage);
    RivBoundary.Stage := Stage;
    Read(FImporter.FFile, Conductance);
    RivBoundary.Conductance := Conductance;
    Read(FImporter.FFile, Bottom);
    RivBoundary.Bottom := Bottom;
    for Index := 0 to FAuxillaryVariables.Count - 1 do
    begin
      Read(FImporter.FFile, AuxVar);
      RivBoundary.AuxilliaryVariables[Index] := AuxVar;
    end;
    Inc(CurrentInstance);
  end;
end;

procedure TRivImporter.ReadParameterLocations;
var
  AuxVar: Double;
  BoundaryIndex: Integer;
  LAYER: Integer;
  Row: Integer;
  Column: Integer;
  Index: Integer;
  RivBoundary : TRivLocationObject;
  Conductance: double;
  Stage: double;
  Bottom: double;
begin
  for BoundaryIndex := 0 to NLST - 1 do
  begin
    RivBoundary := FParameters[CurrentParameter].Instances[CurrentInstance].
      Locations[BoundaryIndex] as TRivLocationObject;
    Read(FImporter.FFile, LAYER);
    RivBoundary.Layer := LAYER;
    Read(FImporter.FFile, Row);
    RivBoundary.Row := Row;
    Read(FImporter.FFile, Column);
    RivBoundary.Column := Column;
    Read(FImporter.FFile, Stage);
    RivBoundary.Stage := Stage;
    Read(FImporter.FFile, Conductance);
    RivBoundary.Conductance := Conductance;
    Read(FImporter.FFile, Bottom);
    RivBoundary.Bottom := Bottom;
    for Index := 0 to FAuxillaryVariables.Count - 1 do
    begin
      Read(FImporter.FFile, AuxVar);
      RivBoundary.AuxilliaryVariables[Index] := AuxVar;
    end;
    ReadLn(FImporter.FFile);
  end;
  Inc(CurrentInstance);
end;

procedure TRivImporter.ReadParameterType;
var
  PARTYP: string;
begin
  Readln(FImporter.FFile, PARTYP);
  PARTYP := Trim(PARTYP);
  PARTYP := UpperCase(PARTYP);
  Assert(PARTYP = 'RIV');
  FParameters[CurrentParameter].PARTYP := PARTYP;
end;

function TRivImporter.ScreenObjectNameRoot: string;
begin
  result := 'Imported_Riv_'
end;

procedure TRivImporter.SetItemValues(Item: TCustomModflowBoundaryItem;
  Boundaries: TList; EndTime, StartTime: Double; ScreenObject: TScreenObject;
  const ParamName: string);
var
  RivBoundary: TRivLocationObject;
  RivItem: TRivItem;
  ValueItem: TValueArrayItem;
  Index: Integer;
  RivBottomValues: TValueArrayStorage;
  RivStageValues: TValueArrayStorage;
  RivConductanceValues: TValueArrayStorage;
  StageName: string;
  ConductanceName: string;
  BottomName: string;
begin
  ValueItem := ScreenObject.ImportedValues.Add as TValueArrayItem;
  StageName := 'RivStage' + ParamName;
  ValueItem.Name := StageName;
  RivStageValues := ValueItem.Values;
  RivStageValues.DataType := rdtDouble;
  RivStageValues.Count := Boundaries.Count;

  ValueItem := ScreenObject.ImportedValues.Add as TValueArrayItem;
  ConductanceName := 'RivConductance' + ParamName;
  ValueItem.Name := ConductanceName;
  RivConductanceValues := ValueItem.Values;
  RivConductanceValues.DataType := rdtDouble;
  RivConductanceValues.Count := Boundaries.Count;

  ValueItem := ScreenObject.ImportedValues.Add as TValueArrayItem;
  BottomName := 'RivBottom' + ParamName;
  ValueItem.Name := BottomName;
  RivBottomValues := ValueItem.Values;
  RivBottomValues.DataType := rdtDouble;
  RivBottomValues.Count := Boundaries.Count;

  RivItem := Item as TRivItem;
  RivItem.StartTime := StartTime;
  RivItem.EndTime := EndTime;
  for Index := 0 to Boundaries.Count - 1 do
  begin
    RivBoundary := Boundaries[Index];
    RivStageValues.RealValues[Index] := RivBoundary.Stage;
    RivConductanceValues.RealValues[Index] := RivBoundary.Conductance;
    RivBottomValues.RealValues[Index] := RivBoundary.Bottom;
  end;
  RivItem.RiverStage := rsObjectImportedValuesR + '("' + StageName + '")';
  RivItem.Conductance := rsObjectImportedValuesR + '("'
    + ConductanceName + '")';
  RivItem.RiverBottom := rsObjectImportedValuesR + '("' + BottomName + '")';
  AssignObservationFactors(ScreenObject, ParamName, Boundaries);
end;

{ TRchImporter }

constructor TRchImporter.Create(Importer: TModflow2005Importer;
  ZoneImporter: TMultZoneImporter);
begin
  inherited Create(Importer, 'RCH:', ZoneImporter);
end;

procedure TRchImporter.CreateBoundary(ScreenObject: TScreenObject);
begin
  ScreenObject.CreateRchBoundary;
end;

procedure TTransientArrayImporter.CreateAssignedLayerDataSet(
  Package: TCustomTransientLayerPackageSelection; DataSetRoot: string;
  ScreenObjectName: string; var AssignedLayerDataSet: TDataArray);
var
  ColumnIndex: Integer;
  Layer: Integer;
  DSAbove: string;
  DSBelow: string;
  ScreenObject: TScreenObject;
  ImportedData: T2DDoubleArray;
  RowIndex: Integer;
begin
  ScreenObject := nil;
  if (Package.LayerOption = loSpecified)
    and not Package.TimeVaryingLayers then
  begin
    Assert(not FReuseLayerIndicator[0]);
    CreateDataSet(-1, DataSetRoot, rdtDouble, AssignedLayerDataSet);
    if (FConstantLayerIndicators <> nil)
      and FConstantLayerIndicators[0].IsConstant then
    begin
      Layer := FConstantLayerIndicators[0].IntegerValue;
      Layer := FModel.LayerStructure.ModflowLayerToDataSetLayer(Layer);
      DSAbove := FModel.LayerStructure.LayerGroups[Layer].DataArrayName;
      DSBelow := FModel.LayerStructure.LayerGroups[Layer + 1].DataArrayName;
      AssignedLayerDataSet.Formula := '(' + DSAbove + ' + ' + DSBelow + ')/2';
    end
    else
    begin
      AssignedLayerDataSet.Formula := '0';
      Assert(FVariableLayerIndicators <> nil);
      Assert(FVariableLayerIndicators[0] <> nil);
      if ScreenObject = nil then
      begin
        CreateOrRetrieveCellCenterScreenObject(ScreenObject);
//        ScreenObject.Name := ScreenObjectName;
      end;
      SetLength(ImportedData, FGrid.RowCount, FGrid.ColumnCount);
      for RowIndex := 0 to FGrid.RowCount - 1 do
      begin
        for ColumnIndex := 0 to FGrid.ColumnCount - 1 do
        begin
          Layer := FVariableLayerIndicators[0, RowIndex, ColumnIndex];
          Layer := FModel.LayerStructure.ModflowLayerToDataSetLayer(Layer);
          ImportedData[RowIndex, ColumnIndex] :=
            FGrid.LayerCenter(ColumnIndex, RowIndex, Layer);
        end;
      end;
      AssignRealValuesToCellCenters(AssignedLayerDataSet,
        ScreenObject, ImportedData);
    end;
  end;
end;

procedure TTransientArrayImporter.ReadVariableIntArray(
  var VariableIntArray: T3DIntArray);
begin
  if VariableIntArray = nil then
  begin
    SetLength(VariableIntArray, FModel.ModflowStressPeriods.Count);
  end;
  SetLength(VariableIntArray[FCurrentStressPeriod],
    FGrid.RowCount, FGrid.ColumnCount);
  ReadVariable2DIntArray(VariableIntArray[FCurrentStressPeriod]);
end;

procedure TTransientArrayImporter.ReadConstantIntArray(IntValue: Integer;
  var ConstantIntArray: TIntegerConstantArray);
var
  Index: Integer;
begin
  if ConstantIntArray = nil then
  begin
    SetLength(ConstantIntArray, FModel.ModflowStressPeriods.Count);
    for Index := 0 to Length(ConstantIntArray) - 1 do
    begin
      ConstantIntArray[Index].IsConstant := False;
      ConstantIntArray[Index].IntegerValue := 0;
    end;
  end;
  ConstantIntArray[FCurrentStressPeriod].IsConstant := True;
  ConstantIntArray[FCurrentStressPeriod].IntegerValue := IntValue;
end;

procedure TTransientArrayImporter.ReadRealVariableArray(
  var VariableArray: T3DDoubleArray);
begin
  if VariableArray = nil then
  begin
    SetLength(VariableArray, FModel.ModflowStressPeriods.Count);
  end;
  SetLength(VariableArray[FCurrentStressPeriod],
    FGrid.RowCount, FGrid.ColumnCount);
  Read2DRealArray(VariableArray[FCurrentStressPeriod]);
end;

procedure TTransientArrayImporter.ReadRealConstantArrayItem(Value: Double;
  var RealConstantArray: TRealConstantRecordArray);
var
  Index: Integer;
begin
  if RealConstantArray = nil then
  begin
    SetLength(RealConstantArray, FModel.ModflowStressPeriods.Count);
    for Index := 0 to Length(RealConstantArray) - 1 do
    begin
      RealConstantArray[Index].IsConstant := False;
      RealConstantArray[Index].RealValue := 0;
    end;
  end;
  RealConstantArray[FCurrentStressPeriod].IsConstant := True;
  RealConstantArray[FCurrentStressPeriod].RealValue := Value;
end;

procedure TTransientArrayImporter.CreateDataSet(StressPeriodIndex: Integer;
  const Root: string; DataType: TRbwDataType;
  var DataSet: TDataArray);
var
  NewName: string;
  Interpolator: TNearestPoint2DInterpolator;
begin
//  DataSet := TDataArray.Create(FModel);

  if StressPeriodIndex >= 0 then
  begin
    NewName := Root + GetStressPeriodString(StressPeriodIndex);
  end
  else
  begin
    NewName := Root;
  end;

  DataSet := FModel.CreateNewDataArray(TDataArray, NewName, '0',
    [], DataType, eaBlocks, dsoTop, '');


//  DataSet.Name := NewName;
//  DataSet.OnNameChange := FModel.DataArrayNameChange;
//  FModel.AddDataSet(DataSet);
//  DataSet.DataType := DataType;
//  DataSet.EvaluatedAt := eaBlocks;
//  DataSet.Orientation := dsoTop;
  DataSet.UpdateDimensions(FGrid.LayerCount, FGrid.RowCount, FGrid.ColumnCount);
//  FModel.CreateVariables(DataSet);
  Interpolator := TNearestPoint2DInterpolator.Create(nil);
  try
    DataSet.TwoDInterpolator := Interpolator;
  finally
    Interpolator.Free;
  end;
end;

constructor TTransientArrayImporter.Create(Importer: TModflow2005Importer;
  const PackageIdentifier: string; ZoneImporter: TMultZoneImporter);
begin
  inherited Create(Importer, PackageIdentifier);
  FZoneImporter := ZoneImporter;
  CurrentParameter := -1;
  FCurrentStressPeriod := -1;
  FParams:= TArrayParamArray.Create;
  FStressPeriods:= TArrayStressPeriodArray.Create;
end;

destructor TTransientArrayImporter.Destroy;
begin
  FStressPeriods.Free;
  FParams.Free;
  FQuadTree.Free;
  inherited;
end;

procedure TTransientArrayImporter.ImportSharedData(const ALabel: string;
  out Handled: boolean);
begin
  Handled := True;
  if ALabel = 'NP:' then
  begin
    ReadNumberOfParameters;
  end
  else if ALabel = 'PARNAM:' then
  begin
    ReadParameterName;
  end
  else if ALabel = 'PARTYP:' then
  begin
    ReadParameterType(FRequiredType);
  end
  else if ALabel = 'Parval:' then
  begin
    ReadParameterValue;
  end
  else if ALabel = 'NCLU:' then
  begin
    ReadNumberOfClusters;
  end
  else if ALabel = 'NUMINST:' then
  begin
    ReadNumberOfInstances;
  end
  else if ALabel = 'INAME(ILOC):' then
  begin
    ReadInstanceName;
  end
  else if ALabel = 'MULTIPLIER ARRAY AND ZONE ARRAY:' then
  begin
    ReadMultiplierAndZoneArrayNames;
  end
  else if ALabel = 'NUMBER OF ZONES:' then
  begin
    ReadNumberOfZones;
  end
  else if ALabel = 'ZONE VALUES:' then
  begin
    ReadZoneValues;
  end
  else if ALabel = 'Pname:' then
  begin
    ReadParameterForStressPeriod;
  end
  else if ALabel = 'Iname:' then
  begin
    ReadInstanceForStressPeriod;
  end
  else
  begin
    Handled := False;
  end;
end;

procedure TTransientArrayImporter.ReadInstanceForStressPeriod;
var
  Iname: string;
begin
  ReadLn(FImporter.FFile, Iname);
  Iname := Trim(Iname);
  FStressPeriods[FCurrentStressPeriod].Instances[CurrentParameter - 1] := Iname;
end;

procedure TTransientArrayImporter.ReadParameterForStressPeriod;
var
  PNAME: string;
begin
  ReadLn(FImporter.FFile, Pname);
  Pname := Trim(Pname);
  FStressPeriods[FCurrentStressPeriod].Parameters[CurrentParameter] := Pname;
  Inc(CurrentParameter);
end;

procedure TTransientArrayImporter.ReadZoneValues;
var
  Zone: Integer;
  Index: Integer;
begin
  for Index := 0 to ZoneCount - 1 do
  begin
    Read(FImporter.FFile, Zone);
    FParams[CurrentParameter].Instances[CurrentInstance].
      Clusters[CurrentCluster].Zones[Index] := Zone;
  end;
  Inc(CurrentCluster);
  if CurrentCluster = NCLU then
  begin
    Inc(CurrentInstance);
  end;
end;

procedure TTransientArrayImporter.ReadNumberOfZones;
begin
  Readln(FImporter.FFile, ZoneCount);
  SetLength(FParams[CurrentParameter].Instances[CurrentInstance].
    Clusters[CurrentCluster].Zones, ZoneCount);
end;

procedure TTransientArrayImporter.ReadMultiplierAndZoneArrayNames;
var
  ZoneName: string;
  MultiplierName: string;
begin
  Readln(FImporter.FFile, MultiplierName);
  Readln(FImporter.FFile, ZoneName);
  MultiplierName := Trim(MultiplierName);
  ZoneName := Trim(ZoneName);
  FParams[CurrentParameter].Instances[CurrentInstance].
    Clusters[CurrentCluster].MultiplierArray := MultiplierName;
  FParams[CurrentParameter].Instances[CurrentInstance].
    Clusters[CurrentCluster].ZoneArray := ZoneName;
  if UpperCase(ZoneName) = 'ALL' then
  begin
    Inc(CurrentCluster);
    if CurrentCluster = NCLU then
    begin
      Inc(CurrentInstance);
    end;
  end;
end;

procedure TTransientArrayImporter.ReadInstanceName;
var
  InstanceName: string;
begin
  Readln(FImporter.FFile, InstanceName);
  InstanceName := Trim(InstanceName);
  FParams[CurrentParameter].Instances[CurrentInstance].Name := InstanceName;
  CurrentCluster := 0;
end;

procedure TTransientArrayImporter.ReadNumberOfInstances;
var
  Index: Integer;
  NUMINST: Integer;
begin
  Readln(FImporter.FFile, NUMINST);
  if NUMINST = 0 then
  begin
    NUMINST := 1;
  end;
  FParams[CurrentParameter].ArrayLength := NUMINST;
  for Index := 0 to NUMINST - 1 do
  begin
    FParams[CurrentParameter].Instances[Index].ArrayLength := NCLU;
  end;
end;

procedure TTransientArrayImporter.ReadNumberOfClusters;
begin
  Readln(FImporter.FFile, NCLU);
  FParams[CurrentParameter].ArrayLength := 1;
  FParams[CurrentParameter].Instances[0].ArrayLength := NCLU;
end;

procedure TTransientArrayImporter.ReadParameterValue;
var
  Parval: Double;
begin
  Readln(FImporter.FFile, Parval);
  FParams[CurrentParameter].Parval := Parval;
end;

procedure TTransientArrayImporter.ReadParameterType(RequiredType: string);
var
  PARTYP: string;
begin
  Readln(FImporter.FFile, PARTYP);
  PARTYP := Trim(PARTYP);
  Assert(PARTYP = RequiredType);
  FParams[CurrentParameter].PARTYP := PARTYP;
end;

procedure TTransientArrayImporter.ReadParameterName;
var
  PNAME: string;
begin
  Readln(FImporter.FFile, PNAME);
  PNAME := Trim(PNAME);
  Inc(CurrentParameter);
  FParams[CurrentParameter].PARNAM := PNAME;
  CurrentInstance := 0;
  CurrentCluster := 0;
  FParams[CurrentParameter].ArrayLength := 1;
end;

procedure TTransientArrayImporter.ReadNumberOfParameters;
begin
  ReadLn(FImporter.FFile, NP);
  FParams.ArrayLength := NP;
end;

procedure TTransientArrayImporter.CreateScreenObjectsFromClusters(
  Param: TArrayParameterObject; var ObjectIndex: Integer;
  ScreenObjectList, ClusterList: TList;
  const ScreenObjectRoot, ImportedElevFormula: string;
  Package: TCustomTransientLayerPackageSelection);
var
  Cluster: TClusterObject;
  ClusterIndex: Integer;
  InstanceIndex: Integer;
  Instance: TArrayInstanceObject;
begin
  for InstanceIndex := 0 to Param.ArrayLength - 1 do
  begin
    Instance := Param[InstanceIndex];
    for ClusterIndex := 0 to Instance.ArrayLength - 1 do
    begin
      Cluster := Instance[ClusterIndex];
      CreateScreenObjectFromCluster(ClusterList, ScreenObjectList,
        ObjectIndex, Cluster, ScreenObjectRoot, ImportedElevFormula,
        Package);
    end;
  end;
end;

procedure TTransientArrayImporter.CreateTransientParam(
  Param: TArrayParameterObject);
var
  ArrayParam: TModflowTransientListParameter;
begin
  ArrayParam := FModel.ModflowTransientParameters.Add
    as TModflowTransientListParameter;
  ArrayParam.ParameterName := Param.PARNAM;
  Param.ModifiedParamName := ArrayParam.ParameterName;
  if AnsiCompareText(Param.PARTYP, 'RCH') = 0 then
  begin
    ArrayParam.ParameterType := ptRCH;
  end
  else if AnsiCompareText(Param.PARTYP, 'EVT') = 0 then
  begin
    ArrayParam.ParameterType := ptEVT;
  end
  else if AnsiCompareText(Param.PARTYP, 'ETS') = 0 then
  begin
    ArrayParam.ParameterType := ptETS;
  end
  else
  begin
    Assert(False);
  end;
  ArrayParam.Value := Param.Parval;
end;

procedure TRchImporter.AssignRechRateNonParam(RechargeName: string;
  NewItemNeeded: Boolean; var RchItem: TRchItem; RchBoundary: TRchBoundary;
  StressPeriodIndex: Integer);
begin
  if NewItemNeeded then
  begin
    RchItem := RchBoundary.Values.Add as TRchItem;
    RchItem.RechargeRate := RechargeName;
    RchItem.StartTime :=
      FModel.ModflowStressPeriods[StressPeriodIndex].StartTime;
    RchItem.EndTime := FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
  end
  else
  begin
    RchItem.EndTime := FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
  end;
end;

procedure TRchImporter.AssignRchLayerNonParam(NewItemNeeded: Boolean;
  var LayerItem: TRchLayerItem; RchBoundary: TRchBoundary;
  RechargeLayerName: string; StressPeriodIndex: Integer);
begin
  if FRchPackage.TimeVaryingLayers then
  begin
    if NewItemNeeded then
    begin
      LayerItem := RchBoundary.RechargeLayers.Add as TRchLayerItem;
      LayerItem.RechargeLayer := RechargeLayerName;
      LayerItem.StartTime :=
        FModel.ModflowStressPeriods[StressPeriodIndex].StartTime;
      LayerItem.EndTime :=
        FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
    end
    else
    begin
      LayerItem.EndTime :=
        FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
    end;
  end;
end;

procedure TRchImporter.CreateRechargeRateDataSet(StressPeriodIndex: Integer);
var
  RechargeDataSet: TDataArray;
  ScreenObject: TScreenObject;
  ValueList: TRealList;
  Values: T2DDoubleArray;
  RowIndex: Integer;
  ColIndex: Integer;
  Value: Double;
  MaxCount: integer;
  Root: string;
begin
  ScreenObject := nil;
  if not FReuseRecharge[StressPeriodIndex] then
  begin
    CreateDataSet(StressPeriodIndex, StrImportedRechargeSt,
      rdtDouble, RechargeDataSet);
    if (FConstantRecharge <> nil)
      and FConstantRecharge[StressPeriodIndex].IsConstant then
    begin
      RechargeDataSet.Formula :=
        FloatToStr(FConstantRecharge[StressPeriodIndex].RealValue);
    end
    else
    begin
      RechargeDataSet.Formula := '0';
      Assert(FVariableRecharge <> nil);
      Assert(FVariableRecharge[StressPeriodIndex] <> nil);

      ValueList := TRealList.Create;
      try
        ValueList.Sorted := True;
        Values := FVariableRecharge[StressPeriodIndex];
        MaxCount := Min((Length(Values) * Length(Values[0])) div 10, 100);
        for RowIndex := 0 to Length(Values) - 1 do
        begin
          for ColIndex := 0 to Length(Values[0]) - 1 do
          begin
            Value := Values[RowIndex,ColIndex];
            ValueList.AddUnique(Value);
            if ValueList.Count >= MaxCount then
            begin
              break;
            end;
          end;
          if ValueList.Count >= MaxCount then
          begin
            break;
          end;
        end;
        if ValueList.Count >= MaxCount then
        begin
          if ScreenObject = nil then
          begin
            CreateOrRetrieveCellCenterScreenObject(ScreenObject);
//            ScreenObject.Name := 'Imported_RCH_Values_SP_'
//              + GetStressPeriodString(StressPeriodIndex);
          end;
          AssignRealValuesToCellCenters(RechargeDataSet, ScreenObject,
            FVariableRecharge[StressPeriodIndex]);
        end
        else if ValueList.Count = 1 then
        begin
          RechargeDataSet.Formula :=
            FloatToStr(ValueList[0]);
        end
        else
        begin
          Root := 'Imported_RCH_Values_SP_'
            + GetStressPeriodString(StressPeriodIndex) + '_Obj_';
          CreateScreenObjectsAroundValues(Values, Root,
            RechargeDataSet, ValueList);
        end;
      finally
        ValueList.Free;
      end;
    end;
  end;
end;

procedure TRchImporter.AssignParamRechargeRate(ScreenObject: TScreenObject;
  StressPeriodIndex: Integer; Cluster: TClusterObject;
  Param: TArrayParameterObject);
var
  RchItem: TRchItem;
  ParamItem: TRchParamItem;
begin
  ParamItem := ScreenObject.ModflowRchBoundary.Parameters.
    GetParamByName(Param.ModifiedParamName) as TRchParamItem;
  if ParamItem = nil then
  begin
    ParamItem := ScreenObject.ModflowRchBoundary.
      Parameters.Add as TRchParamItem;
    ParamItem.Param.ParamName := Param.ModifiedParamName;
  end;
  RchItem := ParamItem.Param.Add as TRchItem;
  RchItem.StartTime := FModel.ModflowStressPeriods[StressPeriodIndex].StartTime;
  RchItem.EndTime := FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
  if AnsiCompareText(Cluster.MultiplierArray, 'NONE') = 0 then
  begin
    RchItem.RechargeRate := '1';
  end
  else
  begin
    RchItem.RechargeRate := FixArrayName(Cluster.MultiplierArray);
  end;
end;

procedure TRchImporter.AssignTimeVaryingLayer(ScreenObject: TScreenObject;
  RechargeLayerName: string; StressPeriodIndex: Integer);
var
  LayerItem: TRchLayerItem;
  RchBoundary: TRchBoundary;
begin
  if FRchPackage.TimeVaryingLayers then
  begin
    RchBoundary := ScreenObject.ModflowRchBoundary;
    if not FReuseLayerIndicator[StressPeriodIndex] then
    begin
      LayerItem := RchBoundary.RechargeLayers.Add as TRchLayerItem;
      LayerItem.RechargeLayer := RechargeLayerName;
      LayerItem.StartTime :=
        FModel.ModflowStressPeriods[StressPeriodIndex].StartTime;
      LayerItem.EndTime :=
        FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
    end
    else
    begin
      LayerItem := RchBoundary.RechargeLayers.
        Items[RchBoundary.RechargeLayers.Count - 1] as TRchLayerItem;
      LayerItem.EndTime :=
        FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
    end;
  end;
end;

procedure TTransientArrayImporter.GetParamInstanceForCurrentStressPeriod(
  var Instance: TArrayInstanceObject;
  Param: TArrayParameterObject; StressPeriod: TArrayStressPeriod);
var
  PNameIndex: Integer;
  InstanceName: string;
  ParamUsed: Boolean;
  InstanceIndex: Integer;
begin
  // In each stress period, identify whether the parameter was used.
  // If it was used, identify the intance used.
  ParamUsed := False;
  InstanceName := '';
  for PNameIndex := 0 to Length(StressPeriod.Parameters) - 1 do
  begin
    if AnsiCompareText(StressPeriod.Parameters[PNameIndex],
      Param.PARNAM) = 0 then
    begin
      ParamUsed := True;
      InstanceName := '';
      if PNameIndex < Length(StressPeriod.Instances) then
      begin
        InstanceName := StressPeriod.Instances[PNameIndex];
      end;
      break;
    end;
  end;
  Instance := nil;
  if ParamUsed then
  begin
    for InstanceIndex := 0 to Param.GetArrayLength - 1 do
    begin
      Instance := Param.Instances[InstanceIndex];
      if AnsiCompareText(InstanceName, Instance.Name) = 0 then
      begin
        break;
      end;
      Instance := nil;
    end;
  end;
end;

procedure TRchImporter.ReuseRchStressPeriodWithParameters(
  StressPeriodIndex: Integer; Param: TArrayParameterObject;
  ScreenObjectList: TList; const RechargeLayerName: string);
var
  RchItem, NewRchItem: TRchItem;
  ParamItem: TRchParamItem;
  ScreenObject: TScreenObject;
  SO_Index: Integer;
begin
  for SO_Index := 0 to ScreenObjectList.Count - 1 do
  begin
    ScreenObject := ScreenObjectList[SO_Index];
    ParamItem := ScreenObject.ModflowRchBoundary.Parameters.
      GetParamByName(Param.ModifiedParamName) as TRchParamItem;
    if ParamItem <> nil then
    begin
      if ParamItem.Param.Count > 0 then
      begin
        RchItem := ParamItem.Param.Items[ParamItem.Param.Count - 1] as TRchItem;
        if RchItem.EndTime =
          FModel.ModflowStressPeriods[StressPeriodIndex].StartTime then
        begin
          if FRchPackage.TimeVaryingLayers then
          begin
            NewRchItem := ParamItem.Param.Add as TRchItem;
            NewRchItem.Assign(RchItem);
            NewRchItem.StartTime :=
              FModel.ModflowStressPeriods[StressPeriodIndex].StartTime;
            NewRchItem.EndTime :=
              FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
          end
          else
          begin
            RchItem.EndTime :=
              FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
          end;
          AssignTimeVaryingLayer(ScreenObject,
            RechargeLayerName, StressPeriodIndex);
        end;
      end;
    end;
  end;
end;

procedure TTransientArrayImporter.EvaluateTimeVaryingLayers(
  Package: TCustomTransientLayerPackageSelection);
var
  Index: Integer;
  TimeVaryingLayers: Boolean;
begin
  TimeVaryingLayers := False;
  if Package.LayerOption = loSpecified then
  begin
    for Index := 1 to Length(FReuseLayerIndicator) - 1 do
    begin
      if not FReuseLayerIndicator[Index] then
      begin
        TimeVaryingLayers := True;
        break;
      end;
    end;
  end;
  Package.TimeVaryingLayers := TimeVaryingLayers;
end;

procedure TRchImporter.SetRechargeOption;
var
  Option: Integer;
begin
  Option := NRCHOP - 1;
  if Option < 0 then
  begin
    Option := 0;
  end;
  if Option > 2 then
  begin
    Option := 2;
  end;
  FRchPackage.LayerOption := TLayerOption(Option);
end;

procedure TTransientArrayImporter.CreateTimeVaryingAssignedLayerDataSet(
  StressPeriodIndex: Integer; const DataSetRoot, ScreenObjectRoot: string;
  Package: TCustomTransientLayerPackageSelection);
var
  AssignedLayerDataSet: TDataArray;
  ScreenObject: TScreenObject;
  ValueList: TIntegerList;
  Values: T2DIntArray;
  MaxCount: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Value: Integer;
  Root: string;
begin
  ScreenObject := nil;
  if Package.TimeVaryingLayers then
  begin
    if not FReuseLayerIndicator[StressPeriodIndex] then
    begin
      CreateDataSet(StressPeriodIndex, DataSetRoot, rdtInteger,
        AssignedLayerDataSet);
      if (FConstantLayerIndicators <> nil)
        and FConstantLayerIndicators[StressPeriodIndex].IsConstant then
      begin
        AssignedLayerDataSet.Formula :=
          IntToStr(FConstantLayerIndicators[StressPeriodIndex].IntegerValue);
      end
      else
      begin
        AssignedLayerDataSet.Formula := '0';
        Assert(FVariableLayerIndicators <> nil);
        Assert(FVariableLayerIndicators[StressPeriodIndex] <> nil);

        ValueList := TIntegerList.Create;
        try
          ValueList.Sorted := True;
          Values := FVariableLayerIndicators[StressPeriodIndex];
          MaxCount := Min((Length(Values) * Length(Values[0])) div 10, 100);
          for RowIndex := 0 to Length(Values) - 1 do
          begin
            for ColIndex := 0 to Length(Values[0]) - 1 do
            begin
              Value := Values[RowIndex,ColIndex];
              ValueList.AddUnique(Value);
              if ValueList.Count >= MaxCount then
              begin
                break;
              end;
            end;
            if ValueList.Count >= MaxCount then
            begin
              break;
            end;
          end;
          if ValueList.Count >= MaxCount then
          begin
            if ScreenObject = nil then
            begin
              CreateOrRetrieveCellCenterScreenObject(ScreenObject);
//              ScreenObject.Name := ScreenObjectRoot
//                + GetStressPeriodString(StressPeriodIndex);
            end;
            AssignIntegerValuesToCellCenters(AssignedLayerDataSet, ScreenObject,
              FVariableLayerIndicators[StressPeriodIndex]);
          end
          else if ValueList.Count = 1 then
          begin
            AssignedLayerDataSet.Formula :=
              IntToStr(ValueList[0]);
          end
          else
          begin
            Root := ScreenObjectRoot
              + GetStressPeriodString(StressPeriodIndex) + '_Obj_';
            CreateScreenObjectsAroundValues(Values, Root,
              AssignedLayerDataSet, ValueList);
          end;
        finally
          ValueList.Free;
        end
      end;
    end;
  end;
end;

procedure TTransientArrayImporter.CreateScreenObjectFromCluster(ClusterList,
  ScreenObjectList: TList; var ObjectIndex: Integer; Cluster: TClusterObject;
  const ScreenObjectRoot, ImportedElevFormula: string;
  Package: TCustomTransientLayerPackageSelection);
var
  ScreenObject: TScreenObject;
  ZoneIndex: Integer;
  ZoneValue: Integer;
  InnerZoneIndex: Integer;
  ZoneArray: T2DIntArray;
  ZoneName: string;
begin
  if not FindSimilarClusterAndAddScreenObject(Cluster,
    ClusterList, ScreenObjectList) then
  begin
    Inc(ObjectIndex);
    if UpperCase(Cluster.ZoneArray) = 'ALL' then
    begin
      ScreenObject := CreateScreenObjectAroundGrid(ScreenObjectRoot
        + IntToStr(ObjectIndex));
      ScreenObject.ElevationFormula := StrModelTop;
    end
    else
    begin
      ScreenObject := nil;
      ZoneName := FixArrayName(Cluster.ZoneArray);
      if Length(ZoneName) > 10 then
      begin
        SetLength(ZoneName, 10);
      end;
      ZoneIndex := FZoneImporter.IndexOfZone(ZoneName);
      Assert(ZoneIndex >= 0);
      if FZoneImporter.ConstantZone(ZoneIndex, ZoneValue) then
      begin
        if ZoneValue <> 0 then
        begin
          for InnerZoneIndex := 0 to Length(Cluster.Zones) - 1 do
          begin
            if ZoneValue = Cluster.Zones[InnerZoneIndex] then
            begin
              ScreenObject := CreateScreenObjectAroundGrid(ScreenObjectRoot
                + IntToStr(ObjectIndex));
              ScreenObject.ElevationFormula := StrModelTop;
              break;
            end;
          end;
        end;
      end
      else
      begin
        ZoneArray := FZoneImporter.ZoneArray(ZoneIndex);
        ScreenObject := CreateScreenObjectAroundZones(ZoneArray, Cluster,
          ScreenObjectRoot + IntToStr(ObjectIndex));
        ScreenObject.ElevationFormula := StrModelTop;
      end;
    end;
    if ScreenObject <> nil then
    begin
      ClusterList.Add(Cluster);
      ScreenObjectList.Add(ScreenObject);
      CreateBoundary(ScreenObject);
      if (Package.LayerOption = loSpecified)
        and not Package.TimeVaryingLayers then
      begin
        ScreenObject.ElevationFormula := ImportedElevFormula;
      end
      else
      begin
        ScreenObject.ElevationFormula := '('
          + FModel.LayerStructure.LayerGroups[0].DataArrayName
          + ' + '
          + FModel.LayerStructure.LayerGroups[1].DataArrayName
          + ') / 2'
      end;
    end;
  end;
end;

function TTransientArrayImporter.GetStressPeriodString(
  StressPeriodIndex: Integer): string;
var
  StressPeriodMaxLength: Integer;
begin
  result := IntToStr(FModel.ModflowStressPeriods.Count);
  StressPeriodMaxLength := Length(result);
  result := IntToStr(StressPeriodIndex + 1);
  while Length(result) < StressPeriodMaxLength do
  begin
    result := '0' + result;
  end;
end;

function TTransientArrayImporter.CreateScreenObjectAroundGrid(
  const Name: string): TScreenObject;
begin
  Result := CreateScreenObject(Name);
  result.Capacity := 5;
  result.AddPoint(FGrid.TwoDElementCorner(0, 0), False);
  result.AddPoint(FGrid.TwoDElementCorner(0, FGrid.RowCount), False);
  result.AddPoint(FGrid.TwoDElementCorner(FGrid.ColumnCount,
    FGrid.RowCount), False);
  result.AddPoint(FGrid.TwoDElementCorner(FGrid.ColumnCount, 0), False);
  result.AddPoint(FGrid.TwoDElementCorner(0, 0), False);
end;

procedure TTransientArrayImporter.InitializeQuadTree;
var
  CornerPoint: TPoint2d;
  XMin, XMax, YMin, YMax: double;
begin
  if FQuadTree = nil then
  begin
    FQuadTree := TRbwQuadTree.Create(nil);

    CornerPoint := FGrid.TwoDElementCorner(0,0);
    XMin := CornerPoint.x;
    XMax := XMin;
    YMin := CornerPoint.y;
    YMax := YMin;

    CornerPoint := FGrid.TwoDElementCorner(FGrid.ColumnCount,0);
    XMin := Min(XMin, CornerPoint.x);
    XMax := Max(XMax, CornerPoint.x);
    YMin := Min(YMin, CornerPoint.y);
    YMax := Max(YMax, CornerPoint.y);

    CornerPoint := FGrid.TwoDElementCorner(FGrid.ColumnCount,FGrid.RowCount);
    XMin := Min(XMin, CornerPoint.x);
    XMax := Max(XMax, CornerPoint.x);
    YMin := Min(YMin, CornerPoint.y);
    YMax := Max(YMax, CornerPoint.y);

    CornerPoint := FGrid.TwoDElementCorner(0,FGrid.RowCount);
    XMin := Min(XMin, CornerPoint.x);
    XMax := Max(XMax, CornerPoint.x);
    YMin := Min(YMin, CornerPoint.y);
    YMax := Max(YMax, CornerPoint.y);

    FQuadTree.XMin := XMin;
    FQuadTree.XMax := XMax;
    FQuadTree.YMin := YMin;
    FQuadTree.YMax := YMax;
  end
  else
  begin
    FQuadTree.Clear;
  end;
end;

procedure TTransientArrayImporter.ImportSegments(Sender: TObject;
  const Segments: TLine2DArray);
var
  Index: Integer;
  Segment: TLine2D;
  Line1, Line2, Line3: TPointList;
  X1, Y1: double;
  X2, Y2: double;
  APoint: TPoint2D;
  PointIndex: Integer;
  APointer: Pointer;
  function SamePoint(X1, X2, Y1, Y2: double): boolean;
  begin
    result := Sqr(X1-X2) + Sqr(Y1-Y2) < FEpsilon;
  end;
begin
  for Index := 0 to Length(Segments) - 1 do
  begin
    Segment := Segments[Index];
    X1 := Segment[1].x;
    Y1 := Segment[1].y;
    if FQuadTree.Count > 0 then
    begin
      FQuadTree.FirstNearestPoint(X1, Y1, APointer);
      Line1 := APointer;
    end
    else
    begin
      Line1 := nil;
    end;
    if Line1 <> nil then
    begin
      if not SamePoint(X1, Segment[1].x, Y1, Segment[1].Y) then
      begin
        Line1 := nil;
      end;
    end;

    X2 := Segment[2].x;
    Y2 := Segment[2].y;
    if FQuadTree.Count > 0 then
    begin
      FQuadTree.FirstNearestPoint(X2, Y2, APointer);
      Line2 := APointer;
    end
    else
    begin
      Line2 := nil;
    end;
    if Line2 <> nil then
    begin
      if not SamePoint(X2, Segment[2].x, Y2, Segment[2].Y) then
      begin
        Line2 := nil;
      end;
    end;
    if (Line1 = nil) and (Line2 = nil) then
    begin
      Line1 := TPointList.Create;
      FPointLists.Add(Line1);
      Line1.Add(Segment[1]);
      Line1.Add(Segment[2]);
      FQuadTree.AddPoint(Segment[1].x, Segment[1].y, Line1);
      FQuadTree.AddPoint(Segment[2].x, Segment[2].y, Line1);
    end
    else if (Line1 = nil) then
    begin
      APoint := Line2.Points[Line2.Count-1];
      if SamePoint(Segment[2].x, APoint.x, Segment[2].y, APoint.y) then
      begin
        Line2.Add(Segment[1]);
      end
      else
      begin
        APoint := Line2.Points[0];
        Assert(SamePoint(Segment[2].x, APoint.x, Segment[2].y, APoint.y));
        Line2.Insert(0, Segment[1]);
      end;
      FQuadTree.RemovePoint(X2, Y2, Line2);
      FQuadTree.AddPoint(Segment[1].x, Segment[1].y, Line2);
    end
    else if (Line2 = nil) then
    begin
      APoint := Line1.Points[Line1.Count-1];
      if SamePoint(Segment[1].x, APoint.x, Segment[1].y, APoint.y) then
      begin
        Line1.Add(Segment[2]);
      end
      else
      begin
        APoint := Line1.Points[0];
        Assert(SamePoint(Segment[1].x, APoint.x, Segment[1].y, APoint.y));
        Line1.Insert(0, Segment[2]);
      end;
      FQuadTree.RemovePoint(X1, Y1, Line1);
      FQuadTree.AddPoint(Segment[2].x, Segment[2].y, Line1);
    end
    else if (Line2 = Line1) then
    begin
      APoint := Line1.Points[Line1.Count-1];
      if SamePoint(Segment[1].x, APoint.x, Segment[1].y, APoint.y) then
      begin
        Line1.Add(Segment[2]);
      end
      else
      begin
        APoint := Line1.Points[0];
        Assert(SamePoint(Segment[1].x, APoint.x, Segment[1].y, APoint.y));
        Line1.Insert(0, Segment[2]);
      end;
      FQuadTree.RemovePoint(X1, Y1, Line1);
      FQuadTree.RemovePoint(X2, Y2, Line1);
    end
    else
    begin
      APoint := Line1.Points[Line1.Count-1];
      if   (SamePoint(Segment[1].x, APoint.x, Segment[1].y, APoint.y))
        or (SamePoint(Segment[2].x, APoint.x, Segment[2].y, APoint.y)) then
      begin
        // Add to end of line 1
        if Line1.Capacity < Line1.Count + Line2.Count then
        begin
          Line1.Capacity := Line1.Count + Line2.Count
        end;
        APoint := Line2.Points[Line2.Count-1];
        if   (SamePoint(Segment[1].x, APoint.x, Segment[1].y, APoint.y))
          or (SamePoint(Segment[2].x, APoint.x, Segment[2].y, APoint.y)) then
        begin
          APoint := Line2.Points[0];
          FQuadTree.RemovePoint(APoint.x, APoint.y, Line2);
          // Add end of line 2 to end of line 1
          for PointIndex := Line2.Count - 1 downto 0 do
          begin
            Line1.Add(Line2.Points[PointIndex]);
          end;
        end
        else
        begin
          APoint := Line2.Points[Line2.Count - 1];
          FQuadTree.RemovePoint(APoint.x, APoint.y, Line2);
          // Join beginning of line 2 to end of line 1
          for PointIndex := 0 to Line2.Count - 1 do
          begin
            Line1.Add(Line2.Points[PointIndex]);
          end;
        end;
        FQuadTree.RemovePoint(X1, Y1, Line1);
        FQuadTree.RemovePoint(X2, Y2, Line2);
        FQuadTree.AddPoint(APoint.x, APoint.y, Line1);
        FPointLists.Remove(Line2);
      end
      else
      begin
        APoint := Line2.Points[Line2.Count-1];
        if   (SamePoint(Segment[1].x, APoint.x, Segment[1].y, APoint.y))
          or (SamePoint(Segment[2].x, APoint.x, Segment[2].y, APoint.y)) then
        begin
          // Add beginning of line 1 to end of line2
          if Line2.Capacity < Line1.Count + Line2.Count then
          begin
            Line2.Capacity := Line1.Count + Line2.Count;
          end;
          APoint := Line1.Points[Line1.Count - 1];
          FQuadTree.RemovePoint(APoint.x, APoint.y, Line1);
          for PointIndex := 0 to Line1.Count - 1 do
          begin
            Line2.Add(Line1.Points[PointIndex]);
          end;
          FQuadTree.RemovePoint(X1, Y1, Line1);
          FQuadTree.RemovePoint(X2, Y2, Line2);
          FQuadTree.AddPoint(APoint.x, APoint.y, Line2);
          FPointLists.Remove(Line1);
        end
        else
        begin
          // Join beginning of line 1 to beginning of line 2
          Line3 := TPointList.Create;
          FPointLists.Add(Line3);
          Line3.Capacity := Line1.Count + Line2.Count;
          for PointIndex := Line1.Count - 1 downto 0 do
          begin
            Line3.Add(Line1.Points[PointIndex]);
          end;
          for PointIndex := 0 to Line2.Count - 1 do
          begin
            Line3.Add(Line2.Points[PointIndex]);
          end;
          FQuadTree.RemovePoint(X1, Y1, Line1);
          FQuadTree.RemovePoint(X2, Y2, Line2);

          APoint := Line1.Points[Line1.Count - 1];
          FQuadTree.RemovePoint(APoint.x, APoint.y, Line1);
          FQuadTree.AddPoint(APoint.x, APoint.y, Line3);

          APoint := Line2.Points[Line2.Count - 1];
          FQuadTree.RemovePoint(APoint.x, APoint.y, Line2);
          FQuadTree.AddPoint(APoint.x, APoint.y, Line3);
          FPointLists.Remove(Line1);
          FPointLists.Remove(Line2);
        end;
      end;
    end;
  end;
end;

procedure TTransientArrayImporter.CreateScreenObjectsAroundValues(
  Values: T2DDoubleArray; const Root: string; DataArray: TDataArray;
  ValueList: TRealList);
var
  RowIndex: Integer;
  ColIndex: Integer;

  Value: Double;
  ContourGrid: T2DGrid;
  MaxRow: Integer;
  MaxCol: Integer;
  ContourCreator: TContourCreator;
  Index: Integer;
  Capacity: Integer;
  PointListIndex: Integer;
  PointList: TPointList;
  ScreenObject: TScreenObject;
  PointIndex: Integer;
  ValueIndex: Integer;
  DSIndex: Integer;
begin
  ContourGrid := FGrid.ContourGrid(eaBlocks, msModflow, vdTop, 0);
  Assert(ContourGrid <> nil);
  MaxRow := Length(ContourGrid[0]) - 1;
  MaxCol := Length(ContourGrid) - 1;
  Assert(ValueList.Count > 1);

  for RowIndex := 0 to MaxRow do
  begin
    for ColIndex := 0 to MaxCol do
    begin
      ContourGrid[ColIndex,RowIndex].Value := 0;
      ContourGrid[ColIndex,RowIndex].Active := True;
    end;
  end;

  FPointLists:= TObjectList.Create;

  FEpsilon := FGrid.ColumnWidth[0];
  for Index := 1 to FGrid.ColumnCount - 1 do
  begin
    FEpsilon := Min(FEpsilon, FGrid.ColumnWidth[Index]);
  end;
  for Index := 0 to FGrid.RowCount - 1 do
  begin
    FEpsilon := Min(FEpsilon, FGrid.RowWidth[Index]);
  end;
  FEpsilon := Sqr(FEpsilon/4);

  ContourCreator:= TContourCreator.Create;
  try
    ContourCreator.EvaluatedAt := eaBlocks;
    ContourCreator.Grid := ContourGrid;
    ContourCreator.OnExtractSegments := ImportSegments;
    ContourCreator.Value := 1;

    for ValueIndex := 0 to ValueList.Count - 1 do
    begin
      ScreenObject := CreateScreenObject(Root + IntToStr(ValueIndex+1));
      ScreenObject.ElevationCount := ecZero;
      Value := ValueList[ValueIndex];
      DSIndex := ScreenObject.AddDataSet(DataArray);
      ScreenObject.DataSetFormulas[DSIndex] :=
        FloatToStr(Value);

      for RowIndex := 0 to Length(Values) - 1 do
      begin
        for ColIndex := 0 to Length(Values[0]) - 1 do
        begin

          if Value = Values[RowIndex,ColIndex] then
          begin
            ContourGrid[ColIndex+1,RowIndex+1].Value := 2;
          end
          else
          begin
            ContourGrid[ColIndex+1,RowIndex+1].Value := 0;
          end;
        end;
      end;
      InitializeQuadTree;
      FPointLists.Clear;

      ContourCreator.ExtractContour;

      Assert(FPointLists.Count> 0);
      Capacity := 0;
      for PointListIndex := 0 to FPointLists.Count - 1 do
      begin
        PointList := FPointLists[PointListIndex];
        Capacity := Capacity + PointList.Count;
      end;
      ScreenObject.Capacity := ScreenObject.Capacity + Capacity;
      for PointListIndex := 0 to FPointLists.Count - 1 do
      begin
        PointList := FPointLists[PointListIndex];
        for PointIndex := 0 to PointList.Count - 1 do
        begin
          if (PointIndex > 0) and (PointIndex < PointList.Count - 1) then
          begin
            if not Collinear(PointList.Points[PointIndex-1],
              PointList.Points[PointIndex],
              PointList.Points[PointIndex+1]) then
            begin
              ScreenObject.AddPoint(PointList.Points[PointIndex], False);
            end;
          end
          else
          begin
            ScreenObject.AddPoint(PointList.Points[PointIndex], PointIndex=0);
          end;
        end;
      end
    end;
  finally
    ContourCreator.Free;
    FPointLists.Free;
  end;
end;

procedure TTransientArrayImporter.CreateScreenObjectsAroundValues(
  Values: T2DIntArray; const Root: string; DataArray: TDataArray;
  ValueList: TIntegerList);
var
  RowIndex: Integer;
  ColIndex: Integer;

  Value: integer;
  ContourGrid: T2DGrid;
  MaxRow: Integer;
  MaxCol: Integer;
  ContourCreator: TContourCreator;
  Index: Integer;
  Capacity: Integer;
  PointListIndex: Integer;
  PointList: TPointList;
  ScreenObject: TScreenObject;
  PointIndex: Integer;
  ValueIndex: Integer;
  DSIndex: Integer;
//  MinValue: double;
//  MaxValue: double;
begin
  ContourGrid := FGrid.ContourGrid(eaBlocks, msModflow, vdTop, 0);
  Assert(ContourGrid <> nil);
  MaxRow := Length(ContourGrid[0]) - 1;
  MaxCol := Length(ContourGrid) - 1;
  Assert(ValueList.Count > 1);

  for RowIndex := 0 to MaxRow do
  begin
    for ColIndex := 0 to MaxCol do
    begin
      ContourGrid[ColIndex,RowIndex].Value := 0;
      ContourGrid[ColIndex,RowIndex].Active := True;
    end;
  end;

  FPointLists:= TObjectList.Create;

  FEpsilon := FGrid.ColumnWidth[0];
  for Index := 1 to FGrid.ColumnCount - 1 do
  begin
    FEpsilon := Min(FEpsilon, FGrid.ColumnWidth[Index]);
  end;
  for Index := 0 to FGrid.RowCount - 1 do
  begin
    FEpsilon := Min(FEpsilon, FGrid.RowWidth[Index]);
  end;
  FEpsilon := Sqr(FEpsilon/4);

  ContourCreator:= TContourCreator.Create;
  try
    ContourCreator.EvaluatedAt := eaBlocks;
    ContourCreator.Grid := ContourGrid;
    ContourCreator.OnExtractSegments := ImportSegments;
    ContourCreator.Value := 1;

    for ValueIndex := 0 to ValueList.Count - 1 do
    begin
      ScreenObject := CreateScreenObject(Root + IntToStr(ValueIndex+1));
      ScreenObject.ElevationCount := ecZero;
      Value := ValueList[ValueIndex];
      DSIndex := ScreenObject.AddDataSet(DataArray);
      ScreenObject.DataSetFormulas[DSIndex] :=
        FloatToStr(Value);

      for RowIndex := 0 to Length(Values) - 1 do
      begin
        for ColIndex := 0 to Length(Values[0]) - 1 do
        begin

          if Value = Values[RowIndex,ColIndex] then
          begin
            ContourGrid[ColIndex+1,RowIndex+1].Value := 2;
          end
          else
          begin
            ContourGrid[ColIndex+1,RowIndex+1].Value := 0;
          end;
        end;
      end;
      InitializeQuadTree;
      FPointLists.Clear;

      ContourCreator.ExtractContour;

      Assert(FPointLists.Count> 0);
      Capacity := 0;
      for PointListIndex := 0 to FPointLists.Count - 1 do
      begin
        PointList := FPointLists[PointListIndex];
        Capacity := Capacity + PointList.Count;
      end;
      ScreenObject.Capacity := ScreenObject.Capacity + Capacity;
      for PointListIndex := 0 to FPointLists.Count - 1 do
      begin
        PointList := FPointLists[PointListIndex];
        for PointIndex := 0 to PointList.Count - 1 do
        begin
          if (PointIndex > 0) and (PointIndex < PointList.Count - 1) then
          begin
            if not Collinear(PointList.Points[PointIndex-1],
              PointList.Points[PointIndex],
              PointList.Points[PointIndex+1]) then
            begin
              ScreenObject.AddPoint(PointList.Points[PointIndex], False);
            end;
          end
          else
          begin
            ScreenObject.AddPoint(PointList.Points[PointIndex], PointIndex=0);
          end;
        end;
      end
    end;
  finally
    ContourCreator.Free;
    FPointLists.Free;
  end;
end;

function TTransientArrayImporter.CreateScreenObjectAroundZones(
  ZoneArray: T2DIntArray; Cluster: TClusterObject;
  const Name: string): TScreenObject;
var
  ContourGrid: T2DGrid;
  ColIndex: Integer;
  MaxRow: Integer;
  MaxCol: Integer;
  RowIndex: Integer;
  ZoneIndex: Integer;
  ContourCreator: TContourCreator;
  Index: Integer;
  PointListIndex: Integer;
  Capacity: Integer;
  PointList: TPointList;
  PointIndex: Integer;
  ZoneExists: boolean;
begin
  InitializeQuadTree;
  Result := CreateScreenObject(Name);
  Result.ElevationCount := ecOne;
  ContourGrid := FGrid.ContourGrid(eaBlocks, msModflow, vdTop, 0);
  Assert(ContourGrid <> nil);
  MaxRow := Length(ContourGrid[0]) - 1;
  MaxCol := Length(ContourGrid) - 1;
  for ColIndex := 0 to MaxCol do
  begin
    for RowIndex := 0 to MaxRow do
    begin
      ContourGrid[ColIndex,RowIndex].Value := 0;
      ContourGrid[ColIndex,RowIndex].Active := True;
    end;
  end;

  ZoneExists := False;
  for RowIndex := 0 to Length(ZoneArray) - 1 do
  begin
    for ColIndex := 0 to Length(ZoneArray[0]) - 1 do
    begin
      for ZoneIndex := 0 to Length(Cluster.Zones) - 1 do
      begin
        if ZoneArray[RowIndex,ColIndex] = Cluster.Zones[ZoneIndex] then
        begin
          ContourGrid[ColIndex+1,RowIndex+1].Value := 2;
          ZoneExists := True;
          break;
        end;
      end;
    end;
  end;

  if not ZoneExists then
  begin
    result.Deleted := True;;
    Exit;
  end;

  FPointLists:= TObjectList.Create;
  ContourCreator:= TContourCreator.Create;
  try
    ContourCreator.EvaluatedAt := eaBlocks;
    FEpsilon := FGrid.ColumnWidth[0];
    for Index := 1 to FGrid.ColumnCount - 1 do
    begin
      FEpsilon := Min(FEpsilon, FGrid.ColumnWidth[Index]);
    end;
    for Index := 0 to FGrid.RowCount - 1 do
    begin
      FEpsilon := Min(FEpsilon, FGrid.RowWidth[Index]);
    end;
    FEpsilon := Sqr(FEpsilon/4);

    ContourCreator.Grid := ContourGrid;
    ContourCreator.Value := 1;
    ContourCreator.OnExtractSegments := ImportSegments;
    ContourCreator.ExtractContour;

    Assert(FPointLists.Count> 0);
    Capacity := 0;
    for PointListIndex := 0 to FPointLists.Count - 1 do
    begin
      PointList := FPointLists[PointListIndex];
      Capacity := Capacity + PointList.Count;
    end;
    Result.Capacity := Capacity;
    for PointListIndex := 0 to FPointLists.Count - 1 do
    begin
      PointList := FPointLists[PointListIndex];
      for PointIndex := 0 to PointList.Count - 1 do
      begin
        if (PointIndex > 0) and (PointIndex < PointList.Count - 1) then
        begin
          if not Collinear(PointList.Points[PointIndex-1],
            PointList.Points[PointIndex],
            PointList.Points[PointIndex+1]) then
          begin
            result.AddPoint(PointList.Points[PointIndex], False);
          end;
        end
        else
        begin
          result.AddPoint(PointList.Points[PointIndex], PointIndex=0);
        end;
      end;
    end;
  finally
    ContourCreator.Free;
    FPointLists.Free;
  end;
end;

procedure TTransientArrayImporter.InitializeCurrentStressPeriod(
  ReadParamArray: Integer);
begin
  if FParams.ArrayLength > 0 then
  begin
    FStressPeriods[FCurrentStressPeriod].Reuse := ReadParamArray < 0;
    if (ReadParamArray > 0) then
    begin
      SetLength(FStressPeriods[FCurrentStressPeriod].Parameters,
        ReadParamArray);
      SetLength(FStressPeriods[FCurrentStressPeriod].Instances,
        ReadParamArray);
    end;
  end;
end;

procedure TTransientArrayImporter.InitializeReuseLayerIndicator;
var
  Index: Integer;
begin
  if FReuseLayerIndicator = nil then
  begin
    SetLength(FReuseLayerIndicator, FModel.ModflowStressPeriods.Count);
    for Index := 0 to Length(FReuseLayerIndicator) - 1 do
    begin
      FReuseLayerIndicator[Index] := False;
    end;
  end;
end;

procedure TRchImporter.InitializeReuseRecharge;
var
  Index: Integer;
begin
  if FReuseRecharge = nil then
  begin
    SetLength(FReuseRecharge, FModel.ModflowStressPeriods.Count);
    for Index := 0 to Length(FReuseRecharge) - 1 do
    begin
      FReuseRecharge[Index] := False;
    end;
  end;
end;

procedure TTransientArrayImporter.InitializeStressPeriods;
begin
  FStressPeriods.ArrayLength :=
    FModel.ModflowStressPeriods.Count;
end;

function TTransientArrayImporter.FindSimilarClusterAndAddScreenObject(
  ACluster: TClusterObject;
  ClusterList, ScreenObjectList: TList): boolean;
var
  Index: integer;
  Cluster: TClusterObject;
begin
  result := False;
  for Index := 0 to ClusterList.Count - 1 do
  begin
    Cluster := ClusterList[Index];
    result := ACluster.SimilarZone(Cluster);
    if result then
    begin
      ClusterList.Add(ACluster);
      ScreenObjectList.Add(ScreenObjectList[Index]);
      break;
    end;
  end;
end;

procedure TRchImporter.HandlePackage;
var
  StressPeriodIndex: Integer;
  ScreenObject: TScreenObject;
  RechargeLayerDataSet: TDataArray;
  NewItemNeeded: Boolean;
  RchBoundary: TRchBoundary;
  RchItem: TRchItem;
  LayerItem: TRchLayerItem;
  RechargeName: string;
  RechargeLayerName: string;
  ParamIndex: Integer;
  Param: TArrayParameterObject;
  Instance: TArrayInstanceObject;
  ClusterIndex: Integer;
  Cluster: TClusterObject;
  ObjectIndex: integer;
  ClusterList: TList;
  ScreenObjectList: TList;
  StressPeriod: TArrayStressPeriod;
  ScreenObIndex: Integer;
begin
  if (FCurrentStressPeriod < 0) or
    (FCurrentStressPeriod < FModel.ModflowStressPeriods.Count -1) then
  begin
    Exit;
  end;
  inherited;
  FRchPackage := FModel.ModflowPackages.RchPackage;
  FRchPackage.IsSelected := True;
  FRchPackage.Comments := FComments;
  SetRechargeOption;
  EvaluateTimeVaryingLayers(FRchPackage);

  CreateAssignedLayerDataSet(FRchPackage, StrImportedRechargeEl,
    'Imported_RCH_Elevation', RechargeLayerDataSet);
//  AssignSteadyRechargeLayerDataSet(RechargeLayerDataSet);

  if NP > 0 then
  begin
    ClusterList := TList.Create;
    ScreenObjectList := TList.Create;
    try
      ObjectIndex := 0;
      for ParamIndex := 0 to FParams.ArrayLength - 1 do
      begin
        Param := FParams[ParamIndex];
        CreateTransientParam(Param);

        ScreenObjectList.Clear;
        ClusterList.Clear;
        CreateScreenObjectsFromClusters(Param, ObjectIndex,
          ScreenObjectList, ClusterList, 'ImportedRchParam_',
          StrImportedRechargeEl, FRchPackage);
        // At this point, ScreenObjectList contains each TScreenObject
        // that has been created for a particular parameter
        // and Cluster list contains a TClusterObject that has the
        // zone array and zones used to define the geometry of the
        // TScreenObject but not necessarily the corresponding Multiplier
        // array.

        // What need's to be done is to identify the stress periods in which
        // each parameter instance was used and find the corresponding
        // TScreenObjects and assign the formulas for those
        // stress periods using the appropriate multiplier arrays.

        // Loop over stress periods
        for StressPeriodIndex := 0 to FStressPeriods.ArrayLength - 1 do
        begin
          if ParamIndex = 0 then
          begin
            CreateTimeVaryingAssignedLayerDataSet(StressPeriodIndex,
              StrImportedRechargeLa, 'Imported_RCH_Layers_SP_', FRchPackage);
          end;
          if FRchPackage.TimeVaryingLayers and
            (not FReuseLayerIndicator[StressPeriodIndex]) then
          begin
            RechargeLayerName := StrImportedRechargeLa +
              GetStressPeriodString(StressPeriodIndex);
          end;
          StressPeriod := FStressPeriods[StressPeriodIndex];
          if StressPeriod.Reuse then
          begin
            ReuseRchStressPeriodWithParameters(StressPeriodIndex,
              Param, ScreenObjectList, RechargeLayerName);
          end
          else
          begin
            // In each stress period, identify whether the parameter was used.
            // If it was used, identify the intance used.
            GetParamInstanceForCurrentStressPeriod(Instance, Param,
              StressPeriod);
            if Instance <> nil then
            begin
              // Identify the cluster associated with the instance.
              for ClusterIndex := 0 to Instance.ArrayLength - 1 do
              begin
                Cluster := Instance.Clusters[ClusterIndex];
                ScreenObIndex := ClusterList.IndexOf(Cluster);
                Assert(ScreenObIndex >= 0);
                // Identify the cluster in ClusterList with the same geometry
                // as the cluster used.
                // Retrieve the corresponding TScreenObject from
                // ScreenObjectList.
                // Assign a formula for this parameter in this
                // stress period.
                // The formula should be either the
                // name of the multiplier array,
                // if one is used, or 1 if a multiplier array is not used.
                ScreenObject := ScreenObjectList[ScreenObIndex];
                if FRchPackage.TimeVaryingLayers then
                begin
                  ScreenObject.ElevationCount := ecZero;
                end;
                AssignParamRechargeRate(ScreenObject, StressPeriodIndex,
                  Cluster, Param);
                AssignTimeVaryingLayer(ScreenObject,
                  RechargeLayerName, StressPeriodIndex);
              end;
            end;
          end;
        end;
      end;
    finally
      ScreenObjectList.Free;
      ClusterList.Free;
    end;
  end
  else
  begin
    for StressPeriodIndex := 0 to FModel.ModflowStressPeriods.Count - 1 do
    begin
      CreateRechargeRateDataSet(StressPeriodIndex);
      CreateTimeVaryingAssignedLayerDataSet(StressPeriodIndex,
        StrImportedRechargeLa, 'Imported_RCH_Layers_SP_', FRchPackage);
    end;

    ScreenObject := CreateScreenObjectAroundGrid('ImportedRecharge');
    if (FRchPackage.LayerOption = loSpecified)
      and not FRchPackage.TimeVaryingLayers then
    begin
      ScreenObject.ElevationFormula := StrImportedRechargeEl;
    end
    else
    begin
      ScreenObject.ElevationFormula := StrModelTop;
//      ScreenObject.ElevationCount := ecZero;
    end;

    ScreenObject.CreateRchBoundary;
    RchBoundary := ScreenObject.ModflowRchBoundary;
    RchItem := nil;
    LayerItem := nil;
    for StressPeriodIndex := 0 to FModel.ModflowStressPeriods.Count - 1 do
    begin
      NewItemNeeded := not FReuseRecharge[StressPeriodIndex];
      if FRchPackage.TimeVaryingLayers then
      begin
        if not FReuseLayerIndicator[StressPeriodIndex] then
        begin
          NewItemNeeded := True;
        end;
      end;
      if not FReuseRecharge[StressPeriodIndex] then
      begin
        RechargeName := StrImportedRechargeSt
          + GetStressPeriodString(StressPeriodIndex);
      end;
      if FRchPackage.TimeVaryingLayers and
        (not FReuseLayerIndicator[StressPeriodIndex]) then
      begin
        RechargeLayerName := StrImportedRechargeLa +
          GetStressPeriodString(StressPeriodIndex);
      end;
      AssignRechRateNonParam(RechargeName, NewItemNeeded, RchItem,
        RchBoundary, StressPeriodIndex);
      AssignRchLayerNonParam(NewItemNeeded, LayerItem,
        RchBoundary, RechargeLayerName, StressPeriodIndex);
    end;
  end;
end;

procedure TRchImporter.ReadData(const ALabel: string);
var
  IRCHCB: integer;
  INRECH: integer;
  INIRCH: integer;
  ALine: string;
  Value: double;
  ID: string;
  IntValue: integer;
  Handled: Boolean;
begin
  inherited;
  FRequiredType := 'RCH';
  ImportSharedData(ALabel, Handled);
  if not Handled then
  begin
    if ALabel = 'NRCHOP,IRCHCB:' then
    begin
      Read(FImporter.FFile, NRCHOP);
      Read(FImporter.FFile, IRCHCB);
      ReadLn(FImporter.FFile);
    end
    else if ALabel = 'INRECH,INIRCH:' then
    begin
      Read(FImporter.FFile, INRECH);
      Read(FImporter.FFile, INIRCH);
      ReadLn(FImporter.FFile);
      Inc(FCurrentStressPeriod);
      InitializeStressPeriods;
      InitializeCurrentStressPeriod(INRECH);
      InitializeReuseRecharge;
      FReuseRecharge[FCurrentStressPeriod] := INRECH < 0;
      InitializeReuseLayerIndicator;
      FReuseLayerIndicator[FCurrentStressPeriod] := INIRCH < 0;
      CurrentParameter := 0;
    end
    else if ALabel = 'INRECH:' then
    begin
      Read(FImporter.FFile, INRECH);
      ReadLn(FImporter.FFile);
      Inc(FCurrentStressPeriod);
      InitializeStressPeriods;
      InitializeCurrentStressPeriod(INRECH);
      InitializeReuseRecharge;
      FReuseRecharge[FCurrentStressPeriod] := INRECH < 0;
      CurrentParameter := 0;
    end
    else if ALabel = StrConstant2DRealArray then
    begin
      ReadLn(FImporter.FFile, ALine);
      ALine := Trim(ALine);
      Readln(FImporter.FFile, Value);
      if ALine = 'RECHARGE' then
      begin
        ReadRealConstantArrayItem(Value, FConstantRecharge);
      end
      else
      begin
        Assert(False);
      end;
    end
    else if ALabel = StrVariable2DRealArray then
    begin
      ReadLn(FImporter.FFile, ALine);
      ALine := Trim(ALine);
      if ALine = 'RECHARGE' then
      begin
        ReadRealVariableArray(FVariableRecharge);
      end
      else
      begin
        Assert(False);
      end;
    end
    else if ALabel = StrConstant2DIntegerArray then
    begin
      ReadLn(FImporter.FFile, ALine);
      ALine := Trim(ALine);
      Readln(FImporter.FFile, IntValue);
      if ALine = 'RECHARGE LAYER INDEX' then
      begin
        ReadConstantIntArray(IntValue, FConstantLayerIndicators);
      end
      else
      begin
        Assert(False);
      end;
    end
    else if ALabel = StrVariable2DIntegerArray then
    begin
      ReadLn(FImporter.FFile, ID);
      ID := Trim(ID);
      if ID = 'RECHARGE LAYER INDEX' then
      begin
        ReadVariableIntArray(FVariableLayerIndicators);
      end
      else
      begin
        Assert(False);
      end;
    end
    else
    begin
      Assert(False);
    end;
  end;
end;

{ TArrayParameterObject }

function TArrayParameterObject.ArrayMemberClass: TArrayMemberClass;
begin
  result := TArrayInstanceObject;
end;

function TArrayParameterObject.GetInstance(
  Index: integer): TArrayInstanceObject;
begin
  result := TArrayInstanceObject(Objects[Index]);
end;

function TArrayParameterObject.GetInstanceByName(
  const InstanceName: string): TArrayInstanceObject;
var
  Index: Integer;
  Instance: TArrayInstanceObject;
begin
  result := nil;
  for Index := 0 to ArrayLength - 1 do
  begin
    Instance := Instances[Index];
    if CompareText(Instance.Name, InstanceName) = 0 then
    begin
      result := Instance;
      Exit;
    end;
  end;
end;

{ TArrayParamArray }

function TArrayParamArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TArrayParameterObject;
end;

function TArrayParamArray.GetParams(Index: integer): TArrayParameterObject;
begin
  result := TArrayParameterObject(Objects[Index]);
end;

{ TArrayInstanceObject }

function TArrayInstanceObject.ArrayMemberClass: TArrayMemberClass;
begin
  result := TClusterObject;
end;

function TArrayInstanceObject.GetCluster(Index: integer): TClusterObject;
begin
  result := TClusterObject(Objects[Index]);
end;

{ TArrayStressPeriodArray }

function TArrayStressPeriodArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TArrayStressPeriod;
end;

function TArrayStressPeriodArray.GetStressPeriod(
  Index: integer): TArrayStressPeriod;
begin
  result := TArrayStressPeriod(Objects[Index]);
end;

{ TPointList }

function TPointList.Add(Point: TPoint2D): integer;
begin
  if FCount = Capacity then
  begin
    Grow;
  end;
  FPoints[FCount] := Point;
  result := FCount;
  Inc(FCount);
end;

function TPointList.GetCapacity: integer;
begin
  result := Length(FPoints);
end;

function TPointList.GetPoint(Index: integer): TPoint2D;
begin
  result := FPoints[Index];
end;

procedure TPointList.Grow;
var
  Delta: Integer;
  LocalCapacity: integer;
begin
  LocalCapacity := Capacity;
  if LocalCapacity < 16 then
  begin
    Delta := 4;
  end
  else
  begin
    Delta := LocalCapacity  div 4;
  end;
  Capacity := LocalCapacity + Delta;
end;

procedure TPointList.Insert(Position: integer; Point: TPoint2D);
var
  Index: Integer;
begin
  if Count = Capacity then
  begin
    Grow;
  end;
  if Position = Count then
  begin
    Add(Point);
  end
  else
  begin
    for Index := FCount - 1 downto Position do
    begin
      FPoints[Index+1] := FPoints[Index];
    end;
    FPoints[Position] := Point;
    Inc(FCount);
  end;
end;

procedure TPointList.SetCapacity(const Value: integer);
begin
  SetLength(FPoints, Value);
end;

procedure TPointList.SetPoint(Index: integer; const Value: TPoint2D);
begin
  FPoints[Index] := Value;
end;

{ TClusterObject }

function TClusterObject.SimilarZone(Cluster: TClusterObject): boolean;
var
  Index: Integer;
begin
  result := AnsiCompareText(Cluster.ZoneArray, ZoneArray) = 0;
  if result then
  begin
    result := Length(Zones) = Length(Cluster.Zones);
    if result then
    begin
      for Index := 0 to Length(Zones) - 1 do
      begin
        result := Zones[Index] = Cluster.Zones[Index];
        if not result then
        begin
          Exit;
        end;
      end;
    end;
  end;
end;

{ TEvtImporter }

procedure TCustomETImporter.ReuseEtStressPeriodWithParameters(
  StressPeriodIndex: Integer; Param: TArrayParameterObject;
  ScreenObjectList: TList;
  const EvtLayerName, EvtSurfaceName, EvtDepthName: string);
var
  EvtItem, NewEvtItem: TEvtItem;
  ParamItem: TEvtParamItem;
  ScreenObject: TScreenObject;
  SO_Index: Integer;
  Boundary: TModflowParamBoundary;
begin
  for SO_Index := 0 to ScreenObjectList.Count - 1 do
  begin
    ScreenObject := ScreenObjectList[SO_Index];
    Boundary := GetBoundary(ScreenObject);
    ParamItem := Boundary.Parameters.
      GetParamByName(Param.ModifiedParamName) as TEvtParamItem;
    if ParamItem <> nil then
    begin
      if ParamItem.Param.Count > 0 then
      begin
        EvtItem := ParamItem.Param.Items[ParamItem.Param.Count - 1]
          as TEvtItem;
        if EvtItem.EndTime =
          FModel.ModflowStressPeriods[StressPeriodIndex].StartTime then
        begin
          if Package.TimeVaryingLayers then
          begin
            NewEvtItem := ParamItem.Param.Add as TEvtItem;
            NewEvtItem.Assign(EvtItem);
            NewEvtItem.StartTime :=
              FModel.ModflowStressPeriods[StressPeriodIndex].StartTime;
            NewEvtItem.EndTime :=
              FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
          end
          else
          begin
            EvtItem.EndTime :=
              FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
          end;
          AssignTimeVaryingLayer(ScreenObject,
            EvtLayerName, StressPeriodIndex);
          AssignSurfaceAndDepth(ScreenObject, EvtSurfaceName, EvtDepthName,
            StressPeriodIndex);
        end;
      end;
    end;
  end;
end;

procedure TEvtImporter.SetEvtOption;
var
  Option: Integer;
begin
  Option := NEVTOP - 1;
  if Option < 0 then
  begin
    Option := 0;
  end;
  if Option > 2 then
  begin
    Option := 2;
  end;
  FEvtPackage.LayerOption := TLayerOption(Option);
end;

procedure TEvtImporter.AssignEvtLayerNonParam(NewItemNeeded: Boolean;
  var LayerItem: TEvtLayerItem; EvtBoundary: TEvtBoundary; EvtLayerName: string;
  StressPeriodIndex: Integer);
begin
  if FEvtPackage.TimeVaryingLayers then
  begin
    if NewItemNeeded then
    begin
      LayerItem := EvtBoundary.EvapotranspirationLayers.Add as TEvtLayerItem;
      LayerItem.EvapotranspirationLayer := EvtLayerName;
      LayerItem.StartTime :=
        FModel.ModflowStressPeriods[StressPeriodIndex].StartTime;
      LayerItem.EndTime :=
        FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
    end
    else
    begin
      LayerItem.EndTime :=
        FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
    end;
  end;
end;

procedure TCustomETImporter.AssignEtRateNonParam(EvtName: string;
  NewItemNeeded: Boolean; var EvtItem: TEvtItem;
  EvtBoundary: TModflowParamBoundary; StressPeriodIndex: Integer);
begin
  if NewItemNeeded then
  begin
    EvtItem := EvtBoundary.Values.Add as TEvtItem;
    EvtItem.EvapotranspirationRate := EvtName;
    EvtItem.StartTime :=
      FModel.ModflowStressPeriods[StressPeriodIndex].StartTime;
    EvtItem.EndTime := FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
  end
  else
  begin
    EvtItem.EndTime := FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
  end;
end;

procedure TEvtImporter.AssignEvtSurfaceNonParam(
  EvtSurfaceName, EvtExtinctName: string;
  NewItemNeeded: Boolean; var EvtItem: TEvtSurfDepthItem;
  EvtBoundary: TEvtBoundary; StressPeriodIndex: Integer);
begin
  if NewItemNeeded then
  begin
    EvtItem := EvtBoundary.EvtSurfDepthCollection.Add as TEvtSurfDepthItem;
    EvtItem.EvapotranspirationSurface := EvtSurfaceName;
    EvtItem.EvapotranspirationDepth := EvtExtinctName;
    EvtItem.StartTime :=
      FModel.ModflowStressPeriods[StressPeriodIndex].StartTime;
    EvtItem.EndTime := FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
  end
  else
  begin
    EvtItem.EndTime := FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
  end;
end;

procedure TCustomETImporter.AssignParamEvtRate(ScreenObject: TScreenObject;
  StressPeriodIndex: Integer; Cluster: TClusterObject;
  Param: TArrayParameterObject);
var
  EvtItem: TEvtItem;
  ParamItem: TEvtParamItem;
  Boundary: TModflowParamBoundary;
begin
  Boundary := GetBoundary(ScreenObject);
  ParamItem := Boundary.Parameters.
    GetParamByName(Param.ModifiedParamName) as TEvtParamItem;
  if ParamItem = nil then
  begin
    ParamItem := Boundary.
      Parameters.Add as TEvtParamItem;
    ParamItem.Param.ParamName := Param.ModifiedParamName;
  end;
  EvtItem := ParamItem.Param.Add as TEvtItem;
  EvtItem.StartTime := FModel.ModflowStressPeriods[StressPeriodIndex].StartTime;
  EvtItem.EndTime := FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
  if AnsiCompareText(Cluster.MultiplierArray, 'NONE') = 0 then
  begin
    EvtItem.EvapotranspirationRate := '1';
  end
  else
  begin
    EvtItem.EvapotranspirationRate := FixArrayName(Cluster.MultiplierArray);
  end;
end;

procedure TEvtImporter.AssignSurfaceAndDepth(ScreenObject: TScreenObject;
  EvtSurfaceName, EvtDepthName: string; StressPeriodIndex: Integer);
var
  EvtBoundary: TEvtBoundary;
  LayerItem: TEvtSurfDepthItem;
begin
  EvtBoundary := ScreenObject.ModflowEvtBoundary;
  if FReuseEtSurface[StressPeriodIndex]
    and FReuseEtExtinctionDepth[StressPeriodIndex] then
  begin
    LayerItem := EvtBoundary.EvtSurfDepthCollection.
      Items[EvtBoundary.EvtSurfDepthCollection.Count - 1] as TEvtSurfDepthItem;
    LayerItem.EndTime :=
      FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
  end
  else
  begin
    LayerItem := EvtBoundary.EvtSurfDepthCollection.Add as TEvtSurfDepthItem;
    LayerItem.EvapotranspirationSurface := EvtSurfaceName;
    LayerItem.EvapotranspirationDepth := EvtDepthName;
    LayerItem.StartTime :=
      FModel.ModflowStressPeriods[StressPeriodIndex].StartTime;
    LayerItem.EndTime :=
      FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
  end;
end;

procedure TEvtImporter.AssignTimeVaryingLayer(ScreenObject: TScreenObject;
  EvtLayerName: string; StressPeriodIndex: Integer);
var
  LayerItem: TEvtLayerItem;
  EvtBoundary: TEvtBoundary;
begin
  if Package.TimeVaryingLayers then
  begin
    EvtBoundary := ScreenObject.ModflowEvtBoundary;
    if not FReuseLayerIndicator[StressPeriodIndex] then
    begin
      LayerItem := EvtBoundary.EvapotranspirationLayers.Add as TEvtLayerItem;
      LayerItem.EvapotranspirationLayer := EvtLayerName;
      LayerItem.StartTime :=
        FModel.ModflowStressPeriods[StressPeriodIndex].StartTime;
      LayerItem.EndTime :=
        FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
    end
    else
    begin
      LayerItem := EvtBoundary.EvapotranspirationLayers.
        Items[EvtBoundary.EvapotranspirationLayers.Count - 1] as TEvtLayerItem;
      LayerItem.EndTime :=
        FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
    end;
  end;
end;

constructor TEvtImporter.Create(Importer: TModflow2005Importer;
  ZoneImporter: TMultZoneImporter);
begin
  inherited Create(Importer, 'EVT:', ZoneImporter);
end;

procedure TEvtImporter.CreateBoundary(ScreenObject: TScreenObject);
begin
  ScreenObject.CreateEvtBoundary;
end;

class function TEvtImporter.ImportedEtSurfaceName: string;
begin
  result := StrImportedEvtSurfSt;
end;

class function TEvtImporter.ImportedEtSurfaceSP: string;
begin
  result := 'Imported_EVT_Surface_SP_';
end;

procedure TCustomETImporter.CreateEtSurfaceDataSet(StressPeriodIndex: Integer);
var
  DataSet: TDataArray;
  ScreenObject: TScreenObject;
  ValueList: TRealList;
  Values: T2DDoubleArray;
  MaxCount: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Value: Double;
  Root: string;
begin
  ScreenObject := nil;
  if not FReuseEtSurface[StressPeriodIndex] then
  begin
    CreateDataSet(StressPeriodIndex, ImportedEtSurfaceName,
      rdtDouble, DataSet);
    if (FConstantEtSurface <> nil)
      and FConstantEtSurface[StressPeriodIndex].IsConstant then
    begin
      DataSet.Formula :=
        FloatToStr(FConstantEtSurface[StressPeriodIndex].RealValue);
    end
    else
    begin
      DataSet.Formula := '0';
      Assert(FVariableEtSurface <> nil);
      Assert(FVariableEtSurface[StressPeriodIndex] <> nil);

      ValueList := TRealList.Create;
      try
        ValueList.Sorted := True;
        Values := FVariableEtSurface[StressPeriodIndex];
        MaxCount := Min((Length(Values) * Length(Values[0])) div 10, 100);
        for RowIndex := 0 to Length(Values) - 1 do
        begin
          for ColIndex := 0 to Length(Values[0]) - 1 do
          begin
            Value := Values[RowIndex,ColIndex];
            ValueList.AddUnique(Value);
            if ValueList.Count >= MaxCount then
            begin
              break;
            end;
          end;
          if ValueList.Count >= MaxCount then
          begin
            break;
          end;
        end;
        if ValueList.Count >= MaxCount then
        begin
          if ScreenObject = nil then
          begin
            CreateOrRetrieveCellCenterScreenObject(ScreenObject);
          end;
          AssignRealValuesToCellCenters(DataSet, ScreenObject,
            FVariableEtSurface[StressPeriodIndex]);
        end
        else if ValueList.Count = 1 then
        begin
          DataSet.Formula :=
            FloatToStr(ValueList[0]);
        end
        else
        begin
          Root := ImportedEtSurfaceSP
            + GetStressPeriodString(StressPeriodIndex) + '_Obj_';
          CreateScreenObjectsAroundValues(Values, Root,
            DataSet, ValueList);
        end;
      finally
        ValueList.Free;
      end;
    end;
  end;
end;

class function TEvtImporter.EtExtinctionDepth_Name: string;
begin
  result := StrImportedEvtExtinctDepthSt;
end;

class function TEvtImporter.EtExtinctionDepth_SP: string;
begin
  result := 'Imported_EVT_ExtinctionDepth_SP_';
end;

function TEvtImporter.GetBoundary(
  ScreenObject: TScreenObject): TModflowParamBoundary;
begin
  result := ScreenObject.ModflowEvtBoundary;
end;

procedure TCustomETImporter.CreateEtExtinctionDepthDataSet(
  StressPeriodIndex: Integer);
var
  DataSet: TDataArray;
  ScreenObject: TScreenObject;
  ValueList: TRealList;
  Values: T2DDoubleArray;
  MaxCount: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Value: Double;
  Root: string;
begin
  ScreenObject := nil;
  if not FReuseEtExtinctionDepth[StressPeriodIndex] then
  begin
    CreateDataSet(StressPeriodIndex, EtExtinctionDepth_Name,
      rdtDouble, DataSet);
    if (FConstantExtinctionDepth <> nil)
      and FConstantExtinctionDepth[StressPeriodIndex].IsConstant then
    begin
      DataSet.Formula :=
        FloatToStr(FConstantExtinctionDepth[StressPeriodIndex].RealValue);
    end
    else
    begin
      DataSet.Formula := '0';
      Assert(FVariableExtinctionDepth <> nil);
      Assert(FVariableExtinctionDepth[StressPeriodIndex] <> nil);

      ValueList := TRealList.Create;
      try
        ValueList.Sorted := True;
        Values := FVariableExtinctionDepth[StressPeriodIndex];
        MaxCount := Min((Length(Values) * Length(Values[0])) div 10, 100);
        for RowIndex := 0 to Length(Values) - 1 do
        begin
          for ColIndex := 0 to Length(Values[0]) - 1 do
          begin
            Value := Values[RowIndex,ColIndex];
            ValueList.AddUnique(Value);
            if ValueList.Count >= MaxCount then
            begin
              break;
            end;
          end;
          if ValueList.Count >= MaxCount then
          begin
            break;
          end;
        end;
        if ValueList.Count >= MaxCount then
        begin
          if ScreenObject = nil then
          begin
            CreateOrRetrieveCellCenterScreenObject(ScreenObject);
          end;
          AssignRealValuesToCellCenters(DataSet, ScreenObject,
            FVariableExtinctionDepth[StressPeriodIndex]);
        end
        else if ValueList.Count = 1 then
        begin
          DataSet.Formula :=
            FloatToStr(ValueList[0]);
        end
        else
        begin
          Root := EtExtinctionDepth_SP
            + GetStressPeriodString(StressPeriodIndex) + '_Obj_';
          CreateScreenObjectsAroundValues(Values, Root,
            DataSet, ValueList);
        end;
      finally
        ValueList.Free;
      end;
    end;
  end;
end;

class function TEvtImporter.ImportedEtRateName: string;
begin
  result := StrImportedEvtSt;
end;

class function TEvtImporter.ImportedEtValuesName: string;
begin
  result := 'Imported_EVT_Values_SP_';
end;

procedure TCustomETImporter.CreateEvtRateDataSet(StressPeriodIndex: Integer);
var
  EtRateDataSet: TDataArray;
  ScreenObject: TScreenObject;
  ValueList: TRealList;
  Values: T2DDoubleArray;
  MaxCount: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Value: Double;
  Root: string;
begin
  ScreenObject := nil;
  if not FReuseEtRate[StressPeriodIndex] then
  begin
    CreateDataSet(StressPeriodIndex, ImportedEtRateName,
      rdtDouble, EtRateDataSet);
    if (FConstantEtRate <> nil)
      and FConstantEtRate[StressPeriodIndex].IsConstant then
    begin
      EtRateDataSet.Formula :=
        FloatToStr(FConstantEtRate[StressPeriodIndex].RealValue);
    end
    else
    begin
      EtRateDataSet.Formula := '0';
      Assert(FVariableEtRate <> nil);
      Assert(FVariableEtRate[StressPeriodIndex] <> nil);

      ValueList := TRealList.Create;
      try
        ValueList.Sorted := True;
        Values := FVariableEtRate[StressPeriodIndex];
        MaxCount := Min((Length(Values) * Length(Values[0])) div 10, 100);
        for RowIndex := 0 to Length(Values) - 1 do
        begin
          for ColIndex := 0 to Length(Values[0]) - 1 do
          begin
            Value := Values[RowIndex,ColIndex];
            ValueList.AddUnique(Value);
            if ValueList.Count >= MaxCount then
            begin
              break;
            end;
          end;
          if ValueList.Count >= MaxCount then
          begin
            break;
          end;
        end;
        if ValueList.Count >= MaxCount then
        begin
          if ScreenObject = nil then
          begin
            CreateOrRetrieveCellCenterScreenObject(ScreenObject);
          end;
          AssignRealValuesToCellCenters(EtRateDataSet, ScreenObject,
            FVariableEtRate[StressPeriodIndex]);
        end
        else if ValueList.Count = 1 then
        begin
          EtRateDataSet.Formula :=
            FloatToStr(ValueList[0]);
        end
        else
        begin
          Root := ImportedEtValuesName
            + GetStressPeriodString(StressPeriodIndex) + '_Obj_';
          CreateScreenObjectsAroundValues(Values, Root,
            EtRateDataSet, ValueList);
        end;
      finally
        ValueList.Free;
      end;
    end;
  end;
end;

procedure TEvtImporter.HandlePackage;
var
  StressPeriodIndex: Integer;
  ScreenObject: TScreenObject;
  EtLayerDataSet: TDataArray;
  NewItemNeeded: Boolean;
  EvtBoundary: TEvtBoundary;
  EvtItem: TEvtItem;
  LayerItem: TEvtLayerItem;
  EvtName: string;
  EvtLayerName: string;
  EvtSurfaceName: string;
  ParamIndex: Integer;
  Param: TArrayParameterObject;
  Instance: TArrayInstanceObject;
  ClusterIndex: Integer;
  Cluster: TClusterObject;
  ObjectIndex: integer;
  ClusterList: TList;
  ScreenObjectList: TList;
  StressPeriod: TArrayStressPeriod;
  ScreenObIndex: Integer;
  EvtExtinctName: string;
  EvtSurf: TEvtSurfDepthItem;
begin
  if (FCurrentStressPeriod < 0) or
    (FCurrentStressPeriod < FModel.ModflowStressPeriods.Count -1) then
  begin
    Exit;
  end;
  inherited;
  FEvtPackage := FModel.ModflowPackages.EvtPackage;
  Package := FEvtPackage;
  FEvtPackage.IsSelected := True;
  FEvtPackage.Comments := FComments;
  SetEvtOption;
  EvaluateTimeVaryingLayers(FEvtPackage);

  CreateAssignedLayerDataSet(FEvtPackage, StrImportedEvtEl,
    'Imported_Evt_Elevation', EtLayerDataSet);

  if NP > 0 then
  begin
    ClusterList := TList.Create;
    ScreenObjectList := TList.Create;
    try
      ObjectIndex := 0;
      for ParamIndex := 0 to FParams.ArrayLength - 1 do
      begin
        Param := FParams[ParamIndex];
        CreateTransientParam(Param);

        ScreenObjectList.Clear;
        ClusterList.Clear;
        CreateScreenObjectsFromClusters(Param, ObjectIndex,
          ScreenObjectList, ClusterList, 'ImportedEvtParam_', StrImportedEvtEl,
          FEvtPackage);
        // At this point, ScreenObjectList contains each TScreenObject
        // that has been created for a particular parameter
        // and Cluster list contains a TClusterObject that has the
        // zone array and zones used to define the geometry of the
        // TScreenObject but not necessarily the corresponding Multiplier
        // array.

        // What need's to be done is to identify the stress periods in which
        // each parameter instance was used and find the corresponding
        // TScreenObjects and assign the formulas for those
        // stress periods using the appropriate multiplier arrays.

        // Loop over stress periods
        for StressPeriodIndex := 0 to FStressPeriods.ArrayLength - 1 do
        begin
          if ParamIndex = 0 then
          begin
            CreateTimeVaryingAssignedLayerDataSet(StressPeriodIndex,
              StrImportedEvtLa, 'Imported_EVT_Layers_SP_', FEvtPackage);
            CreateEtSurfaceDataSet(StressPeriodIndex);
            CreateEtExtinctionDepthDataSet(StressPeriodIndex);
          end;
          if FEvtPackage.TimeVaryingLayers and
            (not FReuseLayerIndicator[StressPeriodIndex]) then
          begin
            EvtLayerName := StrImportedEvtLa +
              GetStressPeriodString(StressPeriodIndex);
          end;
          if not FReuseEtSurface[StressPeriodIndex] then
          begin
            EvtSurfaceName := ImportedEtSurfaceName
              + GetStressPeriodString(StressPeriodIndex);
          end;
          if not FReuseEtExtinctionDepth[StressPeriodIndex] then
          begin
            EvtExtinctName := EtExtinctionDepth_Name
              + GetStressPeriodString(StressPeriodIndex);
          end;
          StressPeriod := FStressPeriods[StressPeriodIndex];
          if StressPeriod.Reuse then
          begin
            ReuseEtStressPeriodWithParameters(StressPeriodIndex,
              Param, ScreenObjectList, EvtLayerName, EvtSurfaceName,
              EvtExtinctName);
          end
          else
          begin
            // In each stress period, identify whether the parameter was used.
            // If it was used, identify the intance used.
            GetParamInstanceForCurrentStressPeriod(Instance, Param,
              StressPeriod);
            if Instance <> nil then
            begin
              // Identify the cluster associated with the instance.
              for ClusterIndex := 0 to Instance.ArrayLength - 1 do
              begin
                Cluster := Instance.Clusters[ClusterIndex];
                ScreenObIndex := ClusterList.IndexOf(Cluster);
                Assert(ScreenObIndex >= 0);
                // Identify the cluster in ClusterList with the same geometry
                // as the cluster used.
                // Retrieve the corresponding TScreenObject from
                // ScreenObjectList.
                // Assign a formula for this parameter in this
                // stress period.
                // The formula should be either the
                // name of the multiplier array,
                // if one is used, or 1 if a multiplier array is not used.
                ScreenObject := ScreenObjectList[ScreenObIndex];
                AssignParamEvtRate(ScreenObject, StressPeriodIndex,
                  Cluster, Param);
                AssignTimeVaryingLayer(ScreenObject,
                  EvtLayerName, StressPeriodIndex);
                AssignSurfaceAndDepth(ScreenObject, EvtSurfaceName,
                  EvtExtinctName, StressPeriodIndex);
              end;
            end;
          end;
        end;
      end;
    finally
      ScreenObjectList.Free;
      ClusterList.Free;
    end;
  end
  else
  begin
    for StressPeriodIndex := 0 to FModel.ModflowStressPeriods.Count - 1 do
    begin
      CreateEvtRateDataSet(StressPeriodIndex);
      CreateEtSurfaceDataSet(StressPeriodIndex);
      CreateEtExtinctionDepthDataSet(StressPeriodIndex);
      CreateTimeVaryingAssignedLayerDataSet(StressPeriodIndex,
        StrImportedEvtLa, 'Imported_EVT_Layers_SP_', FEvtPackage);
    end;

    ScreenObject := CreateScreenObjectAroundGrid('ImportedEvt');
    if (FEvtPackage.LayerOption = loSpecified)
      and not FEvtPackage.TimeVaryingLayers then
    begin
      ScreenObject.ElevationFormula := StrImportedEvtEl;
    end
    else
    begin
      ScreenObject.ElevationFormula := StrModelTop;
//      ScreenObject.ElevationCount := ecZero;
    end;

    ScreenObject.CreateEVTBoundary;
    EvtBoundary := ScreenObject.ModflowEvtBoundary;
    EvtItem := nil;
    LayerItem := nil;
    for StressPeriodIndex := 0 to FModel.ModflowStressPeriods.Count - 1 do
    begin
      NewItemNeeded := not FReuseEtRate[StressPeriodIndex];
      if FEvtPackage.TimeVaryingLayers then
      begin
        if not FReuseLayerIndicator[StressPeriodIndex] then
        begin
          NewItemNeeded := True;
        end;
      end;
      if not FReuseEtSurface[StressPeriodIndex] then
      begin
        NewItemNeeded := True;
      end;
      if not FReuseEtExtinctionDepth[StressPeriodIndex] then
      begin
        NewItemNeeded := True;
      end;

      if not FReuseEtRate[StressPeriodIndex] then
      begin
        EvtName := ImportedEtRateName
          + GetStressPeriodString(StressPeriodIndex);
      end;
      if FEvtPackage.TimeVaryingLayers and
        (not FReuseLayerIndicator[StressPeriodIndex]) then
      begin
        EvtLayerName := StrImportedEvtLa +
          GetStressPeriodString(StressPeriodIndex);
      end;
      if not FReuseEtSurface[StressPeriodIndex] then
      begin
        EvtSurfaceName := ImportedEtSurfaceName
          + GetStressPeriodString(StressPeriodIndex);
      end;
      if not FReuseEtExtinctionDepth[StressPeriodIndex] then
      begin
        EvtExtinctName := EtExtinctionDepth_Name
          + GetStressPeriodString(StressPeriodIndex);
      end;

      AssignEtRateNonParam(EvtName, NewItemNeeded, EvtItem,
        EvtBoundary, StressPeriodIndex);
      AssignEvtLayerNonParam(NewItemNeeded, LayerItem,
        EvtBoundary, EvtLayerName, StressPeriodIndex);
      AssignEvtSurfaceNonParam(EvtSurfaceName,EvtExtinctName, NewItemNeeded,
        EvtSurf, EvtBoundary, StressPeriodIndex);
    end;
  end;
end;

procedure TEvtImporter.ReadData(const ALabel: string);
var
  Handled: Boolean;
  IEVTCB: integer;
  INSURF: integer;
  INEVTR: integer;
  INEXDP: integer;
  INIEVT: integer;
  ALine: string;
  Value: double;
  IntValue: integer;
  ID: string;
begin
  inherited;
  FRequiredType := 'EVT';
  ImportSharedData(ALabel, Handled);
  if not Handled then
  begin
    if ALabel = 'NEVTOP,IEVTCB:' then
    begin
      Read(FImporter.FFile, NEVTOP);
      Read(FImporter.FFile, IEVTCB);
      ReadLn(FImporter.FFile);
    end
    else if ALabel = 'INSURF,INEVTR,INEXDP,INIEVT:' then
    begin
      Read(FImporter.FFile, INSURF);
      Read(FImporter.FFile, INEVTR);
      Read(FImporter.FFile, INEXDP);
      Read(FImporter.FFile, INIEVT);
      ReadLn(FImporter.FFile);
      Inc(FCurrentStressPeriod);
      InitializeStressPeriods;
      InitializeCurrentStressPeriod(INEVTR);
      InitializeReuseEtSurface;
      FReuseEtSurface[FCurrentStressPeriod] := INSURF < 0;
      InitializeReuseEtRate;
      FReuseEtRate[FCurrentStressPeriod] := INEVTR < 0;
      InitializeReuseExtinctionDepth;
      FReuseEtExtinctionDepth[FCurrentStressPeriod] := INEXDP < 0;
      InitializeReuseLayerIndicator;
      FReuseLayerIndicator[FCurrentStressPeriod] := INIEVT < 0;
      CurrentParameter := 0;
    end
    else if ALabel = 'INSURF,INEVTR,INEXDP:' then
    begin
      Read(FImporter.FFile, INSURF);
      Read(FImporter.FFile, INEVTR);
      Read(FImporter.FFile, INEXDP);
      ReadLn(FImporter.FFile);
      Inc(FCurrentStressPeriod);
      InitializeStressPeriods;
      InitializeCurrentStressPeriod(INEVTR);
      InitializeReuseEtSurface;
      FReuseEtSurface[FCurrentStressPeriod] := INSURF < 0;
      InitializeReuseEtRate;
      FReuseEtRate[FCurrentStressPeriod] := INEVTR < 0;
      InitializeReuseExtinctionDepth;
      FReuseEtExtinctionDepth[FCurrentStressPeriod] := INEXDP < 0;
      CurrentParameter := 0;
    end
    else if ALabel = StrConstant2DRealArray then
    begin
      ReadLn(FImporter.FFile, ALine);
      ALine := Trim(ALine);
      Readln(FImporter.FFile, Value);
      if ALine = 'ET SURFACE' then
      begin
        ReadRealConstantArrayItem(Value, FConstantEtSurface);
      end
      else if ALine = 'EVAPOTRANSPIRATION RATE' then
      begin
        ReadRealConstantArrayItem(Value, FConstantEtRate);
      end
      else if ALine = 'EXTINCTION DEPTH' then
      begin
        ReadRealConstantArrayItem(Value, FConstantExtinctionDepth);
      end
      else
      begin
        Assert(False);
      end;
    end
    else if ALabel = StrVariable2DRealArray then
    begin
      ReadLn(FImporter.FFile, ALine);
      ALine := Trim(ALine);
      if ALine = 'ET SURFACE' then
      begin
        ReadRealVariableArray(FVariableEtSurface);
      end
      else if ALine = 'EVAPOTRANSPIRATION RATE' then
      begin
        ReadRealVariableArray(FVariableEtRate);
      end
      else if ALine = 'EXTINCTION DEPTH' then
      begin
        ReadRealVariableArray(FVariableExtinctionDepth);
      end
      else
      begin
        Assert(False);
      end;
    end
    else if ALabel = StrConstant2DIntegerArray then
    begin
      ReadLn(FImporter.FFile, ALine);
      ALine := Trim(ALine);
      Readln(FImporter.FFile, IntValue);
      if ALine = 'ET LAYER INDEX' then
      begin
        ReadConstantIntArray(IntValue, FConstantLayerIndicators);
      end
      else
      begin
        Assert(False);
      end;
    end
    else if ALabel = StrVariable2DIntegerArray then
    begin
      ReadLn(FImporter.FFile, ID);
      ID := Trim(ID);
      if ID = 'ET LAYER INDEX' then
      begin
        ReadVariableIntArray(FVariableLayerIndicators);
      end
      else
      begin
        Assert(False);
      end;
    end
    else
    begin
      Assert(False);
    end;
  end;
end;

procedure TCustomChdImporter.SetItemValues(Item: TCustomModflowBoundaryItem;
  Boundaries: TList; EndTime: Double; StartTime: Double;
  ScreenObject: TScreenObject; const ParamName: string);
var
  ChdBoundary: TChdLocationObject;
  ChdItem: TChdItem;
  ValueItem: TValueArrayItem;
  Index: Integer;
  StartValues: TValueArrayStorage;
  EndValues: TValueArrayStorage;
  StartName: string;
  EndName: string;
begin
  ValueItem := ScreenObject.ImportedValues.Add as TValueArrayItem;
  StartName := 'ChdStart' + ParamName;
  ValueItem.Name := StartName;
  StartValues := ValueItem.Values;
  StartValues.DataType := rdtDouble;
  StartValues.Count := Boundaries.Count;

  ValueItem := ScreenObject.ImportedValues.Add as TValueArrayItem;
  EndName := 'ChdEnd' + ParamName;
  ValueItem.Name := EndName;
  EndValues := ValueItem.Values;
  EndValues.DataType := rdtDouble;
  EndValues.Count := Boundaries.Count;

  ChdItem := Item as TChdItem;
  ChdItem.StartTime := StartTime;
  ChdItem.EndTime := EndTime;
  for Index := 0 to Boundaries.Count - 1 do
  begin
    ChdBoundary := Boundaries[Index];
    StartValues.RealValues[Index] := ChdBoundary.StartFactor;
    EndValues.RealValues[Index] := ChdBoundary.EndFactor;
  end;
  ChdItem.StartHead := rsObjectImportedValuesR + '("' + StartName + '")';
  ChdItem.EndHead := rsObjectImportedValuesR + '("' + EndName + '")';
  AssignObservationFactors(ScreenObject, ParamName, Boundaries);
end;

{ TSfrImporter }

constructor TSfrImporter.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'SFR');
  FStressPeriods:= TSfrStressPeriodArray.Create;
  FParameters:= TSfrParamArray.Create;
  DLEAK := 0.0001;
  ISFROPT := 0;
  Nstrail := 10;
  ISUZN := 10;
  NSFRSETS := 30;
  FCurrentReachIndex := -1;
  FCurrentSegmentIndex := -1;
  FReaches := TSfReaches.Create;
  FStressPeriodSegments := TStressPeriodSegments.Create;
end;

procedure TSfrImporter.ReadUnsatParameters;
begin
  Read(FImporter.FFile, NSTRAIL);
  Read(FImporter.FFile, ISUZN);
  Read(FImporter.FFile, NSFRSETS);
  Readln(FImporter.FFile);
end;

function TSfrImporter.ScreenObjectNameRoot: string;
begin
  result := 'Imported_Sfr_'
end;

procedure TSfrImporter.SetItemValues(Item: TCustomModflowBoundaryItem;
  Boundaries: TList; EndTime, StartTime: Double; ScreenObject: TScreenObject;
  const ParamName: string);
begin
  Assert(False);
end;

procedure TSfrImporter.ReadIsfropt;
begin
  Read(FImporter.FFile, ISFROPT);
  Readln(FImporter.FFile);
end;

procedure TSfrImporter.ReadNumberOfInstances;
var
  NUMINST: Integer;
  Index: Integer;
  Instance: TSfrInstanceObject;
begin
  Read(FImporter.FFile, NUMINST);
  Readln(FImporter.FFile);
  FParameters[CurrentParameter].ArrayLength := NUMINST;
  for Index := 0 to NUMINST - 1 do
  begin
    Instance := FParameters[CurrentParameter].Instances[Index]
      as TSfrInstanceObject;
    Instance.Segments.ArrayLength := NLST;
  end;
end;

procedure TSfrImporter.ReadParameterType;
var
  PARTYP: string;
begin
  Readln(FImporter.FFile, PARTYP);
  PARTYP := Trim(PARTYP);
  PARTYP := UpperCase(PARTYP);
  Assert(PARTYP = 'SFR');
  FParameters[CurrentParameter].PARTYP := PARTYP;
end;

procedure TSfrImporter.ReadParameterValueAndLocationCount;
var
  Index: Integer;
  Parval: Double;
  Instance: TSfrInstanceObject;
begin
  Read(FImporter.FFile, Parval);
  Read(FImporter.FFile, NLST);
  Readln(FImporter.FFile);
  FParameters[CurrentParameter].Parval := Parval;
  for Index := 0 to FParameters[CurrentParameter].ArrayLength - 1 do
  begin
    Instance := FParameters[CurrentParameter].Instances[Index]
      as TSfrInstanceObject;
    Instance.Segments.ArrayLength := NLST;
  end;
  SegCount := 0;
end;

procedure TSfrImporter.ReadBasicData;
begin
  Read(FImporter.FFile, NSTRM);
  Read(FImporter.FFile, NSS);
  Read(FImporter.FFile, NP);
  Read(FImporter.FFile, NPARSEG);
  Read(FImporter.FFile, SfrCONST);
  Read(FImporter.FFile, DLEAK);
  Read(FImporter.FFile, ISTCB1);
  Read(FImporter.FFile, ISTCB2);
  Readln(FImporter.FFile);
  FParameters.ArrayLength := NP;
  if NSTRM < 0 then
  begin
    NSTRM := -NSTRM;
  end;
  FReaches.ArrayLength := NSTRM;
end;

procedure TSfrImporter.ReadDataSet5WithoutParameters;
var
  IPTFLG: Integer;
  IRDFLG: Integer;
begin
  Read(FImporter.FFile, ITMP);
  Read(FImporter.FFile, IRDFLG);
  Read(FImporter.FFile, IPTFLG);
  NP := 0;
  Inc(FCurrentStressPeriod);
  InitializeCurrentStressPeriod;
end;

procedure TSfrImporter.ReadDataSet5WithParameters;
var
  IRDFLG: Integer;
  IPTFLG: Integer;
begin
  Read(FImporter.FFile, ITMP);
  Read(FImporter.FFile, IRDFLG);
  Read(FImporter.FFile, IPTFLG);
  Read(FImporter.FFile, NP);
  Inc(FCurrentStressPeriod);
  InitializeCurrentStressPeriod;
end;

function TSfrImporter.CreateStream(List: TList;
  var ScreenObjectIndex: integer): TScreenObject;
var
  UndoCreateScreenObject: TCustomUndo;
  Index: Integer;
  Reach: TSfrLocationObject;
  ImportedElevations: TValueArrayStorage;
begin
  result := TScreenObject.CreateWithViewDirection(FModel, vdTop,
    UndoCreateScreenObject, False);
  Inc(ScreenObjectIndex);
  result.Name := ScreenObjectNameRoot + '_'
    + IntToStr(ScreenObjectIndex);
  FModel.AddScreenObject(result);
  result.ElevationCount := ecOne;
  result.SetValuesOfIntersectedCells := True;
  result.EvaluatedAt := eaBlocks;
  result.Visible := False;
  result.Capacity := List.Count;
  ImportedElevations := result.ImportedSectionElevations;
  ImportedElevations.DataType := rdtDouble;
  ImportedElevations.Count := List.Count;
  for Index := 0 to List.Count - 1 do
  begin
    Reach := List[Index];
    result.AddPoint(FImporter.CenterPoints[Reach.Row - 1,
      Reach.Column - 1], True);
    ImportedElevations.RealValues[Index] :=
      FGrid.LayerCenter(Reach.Column - 1, Reach.Row - 1,
      FModel.LayerStructure.ModflowLayerToDataSetLayer(Reach.Layer));
  end;
  result.ElevationFormula := rsObjectImportedValuesR
    + '("' + StrImportedElevations + '")';

  result.CreateSfrBoundary;
  FSfrBoundary := result.ModflowSfrBoundary;
  Reach := List[0];
  FSfrBoundary.SegementNumber := Reach.SegmentNumber;
  AssignStartAndEndTimes;
  AssignReachValues(List, Result);
end;

destructor TSfrImporter.Destroy;
begin
  FStressPeriodSegments.Free;
  FReaches.Free;
  FParameters.Free;
  FStressPeriods.Free;
  inherited;
end;

procedure TSfrImporter.AssignParameterSegmentProperties(PriorSegNumber: Integer);
var
  ICalcItem: TSfrParamIcalcItem;
  Segment: TSegment;
  Instance: TSfrInstanceObject;
  InstanceName: string;
  ParamPosition: Integer;
  StressPeriod: TSegmentArray;
  StressPeriodIndex: Integer;
  IcalcIndex: Integer;
  Param: TSfrParameterObject;
  PIndex: Integer;
begin
  for PIndex := 0 to FParameters.ArrayLength - 1 do
  begin
    Param := FParameters[PIndex] as TSfrParameterObject;
    IcalcIndex := -1;
    for StressPeriodIndex := 0 to FStressPeriods.ArrayLength - 1 do
    begin
      Inc(IcalcIndex);
      StressPeriod := FStressPeriods[StressPeriodIndex] as TSegmentArray;
      ParamPosition := StressPeriod.IndexOfParameter(Param.PARNAM);
      if ParamPosition >= 0 then
      begin
        InstanceName := StressPeriod.Instances[ParamPosition];
        if InstanceName = '' then
        begin
          Instance := Param.Instances[0] as TSfrInstanceObject;
//          IcalcIndex := 0;
        end
        else
        begin
          Instance := Param.GetInstanceByName(InstanceName)
            as TSfrInstanceObject;
        end;
        Segment := Instance.GetSegByNumber(PriorSegNumber);
        if (Segment <> nil) then
        begin
          AssignParamIcalcValues(IcalcIndex, Segment);
          ICalcItem := FSfrBoundary.ParamIcalc.Items[IcalcIndex];
          ICalcItem.Param := Param.PARNAM;
          ICalcItem.ParamInstance := DefaultInstanceName(StressPeriodIndex);
          AssignFlowTableValues(IcalcIndex, Segment);
          AssignSegmentFlowValues(IcalcIndex, Segment);
          AssignChannelValues(IcalcIndex, StressPeriodIndex, Segment);
          AssignEquationValues(IcalcIndex, Segment);
          AssignUpstreamValues(IcalcIndex, StressPeriodIndex, Segment);
          AssignDownstreamValues(Segment, StressPeriodIndex, IcalcIndex);
          AssignUnsatValues(Segment, StressPeriodIndex, IcalcIndex);
        end;
      end;
    end;
  end;
end;

procedure TSfrImporter.CreateParamInstances(ParameterSegments: TIntegerList);
var
  Segment: TSegment;
  Instance: TSfrInstanceObject;
  InstanceItem: TSfrParamInstance;
  ParamPosition: Integer;
  StressPeriod: TSegmentArray;
  StressPeriodIndex: Integer;
  ParamIndex: Integer;
  InstanceIndex: Integer;
  SegIndex: Integer;
  ParamObject: TSfrParameterObject;
  Parameter: TModflowTransientListParameter;
begin
  for ParamIndex := 0 to FParameters.ArrayLength - 1 do
  begin
    ParamObject := FParameters[ParamIndex] as TSfrParameterObject;
    Parameter := FModel.ModflowTransientParameters.Add
      as TModflowTransientListParameter;
    Parameter.ParameterName := ParamObject.PARNAM;
    Parameter.ParameterType := ptSFR;
    Parameter.Value := ParamObject.Parval;
    for StressPeriodIndex := 0 to FStressPeriods.ArrayLength - 1 do
    begin
      StressPeriod := FStressPeriods[StressPeriodIndex] as TSegmentArray;
      ParamPosition := StressPeriod.IndexOfParameter(Parameter.ParameterName);
      if ParamPosition >= 0 then
      begin
        InstanceItem := FSfrPackage.ParameterInstances.Add as TSfrParamInstance;
        InstanceItem.ParameterName := ParamObject.PARNAM;
        InstanceItem.ParameterInstance :=
          DefaultInstanceName(StressPeriodIndex);
        InstanceItem.StartTime :=
          FModel.ModflowStressPeriods[StressPeriodIndex].StartTime;
        InstanceItem.EndTime :=
          FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
      end;
    end;
    for InstanceIndex := 0 to ParamObject.ArrayLength - 1 do
    begin
      Instance := ParamObject.Instances[InstanceIndex] as TSfrInstanceObject;
      for SegIndex := 0 to Instance.Segments.ArrayLength - 1 do
      begin
        Segment := Instance.Segments[SegIndex] as TSegment;
        if Segment.NSEG > 0 then
        begin
          ParameterSegments.AddUnique(Segment.NSEG);
        end;
      end;
    end;
  end;
end;

procedure TSfrImporter.AssignSegmentProperties;
var
  Segment: TSegment;
  SegmentArray: TSegmentArray;
  StressPeriod: TListStressPeriod;
  StressPeriodIndex: Integer;
  IcalcIndex: Integer;
begin
  IcalcIndex := -1;
  for StressPeriodIndex := 0 to FStressPeriods.ArrayLength - 1 do
  begin
    // initialize the start and end times for when the boundary will be
    // applied.
    StressPeriod := FStressPeriods[StressPeriodIndex];
    if not StressPeriod.Reuse then
    begin
      Inc(IcalcIndex);
      SegmentArray := FStressPeriodSegments[StressPeriodIndex];
      Segment := SegmentArray[FSfrBoundary.SegementNumber - 1];
      AssignParamIcalcValues(IcalcIndex, Segment);
      AssignFlowTableValues(IcalcIndex, Segment);
      AssignSegmentFlowValues(IcalcIndex, Segment);
      AssignChannelValues(IcalcIndex, StressPeriodIndex, Segment);
      AssignEquationValues(IcalcIndex, Segment);
      AssignUpstreamValues(IcalcIndex, StressPeriodIndex, Segment);
      AssignDownstreamValues(Segment, StressPeriodIndex, IcalcIndex);
      AssignUnsatValues(Segment, StressPeriodIndex, IcalcIndex);
    end;
  end;
end;

procedure TSfrImporter.AssignUnsatValues(Segment: TSegment;
  StressPeriodIndex: Integer; IcalcIndex: Integer);
var
  DownstreamUnsatItem: TSfrUnsatSegmentItem;
  UpstreamUnsatItem: TSfrUnsatSegmentItem;
begin
  if StressPeriodIndex = 0 then
  begin
    if (Segment.ICalc in [1, 2]) and (FSfrPackage.Isfropt in [4, 5]) then
    begin
      UpstreamUnsatItem := FSfrBoundary.
        UpstreamUnsatSegmentValues[IcalcIndex] as TSfrUnsatSegmentItem;
      UpstreamUnsatItem.BrooksCoreyExponent := FloatToStr(Segment.EPS1);
      UpstreamUnsatItem.InitialWaterContent := FloatToStr(Segment.THTI1);
      UpstreamUnsatItem.SaturatedWaterContent := FloatToStr(Segment.THTS1);
      UpstreamUnsatItem.VerticalSaturatedK := FloatToStr(Segment.UHC1);

      DownstreamUnsatItem := FSfrBoundary.
        DownstreamUnsatSegmentValues[IcalcIndex] as TSfrUnsatSegmentItem;
      DownstreamUnsatItem.BrooksCoreyExponent := FloatToStr(Segment.EPS2);
      DownstreamUnsatItem.InitialWaterContent := FloatToStr(Segment.THTI2);
      DownstreamUnsatItem.SaturatedWaterContent := FloatToStr(Segment.THTS2);
      DownstreamUnsatItem.VerticalSaturatedK := FloatToStr(Segment.UHC2);
    end;
  end;
end;

procedure TSfrImporter.AssignDownstreamValues(Segment: TSegment;
  StressPeriodIndex: Integer; IcalcIndex: Integer);
var
  DownstreamItem: TSfrSegmentItem;
begin
  DownstreamItem := FSfrBoundary.
    DownstreamSegmentValues[IcalcIndex] as TSfrSegmentItem;
  if FSfrPackage.Isfropt in [0, 4, 5] then
  begin
    DownstreamItem.HydraulicConductivity := FloatToStr(Segment.HCOND2);
    if FSfrPackage.Isfropt = 0 then
    begin
      DownstreamItem.StreamBedThickness := FloatToStr(Segment.THICKM2);
      DownstreamItem.StreambedElevation := FloatToStr(Segment.ELEVDN);
      DownstreamItem.StreamWidth := FloatToStr(Segment.WIDTH2);
      DownstreamItem.StreamDepth := FloatToStr(Segment.DEPTH2);
    end;
  end;
  if FSfrPackage.Isfropt in [4, 5] then
  begin
    if (Segment.ICalc in [1, 2]) and (StressPeriodIndex = 0) then
    begin
      DownstreamItem.StreamBedThickness := FloatToStr(Segment.THICKM2);
      DownstreamItem.StreambedElevation := FloatToStr(Segment.ELEVDN);
    end;
  end;
  if FSfrPackage.Isfropt <= 1 then
  begin
    DownstreamItem.StreamWidth := FloatToStr(Segment.WIDTH2);
  end
  else
  begin
    if StressPeriodIndex = 0 then
    begin
      DownstreamItem.StreamWidth := FloatToStr(Segment.WIDTH2);
    end;
  end;
end;

procedure TSfrImporter.AssignUpstreamValues(IcalcIndex: Integer;
  StressPeriodIndex: Integer; Segment: TSegment);
var
  UpstreamItem: TSfrSegmentItem;
begin
  UpstreamItem := FSfrBoundary.
    UpstreamSegmentValues[IcalcIndex] as TSfrSegmentItem;
  if FSfrPackage.Isfropt in [0, 4, 5] then
  begin
    UpstreamItem.HydraulicConductivity := FloatToStr(Segment.HCOND1);
    if FSfrPackage.Isfropt = 0 then
    begin
      UpstreamItem.StreamBedThickness := FloatToStr(Segment.THICKM1);
      UpstreamItem.StreambedElevation := FloatToStr(Segment.ELEVUP);
      UpstreamItem.StreamWidth := FloatToStr(Segment.WIDTH1);
      UpstreamItem.StreamDepth := FloatToStr(Segment.DEPTH1);
    end;
  end;
  if FSfrPackage.Isfropt in [4, 5] then
  begin
    if (Segment.ICalc in [1, 2]) and (StressPeriodIndex = 0) then
    begin
      UpstreamItem.StreamBedThickness := FloatToStr(Segment.THICKM1);
      UpstreamItem.StreambedElevation := FloatToStr(Segment.ELEVUP);
    end;
  end;
  if FSfrPackage.Isfropt <= 1 then
  begin
    UpstreamItem.StreamWidth := FloatToStr(Segment.WIDTH1);
  end
  else
  begin
    if StressPeriodIndex = 0 then
    begin
      UpstreamItem.StreamWidth := FloatToStr(Segment.WIDTH1);
    end;
  end;
end;

procedure TSfrImporter.AssignEquationValues(IcalcIndex: Integer;
  Segment: TSegment);
var
  EqItem: TSfrEquationItem;
begin
  if Segment.ICalc = 3 then
  begin
    EqItem := FSfrBoundary.EquationValues[IcalcIndex] as TSfrEquationItem;
    EqItem.DepthCoefficient := FloatToStr(Segment.CDPTH);
    EqItem.DepthExponent := FloatToStr(Segment.FDPTH);
    EqItem.WidthCoefficient := FloatToStr(Segment.AWDTH);
    EqItem.WidthExponent := FloatToStr(Segment.BWDTH);
  end;
end;

procedure TSfrImporter.AssignChannelValues(IcalcIndex: Integer;
  StressPeriodIndex: Integer; Segment: TSegment);
var
  CrossSectionIndex: Integer;
  ChannelItem: TSfrChannelItem;
begin
  if Segment.ICalc in [1, 2] then
  begin
    ChannelItem := FSfrBoundary.ChannelValues[IcalcIndex] as TSfrChannelItem;
    ChannelItem.ChannelRoughness := FloatToStr(Segment.ROUGHCH);
    if Segment.ICalc = 2 then
    begin
      ChannelItem.BankRoughness := FloatToStr(Segment.ROUGHBK);
    end;
    if Segment.ICalc = 2 then
    begin
      if (StressPeriodIndex = 0) or (FSfrPackage.Isfropt <= 1) then
      begin
        for CrossSectionIndex := 0 to 7 do
        begin
          ChannelItem.X[CrossSectionIndex] :=
            FloatToStr(Segment.TableX[CrossSectionIndex]);
          ChannelItem.Z[CrossSectionIndex] :=
            FloatToStr(Segment.TableZ[CrossSectionIndex]);
        end;
      end;
    end;
  end;
end;

procedure TSfrImporter.AssignSegmentFlowValues(IcalcIndex: Integer;
  Segment: TSegment);
var
  FlowItem: TSfrSegmentFlowItem;
begin
  FlowItem := FSfrBoundary.SegmentFlows[IcalcIndex] as TSfrSegmentFlowItem;
  FlowItem.Flow := FloatToStr(Segment.FLOW);
  FlowItem.Precipitation := FloatToStr(Segment.PPTSW);
  FlowItem.Evapotranspiration := FloatToStr(Segment.ETSW);
  FlowItem.Runnoff := FloatToStr(Segment.RUNOFF);
end;

procedure TSfrImporter.AssignFlowTableValues(IcalcIndex: Integer;
  Segment: TSegment);
var
  Table: TSfrTablelItem;
  TableIndex: Integer;
  TableRow: TSfrTableRowItem;
  TableItem: TFlowTableItem;
begin
  if Segment.ICalc = 4 then
  begin
    Table := FSfrBoundary.TableCollection[IcalcIndex] as TSfrTablelItem;
    for TableIndex := 0 to Segment.NSTRPTS - 1 do
    begin
      TableRow := Table.SfrTable.Add as TSfrTableRowItem;
      TableItem := Segment.FFlowTable[TableIndex];
      TableRow.Flow := FloatToStr(TableItem.FLOWTAB);
      TableRow.Depth := FloatToStr(TableItem.DPTHTAB);
      TableRow.Width := FloatToStr(TableItem.WDTHTAB);
    end;
  end;
end;

procedure TSfrImporter.AssignParamIcalcValues(IcalcIndex: Integer;
  Segment: TSegment);
var
  Item: TSfrParamIcalcItem;
begin
  Item := FSfrBoundary.ParamIcalc.Items[IcalcIndex];
  Item.ICalc := Segment.ICALC;
  Item.OutflowSegment := Segment.OUTSEG;
  Item.DiversionSegment := Segment.IUPSEG;
  if Item.DiversionSegment > 0 then
  begin
    Item.IPRIOR := Segment.IPRIOR;
  end;
end;

procedure TSfrImporter.AssignStartAndEndTimes;
var
  StressPeriod: TListStressPeriod;
  EndTime: Double;
  StartTime: Double;
  StressPeriodIndex: Integer;
  Item: TSfrItem;
  SP: TListStressPeriod;
  InnerIndex: Integer;
  ParamIcalcItem: TSfrParamIcalcItem;
  TableItem: TSfrTablelItem;
  FlowItem: TSfrSegmentFlowItem;
  ChannelItem: TSfrChannelItem;
  EqItem: TSfrEquationItem;
  UpstreamItem: TSfrSegmentItem;
  DownstreamItem: TSfrSegmentItem;
  UpUnsatItem: TSfrUnsatSegmentItem;
  DownUnsatItem: TSfrUnsatSegmentItem;
begin
  for StressPeriodIndex := 0 to FStressPeriods.ArrayLength - 1 do
  begin
    // initialize the start and end times for when the boundary will be
    // applied.
    StartTime := FModel.ModflowStressPeriods[StressPeriodIndex].StartTime;
    EndTime := FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
    StressPeriod := FStressPeriods[StressPeriodIndex];
    if StressPeriod.Reuse and not FParamUsed then
    begin
      Continue;
    end
    else
    begin
      // Update the endtime if the boundaries from the current
      // stress period will be reused in subsequent stress periods.
      for InnerIndex := StressPeriodIndex + 1 to FStressPeriods.ArrayLength - 1 do
      begin
        SP := FStressPeriods[InnerIndex];
        if SP.Reuse and not FParamUsed then
        begin
          EndTime := FModel.ModflowStressPeriods[InnerIndex].EndTime;
        end
        else
        begin
          break;
        end;
      end;
    end;
    if StressPeriodIndex = 0 then
    begin
      Item := FSfrBoundary.Values.Add as TSfrItem;
      Item.StartTime := StartTime;
      Item.EndTime := EndTime;
    end;

    ParamIcalcItem := FSfrBoundary.ParamIcalc.Add as TSfrParamIcalcItem;
    ParamIcalcItem.StartTime := StartTime;
    ParamIcalcItem.EndTime := EndTime;

    TableItem := FSfrBoundary.TableCollection.Add as TSfrTablelItem;
    TableItem.StartTime := StartTime;
    TableItem.EndTime := EndTime;

    FlowItem := FSfrBoundary.SegmentFlows.Add as TSfrSegmentFlowItem;
    FlowItem.StartTime := StartTime;
    FlowItem.EndTime := EndTime;

    ChannelItem := FSfrBoundary.ChannelValues.Add as TSfrChannelItem;
    ChannelItem.StartTime := StartTime;
    ChannelItem.EndTime := EndTime;

    EqItem := FSfrBoundary.EquationValues.Add as TSfrEquationItem;
    EqItem.StartTime := StartTime;
    EqItem.EndTime := EndTime;

    UpstreamItem := FSfrBoundary.UpstreamSegmentValues.Add as TSfrSegmentItem;
    UpstreamItem.StartTime := StartTime;
    UpstreamItem.EndTime := EndTime;

    DownstreamItem := FSfrBoundary.DownstreamSegmentValues.Add
      as TSfrSegmentItem;
    DownstreamItem.StartTime := StartTime;
    DownstreamItem.EndTime := EndTime;

    if StressPeriodIndex = 0 then
    begin
      UpUnsatItem := FSfrBoundary.UpstreamUnsatSegmentValues.Add
        as TSfrUnsatSegmentItem;
      UpUnsatItem.StartTime := StartTime;
      UpUnsatItem.EndTime := EndTime;

      DownUnsatItem := FSfrBoundary.DownstreamUnsatSegmentValues.Add
        as TSfrUnsatSegmentItem;
      DownUnsatItem.StartTime := StartTime;
      DownUnsatItem.EndTime := EndTime;
    end;
  end;
end;

procedure TSfrImporter.AssignReachValues(List: TList;
  ScreenObject: TScreenObject);
var
  Values: TValueArrayStorage;
  ValueArray: TValueArrayItem;
  Item: TSfrItem;
  Index: Integer;
  Reach: TSfrLocationObject;
begin

  Item := FSfrBoundary.Values[0] as TSfrItem;
  if List.Count > 1 then
  begin
    ValueArray := ScreenObject.ImportedValues.Add as TValueArrayItem;
    ValueArray.Name := 'RCHLEN';
    Values := ValueArray.Values;
    Values.DataType := rdtDouble;
    Values.Count := List.Count;
    for Index := 0 to List.Count - 1 do
    begin
      Reach := List[Index];
      Values.RealValues[Index] := Reach.RCHLEN;
    end;
    Item.ReachLength := rsObjectImportedValuesR + '("RCHLEN")';
  end
  else
  begin
    Reach := List[0];
    Item.ReachLength := FloatToStr(Reach.RCHLEN);
  end;
  if FSfrPackage.Isfropt in [1, 2, 3] then
  begin
    if List.Count > 1 then
    begin
      ValueArray := ScreenObject.ImportedValues.Add as TValueArrayItem;
      ValueArray.Name := 'STRTOP';
      Values := ValueArray.Values;
      Values.DataType := rdtDouble;
      Values.Count := List.Count;
      for Index := 0 to List.Count - 1 do
      begin
        Reach := List[Index];
        Values.RealValues[Index] := Reach.STRTOP;
      end;
      Item.StreambedElevation := rsObjectImportedValuesR + '("STRTOP")';
    end
    else
    begin
      Reach := List[0];
      Item.StreambedElevation := FloatToStr(Reach.STRTOP);
    end;
    if List.Count > 1 then
    begin
      ValueArray := ScreenObject.ImportedValues.Add as TValueArrayItem;
      ValueArray.Name := 'SLOPE';
      Values := ValueArray.Values;
      Values.DataType := rdtDouble;
      Values.Count := List.Count;
      for Index := 0 to List.Count - 1 do
      begin
        Reach := List[Index];
        Values.RealValues[Index] := Reach.SLOPE;
      end;
      Item.StreamSlope := rsObjectImportedValuesR + '("SLOPE")';
    end
    else
    begin
      Reach := List[0];
      Item.StreamSlope := FloatToStr(Reach.SLOPE);
    end;
    if List.Count > 1 then
    begin
      ValueArray := ScreenObject.ImportedValues.Add as TValueArrayItem;
      ValueArray.Name := 'STRTHICK';
      Values := ValueArray.Values;
      Values.DataType := rdtDouble;
      Values.Count := List.Count;
      for Index := 0 to List.Count - 1 do
      begin
        Reach := List[Index];
        Values.RealValues[Index] := Reach.STRTHICK;
      end;
      Item.StreamBedThickness := rsObjectImportedValuesR + '("STRTHICK")';
    end
    else
    begin
      Reach := List[0];
      Item.StreamBedThickness := FloatToStr(Reach.STRTHICK);
    end;
    if List.Count > 1 then
    begin
      ValueArray := ScreenObject.ImportedValues.Add as TValueArrayItem;
      ValueArray.Name := 'STRHC1';
      Values := ValueArray.Values;
      Values.DataType := rdtDouble;
      Values.Count := List.Count;
      for Index := 0 to List.Count - 1 do
      begin
        Reach := List[Index];
        Values.RealValues[Index] := Reach.STRHC1;
      end;
      Item.HydraulicConductivity := rsObjectImportedValuesR + '("STRHC1")';
    end
    else
    begin
      Reach := List[0];
      Item.HydraulicConductivity := FloatToStr(Reach.STRHC1);
    end;
  end;
  if FSfrPackage.Isfropt in [2, 3] then
  begin
    if List.Count > 1 then
    begin
      ValueArray := ScreenObject.ImportedValues.Add as TValueArrayItem;
      ValueArray.Name := 'THTS';
      Values := ValueArray.Values;
      Values.DataType := rdtDouble;
      Values.Count := List.Count;
      for Index := 0 to List.Count - 1 do
      begin
        Reach := List[Index];
        Values.RealValues[Index] := Reach.THTS;
      end;
      Item.SaturatedWaterContent := rsObjectImportedValuesR + '("THTS")';
    end
    else
    begin
      Reach := List[0];
      Item.SaturatedWaterContent := FloatToStr(Reach.THTS);
    end;
    if List.Count > 1 then
    begin
      ValueArray := ScreenObject.ImportedValues.Add as TValueArrayItem;
      ValueArray.Name := 'THTI';
      Values := ValueArray.Values;
      Values.DataType := rdtDouble;
      Values.Count := List.Count;
      for Index := 0 to List.Count - 1 do
      begin
        Reach := List[Index];
        Values.RealValues[Index] := Reach.THTI;
      end;
      Item.InitialWaterContent := rsObjectImportedValuesR + '("THTI")';
    end
    else
    begin
      Reach := List[0];
      Item.InitialWaterContent := FloatToStr(Reach.THTI);
    end;
    if List.Count > 1 then
    begin
      ValueArray := ScreenObject.ImportedValues.Add as TValueArrayItem;
      ValueArray.Name := 'EPS';
      Values := ValueArray.Values;
      Values.DataType := rdtDouble;
      Values.Count := List.Count;
      for Index := 0 to List.Count - 1 do
      begin
        Reach := List[Index];
        Values.RealValues[Index] := Reach.EPS;
      end;
      Item.BrooksCoreyExponent := rsObjectImportedValuesR + '("EPS")';
    end
    else
    begin
      Reach := List[0];
      Item.BrooksCoreyExponent := FloatToStr(Reach.EPS);
    end;
  end;
  if FSfrPackage.Isfropt in [2, 3] then
  begin
    if List.Count > 1 then
    begin
      ValueArray := ScreenObject.ImportedValues.Add as TValueArrayItem;
      ValueArray.Name := 'UHC';
      Values := ValueArray.Values;
      Values.DataType := rdtDouble;
      Values.Count := List.Count;
      for Index := 0 to List.Count - 1 do
      begin
        Reach := List[Index];
        Values.RealValues[Index] := Reach.UHC;
      end;
      Item.VerticalK := rsObjectImportedValuesR + '("UHC")';
    end
    else
    begin
      Reach := List[0];
      Item.VerticalK := FloatToStr(Reach.UHC);
    end;
  end;
end;

procedure TSfrImporter.ReadFirstDataSet5WithParameters;
var
  IRDFLG: Integer;
  IPTFLG: Integer;
begin
  Read(FImporter.FFile, ITMP);
  Read(FImporter.FFile, IRDFLG);
  Read(FImporter.FFile, IPTFLG);
  Read(FImporter.FFile, NP);
  Inc(FCurrentStressPeriod);
  InitializeStressPeriods;
  FStressPeriodSegments.ArrayLength :=
    FModel.ModflowStressPeriods.Count;
  InitializeCurrentStressPeriod;
end;

procedure TSfrImporter.ReadFirstDataSet5WithoutParameters;
var
  IPTFLG: Integer;
  IRDFLG: Integer;
begin
  Read(FImporter.FFile, ITMP);
  Read(FImporter.FFile, IRDFLG);
  Read(FImporter.FFile, IPTFLG);
  NP := 0;
  Inc(FCurrentStressPeriod);
  InitializeStressPeriods;
  FStressPeriodSegments.ArrayLength :=
    FModel.ModflowStressPeriods.Count;
  InitializeCurrentStressPeriod;
end;

procedure TSfrImporter.ReadFlowTableWidths;
var
  Index: Integer;
begin
  for Index := 0 to FCurrentSegment.NSTRPTS - 1 do
  begin
    Read(FImporter.FFile, FCurrentSegment.FFlowTable[Index].WDTHTAB);
  end;
  ReadLn(FImporter.FFile);
  if SegCount = NLST then
  begin
    Inc(CurrentInstance);
  end;
end;

procedure TSfrImporter.ReadFlowTableDepths;
var
  Index: Integer;
begin
  for Index := 0 to FCurrentSegment.NSTRPTS - 1 do
  begin
    Read(FImporter.FFile, FCurrentSegment.FFlowTable[Index].DPTHTAB);
  end;
  ReadLn(FImporter.FFile);
end;

procedure TSfrImporter.ReadFlowTableFlows;
var
  Index: Integer;
begin
  for Index := 0 to FCurrentSegment.NSTRPTS - 1 do
  begin
    Read(FImporter.FFile, FCurrentSegment.FFlowTable[Index].FLOWTAB);
  end;
  ReadLn(FImporter.FFile);
end;

procedure TSfrImporter.ReadTableZValues;
var
  Index: Integer;
begin
  for Index := 0 to Length(FCurrentSegment.TableZ) - 1 do
  begin
    Read(FImporter.FFile, FCurrentSegment.TableZ[Index]);
  end;
  ReadLn(FImporter.FFile);
  if SegCount = NLST then
  begin
    Inc(CurrentInstance);
  end;
end;

procedure TSfrImporter.ReadTableXValues;
var
  Index: Integer;
begin
  for Index := 0 to Length(FCurrentSegment.TableX) - 1 do
  begin
    Read(FImporter.FFile, FCurrentSegment.TableX[Index]);
  end;
  ReadLn(FImporter.FFile);
end;

procedure TSfrImporter.ReadSegment4b6a(const ALabel: string;
  IsParameter: boolean);
var
  LabelList: TStringList;
  Index: Integer;
  DataLabel: string;
  Instance: TSfrInstanceObject;
  NSEG: integer;
begin
  Inc(SegCount);

  LabelList := TStringList.Create;
  try
    LabelList.Delimiter := ',';
    LabelList.DelimitedText := ALabel;
    Assert(LabelList.Count > 0);
    Assert(LabelList[0] = 'NSEG');
    Read(FImporter.FFile, NSEG);
    if IsParameter then
    begin
      Instance := FParameters[CurrentParameter].
        Instances[CurrentInstance] as TSfrInstanceObject;
      FCurrentSegment := Instance.Segments[FCurrentSegmentIndex];
    end
    else
    begin
      FCurrentSegment := FCurrentSegments[NSEG - 1];
    end;
    FCurrentSegment.NSEG := NSEG;
    for Index := 1 to LabelList.Count - 1 do
    begin
      DataLabel := LabelList[Index];
      if Index = LabelList.Count - 1 then
      begin
        // Remove colon at end.
        SetLength(DataLabel, Length(DataLabel)-1);
      end;
      if DataLabel = 'ICALC' then
      begin
        Read(FImporter.FFile, FCurrentSegment.ICALC);
      end
      else if DataLabel = 'OUTSEG' then
      begin
        Read(FImporter.FFile, FCurrentSegment.OUTSEG);
      end
      else if DataLabel = 'IUPSEG' then
      begin
        Read(FImporter.FFile, FCurrentSegment.IUPSEG);
      end
      else if DataLabel = 'IPRIOR' then
      begin
        Read(FImporter.FFile, FCurrentSegment.IPRIOR);
      end
      else if DataLabel = 'NSTRPTS' then
      begin
        Read(FImporter.FFile, FCurrentSegment.NSTRPTS);
        FCurrentSegment.FFlowTable.ArrayLength := FCurrentSegment.NSTRPTS;
      end
      else if DataLabel = 'FLOW' then
      begin
        Read(FImporter.FFile, FCurrentSegment.FLOW);
      end
      else if DataLabel = 'RUNOFF' then
      begin
        Read(FImporter.FFile, FCurrentSegment.RUNOFF);
      end
      else if DataLabel = 'ETSW' then
      begin
        Read(FImporter.FFile, FCurrentSegment.ETSW);
      end
      else if DataLabel = 'PPTSW' then
      begin
        Read(FImporter.FFile, FCurrentSegment.PPTSW);
      end
      else if DataLabel = 'ROUGHCH' then
      begin
        Read(FImporter.FFile, FCurrentSegment.ROUGHCH);
      end
      else if DataLabel = 'ROUGHBK' then
      begin
        Read(FImporter.FFile, FCurrentSegment.ROUGHBK);
      end
      else if DataLabel = 'CDPTH' then
      begin
        Read(FImporter.FFile, FCurrentSegment.CDPTH);
      end
      else if DataLabel = 'FDPTH' then
      begin
        Read(FImporter.FFile, FCurrentSegment.FDPTH);
      end
      else if DataLabel = 'AWDTH' then
      begin
        Read(FImporter.FFile, FCurrentSegment.AWDTH);
      end
      else if DataLabel = 'BWDTH' then
      begin
        Read(FImporter.FFile, FCurrentSegment.BWDTH);
      end
      else
      begin
        Assert(False);
      end;
    end;
    ReadLn(FImporter.FFile);
  finally
    LabelList.Free;
  end;
end;

procedure TSfrImporter.ReadSegment4c6b(const ALabel: string);
var
  LabelList: TStringList;
  Index: Integer;
  DataLabel: string;
begin
  LabelList := TStringList.Create;
  try
    LabelList.Delimiter := ',';
    LabelList.DelimitedText := ALabel;
    Assert(LabelList.Count > 0);
    for Index := 0 to LabelList.Count - 1 do
    begin
      DataLabel := LabelList[Index];
      if Index = LabelList.Count - 1 then
      begin
        // Remove colon at end.
        SetLength(DataLabel, Length(DataLabel)-1);
      end;
      if DataLabel = 'HCOND1' then
      begin
        Read(FImporter.FFile, FCurrentSegment.HCOND1);
      end
      else if DataLabel = 'THICKM1' then
      begin
        Read(FImporter.FFile, FCurrentSegment.THICKM1);
      end
      else if DataLabel = 'ELEVUP' then
      begin
        Read(FImporter.FFile, FCurrentSegment.ELEVUP);
      end
      else if DataLabel = 'WIDTH1' then
      begin
        Read(FImporter.FFile, FCurrentSegment.WIDTH1);
      end
      else if DataLabel = 'DEPTH1' then
      begin
        Read(FImporter.FFile, FCurrentSegment.DEPTH1);
      end
      else if DataLabel = 'THTS1' then
      begin
        Read(FImporter.FFile, FCurrentSegment.THTS1);
      end
      else if DataLabel = 'THTI1' then
      begin
        Read(FImporter.FFile, FCurrentSegment.THTI1);
      end
      else if DataLabel = 'EPS1' then
      begin
        Read(FImporter.FFile, FCurrentSegment.EPS1);
      end
      else if DataLabel = 'UHC1' then
      begin
        Read(FImporter.FFile, FCurrentSegment.UHC1);
      end
      else
      begin
        Assert(False);
      end;
    end;
    ReadLn(FImporter.FFile);
  finally
    LabelList.Free;
  end;
end;

procedure TSfrImporter.ReadSegment4d6c(const ALabel: string);
var
  LabelList: TStringList;
  Index: Integer;
  DataLabel: string;
begin
  LabelList := TStringList.Create;
  try
    LabelList.Delimiter := ',';
    LabelList.DelimitedText := ALabel;
    Assert(LabelList.Count > 0);
    for Index := 0 to LabelList.Count - 1 do
    begin
      DataLabel := LabelList[Index];
      if Index = LabelList.Count - 1 then
      begin
        // Remove colon at end.
        SetLength(DataLabel, Length(DataLabel)-1);
      end;
      if DataLabel = 'HCOND2' then
      begin
        Read(FImporter.FFile, FCurrentSegment.HCOND2);
      end
      else if DataLabel = 'THICKM2' then
      begin
        Read(FImporter.FFile, FCurrentSegment.THICKM2);
      end
      else if DataLabel = 'ELEVDN' then
      begin
        Read(FImporter.FFile, FCurrentSegment.ELEVDN);
      end
      else if DataLabel = 'WIDTH2' then
      begin
        Read(FImporter.FFile, FCurrentSegment.WIDTH2);
      end
      else if DataLabel = 'DEPTH2' then
      begin
        Read(FImporter.FFile, FCurrentSegment.DEPTH2);
      end
      else if DataLabel = 'THTS2' then
      begin
        Read(FImporter.FFile, FCurrentSegment.THTS2);
      end
      else if DataLabel = 'THTI2' then
      begin
        Read(FImporter.FFile, FCurrentSegment.THTI2);
      end
      else if DataLabel = 'EPS2' then
      begin
        Read(FImporter.FFile, FCurrentSegment.EPS2);
      end
      else if DataLabel = 'UHC2' then
      begin
        Read(FImporter.FFile, FCurrentSegment.UHC2);
      end
      else
      begin
        Assert(False);
      end;
    end;
    ReadLn(FImporter.FFile);
  finally
    LabelList.Free;
  end;
  if not (FCurrentSegment.ICALC in [2,4]) then
  begin
    if SegCount = NLST then
    begin
      Inc(CurrentInstance);
    end;
  end;
end;

procedure TSfrImporter.ReadSegmentStart(IsParameter: boolean);
var
  NSEG: Integer;
  Instance: TSfrInstanceObject;

begin
  SegCount := 0;
  Read(FImporter.FFile, NSEG);
  if IsParameter then
  begin
    Inc(FCurrentSegmentIndex);
    Instance := FParameters[CurrentParameter].
      Instances[CurrentInstance] as TSfrInstanceObject;
    FCurrentSegment := Instance.Segments[FCurrentSegmentIndex];
  end
  else
  begin
    FCurrentSegment := FCurrentSegments[NSEG - 1];
  end;
  FCurrentSegment.NSEG := NSEG;
  Read(FImporter.FFile, FCurrentSegment.ICALC);
  Read(FImporter.FFile, FCurrentSegment.OUTSEG);
  Read(FImporter.FFile, FCurrentSegment.IUPSEG);
  ReadLn(FImporter.FFile);
end;

function TSfrImporter.GetBoundary(
  ScreenObject: TScreenObject): TModflowParamBoundary;
begin
  Assert(False);
  result := nil;
end;

function TSfrImporter.DefaultInstanceName(StressPeriodIndex: integer): string;
begin
  result := 'I_' + IntToStr(StressPeriodIndex+1);
end;

procedure TSfrImporter.HandlePackage;
var
  LocationList: TList;
  Index: Integer;
  PriorSegNumber: Integer;
  Location: TSfrLocationObject;
  ScreenObjectIndex: integer;
  ParameterSegments: TIntegerList;
begin
  if FImportedPackage or (FCurrentStressPeriod < 0) or
    (FCurrentStressPeriod < FStressPeriods.ArrayLength -1) then
  begin
    Exit;
  end;
  inherited;
  FSfrPackage := FModel.ModflowPackages.SfrPackage;
  FSfrPackage.IsSelected := True;
  FSfrPackage.Comments := FComments;
  FSfrPackage.Dleak := self.DLEAK;
  FSfrPackage.Isfropt := self.ISFROPT;
  FSfrPackage.Nstrail := self.NSTRAIL;
  FSfrPackage.Isuzn := self.ISUZN;
  FSfrPackage.Nsfrsets := self.NSFRSETS;
  FSfrPackage.KinematicRouting := self.IRTFLG > 0;
  if FSfrPackage.KinematicRouting then
  begin
    FSfrPackage.TimeStepsForKinematicRouting := NUMTIM;
    FSfrPackage.KinematicRoutingTolerance := FLWTOL;
    FSfrPackage.KinematicRoutingWeight := WEIGHT;
  end;

  ParameterSegments := TIntegerList.Create;
  try
    ParameterSegments.Sorted := True;
    CreateParamInstances(ParameterSegments);

    ScreenObjectIndex := 0;
    LocationList := TList.Create;
    try
      LocationList.Capacity := FReaches.ArrayLength;
      PriorSegNumber := -1;
      for Index := 0 to FReaches.ArrayLength - 1 do
      begin
        Location := FReaches.Reaches[Index];
        if (Index = 0) then
        begin
          PriorSegNumber := Location.SegmentNumber;
        end;
        if (Location.SegmentNumber = PriorSegNumber) then
        begin
          LocationList.Add(Location);
        end;
        if (Location.SegmentNumber <> PriorSegNumber) then
        begin
          if LocationList.Count > 0 then
          begin
            FParamUsed := ParameterSegments.IndexOf(PriorSegNumber) >= 0;
            CreateStream(LocationList, ScreenObjectIndex);
            if FParamUsed then
            begin
              AssignParameterSegmentProperties(PriorSegNumber);
            end
            else
            begin
              AssignSegmentProperties;
            end;
            LocationList.Clear;
            LocationList.Capacity := FReaches.ArrayLength;
          end;
          LocationList.Add(Location);
          PriorSegNumber := Location.SegmentNumber;
        end;
        if (Index = FReaches.ArrayLength - 1) then
        begin
          if (LocationList.Count = 0)
            or (LocationList[LocationList.Count-1] <> Location) then
          begin
            LocationList.Add(Location);
            PriorSegNumber := Location.SegmentNumber;
          end;
          FParamUsed := ParameterSegments.IndexOf(PriorSegNumber) >= 0;
          CreateStream(LocationList, ScreenObjectIndex);
          if FParamUsed then
          begin
            AssignParameterSegmentProperties(PriorSegNumber);
          end
          else
          begin
            AssignSegmentProperties;
          end;
          LocationList.Clear;
        end;
      end;
    finally
      LocationList.Free;
    end;
  finally
    ParameterSegments.Free;
  end;
  ReleaseMemory;
end;

procedure TSfrImporter.ReadData(const ALabel: string);
begin
  inherited;
  if FCurrentStressPeriod = -1 then
  begin
    if ALabel =
      'NSTRM, NSS, NSFRPAR, NPARSEG, CONST, DLEAK, ISTCB1, ISTCB2:' then
    begin
      ReadBasicData;
    end
    else if ALabel = 'ISFROPT:' then
    begin
      ReadIsfropt;
    end
    else if ALabel = 'NSTRAIL, ISUZN, NSFRSETS:' then
    begin
      ReadUnsatParameters;
    end
    else if ALabel = 'IRTFLG, NUMTIM, WEIGHT, FLWTOL:' then
    begin
      Read(FImporter.FFile, IRTFLG);
      Read(FImporter.FFile, NUMTIM);
      Read(FImporter.FFile, WEIGHT);
      Read(FImporter.FFile, FLWTOL);
      Readln(FImporter.FFile);
    end
    else if ALabel = 'KRCH IRCH JRCH ISEG IREACH:' then
    begin
      Inc(FCurrentReachIndex);
      FCurrentReach := FReaches[FCurrentReachIndex] as TSfrLocationObject;
      Read(FImporter.FFile, FCurrentReach.Layer);
      Read(FImporter.FFile, FCurrentReach.Row);
      Read(FImporter.FFile, FCurrentReach.Column);
      Read(FImporter.FFile, FCurrentReach.SegmentNumber);
      Read(FImporter.FFile, FCurrentReach.ReachNumber);
      Readln(FImporter.FFile);
    end
    else if ALabel = 'RCHLEN, STRTOP, SLOPE, STRTHICK, STRHC1:' then
    begin
      Read(FImporter.FFile, FCurrentReach.RCHLEN);
      Read(FImporter.FFile, FCurrentReach.STRTOP);
      Read(FImporter.FFile, FCurrentReach.SLOPE);
      Read(FImporter.FFile, FCurrentReach.STRTHICK);
      Read(FImporter.FFile, FCurrentReach.STRHC1);
      Readln(FImporter.FFile);
    end
    else if ALabel = 'THTS,THTI,EPS:' then
    begin
      Read(FImporter.FFile, FCurrentReach.THTS);
      Read(FImporter.FFile, FCurrentReach.THTI);
      Read(FImporter.FFile, FCurrentReach.EPS);
      Readln(FImporter.FFile);
    end
    else if ALabel = 'UHC:' then
    begin
      Read(FImporter.FFile, FCurrentReach.UHC);
      Readln(FImporter.FFile);
    end
    else if ALabel = 'PARNAM:' then
    begin
      ReadParameterName;
      FCurrentSegmentIndex := -1;
    end
    else if ALabel = 'PARTYP:' then
    begin
      ReadParameterType;
    end
    else if ALabel = 'Parval,NLST:' then
    begin
      ReadParameterValueAndLocationCount;
    end
    else if ALabel = 'INSTANCES:' then
    begin
      // do nothing
    end
    else if ALabel = 'NUMINST:' then
    begin
      ReadNumberOfInstances;
    end
    else if ALabel = 'INAME(ILOC):' then
    begin
      ReadInstanceName;
      FCurrentSegmentIndex := -1;
      SegCount := 0;
    end
    else if ALabel = 'n, icalc, noutseg, iupseg:' then
    begin
      ReadSegmentStart(True);
    end
    else if Copy(ALabel, 1,5) = 'NSEG,' then
    begin
      ReadSegment4b6a(ALabel, True);
    end
    else if (Copy(ALabel, 1,7) = 'HCOND1,')
      or (Copy(ALabel, 1,7) = 'WIDTH1,') then
    begin
      ReadSegment4c6b(ALabel);
    end
    else if (Copy(ALabel, 1,7) = 'HCOND2,')
      or (Copy(ALabel, 1,7) = 'WIDTH2,') then
    begin
      ReadSegment4d6c(ALabel);
    end
    else if (ALabel = 'XCPT1 XCPT2 ... XCPT8:') then
    begin
      ReadTableXValues;
    end
    else if (ALabel = 'ZCPT1 ZCPT2 ... ZCPT8:') then
    begin
      ReadTableZValues;
    end
    else if (ALabel = 'FLOWTAB(1) FLOWTAB(2) ... FLOWTAB(NSTRPTS):') then
    begin
      ReadFlowTableFlows;
    end
    else if (ALabel = 'DPTHTAB(1) DPTHTAB(2) ... DPTHTAB(NSTRPTS):') then
    begin
      ReadFlowTableDepths;
    end
    else if (ALabel = 'WDTHTAB(1) WDTHTAB(2) ... WDTHTAB(NSTRPTS):') then
    begin
      ReadFlowTableWidths;
    end
    else if ALabel = 'ITMP, IRDFLG, IPTFLG:' then
    begin
      ReadFirstDataSet5WithoutParameters;
    end
    else if ALabel = 'ITMP, IRDFLG, IPTFLG, NP:' then
    begin
      ReadFirstDataSet5WithParameters;
    end
    else
    begin
      Assert(False);
    end;
  end
  else
  begin
    if ALabel = 'ITMP, IRDFLG, IPTFLG:' then
    begin
      ReadDataSet5WithoutParameters;
    end
    else if ALabel = 'ITMP, IRDFLG, IPTFLG, NP:' then
    begin
      ReadDataSet5WithParameters;
    end
    else if ALabel = 'n, icalc, noutseg, iupseg:' then
    begin
      ReadSegmentStart(False);
    end
    else if Copy(ALabel, 1,5) = 'NSEG,' then
    begin
      ReadSegment4b6a(ALabel, False);
    end
    else if (Copy(ALabel, 1,6) = 'HCOND1')
      or (Copy(ALabel, 1,6) = 'WIDTH1') then
    begin
      ReadSegment4c6b(ALabel);
    end
    else if (Copy(ALabel, 1,6) = 'HCOND2')
      or (Copy(ALabel, 1,6) = 'WIDTH2') then
    begin
      ReadSegment4d6c(ALabel);
    end
    else if (ALabel = 'XCPT1 XCPT2 ... XCPT8:') then
    begin
      ReadTableXValues;
    end
    else if (ALabel = 'ZCPT1 ZCPT2 ... ZCPT8:') then
    begin
      ReadTableZValues;
    end
    else if (ALabel = 'FLOWTAB(1) FLOWTAB(2) ... FLOWTAB(NSTRPTS):') then
    begin
      ReadFlowTableFlows;
    end
    else if (ALabel = 'DPTHTAB(1) DPTHTAB(2) ... DPTHTAB(NSTRPTS):') then
    begin
      ReadFlowTableDepths;
    end
    else if (ALabel = 'WDTHTAB(1) WDTHTAB(2) ... WDTHTAB(NSTRPTS):') then
    begin
      ReadFlowTableWidths;
    end
    else if (ALabel = 'Pname:') then
    begin
      ReadParamNameForStressPeriod
    end
    else if (ALabel = 'Iname:') then
    begin
      ReadInstanceNameForStressPeriod;
    end
    else
    begin
      Assert(False);
    end;
  end;
end;

procedure TSfrImporter.InitializeCurrentStressPeriod;
begin
  inherited;
  FCurrentSegments := FStressPeriodSegments[FCurrentStressPeriod];
  FCurrentSegments.ArrayLength := NSS;
end;

function TSfrImporter.ParameterType: TParameterType;
begin
  result := ptSFR;
end;

{ TSfrInstanceObject }

function TSfrInstanceObject.ArrayMemberClass: TArrayMemberClass;
begin
  result := TSfrLocationObject;
end;

constructor TSfrInstanceObject.Create;
begin
  inherited;
  FSegments:= TSegmentArray.Create;
end;

destructor TSfrInstanceObject.Destroy;
begin
  FSegments.Free;
  inherited;
end;

function TSfrInstanceObject.GetSegByNumber(NSEG: integer): TSegment;
var
  Index: Integer;
  Segment: TSegment;
begin
  result := nil;
  for Index := 0 to Segments.ArrayLength - 1 do
  begin
    Segment := Segments[Index];
    if Segment.NSEG = NSEG then
    begin
      result := Segment;
      Exit;
    end;
  end;
end;

{ TSfrParameterObject }

function TSfrParameterObject.ArrayMemberClass: TArrayMemberClass;
begin
  result := TSfrInstanceObject;
end;

{ TSfrParamArray }

function TSfrParamArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TSfrParameterObject;
end;

{ TSfrStressPeriod }

function TSfReaches.ArrayMemberClass: TArrayMemberClass;
begin
  result := TSfrLocationObject;
end;

{ TSfrStressPeriodArray }

function TSfrStressPeriodArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TSegmentArray;
end;

{ TSegmentArray }

function TSegmentArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TSegment;
end;

function TSegmentArray.GetSegments(Index: integer): TSegment;
begin
  result := Objects[Index] as TSegment;
end;

{ TFlowTableArray }

function TFlowTableArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TFlowTableItem;
end;

function TFlowTableArray.GetItem(Index: integer): TFlowTableItem;
begin
  result := Objects[Index] as TFlowTableItem;
end;

{ TSegment }

constructor TSegment.Create;
begin
  inherited;
  FFlowTable := TFlowTableArray.Create;
end;

destructor TSegment.Destroy;
begin
  FFlowTable.Free;
  inherited;
end;

function TSfReaches.GetReach(Index: integer): TSfrLocationObject;
begin
  result := Objects[Index] as TSfrLocationObject;
end;

{ TStressPeriodSegments }

function TStressPeriodSegments.ArrayMemberClass: TArrayMemberClass;
begin
  result := TSegmentArray;
end;

function TStressPeriodSegments.GetSegmentArray(Index: integer): TSegmentArray;
begin
  result := Objects[Index] as TSegmentArray;
end;

function TListStressPeriod.IndexOfParameter(
  const ParameterName: string): integer;
var
  Index: Integer;
begin
  result := -1;
  for Index := 0 to Length(Parameters) - 1 do
  begin
    if SameText(ParameterName, Parameters[Index]) then
    begin
      result := Index;
      Exit; 
    end;
  end;
end;

function TListParameterObject.GetInstanceByName(
  const InstanceName: string): TListInstanceObject;
var
  Index: Integer;
  Instance: TListInstanceObject;
begin
  result := nil;
  for Index := 0 to ArrayLength - 1 do
  begin
    Instance := Instances[Index];
    if CompareText(Instance.Name, InstanceName) = 0 then
    begin
      result := Instance;
      Exit;
    end;
  end;
end;

{ THobImporter }

function ML_Compare(Item1, Item2: Pointer): Integer;
var
  ML1, ML2: TMultLayerFactor;
begin
  ML1 := Item1;
  ML2 := Item2;
  result := ML1.Layer - ML2.Layer;
end;



constructor THobImporter.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'HOB');
  FHeadObservations:= THeadObsevationArray.Create;
  FCurrentObsIndex := -1;
end;

destructor THobImporter.Destroy;
begin
  FHeadObservations.Free;
  inherited;
end;

procedure THobImporter.HandlePackage;
var
  ObsIndex: Integer;
  Obs: THeadObservation;
  ScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  APoint: TPoint2D;
  HeadObservations: THobBoundary;
  ValueIndex: Integer;
  ObsTime: TMultiTimeObs;
  ObsItem: THobItem;
  Layer: Integer;
  Elevation: Real;
  HighLayer: Integer;
  LowLayer: Integer;
  LayerIndex: Integer;
  ML: TMultLayerFactor;
  MHItem: TMultiHeadItem;
  ML_List: TList;
  Row: integer;
  Column: integer;
const
  ScreenObjectNameRoot = 'Imported_Head_Observation';   
begin
  inherited;
  FHobPackage := FModel.ModflowPackages.HobPackage;
  FHobPackage.IsSelected := True;
  FHobPackage.Comments := FComments;
  FHobPackage.DryHead := HOBDRY;
  FHeadObservations.ArrayLength := FCurrentObsIndex+1;
  for ObsIndex := 0 to FHeadObservations.ArrayLength - 1 do
  begin
    Obs := FHeadObservations[ObsIndex];

    ScreenObject := TScreenObject.CreateWithViewDirection(FModel, vdTop,
      UndoCreateScreenObject, False);
    ScreenObject.Name := ScreenObjectNameRoot + '_'
      + IntToStr(ObsIndex+1);
    FModel.AddScreenObject(ScreenObject);
    ScreenObject.ElevationCount := ecZero;
    ScreenObject.SetValuesOfIntersectedCells := True;
    ScreenObject.EvaluatedAt := eaBlocks;
    ScreenObject.Visible := False;
    ScreenObject.Capacity := 1;
    APoint := FImporter.CenterPoints[Obs.Row-1, Obs.Column-1];
    APoint := FModel.Grid.
      RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
    APoint.x := APoint.x
      + FModel.Grid.ColumnWidth[Obs.Column-1]*Obs.ColumnOffset;
    APoint.y := APoint.y - FModel.Grid.RowWidth[Obs.Row-1]*Obs.RowOffset;
    APoint := FModel.Grid.
      RotateFromGridCoordinatesToRealWorldCoordinates(APoint);
    ScreenObject.AddPoint(APoint, True);

    Row := Obs.Row-1;
    if (Obs.RowOffset = -0.5) and (Row > 0) then
    begin
      Dec(Row);
    end;
    Column := Obs.Column-1;
    if (Obs.ColumnOffset = 0.5) and (Column < FModel.Grid.ColumnCount-1) then
    begin
      Inc(Column);
    end;
    if Obs.FMultiLayers.ArrayLength > 0 then
    begin
      HighLayer := Obs.FMultiLayers[0].Layer;
      LowLayer := HighLayer;
      for LayerIndex := 1 to Obs.FMultiLayers.ArrayLength - 1 do
      begin
        HighLayer := Min(HighLayer, Obs.FMultiLayers[LayerIndex].Layer);
        LowLayer  := Max(LowLayer , Obs.FMultiLayers[LayerIndex].Layer);
      end;
      ScreenObject.ElevationCount := ecTwo;

      Layer := FModel.LayerStructure.ModflowLayerToDataSetLayer(HighLayer);
      Elevation := FModel.Grid.ThreeDElementCenter(Column, Obs.Row, Layer).Z;
      ScreenObject.HigherElevationFormula := FloatToStr(Elevation);

      Layer := FModel.LayerStructure.ModflowLayerToDataSetLayer(LowLayer);
      Elevation := FModel.Grid.ThreeDElementCenter(Column, Row, Layer).Z;
      ScreenObject.LowerElevationFormula := FloatToStr(Elevation);
    end
    else
    begin
      Layer := FModel.LayerStructure.ModflowLayerToDataSetLayer(Obs.Layer);
      Elevation := FModel.Grid.ThreeDElementCenter(Column, Row, Layer).Z;
      ScreenObject.ElevationCount := ecOne;
      ScreenObject.ElevationFormula := FloatToStr(Elevation);
    end;

    ScreenObject.CreateHeadObservations;
    HeadObservations := ScreenObject.ModflowHeadObservations;
    HeadObservations.ObservationName := Trim(Obs.OBSNAM);
    HeadObservations.Values.Capacity := Obs.FObsTimes.ArrayLength;
    for ValueIndex := 0 to Obs.FObsTimes.ArrayLength - 1 do
    begin
      ObsTime := Obs.FObsTimes[ValueIndex];
      ObsItem := HeadObservations.Values.Add as THobItem;
      ObsItem.Head := ObsTime.HeadObservation;
      ObsItem.Time := FModel.ModflowStressPeriods[
        ObsTime.RefStressPeriod-1].StartTime
        + ObsTime.TimeOffset;
    end;
    if Obs.FObsTimes.ArrayLength > 1 then
    begin
      case Obs.ITT of
        1:
          begin
            HeadObservations.MultiObsMethod := momAllHeads;
          end;
        2:
          begin
            HeadObservations.MultiObsMethod := momHeadAndDrawdown;
          end;
        else Assert(False);
      end;
    end;
    if Obs.FMultiLayers.ArrayLength > 0 then
    begin
      HeadObservations.LayerFractions.Capacity := Obs.FMultiLayers.ArrayLength;
      ML_List := TList.Create;
      try
        ML_List.Capacity := Obs.FMultiLayers.ArrayLength;
        for LayerIndex := 0 to Obs.FMultiLayers.ArrayLength - 1 do
        begin
          ML := Obs.FMultiLayers[LayerIndex];
          ML_List.Add(ML);
        end;
        ML_List.Sort(ML_Compare);
        for LayerIndex := 0 to ML_List.Count - 1 do
        begin
          ML := ML_List[LayerIndex];
          MHItem := HeadObservations.LayerFractions.Add as TMultiHeadItem;
          Layer := FModel.LayerStructure.ModflowLayerToDataSetLayer(ML.Layer)+1;
          MHItem.Layer := Layer;
          MHItem.Proportion := ML.Proportion;
        end;
      finally
        ML_List.Free;
      end;
    end;
  end;
end;

procedure THobImporter.ReadDataSet6Values;
begin
  Read(FImporter.FFile, FCurrentObsTime.RefStressPeriod);
  Read(FImporter.FFile, FCurrentObsTime.TimeOffset);
  Read(FImporter.FFile, FCurrentObsTime.HeadObservation);
  ReadLn(FImporter.FFile);
  FCurrentObsTime.TimeOffset := FCurrentObsTime.TimeOffset*TOMULTH;
end;

procedure THobImporter.ReadDataSet6ObsName;
var
  OBSNAM: string;
begin
  Readln(FImporter.FFile, OBSNAM);
  Inc(FCurrentTimeIndex);
  FCurrentObsTime := FCurrentObs.FObsTimes[FCurrentTimeIndex];
  FCurrentObsTime.Name := OBSNAM;
end;

procedure THobImporter.ReadDataSet5;
begin
  Read(FImporter.FFile, FCurrentObs.ITT);
  ReadLn(FImporter.FFile);
end;

procedure THobImporter.ReadDataSet4;
var
  LayerIndex: Integer;
  ML: TMultLayerFactor;
begin
  Assert(FCurrentObs.FMultiLayers.ArrayLength > 0);
  for LayerIndex := 0 to FCurrentObs.FMultiLayers.ArrayLength - 1 do
  begin
    ML := FCurrentObs.FMultiLayers[LayerIndex];
    Read(FImporter.FFile, ML.Layer);
    Read(FImporter.FFile, ML.Proportion);
  end;
  ReadLn(FImporter.FFile);
end;

procedure THobImporter.ReadDataSet3Observation;
begin
  Read(FImporter.FFile, FCurrentObs.LAYER);
  Read(FImporter.FFile, FCurrentObs.ROW);
  Read(FImporter.FFile, FCurrentObs.COLUMN);
  Read(FImporter.FFile, FCurrentObsTime.RefStressPeriod);
  Read(FImporter.FFile, FCurrentObsTime.TimeOffset);
  Read(FImporter.FFile, FCurrentObs.RowOffset);
  Read(FImporter.FFile, FCurrentObs.ColumnOffset);
  Read(FImporter.FFile, FCurrentObsTime.HeadObservation);
  ReadLn(FImporter.FFile);
  if FCurrentObs.LAYER < 0 then
  begin
    FCurrentObs.FMultiLayers.ArrayLength := -FCurrentObs.LAYER;
  end;
  if FCurrentObsTime.RefStressPeriod < 0 then
  begin
    FCurrentObs.FObsTimes.ArrayLength := - FCurrentObsTime.RefStressPeriod
  end;
  FCurrentObsTime.TimeOffset := FCurrentObsTime.TimeOffset*TOMULTH;
end;

procedure THobImporter.ReadDataSet3ObsName;
begin
  Inc(FCurrentObsIndex);
  FCurrentObs := FHeadObservations[FCurrentObsIndex];
  Readln(FImporter.FFile, FCurrentObs.OBSNAM);
  FCurrentObs.FObsTimes.ArrayLength := 1;
  FCurrentObsTime := FCurrentObs.FObsTimes[0];
  FCurrentObsTime.Name := FCurrentObs.OBSNAM;
  FCurrentTimeIndex := -1;
end;

procedure THobImporter.ReadDataSet2;
begin
  Read(FImporter.FFile, TOMULTH);
  ReadLn(FImporter.FFile);
end;

procedure THobImporter.ReadDataSet1;
var
  NH: integer;
  MOBS: integer;
  MAXM: integer;
  IUHOBSV: integer;
begin
  Read(FImporter.FFile, NH);
  Read(FImporter.FFile, MOBS);
  Read(FImporter.FFile, MAXM);
  Read(FImporter.FFile, IUHOBSV);
  Read(FImporter.FFile, HOBDRY);
  ReadLn(FImporter.FFile);
  FHeadObservations.ArrayLength := NH
end;

procedure THobImporter.ReadData(const ALabel: string);
begin
  inherited;
  if ALabel = 'NH, MOBS, MAXM, IUHOBSV, HOBDRY:' then
  begin
    ReadDataSet1;
  end
  else if ALabel = 'TOMULTH:' then
  begin
    ReadDataSet2;
  end
  else if ALabel = 'OBSNAM_D3:' then
  begin
    ReadDataSet3ObsName;
  end
  else if ALabel = 'LAYER ROW COLUMN IREFSP TOFFSET ROFF COFF HOBS:' then
  begin
    ReadDataSet3Observation;
  end
  else if ALabel = '(MLAY(M,ML),PR(M,ML),M=1,NL):' then
  begin
    ReadDataSet4;
  end
  else if ALabel = 'ITT:' then
  begin
    ReadDataSet5;
  end
  else if ALabel = 'OBSNAM_D6:' then
  begin
    ReadDataSet6ObsName;
  end
  else if ALabel = 'IREFSP, TOFFSET, HOBS(N):' then
  begin
    ReadDataSet6Values;
  end
end;

{ TMultiLayers }

function TMultiLayerArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TMultLayerFactor;
end;

function TMultiLayerArray.GetMultLayerFactor(Index: integer): TMultLayerFactor;
begin
  result := TMultLayerFactor(Objects[Index]);
end;

{ TMultiTimeObsArray }

function TMultiTimeObsArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TMultiTimeObs;
end;

function TMultiTimeObsArray.GetMultiTimeObs(Index: integer): TMultiTimeObs;
begin
  result := TMultiTimeObs(Objects[Index]);
end;

{ THeadObsevationArray }

function THeadObsevationArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := THeadObservation;
end;

function THeadObsevationArray.GetHeadObservation(
  Index: integer): THeadObservation;
begin
  result := THeadObservation(Objects[Index]);
end;

{ THeadObservation }

constructor THeadObservation.Create;
begin
  inherited;
  FMultiLayers:= TMultiLayerArray.Create;
  FObsTimes:= TMultiTimeObsArray.Create;
end;

destructor THeadObservation.Destroy;
begin
  FObsTimes.Free;
  FMultiLayers.Free;
  inherited;
end;

{ THfbImporter }

constructor THfbImporter.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'HFB');
  FParameters := THfbParamArray.Create;
  FStressPeriods := THfbStressPeriodArray.Create;
  FCurrentLocation := -1;
end;

destructor THfbImporter.Destroy;
begin
  FStressPeriods.Free;
  FParameters.Free;
  inherited;
end;

function THfbImporter.GetBoundary(
  ScreenObject: TScreenObject): TModflowParamBoundary;
begin
  result := nil;
  Assert(False);
end;

procedure THfbImporter.DefineBarrier(Location: THfbLocation;
  Const ParamName: string; var Count: integer);
var
  ScreenObject: TScreenObject;
  Boundary: THfbBoundary;
  UndoCreateScreenObject: TCustomUndo;
  Grid: TCustomGrid;
  Layer: Integer;
  APoint: T3DRealPoint;
  Point1: TPoint2D;
  Point2: TPoint2D;
  Column: Integer;
  Row: Integer;
begin
  UndoCreateScreenObject := nil;
  Inc(Count);

  ScreenObject := TScreenObject.CreateWithViewDirection(FModel, vdTop,
    UndoCreateScreenObject, False);
  ScreenObject.Name := ScreenObjectNameRoot + '_'
    + IntToStr(Count);
  FModel.AddScreenObject(ScreenObject);
  ScreenObject.ElevationCount := ecOne;
  ScreenObject.SetValuesOfIntersectedCells := True;
  ScreenObject.EvaluatedAt := eaBlocks;
  ScreenObject.Visible := False;
  ScreenObject.Capacity := 2;

  Layer := FModel.LayerStructure.ModflowLayerToDataSetLayer(Location.Layer);
  Grid := FModel.Grid;
  APoint := Grid.ThreeDElementCenter(Location.Column-1, Location.Row-1, Layer);
  ScreenObject.ElevationFormula := FloatToStr(APoint.Z);

  Column := Max(Location.Column, Location.Col2)-1;
  Row := Max(Location.Row, Location.Row2)-1;

  Point1 := Grid.TwoDElementCorner(Column, Row);
  if Location.Column = Location.Col2 then
  begin
    Inc(Column);
  end
  else
  begin
    Inc(Row);
  end;
  Point2 := Grid.TwoDElementCorner(Column, Row);

  ScreenObject.AddPoint(Point1, True);
  ScreenObject.AddPoint(Point2, False);

  ScreenObject.CreateHfbBoundary;
  Boundary := ScreenObject.ModflowHfbBoundary;
  Boundary.IsUsed := True;
  Boundary.HydraulicConductivityFormula := FloatToStr(Location.Factor);
  Boundary.ThicknessFormula := '1';
  Boundary.AdjustmentMethod := amNone;
  Boundary.ParameterName := ParamName;
end;

procedure THfbImporter.HandlePackage;
var
  ParamIndex: Integer;
  ParamItem: THfbParam;
  UsedParameters: TStringList;
  Param: TModflowSteadyParameter;
  LocationIndex: Integer;
  Locations: THfbLocations;
  Location: THfbLocation;
  SingleStressPeriod: THfbStressPeriod;
  Count: integer;
begin
  inherited;
  FHfbPackage := FModel.ModflowPackages.HfbPackage;
  FHfbPackage.IsSelected := True;
  FHfbPackage.Comments := FComments;
  Count := 0;
  UsedParameters := TStringList.Create;
  try
    for ParamIndex := 0 to Length(ParameterNames) - 1 do
    begin
      UsedParameters.Add(UpperCase(ParameterNames[ParamIndex]));
    end;
    for ParamIndex := 0 to FParameters.ArrayLength - 1 do
    begin
      ParamItem := FParameters[ParamIndex] as THfbParam;
      if UsedParameters.IndexOf(UpperCase(ParamItem.PARNAM)) >= 0 then
      begin
        Param := FModel.ModflowSteadyParameters.Add as TModflowSteadyParameter;
        Param.ParameterName := ParamItem.PARNAM;
        Param.ParameterType := ptHFB;
        Param.Value := ParamItem.Parval;

        Locations := ParamItem.Instances[0] as THfbLocations;
        for LocationIndex := 0 to Locations.ArrayLength - 1 do
        begin
          Location := Locations[LocationIndex];
          DefineBarrier(Location, ParamItem.PARNAM, Count);
        end;
      end;
    end;
  finally
    UsedParameters.Free;
  end;
  SingleStressPeriod := FStressPeriods.StressPeriods[0] as THfbStressPeriod;
  Locations := SingleStressPeriod[0] as THfbLocations;
  for LocationIndex := 0 to Locations.ArrayLength - 1 do
  begin
    Location := Locations[LocationIndex];
    DefineBarrier(Location, '', Count);
  end;
  ReleaseMemory;
end;

procedure THfbImporter.ReadData(const ALabel: string);
var
  NPHFB: integer;
  MXFBP: integer;
  SingleStressPeriod: THfbStressPeriod;
  Barrier: THfbLocation;
  ParameterName: string;
begin
  inherited;
  if ALabel = 'NPHFB,MXFBP,NHFBNP:' then
  begin
    Read(FImporter.FFile, NPHFB);
    Read(FImporter.FFile, MXFBP);
    Read(FImporter.FFile, NHFBNP);
    ReadLn(FImporter.FFile);
    FStressPeriods.ArrayLength := 1;
    FParameters.ArrayLength := NPHFB;
  end
  else if ALabel = 'NOPRINT:' then
  begin
    NoPrintOption := True;
  end
  else if ALabel = 'PARNAM:' then
  begin
    ReadParameterName;
    FParam := FParameters[CurrentParameter] as THfbParam;
    FLocations := FParam[0] as THfbLocations;
    FCurrentLocation := -1;
  end
  else if ALabel = 'PARTYP:' then
  begin
    ReadParameterType;
  end
  else if ALabel = 'Parval,NLST:' then
  begin
    ReadParameterValueAndLocationCount;
  end
  else if ALabel = 'NACTHFB:' then
  begin
    Read(FImporter.FFile, NACTHFB);
    ReadLn(FImporter.FFile);
    SetLength(ParameterNames, NACTHFB);
    CurrentParameter := 0;
  end
  else if ALabel = 'BARRIERS NOT DEFINED BY PARAMETERS:' then
  begin
    SingleStressPeriod := FStressPeriods.StressPeriods[0] as THfbStressPeriod;
    SingleStressPeriod.ArrayLength := 1;
    FLocations := SingleStressPeriod[0] as THfbLocations;
    FLocations.ArrayLength := NHFBNP;
    FCurrentLocation := -1;
  end
  else if ALabel = 'Layer IROW1 ICOL1 IROW2  ICOL2 Factor:' then
  begin
    Inc(FCurrentLocation);
    Barrier := FLocations[FCurrentLocation];
    Read(FImporter.FFile, Barrier.Layer);
    Read(FImporter.FFile, Barrier.Row);
    Read(FImporter.FFile, Barrier.Column);
    Read(FImporter.FFile, Barrier.Row2);
    Read(FImporter.FFile, Barrier.Col2);
    Read(FImporter.FFile, Barrier.Factor);
    ReadLn(FImporter.FFile);
  end
  else if ALabel = 'Pname:' then
  begin
    Readln(FImporter.FFile, ParameterName);
    ParameterName := Trim(ParameterName);
    if Length(ParameterName) > 10 then
    begin
      SetLength(ParameterName, 10);
    end;
    ParameterNames[CurrentParameter] := ParameterName;
    Inc(CurrentParameter);
  end
  else
  begin
    Assert(False);
  end;
end;

function THfbImporter.ParameterType: TParameterType;
begin
  result := ptUndefined;
  Assert(False);
end;

procedure THfbImporter.ReadParameterType;
var
  PARTYP: string;
begin
  Readln(FImporter.FFile, PARTYP);
  PARTYP := Trim(PARTYP);
  PARTYP := UpperCase(PARTYP);
  Assert(PARTYP = 'HFB');
  FParameters[CurrentParameter].PARTYP := PARTYP;

end;

function THfbImporter.ScreenObjectNameRoot: string;
begin
  result := 'Imported_Hfb_';
end;

procedure THfbImporter.SetItemValues(Item: TCustomModflowBoundaryItem;
  Boundaries: TList; EndTime, StartTime: Double; ScreenObject: TScreenObject;
  const ParamName: string);
begin
  Assert(False);
end;

{ THfbLocations }

function THfbLocations.ArrayMemberClass: TArrayMemberClass;
begin
  result := THfbLocation;
end;

function THfbLocations.GetBarriers(Index: integer): THfbLocation;
begin
  result := Objects[Index] as THfbLocation
end;

{ THfbParam }

function THfbParam.ArrayMemberClass: TArrayMemberClass;
begin
  result := THfbLocations;
end;

{ THfbParamArray }

function THfbParamArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := THfbParam;
end;

{ THfbStressPeriod }

function THfbStressPeriod.ArrayMemberClass: TArrayMemberClass;
begin
  result := THfbLocations;
end;

function THfbStressPeriod.GetBarrierList(Index: integer): THfbLocations;
begin
  result := Objects[Index] as THfbLocations
end;

{ THfbStressPeriodArray }

function THfbStressPeriodArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := THfbStressPeriod;
end;

{ TLakeImporter }

constructor TLakImporter.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'LAK', nil);
  FLakPackage := FModel.ModflowPackages.LakPackage;
  THETA := FLakPackage.Theta;
  NSSITR := FLakPackage.NumberOfIterations;
  SSCNCR := FLakPackage.ConvergenceCriterion;
  FCurrentStressPeriod := -1;
  FLakeStressPeriodValues := TLakeStressPeriodValues.Create;
  FPriorStressPeriodValues := nil;
end;

procedure TLakImporter.DefineLakeOutlines;
var
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  CompleteLake: Boolean;
  Layer: Integer;
begin
  // define the lakes.
  // first check the rather unlikely possibility that the
  // entire top surface is a single lake.
  CompleteLake := True;
  for LayerIndex := 0 to Length(FConstantLkarr) - 1 do
  begin
    if FConstantLkarr[LayerIndex].IsConstant and
      (FConstantLkarr[LayerIndex].IntegerValue <> 0) then
    begin
      Layer := FModel.LayerStructure.
        ModflowLayerToDataSetLayer(LayerIndex+1);

      for RowIndex := 0 to Length(FLakeOutline) - 1 do
      begin
        for ColIndex := 0 to Length(FLakeOutline[RowIndex]) - 1 do
        begin
          FLakeOutline[RowIndex, ColIndex] :=
            FConstantLkarr[LayerIndex].IntegerValue;
          FLakeBottom[RowIndex, ColIndex] :=
            FModel.ModflowGrid.CellElevation[ColIndex,RowIndex,Layer+1];
        end;
      end;

    end
    else
    begin
      CompleteLake := False;
      break;
    end;
  end;
  if not CompleteLake then
  begin
    for RowIndex := 0 to Length(FLakeOutline) - 1 do
    begin
      for ColIndex := 0 to Length(FLakeOutline[RowIndex]) - 1 do
      begin
        FLakeOutline[RowIndex, ColIndex] := 0;
        for LayerIndex := 0 to Length(LKARR) - 1 do
        begin
          Layer := FModel.LayerStructure.
            ModflowLayerToDataSetLayer(LayerIndex+1);
          if LKARR[LayerIndex, RowIndex, ColIndex] <> 0 then
          begin
            FLakeOutline[RowIndex, ColIndex] :=
              LKARR[LayerIndex, RowIndex, ColIndex];
            FLakeBottom[RowIndex, ColIndex] :=
              FModel.ModflowGrid.CellElevation[ColIndex,RowIndex,Layer+1];
          end;
        end;
      end;
    end;
  end;
end;

destructor TLakImporter.Destroy;
begin
  FLakeStressPeriodValues.Free;
  inherited;
end;

procedure TLakImporter.CreateBoundary(ScreenObject: TScreenObject);
begin
  ScreenObject.CreateLakBoundary;
end;

procedure TLakImporter.HandlePackage;
var
  LakeIndex: Integer;
  Cluster: TClusterObject;
  ScreenObject: TScreenObject;
  LakeBoundary: TLakBoundary;
  LakeID: Integer;
  LakeSystemIndex: Integer;
  SubLakeIndex: Integer;
  Lakes: TList;
  LayerIndex: Integer;
  Formula: string;
  FirstValue: Double;
  IsConstant: Boolean;
  Values: TStringList;
  Index: Integer;
  Layer: Integer;
  NewName: string;
  DataArray: TDataArray;
  TimeIndex: Integer;
  ValuesArray: TLakeValueArray;
  LakeValues: TLakeValues;
  LakeItem: TLakItem;
  StressPeriod: TModflowStressPeriod;
  ConstantLakeBottom: Boolean;
  RowIndex: Integer;
  ColIndex: Integer;
  BottomFormula: string;
begin
  if FCurrentStressPeriod < FModel.ModflowStressPeriods.Count-1 then
  begin
    Exit;
  end;
  inherited;
  FLakPackage := FModel.ModflowPackages.LakPackage;
  FLakPackage.IsSelected := True;
  FLakPackage.Comments := FComments;
  FLakPackage.Theta := THETA;
  FLakPackage.NumberOfIterations := NSSITR;
  FLakPackage.ConvergenceCriterion := SSCNCR;
  FLakPackage.SurfDepth.Value := SURFDEPTH;

  FModel.CreateInitialDataSets;

  DefineLakeOutlines;

  ConstantLakeBottom := True;
  FirstValue := FLakeBottom[0,0];
  for RowIndex := 0 to Length(FLakeBottom) - 1 do
  begin
    for ColIndex := 0 to Length(FLakeBottom[0]) - 1 do
    begin
      ConstantLakeBottom := FirstValue = FLakeBottom[RowIndex,ColIndex];
      if not ConstantLakeBottom then
      begin
        break;
      end;
    end;
    if not ConstantLakeBottom then
    begin
      break;
    end;
  end;
  if ConstantLakeBottom then
  begin
    BottomFormula := FloatToStr(FirstValue);
  end
  else
  begin
    BottomFormula := StrImportedLakeBottom;
    CreateDataSet(-1, StrImportedLakeBottom, rdtDouble, DataArray);
    CreateOrRetrieveCellCenterScreenObject(ScreenObject);
    AssignRealValuesToCellCenters(DataArray, ScreenObject, FLakeBottom);
  end;


  Lakes := TList.Create;
  try
    Cluster := TClusterObject.Create;
    try
      SetLength(Cluster.Zones, 1);
      for LakeIndex := 0 to NLAKES-1 do
      begin
        LakeID := LakeIndex+1;
        Cluster.Zones[0] := LakeID;
        ScreenObject := CreateScreenObjectAroundZones(
          FLakeOutline, Cluster, 'ImportedLake_'
          + IntToStr(LakeID));
        ScreenObject.ElevationCount := ecTwo;
        ScreenObject.HigherElevationFormula := StrModelTop;
        ScreenObject.LowerElevationFormula := BottomFormula;

        CreateBoundary(ScreenObject);
        ScreenObject.ModflowLakBoundary;
        LakeBoundary := ScreenObject.ModflowLakBoundary;
        LakeBoundary.LakeID := LakeID;
        LakeBoundary.InitialStage := Stages[LakeIndex];
        LakeBoundary.CenterLake := 0;
        Lakes.Add(LakeBoundary);

        LakeItem := nil;
        for TimeIndex := 0 to FLakeStressPeriodValues.ArrayLength - 1 do
        begin
          StressPeriod := FModel.ModflowStressPeriods[TimeIndex];
          ValuesArray := FLakeStressPeriodValues[TimeIndex];
          if ValuesArray.Reuse then
          begin
            Assert(LakeItem <> nil);
            LakeItem.EndTime := StressPeriod.EndTime;
          end
          else
          begin
            LakeValues := ValuesArray[LakeIndex];
            LakeItem := LakeBoundary.Values.Add as TLakItem;
            LakeItem.StartTime := StressPeriod.StartTime;
            LakeItem.EndTime := StressPeriod.EndTime;
            LakeItem.MinimumStage := FloatToStr(LakeValues.SSMN);
            LakeItem.MaximumStage := FloatToStr(LakeValues.SSMX);
            LakeItem.Precipitation := FloatToStr(LakeValues.PRCPLK);
            LakeItem.Evaporation := FloatToStr(LakeValues.EVAPLK);
            LakeItem.OverlandRunoff := FloatToStr(LakeValues.RNF);
            LakeItem.Withdrawal := FloatToStr(LakeValues.WTHDRW);
          end;
        end;
      end;
    finally
      Cluster.Free;
    end;
    for LakeSystemIndex := 0 to Length(SubLakes) - 1 do
    begin
      for SubLakeIndex := 1 to Length(SubLakes[LakeSystemIndex]) - 1 do
      begin
        LakeIndex := SubLakes[LakeSystemIndex,SubLakeIndex]-1;
        LakeBoundary := Lakes[LakeIndex];
        LakeBoundary.CenterLake := SubLakes[LakeSystemIndex,0];
        LakeBoundary.Sill := Sills[LakeSystemIndex,SubLakeIndex-1];
      end;
    end;
  finally
    Lakes.Free;
  end;

  IsConstant := True;
  for LayerIndex := 0 to Length(FConstantBdlknc) - 1 do
  begin
    if FConstantBdlknc[LayerIndex].IsConstant then
    begin
      if LayerIndex = 0 then
      begin
        FirstValue := FConstantBdlknc[LayerIndex].RealValue;
      end
      else if FConstantBdlknc[LayerIndex].RealValue <> FirstValue then
      begin
        IsConstant := False;
        break;
      end;
    end
    else
    begin
      IsConstant := False;
      break;
    end;
  end;
  ScreenObject := nil;
  if IsConstant then
  begin
    Formula := FloatToStr(FirstValue);
  end
  else
  begin
    Values := TStringList.Create;
    try
      Values.Capacity := FModel.Grid.LayerCount;
      for Index := 0 to Values.Capacity - 1 do
      begin
        Values.Add('0');
      end;

      for LayerIndex := 0 to Length(FConstantBdlknc) - 1 do
      begin
        Layer :=FModel.LayerStructure.
          ModflowLayerToDataSetLayer(LayerIndex + 1);
        if FConstantBdlknc[LayerIndex].IsConstant then
        begin
          Values[Layer] := FloatToStr(FConstantBdlknc[LayerIndex].RealValue);
        end
        else
        begin
          NewName := 'Imported_LakeLeakance_' + IntToStr(LayerIndex+1);
          CreateDataSet(-1, NewName, rdtDouble, DataArray);
          if ScreenObject = nil then
          begin
            CreateOrRetrieveCellCenterScreenObject(ScreenObject);
          end;
          AssignRealValuesToCellCenters(DataArray, ScreenObject,
            BDLKNC[LayerIndex]);
          Values[Layer] := DataArray.Name;
        end;
      end;
      Values.Delimiter := ',';
      Formula := 'CaseR(Layer, ' + Values.DelimitedText + ')';
    finally
      Values.Free;
    end;
  end;

  DataArray := FModel.GetDataSetByName(rsLakeLeakance);
  DataArray.Formula := Formula;

end;

procedure TLakImporter.ReadDataSet1;
var
  ILKCB: Integer;
  NLAY: Integer;
  NROW: Integer;
  NCOL: Integer;
  Index: Integer;
begin
  Read(FImporter.FFile, NLAKES);
  Read(FImporter.FFile, ILKCB);
  ReadLn(FImporter.FFile);
  SetLength(Stages, NLAKES);
  SetLength(MinStages, NLAKES);
  SetLength(MaxStages, NLAKES);

  NLAY := FModel.LayerStructure.ModflowLayerCount;
  NROW := FGrid.RowCount;
  NCOL := FGrid.ColumnCount;

  SetLength(FLakeOutline, NROW, NCOL);
  SetLength(FLakeBottom, NROW, NCOL);


  SetLength(LKARR, NLAY, NROW, NCOL);
  SetLength(FConstantLkarr, NLAY);
  for Index := 0 to NLAY - 1 do
  begin
    FConstantLkarr[Index].IsConstant := False;
  end;

  SetLength(BDLKNC, NLAY, NROW, NCOL);
  SetLength(FConstantBdlknc, NLAY);
  for Index := 0 to NLAY - 1 do
  begin
    FConstantBdlknc[Index].IsConstant := False;
  end;

  FLakeStressPeriodValues.ArrayLength := FModel.ModflowStressPeriods.Count;
end;

procedure TLakImporter.ReadTheta;
begin
  Read(FImporter.FFile, THETA);
  ReadLn(FImporter.FFile);
end;

procedure TLakImporter.ReadTransientControls;
begin
  Read(FImporter.FFile, NSSITR);
  Read(FImporter.FFile, SSCNCR);
  Read(FImporter.FFile, SURFDEPTH);
  ReadLn(FImporter.FFile);
end;

procedure TLakImporter.ReadLakeStageAndLimits;
var
  LM: Integer;
begin
  Read(FImporter.FFile, LM);
  Read(FImporter.FFile, Stages[LM - 1]);
  Read(FImporter.FFile, MinStages[LM - 1]);
  Read(FImporter.FFile, MaxStages[LM - 1]);
  ReadLn(FImporter.FFile);
end;

procedure TLakImporter.ReadLakeStage;
var
  LM: Integer;
begin
  Read(FImporter.FFile, LM);
  Read(FImporter.FFile, Stages[LM - 1]);
  // supply default values for min and max stage.
  MinStages[LM - 1] := Stages[LM - 1] - 1;
  MaxStages[LM - 1] := Stages[LM - 1] + 1;
  ReadLn(FImporter.FFile);
end;

procedure TLakImporter.ReadNumberOfSolutes;
begin
  Read(FImporter.FFile, NSOL);
  ReadLn(FImporter.FFile);
end;

procedure TLakImporter.ReadLakeSolutes;
var
  CLAKE: Double;
  Index: Integer;
  LM: Integer;
begin
  Read(FImporter.FFile, LM);
  for Index := 0 to LM - 1 do
  begin
    Read(FImporter.FFile, CLAKE);
  end;
  ReadLn(FImporter.FFile);
end;

procedure TLakImporter.ReadDataSet4;
var
  ITMP: Integer;
  ITMP1: Integer;
  LWRT: Integer;
  StressPeriodValues: TLakeValueArray;
begin
  Read(FImporter.FFile, ITMP);
  Read(FImporter.FFile, ITMP1);
  Read(FImporter.FFile, LWRT);
  ReadLn(FImporter.FFile);
  Inc(FCurrentStressPeriod);
  FCurrentLakeDefinition := -1;
  FCurrentLakeValues := -1;
  StressPeriodValues := FLakeStressPeriodValues[FCurrentStressPeriod];
  StressPeriodValues.Reuse := ITMP1 < 0;
  if not StressPeriodValues.Reuse then
  begin
    StressPeriodValues.ArrayLength := NLAKES;
    FPriorStressPeriodValues := FCurrentStressPeriodValues;
  end;
  FCurrentStressPeriodValues := StressPeriodValues;
end;

procedure TLakImporter.ReadVariableLakeID;
var
  IntArray: T2DIntArray;
  Layer: Integer;
  ID: string;
begin
  ReadLn(FImporter.FFile, ID);
  Assert(Trim(ID) = StrLAKEIDARRAY);
  ReadLn(FImporter.FFile, Layer);
  Dec(Layer);
  IntArray := LKARR[Layer];
  ReadVariable2DIntArray(IntArray);
end;

procedure TLakImporter.ReadConstantLakeID;
var
  ColIndex: Integer;
  RowIndex: Integer;
  IntegerConstant: Integer;
  IntArray: T2DIntArray;
  Layer: Integer;
  ID: string;
begin
  ReadLn(FImporter.FFile, ID);
  Assert(Trim(ID) = StrLAKEIDARRAY);
  ReadLn(FImporter.FFile, Layer);
  Dec(Layer);
  ReadLn(FImporter.FFile, IntegerConstant);
  IntArray := LKARR[Layer];
  for RowIndex := 0 to Length(IntArray) - 1 do
  begin
    for ColIndex := 0 to Length(IntArray[RowIndex]) - 1 do
    begin
      IntArray[RowIndex, ColIndex] := IntegerConstant;
    end;
  end;
  FConstantLkarr[Layer].IsConstant := True;
  FConstantLkarr[Layer].IntegerValue := IntegerConstant;
end;

procedure TLakImporter.ReadVariableLakebedLeakance;
var
  Layer: Integer;
  ID: string;
  RealArray: T2DDoubleArray;
begin
  ReadLn(FImporter.FFile, ID);
  Assert(Trim(ID) = StrLAKEBEDLEAKANCEARR);
  ReadLn(FImporter.FFile, Layer);
  Dec(Layer);
  RealArray := BDLKNC[Layer];
  Read2DRealArray(RealArray);
end;

procedure TLakImporter.ReadConstantLakebedLeakance;
var
  Value: Double;
  ID: string;
  RealArray: T2DDoubleArray;
  Layer: Integer;
begin
  ReadLn(FImporter.FFile, ID);
  Assert(Trim(ID) = StrLAKEBEDLEAKANCEARR);
  ReadLn(FImporter.FFile, Layer);
  Dec(Layer);
  Readln(FImporter.FFile, Value);
  RealArray := BDLKNC[Layer];
  AssignConstant2DArray(Value, RealArray);
  FConstantBdlknc[Layer].IsConstant := True;
  FConstantBdlknc[Layer].RealValue := Value;
end;

procedure TLakImporter.ReadNumberOfSublakes;
begin
  Read(FImporter.FFile, NSLMS);
  ReadLn(FImporter.FFile);
  if FCurrentStressPeriod > 0 then
  begin
    Exit;
  end;
  SetLength(SubLakes, NSLMS);
  SetLength(Sills, NSLMS);
end;

procedure TLakImporter.ReadSublakes;
var
  ISUB: Integer;
  Index: Integer;
begin
  Inc(FCurrentLakeDefinition);
  Read(FImporter.FFile, IC);
  if FCurrentStressPeriod = 0 then
  begin
    SetLength(SubLakes[FCurrentLakeDefinition],IC);
  end;
  for Index := 0 to IC - 1 do
  begin
    Read(FImporter.FFile, ISUB);
    if FCurrentStressPeriod = 0 then
    begin
      SubLakes[FCurrentLakeDefinition, Index] := ISUB;
    end;
  end;
  ReadLn(FImporter.FFile);
end;

procedure TLakImporter.ReadSills;
var
  IC1: Integer;
  SILLVT: Double;
  Index: Integer;
begin
  IC1 := IC - 1;
  if FCurrentStressPeriod = 0 then
  begin
    SetLength(Sills[FCurrentLakeDefinition],IC1);
  end;
  for Index := 0 to IC1 - 1 do
  begin
    Read(FImporter.FFile, SILLVT);
    if FCurrentStressPeriod = 0 then
    begin
      Sills[FCurrentLakeDefinition, Index] := SILLVT;
    end;
  end;
  ReadLn(FImporter.FFile);
end;

procedure TLakImporter.ReadDataSet9aWithLimits;
var
  CurrentValues: TLakeValues;
begin
  Inc(FCurrentLakeValues);
  CurrentValues := FCurrentStressPeriodValues[FCurrentLakeValues];

  Read(FImporter.FFile, CurrentValues.PRCPLK);
  Read(FImporter.FFile, CurrentValues.EVAPLK);
  Read(FImporter.FFile, CurrentValues.RNF);
  Read(FImporter.FFile, CurrentValues.WTHDRW);
  Read(FImporter.FFile, CurrentValues.SSMN);
  Read(FImporter.FFile, CurrentValues.SSMX);
  ReadLn(FImporter.FFile);
  Inc(FCurrentLakeValues);
end;

procedure TLakImporter.ReadDataSet9aNoLimits;
var
  CurrentValues: TLakeValues;
  PriorValues: TLakeValues;
begin
  Inc(FCurrentLakeValues);
  CurrentValues := FCurrentStressPeriodValues[FCurrentLakeValues];

  Read(FImporter.FFile, CurrentValues.PRCPLK);
  Read(FImporter.FFile, CurrentValues.EVAPLK);
  Read(FImporter.FFile, CurrentValues.RNF);
  Read(FImporter.FFile, CurrentValues.WTHDRW);

  // use previous values of SSMN and SSMX.
  if FPriorStressPeriodValues = nil then
  begin
    CurrentValues.SSMN := MinStages[FCurrentLakeValues];
    CurrentValues.SSMX := MaxStages[FCurrentLakeValues];
  end
  else
  begin
    PriorValues := FPriorStressPeriodValues[FCurrentLakeValues];
    CurrentValues.SSMN := PriorValues.SSMN;
    CurrentValues.SSMX := PriorValues.SSMX;
  end;

  ReadLn(FImporter.FFile);

end;

procedure TLakImporter.ReadDataSet9bWithAugmentation;
var
  CAUG: Double;
  CRNF: Double;
  CPPT: Double;
begin
  Read(FImporter.FFile, CPPT);
  Read(FImporter.FFile, CRNF);
  Read(FImporter.FFile, CAUG);
  ReadLn(FImporter.FFile);
end;

procedure TLakImporter.ReadDataSet9bNoAugmentation;
var
  CRNF: Double;
  CPPT: Double;
begin
  Read(FImporter.FFile, CPPT);
  Read(FImporter.FFile, CRNF);
  ReadLn(FImporter.FFile);
end;

procedure TLakImporter.ReadData(const ALabel: string);
begin
  inherited;
  if ALabel = 'NLAKES,ILKCB:' then
  begin
    ReadDataSet1;
  end
  else if ALabel = 'THETA:' then
  begin
    ReadTheta;
  end
  else if ALabel = 'NSSITR,SSCNCR,SURFDEPTH:' then
  begin
    ReadTransientControls;
  end
  else if ALabel = 'LM,STAGES(LM),SSMN(LM),SSMX(LM):' then
  begin
    ReadLakeStageAndLimits;
  end
  else if ALabel = 'LM,STAGES(LM):' then
  begin
    ReadLakeStage;
  end
  else if ALabel = 'NSOL:' then
  begin
    ReadNumberOfSolutes;
  end
  else if ALabel = 'LM,(CLAKE(LM,ISOL),ISOL=1,NSOL):' then
  begin
    ReadLakeSolutes;
  end
  else if ALabel = 'ITMP, ITMP1, LWRT:' then
  begin
    ReadDataSet4;
  end
  else if ALabel = StrVariable2DIntegerArrayForLayer then
  begin
    ReadVariableLakeID;
  end
  else if ALabel = StrConstant2DIntegerArrayForLayer then
  begin
    ReadConstantLakeID;
  end
  else if ALabel = StrVariable2DRealArrayForLayer then
  begin
    ReadVariableLakebedLeakance;
  end
  else if ALabel = StrConstant2DRealArrayForLayer then
  begin
    ReadConstantLakebedLeakance;
  end
  else if ALabel = 'NSLMS:' then
  begin
    ReadNumberOfSublakes;
  end
  else if ALabel = 'IC,(ISUB(IS,I),I=1,IC):' then
  begin
    ReadSublakes;
  end
  else if ALabel = '(SILLVT(IS,I),I=1,IC1):' then
  begin
    ReadSills;
  end
  else if ALabel =
    'PRCPLK(LM),EVAPLK(LM),RNF(LM),WTHDRW(LM),SSMN(LM),SSMX(LM):' then
  begin
    ReadDataSet9aWithLimits;
  end
  else if ALabel = 'PRCPLK(LM),EVAPLK(LM),RNF(LM),WTHDRW(LM):' then
  begin
    ReadDataSet9aNoLimits;
  end
  else if ALabel = 'CPPT(LM,ISOL),CRNF(LM,ISOL),CAUG(LM,ISOL):' then
  begin
    ReadDataSet9bWithAugmentation;
  end
  else if ALabel = 'CPPT(LM,ISOL),CRNF(LM,ISOL):' then
  begin
    ReadDataSet9bNoAugmentation;
  end
  else
  begin
    Assert(False);
  end;
end;

{ TLakeValueArray }

function TLakeValueArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TLakeValues;
end;

function TLakeValueArray.GetLakeValues(Index: integer): TLakeValues;
begin
  result := Objects[Index] as TLakeValues;
end;

{ TLakeStressPeriodValues }

function TLakeStressPeriodValues.ArrayMemberClass: TArrayMemberClass;
begin
  result := TLakeValueArray;
end;

function TLakeStressPeriodValues.GetLakeValueArray(
  Index: integer): TLakeValueArray;
begin
  result := Objects[Index] as TLakeValueArray;
end;

{ TDrtInstanceObject }

function TDrtInstanceObject.ArrayMemberClass: TArrayMemberClass;
begin
  result := TDrtLocationObject;
end;

{ TDrtParameterObject }

function TDrtParameterObject.ArrayMemberClass: TArrayMemberClass;
begin
  result := TDrtInstanceObject;
end;

{ TDrtParamArray }

function TDrtParamArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TDrtParameterObject;
end;

{ TDrtStressPeriod }

function TDrtStressPeriod.ArrayMemberClass: TArrayMemberClass;
begin
  result := TDrtLocationObject;
end;

{ TDrtStressPeriodArray }

function TDrtStressPeriodArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TDrtStressPeriod;
end;

{ TDrtImporter }

constructor TDrtImporter.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'DRT:');
  FStressPeriods:= TDrtStressPeriodArray.Create;
  FParameters:= TDrtParamArray.Create;
end;

function TDrtImporter.CreateScreenObject(List: TList;
  var ScreenObjectIndex: Integer; LayerIndex,
  StressPeriodIndex: integer): TScreenObject;
begin
  result := inherited CreateScreenObject(List, ScreenObjectIndex,
    LayerIndex, StressPeriodIndex);
  result.CreateDrtBoundary;
  result.ModflowDrtBoundary.FormulaInterpretation := fiDirect;
end;

destructor TDrtImporter.Destroy;
begin
  FStressPeriods.Free;
  FParameters.Free;
  inherited;
end;

procedure TDrtImporter.ReadAuxilliaryVariables;
var
  AuxVar: Double;
  Index: Integer;
begin
  for Index := 0 to FAuxillaryVariables.Count - 1 do
  begin
    Read(FImporter.FFile, AuxVar);
    FCurrentDrtBoundary.AuxilliaryVariables[Index] := AuxVar;
  end;
  ReadLn(FImporter.FFile);
end;

procedure TDrtImporter.ReadDataSet1DRT;
var
  IDRTCB: Integer;
  MXADRT: Integer;
  MXL: Integer;
begin
  Read(FImporter.FFile, MXADRT);
  Read(FImporter.FFile, IDRTCB);
  Read(FImporter.FFile, NP);
  Read(FImporter.FFile, MXL);
  Readln(FImporter.FFile);
  FParameters.ArrayLength := NP;
end;

procedure TDrtImporter.ReadNonParameterLocationsWithoutReturnFlow;
begin
  FCurrentDrtBoundary := FStressPeriods[FCurrentStressPeriod].
    Boundaries[CurrentInstance] as TDrtLocationObject;
  Read(FImporter.FFile, FCurrentDrtBoundary.Layer);
  Read(FImporter.FFile, FCurrentDrtBoundary.Row);
  Read(FImporter.FFile, FCurrentDrtBoundary.Column);
  Read(FImporter.FFile, FCurrentDrtBoundary.Elevation);
  Read(FImporter.FFile, FCurrentDrtBoundary.Conductance);
  FCurrentDrtBoundary.LayR := 0;
  FCurrentDrtBoundary.RowR := 0;
  FCurrentDrtBoundary.ColR := 0;
  FCurrentDrtBoundary.Rfprop := 0;
  ReadLn(FImporter.FFile);
  Inc(CurrentInstance);
end;

procedure TDrtImporter.ReadNonParameterLocationsWithReturnFlow;
begin
  FCurrentDrtBoundary := FStressPeriods[FCurrentStressPeriod].
    Boundaries[CurrentInstance] as TDrtLocationObject;
  Read(FImporter.FFile, FCurrentDrtBoundary.Layer);
  Read(FImporter.FFile, FCurrentDrtBoundary.Row);
  Read(FImporter.FFile, FCurrentDrtBoundary.Column);
  Read(FImporter.FFile, FCurrentDrtBoundary.Elevation);
  Read(FImporter.FFile, FCurrentDrtBoundary.Conductance);

  Read(FImporter.FFile, FCurrentDrtBoundary.LayR);
  Read(FImporter.FFile, FCurrentDrtBoundary.RowR);
  Read(FImporter.FFile, FCurrentDrtBoundary.ColR);
  Read(FImporter.FFile, FCurrentDrtBoundary.Rfprop);
  ReadLn(FImporter.FFile);
  Inc(CurrentInstance);
end;

procedure TDrtImporter.ReadParameterLocationsWithoutReturnFlow;
begin
  Inc(FBoundaryIndex);
  FCurrentDrtBoundary :=
    FParameters[CurrentParameter].Instances[CurrentInstance].
    Locations[FBoundaryIndex] as TDrtLocationObject;
  Read(FImporter.FFile, FCurrentDrtBoundary.Layer);
  Read(FImporter.FFile, FCurrentDrtBoundary.Row);
  Read(FImporter.FFile, FCurrentDrtBoundary.Column);
  Read(FImporter.FFile, FCurrentDrtBoundary.Elevation);
  Read(FImporter.FFile, FCurrentDrtBoundary.Conductance);
  FCurrentDrtBoundary.LayR := 0;
  FCurrentDrtBoundary.RowR := 0;
  FCurrentDrtBoundary.ColR := 0;
  FCurrentDrtBoundary.Rfprop := 0;
  ReadLn(FImporter.FFile);

  if FBoundaryIndex = NLST - 1 then
  begin
    Inc(CurrentInstance);
    FBoundaryIndex := -1;
  end;
end;

procedure TDrtImporter.ReadParameterLocationsWithReturnFlow;
begin
  Inc(FBoundaryIndex);
  FCurrentDrtBoundary :=
    FParameters[CurrentParameter].Instances[CurrentInstance].
    Locations[FBoundaryIndex] as TDrtLocationObject;
  Read(FImporter.FFile, FCurrentDrtBoundary.Layer);
  Read(FImporter.FFile, FCurrentDrtBoundary.Row);
  Read(FImporter.FFile, FCurrentDrtBoundary.Column);
  Read(FImporter.FFile, FCurrentDrtBoundary.Elevation);
  Read(FImporter.FFile, FCurrentDrtBoundary.Conductance);

  Read(FImporter.FFile, FCurrentDrtBoundary.LayR);
  Read(FImporter.FFile, FCurrentDrtBoundary.RowR);
  Read(FImporter.FFile, FCurrentDrtBoundary.ColR);
  Read(FImporter.FFile, FCurrentDrtBoundary.Rfprop);
  ReadLn(FImporter.FFile);
  if FBoundaryIndex = NLST - 1 then
  begin
    Inc(CurrentInstance);
    FBoundaryIndex := -1;
  end;
end;

procedure TDrtImporter.ReadParameterType;
var
  PARTYP: string;
begin
  Readln(FImporter.FFile, PARTYP);
  PARTYP := Trim(PARTYP);
  PARTYP := UpperCase(PARTYP);
  Assert(PARTYP = 'DRT');
  FParameters[CurrentParameter].PARTYP := PARTYP;
end;

function TDrtImporter.GetBoundary(
  ScreenObject: TScreenObject): TModflowParamBoundary;
begin
  result := ScreenObject.ModflowDrtBoundary;
end;

procedure TDrtImporter.HandlePackage;
var
  ScreenObjectIndex: integer;
  DrtPackage: TModflowPackageSelection;
begin
  if (FCurrentStressPeriod < 0) or
    (FCurrentStressPeriod < FStressPeriods.ArrayLength -1) then
  begin
    Exit;
  end;
  inherited;
  DrtPackage := FModel.ModflowPackages.DrtPackage;
  DrtPackage.IsSelected := True;
  DrtPackage.Comments := FComments;

  ScreenObjectIndex := 0;
  ImportNonParameterBoundaries(ScreenObjectIndex);
  ImportParameterBoundaries(ScreenObjectIndex);
  ReleaseMemory;
end;

procedure TDrtImporter.ReadData(const ALabel: string);
begin
  inherited;
  if FCurrentStressPeriod = -1 then
  begin
    if ALabel = 'MXADRT,IDRTCB,NPDRT,MXL:' then
    begin
      ReadDataSet1DRT;
    end
    else if ALabel = 'DRTAUX(NAUX):' then
    begin
      ReadAuxilliaryVariableName;
    end
    else if ALabel = 'RETURNFLOW:' then
    begin
      ReturnFlow := True;
    end
    else if ALabel = 'NOPRINT:' then
    begin
      FNoPrint := True;
    end
    else if ALabel = 'PARNAM:' then
    begin
      ReadParameterName;
    end
    else if ALabel = 'PARTYP:' then
    begin
      ReadParameterType;
    end
    else if ALabel = 'Parval,NLST:' then
    begin
      ReadParameterValueAndLocationCount;
    end
    else if ALabel = 'INSTANCES:' then
    begin
//       do nothing
    end
    else if ALabel = 'NUMINST:' then
    begin
      ReadNumberOfInstances;
    end
    else if ALabel = 'INAME(ILOC):' then
    begin
      ReadInstanceName;
    end
    else if ALabel = 'Layer Row Column Elevation Cond:' then
    begin
      ReadParameterLocationsWithoutReturnFlow;
    end
    else if ALabel
      = 'Layer Row Column Elevation Cond LayR RowR ColR Rfprop:' then
    begin
      ReadParameterLocationsWithReturnFlow;
    end
    else if ALabel = 'xyz:' then
    begin
      ReadAuxilliaryVariables;
    end
    else if ALabel = 'ITMP,NP:' then
    begin
      ReadFirstStressPeriodDataSet5WithParameters;
    end
    else if ALabel = 'ITMP:' then
    begin
      ReadFirstStressPeriodDataSet5WithoutParameters;
    end
    else
    begin
      Assert(False);
    end;
  end
  else
  begin
    if ALabel = 'ITMP,NP:' then
    begin
      ReadNewStressPeriodDataSet5WithParameters;
    end
    else if ALabel = 'ITMP:' then
    begin
      ReadNewStressPeriodDataSet5WithoutParameters;
    end
    else if ALabel = 'Layer Row Column Elevation Cond:' then
    begin
      ReadNonParameterLocationsWithoutReturnFlow;
    end
    else if ALabel
      = 'Layer Row Column Elevation Cond LayR RowR ColR Rfprop:' then
    begin
      ReadNonParameterLocationsWithReturnFlow;
    end
    else if ALabel = 'xyz:' then
    begin
      ReadAuxilliaryVariables;
    end
    else if ALabel = 'Pname:' then
    begin
      ReadParamNameForStressPeriod;
    end
    else if ALabel = 'Iname:' then
    begin
      ReadInstanceNameForStressPeriod;
    end
    else
    begin
      Assert(False);
    end;
  end;
end;

procedure TDrtImporter.ImportNonParameterBoundaries(
  var ScreenObjectIndex: Integer);
var
  Item: TCustomModflowBoundaryItem;
  SO_Boundary: TModflowParamBoundary;
  ScreenObject: TScreenObject;
  Boundary, AnotherBoundary: TDrtLocationObject;
  BoundaryIndex: Integer;
  SP: TListStressPeriod;
  InnerIndex: Integer;
  StressPeriod: TListStressPeriod;
  EndTime: Double;
  StartTime: Double;
  StressPeriodIndex: Integer;
  UsedLocations: array of array of boolean;
  LocationsToUse: TList;
  InnerBoundaryIndex: Integer;
  InstanceCount: integer;
  procedure InitializeTestArray;
  var
    RowIndex: Integer;
    ColIndex: Integer;
  begin
    for RowIndex := 0 to FGrid.RowCount - 1 do
    begin
      for ColIndex := 0 to FGrid.ColumnCount - 1 do
      begin
        UsedLocations[RowIndex,ColIndex] := false;
      end;
    end;
  end;
begin
  InstanceCount := 0;
  SetLength(UsedLocations, FGrid.RowCount, FGrid.ColumnCount);
  LocationsToUse := TList.Create;
  try
    for StressPeriodIndex := 0 to FStressPeriods.ArrayLength - 1 do
    begin
      // initialize the start and end times for when the boundary will be
      // applied.
      StartTime := FModel.ModflowStressPeriods[StressPeriodIndex].StartTime;
      EndTime := FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
      StressPeriod := FStressPeriods[StressPeriodIndex];
      if StressPeriod.Reuse then
      begin
        Continue;
      end
      else
      begin
        // Update the endtime if the boundaries from the current
        // stress period will be reused in subsequent stress periods.
        for InnerIndex := StressPeriodIndex + 1 to
          FStressPeriods.ArrayLength - 1 do
        begin
          SP := FStressPeriods[InnerIndex];
          if SP.Reuse then
          begin
            EndTime := FModel.ModflowStressPeriods[InnerIndex].EndTime;
          end
          else
          begin
            break;
          end;
        end;

        for BoundaryIndex := 0 to StressPeriod.ArrayLength - 1 do
        begin
          Boundary := StressPeriod.Boundaries[BoundaryIndex]
            as TDrtLocationObject;
          if Boundary.Used then
          begin
            Continue;
          end;
          InitializeTestArray;
          LocationsToUse.Clear;
          LocationsToUse.Add(Boundary);
          UsedLocations[Boundary.Row-1, Boundary.Column-1] := True;
          Boundary.Used := True;
          for InnerBoundaryIndex := BoundaryIndex+1 to
            StressPeriod.ArrayLength - 1 do
          begin
            AnotherBoundary := StressPeriod.Boundaries[InnerBoundaryIndex]
              as TDrtLocationObject;
            if not AnotherBoundary.Used
              and (Boundary.Layer = AnotherBoundary.Layer)
              and not UsedLocations[AnotherBoundary.Row-1,
              AnotherBoundary.Column-1]
              and (Boundary.LayR = AnotherBoundary.LayR)
              and (Boundary.RowR = AnotherBoundary.RowR)
              and (Boundary.ColR = AnotherBoundary.ColR)
              then
            begin
              LocationsToUse.Add(AnotherBoundary);
              UsedLocations[AnotherBoundary.Row-1,
                AnotherBoundary.Column-1] := True;
              AnotherBoundary.Used := True;
            end;
          end;
          ScreenObject := CreateScreenObject(LocationsToUse, ScreenObjectIndex,
            Boundary.Layer, StressPeriodIndex);

          // Assign values to the TScreenObject
          // for the current stress period(s).
          SO_Boundary := GetBoundary(ScreenObject);
          Item := SO_Boundary.Values.Add as TCustomModflowBoundaryItem;
          Inc(InstanceCount);
          SetItemValues(Item, LocationsToUse, EndTime, StartTime, ScreenObject,
            IntToStr(InstanceCount));
        end;
      end;
    end;
  finally
    LocationsToUse.Free;
  end;
end;

procedure TDrtImporter.ImportParameterBoundaries(ScreenObjectIndex: Integer);
var
  Item: TCustomModflowBoundaryItem;
  EndTime: Double;
  LocationIndex: Integer;
  InnerParamIndex: Integer;
  StressPeriod: TListStressPeriod;
  StressPeriodIndex: Integer;
  Instance: TListInstanceObject;
  InstanceIndex: Integer;
  InstanceCount: Integer;
  Instances: TIntegerList;
  UsedStressPeriods: TIntegerList;
  NewParameter: TModflowTransientListParameter;
  Parameter: TListParameterObject;
  ParameterIndex: Integer;
  Parameters: TModflowTransientListParameters;
  StartTime: Double;
  StressPeriodI: Integer;
  Param: TCustomMF_BoundColl;
  ParamItem: TModflowParamItem;
  SO_Boundary: TModflowParamBoundary;
  ScreenObject: TScreenObject;
  Boundary: TDrtLocationObject;
  UsedLocations: array of array of boolean;
  LocationsToUse: TList;
  InnerBoundaryIndex: Integer;
  AnotherBoundary: TDrtLocationObject;
  ACount: integer;
  procedure InitializeTestArray;
  var
    RowIndex: Integer;
    ColIndex: Integer;
  begin
    for RowIndex := 0 to FGrid.RowCount - 1 do
    begin
      for ColIndex := 0 to FGrid.ColumnCount - 1 do
      begin
        UsedLocations[RowIndex,ColIndex] := false;
      end;
    end;
  end;
begin
  ACount := 0;
  SetLength(UsedLocations, FGrid.RowCount, FGrid.ColumnCount);
  // Create parameters in the model.
  Parameters := TModflowTransientListParameters.Create(nil);
  try
    Parameters.Assign(FModel.ModflowTransientParameters);
    for ParameterIndex := 0 to FParameters.ArrayLength - 1 do
    begin
      Parameter := FParameters[ParameterIndex];
      NewParameter := Parameters.Add as TModflowTransientListParameter;
      NewParameter.ParameterName := Parameter.PARNAM;
      Parameter.ModifiedParamName := NewParameter.ParameterName;
      NewParameter.ParameterType := ParameterType;
      NewParameter.Value := Parameter.Parval;
    end;
    FModel.ModflowTransientParameters := Parameters;
  finally
    Parameters.Free;
  end;

  LocationsToUse := TList.Create;
  // Create TScreenObjects to represent the boundaries.
  // UsedStressPeriods will list the stress periods
  // in which a parameter is used.
  UsedStressPeriods := TIntegerList.Create;
  // Instances will indicate which instance of a parameter will be used
  // in any particular stress period.
  Instances := TIntegerList.Create;
  try
    for ParameterIndex := 0 to FParameters.ArrayLength - 1 do
    begin
      Parameter := FParameters[ParameterIndex];
      InstanceCount := Parameter.ArrayLength;
      UsedStressPeriods.Clear;
      Instances.Clear;
      if InstanceCount > 1 then
      begin
        for InstanceIndex := 0 to InstanceCount - 1 do
        begin
          Instance := Parameter.Instances[InstanceIndex];
          Assert(Instance.Name <> '');
        end;
      end;
      // Identify the stress periods in which the parameter is used
      // and the instance used in each stress period.
      for StressPeriodIndex := 0 to FStressPeriods.ArrayLength - 1 do
      begin
        StressPeriod := FStressPeriods[StressPeriodIndex];
        for InnerParamIndex := 0 to Length(StressPeriod.Parameters) - 1 do
        begin
          if UpperCase(Parameter.PARNAM) =
            UpperCase(StressPeriod.Parameters[InnerParamIndex]) then
          begin
            if InstanceCount = 1 then
            begin
              UsedStressPeriods.Add(StressPeriodIndex);
              Instances.Add(0);
            end
            else
            begin
              for InstanceIndex := 0 to InstanceCount - 1 do
              begin
                Instance := Parameter.Instances[InstanceIndex];
                if (UpperCase(Instance.Name) =
                  UpperCase(StressPeriod.Instances[InnerParamIndex])) then
                begin
                  UsedStressPeriods.Add(StressPeriodIndex);
                  Instances.Add(InstanceIndex);
                end;
              end;
            end;
          end;
        end;
      end;
      // Create the TScreenObjects to represent the boundaries.
      for InstanceIndex := 0 to InstanceCount - 1 do
      begin
        Instance := Parameter.Instances[InstanceIndex];
        for LocationIndex := 0 to Instance.ArrayLength - 1 do
        begin
          Boundary := Instance.Locations[LocationIndex] as TDrtLocationObject;
          if Boundary.Used then
          begin
            Continue;
          end;
          InitializeTestArray;
          LocationsToUse.Clear;
          LocationsToUse.Add(Boundary);
          UsedLocations[Boundary.Row-1, Boundary.Column-1] := True;
          Boundary.Used := True;
          for InnerBoundaryIndex := LocationIndex+1 to
            Instance.ArrayLength - 1 do
          begin
            AnotherBoundary := Instance.Locations[InnerBoundaryIndex]
              as TDrtLocationObject;
            if not AnotherBoundary.Used
              and (Boundary.Layer = AnotherBoundary.Layer)
              and not UsedLocations[AnotherBoundary.Row-1,
              AnotherBoundary.Column-1]
              and (Boundary.LayR = AnotherBoundary.LayR)
              and (Boundary.RowR = AnotherBoundary.RowR)
              and (Boundary.ColR = AnotherBoundary.ColR)
              then
            begin
              LocationsToUse.Add(AnotherBoundary);
              UsedLocations[AnotherBoundary.Row-1,
                AnotherBoundary.Column-1] := True;
              AnotherBoundary.Used := True;
            end;
          end;
          ScreenObject := CreateScreenObject(LocationsToUse, ScreenObjectIndex,
            Boundary.Layer, InstanceIndex);
          SO_Boundary := GetBoundary(ScreenObject);
          ParamItem := SO_Boundary.Parameters.Add;
          Param := ParamItem.Param;
          Param.ParamName := Parameter.ModifiedParamName;
          // Loop over the stress periods in which this parameter
          // is used.
          for StressPeriodI := 0 to UsedStressPeriods.Count - 1 do
          begin
            // If this instance is used in the current stress period,
            // assign new values.
            if Instances[StressPeriodI] = InstanceIndex then
            begin
              StressPeriodIndex := UsedStressPeriods[StressPeriodI];
              StartTime :=
                FModel.ModflowStressPeriods[StressPeriodIndex].StartTime;
              EndTime := FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
              Item := Param.Add as TCustomModflowBoundaryItem;
              Inc(ACount);
              SetItemValues(Item, LocationsToUse, EndTime, StartTime,
                ScreenObject, IntToStr(ACount));
            end;
          end;
        end;
      end;
    end;
  finally
    UsedStressPeriods.Free;
    Instances.Free;
    LocationsToUse.Free;
  end;
end;

function TDrtImporter.ParameterType: TParameterType;
begin
  result := ptDRT;
end;

function TDrtImporter.ScreenObjectNameRoot: string;
begin
  result := 'Imported_DRT_'
end;

procedure TDrtImporter.SetItemValues(Item: TCustomModflowBoundaryItem;
  Boundaries: TList; EndTime, StartTime: Double; ScreenObject: TScreenObject;
  const ParamName: string);
var
  DrtBoundary: TDrtLocationObject;
  DrtItem: TDrtItem;
  ValueItem: TValueArrayItem;
  Index: Integer;
  DrtHeadValues: TValueArrayStorage;
  DrtConductanceValues: TValueArrayStorage;
  DrtElevName: string;
  ConductanceName: string;
  Layer: Integer;
  ReturnLocation: T3DRealPoint;
  DrainReturn: TDrainReturn;
  APoint: TPoint2D;
begin
  ValueItem := ScreenObject.ImportedValues.Add as TValueArrayItem;
  DrtElevName := 'DrtElevation' + ParamName;
  ValueItem.Name := DrtElevName;
  DrtHeadValues := ValueItem.Values;
  DrtHeadValues.DataType := rdtDouble;
  DrtHeadValues.Count := Boundaries.Count;

  ValueItem := ScreenObject.ImportedValues.Add as TValueArrayItem;
  ConductanceName := 'DrtConductance' + ParamName;
  ValueItem.Name := ConductanceName;
  DrtConductanceValues := ValueItem.Values;
  DrtConductanceValues.DataType := rdtDouble;
  DrtConductanceValues.Count := Boundaries.Count;

  DrtItem := Item as TDrtItem;
  DrtItem.StartTime := StartTime;
  DrtItem.EndTime := EndTime;
  for Index := 0 to Boundaries.Count - 1 do
  begin
    DrtBoundary := Boundaries[Index];
    DrtHeadValues.RealValues[Index] := DrtBoundary.Elevation;
    DrtConductanceValues.RealValues[Index] := DrtBoundary.Conductance;
  end;
  DrtItem.Elevation := rsObjectImportedValuesR + '("' + DrtElevName + '")';
  DrtItem.Conductance := rsObjectImportedValuesR
    + '("' + ConductanceName + '")';

  DrtBoundary := Boundaries[0];

  if DrtBoundary.LayR > 0 then
  begin
    Layer := FModel.LayerStructure.ModflowLayerToDataSetLayer
      (DrtBoundary.LayR);
    ReturnLocation := FModel.ModflowGrid.ThreeDElementCenter(DrtBoundary.ColR-1,
      DrtBoundary.RowR-1, Layer);
    APoint := FModel.ModflowGrid.TwoDElementCenter(DrtBoundary.ColR-1,
      DrtBoundary.RowR-1);

    DrainReturn := ScreenObject.ModflowDrtBoundary.DrainReturn;
    DrainReturn.ReturnChoice := rtLocation;
    DrainReturn.ReturnLocation.X := APoint.X;
    DrainReturn.ReturnLocation.Y := APoint.Y;
    DrainReturn.ReturnLocation.Z := ReturnLocation.Z;

    DrtItem.ReturnFraction := FloatToStr(DrtBoundary.Rfprop);
  end
  else
  begin
    DrainReturn := ScreenObject.ModflowDrtBoundary.DrainReturn;
    DrainReturn.ReturnChoice := rtNone;
  end;

end;

{ TEtsImporter }

procedure TEtsImporter.AssignEtsLayerNonParam(NewItemNeeded: Boolean;
  var LayerItem: TEvtLayerItem; EvtBoundary: TEtsBoundary; EvtLayerName: string;
  StressPeriodIndex: Integer);
begin
  if FEtsPackage.TimeVaryingLayers then
  begin
    if NewItemNeeded then
    begin
      LayerItem := EvtBoundary.EvapotranspirationLayers.Add as TEvtLayerItem;
      LayerItem.EvapotranspirationLayer := EvtLayerName;
      LayerItem.StartTime :=
        FModel.ModflowStressPeriods[StressPeriodIndex].StartTime;
      LayerItem.EndTime :=
        FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
    end
    else
    begin
      LayerItem.EndTime :=
        FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
    end;
  end;
end;

procedure TEtsImporter.AssignEtsSurfaceNonParam(EvtSurfaceName,
  EvtExtinctName: string; NewItemNeeded: Boolean;
  var EvtItem: TEtsSurfDepthItem; EvtBoundary: TEtsBoundary;
  StressPeriodIndex: Integer);
begin
  if NewItemNeeded then
  begin
    EvtItem := EvtBoundary.EtsSurfDepthCollection.Add as TEtsSurfDepthItem;
    EvtItem.EvapotranspirationSurface := EvtSurfaceName;
    EvtItem.EvapotranspirationDepth := EvtExtinctName;
    EvtItem.StartTime :=
      FModel.ModflowStressPeriods[StressPeriodIndex].StartTime;
    EvtItem.EndTime := FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
    AssignSegments(EvtItem, StressPeriodIndex);
  end
  else
  begin
    EvtItem.EndTime := FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
  end;
end;

function TEtsImporter.SegIndexToStr(SegIndex: integer): string;
var
  MaxSeg: Integer;
begin
  MaxSeg := Length(IntToStr(NETSEG - 1));
  result := IntToStr(SegIndex);
  while Length(result) < MaxSeg do
  begin
    result := '0' + result;
  end;
end;

procedure TEtsImporter.AssignSurfaceAndDepth(ScreenObject: TScreenObject;
  EvtSurfaceName, EvtDepthName: string; StressPeriodIndex: Integer);
var
  EtsBoundary: TEtsBoundary;
  LayerItem: TEtsSurfDepthItem;
begin
  EtsBoundary := ScreenObject.ModflowEtsBoundary;
  if FReuseEtSurface[StressPeriodIndex]
    and FReuseEtExtinctionDepth[StressPeriodIndex]
    and FReuseSegmentDefinition[StressPeriodIndex] then
  begin
    LayerItem := EtsBoundary.EtsSurfDepthCollection.
      Items[EtsBoundary.EtsSurfDepthCollection.Count - 1] as TEtsSurfDepthItem;
    LayerItem.EndTime :=
      FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
  end
  else
  begin
    LayerItem := EtsBoundary.EtsSurfDepthCollection.Add as TEtsSurfDepthItem;
    LayerItem.EvapotranspirationSurface := EvtSurfaceName;
    LayerItem.EvapotranspirationDepth := EvtDepthName;
    LayerItem.StartTime :=
      FModel.ModflowStressPeriods[StressPeriodIndex].StartTime;
    LayerItem.EndTime :=
      FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;

    AssignSegments(LayerItem, StressPeriodIndex);
  end;
end;

procedure TEtsImporter.AssignTimeVaryingLayer(ScreenObject: TScreenObject;
  EvtLayerName: string; StressPeriodIndex: Integer);
var
  LayerItem: TEvtLayerItem;
  EtsBoundary: TEtsBoundary;
begin
  if Package.TimeVaryingLayers then
  begin
    EtsBoundary := ScreenObject.ModflowEtsBoundary;
    if not FReuseLayerIndicator[StressPeriodIndex] then
    begin
      LayerItem := EtsBoundary.EvapotranspirationLayers.Add as TEvtLayerItem;
      LayerItem.EvapotranspirationLayer := EvtLayerName;
      LayerItem.StartTime :=
        FModel.ModflowStressPeriods[StressPeriodIndex].StartTime;
      LayerItem.EndTime :=
        FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
    end
    else
    begin
      LayerItem := EtsBoundary.EvapotranspirationLayers.
        Items[EtsBoundary.EvapotranspirationLayers.Count - 1] as TEvtLayerItem;
      LayerItem.EndTime :=
        FModel.ModflowStressPeriods[StressPeriodIndex].EndTime;
    end;
  end;
end;

constructor TEtsImporter.Create(Importer: TModflow2005Importer;
  ZoneImporter: TMultZoneImporter);
begin
  inherited Create(Importer, 'ETS:', ZoneImporter);
end;

procedure TEtsImporter.AssignSegments(LayerItem: TEtsSurfDepthItem;
  StressPeriodIndex: Integer);
var
  Item: TStringValueItem;
  EtFProportionName: string;
  SegString: string;
  SegIndex: Integer;
  DepthProportionName: string;
begin
  LayerItem.EtFractions.Capacity := NETSEG - 1;
  LayerItem.DepthFractions.Capacity := NETSEG - 1;
  for SegIndex := 1 to NETSEG - 1 do
  begin
    SegString := SegIndexToStr(SegIndex);
    EtFProportionName := StrImportedETSFractionalRate + SegString
      + '_' + GetStressPeriodString(StressPeriodIndex);
    Item := LayerItem.EtFractions.Add as TStringValueItem;
    Item.Value := EtFProportionName;
    DepthProportionName := 'Imported_ETS_FractionalDepth_' + SegString
      + '_' + GetStressPeriodString(StressPeriodIndex);
    Item := LayerItem.DepthFractions.Add as TStringValueItem;
    Item.Value := DepthProportionName;
  end;
end;

procedure TEtsImporter.CreateBoundary(ScreenObject: TScreenObject);
begin
  ScreenObject.CreateEtsBoundary;
end;

procedure TEtsImporter.CreateDepthFractionDataSet(SegmentIndex,
  StressPeriodIndex: Integer);
var
  DataSet: TDataArray;
  ScreenObject: TScreenObject;
  ValueList: TRealList;
  Values: T2DDoubleArray;
  MaxCount: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Value: Double;
  Root: string;
  DataSetRoot: string;
  SegString: string;
begin
  ScreenObject := nil;
  if not FReuseSegmentDefinition[StressPeriodIndex] then
  begin
    SegString := SegIndexToStr(SegmentIndex);
    DataSetRoot := StrImportedETSFractionalDepth
      + SegString + '_';
    CreateDataSet(StressPeriodIndex, DataSetRoot,
      rdtDouble, DataSet);
    if (FConstantDepthProportions[SegmentIndex-1] <> nil)
      and FConstantDepthProportions[SegmentIndex-1][
      StressPeriodIndex].IsConstant then
    begin
      DataSet.Formula :=
        FloatToStr(FConstantDepthProportions[SegmentIndex-1][
        StressPeriodIndex].RealValue);
    end
    else
    begin
      DataSet.Formula := '0';
      Assert(FVariableDepthProportions[SegmentIndex-1] <> nil);
      Assert(FVariableDepthProportions[SegmentIndex-1][
        StressPeriodIndex] <> nil);

      ValueList := TRealList.Create;
      try
        ValueList.Sorted := True;
        Values := FVariableDepthProportions[SegmentIndex-1][StressPeriodIndex];
        MaxCount := Min((Length(Values) * Length(Values[0])) div 10, 100);
        for RowIndex := 0 to Length(Values) - 1 do
        begin
          for ColIndex := 0 to Length(Values[0]) - 1 do
          begin
            Value := Values[RowIndex,ColIndex];
            ValueList.AddUnique(Value);
            if ValueList.Count >= MaxCount then
            begin
              break;
            end;
          end;
          if ValueList.Count >= MaxCount then
          begin
            break;
          end;
        end;
        if ValueList.Count >= MaxCount then
        begin
          if ScreenObject = nil then
          begin
            CreateOrRetrieveCellCenterScreenObject(ScreenObject);
          end;
          AssignRealValuesToCellCenters(DataSet, ScreenObject,
            FVariableDepthProportions[SegmentIndex-1][StressPeriodIndex]);
        end
        else if ValueList.Count = 1 then
        begin
          DataSet.Formula :=
            FloatToStr(ValueList[0]);
        end
        else
        begin
          Root := StrETSImportedFractionalDepth + SegString + '_'
            + GetStressPeriodString(StressPeriodIndex) + '_Obj_';
          CreateScreenObjectsAroundValues(Values, Root,
            DataSet, ValueList);
        end;
      finally
        ValueList.Free;
      end;
    end;
  end;
end;

procedure TEtsImporter.CreateRateFractionDataSet(SegmentIndex,
  StressPeriodIndex: Integer);
var
  DataSet: TDataArray;
  ScreenObject: TScreenObject;
  ValueList: TRealList;
  Values: T2DDoubleArray;
  MaxCount: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Value: Double;
  Root: string;
  DataSetRoot: string;
  SegString: string;
begin
  ScreenObject := nil;
  if not FReuseSegmentDefinition[StressPeriodIndex] then
  begin
    SegString := SegIndexToStr(SegmentIndex);
    DataSetRoot := StrImportedETSFractionalRate
      + SegString + '_';
    CreateDataSet(StressPeriodIndex, DataSetRoot,
      rdtDouble, DataSet);
    if (FConstantRateProportions[SegmentIndex-1] <> nil)
      and FConstantRateProportions[SegmentIndex-1][
      StressPeriodIndex].IsConstant then
    begin
      DataSet.Formula :=
        FloatToStr(FConstantRateProportions[SegmentIndex-1][
        StressPeriodIndex].RealValue);
    end
    else
    begin
      DataSet.Formula := '0';
      Assert(FVariableRateProportions[SegmentIndex-1] <> nil);
      Assert(FVariableRateProportions[SegmentIndex-1][
        StressPeriodIndex] <> nil);

      ValueList := TRealList.Create;
      try
        ValueList.Sorted := True;
        Values := FVariableRateProportions[SegmentIndex-1][StressPeriodIndex];
        MaxCount := Min((Length(Values) * Length(Values[0])) div 10, 100);
        for RowIndex := 0 to Length(Values) - 1 do
        begin
          for ColIndex := 0 to Length(Values[0]) - 1 do
          begin
            Value := Values[RowIndex,ColIndex];
            ValueList.AddUnique(Value);
            if ValueList.Count >= MaxCount then
            begin
              break;
            end;
          end;
          if ValueList.Count >= MaxCount then
          begin
            break;
          end;
        end;
        if ValueList.Count >= MaxCount then
        begin
          if ScreenObject = nil then
          begin
            CreateOrRetrieveCellCenterScreenObject(ScreenObject);
          end;
          AssignRealValuesToCellCenters(DataSet, ScreenObject,
            FVariableRateProportions[SegmentIndex-1][StressPeriodIndex]);
        end
        else if ValueList.Count = 1 then
        begin
          DataSet.Formula :=
            FloatToStr(ValueList[0]);
        end
        else
        begin
          Root := StrETSImportedFractionalRate
            + GetStressPeriodString(StressPeriodIndex) + '_Obj_';
          CreateScreenObjectsAroundValues(Values, Root,
            DataSet, ValueList);
        end;
      finally
        ValueList.Free;
      end;
    end;
  end;
end;

class function TEtsImporter.EtExtinctionDepth_Name: string;
begin
  result := 'Imported_EtsExtinctionDepth_StressPeriod';
end;

class function TEtsImporter.EtExtinctionDepth_SP: string;
begin
  result := 'Imported_ETS_ExtinctionDepth_SP_';
end;

function TEtsImporter.GetBoundary(
  ScreenObject: TScreenObject): TModflowParamBoundary;
begin
  result := ScreenObject.ModflowEtsBoundary;
end;

procedure TEtsImporter.HandlePackage;
var
  StressPeriodIndex: Integer;
  ScreenObject: TScreenObject;
  EtLayerDataSet: TDataArray;
  NewItemNeeded: Boolean;
  EtsBoundary: TEtsBoundary;
  EvtItem: TEvtItem;
  LayerItem: TEvtLayerItem;
  EvtName: string;
  EvtLayerName: string;
  EvtSurfaceName: string;
  ParamIndex: Integer;
  Param: TArrayParameterObject;
  Instance: TArrayInstanceObject;
  ClusterIndex: Integer;
  Cluster: TClusterObject;
  ObjectIndex: integer;
  ClusterList: TList;
  ScreenObjectList: TList;
  StressPeriod: TArrayStressPeriod;
  ScreenObIndex: Integer;
  EvtExtinctName: string;
  EvtSurf: TEtsSurfDepthItem;
  SegmentIndex: Integer;
begin
  if (FCurrentStressPeriod < 0) or
    (FCurrentStressPeriod < FModel.ModflowStressPeriods.Count -1) then
  begin
    Exit;
  end;
  inherited;
  FEtsPackage := FModel.ModflowPackages.EtsPackage;
  Package := FEtsPackage;
  FEtsPackage.IsSelected := True;
  FEtsPackage.Comments := FComments;
  SetEtsOption;
  FEtsPackage.SegmentCount := NETSEG;
  FEtsPackage.UpdateEtsSegmentCount;
  EvaluateTimeVaryingLayers(FEtsPackage);

  CreateAssignedLayerDataSet(FEtsPackage, 'Imported_Ets_Elevation',
    'Imported_Ets_Elevation', EtLayerDataSet);

  if NP > 0 then
  begin
    ClusterList := TList.Create;
    ScreenObjectList := TList.Create;
    try
//      ObjectIndex := 0;
      for ParamIndex := 0 to FParams.ArrayLength - 1 do
      begin
        Param := FParams[ParamIndex];
        CreateTransientParam(Param);

        ScreenObjectList.Clear;
        ClusterList.Clear;
        CreateScreenObjectsFromClusters(Param, ObjectIndex,
          ScreenObjectList, ClusterList, 'ImportedEtsParam_',
          'Imported_Ets_Elevation', FEtsPackage);
        // At this point, ScreenObjectList contains each TScreenObject
        // that has been created for a particular parameter
        // and Cluster list contains a TClusterObject that has the
        // zone array and zones used to define the geometry of the
        // TScreenObject but not necessarily the corresponding Multiplier
        // array.

        // What need's to be done is to identify the stress periods in which
        // each parameter instance was used and find the corresponding
        // TScreenObjects and assign the formulas for those
        // stress periods using the appropriate multiplier arrays.

        // Loop over stress periods
        for StressPeriodIndex := 0 to FStressPeriods.ArrayLength - 1 do
        begin
          if ParamIndex = 0 then
          begin
            CreateTimeVaryingAssignedLayerDataSet(StressPeriodIndex,
              'Imported_Ets_Layer_StressPeriod',
              'Imported_ETS_Layers_SP_', FEtsPackage);
            CreateEtSurfaceDataSet(StressPeriodIndex);
            CreateEtExtinctionDepthDataSet(StressPeriodIndex);
            for SegmentIndex := 1 to NETSEG - 1 do
            begin
              CreateDepthFractionDataSet(SegmentIndex,StressPeriodIndex);
              CreateRateFractionDataSet(SegmentIndex,StressPeriodIndex);
            end;
          end;
          if FEtsPackage.TimeVaryingLayers and
            (not FReuseLayerIndicator[StressPeriodIndex]) then
          begin
            EvtLayerName := 'Imported_Ets_Layer_StressPeriod' +
              GetStressPeriodString(StressPeriodIndex);
          end;
          if not FReuseEtSurface[StressPeriodIndex] then
          begin
            EvtSurfaceName := 'Imported_EtsSurface_StressPeriod'
              + GetStressPeriodString(StressPeriodIndex);
          end;
          if not FReuseEtExtinctionDepth[StressPeriodIndex] then
          begin
            EvtExtinctName := 'Imported_EtsExtinctionDepth_StressPeriod'
              + GetStressPeriodString(StressPeriodIndex);
          end;
          StressPeriod := FStressPeriods[StressPeriodIndex];
          if StressPeriod.Reuse then
          begin
            ReuseEtStressPeriodWithParameters(StressPeriodIndex,
              Param, ScreenObjectList, EvtLayerName, EvtSurfaceName,
              EvtExtinctName);
          end
          else
          begin
            // In each stress period, identify whether the parameter was used.
            // If it was used, identify the intance used.
            GetParamInstanceForCurrentStressPeriod(Instance, Param,
              StressPeriod);
            if Instance <> nil then
            begin
              // Identify the cluster associated with the instance.
              for ClusterIndex := 0 to Instance.ArrayLength - 1 do
              begin
                Cluster := Instance.Clusters[ClusterIndex];
                ScreenObIndex := ClusterList.IndexOf(Cluster);
                Assert(ScreenObIndex >= 0);
                // Identify the cluster in ClusterList with the same geometry
                // as the cluster used.
                // Retrieve the corresponding TScreenObject from
                // ScreenObjectList.
                // Assign a formula for this parameter in this
                // stress period.
                // The formula should be either the
                // name of the multiplier array,
                // if one is used, or 1 if a multiplier array is not used.
                ScreenObject := ScreenObjectList[ScreenObIndex];
                AssignParamEvtRate(ScreenObject, StressPeriodIndex,
                  Cluster, Param);
                AssignTimeVaryingLayer(ScreenObject,
                  EvtLayerName, StressPeriodIndex);
                AssignSurfaceAndDepth(ScreenObject, EvtSurfaceName,
                  EvtExtinctName, StressPeriodIndex);
              end;
            end;
          end;
        end;
      end;
    finally
      ScreenObjectList.Free;
      ClusterList.Free;
    end;
  end
  else
  begin
    for StressPeriodIndex := 0 to FModel.ModflowStressPeriods.Count - 1 do
    begin
      CreateEvtRateDataSet(StressPeriodIndex);
      CreateEtSurfaceDataSet(StressPeriodIndex);
      CreateEtExtinctionDepthDataSet(StressPeriodIndex);
      for SegmentIndex := 1 to NETSEG - 1 do
      begin
        CreateDepthFractionDataSet(SegmentIndex,StressPeriodIndex);
        CreateRateFractionDataSet(SegmentIndex,StressPeriodIndex);
      end;
      CreateTimeVaryingAssignedLayerDataSet(StressPeriodIndex,
        'Imported_Ets_Layer_StressPeriod', 'Imported_ETS_Layers_SP_',
        FEtsPackage);
    end;

    ScreenObject := CreateScreenObjectAroundGrid('ImportedEts');
    if (FEtsPackage.LayerOption = loSpecified)
      and not FEtsPackage.TimeVaryingLayers then
    begin
      ScreenObject.ElevationFormula := 'Imported_Ets_Elevation';
    end
    else
    begin
      ScreenObject.ElevationFormula := StrModelTop;
//      ScreenObject.ElevationCount := ecZero;
    end;

    CreateBoundary(ScreenObject);
    EtsBoundary := ScreenObject.ModflowEtsBoundary;
    EvtItem := nil;
    LayerItem := nil;
    for StressPeriodIndex := 0 to FModel.ModflowStressPeriods.Count - 1 do
    begin
      NewItemNeeded := not FReuseEtRate[StressPeriodIndex];
      if FEtsPackage.TimeVaryingLayers then
      begin
        if not FReuseLayerIndicator[StressPeriodIndex] then
        begin
          NewItemNeeded := True;
        end;
      end;
      if not FReuseEtSurface[StressPeriodIndex] then
      begin
        NewItemNeeded := True;
      end;
      if not FReuseEtExtinctionDepth[StressPeriodIndex] then
      begin
        NewItemNeeded := True;
      end;
      if not FReuseSegmentDefinition[StressPeriodIndex] then
      begin
        NewItemNeeded := True;
      end;

      if not FReuseEtRate[StressPeriodIndex] then
      begin
        EvtName := ImportedEtRateName
          + GetStressPeriodString(StressPeriodIndex);
      end;
      if FEtsPackage.TimeVaryingLayers and
        (not FReuseLayerIndicator[StressPeriodIndex]) then
      begin
        EvtLayerName := 'Imported_Ets_Layer_StressPeriod' +
          GetStressPeriodString(StressPeriodIndex);
      end;
      if not FReuseEtSurface[StressPeriodIndex] then
      begin
        EvtSurfaceName := ImportedEtSurfaceName
          + GetStressPeriodString(StressPeriodIndex);
      end;
      if not FReuseEtExtinctionDepth[StressPeriodIndex] then
      begin
        EvtExtinctName := EtExtinctionDepth_Name
          + GetStressPeriodString(StressPeriodIndex);
      end;

      AssignEtRateNonParam(EvtName, NewItemNeeded, EvtItem,
        EtsBoundary, StressPeriodIndex);
      AssignEtsLayerNonParam(NewItemNeeded, LayerItem,
        EtsBoundary, EvtLayerName, StressPeriodIndex);
      AssignEtsSurfaceNonParam(EvtSurfaceName,EvtExtinctName, NewItemNeeded,
        EvtSurf, EtsBoundary, StressPeriodIndex);
    end;
  end;
end;

procedure TEtsImporter.ReadData(const ALabel: string);
var
  Handled: Boolean;
  IETSCB: integer;
  NPETS: integer;
  INETSS: integer;
  INETSR: integer;
  INETSX: integer;
  INIETS: integer;
  INSGDF: integer;
  ALine: string;
  Value: double;
  IntValue: integer;
  ID: string;
begin
  inherited;
  FRequiredType := 'ETS';
  ImportSharedData(ALabel, Handled);
  if not Handled then
  begin
    if ALabel = 'NETSOP,IETSCB,NPETS,NETSEG:' then
    begin
      Read(FImporter.FFile, NETSOP);
      Read(FImporter.FFile, IETSCB);
      Read(FImporter.FFile, NPETS);
      Read(FImporter.FFile, NETSEG);
      ReadLn(FImporter.FFile);
      SetLength(FConstantDepthProportions, NETSEG-1);
      SetLength(FVariableDepthProportions, NETSEG-1);
      SetLength(FConstantRateProportions, NETSEG-1);
      SetLength(FVariableRateProportions, NETSEG-1);
    end
    else if ALabel = 'INETSS,INETSR,INETSX,INIETS,INSGDF:' then
    begin
      Read(FImporter.FFile, INETSS);
      Read(FImporter.FFile, INETSR);
      Read(FImporter.FFile, INETSX);
      Read(FImporter.FFile, INIETS);
      Read(FImporter.FFile, INSGDF);
      ReadLn(FImporter.FFile);
      FCurrentSegment := -1;
      Inc(FCurrentStressPeriod);
      InitializeStressPeriods;
      InitializeCurrentStressPeriod(INETSR);
      InitializeReuseEtSurface;
      FReuseEtSurface[FCurrentStressPeriod] := INETSS < 0;
      InitializeReuseEtRate;
      FReuseEtRate[FCurrentStressPeriod] := INETSR < 0;
      InitializeReuseExtinctionDepth;
      FReuseEtExtinctionDepth[FCurrentStressPeriod] := INETSX < 0;
      InitializeReuseLayerIndicator;
      FReuseLayerIndicator[FCurrentStressPeriod] := INIETS < 0;
      InitializeReuseSegmentDefinition;
      FReuseSegmentDefinition[FCurrentStressPeriod] := INSGDF < 0;
      CurrentParameter := 0;
    end
    else if ALabel = 'INETSS,INETSR,INETSX,INIETS:' then
    begin
      Read(FImporter.FFile, INETSS);
      Read(FImporter.FFile, INETSR);
      Read(FImporter.FFile, INETSX);
      Read(FImporter.FFile, INIETS);
      ReadLn(FImporter.FFile);
      Inc(FCurrentStressPeriod);
      InitializeStressPeriods;
      InitializeCurrentStressPeriod(INETSR);
      InitializeReuseEtSurface;
      FReuseEtSurface[FCurrentStressPeriod] := INETSS < 0;
      InitializeReuseEtRate;
      FReuseEtRate[FCurrentStressPeriod] := INETSR < 0;
      InitializeReuseExtinctionDepth;
      FReuseEtExtinctionDepth[FCurrentStressPeriod] := INETSX < 0;
      InitializeReuseLayerIndicator;
      FReuseLayerIndicator[FCurrentStressPeriod] := INIETS < 0;
      CurrentParameter := 0;
    end
    else if ALabel = 'INETSS,INETSR,INETSX:' then
    begin
      Read(FImporter.FFile, INETSS);
      Read(FImporter.FFile, INETSR);
      Read(FImporter.FFile, INETSX);
      ReadLn(FImporter.FFile);
      Inc(FCurrentStressPeriod);
      InitializeStressPeriods;
      InitializeCurrentStressPeriod(INETSR);
      InitializeReuseEtSurface;
      FReuseEtSurface[FCurrentStressPeriod] := INETSS < 0;
      InitializeReuseEtRate;
      FReuseEtRate[FCurrentStressPeriod] := INETSR < 0;
      InitializeReuseExtinctionDepth;
      FReuseEtExtinctionDepth[FCurrentStressPeriod] := INETSX < 0;
      CurrentParameter := 0;
    end
    else if ALabel = StrConstant2DRealArray then
    begin
      ReadLn(FImporter.FFile, ALine);
      ALine := Trim(ALine);
      Readln(FImporter.FFile, Value);
      if ALine = StrETSURFACEETSS then
      begin
        ReadRealConstantArrayItem(Value, FConstantEtSurface);
      end
      else if ALine = StrEVAPOTRANSRATEET then
      begin
        ReadRealConstantArrayItem(Value, FConstantEtRate);
      end
      else if ALine = StrEXTINCTIONDEPTHET then
      begin
        ReadRealConstantArrayItem(Value, FConstantExtinctionDepth);
      end
      else if ALine = 'EXTINCT. DEP. PROPORTION' then
      begin
        Inc(FCurrentSegment);
        ReadRealConstantArrayItem(Value,
          FConstantDepthProportions[FCurrentSegment]);
      end
      else if ALine = 'ET RATE PROPORTION' then
      begin
        ReadRealConstantArrayItem(Value,
          FConstantRateProportions[FCurrentSegment]);
      end
      else
      begin
        Assert(False);
      end;
    end
    else if ALabel = StrVariable2DRealArray then
    begin
      ReadLn(FImporter.FFile, ALine);
      ALine := Trim(ALine);
      if ALine = StrETSURFACEETSS then
      begin
        ReadRealVariableArray(FVariableEtSurface);
      end
      else if ALine = StrEVAPOTRANSRATEET then
      begin
        ReadRealVariableArray(FVariableEtRate);
      end
      else if ALine = StrEXTINCTIONDEPTHET then
      begin
        ReadRealVariableArray(FVariableExtinctionDepth);
      end
      else if ALine = 'EXTINCT. DEP. PROPORTION' then
      begin
        Inc(FCurrentSegment);
        ReadRealVariableArray(FVariableDepthProportions[FCurrentSegment]);
      end
      else if ALine = 'ET RATE PROPORTION' then
      begin
        ReadRealVariableArray(FVariableRateProportions[FCurrentSegment]);
      end
      else
      begin
        Assert(False);
      end;
    end
    else if ALabel = StrConstant2DIntegerArray then
    begin
      ReadLn(FImporter.FFile, ALine);
      ALine := Trim(ALine);
      Readln(FImporter.FFile, IntValue);
      if ALine = StrETLAYERINDEXIETS then
      begin
        ReadConstantIntArray(IntValue, FConstantLayerIndicators);
      end
      else
      begin
        Assert(False);
      end;
    end
    else if ALabel = StrVariable2DIntegerArray then
    begin
      ReadLn(FImporter.FFile, ID);
      ID := Trim(ID);
      if ID = StrETLAYERINDEXIETS then
      begin
        ReadVariableIntArray(FVariableLayerIndicators);
      end
      else
      begin
        Assert(False);
      end;
    end
    else
    begin
      Assert(False);
    end;
  end;
end;

class function TEtsImporter.ImportedEtRateName: string;
begin
  result := 'Imported_EtsRate_StressPeriod';
end;

class function TEtsImporter.ImportedEtSurfaceName: string;
begin
  result := 'Imported_EtsSurface_StressPeriod';
end;

class function TEtsImporter.ImportedEtSurfaceSP: string;
begin
  result := 'Imported_ETS_Surface_SP_';
end;

class function TEtsImporter.ImportedEtValuesName: string;
begin
  result := 'Imported_ETS_Values_SP_';
end;

procedure TEtsImporter.InitializeReuseSegmentDefinition;
var
  Index: Integer;
begin
  if FReuseSegmentDefinition = nil then
  begin
    SetLength(FReuseSegmentDefinition, FModel.ModflowStressPeriods.Count);
    for Index := 0 to Length(FReuseSegmentDefinition) - 1 do
    begin
      FReuseSegmentDefinition[Index] := False;
    end;
  end;
end;

procedure TEtsImporter.SetEtsOption;
var
  Option: Integer;
begin
  Option := NETSOP - 1;
  if Option < 0 then
  begin
    Option := 0;
  end;
  if Option > 2 then
  begin
    Option := 2;
  end;
  FEtsPackage.LayerOption := TLayerOption(Option);
end;

procedure TCustomETImporter.InitializeReuseEtSurface;
var
  Index: Integer;
begin
  if FReuseEtSurface = nil then
  begin
    SetLength(FReuseEtSurface, FModel.ModflowStressPeriods.Count);
    for Index := 0 to Length(FReuseEtSurface) - 1 do
    begin
      FReuseEtSurface[Index] := False;
    end;
  end;
end;

procedure TCustomETImporter.InitializeReuseExtinctionDepth;
var
  Index: Integer;
begin
  if FReuseEtExtinctionDepth = nil then
  begin
    SetLength(FReuseEtExtinctionDepth, FModel.ModflowStressPeriods.Count);
    for Index := 0 to Length(FReuseEtExtinctionDepth) - 1 do
    begin
      FReuseEtExtinctionDepth[Index] := False;
    end;
  end;
end;

procedure TCustomETImporter.InitializeReuseEtRate;
var
  Index: Integer;
begin
  if FReuseEtRate = nil then
  begin
    SetLength(FReuseEtRate, FModel.ModflowStressPeriods.Count);
    for Index := 0 to Length(FReuseEtRate) - 1 do
    begin
      FReuseEtRate[Index] := False;
    end;
  end;
end;

{ TResImporter }

constructor TResImporter.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'RES', nil);
  FStressPeriods:= TResStressPeriods.Create;
end;

procedure TResImporter.CreateBoundary(ScreenObject: TScreenObject);
begin
  ScreenObject.CreateResBoundary;
end;

destructor TResImporter.Destroy;
begin
  FStressPeriods.Free;
  inherited;
end;

procedure TResImporter.HandlePackage;
var
  DataArray: TDataArray;
  LayerIndex: Integer;
  Cluster : TClusterObject;
  ScreenObjectName: string;
  ScreenObject: TScreenObject;
  Position: Integer;
  ResIndex: Integer;
  ResBoundary: TResBoundary;
  StressPeriodIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  ResItem: TResItem;
  ResStressPeriod: TResStressPeriod;
  ResValues: TResStage;
begin
  if (FCurrentStressPeriod < 0) or
    (FCurrentStressPeriod < FModel.ModflowStressPeriods.Count -1) then
  begin
    Exit;
  end;
  inherited;
  FResPackage := FModel.ModflowPackages.ResPackage;
  FResPackage.IsSelected := True;
  FResPackage.Comments := FComments;
  FResPackage.LayerOption := TLayerOption(NRESOP-1);
  FResPackage.PrintStage := IRESPT > 0;
  FResPackage.TableStages := NPTS;

  FModel.CreateInitialDataSets;

  if FResPackage.LayerOption = loSpecified then
  begin
    DataArray := FModel.GetDataSetByName(rsResLayer);
    Assert(DataArray <> nil);
    if ConstantResLayer then
    begin
      DataArray.Formula := IntToStr(ResLayer[0,0]);
    end
    else
    begin
      Cluster := TClusterObject.Create;
      try
        SetLength(Cluster.Zones, 1);
        for LayerIndex := 1 to FModel.LayerStructure.ModflowLayerCount do
        begin
          Cluster.Zones[0] := LayerIndex;
          ScreenObjectName := 'Imported_RES_Layer_' + IntToStr(LayerIndex);
          ScreenObject := CreateScreenObjectAroundZones(
            ResLayer, Cluster, ScreenObjectName);
          ScreenObject.ElevationFormula := StrModelTop;
          Position := ScreenObject.AddDataSet(DataArray);
          ScreenObject.DataSetFormulas[Position] := IntToStr(LayerIndex);
        end;
      finally
        Cluster.Free;
      end;
    end;
  end;

  ScreenObject := nil;
  DataArray := FModel.GetDataSetByName(rsResBottom);
  Assert(DataArray <> nil);
  if ConstantLandSurface then
  begin
    DataArray.Formula := FloatToStr(LandSurface[0,0]);
  end
  else
  begin
    if ScreenObject = nil then
    begin
      CreateOrRetrieveCellCenterScreenObject(ScreenObject);
//      ScreenObject.Name := 'Imported_RES_Land_Surface';
    end;
    AssignRealValuesToCellCenters(DataArray, ScreenObject, LandSurface);
  end;

  DataArray := FModel.GetDataSetByName(rsResKv);
  Assert(DataArray <> nil);
  if ConstantVertK then
  begin
    DataArray.Formula := FloatToStr(VertK[0,0]);
  end
  else
  begin
    if ScreenObject = nil then
    begin
      CreateOrRetrieveCellCenterScreenObject(ScreenObject);
//      ScreenObject.Name := 'Imported_RES_Vertical_K';
    end;
    AssignRealValuesToCellCenters(DataArray, ScreenObject, VertK);
  end;

  DataArray := FModel.GetDataSetByName(rsResBedThickness);
  Assert(DataArray <> nil);
  if ConstantBedThick then
  begin
    DataArray.Formula := FloatToStr(BedThickness[0,0]);
  end
  else
  begin
    if ScreenObject = nil then
    begin
      CreateOrRetrieveCellCenterScreenObject(ScreenObject);
    end;
    AssignRealValuesToCellCenters(DataArray, ScreenObject, BedThickness);
  end;

  Cluster := TClusterObject.Create;
  try
    SetLength(Cluster.Zones, 1);
    for ResIndex := 1 to NRES do
    begin
      Cluster.Zones[0] := ResIndex;
      ScreenObjectName := 'Imported_Reservoir_' + IntToStr(ResIndex);
      ScreenObject := CreateScreenObjectAroundZones(
        ResLocation, Cluster, ScreenObjectName);
      ScreenObject.ElevationFormula := StrModelTop;
      CreateBoundary(ScreenObject);
      ResBoundary := ScreenObject.ModflowResBoundary;
      ResBoundary.Values.Capacity := FModel.ModflowStressPeriods.Count;
      for StressPeriodIndex := 0 to FModel.ModflowStressPeriods.Count - 1 do
      begin
        StressPeriod := FModel.ModflowStressPeriods[StressPeriodIndex];
        ResItem := ResBoundary.Values.Add as TResItem;
        ResStressPeriod := FStressPeriods[StressPeriodIndex];
        ResValues := ResStressPeriod[ResIndex-1];
        ResItem.StartTime := StressPeriod.StartTime;
        ResItem.EndTime := StressPeriod.EndTime;
        ResItem.StartHead := FloatToStr(ResValues.Ststage);
        ResItem.EndHead := FloatToStr(ResValues.Endstage);
      end;
    end;
  finally
    Cluster.Free;
  end;

end;

procedure TResImporter.ReadData(const ALabel: string);
var
  ID: string;
  Value: double;
  IntValue: integer;
  Layer: integer;
  IRESCB: integer;
  Index: Integer;
  FResStressPeriod: TResStressPeriod;
begin
  inherited;
  if ALabel = 'NRES,IRESCB,NRESOP,IRESPT,NPTS:' then
  begin
    Read(FImporter.FFile, NRES);
    Read(FImporter.FFile, IRESCB);
    Read(FImporter.FFile, NRESOP);
    Read(FImporter.FFile, IRESPT);
    Read(FImporter.FFile, NPTS);
    ReadLn(FImporter.FFile);
    FStressPeriods.ArrayLength := FModel.ModflowStressPeriods.Count;
  end
  else if ALabel = 'HRESSE(1,N),HRESSE(2,N):' then
  begin
    Inc(FCurrentStressPeriod);
    FResStressPeriod := FStressPeriods[FCurrentStressPeriod];
    FResStressPeriod.ArrayLength := NRES;
    for Index := 0 to NRES - 1 do
    begin
      Read(FImporter.FFile, FResStressPeriod[Index].Ststage);
      Read(FImporter.FFile, FResStressPeriod[Index].Endstage);
      ReadLn(FImporter.FFile);
    end;
  end
  else if ALabel = StrConstant2DRealArrayForLayer then
  begin
    ReadLn(FImporter.FFile, ID);
    ReadLn(FImporter.FFile, Layer);
    Readln(FImporter.FFile, Value);
    ID := Trim(ID);
    if ID = 'RESERVOIR LAND SURF ELEV' then
    begin
      ConstantLandSurface := True;
      SetLength(LandSurface, FModel.Grid.RowCount, FModel.Grid.ColumnCount);
      AssignConstant2DArray(Value, LandSurface);
    end
    else if ID = 'RES. BED VERT HYD COND' then
    begin
      ConstantVertK := True;
      SetLength(VertK, FModel.Grid.RowCount, FModel.Grid.ColumnCount);
      AssignConstant2DArray(Value, VertK);
    end
    else if ID = 'RESERVOIR BED THICKNESS' then
    begin
      ConstantBedThick := True;
      SetLength(BedThickness, FModel.Grid.RowCount, FModel.Grid.ColumnCount);
      AssignConstant2DArray(Value, BedThickness);
    end
    else
    begin
      Assert(False);
    end;
  end
  else if ALabel = StrVariable2DRealArrayForLayer then
  begin
    ReadLn(FImporter.FFile, ID);
    ReadLn(FImporter.FFile, Layer);
    ID := Trim(ID);
    if ID = 'RESERVOIR LAND SURF ELEV' then
    begin
      ConstantLandSurface := False;
      SetLength(LandSurface, FModel.Grid.RowCount, FModel.Grid.ColumnCount);
      Read2DRealArray(LandSurface);
    end
    else if ID = 'RES. BED VERT HYD COND' then
    begin
      ConstantVertK := False;
      SetLength(VertK, FModel.Grid.RowCount, FModel.Grid.ColumnCount);
      Read2DRealArray(VertK);
    end
    else if ID = 'RESERVOIR BED THICKNESS' then
    begin
      ConstantBedThick := False;
      SetLength(BedThickness, FModel.Grid.RowCount, FModel.Grid.ColumnCount);
      Read2DRealArray(BedThickness);
    end
    else
    begin
      Assert(False);
    end;
  end
  else if ALabel = StrConstant2DIntegerArrayForLayer then
  begin
    ReadLn(FImporter.FFile, ID);
    ReadLn(FImporter.FFile, Layer);
//    Dec(Layer);
    ReadLn(FImporter.FFile, IntValue);
    ID := Trim(ID);
    if ID = 'RESERVOIR LOCATION' then
    begin
      ConstantResLocation := True;
      SetLength(ResLocation, FModel.Grid.RowCount, FModel.Grid.ColumnCount);
      AssignConstant2DIntArray(IntValue, ResLocation);
    end
    else if ID = 'RESERVOIR LAYER INDEX' then
    begin
      ConstantResLayer := True;
      SetLength(ResLayer, FModel.Grid.RowCount, FModel.Grid.ColumnCount);
      AssignConstant2DIntArray(IntValue, ResLayer);
    end
    else
    begin
      Assert(False);
    end;
  end
  else if ALabel = StrVariable2DIntegerArrayForLayer then
  begin
    ReadLn(FImporter.FFile, ID);
    ReadLn(FImporter.FFile, Layer);
    ID := Trim(ID);
    if ID = 'RESERVOIR LOCATION' then
    begin
      ConstantResLocation := False;
      SetLength(ResLocation, FModel.Grid.RowCount, FModel.Grid.ColumnCount);
      ReadVariable2DIntArray(ResLocation);
    end
    else if ID = 'RESERVOIR LAYER INDEX' then
    begin
      ConstantResLayer := False;
      SetLength(ResLayer, FModel.Grid.RowCount, FModel.Grid.ColumnCount);
      ReadVariable2DIntArray(ResLayer);
    end
    else
    begin
      Assert(False);
    end;
  end
  else
  begin
    Assert(False);
  end;
end;

{ TResStressPeriod }

function TResStressPeriod.ArrayMemberClass: TArrayMemberClass;
begin
  result := TResStage;
end;

function TResStressPeriod.GetStages(Index: integer): TResStage;
begin
  result := Objects[Index] as TResStage
end;

{ TResStressPeriods }

function TResStressPeriods.ArrayMemberClass: TArrayMemberClass;
begin
  result := TResStressPeriod;
end;

function TResStressPeriods.GetStressPeriods(Index: integer): TResStressPeriod;
begin
  result := Objects[Index] as TResStressPeriod
end;

{ TUzfImporter }

constructor TUzfImporter.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'UZF', nil);
  FEtStressPeriods:= TArrayStressPeriodArray.Create;
  FEtExtinctDepthStressPeriods:= TArrayStressPeriodArray.Create;
  FEtExtinctWaterContentStressPeriods:= TArrayStressPeriodArray.Create;
  FGages:= TUzfGageArray.Create;
  FCurrentGage := -1;
end;

procedure TUzfImporter.CreateBoundary(ScreenObject: TScreenObject);
begin
  ScreenObject.CreateUzfBoundary;
end;

destructor TUzfImporter.Destroy;
begin
  FGages.Free;
  FEtStressPeriods.Free;
  FEtExtinctDepthStressPeriods.Free;
  FEtExtinctWaterContentStressPeriods.Free;
  inherited;
end;

procedure TPackageImporter.AssignImportedValues(ImportedValues: TValueArrayItem;
  ImportedData: T2DDoubleArray);
var
  RowIndex: Integer;
  ColIndex: Integer;
  PointIndex: Integer;
begin
  ImportedValues.Values.DataType := rdtDouble;
  ImportedValues.Values.Count := FGrid.RowCount * FGrid.ColumnCount;
  PointIndex := 0;
  for RowIndex := 0 to FGrid.RowCount - 1 do
  begin
    for ColIndex := 0 to FGrid.ColumnCount - 1 do
    begin
      ImportedValues.Values.RealValues[PointIndex] :=
        ImportedData[RowIndex, ColIndex];
      Inc(PointIndex);
    end;
  end;
end;

procedure TUzfImporter.HandlePackage;
var
  DataArray: TDataArray;
  Index: Integer;
  Gage: TUzfGage;
  GageIndex: Integer;
  AScreenObject: TScreenObject;
  APoint: TPoint2D;
  Boundary: TUzfBoundary;
  StressPeriodIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  InfiltrationItem: TRchItem;
  ImportedValues: TValueArrayItem;
  ImportedData: T2DDoubleArray;
  EvtItem: TEvtItem;
  ExtinctDetphItem: TUzfExtinctDepthItem;
  WaterContentItem: TUzfWaterContentItem;
  CellCenterScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  RowIndex: Integer;
  ColIndex: Integer;
  ReUse: Boolean;
  PriorInfiltrationItem: TRchItem;
  PriorEvtItem: TEvtItem;
  PriorWaterContentItem: TUzfWaterContentItem;
  PriorExtinctDetphItem: TUzfExtinctDepthItem;
  procedure AssignVariableIntValues(const ScreenObjectName: string;
    Data: T2DIntArray);
  var
    Interpolator: TNearestPoint2DInterpolator;
  begin
    if CellCenterScreenObject = nil then
    begin
      CreateOrRetrieveCellCenterScreenObject(CellCenterScreenObject);
    end;


    AssignIntegerValuesToCellCenters(DataArray, CellCenterScreenObject, Data);
    Interpolator := TNearestPoint2DInterpolator.Create(nil);
    try
      DataArray.TwoDInterpolator := Interpolator;
    finally
      Interpolator.Free;
    end;
  end;
  procedure AssignVariableRealValues(const ScreenObjectName: string;
    Data: T2DDoubleArray);
  var
    Interpolator: TNearestPoint2DInterpolator;
  begin
    if CellCenterScreenObject = nil then
    begin
      CreateOrRetrieveCellCenterScreenObject(CellCenterScreenObject);
    end;

    AssignRealValuesToCellCenters(DataArray, CellCenterScreenObject, Data);
    Interpolator := TNearestPoint2DInterpolator.Create(nil);
    try
      DataArray.TwoDInterpolator := Interpolator;
    finally
      Interpolator.Free;
    end;
  end;
begin
  if (FCurrentStressPeriod < 0) or
    (FCurrentStressPeriod < FModel.ModflowStressPeriods.Count -1) then
  begin
    Exit;
  end;
  CellCenterScreenObject := nil;
  inherited;
  FUzfPackage := FModel.ModflowPackages.UzfPackage;
  FUzfPackage.IsSelected := True;
  FUzfPackage.Comments := FComments;

  FUzfPackage.VerticalKSource := Abs(IUZFOPT);
  FUzfPackage.RouteDischargeToStreams := IRUNFLG <> 0;
  FUzfPackage.SimulateET := IETFLG <> 0;
  FUzfPackage.NumberOfTrailingWaves := NTRAIL2;
  FUzfPackage.NumberOfWaveSets := NSETS2;
  FUzfPackage.DepthOfUndulations := SURFDEP;

  FModel.CreateInitialDataSets;

  DataArray := FModel.GetDataSetByName(StrUzfLayer);
  if IsConstIUZFBND then
  begin
    DataArray.Formula := IntToStr(ConstIUZFBND);
  end
  else
  begin
    AssignVariableIntValues('Imported_IUZFBND', IUZFBND);
  end;

  if IRUNFLG > 0 then
  begin
    DataArray := FModel.GetDataSetByName(StrUzfDischargeRouting);
    if IsConstIRUNBND then
    begin
      DataArray.Formula := IntToStr(ConstIRUNBND);
    end
    else
    begin
      AssignVariableIntValues('Imported_IRUNBND', IRUNBND);
    end;
  end;

  if Abs(IUZFOPT) = 1 then
  begin
    DataArray := FModel.GetDataSetByName(StrUzfVerticalK);
    if IsConstVks then
    begin
      DataArray.Formula := FloatToStr(ConstVks);
    end
    else
    begin
      AssignVariableRealValues('Imported_IRUNBND', VKS);
    end;
  end;
  
  DataArray := FModel.GetDataSetByName(StrUzfBrooksCoreyEpsilon);
  if IsConstEps then
  begin
    DataArray.Formula := FloatToStr(ConstEps);
  end
  else
  begin
    AssignVariableRealValues('Imported_EPS', EPS);
  end;

  DataArray := FModel.GetDataSetByName(StrUzfSaturatedWaterContent);
  if IsConstThts then
  begin
    DataArray.Formula := FloatToStr(ConstThts);
  end
  else
  begin
    AssignVariableRealValues('Imported_THTS', THTS);
  end;

  if Length(THTI) > 0 then
  begin
    DataArray := FModel.GetDataSetByName(StrUzfInitialUnsaturatedWaterContent);
    if IsConstThti then
    begin
      DataArray.Formula := FloatToStr(ConstThti);
    end
    else
    begin
      AssignVariableRealValues('Imported_THTI', THTI);
    end;
  end;

  GageIndex := 1;
  for Index := 0 to FGages.ArrayLength - 1 do
  begin
    Gage := FGages[Index];
    if Gage.IFTUNIT < 0 then
    begin
      FUzfPackage.PrintSummary := 1;
    end
    else
    begin
      AScreenObject := CreateScreenObject('Imported_UZF_Gage'
        + IntToStr(GageIndex));
      Inc(GageIndex);
      APoint := FGrid.TwoDElementCenter(Gage.IUZCOL-1, Gage.IUZROW-1);
      AScreenObject.AddPoint(APoint, True);
      AScreenObject.ElevationCount := ecZero;
      AScreenObject.SetValuesOfIntersectedCells := True;
      CreateBoundary(AScreenObject);
      Boundary := AScreenObject.ModflowUzfBoundary;
      case Gage.IUZOPT of
        1:
          begin
            Boundary.GageOption1 := 1;
          end;
        2:
          begin
            Boundary.GageOption1 := 2;
          end;
        3:
          begin
            Boundary.GageOption2 := 3;
          end;
        else Assert(False);
      end;
    end;
  end;

  InfiltrationItem := nil;
  EvtItem := nil;
  ExtinctDetphItem := nil;
  WaterContentItem := nil;
  AScreenObject := nil;
  for StressPeriodIndex := 0 to FModel.ModflowStressPeriods.Count - 1 do
  begin
    StressPeriod := FModel.ModflowStressPeriods[StressPeriodIndex];

    ReUse := FStressPeriods[StressPeriodIndex].Reuse;
    if ReUse and FUzfPackage.SimulateET then
    begin
      ReUse := FEtStressPeriods[StressPeriodIndex].Reuse
        and FEtExtinctDepthStressPeriods[StressPeriodIndex].Reuse
        and FEtExtinctWaterContentStressPeriods[StressPeriodIndex].Reuse;
    end;

    if Reuse then
    begin
      InfiltrationItem.EndTime := StressPeriod.EndTime;
      if FUzfPackage.SimulateET then
      begin
        EvtItem.EndTime := StressPeriod.EndTime;
        ExtinctDetphItem.EndTime := StressPeriod.EndTime;
        WaterContentItem.EndTime := StressPeriod.EndTime;
      end;
    end
    else
    begin
//      Boundary := nil;
      if AScreenObject = nil then
      begin
        AScreenObject := TScreenObject.CreateWithViewDirection(FModel, vdTop,
          UndoCreateScreenObject, False);
        AScreenObject.Name := 'Imported_UZF_Rates';

        FModel.AddScreenObject(AScreenObject);
        AScreenObject.ElevationCount := ecZero;
        AScreenObject.SetValuesByInterpolation := True;
        AScreenObject.EvaluatedAt := eaBlocks;
        AScreenObject.Visible := False;
        AScreenObject.Capacity := FGrid.RowCount * FGrid.ColumnCount;
        for RowIndex := 0 to FGrid.RowCount - 1 do
        begin
          for ColIndex := 0 to FGrid.ColumnCount - 1 do
          begin
            AScreenObject.AddPoint(FImporter.
              CenterPoints[RowIndex, ColIndex], True);
          end;
        end;
        AScreenObject.SetValuesOfIntersectedCells := True;
        AScreenObject.SetValuesByInterpolation := False;

        CreateBoundary(AScreenObject);
      end;
      Boundary := AScreenObject.ModflowUzfBoundary;
      InfiltrationItem := Boundary.Values.Add as TRchItem;
      InfiltrationItem.StartTime := StressPeriod.StartTime;
      InfiltrationItem.EndTime := StressPeriod.EndTime;

      if FStressPeriods[StressPeriodIndex].Reuse then
      begin
        PriorInfiltrationItem := Boundary.Values[Boundary.Values.Count -2]
          as TRchItem;
        InfiltrationItem.RechargeRate := PriorInfiltrationItem.RechargeRate
      end
      else
      begin
        if FConstantInfiltration[StressPeriodIndex].IsConstant then
        begin
          InfiltrationItem.RechargeRate :=
            FloatToStr(FConstantInfiltration[StressPeriodIndex].RealValue)
        end
        else
        begin
          ImportedData := FVariableInfiltration[StressPeriodIndex];
          ImportedValues := AScreenObject.ImportedValues.Add as TValueArrayItem;
          ImportedValues.Name := 'Imported_UZF_Infiltration_'
            + IntToStr(StressPeriodIndex+1);
          AssignImportedValues(ImportedValues, ImportedData);
          InfiltrationItem.RechargeRate := rsObjectImportedValuesR
            + '("' + ImportedValues.Name + '")';
        end;
      end;
      if FUzfPackage.SimulateET then
      begin
        if Reuse then
        begin
          EvtItem.EndTime := StressPeriod.EndTime;
        end
        else
        begin
          EvtItem := Boundary.EvapotranspirationDemand.Add as TEvtItem;
          EvtItem.StartTime := StressPeriod.StartTime;
          EvtItem.EndTime := StressPeriod.EndTime;
          if FEtStressPeriods[StressPeriodIndex].Reuse then
          begin
            PriorEvtItem := Boundary.EvapotranspirationDemand.Items[
              Boundary.EvapotranspirationDemand.Count-2] as TEvtItem;
            EvtItem.EvapotranspirationRate :=
              PriorEvtItem.EvapotranspirationRate;
          end
          else
          begin
            if FConstantET[StressPeriodIndex].IsConstant then
            begin
              EvtItem.EvapotranspirationRate :=
                FloatToStr(FConstantET[StressPeriodIndex].RealValue)
            end
            else
            begin
              ImportedData := FVariableET[StressPeriodIndex];
              ImportedValues := AScreenObject.ImportedValues.Add as TValueArrayItem;
              ImportedValues.Name := 'Imported_UZF_ET_'
                + IntToStr(StressPeriodIndex+1);
              AssignImportedValues(ImportedValues, ImportedData);
              EvtItem.EvapotranspirationRate := rsObjectImportedValuesR
                + '("' + ImportedValues.Name + '")';
            end;
          end;
        end;

        if Reuse then
        begin
          ExtinctDetphItem.EndTime := StressPeriod.EndTime;
        end
        else
        begin
          ExtinctDetphItem := Boundary.ExtinctionDepth.Add as TUzfExtinctDepthItem;
          ExtinctDetphItem.StartTime := StressPeriod.StartTime;
          ExtinctDetphItem.EndTime := StressPeriod.EndTime;
          if FEtExtinctDepthStressPeriods[StressPeriodIndex].Reuse then
          begin
            PriorExtinctDetphItem := Boundary.ExtinctionDepth.Items[
              Boundary.ExtinctionDepth.Count-2] as TUzfExtinctDepthItem;
            ExtinctDetphItem.UzfExtinctDepth :=
              PriorExtinctDetphItem.UzfExtinctDepth;
          end
          else
          begin
            if FConstantExtinctDepth[StressPeriodIndex].IsConstant then
            begin
              ExtinctDetphItem.UzfExtinctDepth :=
                FloatToStr(FConstantExtinctDepth[StressPeriodIndex].RealValue)
            end
            else
            begin
              ImportedData := FVariableExtinctDepth[StressPeriodIndex];
              ImportedValues := AScreenObject.ImportedValues.Add as TValueArrayItem;
              ImportedValues.Name := 'Imported_UZF_ExtinctionDepth_'
                + IntToStr(StressPeriodIndex+1);
              AssignImportedValues(ImportedValues, ImportedData);
              ExtinctDetphItem.UzfExtinctDepth := rsObjectImportedValuesR
                + '("' + ImportedValues.Name + '")';
            end;
          end;
        end;

        if Reuse then
        begin
          WaterContentItem.EndTime := StressPeriod.EndTime;
        end
        else
        begin
          WaterContentItem := Boundary.WaterContent.Add as TUzfWaterContentItem;
          WaterContentItem.StartTime := StressPeriod.StartTime;
          WaterContentItem.EndTime := StressPeriod.EndTime;
          if FEtExtinctWaterContentStressPeriods[StressPeriodIndex].Reuse then
          begin
            PriorWaterContentItem := Boundary.WaterContent.Items[
              Boundary.WaterContent.Count-2] as TUzfWaterContentItem;
            WaterContentItem.UzfWaterContent := PriorWaterContentItem.UzfWaterContent;
          end
          else
          begin
            if FConstantExtinctWaterContent[StressPeriodIndex].IsConstant then
            begin
              WaterContentItem.UzfWaterContent :=
                FloatToStr(FConstantExtinctWaterContent[StressPeriodIndex].RealValue)
            end
            else
            begin
              ImportedData := FVariableExtinctWaterContent[StressPeriodIndex];
              ImportedValues := AScreenObject.ImportedValues.Add as TValueArrayItem;
              ImportedValues.Name := 'Imported_UZF_ExtinctionWaterContent_'
                + IntToStr(StressPeriodIndex+1);
              AssignImportedValues(ImportedValues, ImportedData);
              WaterContentItem.UzfWaterContent := rsObjectImportedValuesR
                + '("' + ImportedValues.Name + '")';
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TUzfImporter.ReadData(const ALabel: string);
var
  IUZFCB1: integer;
  IUZFCB2: integer;
  Gage: TUzfGage;
  nuzf1: integer;
  nuzf2: integer;
  nuzf3: integer;
  nuzf4: integer;
  ID: string;
  Value: double;
  IntValue: integer;
begin
  inherited;
  if ALabel = 'NUZTOP IUZFOPT IRUNFLG IETFLG IUZFCB1 IUZFCB2 NTRAIL NSETS2 NUZGAG:' then
  begin
    Read(FImporter.FFile, NUZTOP);
    Read(FImporter.FFile, IUZFOPT);
    Read(FImporter.FFile, IRUNFLG);
    Read(FImporter.FFile, IETFLG);
    Read(FImporter.FFile, IUZFCB1);
    Read(FImporter.FFile, IUZFCB2);
    Read(FImporter.FFile, NTRAIL2);
    Read(FImporter.FFile, NSETS2);
    Read(FImporter.FFile, NUZGAG);
    Readln(FImporter.FFile);
    FGages.ArrayLength := NUZGAG;
  end
  else if ALabel = 'NUZTOP IUZFOPT IRUNFLG IETFLG IUZFCB1 IUZFCB2 NUZGAG:' then
  begin
    Read(FImporter.FFile, NUZTOP);
    Read(FImporter.FFile, IUZFOPT);
    Read(FImporter.FFile, IRUNFLG);
    Read(FImporter.FFile, IETFLG);
    Read(FImporter.FFile, IUZFCB1);
    Read(FImporter.FFile, IUZFCB2);
    Read(FImporter.FFile, NUZGAG);
    Readln(FImporter.FFile);
    FGages.ArrayLength := NUZGAG;
  end
  else if ALabel = 'SURFDEP:' then
  begin
    Read(FImporter.FFile, SURFDEP);
    Readln(FImporter.FFile);
  end
  else if ALabel = 'IUZROW IUZCOL IFTUNIT IUZOPT:' then
  begin
    Inc(FCurrentGage);
    Gage := FGages[FCurrentGage];
    Read(FImporter.FFile, Gage.IUZROW);
    Read(FImporter.FFile, Gage.IUZCOL);
    Read(FImporter.FFile, Gage.IFTUNIT);
    Read(FImporter.FFile, Gage.IUZOPT);
    Readln(FImporter.FFile);
  end
  else if ALabel = 'IFTUNIT:' then
  begin
    Inc(FCurrentGage);
    Gage := FGages[FCurrentGage];
    Read(FImporter.FFile, Gage.IFTUNIT);
    Readln(FImporter.FFile);
  end
  else if ALabel = 'nuzf1:' then
  begin
    Read(FImporter.FFile, nuzf1);
    Readln(FImporter.FFile);
    Inc(FCurrentStressPeriod);
    if FCurrentStressPeriod = 0 then
    begin
      FStressPeriods.ArrayLength :=
        FModel.ModflowStressPeriods.Count;
      SetLength(FConstantInfiltration,
        FModel.ModflowStressPeriods.Count);
      SetLength(FVariableInfiltration,
        FModel.ModflowStressPeriods.Count);
    end;
    FStressPeriods[FCurrentStressPeriod].Reuse := nuzf1 < 0;
  end
  else if ALabel = 'nuzf2:' then
  begin
    Read(FImporter.FFile, nuzf2);
    Readln(FImporter.FFile);
    if FCurrentStressPeriod = 0 then
    begin
      FEtStressPeriods.ArrayLength :=
        FModel.ModflowStressPeriods.Count;
      SetLength(FConstantET,
        FModel.ModflowStressPeriods.Count);
      SetLength(FVariableET,
        FModel.ModflowStressPeriods.Count);
    end;
    FEtStressPeriods[FCurrentStressPeriod].Reuse := nuzf2 < 0;
  end
  else if ALabel = 'nuzf3:' then
  begin
    Read(FImporter.FFile, nuzf3);
    Readln(FImporter.FFile);
    if FCurrentStressPeriod = 0 then
    begin
      FEtExtinctDepthStressPeriods.ArrayLength :=
        FModel.ModflowStressPeriods.Count;
      SetLength(FConstantExtinctDepth,
        FModel.ModflowStressPeriods.Count);
      SetLength(FVariableExtinctDepth,
        FModel.ModflowStressPeriods.Count);
    end;
    FEtExtinctDepthStressPeriods[FCurrentStressPeriod].Reuse := nuzf3 < 0;
  end
  else if ALabel = 'nuzf4:' then
  begin
    Read(FImporter.FFile, nuzf4);
    Readln(FImporter.FFile);
    if FCurrentStressPeriod = 0 then
    begin
      FEtExtinctWaterContentStressPeriods.ArrayLength :=
        FModel.ModflowStressPeriods.Count;
      SetLength(FConstantExtinctWaterContent,
        FModel.ModflowStressPeriods.Count);
      SetLength(FVariableExtinctWaterContent,
        FModel.ModflowStressPeriods.Count);
    end;
    FEtExtinctWaterContentStressPeriods[FCurrentStressPeriod].
      Reuse := nuzf4 < 0;
  end
  else if ALabel = StrConstant2DRealArray then
  begin
    ReadLn(FImporter.FFile, ID);
    ID := Trim(ID);
    Readln(FImporter.FFile, Value);
    if ID = 'SATURATED VERTICAL K' then
    begin
      IsConstVks := True;
      ConstVks := Value;
    end
    else if ID = 'BROOKS-COREY EPSILON' then
    begin
      IsConstEps := True;
      ConstEps := Value;
    end
    else if ID = 'SATURATED WATER CONTENT' then
    begin
      IsConstThts := True;
      ConstThts := Value;
    end
    else if ID = 'INITIAL WATER CONTENT' then
    begin
      IsConstThti := True;
      ConstThti := Value;
    end
    else if ID = 'AREAL INFILTRATION RATE' then
    begin
      ReadRealConstantArrayItem(Value, FConstantInfiltration);
    end
    else if ID = 'ET RATE' then
    begin
      ReadRealConstantArrayItem(Value, FConstantET);
    end
    else if ID = 'ET EXTINCTION DEPTH' then
    begin
      ReadRealConstantArrayItem(Value, FConstantExtinctDepth);
    end
    else if ID = 'EXTINCTION WATER CONTENT' then
    begin
      ReadRealConstantArrayItem(Value, FConstantExtinctWaterContent);
    end
    else
    begin
      Assert(False);
    end;
  end
  else if ALabel = StrVariable2DRealArray then
  begin
    ReadLn(FImporter.FFile, ID);
    ID := Trim(ID);
    if ID = 'SATURATED VERTICAL K' then
    begin
      IsConstVks := False;
      SetLength(VKS, FGrid.RowCount, FGrid.ColumnCount);
      Read2DRealArray(VKS);
    end
    else if ID = 'BROOKS-COREY EPSILON' then
    begin
      IsConstEps := False;
      SetLength(EPS, FGrid.RowCount, FGrid.ColumnCount);
      Read2DRealArray(EPS);
    end
    else if ID = 'SATURATED WATER CONTENT' then
    begin
      IsConstThts := False;
      SetLength(THTS, FGrid.RowCount, FGrid.ColumnCount);
      Read2DRealArray(THTS);
    end
    else if ID = 'INITIAL WATER CONTENT' then
    begin
      IsConstThti := False;
      SetLength(THTI, FGrid.RowCount, FGrid.ColumnCount);
      Read2DRealArray(THTI);
    end
    else if ID = 'AREAL INFILTRATION RATE' then
    begin
      ReadRealVariableArray(FVariableInfiltration)
    end
    else if ID = 'ET RATE' then
    begin
      ReadRealVariableArray(FVariableET)
    end
    else if ID = 'ET EXTINCTION DEPTH' then
    begin
      ReadRealVariableArray(FVariableExtinctDepth)
    end
    else if ID = 'EXTINCTION WATER CONTENT' then
    begin
      ReadRealVariableArray(FVariableExtinctWaterContent)
    end
    else
    begin
      Assert(False);
    end;
  end
  else if ALabel = StrConstant2DIntegerArray then
  begin
    ReadLn(FImporter.FFile, ID);
    ID := Trim(ID);
    Readln(FImporter.FFile, IntValue);
    if ID = 'AREAL EXTENT OF UZ FLOW' then
    begin
      IsConstIUZFBND := True;
      ConstIUZFBND := IntValue;
    end
    else if ID = 'ROUTING OVERLAND RUNOFF' then
    begin
      IsConstIRUNBND := True;
      ConstIRUNBND := IntValue;
    end
    else
    begin
      Assert(False);
    end;
  end
  else if ALabel = StrVariable2DIntegerArray then
  begin
    ReadLn(FImporter.FFile, ID);
    ID := Trim(ID);
    if ID = 'AREAL EXTENT OF UZ FLOW' then
    begin
      IsConstIUZFBND := False;
      SetLength(IUZFBND, FGrid.RowCount, FGrid.ColumnCount);
      ReadVariable2DIntArray(IUZFBND);
    end
    else if ID = 'ROUTING OVERLAND RUNOFF' then
    begin
      IsConstIRUNBND := False;
      SetLength(IRUNBND, FGrid.RowCount, FGrid.ColumnCount);
      ReadVariable2DIntArray(IRUNBND);
    end
    else
    begin
      Assert(False);
    end;
  end
  else
  begin
    Assert(False);
  end;
end;

{ TUzfGageArray }

function TUzfGageArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TUzfGage;
end;

function TUzfGageArray.GetGage(Index: integer): TUzfGage;
begin
  result := Objects[Index] as TUzfGage;
end;

{ TSolverImporter }

procedure TSolverImporter.DeselectAllSolvers;
begin
  FModel.ModflowPackages.PcgPackage.IsSelected := False;
  FModel.ModflowPackages.GmgPackage.IsSelected := False;
  FModel.ModflowPackages.SipPackage.IsSelected := False;
  FModel.ModflowPackages.De4Package.IsSelected := False;
end;

procedure TSolverImporter.HandlePackage;
begin
  inherited;
  DeselectAllSolvers;
end;

{ TGageImporter }

constructor TGageImporter.Create(Importer: TModflow2005Importer;
  LakImporter: TLakImporter; SfrImporter: TSfrImporter);
begin
  inherited Create(Importer, 'GAG');
  FGages := TGageArray.Create;
  FCurrentGage := -1;
  FLakImporter := LakImporter;
  FSfrImporter := SfrImporter;
end;

destructor TGageImporter.Destroy;
begin
  FGages.Free;
  inherited;
end;

procedure TGageImporter.HandlePackage;
var
  GageIndex: Integer;
  Gage: TGage;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Lake: TLakBoundary;
  ReachIndex: Integer;
  Reach: TSfrLocationObject;
  Layer: Integer;
  Point3D: T3DRealPoint;
  Point2D: TPoint2D;
  SfrGageNumber: Integer;
  SfrGage: TStreamGage;
begin
  if FImportedPackage then
  begin
    Exit;
  end;
  if (FModel.ModflowPackages.SfrPackage.IsSelected
    or (FSfrImporter.FReaches.ArrayLength > 0))
    and not FSfrImporter.FImportedPackage then
  begin
    Exit;
  end;
  if (FModel.ModflowPackages.LakPackage.IsSelected
    or (FLakImporter.FLakeStressPeriodValues.ArrayLength > 0))
    and not FLakImporter.FImportedPackage then
  begin
    Exit;
  end;
  inherited HandlePackage;
  SfrGageNumber := 0;
  for GageIndex := 0 to FGages.ArrayLength - 1 do
  begin
    Gage := FGages[GageIndex];
    case Gage.GageType of
      gtLake:
        begin
          for ScreenObjectIndex := 0 to FModel.ScreenObjectCount - 1 do
          begin
            ScreenObject := FModel.ScreenObjects[ScreenObjectIndex];
            Lake := ScreenObject.ModflowLakBoundary;
            if (Lake <> nil) and (Gage.LakeNumber = Lake.LakeID) then
            begin
              case Gage.OUTTYPE of
                0:
                  begin
                    Lake.StandardGage := True;
                  end;
                1:
                  begin
                    Lake.FluxCondGage := True;
                  end;
                2:
                  begin
                    Lake.DeltaGage := True;
                  end;
                3:
                  begin
                    Lake.StandardGage := True;
                    Lake.FluxCondGage := True;
                    Lake.DeltaGage := True;
                  end;
                else Assert(False);
              end;
              break;
            end;
          end;
        end;
      gtStream:
        begin
          for ReachIndex := 0 to FSfrImporter.FReaches.ArrayLength - 1 do
          begin
            Reach := FSfrImporter.FReaches[ReachIndex];
            if (Gage.GAGESEG = Reach.SegmentNumber)
              and (Gage.GAGERCH = Reach.ReachNumber) then
            begin
              Layer := FModel.LayerStructure.ModflowLayerToDataSetLayer(
                Reach.Layer);
              Point3D := FGrid.ThreeDElementCenter(
                Reach.Column-1, Reach.Row-1, Layer);
              Point2D := FGrid.TwoDElementCenter(Reach.Column-1, Reach.Row-1);
              Inc(SfrGageNumber);
              ScreenObject := CreateScreenObject('Imported_SFR_Gage_'
                + IntToStr(SfrGageNumber));
              ScreenObject.AddPoint(Point2D, True);
              ScreenObject.SetValuesOfEnclosedCells := False;
              ScreenObject.SetValuesOfIntersectedCells := True;

              ScreenObject.ElevationFormula := FloatToStr(Point3D.Z);
              ScreenObject.CreateGagBoundary;
              ScreenObject.ModflowStreamGage;
              SfrGage := ScreenObject.ModflowStreamGage;
              case Gage.OUTTYPE of
                0:
                  begin
                    SfrGage.Gage0 := True;
                  end;
                1:
                  begin
                    SfrGage.Gage1 := True;
                  end;
                2:
                  begin
                    SfrGage.Gage2 := True;
                  end;
                3:
                  begin
                    SfrGage.Gage3 := True;
                  end;
                4:
                  begin
                    SfrGage.Gage0 := True;
                    SfrGage.Gage1 := True;
                    SfrGage.Gage2 := True;
                    SfrGage.Gage3 := True;
                  end;
                5:
                  begin
                    SfrGage.Gage5 := True;
                  end;
                6:
                  begin
                    SfrGage.Gage6 := True;
                  end;
                7:
                  begin
                    SfrGage.Gage7 := True;
                  end;
              end;
              break;
            end;
          end;
        end;
      else Assert(False);
    end;
  end;
end;

procedure TGageImporter.ReadData(const ALabel: string);
var
  NUMGAGE: integer;
  UNIT_Number: integer;
  LAKE: integer;
  Gage: TGage;
begin
  inherited;
  if ALabel = 'NUMGAGE:' then
  begin
    Read(FImporter.FFile, NUMGAGE);
    Readln(FImporter.FFile);
    FGages.ArrayLength := NUMGAGE
  end
  else if ALabel = 'GAGESEG GAGERCH UNIT OUTTYPE:' then
  begin
    Inc(FCurrentGage);
    Gage := FGages[FCurrentGage];
    Gage.GageType := gtStream;
    Read(FImporter.FFile, Gage.GAGESEG);
    Read(FImporter.FFile, Gage.GAGERCH);
    Read(FImporter.FFile, UNIT_Number);
    Read(FImporter.FFile, Gage.OUTTYPE);
    Readln(FImporter.FFile);
  end
  else if ALabel = 'LAKE UNIT OUTTYPE:' then
  begin
    Inc(FCurrentGage);
    Gage := FGages[FCurrentGage];
    Gage.GageType := gtLake;
    Read(FImporter.FFile, LAKE);
    Gage.LakeNumber := Abs(LAKE);
    Read(FImporter.FFile, UNIT_Number);
    Read(FImporter.FFile, Gage.OUTTYPE);
    Readln(FImporter.FFile);
  end
  else
  begin
    Assert(False);
  end;
end;

{ TGageArray }

function TGageArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TGage;
end;

function TGageArray.GetGage(Index: integer): TGage;
begin
  result := Objects[Index] as TGage;
end;

function TPackageImporter.CreateScreenObject(const Name: string): TScreenObject;
var
  UndoCreateScreenObject: TCustomUndo;
begin
  result := TScreenObject.CreateWithViewDirection(FModel, vdTop,
    UndoCreateScreenObject, False);
  FModel.AddScreenObject(result);
  result.Name := Name;
  result.ElevationCount := ecOne;
  result.SetValuesOfEnclosedCells := True;
  result.EvaluatedAt := eaBlocks;
  result.Visible := False;
end;

{ THufImporter }

constructor THufImporter.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'HUF2:');
  FNextHufIndexIndex := -1;
  FNextParameterIndex := -1;
  FIsSelected := False;
  FReadLthuf := False;
  FReadKdep := True;
  FReadLvda := True;
end;

procedure THufImporter.CreateParameters(
  HydrogeologicUnits: THydrogeologicUnits);
var
  UsedParam: THufUsedParameter;
  ClusterIndex: Integer;
  Clusters: TClusterRecordArray;
  SteadyParam: TModflowSteadyParameter;
  Param: THufParameter;
  ParamIndex: Integer;
  ModflowSteadyParameters: TModflowSteadyParameters;
  HufParameters: THufModflowParameters;
  HGU: THydrogeologicUnit;
begin
  HufParameters := THufModflowParameters.Create(nil);
  ModflowSteadyParameters := TModflowSteadyParameters.Create(nil);
  try
    HufParameters.Assign(FModel.HufParameters);
    ModflowSteadyParameters.Assign(FModel.ModflowSteadyParameters);
    for ParamIndex := 0 to Length(FParameters) - 1 do
    begin
      Param := nil;
      SteadyParam := nil;
      if SameText(FParameters[ParamIndex].PARTYP, 'HK') then
      begin
        Param := HufParameters.Add as THufParameter;
        Param.ParameterType := ptHUF_HK;
      end
      else if SameText(FParameters[ParamIndex].PARTYP, 'HANI') then
      begin
        Param := HufParameters.Add as THufParameter;
        Param.ParameterType := ptHUF_HANI;
      end
      else if SameText(FParameters[ParamIndex].PARTYP, 'VK') then
      begin
        Param := HufParameters.Add as THufParameter;
        Param.ParameterType := ptHUF_VK;
      end
      else if SameText(FParameters[ParamIndex].PARTYP, 'VANI') then
      begin
        Param := HufParameters.Add as THufParameter;
        Param.ParameterType := ptHUF_VANI;
      end
      else if SameText(FParameters[ParamIndex].PARTYP, 'SS') then
      begin
        Param := HufParameters.Add as THufParameter;
        Param.ParameterType := ptHUF_SS;
      end
      else if SameText(FParameters[ParamIndex].PARTYP, 'SY') then
      begin
        Param := HufParameters.Add as THufParameter;
        Param.ParameterType := ptHUF_SY;
      end
      else if SameText(FParameters[ParamIndex].PARTYP, 'SYTP') then
      begin
        SteadyParam := ModflowSteadyParameters.Add as TModflowSteadyParameter;
        SteadyParam.ParameterType := ptHUF_SYTP;
      end
      else
      begin
        Assert(False);
      end;
      Clusters := FParameters[ParamIndex].Instances[0].Clusters;
      if Param <> nil then
      begin
        Param.ParameterName := FParameters[ParamIndex].PARNAM;
        Param.Value := FParameters[ParamIndex].Parval;
        for ClusterIndex := 0 to Length(Clusters) - 1 do
        begin
          HGU := HydrogeologicUnits[Clusters[ClusterIndex].Layer - 1];
          UsedParam := HGU.HufUsedParameters.Add as THufUsedParameter;
          UsedParam.ParameterName := Param.ParameterName;
          UsedParam.UseMultiplier :=
            not SameText(Clusters[ClusterIndex].MultiplierName, 'NONE');
          UsedParam.UseZone :=
            not SameText(Clusters[ClusterIndex].ZoneName, 'ALL');
        end;
      end
      else
      begin
        Assert(SteadyParam <> nil);
        SteadyParam.ParameterName := FParameters[ParamIndex].PARNAM;
        SteadyParam.Value := FParameters[ParamIndex].Parval;
        SteadyParam.UseMultiplier := False;
        SteadyParam.UseZone := False;
        for ClusterIndex := 0 to Length(Clusters) - 1 do
        begin
          if not SameText(Clusters[ClusterIndex].MultiplierName, 'NONE') then
          begin
            SteadyParam.UseMultiplier := True;
          end;
          if SameText(Clusters[ClusterIndex].MultiplierName, 'ALL') then
          begin
            SteadyParam.UseZone := True;
          end;
        end;
      end;
    end;
    FModel.HufParameters := HufParameters;
    FModel.ModflowSteadyParameters := ModflowSteadyParameters;
  finally
    HufParameters.Free;
    ModflowSteadyParameters.Free;
  end;
end;

procedure THufImporter.AssignSteadyParametersZoneAndMultiplier;
var
  ZoneFunctionList: TStringList;
  ParamIndex: Integer;
  MultFunction: string;
  ZoneIndex: Integer;
  ZoneFunction: string;
  LayerIndex: Integer;
  MultFunctionList: TStringList;
  ZoneDataArray: TDataArray;
  MultDataArray: TDataArray;
  SteadyParam: TModflowSteadyParameter;
  Cluster: TClusterRecord;
  ClusterIndex: Integer;
  PIndex: Integer;
  Clusters: TClusterRecordArray;
begin
  for ParamIndex := 0 to FModel.ModflowSteadyParameters.Count - 1 do
  begin
    SteadyParam := FModel.ModflowSteadyParameters[ParamIndex];
    if SteadyParam.ParameterType = ptHUF_SYTP then
    begin
      MultDataArray := nil;
      if SteadyParam.UseMultiplier then
      begin
        MultDataArray := FModel.GetDataSetByName(SteadyParam.MultiplierName);
      end;
      ZoneDataArray := nil;
      if SteadyParam.UseZone then
      begin
        ZoneDataArray := FModel.GetDataSetByName(SteadyParam.ZoneName);
      end;
      if (MultDataArray = nil) and (ZoneDataArray = nil) then
      begin
        Continue;
      end;
      MultFunctionList := TStringList.Create;
      ZoneFunctionList := TStringList.Create;
      try
        MultFunctionList.Add('0');
        ZoneFunctionList.Add('False');
        Clusters := nil;
        for PIndex := 0 to Length(FParameters) - 1 do
        begin
          if SameText(FParameters[PIndex].PARNAM,
            SteadyParam.ParameterName) then
          begin
            Clusters := FParameters[PIndex].Instances[0].Clusters;
            break;
          end;
        end;
        for ClusterIndex := 0 to Length(Clusters) - 1 do
        begin
          Cluster := Clusters[ClusterIndex];
          LayerIndex := 0;
          if SameText(Cluster.MultiplierName, StrNone) then
          begin
            MultFunctionList[LayerIndex] := '1';
          end
          else
          begin
            MultFunctionList[LayerIndex] := Cluster.MultiplierName;
          end;
          if SameText(Cluster.ZoneName, StrAll) then
          begin
            ZoneFunctionList[LayerIndex] := 'True';
          end
          else
          begin
            ZoneFunction := '';
            for ZoneIndex := 0 to Length(Cluster.ZoneValues) - 1 do
            begin
              ZoneFunction := ZoneFunction + '(' + Cluster.ZoneName
                + ' = ' + IntToStr(Cluster.ZoneValues[ZoneIndex]) + ')';
              if ZoneIndex < Length(Cluster.ZoneValues) - 1 then
              begin
                ZoneFunction := ZoneFunction + ' or ';
              end;
            end;
            ZoneFunctionList[LayerIndex] := ZoneFunction;
          end;
        end;
        if MultDataArray <> nil then
        begin
          Assert(MultFunctionList.Count = 1);
          MultFunction := MultFunctionList[0];
          MultDataArray.Formula := MultFunction;
        end;
        if ZoneDataArray <> nil then
        begin
          Assert(ZoneFunctionList.Count = 1);
          ZoneFunction := ZoneFunctionList[0];
          ZoneDataArray.Formula := ZoneFunction;
        end;
      finally
        MultFunctionList.Free;
        ZoneFunctionList.Free;
      end;
    end;
  end;
end;

procedure THufImporter.HandlePackage;
begin
  if FReadLthuf and FReadKdep and FReadLvda then
  begin
    inherited;
    HufPackage := FModel.ModflowPackages.HufPackage;
    HufPackage.IsSelected := FIsSelected;
    if FIsSelected then
    begin
      FModel.ModflowPackages.LpfPackage.IsSelected := False;
      FModel.ModflowPackages.BcfPackage.IsSelected := False;
      HufPackage.Comments := FComments;
      ImportDataSet1;
      ImportDataSet2;
      ImportDataSet3;
      ImportDataSet4;
      ImportWetDry;
      ImportHydrogeologicUnits;
      AssignHufParametersZoneAndMultiplier;
      AssignSteadyParametersZoneAndMultiplier;
      ImportGeometry;
      frmGoPhast.EnableHufMenuItems;
      if (FKdepImporter <> nil) and not FKdepImporter.ImportedPackage then
      begin
        FKdepImporter.HandlePackage;
      end;
      if (FLvdaImporter <> nil) and not FLvdaImporter.ImportedPackage then
      begin
        FLvdaImporter.HandlePackage;
      end;
    end;
    ReleaseMemory;
  end;
end;

procedure THufImporter.ImportDataSet2;
var
  LayerGroup: TLayerGroup;
  Index: Integer;
  LayerIndex: Integer;
begin
  // Data set 2.
  LayerIndex := -1;
  for Index := 1 to FModel.LayerStructure.Count - 1 do
  begin
    LayerGroup := FModel.LayerStructure.LayerGroups[Index];
    if LayerGroup.Simulated then
    begin
      Inc(LayerIndex);
    end;
    if LayerIndex >= 0 then
    begin
      if LTHUF[LayerIndex] <> 0 then
      begin
        LayerGroup.AquiferType := 1;
      end
      else if LTHUF[LayerIndex] = 0 then
      begin
        LayerGroup.AquiferType := 0;
      end
      else
      begin
        Assert(False);
      end;
    end;
  end;
  FModel.CreateInitialDataSets;
end;

procedure THufImporter.ImportDataSet3;
var
  LayerGroup: TLayerGroup;
  Index: Integer;
  LayerIndex: Integer;
begin
  // Data set 3.
  LayerIndex := -1;
  for Index := 1 to FModel.LayerStructure.Count - 1 do
  begin
    LayerGroup := FModel.LayerStructure.LayerGroups[Index];
    if LayerGroup.Simulated then
    begin
      Inc(LayerIndex);
      if LAYWT[LayerIndex] <> 0 then
      begin
        FModel.ModflowWettingOptions.WettingActive := True;
        break;
      end;
    end;
  end;
end;

procedure THufImporter.ImportDataSet4;
begin
  if FWetDryConst <> nil then
  begin
    FModel.ModflowWettingOptions.WettingActive := True;
    FModel.CreateInitialDataSets;
  end;
  if FModel.ModflowWettingOptions.WettingActive then
  begin
    FModel.ModflowWettingOptions.WettingFactor := WETFCT;
    FModel.ModflowWettingOptions.WettingIterations := IWETIT;
    FModel.ModflowWettingOptions.WettingEquation := IHDWET;
  end;
end;

procedure THufImporter.ImportGeometry;
var
  HufIndex: Integer;
  HGU: THydrogeologicUnit;
  TopArray: TDataArray;
  ScreenObject: TScreenObject;
  ThicknessArray: TDataArray;
  Interpolator: TNearestPoint2DInterpolator;

begin
  ScreenObject := nil;
  for HufIndex := 0 to FModel.HydrogeologicUnits.Count - 1 do
  begin
    HGU := FModel.HydrogeologicUnits[HufIndex];
    TopArray := FModel.GetDataSetByName(HGU.TopDataArrayName);
    Assert(TopArray <> nil);
    if (FConstantTopElevations <> nil)
      and FConstantTopElevations[HufIndex].IsConstant then
    begin
      TopArray.Formula :=
        FloatToStr(FConstantTopElevations[HufIndex].RealValue);
    end
    else
    begin
      if ScreenObject = nil then
      begin
        CreateOrRetrieveCellCenterScreenObject(ScreenObject);
      end;
      Interpolator := TNearestPoint2DInterpolator.Create(nil);
      try
        TopArray.TwoDInterpolator := Interpolator;
      finally
        Interpolator.Free;
      end;
      AssignRealValuesToCellCenters(TopArray,
        ScreenObject, Top[HufIndex]);
    end;

    ThicknessArray := FModel.GetDataSetByName(HGU.ThickessDataArrayName);
    Assert(ThicknessArray <> nil);
    if (FConstantThicknesses <> nil)
      and FConstantThicknesses[HufIndex].IsConstant then
    begin
      ThicknessArray.Formula :=
        FloatToStr(FConstantThicknesses[HufIndex].RealValue);
    end
    else
    begin
      if ScreenObject = nil then
      begin
        CreateOrRetrieveCellCenterScreenObject(ScreenObject);
      end;
      Interpolator := TNearestPoint2DInterpolator.Create(nil);
      try
        ThicknessArray.TwoDInterpolator := Interpolator;
      finally
        Interpolator.Free;
      end;
      AssignRealValuesToCellCenters(ThicknessArray,
        ScreenObject, Thickness[HufIndex]);
    end;
  end;
end;

procedure THufImporter.ImportHydrogeologicUnits;
var
  HufIndex: Integer;
  HGU: THydrogeologicUnit;
  HydrogeologicUnits: THydrogeologicUnits;
begin
  HydrogeologicUnits:= THydrogeologicUnits.Create(nil);
  try
    for HufIndex := 0 to NHUF - 1 do
    begin
      HGU := HydrogeologicUnits.Add as THydrogeologicUnit;
      HGU.HufName := FHydrogeologicUnits[HufIndex].HUFNAME;
      HGU.HorizontalAnisotropy := FHydrogeologicUnits[HufIndex].HGUHANI;
      HGU.VerticalAnisotropy := FHydrogeologicUnits[HufIndex].HGUVANI;
      if HGU.VerticalAnisotropy = 0 then
      begin
        HGU.VK_Method := vkVK;
      end
      else
      begin
        HGU.VK_Method := vkVANI;
      end;
      if FHydrogeologicUnits[HufIndex].PrintCode <> 0 then
      begin
        HGU.PrintFormat := FHydrogeologicUnits[HufIndex].PrintCode;
        HGU.Print[pprHK] := FHydrogeologicUnits[HufIndex].PrintHK <> 0;
        HGU.Print[pprHANI] := FHydrogeologicUnits[HufIndex].PrintHANI <> 0;
        HGU.Print[pprVK] := FHydrogeologicUnits[HufIndex].PrintVK <> 0;
        HGU.Print[pprSS] := FHydrogeologicUnits[HufIndex].PrintSS <> 0;
        HGU.Print[pprSY] := FHydrogeologicUnits[HufIndex].PrintSY <> 0;
      end;
    end;
    CreateParameters(HydrogeologicUnits);
    FModel.HydrogeologicUnits := HydrogeologicUnits;
  finally
    HydrogeologicUnits.Free;
  end;
end;

procedure THufImporter.ImportWetDry;
begin
  ImportDataSet('WetDry', rsWetDry, FWetDryConst, FWetDry);
end;

procedure THufImporter.ImportDataSet1;
begin
  FModel.ModflowOptions.HDry := HDRY;
  HufPackage.SaveHeads := IOHUFHDS > 0;
  HufPackage.SaveFlows := IOHUFFLWS > 0;
end;

procedure THufImporter.ReadData(const ALabel: string);
var
  AnInt: integer;
begin
  inherited;
  if ALabel = 'IHUFCB, HDRY, NHUF, NPHUF, IOHUFHDS, IOHUFFLWS:' then
  begin
    ReadDataSet1;
  end
  else if ALabel = 'NP:' then
  begin
    Readln(FImporter.FFile, AnInt);
    Assert(AnInt = NPHUF);
  end
  else if ALabel = '(LTHUF(K),K=1,NLAY):' then
  begin
    ReadDataSet2;
  end
  else if ALabel = '(LAYWT(K),K=1,NLAY):' then
  begin
    ReadDataSet3;
  end
  else if ALabel = 'WETFCT,IWETIT,IHDWET:' then
  begin
    ReadDataSet4;
  end
  else if ALabel = StrVariable2DRealArrayForLayer then
  begin
    ReadVariableWetDry;
  end
  else if ALabel = StrConstant2DRealArrayForLayer then
  begin
    ReadConstantWetDry;
  end
  else if ALabel = 'HGUNAM(M):' then
  begin
    ReadDataSet6;
  end
  else if ALabel = StrVariable2DRealArray then
  begin
    ReadVariableHguGeometry
  end
  else if ALabel = StrConstant2DRealArray then
  begin
    ReadConstantHguGeometry;
  end
  else if ALabel = 'HUF ITEM 9:' then
  begin
    ReadDataSet9;
  end
  else if ALabel = 'PARNAM:' then
  begin
    ReadArrayParameter;
  end
  else if ALabel = 'HUF ITEM 12:' then
  begin
    ReadDataSet12;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure THufImporter.ReadDataSet12;
var
  HufIndex: Integer;
  ALine: string;
  HGUNAM: string;
begin
  for HufIndex := 0 to NHUF - 1 do
  begin
    ReadLn(FImporter.FFile, ALine);
    Assert(Trim(ALine) = 'HGUNAM(NU):');
    ReadLn(FImporter.FFile, ALine);
    HGUNAM := Trim(ALine);
    Assert(SameText(FHydrogeologicUnits[HufIndex].HUFNAME, HGUNAM));
    ReadLn(FImporter.FFile, ALine);
    Assert(Trim(ALine) = '(IHGUFLG(I,NU),I=1,5):');
    Readln(FImporter.FFile,
      FHydrogeologicUnits[HufIndex].PrintHK,
      FHydrogeologicUnits[HufIndex].PrintHANI,
      FHydrogeologicUnits[HufIndex].PrintVK,
      FHydrogeologicUnits[HufIndex].PrintSS,
      FHydrogeologicUnits[HufIndex].PrintSY);
    if FHydrogeologicUnits[HufIndex].PrintHK <> 0 then
    begin
      FHydrogeologicUnits[HufIndex].PrintCode :=
        FHydrogeologicUnits[HufIndex].PrintHK;
    end
    else if FHydrogeologicUnits[HufIndex].PrintHANI <> 0 then
    begin
      FHydrogeologicUnits[HufIndex].PrintCode :=
        FHydrogeologicUnits[HufIndex].PrintHANI;
    end
    else if FHydrogeologicUnits[HufIndex].PrintVK <> 0 then
    begin
      FHydrogeologicUnits[HufIndex].PrintCode :=
        FHydrogeologicUnits[HufIndex].PrintVK;
    end
    else if FHydrogeologicUnits[HufIndex].PrintSS <> 0 then
    begin
      FHydrogeologicUnits[HufIndex].PrintCode :=
        FHydrogeologicUnits[HufIndex].PrintSS;
    end
    else if FHydrogeologicUnits[HufIndex].PrintSY <> 0 then
    begin
      FHydrogeologicUnits[HufIndex].PrintCode :=
        FHydrogeologicUnits[HufIndex].PrintSY;
    end;
  end;
end;

procedure THufImporter.ReadDataSet9;
var
  HufIndex: Integer;
  ALine: string;
  HGUNAM: string;
  HGUHANI: double;
  HGUVANI: double;
  InnerHufIndex: Integer;
begin
  for HufIndex := 0 to NHUF - 1 do
  begin
    ReadLn(FImporter.FFile, ALine);
    Assert(Trim(ALine) = 'HGUNAM:');
    ReadLn(FImporter.FFile, ALine);
    HGUNAM := Trim(ALine);
    Assert(SameText(FHydrogeologicUnits[HufIndex].HUFNAME, HGUNAM)
      or SameText('ALL', HGUNAM));
    ReadLn(FImporter.FFile, ALine);
    Assert(Trim(ALine) = 'HGUHANI, HGUVANI:');
    Readln(FImporter.FFile, HGUHANI, HGUVANI);
    if SameText('ALL', HGUNAM) then
    begin
      for InnerHufIndex := 0 to NHUF - 1 do
      begin
        FHydrogeologicUnits[InnerHufIndex].HGUHANI := HGUHANI;
        FHydrogeologicUnits[InnerHufIndex].HGUVANI := HGUVANI;
      end;
      break;
    end;
    FHydrogeologicUnits[HufIndex].HGUHANI := HGUHANI;
    FHydrogeologicUnits[HufIndex].HGUVANI := HGUVANI;
  end;
end;

function THufImporter.ReadInstance: boolean;
begin
  result := False;
end;

procedure THufImporter.ReadConstantHguGeometry;
var
  Index: Integer;
  Value: Double;
  ALine: string;
begin
  Assert(FNextHufIndexIndex >= 0);
  ReadLn(FImporter.FFile, ALine);
  ALine := Trim(ALine);
  Readln(FImporter.FFile, Value);
  if Pos(StrTOPELEVATN, ALine) = 1 then
  begin
    if FConstantTopElevations = nil then
    begin
      SetLength(FConstantTopElevations, NHUF);
      for Index := 0 to NHUF - 1 do
      begin
        FConstantTopElevations[Index].IsConstant := False;
      end;
    end;
    FConstantTopElevations[FNextHufIndexIndex-1].IsConstant := True;
    FConstantTopElevations[FNextHufIndexIndex-1].RealValue := Value;
  end
  else if Pos(StrTHICKNESS, ALine) = 1 then
  begin
    if FConstantThicknesses = nil then
    begin
      SetLength(FConstantThicknesses, NHUF);
      for Index := 0 to NHUF - 1 do
      begin
        FConstantThicknesses[Index].IsConstant := False;
      end;
    end;
    FConstantThicknesses[FNextHufIndexIndex-1].IsConstant := True;
    FConstantThicknesses[FNextHufIndexIndex-1].RealValue := Value;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure THufImporter.ReadVariableHguGeometry;
var
  HUFNAME: string;
  ID: string;
begin
  Assert(FNextHufIndexIndex >= 0);
  ReadLn(FImporter.FFile, ID);
  ID := Trim(ID);
  if Pos(StrTOPELEVATN, ID) = 1 then
  begin
    HUFNAME := Trim(Copy(ID, Length(StrTOPELEVATN) + 1, MAXINT));
    Assert(HUFNAME = FHydrogeologicUnits[FNextHufIndexIndex-1].HUFNAME);
    IsConstTopHuf := False;
    if Top = nil then
    begin
      SetLength(Top, NHUF);
    end;
    SetLength(TOP[FNextHufIndexIndex-1], FGrid.RowCount, FGrid.ColumnCount);
    Read2DRealArray(TOP[FNextHufIndexIndex-1]);
  end
  else if Pos(StrTHICKNESS, ID) = 1 then
  begin
    HUFNAME := Trim(Copy(ID, Length(StrTHICKNESS) + 1, MAXINT));
    Assert(HUFNAME = FHydrogeologicUnits[FNextHufIndexIndex-1].HUFNAME);
    IsConstThicknessHuf := False;
    if Thickness = nil then
    begin
      SetLength(Thickness, NHUF);
    end;
    SetLength(Thickness[FNextHufIndexIndex-1],
      FGrid.RowCount, FGrid.ColumnCount);
    Read2DRealArray(Thickness[FNextHufIndexIndex-1]);
  end
  else
  begin
    Assert(False);
  end;
end;

procedure THufImporter.ReadDataSet6;
var
  HUFNAME: string;
begin
  Readln(FImporter.FFile, HUFNAME);
  FHydrogeologicUnits[FNextHufIndexIndex].HUFNAME := Trim(HUFNAME);
  Inc(FNextHufIndexIndex);
end;

procedure THufImporter.ReadConstantWetDry;
var
  Value: Double;
  Layer: Integer;
  ID: string;
  ConstArray: TRealConstantRecordArray;
  Index: Integer;
begin
  ReadLn(FImporter.FFile, ID);
  ID := Trim(ID);
  ReadLn(FImporter.FFile, Layer);
  Readln(FImporter.FFile, Value);
  Dec(Layer);
  if ID = StrWetDry then
  begin
    if FWetDryConst = nil then
    begin
      SetLength(FWetDryConst, FModel.LayerStructure.ModflowLayerCount);
      InitializeConstArray(FWetDryConst);
      for Index := 0 to Length(FWetDryConst) - 1 do
      begin
        FWetDryConst[Index].IsConstant := True;
      end;
    end;
    ConstArray := FWetDryConst;
  end
  else
  begin
    Assert(False);
  end;
  ConstArray[Layer].IsConstant := True;
  ConstArray[Layer].RealValue := Value;
end;

procedure THufImporter.ReadVariableWetDry;
var
  ThreeDArray: T3DDoubleArray;
  Layer: Integer;
  ID: string;
  Index: Integer;
begin
  //    ReadDataSets10to16Variable;
  ReadLn(FImporter.FFile, ID);
  ID := Trim(ID);
  ReadLn(FImporter.FFile, Layer);
  Dec(Layer);
  if ID = StrWetDry then
  begin
    if FWetDryConst = nil then
    begin
      SetLength(FWetDryConst, FModel.LayerStructure.ModflowLayerCount);
      InitializeConstArray(FWetDryConst);
      for Index := 0 to Length(FWetDryConst) - 1 do
      begin
        FWetDryConst[Index].IsConstant := True;
      end;
    end;
    if FWetDry = nil then
    begin
      SetLength(FWetDry, FModel.LayerStructure.ModflowLayerCount);
    end;
    ThreeDArray := FWetDry;
  end
  else
  begin
    Assert(False);
  end;
  if ThreeDArray[Layer] = nil then
  begin
    SetLength(ThreeDArray[Layer], FGrid.RowCount, FGrid.ColumnCount);
  end;
  Read2DRealArray(ThreeDArray[Layer]);
  FWetDryConst[Layer].IsConstant := False;
end;

procedure THufImporter.ReleaseMemory;
begin
  SetLength(FHydrogeologicUnits, 0);
  SetLength(LTHUF, 0);
  SetLength(LAYWT, 0);
  SetLength(Top, 0, 0 , 0);
  SetLength(Thickness, 0, 0 , 0);
  SetLength(FWetDry, 0, 0 , 0);
  SetLength(FConstantTopElevations, 0);
  SetLength(FConstantThicknesses, 0);
  SetLength(FWetDryConst, 0);
end;

procedure THufImporter.ReadDataSet4;
begin
  Read(FImporter.FFile, WETFCT);
  Read(FImporter.FFile, IWETIT);
  Read(FImporter.FFile, IHDWET);
  Readln(FImporter.FFile);
end;

procedure THufImporter.ReadDataSet3;
var
  Index: Integer;
  NLAY: Integer;
begin
  NLAY := FModel.LayerStructure.ModflowLayerCount;
  SetLength(LAYWT, NLAY);
  for Index := 0 to NLAY - 1 do
  begin
    Read(FImporter.FFile, LAYWT[Index]);
  end;
  Readln(FImporter.FFile);
end;

procedure THufImporter.ReadDataSet2;
var
  Index: Integer;
  NLAY: Integer;
begin
  NLAY := FModel.LayerStructure.ModflowLayerCount;
  SetLength(LTHUF, NLAY);
  for Index := 0 to NLAY - 1 do
  begin
    Read(FImporter.FFile, LTHUF[Index]);
  end;
  Readln(FImporter.FFile);
  FReadLthuf := True;
end;

procedure THufImporter.ReadDataSet1;
var
  IHUFCB: integer;
begin
  FIsSelected := True;
  Read(FImporter.FFile, IHUFCB);
  Read(FImporter.FFile, HDRY);
  Read(FImporter.FFile, NHUF);
  Read(FImporter.FFile, NPHUF);
  Read(FImporter.FFile, IOHUFHDS);
  Read(FImporter.FFile, IOHUFFLWS);
  Readln(FImporter.FFile);
  SetLength(FParameters, NPHUF);
  FNextParameterIndex := 0;
  SetLength(FHydrogeologicUnits, NHUF);
  FNextHufIndexIndex := 0;
end;

{ TKdepImporter }

constructor TKdepImporter.Create(Importer: TModflow2005Importer;
  HufImporter: THufImporter);
begin
  inherited Create(Importer, 'KDEP:');
  FHufImporter := HufImporter;
  FHufImporter.FKdepImporter := self;
end;

procedure TKdepImporter.CreateParameters;
var
  Param: THufParameter;
  ParamIndex: Integer;
  HufParameters: THufModflowParameters;
  HydrogeologicUnits: THydrogeologicUnits;
  UsedParam: THufUsedParameter;
  HGU: THydrogeologicUnit;
  ClusterIndex: Integer;
  Clusters: TClusterRecordArray;
begin
  HydrogeologicUnits := THydrogeologicUnits.Create(nil);
  HufParameters := THufModflowParameters.Create(nil);
  try
    HufParameters.Assign(FModel.HufParameters);
    HydrogeologicUnits.Assign(FModel.HydrogeologicUnits);
    for ParamIndex := 0 to Length(FParameters) - 1 do
    begin
      Param := HufParameters.Add as THufParameter;
      Param.ParameterType := ptHUF_KDEP;
      Clusters := FParameters[ParamIndex].Instances[0].Clusters;
      Param.ParameterName := FParameters[ParamIndex].PARNAM;
      Param.Value := FParameters[ParamIndex].Parval;
      for ClusterIndex := 0 to Length(Clusters) - 1 do
      begin
        HGU := HydrogeologicUnits[Clusters[ClusterIndex].Layer - 1];
        UsedParam := HGU.HufUsedParameters.Add as THufUsedParameter;
        UsedParam.ParameterName := Param.ParameterName;
        UsedParam.UseMultiplier :=
          not SameText(Clusters[ClusterIndex].MultiplierName, 'NONE');
        UsedParam.UseZone :=
          not SameText(Clusters[ClusterIndex].ZoneName, 'ALL');
      end;
    end;
    FModel.HufParameters := HufParameters;
    FModel.HydrogeologicUnits := HydrogeologicUnits;
  finally
    HufParameters.Free;
    HydrogeologicUnits.Free;
  end;
end;

procedure TKdepImporter.ImportReferenceLayer;
var
  RefDataArray: TDataArray;
  ScreenObject: TScreenObject;
  Interpolator: TNearestPoint2DInterpolator;
begin
  if FHufPackage.ReferenceChoice = hrcReferenceLayer then
  begin
    RefDataArray := FModel.GetDataSetByName(StrHufReferenceSurface);
    Assert(RefDataArray <> nil);
    if FIsConstantGroundSurface then
    begin
      RefDataArray.Formula := FloatToStr(FConstantGroundSurface);
    end
    else
    begin
      CreateOrRetrieveCellCenterScreenObject(ScreenObject);
      Interpolator := TNearestPoint2DInterpolator.Create(nil);
      try
        RefDataArray.TwoDInterpolator := Interpolator;
      finally
        Interpolator.Free;
      end;
      AssignRealValuesToCellCenters(RefDataArray, ScreenObject, FGroundSurface);
    end;
  end;
end;

procedure TKdepImporter.ImportReferenceChoice;
begin
  if IFKDEP = 0 then
  begin
    FHufPackage.ReferenceChoice := hrcModelTop;
  end
  else
  begin
    FHufPackage.ReferenceChoice := hrcReferenceLayer;
  end;
end;

procedure TKdepImporter.ReadConstantGroundSurface;
var
  ALine: string;
begin
  FIsConstantGroundSurface := True;
  ReadLn(FImporter.FFile, ALine);
  ALine := Trim(ALine);
  Assert(ALine = 'GROUND SURFACE');
  Readln(FImporter.FFile, FConstantGroundSurface);
end;

procedure TKdepImporter.ReadVariableGroundSurface;
var
  ID: string;
begin
  FIsConstantGroundSurface := False;
  ReadLn(FImporter.FFile, ID);
  ID := Trim(ID);
  Assert(ID = 'GROUND SURFACE');
  SetLength(FGroundSurface, FGrid.RowCount, FGrid.ColumnCount);
  Read2DRealArray(FGroundSurface);
end;

procedure TKdepImporter.ReadDataSet1;
begin
  Read(FImporter.FFile, NPKDEP);
  Read(FImporter.FFile, IFKDEP);
  Readln(FImporter.FFile);
  SetLength(FParameters, NPKDEP);
  FNextParameterIndex := 0;
  FHufImporter.FReadKdep := False;
  FHufImporter.FComments.AddStrings(FComments);
end;

function TKdepImporter.ReadInstance: boolean;
begin
  result := False;
end;

procedure TKdepImporter.HandlePackage;
begin
  if FHufImporter.FReadKdep then
  begin
    if not FHufImporter.ImportedPackage then
    begin
      FHufImporter.HandlePackage;
    end;
    if FHufImporter.ImportedPackage and not ImportedPackage then
    begin
      inherited;
      FHufPackage := FModel.ModflowPackages.HufPackage;
      ImportReferenceChoice;
      CreateParameters;
      AssignHufParametersZoneAndMultiplier;
      FModel.CreateInitialDataSets;
      ImportReferenceLayer;
    end;
  end;
end;

procedure TKdepImporter.ReadData(const ALabel: string);
var
  AnInt: integer;
begin
  inherited;
  if ALabel = 'NPKDEP, IFKDEP:' then
  begin
    ReadDataSet1;
  end
  else if ALabel = 'NP:' then
  begin
    Readln(FImporter.FFile, AnInt);
    Assert(AnInt = NPKDEP);
  end
  else if ALabel = StrVariable2DRealArray then
  begin
    ReadVariableGroundSurface;
  end
  else if ALabel = StrConstant2DRealArray then
  begin
    ReadConstantGroundSurface;
  end
  else if ALabel = 'PARNAM:' then
  begin
    ReadArrayParameter;
    FHufImporter.FReadKdep := True;
  end
  else
  begin
    Assert(False);
  end;
end;

{ TCustomHufImporter }

procedure TCustomHufImporter.AssignHufParametersZoneAndMultiplier;
var
  HGU: THydrogeologicUnit;
  ParamIndex: Integer;
  UsedParam: THufUsedParameter;
  MultDataArray: TDataArray;
  ZoneDataArray: TDataArray;
  PIndex: Integer;
  Clusters: TClusterRecordArray;
  ClusterIndex: Integer;
  Cluster: TClusterRecord;
  MultFunction: string;
  ZoneFunction: string;
  ZoneIndex: Integer;
  HufIndex: Integer;
begin
  for HufIndex := 0 to FModel.HydrogeologicUnits.Count - 1 do
  begin
    HGU := FModel.HydrogeologicUnits[HufIndex];
    for ParamIndex := 0 to HGU.HufUsedParameters.Count - 1 do
    begin
      UsedParam := HGU.HufUsedParameters[ParamIndex];
      MultDataArray := nil;
      if UsedParam.UseMultiplier then
      begin
        MultDataArray := FModel.GetDataSetByName(
          UsedParam.MultiplierDataSetName);
      end;
      ZoneDataArray := nil;
      if UsedParam.UseZone then
      begin
        ZoneDataArray := FModel.GetDataSetByName(UsedParam.ZoneDataSetName);
      end;
      if (MultDataArray = nil) and (ZoneDataArray = nil) then
      begin
        Continue;
      end;
      for PIndex := 0 to Length(FParameters) - 1 do
      begin
        if SameText(FParameters[PIndex].PARNAM, UsedParam.ParameterName) then
        begin
          Clusters := FParameters[PIndex].Instances[0].Clusters;
          for ClusterIndex := 0 to Length(Clusters) - 1 do
          begin
            if Clusters[ClusterIndex].Layer - 1 = HufIndex then
            begin
              Cluster := Clusters[ClusterIndex];
              if SameText(Cluster.MultiplierName, StrNone) then
              begin
                MultFunction := '1';
              end
              else
              begin
                MultFunction := Cluster.MultiplierName;
              end;
              if SameText(Cluster.ZoneName, StrAll) then
              begin
                ZoneFunction := 'True';
              end
              else
              begin
                ZoneFunction := '';
                for ZoneIndex := 0 to Length(Cluster.ZoneValues) - 1 do
                begin
                  ZoneFunction := ZoneFunction + '(' + Cluster.ZoneName
                    + ' = ' + IntToStr(Cluster.ZoneValues[ZoneIndex]) + ')';
                  if ZoneIndex < Length(Cluster.ZoneValues) - 1 then
                  begin
                    ZoneFunction := ZoneFunction + ' or ';
                  end;
                end;
              end;
              if MultDataArray <> nil then
              begin
                MultDataArray.Formula := MultFunction;
              end;
              if ZoneDataArray <> nil then
              begin
                ZoneDataArray.Formula := ZoneFunction;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

{ TLvdaImporter }

constructor TLvdaImporter.Create(Importer: TModflow2005Importer;
  HufImporter: THufImporter);
begin
  inherited Create(Importer, 'LVDA:');
  FHufImporter := HufImporter;
  FHufImporter.FLvdaImporter := self;
end;

procedure TLvdaImporter.ReadDataSet1;
begin
  Read(FImporter.FFile, NPLVDA);
  Readln(FImporter.FFile);
  SetLength(FParameters, NPLVDA);
  FNextParameterIndex := 0;
  FHufImporter.FReadLvda := False;
  FHufImporter.FComments.AddStrings(FComments);
end;

function TLvdaImporter.ReadInstance: boolean;
begin
  result := False;
end;

procedure TLvdaImporter.CreateParameters;
var
  ClusterIndex: Integer;
  Cluster: TClusterRecord;
  MultName: string;
  MultDataArray: TDataArray;
  ZoneName: string;
  ZoneDataArray: TDataArray;
  MultFunctionList: TStringList;
  ZoneFunctionList: TStringList;
  LayerIndex: Integer;
  ZoneFunction: string;
  ZoneIndex: Integer;
  MultFunction: string;
  MultIndex: Integer;
  Index: Integer;
  Param: TModflowSteadyParameter;
  Instance: TInstanceRecord;
  MultUsed: Boolean;
  ZoneUsed: Boolean;
  IntList: TIntegerList;
begin
  for Index := 0 to Length(FParameters) - 1 do
  begin
    Param := FModel.ModflowSteadyParameters.Add as TModflowSteadyParameter;
    Param.ParameterName := FParameters[Index].PARNAM;
    Param.Value := FParameters[Index].Parval;
    Assert(FParameters[Index].PARTYP = 'LVDA');
    Param.ParameterType := ptHUF_LVDA;
    Assert(Length(FParameters[Index].Instances) = 1);
    Instance := FParameters[Index].Instances[0];
    MultUsed := False;
    ZoneUsed := False;
    IntList := TIntegerList.Create;
    try
      IntList.Sorted := True;
      for ClusterIndex := 0 to Length(Instance.Clusters) - 1 do
      begin
        Cluster := Instance.Clusters[ClusterIndex];
        IntList.AddUnique(Cluster.Layer);
        if not SameText(Cluster.MultiplierName, StrNone) then
        begin
          MultUsed := True;
        end;
        if not SameText(Cluster.ZoneName, StrAll) then
        begin
          ZoneUsed := True;
        end;
      end;
      Param.UseMultiplier := MultUsed or
        (IntList.Count < FModel.LayerStructure.ModflowLayerCount);
      if Param.UseMultiplier then
      begin
        MultName := Param.MultiplierName;
        MultDataArray := FModel.GetDataSetByName(MultName);
      end
      else
      begin
        MultDataArray := nil;
      end;
      Param.UseZone := ZoneUsed or
        (IntList.Count < FModel.LayerStructure.ModflowLayerCount);
      if Param.UseZone then
      begin
        ZoneName := Param.ZoneName;
        ZoneDataArray := FModel.GetDataSetByName(ZoneName);
      end
      else
      begin
        ZoneDataArray := nil;
      end;
    finally
      IntList.Free;
    end;
    MultFunctionList := TStringList.Create;
    ZoneFunctionList := TStringList.Create;
    try
      for LayerIndex := 0 to FGrid.LayerCount - 1 do
      begin
        MultFunctionList.Add('0');
        ZoneFunctionList.Add('False');
      end;
      for ClusterIndex := 0 to Length(Instance.Clusters) - 1 do
      begin
        Cluster := Instance.Clusters[ClusterIndex];
        LayerIndex := FModel.LayerStructure.
          ModflowLayerToDataSetLayer(Cluster.Layer);
        if Param.ParameterType = ptLPF_VKCB then
        begin
          Inc(LayerIndex);
        end;
        if SameText(Cluster.MultiplierName, StrNone) then
        begin
          MultFunctionList[LayerIndex] := '1';
        end
        else
        begin
          MultFunctionList[LayerIndex] := Cluster.MultiplierName;
        end;
        if SameText(Cluster.ZoneName, StrAll) then
        begin
          ZoneFunctionList[LayerIndex] := 'True';
        end
        else
        begin
          ZoneFunction := '';
          for ZoneIndex := 0 to Length(Cluster.ZoneValues) - 1 do
          begin
            ZoneFunction := ZoneFunction + '(' + Cluster.ZoneName
              + ' = ' + IntToStr(Cluster.ZoneValues[ZoneIndex]) + ')';
            if ZoneIndex < Length(Cluster.ZoneValues) - 1 then
            begin
              ZoneFunction := ZoneFunction + ' or ';
            end;
          end;
          ZoneFunctionList[LayerIndex] := ZoneFunction;
        end;
      end;
      if MultDataArray <> nil then
      begin
        MultFunction := 'CaseR(Layer, ';
        for MultIndex := 0 to MultFunctionList.Count - 1 do
        begin
          MultFunction := MultFunction + MultFunctionList[MultIndex];
          if MultIndex < MultFunctionList.Count - 1 then
          begin
            MultFunction := MultFunction + ', ';
          end;
        end;
        MultFunction := MultFunction + ')';
        MultDataArray.Formula := MultFunction;
      end;
      if ZoneDataArray <> nil then
      begin
        if ZoneFunctionList.Count = 1 then
        begin
          ZoneFunction :=ZoneFunctionList[0];
        end
        else
        begin
          ZoneFunction := 'CaseB(Layer, ';
          for ZoneIndex := 0 to ZoneFunctionList.Count - 1 do
          begin
            ZoneFunction := ZoneFunction + ZoneFunctionList[ZoneIndex];
            if ZoneIndex < ZoneFunctionList.Count - 1 then
            begin
              ZoneFunction := ZoneFunction + ', ';
            end;
          end;
          ZoneFunction := ZoneFunction + ')';
        end;
        ZoneDataArray.Formula := ZoneFunction;
      end;
    finally
      MultFunctionList.Free;
      ZoneFunctionList.Free;
    end;
  end;
end;

procedure TLvdaImporter.HandlePackage;
begin
  if FHufImporter.FReadLvda then
  begin
    if not FHufImporter.ImportedPackage then
    begin
      FHufImporter.HandlePackage;
    end;
    if FHufImporter.ImportedPackage and not ImportedPackage then
    begin
      inherited;
      CreateParameters;
    end;
  end;
end;

procedure TLvdaImporter.ReadData(const ALabel: string);
var
  AnInt: integer;
begin
  inherited;
  if ALabel = 'NPLVDA:' then
  begin
    ReadDataSet1;
  end
  else if ALabel = 'NP:' then
  begin
    Readln(FImporter.FFile, AnInt);
    Assert(AnInt = NPLVDA);
  end
  else if ALabel = 'PARNAM:' then
  begin
    ReadArrayParameter;
    FHufImporter.FReadLvda := True;
  end
  else
  begin
    Assert(False);
  end;
end;

{ TCustomFlowObservationImporter }

constructor TCustomFlowObservationImporter.Create(
  Importer: TModflow2005Importer; const PackageIdentifier: string);
begin
  inherited;
  FObservations:= TObservations.Create;
  FFlowObsGroups := TFluxObservationGroups.Create(nil);
end;

destructor TCustomFlowObservationImporter.Destroy;
begin
  FObservations.Free;
  FFlowObsGroups.Free;
  inherited;
end;

procedure TCustomFlowObservationImporter.CreateObservationGroups;
var
  ObsTime: TObservationTime;
  TimeIndex: Integer;
  Group: TFluxObservationGroup;
  ObsGroup: TObservationGroup;
  GroupIndex: Integer;
  RefStressPeriod: TModflowStressPeriod;
  FlowObs: TFluxObservation;
begin
  if FFlowObsGroups.Count > 0 then
  begin
    Exit;
  end;
  for GroupIndex := 0 to FObservations.ArrayLength - 1 do
  begin
    ObsGroup := FObservations[GroupIndex];
    Group := FFlowObsGroups.Add;
    Group.ObservationName := FObsPrefix + IntToStr(GroupIndex + 1);
    ObsGroup.FGroup := Group;
    for TimeIndex := 0 to ObsGroup.FTimes.ArrayLength - 1 do
    begin
      ObsTime := ObsGroup.FTimes[TimeIndex];
      FlowObs := Group.ObservationTimes.Add;
      FlowObs.ObservedValue := ObsTime.FLWOBS;
      RefStressPeriod := FModel.ModflowStressPeriods.Items[ObsTime.IREFSP - 1];
      FlowObs.Time := RefStressPeriod.StartTime + ObsTime.TOFFSET * TOMULT;
    end;
  end;
end;

procedure TCustomFlowObservationImporter.ReadData(const ALabel: string);
begin
  inherited;
  if ALabel = FDataSet1Label then
  begin
    ReadDataSet1;
  end
  else if ALabel = FDataSet2Label then
  begin
    ReadDataSet2;
  end
  else if ALabel = FDataSet3Label then
  begin
    ReadDataSet3;
  end
  else if ALabel = FDataSet4Label then
  begin
    ReadDataSet4;
  end
  else if ALabel = FDataSet5Label then
  begin
    ReadDataSet5;
  end
  else
  begin
    ShowMessage(ALabel);
    Assert(False);
  end;
end;

procedure TCustomFlowObservationImporter.ReadDataSet5;
var
  CurrentLoc: TFlowObservationLocation;
begin
  Inc(FCurrentLocationIndex);
  CurrentLoc := FCurrentGroup.FCells[FCurrentLocationIndex];
  Readln(FImporter.FFile, CurrentLoc.LAYER, CurrentLoc.ROW,
    CurrentLoc.COLUMN, CurrentLoc.FACTOR);
  if NQCL < 0 then
  begin
    CurrentLoc.FACTOR := 1;
  end;
end;

procedure TCustomFlowObservationImporter.ReadDataSet4;
var
  CurrentObs: TObservationTime;
begin
  Inc(FCurrentTimeIndex);
  CurrentObs := FCurrentGroup.FTimes[FCurrentTimeIndex];
  Readln(FImporter.FFile, CurrentObs.OBSNAM);
  Readln(FImporter.FFile, CurrentObs.IREFSP,
    CurrentObs.TOFFSET, CurrentObs.FLWOBS);
  CurrentObs.OBSNAM := Trim(CurrentObs.OBSNAM);
end;

procedure TCustomFlowObservationImporter.ReadDataSet3;
var
  NQOB: Integer;
begin
  Readln(FImporter.FFile, NQOB, NQCL);
  Inc(FCurrentGroupIndex);
  FCurrentGroup := FObservations[FCurrentGroupIndex];
  FCurrentGroup.FTimes.ArrayLength := NQOB;
  FCurrentGroup.FCells.ArrayLength := Abs(NQCL);
  FCurrentTimeIndex := -1;
  FCurrentLocationIndex := -1;
end;

procedure TCustomFlowObservationImporter.ReadDataSet2;
begin
  Readln(FImporter.FFile, TOMULT);
end;

procedure TCustomFlowObservationImporter.ReadDataSet1;
var
  NQC: integer;
  NQT: integer;
begin
  Readln(FImporter.FFile, NQ, NQC, NQT, IUOBSV);
  FObservations.ArrayLength := NQ;
  FCurrentGroupIndex := -1;
  FIsActive := True;
end;

{ TFlowObsLocArray }

function TFlowObsLocArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TFlowObservationLocation;
end;

function TFlowObsLocArray.GetItem(Index: integer): TFlowObservationLocation;
begin
  result := Objects[Index] as TFlowObservationLocation;
end;

{ TObsTimeArray }

function TObsTimeArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TObservationTime;
end;

function TObsTimeArray.GetItem(Index: integer): TObservationTime;
begin
  result := Objects[Index] as TObservationTime
end;

{ TObservations }

function TObservations.ArrayMemberClass: TArrayMemberClass;
begin
  result := TObservationGroup;
end;

function TObservations.GetItem(Index: integer): TObservationGroup;
begin
  result := Objects[Index] as TObservationGroup;
end;

{ TDrnObsImporter }

constructor TDrnObsImporter.Create(Importer: TModflow2005Importer;
      DrnImporter: TDrnImporter);
begin
  inherited Create(Importer, 'DROB');
  FDrnImporter := DrnImporter;
  FDrnImporter.FObsImporter := self;
  FDataSet1Label := 'NQDR, NQCDR, NQTDR, IUDROBSV:';
  FDataSet2Label := 'TOMULTDR:';
  FDataSet3Label := 'NQOBDR(IQ), NQCLDR(IQ):';
  FDataSet4Label := 'OBSNAM(J),IREFSP,TOFFSET,FLWOBS(J):';
  FDataSet5Label := 'Layer Row Column Factor:';
  FObsPrefix := 'DROB';
end;

procedure TDrnObsImporter.HandlePackage;
begin
  if FDrnImporter.ImportedPackage then
  begin
    inherited;
    FModel.ModflowPackages.DrobPackage.IsSelected := FIsActive;
    FModel.DrainObservations := FFlowObsGroups;
    frmGoPhast.EnableManageObservations;
  end;
end;

{ TObservationGroup }

constructor TObservationGroup.Create;
begin
  inherited;
  FTimes:= TObsTimeArray.Create;
  FCells:= TFlowObsLocArray.Create;
end;

destructor TObservationGroup.Destroy;
begin
  FCells.Free;
  FTimes.Free;
  inherited;
end;

{ TLocation }

constructor TLocation.Create;
begin
  inherited;
  FObservationGroups := TList.Create;
  FObservationCells := TList.Create;
end;

destructor TLocation.Destroy;
begin
  FObservationCells.Free;
  FObservationGroups.Free;
  inherited;
end;

function TLocation.SameObservations(ALocation: TLocation): boolean;
var
  Index: Integer;
begin
  result := FObservationGroups.Count = ALocation.FObservationGroups.Count;
  if result then
  begin
    for Index := 0 to FObservationGroups.Count - 1 do
    begin
      result := FObservationGroups[Index] = ALocation.FObservationGroups[Index];
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

{ TGhbObsImporter }

constructor TGhbObsImporter.Create(Importer: TModflow2005Importer;
  GhbImporter: TGhbImporter);
begin
  inherited Create(Importer, 'GBOB');
  FGhbImporter := GhbImporter;
  FGhbImporter.FObsImporter := self;
  FDataSet1Label := 'NQGB, NQCGB, NQTGB, IUGBOBSV:';
  FDataSet2Label := 'TOMULTGB:';
  FDataSet3Label := 'NQOBGB(IQ), NQCLGB(IQ):';
  FDataSet4Label := 'OBSNAM(J),IREFSP,TOFFSET,FLWOBS(J):';
  FDataSet5Label := 'Layer Row Column Factor:';
  FObsPrefix := 'GBOB';
end;

procedure TGhbObsImporter.HandlePackage;
begin
  if FGhbImporter.ImportedPackage then
  begin
    inherited;
    FModel.ModflowPackages.GbobPackage.IsSelected := FIsActive;
    FModel.GhbObservations := FFlowObsGroups;
    frmGoPhast.EnableManageObservations;
  end;
end;

{ TRivObsImporter }

constructor TRivObsImporter.Create(Importer: TModflow2005Importer;
  RivImporter: TRivImporter);
begin
  inherited Create(Importer, 'RVOB');
  FRivImporter := RivImporter;
  FRivImporter.FObsImporter := self;
  FDataSet1Label := 'NQRV, NQCRV, NQTRV, IURVOBSV:';
  FDataSet2Label := 'TOMULTRV:';
  FDataSet3Label := 'NQOBRV(IQ), NQCLRV(IQ):';
  FDataSet4Label := 'OBSNAM(J),IREFSP,TOFFSET,FLWOBS(J):';
  FDataSet5Label := 'Layer Row Column Factor:';
  FObsPrefix := 'RVOB';
end;

procedure TRivObsImporter.HandlePackage;
begin
  if FRivImporter.ImportedPackage then
  begin
    inherited;
    FModel.ModflowPackages.RvobPackage.IsSelected := FIsActive;
    FModel.RiverObservations := FFlowObsGroups;
    frmGoPhast.EnableManageObservations;
  end;
end;

{ TChdObsImporter }

constructor TChdObsImporter.Create(Importer: TModflow2005Importer;
  ChdImporter: TChdImporter; BasImporter: TBasImporter);
begin
  inherited Create(Importer, 'CHOB');
  FChdImporter := ChdImporter;
  FChdImporter.FObsImporter := self;
  FBasImporter := BasImporter;
  BasImporter.FObsImporter := self;
  FDataSet1Label := 'NQCH, NQCCH, NQTCH, IUCHOBSV:';
  FDataSet2Label := 'TOMULTCH:';
  FDataSet3Label := 'NQOBCH(IQ), NQCLCH(IQ):';
  FDataSet4Label := 'OBSNAM(N), IREFSP, TOFFSET, FLWOBS(N):';
  FDataSet5Label := 'Layer Row Column Factor:';
  FObsPrefix := 'CHOB';
end;

procedure TChdObsImporter.HandlePackage;
begin
  if not FImportedPackage and
    (FChdImporter.ImportedPackage or not FChdImporter.FReadData)
    and FBasImporter.ImportedPackage then
  begin
    CreateObservationGroups;
    FBasImporter.ImportSpecifiedHeads;
    inherited;
    FModel.ModflowPackages.ChobPackage.IsSelected := FIsActive;
    FModel.HeadFluxObservations := FFlowObsGroups;
    frmGoPhast.EnableManageObservations;
  end;
end;

{ TBasChdObjects }

constructor TBasChdObjects.Create;
begin
  FList := TObjectList.Create;
end;

destructor TBasChdObjects.Destroy;
begin
  FList.Free;
  inherited;
end;

{ TPvalImporter }

constructor TPvalImporter.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'PVAL');
  FPvalParams:= TPvalParamArray.Create;
  FCurrentParam := -1;
end;

destructor TPvalImporter.Destroy;
begin
  FPvalParams.Free;
  inherited;
end;

procedure TPvalImporter.HandlePackage;
var
  ReadAll: boolean;
  Index: Integer;
  AParam: TModflowParameter;
begin
  if FImportedPackage then
  begin
    Exit;
  end;
  ReadAll := True;
  for Index := 0 to FPvalParams.ArrayLength - 1 do
  begin
    AParam := FModel.ModflowSteadyParameters.
      GetParamByName(FPvalParams[Index].PARNAM);
    if AParam = nil then
    begin
      AParam := FModel.ModflowTransientParameters.
        GetParamByName(FPvalParams[Index].PARNAM);
    end;
    if AParam = nil then
    begin
      AParam := FModel.HufParameters.
        GetParamByName(FPvalParams[Index].PARNAM);
    end;
    if AParam = nil then
    begin
      ReadAll := False;
    end
    else
    begin
      AParam.Value := FPvalParams[Index].Value;
    end;
  end;
  if ReadAll then
  begin
    inherited;
  end;

end;

procedure TPvalImporter.ReadData(const ALabel: string);
var
  NPVAL: integer;
begin
  inherited;
  if ALabel = 'NPVAL:' then
  begin
    Readln(FImporter.FFile, NPVAL);
    FPvalParams.ArrayLength := NPVAL;
  end
  else if ALabel = 'PARNAM(I),B(I):' then
  begin
    Inc(FCurrentParam);
    Readln(FImporter.FFile, FPvalParams[FCurrentParam].PARNAM);
    Readln(FImporter.FFile, FPvalParams[FCurrentParam].Value);
    FPvalParams[FCurrentParam].PARNAM :=
      Trim(FPvalParams[FCurrentParam].PARNAM);
  end;
end;

{ TPvalParamArray }

function TPvalParamArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TPvalParam;
end;

function TPvalParamArray.GetParam(Index: integer): TPvalParam;
begin
  result := Objects[Index] as TPvalParam
end;

{ TMnw2Importer }

procedure TMnw2Importer.AddStressPeriod(ITMP: integer);
var
  Index: Integer;
  AWell: TMnw2Well;
  StressPeriod: TMnw2WellStressPeriod;
begin
  Inc(FCurrentStressPeriod);
  for Index := 0 to FWells.ArrayLength - 1 do
  begin
    AWell := FWells[Index];
    AWell.FStressPeriods.ArrayLength := FCurrentStressPeriod+1;
    StressPeriod := AWell.FStressPeriods[FCurrentStressPeriod];
    if ITMP < 0 then
    begin
      StressPeriod.WellState := wsReuse;
    end
    else
    begin
      StressPeriod.WellState := wsInactive;
    end;
    StressPeriod.Hlim := AWell.Hlim;
    StressPeriod.QCUT := AWell.QCUT;
    StressPeriod.Qfrcmn := AWell.Qfrcmn;
    StressPeriod.Qfrcmx := AWell.Qfrcmx;
  end;
end;

constructor TMnw2Importer.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'MNW2');
  FWells:= TMnw2WellArray.Create;
  FCurrentStressPeriod := -1;
end;

destructor TMnw2Importer.Destroy;
begin
  FWells.Free;
  inherited;
end;

procedure TMnw2Importer.AssignScreens(AScreenObject: TScreenObject;
  AWell: TMnw2Well);
var
  FirstScreen: TMnw2WellScreen;
  VerticalScreen: TVerticalScreen;
  AScreen: TMnw2WellScreen;
  ScreenIndex: Integer;
  APoint3D: T3DRealPoint;
  APoint: TPoint2D;
  SpatialItem: TMnw2SpatialItem;
begin
  Assert(AWell.FScreens <> nil);
  AScreenObject.Capacity := 1;
  AScreenObject.ElevationCount := ecTwo;
  FirstScreen := AWell.FScreens[0];
  AScreenObject.HigherElevationFormula := FloatToStr(FirstScreen.Ztop);
  AScreenObject.LowerElevationFormula := FloatToStr(FirstScreen.Zbotm);
  APoint := FGrid.TwoDElementCenter(FirstScreen.IC - 1, FirstScreen.IR - 1);
  AScreenObject.AddPoint(APoint, True);
  APoint3D := FGrid.ThreeDElementCenter(FirstScreen.IC - 1,
    FirstScreen.IR - 1, 0);

  SpatialItem := nil;
  if AWell.LossType in [mltThiem, mltSkin, mltEquation] then
  begin
    if SpatialItem = nil then
    begin
      SpatialItem := AScreenObject.ModflowMnw2Boundary.Values.Add
        as TMnw2SpatialItem;
      SpatialItem.WellRadius := FloatToStr(FirstScreen.Rw);
    end;
  end;
  if AWell.LossType = mltSkin then
  begin
    if SpatialItem = nil then
    begin
      SpatialItem := AScreenObject.ModflowMnw2Boundary.Values.Add
        as TMnw2SpatialItem;
    end;
    SpatialItem.SkinRadius := FloatToStr(FirstScreen.Rskin);
    SpatialItem.SkinK := FloatToStr(FirstScreen.Kskin);
  end;
  if AWell.LossType = mltEquation then
  begin
    if SpatialItem = nil then
    begin
      SpatialItem := AScreenObject.ModflowMnw2Boundary.Values.Add
        as TMnw2SpatialItem;
    end;
    SpatialItem.B := FloatToStr(FirstScreen.B);
    SpatialItem.C := FloatToStr(FirstScreen.C);
    SpatialItem.P := FloatToStr(FirstScreen.P);
  end;
  if AWell.LossType = mtlSpecify then
  begin
    if SpatialItem = nil then
    begin
      SpatialItem := AScreenObject.ModflowMnw2Boundary.Values.Add
        as TMnw2SpatialItem;
    end;
    SpatialItem.CellToWellConductance := FloatToStr(FirstScreen.CWC);
  end;

  if AWell.FScreens.ArrayLength > 1 then
  begin
    AScreenObject.ModflowMnw2Boundary.VerticalScreens.Capacity :=
      AWell.FScreens.ArrayLength;
    for ScreenIndex := 0 to AWell.FScreens.ArrayLength - 1 do
    begin
      AScreen := AWell.FScreens[ScreenIndex];
      VerticalScreen := AScreenObject.ModflowMnw2Boundary.VerticalScreens.Add
        as TVerticalScreen;
      VerticalScreen.ZTop := AScreen.Ztop;
      VerticalScreen.ZBottom := AScreen.Zbotm;
      VerticalScreen.WellRadius := FloatToStr(AScreen.Rw);
      VerticalScreen.SkinRadius := FloatToStr(AScreen.Rskin);
      VerticalScreen.SkinK := FloatToStr(AScreen.Kskin);
      VerticalScreen.B := FloatToStr(AScreen.B);
      VerticalScreen.C := FloatToStr(AScreen.C);
      VerticalScreen.P := FloatToStr(AScreen.P);
      VerticalScreen.CellToWellConductance := FloatToStr(AScreen.CWC);
    end;
  end;
end;

procedure TMnw2Importer.AssignCells(AScreenObject: TScreenObject;
  AWell: TMnw2Well);
var
  ImportedValues: TValueArrayItem;
  AValue: Double;
  SpatialItem: TMnw2SpatialItem;
  ACell: TMnw2WellCell;
  CellIndex: Integer;
  ImportedCWCData: TDoubleArray;
  ImportedPData: TDoubleArray;
  ImportedCData: TDoubleArray;
  ImportedBData: TDoubleArray;
  ImportedKSkinData: TDoubleArray;
  ImportedRSkinData: TDoubleArray;
  ImportedRwData: TDoubleArray;
  ImportedElevations: TValueArrayStorage;
  APoint3D: T3DRealPoint;
  APoint: TPoint2D;
begin
  AScreenObject.Capacity := AWell.FCells.ArrayLength;
  ImportedElevations := nil;
  if AWell.FCells.ArrayLength > 1 then
  begin
    ImportedElevations := AScreenObject.ImportedSectionElevations;
    ImportedElevations.DataType := rdtDouble;
    ImportedElevations.Count := AWell.FCells.ArrayLength;
    AScreenObject.ElevationFormula := rsObjectImportedValuesR
      + '("' + StrImportedElevations + '")';
  end;
  SetLength(ImportedRwData, AWell.FCells.ArrayLength);
  SetLength(ImportedRSkinData, AWell.FCells.ArrayLength);
  SetLength(ImportedKSkinData, AWell.FCells.ArrayLength);
  SetLength(ImportedBData, AWell.FCells.ArrayLength);
  SetLength(ImportedCData, AWell.FCells.ArrayLength);
  SetLength(ImportedPData, AWell.FCells.ArrayLength);
  SetLength(ImportedCWCData, AWell.FCells.ArrayLength);
  for CellIndex := 0 to AWell.FCells.ArrayLength - 1 do
  begin
    ACell := AWell.FCells[CellIndex];
    APoint := FGrid.TwoDElementCenter(ACell.IC - 1, ACell.IR - 1);
    AScreenObject.AddPoint(APoint, True);
    APoint3D := FGrid.ThreeDElementCenter(ACell.IC - 1,
      ACell.IR - 1, ACell.IL - 1);
    if AWell.FCells.ArrayLength > 1 then
    begin
      ImportedElevations.RealValues[CellIndex] := APoint3D.Z;
    end
    else
    begin
      AScreenObject.ElevationFormula := FloatToStr(APoint3D.Z);
    end;
    ImportedRwData[CellIndex] := ACell.Rw;
    ImportedRSkinData[CellIndex] := ACell.Rskin;
    ImportedKSkinData[CellIndex] := ACell.Kskin;
    ImportedBData[CellIndex] := ACell.B;
    ImportedCData[CellIndex] := ACell.C;
    ImportedPData[CellIndex] := ACell.P;
    ImportedCWCData[CellIndex] := ACell.CWC;
  end;
  SpatialItem := nil;
  if AWell.LossType in [mltThiem, mltSkin, mltEquation] then
  begin
    if SpatialItem = nil then
    begin
      SpatialItem := AScreenObject.ModflowMnw2Boundary.Values.Add
        as TMnw2SpatialItem;
    end;
    if UniformArray(ImportedRwData) then
    begin
      AValue := ImportedRwData[0];
      SpatialItem.WellRadius := FloatToStr(AValue);
    end
    else
    begin
      ImportedValues := AScreenObject.ImportedValues.Add as TValueArrayItem;
      ImportedValues.Name := 'Imported_MNW2_Well_Radius';
      AssignImportedValues(ImportedValues, ImportedRwData);
      SpatialItem.WellRadius := rsObjectImportedValuesR
        + '("' + ImportedValues.Name + '")';
    end;
  end;
  if AWell.LossType = mltSkin then
  begin
    if SpatialItem = nil then
    begin
      SpatialItem := AScreenObject.ModflowMnw2Boundary.Values.Add
        as TMnw2SpatialItem;
    end;
    if UniformArray(ImportedRSkinData) then
    begin
      AValue := ImportedRSkinData[0];
      SpatialItem.SkinRadius := FloatToStr(AValue);
    end
    else
    begin
      ImportedValues := AScreenObject.ImportedValues.Add as TValueArrayItem;
      ImportedValues.Name := 'Imported_MNW2_Skin_Radius';
      AssignImportedValues(ImportedValues, ImportedRSkinData);
      SpatialItem.SkinRadius := rsObjectImportedValuesR
        + '("' + ImportedValues.Name + '")';
    end;
    if UniformArray(ImportedKSkinData) then
    begin
      AValue := ImportedKSkinData[0];
      SpatialItem.SkinK := FloatToStr(AValue);
    end
    else
    begin
      ImportedValues := AScreenObject.ImportedValues.Add as TValueArrayItem;
      ImportedValues.Name := 'Imported_MNW2_Skin_K';
      AssignImportedValues(ImportedValues, ImportedKSkinData);
      SpatialItem.SkinK := rsObjectImportedValuesR
        + '("' + ImportedValues.Name + '")';
    end;
  end;
  if AWell.LossType = mltEquation then
  begin
    if SpatialItem = nil then
    begin
      SpatialItem := AScreenObject.ModflowMnw2Boundary.Values.Add
        as TMnw2SpatialItem;
    end;
    if UniformArray(ImportedBData) then
    begin
      AValue := ImportedBData[0];
      SpatialItem.B := FloatToStr(AValue);
    end
    else
    begin
      ImportedValues := AScreenObject.ImportedValues.Add as TValueArrayItem;
      ImportedValues.Name := 'Imported_MNW2_B';
      AssignImportedValues(ImportedValues, ImportedBData);
      SpatialItem.B := rsObjectImportedValuesR
        + '("' + ImportedValues.Name + '")';
    end;
    if UniformArray(ImportedCData) then
    begin
      AValue := ImportedCData[0];
      SpatialItem.C := FloatToStr(AValue);
    end
    else
    begin
      ImportedValues := AScreenObject.ImportedValues.Add as TValueArrayItem;
      ImportedValues.Name := 'Imported_MNW2_C';
      AssignImportedValues(ImportedValues, ImportedCData);
      SpatialItem.C := rsObjectImportedValuesR
        + '("' + ImportedValues.Name + '")';
    end;
    if UniformArray(ImportedPData) then
    begin
      AValue := ImportedPData[0];
      SpatialItem.P := FloatToStr(AValue);
    end
    else
    begin
      ImportedValues := AScreenObject.ImportedValues.Add as TValueArrayItem;
      ImportedValues.Name := 'Imported_MNW2_P';
      AssignImportedValues(ImportedValues, ImportedPData);
      SpatialItem.P := rsObjectImportedValuesR
        + '("' + ImportedValues.Name + '")';
    end;
  end;
  if AWell.LossType = mtlSpecify then
  begin
    if SpatialItem = nil then
    begin
      SpatialItem := AScreenObject.ModflowMnw2Boundary.Values.Add
        as TMnw2SpatialItem;
    end;
    if UniformArray(ImportedCWCData) then
    begin
      AValue := ImportedCWCData[0];
      SpatialItem.CellToWellConductance := FloatToStr(AValue);
    end
    else
    begin
      ImportedValues := AScreenObject.ImportedValues.Add as TValueArrayItem;
      ImportedValues.Name := 'Imported_MNW2_Cell_to_Well_Conductance';
      AssignImportedValues(ImportedValues, ImportedCWCData);
      SpatialItem.CellToWellConductance := rsObjectImportedValuesR
        + '("' + ImportedValues.Name + '")';
    end;
  end;
end;

procedure TMnw2Importer.ReadWellForCurrentStressPeriod;
var
  AWell: TMnw2Well;
  StressPeriod: TMnw2WellStressPeriod;
  WELLNAME: string;
begin
  Readln(FImporter.FFile, WELLNAME);
  WELLNAME := Trim(WELLNAME);
  AWell := FWells.GetWellByName(WELLNAME);
  StressPeriod := AWell.FStressPeriods[FCurrentStressPeriod];
  StressPeriod.WellState := wsActive;
end;

procedure TMnw2Importer.ReadItmp;
var
  ITMP: Integer;
begin
  Readln(FImporter.FFile, ITMP);
  AddStressPeriod(ITMP);
end;

procedure TMnw2Importer.ReadLiftTableItem;
var
  AWell: TMnw2Well;
  PumpItem: TMnw2PumpTableItem;
begin
  AWell := FWells[FCurrentWell];
  Inc(AWell.FCurrentPumpItem);
  PumpItem := AWell.FPumpTable[AWell.FCurrentPumpItem];
  Readln(FImporter.FFile, PumpItem.Liftn, PumpItem.Qn);
end;

procedure TMnw2Importer.ReadLiftTableLimits;
var
  AWell: TMnw2Well;
begin
  AWell := FWells[FCurrentWell];
  Readln(FImporter.FFile, AWell.Hlift, AWell.LIFTq0,
    AWell.LIFTqdes, AWell.HWtol);
end;

procedure TMnw2Importer.ReadPartialPumpLimits;
var
  AWell: TMnw2Well;
  StressPeriod: TMnw2WellStressPeriod;
begin
  if FCurrentStressPeriod < 0 then
  begin
    AWell := FWells[FCurrentWell];
    Readln(FImporter.FFile, AWell.Hlim, AWell.QCUT);
  end
  else
  begin
    AWell := FWells.FCurrentWell;
    StressPeriod := AWell.FStressPeriods[FCurrentStressPeriod];
    Readln(FImporter.FFile, StressPeriod.Hlim, StressPeriod.QCUT);
  end;
end;

procedure TMnw2Importer.ReadFullPumpLimits;
var
  AWell: TMnw2Well;
  StressPeriod: TMnw2WellStressPeriod;
begin
  if FCurrentStressPeriod < 0 then
  begin
    AWell := FWells[FCurrentWell];
    Readln(FImporter.FFile, AWell.Hlim, AWell.QCUT, AWell.Qfrcmn, AWell.Qfrcmx);
  end
  else
  begin
    AWell := FWells.FCurrentWell;
    StressPeriod := AWell.FStressPeriods[FCurrentStressPeriod];
    Readln(FImporter.FFile, StressPeriod.Hlim, StressPeriod.QCUT,
      StressPeriod.Qfrcmn, StressPeriod.Qfrcmx);
  end;
end;

procedure TMnw2Importer.ReadPumpZ;
var
  AWell: TMnw2Well;
begin
  AWell := FWells[FCurrentWell];
  Readln(FImporter.FFile, AWell.Zpump);
end;

procedure TMnw2Importer.ReadPumpCell;
var
  AWell: TMnw2Well;
begin
  AWell := FWells[FCurrentWell];
  Readln(FImporter.FFile, AWell.PUMPLAY, AWell.PUMPROW, AWell.PUMPCOL);
end;

procedure TMnw2Importer.ReadAWellScreen(const ALabel: string);
var
  B: Double;
  CWC: Double;
  AWell: TMnw2Well;
  AScreen: TMnw2WellScreen;
  DataLabels: TStringList;
  Index: Integer;
  Ztop: Double;
  Zbotm: Double;
  IR: Integer;
  IC: Integer;
  Rw: Double;
  Kskin: Double;
  Rskin: Double;
  P: Double;
  C: Double;
begin
  AWell := FWells[FCurrentWell];
  Inc(AWell.FCurrentScreen);
  AScreen := AWell.FScreens[AWell.FCurrentScreen];

//  AScreen.Rw := AWell.Rw;
//  AScreen.Rskin := AWell.Rskin;
//  AScreen.Kskin := AWell.Kskin;
//  AScreen.B := AWell.B;
//  AScreen.C := AWell.C;
//  AScreen.P := AWell.P;
//  AScreen.CWC := AWell.CWC;

  DataLabels := TStringList.Create;
  try
    DataLabels.Delimiter := ',';
    // Remove colon from ALabel and split.
    DataLabels.DelimitedText := Copy(ALabel, 1, Length(ALabel) - 1);
    for Index := 0 to DataLabels.Count - 1 do
    begin
      if DataLabels[Index] = 'Ztop' then
      begin
        Read(FImporter.FFile, Ztop);
        AScreen.Ztop := Ztop;
      end
      else if DataLabels[Index] = 'Zbotm' then
      begin
        Read(FImporter.FFile, Zbotm);
        AScreen.Zbotm := Zbotm;
      end
      else if DataLabels[Index] = 'IR' then
      begin
        Read(FImporter.FFile, IR);
        AScreen.IR := IR;
      end
      else if DataLabels[Index] = 'IC' then
      begin
        Read(FImporter.FFile, IC);
        AScreen.IC := IC;
      end
      else if DataLabels[Index] = 'RwNode' then
      begin
        Read(FImporter.FFile, Rw);
        AScreen.Rw := Rw;
      end
      else if DataLabels[Index] = 'KskinNode' then
      begin
        Read(FImporter.FFile, Kskin);
        AScreen.Kskin := Kskin;
      end
      else if DataLabels[Index] = 'RskinNode' then
      begin
        Read(FImporter.FFile, Rskin);
        AScreen.Rskin := Rskin;
      end
      else if DataLabels[Index] = 'PNode' then
      begin
        Read(FImporter.FFile, P);
        AScreen.P := P;
      end
      else if DataLabels[Index] = 'CNode' then
      begin
        Read(FImporter.FFile, C);
        AScreen.C := C;
      end
      else if DataLabels[Index] = 'BNode' then
      begin
        Read(FImporter.FFile, B);
        AScreen.B := B;
      end
      else if DataLabels[Index] = 'CWCNode' then
      begin
        Read(FImporter.FFile, CWC);
        AScreen.CWC := CWC;
      end
      else
      begin
        ShowMessage(ALabel);
        Assert(False);
      end;
    end;
  finally
    DataLabels.Free;
  end;
end;

procedure TMnw2Importer.ReadACell(const ALabel: string);
var
  PP: Double;
  CWC: Double;
  B: Double;
  C: Double;
  P: Double;
  Rskin: Double;
  Kskin: Double;
  Rw: Double;
  IC: Integer;
  IR: Integer;
  IL: Integer;
  Index: Integer;
  DataLabels: TStringList;
  ACell: TMnw2WellCell;
  AWell: TMnw2Well;
begin
  AWell := FWells[FCurrentWell];
  Inc(AWell.FCurrentCell);
  ACell := AWell.FCells[AWell.FCurrentCell];
//  ACell.Rw := AWell.Rw;
//  ACell.Rskin := AWell.Rskin;
//  ACell.Kskin := AWell.Kskin;
//  ACell.B := AWell.B;
//  ACell.C := AWell.C;
//  ACell.P := AWell.P;
//  ACell.CWC := AWell.CWC;

  DataLabels := TStringList.Create;
  try
    DataLabels.Delimiter := ',';
    // Remove colon from ALabel and split.
    DataLabels.DelimitedText := Copy(ALabel, 1, Length(ALabel) - 1);
    for Index := 0 to DataLabels.Count - 1 do
    begin
      if DataLabels[Index] = 'IL' then
      begin
        Read(FImporter.FFile, IL);
        ACell.IL := IL;
      end
      else if DataLabels[Index] = 'IR' then
      begin
        Read(FImporter.FFile, IR);
        ACell.IR := IR;
      end
      else if DataLabels[Index] = 'IC' then
      begin
        Read(FImporter.FFile, IC);
        ACell.IC := IC;
      end
      else if DataLabels[Index] = 'RwNode' then
      begin
        Read(FImporter.FFile, Rw);
        ACell.Rw := Rw;
      end
      else if DataLabels[Index] = 'KskinNode' then
      begin
        Read(FImporter.FFile, Kskin);
        ACell.Kskin := Kskin;
      end
      else if DataLabels[Index] = 'RskinNode' then
      begin
        Read(FImporter.FFile, Rskin);
        ACell.Rskin := Rskin;
      end
      else if DataLabels[Index] = 'PNode' then
      begin
        Read(FImporter.FFile, P);
        ACell.P := P;
      end
      else if DataLabels[Index] = 'CNode' then
      begin
        Read(FImporter.FFile, C);
        ACell.C := C;
      end
      else if DataLabels[Index] = 'BNode' then
      begin
        Read(FImporter.FFile, B);
        ACell.B := B;
      end
      else if DataLabels[Index] = 'CWCNode' then
      begin
        Read(FImporter.FFile, CWC);
        ACell.CWC := CWC;
      end
      else if DataLabels[Index] = 'PP' then
      begin
        Read(FImporter.FFile, PP);
        ACell.PP := PP;
      end
      else
      begin
        ShowMessage(ALabel);
        Assert(False);
      end;
    end;
    Readln(FImporter.FFile);
  finally
    DataLabels.Free;
  end;
end;

procedure TMnw2Importer.ReadSpecifyCwcData;
var
  CWC: Double;
  AWell: TMnw2Well;
begin
  AWell := FWells[FCurrentWell];
  Readln(FImporter.FFile, CWC);
  AWell.CWC := CWC;
end;

procedure TMnw2Importer.ReadEquationData;
var
  Rw: Double;
  B: Double;
  C: Double;
  P: Double;
  AWell: TMnw2Well;
begin
  AWell := FWells[FCurrentWell];
  Readln(FImporter.FFile, Rw, B, C, P);
  AWell.Rw := Rw;
  AWell.B := B;
  AWell.C := C;
  AWell.P := P;
end;

procedure TMnw2Importer.ReadSkinData;
var
  Kskin: Double;
  Rskin: Double;
  Rw: Double;
  AWell: TMnw2Well;
begin
  AWell := FWells[FCurrentWell];
  Readln(FImporter.FFile, Rw, Rskin, Kskin);
  AWell.Rw := Rw;
  AWell.Rskin := Rskin;
  AWell.Kskin := Kskin;
end;

procedure TMnw2Importer.ReadThiemData;
var
  AWell: TMnw2Well;
  Rw: double;
begin
  AWell := FWells[FCurrentWell];
  Readln(FImporter.FFile, Rw);
  AWell.Rw := Rw;
end;

procedure TMnw2Importer.ReadFlags;
var
  AWell: TMnw2Well;
  PUMPCAP: integer;
begin
  AWell := FWells[FCurrentWell];
  Readln(FImporter.FFile, AWell.PUMPLOC, AWell.Qlimit, AWell.PPFLAG, PUMPCAP);
  AWell.PUMPCAP := PUMPCAP;
end;

procedure TMnw2Importer.ReadLossType;
var
  LOSSTYPE: string;
  AWell: TMnw2Well;
begin
  AWell := FWells[FCurrentWell];
  Readln(FImporter.FFile, LOSSTYPE);
  LOSSTYPE := UpperCase(Trim(LOSSTYPE));
  if LOSSTYPE = 'NONE' then
  begin
    AWell.LossType := mltNone;
  end
  else if LOSSTYPE = 'THIEM' then
  begin
    AWell.LossType := mltThiem;
  end
  else if LOSSTYPE = 'SKIN' then
  begin
    AWell.LossType := mltSkin;
  end
  else if LOSSTYPE = 'GENERAL' then
  begin
    AWell.LossType := mltEquation;
  end
  else if LOSSTYPE = 'SPECIFYCWC' then
  begin
    AWell.LossType := mtlSpecify;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TMnw2Importer.ReadNumberOfNodes;
var
  AWell: TMnw2Well;
  NNodes: Integer;
begin
  AWell := FWells[FCurrentWell];
  Readln(FImporter.FFile, NNodes);
  AWell.NNodes := NNodes;
end;

procedure TMnw2Importer.ReadWellId;
var
  AWell: TMnw2Well;
begin
  Inc(FCurrentWell);
  AWell := FWells[FCurrentWell];
  Readln(FImporter.FFile, AWell.WellId);
  AWell.WellId := Trim(AWell.WellId);
end;

procedure TMnw2Importer.ReadAuxiliary;
var
  Auxiliary: string;
begin
  Readln(FImporter.FFile, Auxiliary);
end;

procedure TMnw2Importer.ReadDataSet1;
var
  MNWMAX: integer;
  IWL2CB: Integer;
begin
  Readln(FImporter.FFile, MNWMAX, IWL2CB, MNWPRNT);
  FWells.ArrayLength := MNWMAX;
  FCurrentWell := -1;
end;

procedure TMnw2Importer.HandlePackage;
const
  ScreenObjectNameRoot = 'ImportedMnw2_';
var
  WellIndex: Integer;
  AWell: TMnw2Well;
  AScreenObject: TScreenObject;
  APoint: TPoint2D;
  APoint3D: T3DRealPoint;
  Target: TTargetLocation;
  LiftIndex: Integer;
  Mnw2LiftItem: TMnw2PumpTableItem;
  LiftItem: TLiftItem;
  MnwStressPeriod: TMnw2WellStressPeriod;
  StressPeriodIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeItem: TMnw2TimeItem;
  ObsWell: TMnwiItem;
begin
  if (FCurrentStressPeriod < 0) or
    (FCurrentStressPeriod < FModel.ModflowStressPeriods.Count -1) then
  begin
    Exit;
  end;
  inherited;
  FMnw2Package := FModel.ModflowPackages.Mnw2Package;
  FMnw2Package.IsSelected := True;
  FMnw2Package.Comments := FComments;
  if MNWPRNT < 0 then
  begin
    MNWPRNT := 0;
  end;
  if MNWPRNT > 2 then
  begin
    MNWPRNT := 2;
  end;
  FMnw2Package.PrintOption := TMnw2PrintOption(MNWPRNT);

  FMnw2Package.CreateWellFile := FMnwiImporter.Wel1flag <> 0;
  FMnw2Package.SummarizeByWell := FMnwiImporter.QSUMflag <> 0;
  FMnw2Package.SummarizeByNode := FMnwiImporter.BYNDflag <> 0;


  for WellIndex := 0 to FWells.ArrayLength - 1 do
  begin
    AWell := FWells[WellIndex];

    AScreenObject := CreateScreenObject(ScreenObjectNameRoot
      + IntToStr(WellIndex+1));
    AScreenObject.SetValuesOfEnclosedCells := False;
    AScreenObject.SetValuesOfIntersectedCells := True;
    AScreenObject.CreateMnw2Boundary;

    AScreenObject.ModflowMnw2Boundary.WellID := AWell.WellId;

    ObsWell := FMnwiImporter.GetWellByName(AWell.WellId);
    if ObsWell <> nil then
    begin
      AScreenObject.ModflowMnw2Boundary.SaveMnwiInfo := True;
      AScreenObject.ModflowMnw2Boundary.SaveExternalFlows :=
        ObsWell.QNDflag <> 0;
      AScreenObject.ModflowMnw2Boundary.SaveInternalFlows :=
        ObsWell.QBHflag <> 0
    end;

    AScreenObject.ModflowMnw2Boundary.LossType := AWell.LossType;
    AScreenObject.ModflowMnw2Boundary.SpecifyPump := AWell.PUMPLOC <> 0;
    if AWell.PUMPLOC < 0 then
    begin
      AScreenObject.ModflowMnw2Boundary.PumpElevation := AWell.Zpump;
    end;
    if AWell.PUMPLOC > 0 then
    begin
      APoint := FGrid.TwoDElementCenter(AWell.PUMPCOL-1, AWell.PUMPROW-1);
      APoint3D := FGrid.ThreeDElementCenter(AWell.PUMPCOL-1,
        AWell.PUMPROW-1, AWell.PUMPLAY-1);

      AScreenObject.ModflowMnw2Boundary.PumpCellTarget.TargetType:= ttLocation;

      Target := TTargetLocation.Create(nil);
      try
        Target.X := APoint.X;
        Target.Y := APoint.Y;
        Target.Z := APoint3D.Z;
        AScreenObject.ModflowMnw2Boundary.
          PumpCellTarget.TargetLocation := Target;
      finally
        Target.Free;
      end;
    end;
    AScreenObject.ModflowMnw2Boundary.ConstrainPumping := AWell.Qlimit <> 0;
    AScreenObject.ModflowMnw2Boundary.PartialPenetrationCorrection :=
      AWell.PPFLAG > 0;
    AScreenObject.ModflowMnw2Boundary.AdjustPumping := AWell.PUMPCAP > 0;
    if AWell.PUMPCAP > 0 then
    begin
      AScreenObject.ModflowMnw2Boundary.ReferenceHead := AWell.Hlift;
      AScreenObject.ModflowMnw2Boundary.MaximumLift := AWell.LIFTq0;
      AScreenObject.ModflowMnw2Boundary.LiftAtMaxRate := AWell.LIFTqdes;
      AScreenObject.ModflowMnw2Boundary.WellTolerance := AWell.HWtol;
      AScreenObject.ModflowMnw2Boundary.LiftValues.Capacity :=
        AWell.FPumpTable.ArrayLength;
      for LiftIndex := 0 to AWell.FPumpTable.ArrayLength - 1 do
      begin
        Mnw2LiftItem := AWell.FPumpTable[LiftIndex];
        LiftItem := AScreenObject.ModflowMnw2Boundary.LiftValues.Add
          as TLiftItem;
        LiftItem.Lift := Mnw2LiftItem.LIFTn;
        LiftItem.Q := Mnw2LiftItem.Qn;
      end;
    end;

    if AWell.FCells <> nil then
    begin
      AssignCells(AScreenObject, AWell);
    end
    else
    begin
      AssignScreens(AScreenObject, AWell);
    end;

    Assert(AWell.FStressPeriods.ArrayLength =
      FModel.ModflowStressPeriods.Count);
    AScreenObject.ModflowMnw2Boundary.TimeValues.Capacity :=
      AWell.FStressPeriods.ArrayLength;
    TimeItem := nil;
    for StressPeriodIndex := 0 to AWell.FStressPeriods.ArrayLength - 1 do
    begin
      MnwStressPeriod := AWell.FStressPeriods[StressPeriodIndex];
      StressPeriod := FModel.ModflowStressPeriods[StressPeriodIndex];
      case MnwStressPeriod.WellState of
        wsInactive:
          begin
            TimeItem := nil;
          end;
        wsActive:
          begin
            TimeItem := AScreenObject.ModflowMnw2Boundary.TimeValues.Add
              as TMnw2TimeItem;
            TimeItem.StartTime := StressPeriod.StartTime;
            TimeItem.EndTime := StressPeriod.EndTime;
            TimeItem.PumpingRate := FloatToStr(MnwStressPeriod.QDes);
            TimeItem.HeadCapacityMultiplier :=
              FloatToStr(MnwStressPeriod.CapMult);
            TimeItem.LimitingWaterLevel := FloatToStr(MnwStressPeriod.Hlim);
            TimeItem.InactivationPumpingRate :=
              FloatToStr(MnwStressPeriod.Qfrcmn);
            TimeItem.ReactivationPumpingRate :=
              FloatToStr(MnwStressPeriod.Qfrcmx);
            if MnwStressPeriod.QCUT > 0 then
            begin
              TimeItem.LimitMethod := mlmRate;
            end
            else if MnwStressPeriod.QCUT < 0 then
            begin
              TimeItem.LimitMethod := mlmFraction;
            end
            else
            begin
              TimeItem.LimitMethod := mlmNoMinimum;
            end;
          end;
        wsReuse: 
          begin
            TimeItem.EndTime := StressPeriod.EndTime;
          end
        else Assert(False);
      end;
    end;
  end;

end;

procedure TMnw2Importer.ReadData(const ALabel: string);
var
  AWell: TMnw2Well;
  StressPeriod: TMnw2WellStressPeriod;
  DataItems: TStringList;
  Index: Integer;
begin
  if ALabel = 'MNWMAX, IWL2CB, MNWPRNT:' then
  begin
    ReadDataSet1;
  end
  else if ALabel = 'MNWAUX(NAUX):' then
  begin
    ReadAuxiliary;
  end
  else if ALabel = 'WELLID(MNWID):' then
  begin
    ReadWellId;
  end
  else if ALabel = 'NNODES:' then
  begin
    ReadNumberOfNodes;
  end
  else if ALabel = 'LOSSTYPE:' then
  begin
    ReadLossType;
  end
  else if ALabel = 'PUMPLOC,Qlimit,PPFLAG,PUMPCAP:' then
  begin
    ReadFlags;
  end
  else if ALabel = 'Rw:' then
  begin
    ReadThiemData;
  end
  else if ALabel = 'Rw,Rskin,Kskin:' then
  begin
    ReadSkinData;
  end
  else if ALabel = 'Rw,B,C,P:' then
  begin
    ReadEquationData;
  end
  else if ALabel = 'CWC:' then
  begin
    ReadSpecifyCwcData;
  end
  else if Pos('IL,IR,IC', ALabel) = 1 then
  begin
    ReadACell(ALabel);
  end
  else if Pos('Ztop,Zbotm,IR,IC', ALabel) = 1 then
  begin
    ReadAWellScreen(ALabel);
  end
  else if ALabel = 'PUMPLAY,PUMPROW,PUMPCOL:' then
  begin
    ReadPumpCell;
  end
  else if ALabel = 'Zpump:' then
  begin
    ReadPumpZ;
  end
  else if ALabel = 'Hlim,QCUT,Qfrcmn,Qfrcmx:' then
  begin
    ReadFullPumpLimits;
  end
  else if ALabel = 'Hlim,QCUT:' then
  begin
    ReadPartialPumpLimits;
  end
  else if ALabel = 'Hlift,LIFTq0,LIFTqdes,HWtol:' then
  begin
    ReadLiftTableLimits;
  end
  else if ALabel = 'Liftn,Qn:' then
  begin
    ReadLiftTableItem;
  end
  else if ALabel = 'ITMP:' then
  begin
    ReadItmp;
  end
  else if ALabel = 'WELLNAME:' then
  begin
    ReadWellForCurrentStressPeriod;
  end
  else if Pos('Qdes',ALabel) = 1 then
  begin
    AWell := FWells.FCurrentWell;
    StressPeriod := AWell.FStressPeriods[FCurrentStressPeriod];
    DataItems := TStringList.Create;
    try
      DataItems.Delimiter := ',';
      DataItems.DelimitedText := Copy(ALabel, 1, Length(ALabel)-1);
      for Index := 0 to DataItems.Count - 1 do
      begin
        if DataItems[Index] = 'Qdes' then
        begin
          Read(FImporter.FFile, StressPeriod.Qdes);
        end
        else if DataItems[Index] = 'CapMult' then
        begin
          Read(FImporter.FFile, StressPeriod.CapMult);
        end
        else if DataItems[Index] = 'Cprime' then
        begin
          break;
        end
        else if DataItems[Index] = 'AUX' then
        begin
          break;
        end
        else
        begin
          ShowMessage(ALabel);
          Assert(False);
        end;
      end;
    finally
      DataItems.Free;
    end;
    Readln(FImporter.FFile);
  end
  else
  begin
    ShowMessage(ALabel);
    Assert(False);
  end;
end;

{ TMnw2WellArray }

function TMnw2WellArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TMnw2Well;
end;

function TMnw2WellArray.GetWell(Index: integer): TMnw2Well;
begin
  result := TMnw2Well(Objects[Index]);
end;

function TMnw2WellArray.GetWellByName(const AWellId: string): TMnw2Well;
var
  Index: Integer;
  AWell: TMnw2Well;
begin
  result := nil;
  for Index := 0 to ArrayLength - 1 do
  begin
    AWell := Wells[Index];
    if SameText(AWellId, AWell.WellId) then
    begin
      FCurrentWell := AWell;
      result := FCurrentWell;
    end;
  end;
end;

{ TMnw2Well }

constructor TMnw2Well.Create;
begin
  inherited;
  FCurrentCell := -1;
  FCurrentScreen := -1;
  FCurrentPumpItem := -1;
  FStressPeriods:= TMnw2StressPeriodArray.Create;
end;

destructor TMnw2Well.Destroy;
begin
  FStressPeriods.Free;
  FCells.Free;
  FScreens.Free;
  FPumpTable.Free;
  inherited;
end;

procedure TMnw2Well.SetB(const Value: double);
var
  Index: Integer;
begin
  if FCells <> nil then
  begin
    for Index := 0 to FCells.ArrayLength - 1 do
    begin
      FCells[Index].B := Value;
    end;
  end;
  if FScreens <> nil then
  begin
    for Index := 0 to FScreens.ArrayLength - 1 do
    begin
      FScreens[Index].B := Value;
    end;
  end;
end;

procedure TMnw2Well.SetC(const Value: double);
var
  Index: Integer;
begin
  if FCells <> nil then
  begin
    for Index := 0 to FCells.ArrayLength - 1 do
    begin
      FCells[Index].C := Value;
    end;
  end;
  if FScreens <> nil then
  begin
    for Index := 0 to FScreens.ArrayLength - 1 do
    begin
      FScreens[Index].C := Value;
    end;
  end;
end;

procedure TMnw2Well.SetCWC(const Value: double);
var
  Index: Integer;
begin
  if FCells <> nil then
  begin
    for Index := 0 to FCells.ArrayLength - 1 do
    begin
      FCells[Index].CWC := Value;
    end;
  end;
  if FScreens <> nil then
  begin
    for Index := 0 to FScreens.ArrayLength - 1 do
    begin
      FScreens[Index].CWC := Value;
    end;
  end;
end;

procedure TMnw2Well.SetKskin(const Value: double);
var
  Index: Integer;
begin
  if FCells <> nil then
  begin
    for Index := 0 to FCells.ArrayLength - 1 do
    begin
      FCells[Index].Kskin := Value;
    end;
  end;
  if FScreens <> nil then
  begin
    for Index := 0 to FScreens.ArrayLength - 1 do
    begin
      FScreens[Index].Kskin := Value;
    end;
  end;
end;

procedure TMnw2Well.SetNNodes(const Value: integer);
begin
  FNNodes := Value;
  if FNNodes > 0 then
  begin
    if FCells = nil then
    begin
      FCells := TMnw2CellArray.Create;
    end;
    FCells.ArrayLength := FNNodes;
  end
  else
  begin
    if FScreens = nil then
    begin
      FScreens := TMnw2ScreenArray.Create;
    end;
    FScreens.ArrayLength := -FNNodes;
  end;
end;

procedure TMnw2Well.SetP(const Value: double);
var
  Index: Integer;
begin
  if FCells <> nil then
  begin
    for Index := 0 to FCells.ArrayLength - 1 do
    begin
      FCells[Index].P := Value;
    end;
  end;
  if FScreens <> nil then
  begin
    for Index := 0 to FScreens.ArrayLength - 1 do
    begin
      FScreens[Index].P := Value;
    end;
  end;
end;

procedure TMnw2Well.SetPUMPCAP(const Value: integer);
begin
  FPUMPCAP := Value;
  if FPUMPCAP > 0 then
  begin
    if FPumpTable = nil then
    begin
      FPumpTable := TMnw2PumpTableArray.Create;
    end;
    FPumpTable.ArrayLength := FPUMPCAP;
  end
  else
  begin
    FreeAndNil(FPumpTable);
  end;
end;

procedure TMnw2Well.SetRskin(const Value: double);
var
  Index: Integer;
begin
  if FCells <> nil then
  begin
    for Index := 0 to FCells.ArrayLength - 1 do
    begin
      FCells[Index].Rskin := Value;
    end;
  end;
  if FScreens <> nil then
  begin
    for Index := 0 to FScreens.ArrayLength - 1 do
    begin
      FScreens[Index].Rskin := Value;
    end;
  end;
end;

procedure TMnw2Well.SetRw(const Value: double);
var
  Index: Integer;
begin
  if FCells <> nil then
  begin
    for Index := 0 to FCells.ArrayLength - 1 do
    begin
      FCells[Index].Rw := Value;
    end;
  end;
  if FScreens <> nil then
  begin
    for Index := 0 to FScreens.ArrayLength - 1 do
    begin
      FScreens[Index].Rw := Value;
    end;
  end;
end;

{ TMnw2CellArray }

function TMnw2CellArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TMnw2WellCell;
end;

function TMnw2CellArray.GetCell(Index: integer): TMnw2WellCell;
begin
  result := TMnw2WellCell(Objects[Index]);
end;

{ TMnw2ScreenArray }

function TMnw2ScreenArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TMnw2WellScreen;
end;

function TMnw2ScreenArray.GetScreen(Index: integer): TMnw2WellScreen;
begin
  result := TMnw2WellScreen(Objects[Index]);
end;

{ TMnw2PumpTableArray }

function TMnw2PumpTableArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TMnw2PumpTableItem;
end;

function TMnw2PumpTableArray.GetItem(Index: integer): TMnw2PumpTableItem;
begin
  result := TMnw2PumpTableItem(Objects[Index]);
end;

{ TMnw2StressPeriodArray }

function TMnw2StressPeriodArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TMnw2WellStressPeriod;
end;

function TMnw2StressPeriodArray.GetStressPeriod(
  Index: integer): TMnw2WellStressPeriod;
begin
  result := TMnw2WellStressPeriod(Objects[Index]);
end;

{ TBcfImporter }

constructor TBcfImporter.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'BCF:');
  FIsSelected := False;
  FTransientModel := False;
end;

procedure TBcfImporter.ImportDataSets2And3;
var
  LayerIndex: Integer;
  Layer: Integer;
  LayerGroup: TLayerGroup;
begin
  Assert(Length(LAYCON) = Length(TRPY));
  for LayerIndex := 0 to Length(LAYCON) - 1 do
  begin
    Layer := FModel.LayerStructure.ModflowLayerToDataSetLayer(LayerIndex + 1);
    LayerGroup := FModel.LayerStructure[Layer + 1];
    LayerGroup.AquiferType := LAYCON[LayerIndex] mod 10;
    LayerGroup.InterblockTransmissivityMethod := LAYCON[LayerIndex] div 10;
    LayerGroup.HorizontalAnisotropy := TRPY[LayerIndex];
  end;
end;

procedure TBcfImporter.ImportDataSet1;
begin
  FModel.ModflowOptions.HDry := HDRY;
  FModel.ModflowWettingOptions.WettingActive := IWDFLG <> 0;
  if FModel.ModflowWettingOptions.WettingActive then
  begin
    FModel.ModflowWettingOptions.WettingFactor := WETFCT;
    FModel.ModflowWettingOptions.WettingIterations := IWETIT;
    FModel.ModflowWettingOptions.WettingEquation := IHDWET;
  end;
end;

procedure TBcfImporter.ReadConstantArrays;
var
  ConstArray: TRealConstantRecordArray;
  Value: Double;
  Layer: Integer;
  ID: string;
  AquiferType: Integer;
begin
  ReadLn(FImporter.FFile, ID);
  ID := Trim(ID);
  ReadLn(FImporter.FFile, Layer);
  Readln(FImporter.FFile, Value);
  Dec(Layer);
  if ID = StrPRIMARYSTORAGECOEF then
  begin
    Assert(FTransientModel);
    AquiferType := LAYCON[Layer] mod 10;

    if AquiferType = 1 then
    begin
      InitializeContArray(FSpecificYield_Const);
      ConstArray := FSpecificYield_Const;
    end
    else
    begin
      InitializeContArray(FConfinedStorage_Const);
      ConstArray := FConfinedStorage_Const;
    end;
  end
  else if ID = StrTRANSMISALONGROWS then
  begin
    InitializeContArray(FTran_Const);
    ConstArray := FTran_Const;
  end
  else if ID = StrHYDCONDALONGROW then
  begin
    InitializeContArray(FHY_Const);
    ConstArray := FHY_Const;
  end
  else if ID = StrVERTHYDCONDTHICK then
  begin
    InitializeContArray(FVcont_Const);
    ConstArray := FVcont_Const;
  end
  else if ID = StrSECONDARYSTORAGECO then
  begin
    InitializeContArray(FSpecificYield_Const);
    ConstArray := FSpecificYield_Const;
  end
  else if ID = StrWETDRYPARAMETER then
  begin
    InitializeContArray(FWetDry_Const);
    ConstArray := FWetDry_Const;
  end
  else
  begin
    Assert(False);
  end;
  ConstArray[Layer].IsConstant := True;
  ConstArray[Layer].RealValue := Value;
end;

procedure TBcfImporter.ReadVariableArrays;
var
  ID: string;
  Layer: Integer;
  ConstArray: TRealConstantRecordArray;
  ThreeDArray: T3DDoubleArray;
  AquiferType: Integer;
begin
  ReadLn(FImporter.FFile, ID);
  ID := Trim(ID);
  ReadLn(FImporter.FFile, Layer);
  Dec(Layer);
  if ID = StrPRIMARYSTORAGECOEF then
  begin
    Assert(FTransientModel);
    AquiferType := LAYCON[Layer] mod 10;
    if AquiferType = 1 then
    begin
      InitializeContArray(FSpecificYield_Const);
      Initialize3DArray(FSpecificYield);
      ThreeDArray := FSpecificYield;
      ConstArray := FSpecificYield_Const;
    end
    else
    begin
      InitializeContArray(FConfinedStorage_Const);
      Initialize3DArray(FConfinedStorage);
      ThreeDArray := FConfinedStorage;
      ConstArray := FConfinedStorage_Const;
    end;
  end
  else if ID = StrTRANSMISALONGROWS then
  begin
    InitializeContArray(FTran_Const);
    Initialize3DArray(FTran);
    ThreeDArray := FTran;
    ConstArray := FTran_Const;
  end
  else if ID = StrHYDCONDALONGROW then
  begin
    InitializeContArray(FHY_Const);
    Initialize3DArray(FHY);
    ThreeDArray := FHY;
    ConstArray := FHY_Const;
  end
  else if ID = StrVERTHYDCONDTHICK then
  begin
    InitializeContArray(FVcont_Const);
    Initialize3DArray(FVcont);
    ThreeDArray := FVcont;
    ConstArray := FVcont_Const;
  end
  else if ID = StrSECONDARYSTORAGECO then
  begin
    Assert(FTransientModel);
    InitializeContArray(FSpecificYield_Const);
    Initialize3DArray(FSpecificYield);
    ThreeDArray := FSpecificYield;
    ConstArray := FSpecificYield_Const;
  end
  else if ID = StrWETDRYPARAMETER then
  begin
    InitializeContArray(FWetDry_Const);
    Initialize3DArray(FWetDry);
    ThreeDArray := FWetDry;
    ConstArray := FWetDry_Const;
  end
  else
  begin
    Assert(False);
  end;
  if ThreeDArray[Layer] = nil then
  begin
    SetLength(ThreeDArray[Layer], FGrid.RowCount, FGrid.ColumnCount);
  end;
  Read2DRealArray(ThreeDArray[Layer]);
  ConstArray[Layer].IsConstant := False;
end;

procedure TBcfImporter.ReadConstantAnisotropy;
var
  TRPY_Const: Double;
  AName: string;
begin
  Readln(FImporter.FFile, AName);
  AName := Trim(AName);
  if AName = StrCOLUMNTOROWANISOT then
  begin
    Readln(FImporter.FFile, TRPY_Const);
    AssignConstant1DArray(TRPY, TRPY_Const);
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TBcfImporter.ReadVariableAnisotropy;
var
  AName: string;
begin
  Readln(FImporter.FFile, AName);
  AName := Trim(AName);
  if AName = StrCOLUMNTOROWANISOT then
  begin
    Read1DRealArray(TRPY);
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TBcfImporter.Initialize3DArray(var ThreeDArray: T3DDoubleArray);
begin
  if ThreeDArray = nil then
  begin
    SetLength(ThreeDArray, NLAY);
  end;
end;

procedure TBcfImporter.InitializeContArray(
  var ConstArray: TRealConstantRecordArray);
var
  Index: Integer;
begin
  if ConstArray = nil then
  begin
    SetLength(ConstArray, NLay);
    InitializeConstArray(ConstArray);
    for Index := 0 to Length(ConstArray) - 1 do
    begin
      ConstArray[Index].IsConstant := True;
      ConstArray[Index].RealValue := 0;
    end;
  end;
end;

procedure TBcfImporter.ReadDataSet2;
var
  Index: Integer;
begin
  Readln(FImporter.FFile, NLAY);
  SetLength(LAYCON, NLAY);
  SetLength(TRPY, NLAY);
  for Index := 0 to NLAY - 1 do
  begin
    Read(FImporter.FFile, LAYCON[Index]);
  end;
  Readln(FImporter.FFile);
end;

procedure TBcfImporter.ReadDataSet1;
var
  IBCFCB: Integer;
begin
  FIsSelected := True;
  Read(FImporter.FFile, IBCFCB);
  Read(FImporter.FFile, HDRY);
  Read(FImporter.FFile, IWDFLG);
  Read(FImporter.FFile, WETFCT);
  Read(FImporter.FFile, IWETIT);
  Read(FImporter.FFile, IHDWET);
  Readln(FImporter.FFile);
end;

procedure TBcfImporter.HandlePackage;
var
  BcfPackage: TModflowPackageSelection;
begin
  inherited;
  BcfPackage := FModel.ModflowPackages.BcfPackage;
  BcfPackage.IsSelected := FIsSelected;
  if FIsSelected then
  begin
    FModel.ModflowPackages.LpfPackage.IsSelected := False;
    FModel.ModflowPackages.HufPackage.IsSelected := False;
    ImportDataSet1;
    ImportDataSets2And3;
    FModel.CreateInitialDataSets;

    ImportDataSet(StrConfinedStorageCoe, StrConfinedStorageCoe,
      FConfinedStorage_Const, FConfinedStorage);
    ImportDataSet(StrTransmissivity, StrTransmissivity,
      FTran_Const, FTran);
    ImportDataSet(rsKx, rsKx, FHY_Const, FHY);
    ImportDataSet(rsSpecificYield, rsSpecificYield,
      FSpecificYield_Const, FSpecificYield);
    ImportDataSet(rsWetDry, rsWetDry, FWetDry_Const, FWetDry);
    ImportDataSet(StrVerticalConductance, StrVerticalConductance,
      FVcont_Const, FVcont);
  end;
end;

procedure TBcfImporter.ReadData(const ALabel: string);
var
  ISS: integer;
begin
  inherited;
  if ALabel = 'IBCFCB,HDRY,IWDFLG,WETFCT,IWETIT,IHDWET:' then
  begin
    ReadDataSet1;
  end
  else if ALabel = 'ISS:' then
  begin
    Read(FImporter.FFile, ISS);
    Readln(FImporter.FFile);
    FTransientModel := ISS = 0;
  end
  else if ALabel = 'LAYCON(I),I=1,NLAY:' then
  begin
    ReadDataSet2;
  end
  else if ALabel = StrConstant1DRealArray then
  begin
    ReadConstantAnisotropy;
  end
  else if ALabel = StrVariable1DRealArray then
  begin
    ReadVariableAnisotropy;
  end
  else if ALabel = StrConstant2DRealArrayForLayer then
  begin
    ReadConstantArrays;
  end
  else if ALabel = StrVariable2DRealArrayForLayer then
  begin
    ReadVariableArrays;
  end
  else
  begin
    Assert(False);
  end;
end;

{ TMnwiImporter }

constructor TMnwiImporter.Create(Importer: TModflow2005Importer;
  MnwImporter: TMnw2Importer);
begin
  inherited Create(Importer, 'MNWI');
  MnwImporter.FMnwiImporter := self;
  FMnwiArray := TMnwiArray.Create;
  FCurrentWell := -1;
  Wel1flag := 0;
  QSUMflag := 0;
  BYNDflag := 0;
end;

destructor TMnwiImporter.Destroy;
begin
  FMnwiArray.Free;
  inherited;
end;

function TMnwiImporter.GetWellByName(const AWellId: string): TMnwiItem;
begin
  result := FMnwiArray.GetWellByName(AWellId);
end;

procedure TMnwiImporter.HandlePackage;
begin
  if FImportedPackage then
  begin
    Exit;
  end;
  inherited;

end;

procedure TMnwiImporter.ReadData(const ALabel: string);
var
  MNWOBS: integer;
  UNIT_Number: double;
  CONCflag: double;
begin
  inherited;
  if ALabel = 'Wel1flag,QSUMflag,BYNDflag:' then
  begin
    Read(FImporter.FFile, Wel1flag);
    Read(FImporter.FFile, QSUMflag);
    Read(FImporter.FFile, BYNDflag);
    Readln(FImporter.FFile);
  end
  else if ALabel = 'MNWOBS:' then
  begin
    Read(FImporter.FFile, MNWOBS);
    Readln(FImporter.FFile);
    FMnwiArray.ArrayLength := MNWOBS;
  end
  else if ALabel = 'WELLID UNIT QNDflag QBHflag CONCflag:' then
  begin
    Inc(FCurrentWell);
    Readln(FImporter.FFile, FMnwiArray[FCurrentWell].WellID);
    FMnwiArray[FCurrentWell].WellID := Trim(FMnwiArray[FCurrentWell].WellID);
    Read(FImporter.FFile, UNIT_Number);
    Read(FImporter.FFile, FMnwiArray[FCurrentWell].QNDflag);
    Read(FImporter.FFile, FMnwiArray[FCurrentWell].QBHflag);
    Read(FImporter.FFile, CONCflag);
    Readln(FImporter.FFile);
  end
  else if ALabel = 'WELLID UNIT QNDflag QBHflag:' then
  begin
    Inc(FCurrentWell);
    Readln(FImporter.FFile, FMnwiArray[FCurrentWell].WellID);
    FMnwiArray[FCurrentWell].WellID := Trim(FMnwiArray[FCurrentWell].WellID);
    Read(FImporter.FFile, UNIT_Number);
    Read(FImporter.FFile, FMnwiArray[FCurrentWell].QNDflag);
    Read(FImporter.FFile, FMnwiArray[FCurrentWell].QBHflag);
    Readln(FImporter.FFile);
  end
  else
  begin
    Assert(False)
  end;

end;

{ TMnwiArray }

function TMnwiArray.ArrayMemberClass: TArrayMemberClass;
begin
  result := TMnwiItem;
end;

function TMnwiArray.GetItem(Index: integer): TMnwiItem;
begin
  result := TMnwiItem(Objects[Index]);
end;

function TMnwiArray.GetWellByName(const AWellId: string): TMnwiItem;
var
  Index: Integer;
begin
  result := nil;
  for Index := 0 to ArrayLength - 1 do
  begin
    if SameText(Items[Index].WellID, AWellId) then
    begin
      result := Items[Index];
      Exit;
    end;
  end;
end;

{ TSubImporter }

constructor TSubImporter.Create(Importer: TModflow2005Importer;
  NamImporter: TNamImporter);
begin
  inherited Create(Importer, 'SUB');
  FNamImporter := NamImporter;
  FDelayBeds := TSubLayerAssignments.Create;
  FNoDelayBeds := TSubLayerAssignments.Create;
  FMaterialZones := TMaterialZones.Create;
  FSubOutputs:= TSubOutputs.Create;
  FRnbIndex := -1;
  FNoDelayIndex := -1;
  FDelayIndex := -1;
end;

destructor TSubImporter.Destroy;
begin
  FSubOutputs.Free;
  FMaterialZones.Free;
  FNoDelayBeds.Free;
  FDelayBeds.Free;
  inherited;
end;

procedure TSubImporter.ImportMaterialZone(DelayItem: TSubDelayBedLayerItem;
  Index: Integer; var ScreenObject: TScreenObject);
var
VKDataArray: TDataArray;

ElastSS: TDataArray;

InelastSS: TDataArray;

  DataArrayName: string;
  ZoneNumber: Integer;
  ColIndex: Integer;
  RowIndex: Integer;
  ImportedValues: T2DDoubleArray;
  Zones: T2DIntArray;
//  DataArray: TDataArray;
  Zone: TMaterialZone;
begin
  VKDataArray := FModel.GetDataSetByName(
    DelayItem.VerticalHydraulicConductivityDataArrayName);
  Assert(VKDataArray <> nil);

  ElastSS := FModel.GetDataSetByName(
    DelayItem.ElasticSpecificStorageDataArrayName);
  Assert(ElastSS <> nil);

  InelastSS := FModel.GetDataSetByName(
    DelayItem.InelasticSpecificStorageDataArrayName);
  Assert(InelastSS <> nil);
  if FConstNZ[Index].IsConstant then
  begin
    Zone := FMaterialZones[FConstNZ[Index].IntegerValue - 1];
    VKDataArray.Formula := FloatToStr(Zone.VerticalHydraulicConductivity);
    ElastSS.Formula := FloatToStr(Zone.ElasticSpecificStorage);
    InelastSS.Formula := FloatToStr(Zone.InelasticSpecificStorage);
  end
  else
  begin
    Assert(FNZ <> nil);
    Zones := FNZ[Index];
    SetLength(ImportedValues, Length(Zones), Length(Zones[0]));
    for RowIndex := 0 to Length(Zones) - 1 do
    begin
      for ColIndex := 0 to Length(Zones[0]) - 1 do
      begin
        ZoneNumber := Zones[RowIndex, ColIndex];
        if (ZoneNumber < 1) or (ZoneNumber > FMaterialZones.ArrayLength) then
        begin
          ImportedValues[RowIndex, ColIndex] := 0;
        end
        else
        begin
          Zone := FMaterialZones[ZoneNumber];
          ImportedValues[RowIndex, ColIndex] :=
            Zone.VerticalHydraulicConductivity;
        end;
      end;
    end;
    if ScreenObject = nil then
    begin
      CreateOrRetrieveCellCenterScreenObject(ScreenObject);
    end;
    DataArrayName := 'Imported_'
      + DelayItem.VerticalHydraulicConductivityDataArrayName;
    CreateDataArrayAndAssignValues(ScreenObject, DataArrayName, ImportedValues);
    VKDataArray.Formula := DataArrayName;
    
    for RowIndex := 0 to Length(Zones) - 1 do
    begin
      for ColIndex := 0 to Length(Zones[0]) - 1 do
      begin
        ZoneNumber := Zones[RowIndex, ColIndex];
        if (ZoneNumber < 1) or (ZoneNumber > FMaterialZones.ArrayLength) then
        begin
          ImportedValues[RowIndex, ColIndex] := 0;
        end
        else
        begin
          Zone := FMaterialZones[ZoneNumber];
          ImportedValues[RowIndex, ColIndex] := Zone.ElasticSpecificStorage;
        end;
      end;
    end;
    DataArrayName := 'Imported_'
      + DelayItem.ElasticSpecificStorageDataArrayName;
    CreateDataArrayAndAssignValues(ScreenObject, DataArrayName, ImportedValues);
    ElastSS.Formula := DataArrayName;

    for RowIndex := 0 to Length(Zones) - 1 do
    begin
      for ColIndex := 0 to Length(Zones[0]) - 1 do
      begin
        ZoneNumber := Zones[RowIndex, ColIndex];
        if (ZoneNumber < 1) or (ZoneNumber > FMaterialZones.ArrayLength) then
        begin
          ImportedValues[RowIndex, ColIndex] := 0;
        end
        else
        begin
          Zone := FMaterialZones[ZoneNumber];
          ImportedValues[RowIndex, ColIndex] := Zone.InelasticSpecificStorage;
        end;
      end;
    end;
    DataArrayName := 'Imported_'
      + DelayItem.InelasticSpecificStorageDataArrayName;
    CreateDataArrayAndAssignValues(ScreenObject, DataArrayName, ImportedValues);
    InelastSS.Formula := DataArrayName;
  end;
end;

procedure TSubImporter.ImportDZ(DelayItem: TSubDelayBedLayerItem;
  Index: Integer; var ScreenObject: TScreenObject);
var
  DataArrayName: string;
  DataArray: TDataArray;
begin
  DataArray := FModel.GetDataSetByName(
    DelayItem.InterbedEquivalentThicknessDataArrayName);
  Assert(DataArray <> nil);
  if FConstDZ[Index].IsConstant then
  begin
    DataArray.Formula := FloatToStr(FConstDZ[Index].RealValue);
  end
  else
  begin
    Assert(FDZ <> nil);
    if ScreenObject = nil then
    begin
      CreateOrRetrieveCellCenterScreenObject(ScreenObject);
    end;
    DataArrayName := 'Imported_'
      + DelayItem.InterbedEquivalentThicknessDataArrayName;
    CreateDataArrayAndAssignValues(ScreenObject, DataArrayName, FDZ[Index]);
    DataArray.Formula := DataArrayName;
  end;
end;

procedure TSubImporter.ImportDCOM(DelayItem: TSubDelayBedLayerItem;
  Index: Integer; var ScreenObject: TScreenObject);
var
  DataArrayName: string;
  DataArray: TDataArray;
begin
  DataArray := FModel.GetDataSetByName(
    DelayItem.InterbedStartingCompactionDataArrayName);
  Assert(DataArray <> nil);
  if FConstDCOM[Index].IsConstant then
  begin
    DataArray.Formula := FloatToStr(FConstDCOM[Index].RealValue);
  end
  else
  begin
    Assert(FDCOM <> nil);
    if ScreenObject = nil then
    begin
      CreateOrRetrieveCellCenterScreenObject(ScreenObject);
    end;
    DataArrayName := 'Imported_'
      + DelayItem.InterbedStartingCompactionDataArrayName;
    CreateDataArrayAndAssignValues(ScreenObject, DataArrayName, FDCOM[Index]);
    DataArray.Formula := DataArrayName;
  end;
end;

procedure TSubImporter.ImportDHC(DelayItem: TSubDelayBedLayerItem;
  Index: Integer; var ScreenObject: TScreenObject);
var
  DataArrayName: string;
  DataArray: TDataArray;
begin
  DataArray := FModel.GetDataSetByName(
    DelayItem.InterbedPreconsolidationHeadDataArrayName);
  Assert(DataArray <> nil);
  if FConstDHC[Index].IsConstant then
  begin
    DataArray.Formula := FloatToStr(FConstDHC[Index].RealValue);
  end
  else
  begin
    Assert(FDHC <> nil);
    if ScreenObject = nil then
    begin
      CreateOrRetrieveCellCenterScreenObject(ScreenObject);
    end;
    DataArrayName := 'Imported_'
      + DelayItem.InterbedPreconsolidationHeadDataArrayName;
    CreateDataArrayAndAssignValues(ScreenObject, DataArrayName, FDHC[Index]);
    DataArray.Formula := DataArrayName;
  end;
end;

procedure TSubImporter.ImportDStart(DelayItem: TSubDelayBedLayerItem;
  Index: Integer; var ScreenObject: TScreenObject);
var
  DataArray: TDataArray;
  DataArrayName: string;
begin
  DataArray := FModel.GetDataSetByName(
    DelayItem.InterbedStartingHeadDataArrayName);
  Assert(DataArray <> nil);
  if FConstDstart[Index].IsConstant then
  begin
    DataArray.Formula := FloatToStr(FConstDstart[Index].RealValue);
  end
  else
  begin
    Assert(FDstart <> nil);
    if ScreenObject = nil then
    begin
      CreateOrRetrieveCellCenterScreenObject(ScreenObject);
    end;
    DataArrayName := 'Imported_' + DelayItem.InterbedStartingHeadDataArrayName;
    CreateDataArrayAndAssignValues(ScreenObject, DataArrayName, FDstart[Index]);
    DataArray.Formula := DataArrayName;
  end;
end;

procedure TSubImporter.ImportRNB(DelayItem: TSubDelayBedLayerItem;
  Index: Integer; var ScreenObject: TScreenObject);
var
  DataArrayName: string;
  DataArray: TDataArray;
begin
  DataArray := FModel.GetDataSetByName(DelayItem.EquivNumberDataArrayName);
  Assert(DataArray <> nil);
  if FConstRNB[Index].IsConstant then
  begin
    DataArray.Formula := FloatToStr(FConstRNB[Index].RealValue);
  end
  else
  begin
    Assert(FRNB <> nil);
    if ScreenObject = nil then
    begin
      CreateOrRetrieveCellCenterScreenObject(ScreenObject);
    end;
    DataArrayName := 'Imported_' + DelayItem.EquivNumberDataArrayName;
    CreateDataArrayAndAssignValues(ScreenObject, DataArrayName, FRNB[Index]);
    DataArray.Formula := DataArrayName;
  end;
end;

procedure TSubImporter.ImportCom(Index: Integer;
  NoDelayItem: TSubNoDelayBedLayerItem; var ScreenObject: TScreenObject);
var
  DataArray: TDataArray;
  DataArrayName: string;
begin
  DataArray := FModel.GetDataSetByName(
    NoDelayItem.InitialCompactionDataArrayName);
  Assert(DataArray <> nil);
  if FConstCom[Index].IsConstant then
  begin
    DataArray.Formula := FloatToStr(FConstCom[Index].RealValue);
  end
  else
  begin
    Assert(FCom <> nil);
    if ScreenObject = nil then
    begin
      CreateOrRetrieveCellCenterScreenObject(ScreenObject);
    end;
    DataArrayName := 'Imported_' + NoDelayItem.InitialCompactionDataArrayName;
    CreateDataArrayAndAssignValues(ScreenObject, DataArrayName, FCom[Index]);
    DataArray.Formula := DataArrayName;
  end;
end;

procedure TSubImporter.ImportSfv(Index: Integer;
  NoDelayItem: TSubNoDelayBedLayerItem; var ScreenObject: TScreenObject);
var
  DataArray: TDataArray;
  DataArrayName: string;
begin
  DataArray := FModel.GetDataSetByName(
    NoDelayItem.InelasticSkeletalStorageCoefficientDataArrayName);
  Assert(DataArray <> nil);
  if FConstSfv[Index].IsConstant then
  begin
    DataArray.Formula := FloatToStr(FConstSfv[Index].RealValue);
  end
  else
  begin
    Assert(FSfv <> nil);
    if ScreenObject = nil then
    begin
      CreateOrRetrieveCellCenterScreenObject(ScreenObject);
    end;
    DataArrayName := 'Imported_'
      + NoDelayItem.InelasticSkeletalStorageCoefficientDataArrayName;
    CreateDataArrayAndAssignValues(ScreenObject, DataArrayName, FSfv[Index]);
    DataArray.Formula := DataArrayName;
  end;
end;

procedure TSubImporter.ImportSfe(Index: Integer;
  NoDelayItem: TSubNoDelayBedLayerItem; var ScreenObject: TScreenObject);
var
  DataArray: TDataArray;
  DataArrayName: string;
begin
  DataArray := FModel.GetDataSetByName(
    NoDelayItem.ElasticSkeletalStorageCoefficientDataArrayName);
  Assert(DataArray <> nil);
  if FConstSfe[Index].IsConstant then
  begin
    DataArray.Formula := FloatToStr(FConstSfe[Index].RealValue);
  end
  else
  begin
    Assert(FSfe <> nil);
    if ScreenObject = nil then
    begin
      CreateOrRetrieveCellCenterScreenObject(ScreenObject);
    end;
    DataArrayName := 'Imported_'
      + NoDelayItem.ElasticSkeletalStorageCoefficientDataArrayName;
    CreateDataArrayAndAssignValues(ScreenObject, DataArrayName, FSfe[Index]);
    DataArray.Formula := DataArrayName;
  end;
end;

procedure TSubImporter.ImportPreconsolidatonHead(Index: Integer;
  NoDelayItem: TSubNoDelayBedLayerItem; var ScreenObject: TScreenObject);
var
  DataArray: TDataArray;
  DataArrayName: string;
begin
  DataArray := FModel.GetDataSetByName(
    NoDelayItem.PreconsolidationHeadDataArrayName);
  Assert(DataArray <> nil);
  if FConstHC[Index].IsConstant then
  begin
    DataArray.Formula := FloatToStr(FConstHC[Index].RealValue);
  end
  else
  begin
    Assert(FHC <> nil);
    if ScreenObject = nil then
    begin
      CreateOrRetrieveCellCenterScreenObject(ScreenObject);
    end;
    DataArrayName := 'Imported_'
      + NoDelayItem.PreconsolidationHeadDataArrayName;
    CreateDataArrayAndAssignValues(ScreenObject, DataArrayName, FHC[Index]);
    DataArray.Formula := DataArrayName;
  end;
end;

procedure TSubImporter.ImportPrintChoices(SubPackage: TSubPackageSelection);
var
  TimeStepEndTimes: TRealList;
  TimeStepStartTimes: TRealList;
  StressPeriod: TModflowStressPeriod;
  PeriodIndex: Integer;
  TimeStepEndLists: TList;
  TimeStepStartLists: TList;
  Defaults: array[0..12] of Boolean;
  Index: Integer;
  SubPrintItem: TSubPrintItem;
  SubOCItem: TSubOC;
  StepIndex: Integer;
  Time: Double;
  TimeStepLength: Double;
  function GetBoolValue(OCIndex: integer): boolean;
  begin
    if SubOCItem.Ifl[OCIndex] < 0 then
    begin
      result := Defaults[OCIndex];
    end
    else if SubOCItem.Ifl[OCIndex] = 0 then
    begin
      result := False;
      Defaults[OCIndex] := result;
    end
    else
    begin
      result := True;
      Defaults[OCIndex] := result;
    end;
  end;
begin
  SubPackage.PrintChoices.Capacity := FSubOutputs.ArrayLength;
  for Index := 0 to 12 do
  begin
    Defaults[Index] := False;
  end;
  TimeStepStartLists := TObjectList.Create;
  TimeStepEndLists := TObjectList.Create;
  try
    for PeriodIndex := 0 to FModel.ModflowStressPeriods.Count - 1 do
    begin
      StressPeriod := FModel.ModflowStressPeriods[PeriodIndex];
      TimeStepStartTimes := TRealList.Create;
      TimeStepStartLists.Add(TimeStepStartTimes);
      TimeStepEndTimes := TRealList.Create;
      TimeStepEndLists.Add(TimeStepEndTimes);
      TimeStepLength := StressPeriod.LengthOfFirstTimeStep;
      Time := StressPeriod.StartTime;
      for StepIndex := 0 to StressPeriod.NumberOfSteps - 1 do
      begin
        TimeStepStartTimes.Add(Time);
        Time := Time + TimeStepLength;
        TimeStepEndTimes.Add(Time);
        TimeStepLength := TimeStepLength * StressPeriod.TimeStepMultiplier;
      end;
    end;
    for Index := 0 to FSubOutputs.ArrayLength - 1 do
    begin
      SubOCItem := FSubOutputs[Index];
      SubPrintItem := SubPackage.PrintChoices.Add as TSubPrintItem;
      if SubOCItem.ISP1 < 1 then
      begin
        SubOCItem.ISP1 := 1;
      end;
      if SubOCItem.ISP1 > FModel.ModflowStressPeriods.Count then
      begin
        SubOCItem.ISP1 := FModel.ModflowStressPeriods.Count;
      end;
      if SubOCItem.ISP2 < 1 then
      begin
        SubOCItem.ISP2 := 1;
      end;
      if SubOCItem.ISP2 > FModel.ModflowStressPeriods.Count then
      begin
        SubOCItem.ISP2 := FModel.ModflowStressPeriods.Count;
      end;
      TimeStepStartTimes := TimeStepStartLists[SubOCItem.ISP1 - 1];
      TimeStepEndTimes := TimeStepEndLists[SubOCItem.ISP2 - 1];
      if SubOCItem.ITS1 < 1 then
      begin
        SubOCItem.ITS1 := 1;
      end;
      if SubOCItem.ITS1 > TimeStepStartTimes.Count then
      begin
        SubOCItem.ITS1 := TimeStepStartTimes.Count;
      end;
      if SubOCItem.ITS2 < 1 then
      begin
        SubOCItem.ITS2 := 1;
      end;
      if SubOCItem.ITS2 > TimeStepEndTimes.Count then
      begin
        SubOCItem.ITS2 := TimeStepEndTimes.Count;
      end;

      SubPrintItem.StartTime := TimeStepStartTimes[SubOCItem.ITS1 - 1];
      SubPrintItem.EndTime := TimeStepEndTimes[SubOCItem.ITS2 - 1];
      SubPrintItem.PrintSubsidence := GetBoolValue(0);
      SubPrintItem.SaveSubsidence := GetBoolValue(1);
      SubPrintItem.PrintCompactionByModelLayer := GetBoolValue(2);
      SubPrintItem.SaveCompactionByModelLayer := GetBoolValue(3);
      SubPrintItem.PrintCompactionByInterbedSystem := GetBoolValue(4);
      SubPrintItem.SaveCompactionByInterbedSystem := GetBoolValue(5);
      SubPrintItem.PrintVerticalDisplacement := GetBoolValue(6);
      SubPrintItem.SaveVerticalDisplacement := GetBoolValue(7);
      SubPrintItem.PrintCriticalHeadNoDelay := GetBoolValue(8);
      SubPrintItem.SaveCriticalHeadNoDelay := GetBoolValue(9);
      SubPrintItem.PrintCriticalHeadDelay := GetBoolValue(10);
      SubPrintItem.SaveCriticalHeadDelay := GetBoolValue(11);
      SubPrintItem.PrintDelayBudgets := GetBoolValue(12);
    end;
  finally
    TimeStepStartLists.Free;
    TimeStepEndLists.Free;
  end;
end;

procedure TSubImporter.ImportPrintFormat(SubPackage: TSubPackageSelection);
begin
  SubPackage.PrintFormats.SubsidenceFormat := Ifm1;
  SubPackage.PrintFormats.CompactionByModelLayerFormat := Ifm2;
  SubPackage.PrintFormats.CompactionByInterbedSystemFormat := Ifm3;
  SubPackage.PrintFormats.VerticalDisplacementFormat := Ifm4;
  SubPackage.PrintFormats.NoDelayPreconsolidationHeadFormat := Ifm5;
  SubPackage.PrintFormats.DelayPreconsolidationHeadFormat := Ifm6;
end;

procedure TSubImporter.ImportDataSet1(SubPackage: TSubPackageSelection);
var
  FileName: string;
  Position: Integer;
begin
  SubPackage.NumberOfNodes := NN;
  SubPackage.AccelerationParameter1 := AC1;
  SubPackage.AccelerationParameter2 := AC2;
  SubPackage.MinIterations := ITMIN;
  SubPackage.SaveDelayRestart := IDSAVE > 0;
  if IDREST > 0 then
  begin
    Position := FNamImporter.FUnitNumbers.IndexOf(IDREST);
    Assert(Position >= 0);
    FileName := FNamImporter.FFileNames[Position];
    FileName := ExpandFileName(FileName);
    SubPackage.ReadDelayRestartFileName := FileName;
  end;
end;

procedure TSubImporter.ReadDataSet15;
var
  Iun6: Integer;
  Iun5: Integer;
  Iun4: Integer;
  Iun3: Integer;
  Iun2: Integer;
  Iun1: Integer;
begin
  Read(FImporter.FFile, Ifm1);
  Read(FImporter.FFile, Iun1);
  Read(FImporter.FFile, Ifm2);
  Read(FImporter.FFile, Iun2);
  Read(FImporter.FFile, Ifm3);
  Read(FImporter.FFile, Iun3);
  Read(FImporter.FFile, Ifm4);
  Read(FImporter.FFile, Iun4);
  Read(FImporter.FFile, Ifm5);
  Read(FImporter.FFile, Iun5);
  Read(FImporter.FFile, Ifm6);
  Read(FImporter.FFile, Iun6);
end;

procedure TSubImporter.ReadVariableIntArrayForLayer;
var
  Layer: Integer;
  ID: string;
begin
  ReadLn(FImporter.FFile, ID);
  ReadLn(FImporter.FFile, Layer);
//  Dec(Layer);
  ID := Trim(ID);
  Assert(ID = 'MATERIAL ZONE INDICES');
  if FNZ = nil then
  begin
    SetLength(FNZ, FDelayBeds.ArrayLength);
  end;
  SetLength(FNZ[FDelayIndex], FModel.Grid.RowCount, FModel.Grid.ColumnCount);
  ReadVariable2DIntArray(FNZ[FDelayIndex]);
  FConstNZ[FDelayIndex].IsConstant := False;
end;

procedure TSubImporter.ReadConstIntArrayForLayer;
var
  IntegerConstant: Integer;
  ID: string;
  Layer: integer;
begin
  ReadLn(FImporter.FFile, ID);
  Assert(Trim(ID) = 'MATERIAL ZONE INDICES');
  ReadLn(FImporter.FFile, Layer);
//  Dec(Layer);
  ReadLn(FImporter.FFile, IntegerConstant);
  FConstNZ[FDelayIndex].IsConstant := True;
  FConstNZ[FDelayIndex].IntegerValue := IntegerConstant;
end;

procedure TSubImporter.ReadDataSet9;
var
  Zone: TMaterialZone;
  Index: Integer;
begin
  for Index := 0 to FMaterialZones.ArrayLength - 1 do
  begin
    Zone := FMaterialZones[Index];
    Read(FImporter.FFile, Zone.VerticalHydraulicConductivity);
    Read(FImporter.FFile, Zone.ElasticSpecificStorage);
    Read(FImporter.FFile, Zone.InelasticSpecificStorage);
    Readln(FImporter.FFile);
  end;
end;

procedure TSubImporter.ReadVariableRealArrayForLayer;
var
  ThreeDArray: T3DDoubleArray;
  Index: Integer;
  Layer: Integer;
  ID: string;
  ConstArray: TRealConstantRecordArray;
begin
  Index := -1;
  ReadLn(FImporter.FFile, ID);
  ID := Trim(ID);
  ReadLn(FImporter.FFile, Layer);
//  Dec(Layer);
  if ID = StrNUMBEROFBEDSINSY then
  begin
    Inc(FRnbIndex);
    ConstArray := FConstRNB;
    Index := FRnbIndex;
    if FRNB = nil then
    begin
      SetLength(FRNB, FDelayBeds.ArrayLength);
    end;
    ThreeDArray := FRNB;
  end
  else if ID = StrPRECONSOLIDATIONHEA then
  begin
    Inc(FNoDelayIndex);
    ConstArray := FConstHC;
    Index := FNoDelayIndex;
    if FHC = nil then
    begin
      SetLength(FHC, FNoDelayBeds.ArrayLength);
    end;
    ThreeDArray := FHC;
  end
  else if ID = StrELASTICINTERBEDSTO then
  begin
    ConstArray := FConstSfe;
    Index := FNoDelayIndex;
    if FSfe = nil then
    begin
      SetLength(FSfe, FNoDelayBeds.ArrayLength);
    end;
    ThreeDArray := FSfe;
  end
  else if ID = StrVIRGININTERBEDSTOR then
  begin
    ConstArray := FConstSfv;
    Index := FNoDelayIndex;
    if FSfv = nil then
    begin
      SetLength(FSfv, FNoDelayBeds.ArrayLength);
    end;
    ThreeDArray := FSfv;
  end
  else if ID = StrSTARTINGCOMPACTION then
  begin
    ConstArray := FConstCom;
    Index := FNoDelayIndex;
    if FCom = nil then
    begin
      SetLength(FCom, FNoDelayBeds.ArrayLength);
    end;
    ThreeDArray := FCom;
  end
  else if ID = StrDELAYSTARTINGHEAD then
  begin
    Inc(FDelayIndex);
    ConstArray := FConstDstart;
    Index := FDelayIndex;
    if FDstart = nil then
    begin
      SetLength(FDstart, FDelayBeds.ArrayLength);
    end;
    ThreeDArray := FDstart;
  end
  else if ID = StrDELAYPRECOLSOLHEA then
  begin
    ConstArray := FConstDHC;
    Index := FDelayIndex;
    if FDHC = nil then
    begin
      SetLength(FDHC, FDelayBeds.ArrayLength);
    end;
    ThreeDArray := FDHC;
  end
  else if ID = StrDELAYINITIALCOMPAC then
  begin
    ConstArray := FConstDCOM;
    Index := FDelayIndex;
    if FDCom = nil then
    begin
      SetLength(FDCom, FDelayBeds.ArrayLength);
    end;
    ThreeDArray := FDCom;
  end
  else if ID = StrDELAYINTERBEDTHICK then
  begin
    ConstArray := FConstDZ;
    Index := FDelayIndex;
    if FDz = nil then
    begin
      SetLength(FDz, FDelayBeds.ArrayLength);
    end;
    ThreeDArray := FDz;
  end
  else
  begin
    Assert(False);
  end;
  if ThreeDArray[Index] = nil then
  begin
    SetLength(ThreeDArray[Index], FGrid.RowCount, FGrid.ColumnCount);
  end;
  Read2DRealArray(ThreeDArray[Index]);
  ConstArray[Index].IsConstant := False;
end;

procedure TSubImporter.ReadConstRealArrayForLayer;
var
  ConstArray: TRealConstantRecordArray;
  Index: Integer;
  ID: string;
  Layer: Integer;
  Value: Double;
begin
  Index := -1;
  ReadLn(FImporter.FFile, ID);
  ID := Trim(ID);
  ReadLn(FImporter.FFile, Layer);
  Readln(FImporter.FFile, Value);
//  Dec(Layer);
  if ID = StrNUMBEROFBEDSINSY then
  begin
    Inc(FRnbIndex);
    ConstArray := FConstRNB;
    Index := FRnbIndex;
  end
  else if ID = StrPRECONSOLIDATIONHEA then
  begin
    Inc(FNoDelayIndex);
    ConstArray := FConstHC;
    Index := FNoDelayIndex;
  end
  else if ID = StrELASTICINTERBEDSTO then
  begin
    ConstArray := FConstSfe;
    Index := FNoDelayIndex;
  end
  else if ID = StrVIRGININTERBEDSTOR then
  begin
    ConstArray := FConstSfv;
    Index := FNoDelayIndex;
  end
  else if ID = StrSTARTINGCOMPACTION then
  begin
    ConstArray := FConstCom;
    Index := FNoDelayIndex;
  end
  else if ID = StrDELAYSTARTINGHEAD then
  begin
    Inc(FDelayIndex);
    ConstArray := FConstDstart;
    Index := FDelayIndex;
  end
  else if ID = StrDELAYPRECOLSOLHEA then
  begin
    ConstArray := FConstDHC;
    Index := FDelayIndex;
  end
  else if ID = StrDELAYINITIALCOMPAC then
  begin
    ConstArray := FConstDCOM;
    Index := FDelayIndex;
  end
  else if ID = StrDELAYINTERBEDTHICK then
  begin
    ConstArray := FConstDZ;
    Index := FDelayIndex;
  end
  else
  begin
    Assert(False);
  end;
  ConstArray[Index].IsConstant := True;
  ConstArray[Index].RealValue := Value;
end;

procedure TSubImporter.ReadDataSet3;
var
  Index: Integer;
  Item: TSubLayerAssignment;
begin
  for Index := 0 to FDelayBeds.ArrayLength - 1 do
  begin
    Item := FDelayBeds[Index];
    Read(FImporter.FFile, Item.Layer);
  end;
  Readln(FImporter.FFile);
end;

procedure TSubImporter.ReadDataSet2;
var
  Item: TSubLayerAssignment;
  Index: Integer;
begin
  for Index := 0 to FNoDelayBeds.ArrayLength - 1 do
  begin
    Item := FNoDelayBeds[Index];
    Read(FImporter.FFile, Item.Layer);
  end;
  Readln(FImporter.FFile);
end;

procedure TSubImporter.ReadDataSet1;
var
  ISUBCB: Integer;
  ISUBOC: Integer;
  NNDB: Integer;
  NDB: Integer;
  NMZ: Integer;
begin
  Read(FImporter.FFile, ISUBCB);
  Read(FImporter.FFile, ISUBOC);
  Read(FImporter.FFile, NNDB);
  Read(FImporter.FFile, NDB);
  Read(FImporter.FFile, NMZ);
  Read(FImporter.FFile, NN);
  Read(FImporter.FFile, AC1);
  Read(FImporter.FFile, AC2);
  Read(FImporter.FFile, ITMIN);
  Read(FImporter.FFile, IDSAVE);
  Read(FImporter.FFile, IDREST);
  Readln(FImporter.FFile);
  if NNDB < 0 then
  begin
    NNDB := 0;
  end;
  if NDB < 0 then
  begin
    NDB := 0;
  end;
  if NMZ < 0 then
  begin
    NMZ := 0;
  end;
  if ISUBOC < 0 then
  begin
    ISUBOC := 0;
  end;
  FDelayBeds.ArrayLength := NDB;
  FNoDelayBeds.ArrayLength := NNDB;
  FMaterialZones.ArrayLength := NMZ;
  FSubOutputs.ArrayLength := ISUBOC;

  SetLength(FConstRNB, NDB);
  InitializeConstArray(FConstRNB);

  SetLength(FConstHC, NNDB);
  InitializeConstArray(FConstHC);
  SetLength(FConstSfe, NNDB);
  InitializeConstArray(FConstSfe);
  SetLength(FConstSfv, NNDB);
  InitializeConstArray(FConstSfv);
  SetLength(FConstCom, NNDB);
  InitializeConstArray(FConstCom);

  SetLength(FConstDstart, NDB);
  InitializeConstArray(FConstDstart);
  SetLength(FConstDHC, NDB);
  InitializeConstArray(FConstDHC);
  SetLength(FConstDCOM, NDB);
  InitializeConstArray(FConstDCOM);
  SetLength(FConstDZ, NDB);
  InitializeConstArray(FConstDZ);
  SetLength(FConstNZ, NDB);
  InitializeConstIntArray(FConstNZ);
end;

procedure TSubImporter.HandlePackage;
var
  SubPackage: TSubPackageSelection;
  Index: Integer;
  LayerItem: TSubLayerAssignment;
  Group: TLayerGroup;
  NoDelayItem: TSubNoDelayBedLayerItem;
  DelayItem: TSubDelayBedLayerItem;
  ScreenObject: TScreenObject;
begin
  inherited;
  SubPackage := FModel.ModflowPackages.SubPackage;
  SubPackage.IsSelected := True;
  ImportDataSet1(SubPackage);

  if FSubOutputs.ArrayLength > 0 then
  begin
    ImportPrintFormat(SubPackage);
    ImportPrintChoices(SubPackage);
  end;

  ScreenObject := nil;
  for Index := 0 to FNoDelayBeds.ArrayLength - 1 do
  begin
    LayerItem := FNoDelayBeds[Index];
    Group := FModel.LayerStructure[LayerItem.Layer];
    NoDelayItem := Group.SubNoDelayBedLayers.Add;
    NoDelayItem.Name := 'ND_Sys' + IntToStr(Index + 1);
    ImportPreconsolidatonHead(Index, NoDelayItem, ScreenObject);
    ImportSfe(Index, NoDelayItem, ScreenObject);
    ImportSfv(Index, NoDelayItem, ScreenObject);
    ImportCom(Index, NoDelayItem, ScreenObject);
  end;

  for Index := 0 to FDelayBeds.ArrayLength - 1 do
  begin
    LayerItem := FDelayBeds[Index];
    Group := FModel.LayerStructure[LayerItem.Layer];
    DelayItem := Group.SubDelayBedLayers.Add;
    DelayItem.Name := 'D_Sys' + IntToStr(Index + 1);
    ImportRNB(DelayItem, Index, ScreenObject);
    ImportDStart(DelayItem, Index, ScreenObject);
    ImportDHC(DelayItem, Index, ScreenObject);
    ImportDCOM(DelayItem, Index, ScreenObject);
    ImportDZ(DelayItem, Index, ScreenObject);
    ImportMaterialZone(DelayItem, Index, ScreenObject);
  end;
end;

procedure TSubImporter.ReadData(const ALabel: string);
var
  Index: Integer;
  Item: TSubOC;
  OCIndex: Integer;
begin
  inherited;
  if ALabel = 'ISUBCB ISUBOC NNDB NDB NMZ NN AC1 AC2 ITMIN IDSAVE IDREST:' then
  begin
    ReadDataSet1;
  end
  else if ALabel = '(LN(N),N=1,NNDB):' then
  begin
    ReadDataSet2;
  end
  else if ALabel = '(LDN(N),N=1,NDB):' then
  begin
    ReadDataSet3;
  end
  else if ALabel = StrConstant2DRealArrayForLayer then
  begin
    ReadConstRealArrayForLayer;
  end
  else if ALabel = StrVariable2DRealArrayForLayer then
  begin
    ReadVariableRealArrayForLayer;
  end
  else if ALabel = '(DP(N,NP),NP=1,3):' then
  begin
    ReadDataSet9;
  end
  else if ALabel = StrConstant2DIntegerArrayForLayer then
  begin
    ReadConstIntArrayForLayer;
  end
  else if ALabel = StrVariable2DIntegerArrayForLayer then
  begin
    ReadVariableIntArrayForLayer;
  end
  else if ALabel =
    'Ifm1 Iun1 Ifm2 Iun2 Ifm3 Iun3 Ifm4 Iun4 Ifm5 Iun5 Ifm6 Iun6:' then
  begin
    ReadDataSet15;
  end
  else if ALabel = 'ISP1, ISP2, JTS1, JTS2, (IFL(II), II=1,13):' then
  begin
    for Index := 0 to FSubOutputs.ArrayLength - 1 do
    begin
      Item := FSubOutputs[Index];
      Read(FImporter.FFile, Item.ISP1);
      Read(FImporter.FFile, Item.ISP2);
      Read(FImporter.FFile, Item.ITS1);
      Read(FImporter.FFile, Item.ITS2);
      for OCIndex := 0 to Length(Item.Ifl)-1 do
      begin
        Read(FImporter.FFile, Item.Ifl[OCIndex]);
      end;
      ReadLn(FImporter.FFile);
    end;
  end
  else
  begin
    Assert(False);
  end;

end;

{ TSubLayerAssignments }

function TSubLayerAssignments.ArrayMemberClass: TArrayMemberClass;
begin
  result := TSubLayerAssignment;
end;

function TSubLayerAssignments.GetLayer(Index: integer): TSubLayerAssignment;
begin
  result := Objects[Index] as TSubLayerAssignment;
end;

{ TMaterialZones }

function TMaterialZones.ArrayMemberClass: TArrayMemberClass;
begin
  result := TMaterialZone;
end;

function TMaterialZones.GetZone(Index: integer): TMaterialZone;
begin
  result := Objects[Index] as TMaterialZone;
end;

{ TSubOutputs }

function TSubOutputs.ArrayMemberClass: TArrayMemberClass;
begin
  result := TSubOC;
end;

function TSubOutputs.GetOC(Index: integer): TSubOC;
begin
  result := Objects[Index] as TSubOC;
end;

{ TNamImporter }

constructor TNamImporter.Create(Importer: TModflow2005Importer);
begin
  inherited Create(Importer, 'NAM');
  FUnitNumbers := TIntegerList.Create;
  FFileNames := TStringList.Create;
end;

destructor TNamImporter.Destroy;
begin
  FFileNames.Free;
  FUnitNumbers.Free;
  inherited;
end;

procedure TNamImporter.ReadData(const ALabel: string);
var
  UnitNumber: integer;
  FileName: string;
begin
  inherited;
  if ALabel = 'OPENING:' then
  begin
    Read(FImporter.FFile, UnitNumber);
    Readln(FImporter.FFile, FileName);
    FUnitNumbers.Add(UnitNumber);
    FFileNames.Add(FileName);
  end;
end;

end.
