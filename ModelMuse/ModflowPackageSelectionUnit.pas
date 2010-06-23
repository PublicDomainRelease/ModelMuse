unit ModflowPackageSelectionUnit;

interface

uses SysUtils, Classes, GoPhastTypes, OrderedCollectionUnit, DataSetUnit,
  ModpathParticleUnit, ModflowBoundaryDisplayUnit, ScreenObjectUnit,
  ModflowBoundaryUnit;

Type
  TSelectionType = (stCheckBox, stRadioButton);

  // modpath ordinals
  TReferenceTimeOption = (rtoPeriodAndStep, rtoTime);
  TCompositeBudgetFileOption = (cbfGenerateNew, cbfUseOldFile);
  TModpathOutputMode = (mopEndpoints, mopPathline, mopTimeSeries);
  TTimeMethod = (tmIntervals, tmIndividual);
  TTrackingDirection = (tdForward, tdBackward);
  TWeakSink = (wsPassThrough, wsStop, wsThreshold);
  TEndpointWrite = (ewAll, ewInStoppingZone);
  TTimeSeriesMethod = (tsmUniform, tsmIndividual);

  TModflowPackageSelection = class(TPersistent)
  private
    FComments: TStrings;
    FModel: TObject;
    FPackageIdentifier: string;
    FClassification: string;
    FFrame: pointer;
    FNode: pointer;
    FSelectionType: TSelectionType;
    procedure InvalidateModel;
    procedure SetComments(const Value: TStrings);
  protected
    FIsSelected: boolean;
    procedure SetIsSelected(const Value: boolean); virtual;
    procedure DischargeRoutingUpdate;
    function PackageUsed(Sender: TObject): boolean;
    procedure AddTimeList(TimeList: TCustomTimeList);
    procedure RemoveTimeList(TimeList: TCustomTimeList);
    procedure UpdateDisplayUseList(NewUseList: TStringList;
      ParamType: TParameterType; DataIndex: integer; const DisplayName: string);
    procedure UpdateUseList(DataIndex: integer; NewUseList: TStringList;
      Item: TCustomModflowBoundaryItem);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TObject);
    Destructor Destroy; override;
    // @name is a string used to label items related to the package.
    // For example, in @link(TfrmShowHideObjects), @name is used to
    // label the objects that set the boundary conditions for a
    // particular package.
    property PackageIdentifier: string read FPackageIdentifier
      write FPackageIdentifier;
    // @name is used to set up a hierarchical arrangement of
    // @link(PackageIdentifier)s.
    property Classification: string read FClassification write FClassification;
    // @name is used in @link(TfrmModflowPackages) to associate a particular
    // package with a particular TFrame.
    property Frame: pointer read FFrame write FFrame;
    // @name is used in @link(TfrmModflowPackages) to display or change
    // @link(IsSelected).
    property Node: pointer read FNode write FNode;
    property SelectionType: TSelectionType read FSelectionType
      write FSelectionType;
    procedure InvalidateAllTimeLists; virtual;
  published
    property IsSelected: boolean read FIsSelected write SetIsSelected;
    property Comments: TStrings read FComments write SetComments;
  end;

  TPackageClass = class of TModflowPackageSelection;

  TWellPackage = class(TModflowPackageSelection)
  private
    FMfWellPumpage: TModflowBoundaryDisplayTimeList;
    procedure InvalidateMfWellPumpage(Sender: TObject);
    procedure InitializeMfWellPumpage(Sender: TObject);
    procedure GetMfWellUseList(Sender: TObject; NewUseList: TStringList);
  public
    Constructor Create(Model: TObject);
    Destructor Destroy; override;
    property MfWellPumpage: TModflowBoundaryDisplayTimeList
      read FMfWellPumpage;
    procedure InvalidateAllTimeLists; override;
  end;

  TGhbPackage = class(TModflowPackageSelection)
  private
    FMfGhbConductance: TModflowBoundaryDisplayTimeList;
    FMfGhbBoundaryHead: TModflowBoundaryDisplayTimeList;
    procedure GetMfGhbConductanceUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfGhbBoundaryHeadUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure InitializeGhbDisplay(Sender: TObject);
  public
    Constructor Create(Model: TObject);
    Destructor Destroy; override;
    property MfGhbConductance: TModflowBoundaryDisplayTimeList
      read FMfGhbConductance;
    property MfGhbBoundaryHead: TModflowBoundaryDisplayTimeList
      read FMfGhbBoundaryHead;
    procedure InvalidateAllTimeLists; override;
  end;

  TDrnPackage = class(TModflowPackageSelection)
  private
    FMfDrnElevation: TModflowBoundaryDisplayTimeList;
    FMfDrnConductance: TModflowBoundaryDisplayTimeList;
    procedure InitializeDrnDisplay(Sender: TObject);
    procedure GetMfDrnConductanceUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfDrnElevationUseList(Sender: TObject;
      NewUseList: TStringList);
  public
    Constructor Create(Model: TObject);
    Destructor Destroy; override;
    property MfDrnConductance: TModflowBoundaryDisplayTimeList
      read FMfDrnConductance;
    property MfDrnElevation: TModflowBoundaryDisplayTimeList
      read FMfDrnElevation;
    procedure InvalidateAllTimeLists; override;
  end;

  TDrtPackage = class(TModflowPackageSelection)
  private
    FMfDrtReturnFraction: TModflowBoundaryDisplayTimeList;
    FMfDrtConductance: TModflowBoundaryDisplayTimeList;
    FMfDrtElevation: TModflowBoundaryDisplayTimeList;
    procedure InitializeDrtDisplay(Sender: TObject);
    procedure GetMfDrtConductanceUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfDrtElevationUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfDrtReturnFractionUseList(Sender: TObject;
      NewUseList: TStringList);
  public
    Constructor Create(Model: TObject);
    Destructor Destroy; override;
    property MfDrtConductance: TModflowBoundaryDisplayTimeList
      read FMfDrtConductance;
    property MfDrtElevation: TModflowBoundaryDisplayTimeList
      read FMfDrtElevation;
    property MfDrtReturnFraction: TModflowBoundaryDisplayTimeList
      read FMfDrtReturnFraction;
    procedure InvalidateAllTimeLists; override;
  end;

  TRivPackage = class(TModflowPackageSelection)
  private
    FMfRivConductance: TModflowBoundaryDisplayTimeList;
    FMfRivBottom: TModflowBoundaryDisplayTimeList;
    FMfRivStage: TModflowBoundaryDisplayTimeList;
    procedure InitializeRivDisplay(Sender: TObject);
    procedure GetMfRivConductanceUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfRivStageUseList(Sender: TObject; NewUseList: TStringList);
    procedure GetMfRivBottomUseList(Sender: TObject; NewUseList: TStringList);
  public
    Constructor Create(Model: TObject);
    Destructor Destroy; override;
    property MfRivConductance: TModflowBoundaryDisplayTimeList
      read FMfRivConductance;
    property MfRivStage: TModflowBoundaryDisplayTimeList read FMfRivStage;
    property MfRivBottom: TModflowBoundaryDisplayTimeList read FMfRivBottom;
    procedure InvalidateAllTimeLists; override;
  end;

  TChdPackage = class(TModflowPackageSelection)
  private
    FMfChdStartingHead: TModflowBoundaryDisplayTimeList;
    FMfChdEndingHead: TModflowBoundaryDisplayTimeList;
    procedure InitializeChdDisplay(Sender: TObject);
    procedure GetMfChdStartingHeadUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfChdEndingHeadUseList(Sender: TObject;
      NewUseList: TStringList);
  public
    Constructor Create(Model: TObject);
    Destructor Destroy; override;
    property MfChdStartingHead: TModflowBoundaryDisplayTimeList
      read FMfChdStartingHead;
    property MfChdEndingHead: TModflowBoundaryDisplayTimeList
      read FMfChdEndingHead;
    procedure InvalidateAllTimeLists; override;
  end;

  TLpfSelection = class(TModflowPackageSelection)
  private
    FUseSaturatedThickness: boolean;
    FUseConstantCV: boolean;
    FUseCvCorrection: boolean;
    FUseVerticalFlowCorrection: boolean;
    FUseStorageCoefficient: boolean;
    procedure SetUseConstantCV(const Value: boolean);
    procedure SetUseCvCorrection(const Value: boolean);
    procedure SetUseSaturatedThickness(const Value: boolean);
    procedure SetUseVerticalFlowCorrection(const Value: boolean);
    procedure SetUseStorageCoefficient(const Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TObject);
  published
    property UseConstantCV: boolean read FUseConstantCV write SetUseConstantCV;
    property UseSaturatedThickness: boolean read FUseSaturatedThickness
      write SetUseSaturatedThickness;
    property UseCvCorrection: boolean read FUseCvCorrection
      write SetUseCvCorrection default True;
    property UseVerticalFlowCorrection: boolean read FUseVerticalFlowCorrection
      write SetUseVerticalFlowCorrection default True;
    property UseStorageCoefficient: boolean read FUseStorageCoefficient write SetUseStorageCoefficient;
  end;

  THufReferenceChoice = (hrcModelTop, hrcReferenceLayer);

  THufPackageSelection = class(TModflowPackageSelection)
  private
    FSaveHeads: boolean;
    FSaveFlows: boolean;
    FReferenceChoice: THufReferenceChoice;
    procedure SetSaveFlows(const Value: boolean);
    procedure SetSaveHeads(const Value: boolean);
    procedure SetReferenceChoice(const Value: THufReferenceChoice);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TObject);
    procedure InitializeVariables;
  published
    property SaveHeads: boolean read FSaveHeads write SetSaveHeads default True;
    property SaveFlows: boolean read FSaveFlows write SetSaveFlows default True;
    property ReferenceChoice: THufReferenceChoice read FReferenceChoice
      write SetReferenceChoice;
  end;

  TPcgMethod = (pmCholesky, pmPolynomial);
  TPcgPrintSelection = (ppsAll, ppsIterations, ppsNone, ppsFail);
  TPcgEstimateMaxEigenvalue = (peeEstimate, peeDontEstimate);

  TPcgSelection = class(TModflowPackageSelection)
  private
    FRELAX: TRealStorage;
    FNBPOL: TPcgEstimateMaxEigenvalue;
    FMUTPCG: TPcgPrintSelection;
    FRCLOSE: TRealStorage;
    FDAMPPCG: TRealStorage;
    FHCLOSE: TRealStorage;
    FNPCOND: TPcgMethod;
    FMXITER: integer;
    FITER1: integer;
    FIPRPCG: integer;
    FDAMPPCGT: TRealStorage;
    procedure SetDAMPPCG(const Value: TRealStorage);
    procedure SetHCLOSE(const Value: TRealStorage);
    procedure SetIPRPCG(const Value: integer);
    procedure SetITER1(const Value: integer);
    procedure SetMUTPCG(const Value: TPcgPrintSelection);
    procedure SetMXITER(const Value: integer);
    procedure SetNBPOL(const Value: TPcgEstimateMaxEigenvalue);
    procedure SetNPCOND(const Value: TPcgMethod);
    procedure SetRCLOSE(const Value: TRealStorage);
    procedure SetRELAX(const Value: TRealStorage);
    procedure SetDAMPPCGT(const Value: TRealStorage);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TObject);
    Destructor Destroy; override;
    procedure InitializeVariables;
  published
    property MXITER: integer read FMXITER write SetMXITER default 20;
    property ITER1: integer read FITER1 write SetITER1 default 30;
    property NPCOND: TPcgMethod read FNPCOND write SetNPCOND;
    property HCLOSE: TRealStorage read FHCLOSE write SetHCLOSE;
    property RCLOSE: TRealStorage read FRCLOSE write SetRCLOSE;
    property RELAX: TRealStorage read FRELAX write SetRELAX;
    property NBPOL: TPcgEstimateMaxEigenvalue read FNBPOL write SetNBPOL;
    property IPRPCG: integer read FIPRPCG write SetIPRPCG default 1;
    property MUTPCG: TPcgPrintSelection read FMUTPCG write SetMUTPCG;
    property DAMPPCG: TRealStorage read FDAMPPCG write SetDAMPPCG;
    property DAMPPCGT: TRealStorage read FDAMPPCGT write SetDAMPPCGT;
  end;

  TGmgPackageSelection = class(TModflowPackageSelection)
  private
    FISM: integer;
    FCHGLIMIT: TRealStorage;
    FISC: integer;
    FIADAMP: integer;
    FDUP: TRealStorage;
    FRELAX: TRealStorage;
    FIITER: integer;
    FRCLOSE: TRealStorage;
    FIUNITMHC: Boolean;
    FDLOW: TRealStorage;
    FDAMP: TRealStorage;
    FHCLOSE: TRealStorage;
    FMXITER: integer;
    FIOUTGMG: integer;
    procedure SetCHGLIMIT(const Value: TRealStorage);
    procedure SetDAMP(const Value: TRealStorage);
    procedure SetDLOW(const Value: TRealStorage);
    procedure SetDUP(const Value: TRealStorage);
    procedure SetHCLOSE(const Value: TRealStorage);
    procedure SetIADAMP(const Value: integer);
    procedure SetIITER(const Value: integer);
    procedure SetIOUTGMG(const Value: integer);
    procedure SetISC(const Value: integer);
    procedure SetISM(const Value: integer);
    procedure SetIUNITMHC(const Value: Boolean);
    procedure SetMXITER(const Value: integer);
    procedure SetRCLOSE(const Value: TRealStorage);
    procedure SetRELAX(const Value: TRealStorage);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TObject);
    Destructor Destroy; override;
    procedure InitializeVariables;
  published
    property RCLOSE: TRealStorage read FRCLOSE write SetRCLOSE;
    property IITER: integer read FIITER write SetIITER;
    property HCLOSE: TRealStorage read FHCLOSE write SetHCLOSE;
    property MXITER: integer read FMXITER write SetMXITER;
    property DAMP: TRealStorage read FDAMP write SetDAMP;
    property IADAMP: integer read FIADAMP write SetIADAMP;
    property IOUTGMG: integer read FIOUTGMG write SetIOUTGMG;
    property IUNITMHC: Boolean read FIUNITMHC write SetIUNITMHC;
    property ISM: integer read FISM write SetISM;
    property ISC: integer read FISC write SetISC;
    property DUP: TRealStorage read FDUP write SetDUP;
    property DLOW: TRealStorage read FDLOW write SetDLOW;
    property CHGLIMIT: TRealStorage read FCHGLIMIT write SetCHGLIMIT;
    property RELAX: TRealStorage read FRELAX write SetRELAX;
  end;

  TSIPPackageSelection = class(TModflowPackageSelection)
  private
    FNPARM: integer;
    FIPRSIP: integer;
    FHCLOSE: TRealStorage;
    FIPCALC: integer;
    FMXITER: integer;
    FWSEED: TRealStorage;
    FACCL: TRealStorage;
    procedure SetACCL(const Value: TRealStorage);
    procedure SetHCLOSE(const Value: TRealStorage);
    procedure SetIPCALC(const Value: integer);
    procedure SetIPRSIP(const Value: integer);
    procedure SetMXITER(const Value: integer);
    procedure SetNPARM(const Value: integer);
    procedure SetWSEED(const Value: TRealStorage);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TObject);
    Destructor Destroy; override;
    procedure InitializeVariables;
  published
    property MXITER: integer read FMXITER write SetMXITER;
    property NPARM: integer read FNPARM write SetNPARM;
    property ACCL: TRealStorage read FACCL write SetACCL;
    property HCLOSE: TRealStorage read FHCLOSE write SetHCLOSE;
    property IPCALC: integer read FIPCALC write SetIPCALC;
    property WSEED: TRealStorage read FWSEED write SetWSEED;
    property IPRSIP: integer read FIPRSIP write SetIPRSIP;
  end;

  TDE4PackageSelection = class(TModflowPackageSelection)
  private
    FMUTD4: integer;
    FIFREQ: integer;
    FMXUP: integer;
    FITMX: integer;
    FHCLOSE: TRealStorage;
    FMXBW: integer;
    FIPRD4: integer;
    FMXLOW: integer;
    FACCL: TRealStorage;
    procedure SetACCL(const Value: TRealStorage);
    procedure SetHCLOSE(const Value: TRealStorage);
    procedure SetIFREQ(const Value: integer);
    procedure SetIPRD4(const Value: integer);
    procedure SetITMX(const Value: integer);
    procedure SetMUTD4(const Value: integer);
    procedure SetMXBW(const Value: integer);
    procedure SetMXLOW(const Value: integer);
    procedure SetMXUP(const Value: integer);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TObject);
    Destructor Destroy; override;
    procedure InitializeVariables;
  published
    property ITMX: integer read FITMX write SetITMX;
    property MXUP: integer read FMXUP write SetMXUP;
    property MXLOW: integer read FMXLOW write SetMXLOW;
    property MXBW: integer read FMXBW write SetMXBW;
    property IFREQ: integer read FIFREQ write SetIFREQ;
    property MUTD4: integer read FMUTD4 write SetMUTD4;
    property ACCL: TRealStorage read FACCL write SetACCL;
    property HCLOSE: TRealStorage read FHCLOSE write SetHCLOSE;
    property IPRD4: integer read FIPRD4 write SetIPRD4;
  end;

  TLayerOption = (loTop, loSpecified, loTopActive);

  // @name is used for MODFLOW packages in which
  // the user specifies an array of layer numbers
  // to which the package applies.
  TCustomLayerPackageSelection = class(TModflowPackageSelection)
  private
    FLayerOption: TLayerOption;
    FOnLayerChoiceChange: TNotifyEvent;
    procedure SetLayerOption(const Value: TLayerOption);
  public
    Constructor Create(Model: TObject);
    procedure Assign(Source: TPersistent); override;
    property OnLayerChoiceChange: TNotifyEvent read FOnLayerChoiceChange
      write FOnLayerChoiceChange;
  published
    property LayerOption: TLayerOption read FLayerOption
      write SetLayerOption;
  end;

  // @name is used for MODFLOW boundary conditions packages in which,
  // for each stress period, the user specifies an array of layer numbers
  // to which the boundary condition applies.
  TCustomTransientLayerPackageSelection = class(TCustomLayerPackageSelection)
  private
    FTimeVaryingLayers: boolean;
    function GetTimeVaryingLayers: boolean;
    procedure SetTimeVaryingLayers(const Value: boolean);
  protected
    procedure UpdateWithElevationFormula(Formula: string;
      ScreenObject: TScreenObject; NewUseList: TStringList);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TObject);
  published
    property TimeVaryingLayers: boolean
      read GetTimeVaryingLayers write SetTimeVaryingLayers;
  end;

  TEvtPackageSelection = class(TCustomTransientLayerPackageSelection)
  private
    FMfEvtEvapDepth: TModflowBoundaryDisplayTimeList;
    FMfEvtEvapLayer: TModflowBoundaryDisplayTimeList;
    FMfEvtEvapRate: TModflowBoundaryDisplayTimeList;
    FMfEvtEvapSurface: TModflowBoundaryDisplayTimeList;
    procedure GetMfEvtDepthUseList(Sender: TObject; NewUseList: TStringList);
    procedure GetMfEvtLayerUseList(Sender: TObject; NewUseList: TStringList);
    procedure GetMfEvtRateUseList(Sender: TObject; NewUseList: TStringList);
    procedure GetMfEvtSurfaceUseList(Sender: TObject; NewUseList: TStringList);
    procedure InitializeEvtDisplay(Sender: TObject);
    procedure UpdateEvtUseList(NewUseList: TStringList;
      ParamType: TParameterType; DataIndex: integer; const DisplayName: string);
  public
    Constructor Create(Model: TObject);
    Destructor Destroy; override;
    procedure InvalidateAllTimeLists; override;
    property MfEvtEvapDepth: TModflowBoundaryDisplayTimeList
      read FMfEvtEvapDepth;
    property MfEvtEvapLayer: TModflowBoundaryDisplayTimeList
      read FMfEvtEvapLayer;
    property MfEvtEvapRate: TModflowBoundaryDisplayTimeList
      read FMfEvtEvapRate;
    property MfEvtEvapSurface: TModflowBoundaryDisplayTimeList
      read FMfEvtEvapSurface;
    procedure InvalidateMfEvtEvapLayer(Sender: TObject);
  end;

  TRchPackageSelection = class(TCustomTransientLayerPackageSelection)
  private
    FMfRchLayer: TModflowBoundaryDisplayTimeList;
    FMfRchRate: TModflowBoundaryDisplayTimeList;
    procedure GetMfRchRateUseList(Sender: TObject; NewUseList: TStringList);
    procedure GetMfRchLayerUseList(Sender: TObject; NewUseList: TStringList);
    procedure InitializeRchDisplay(Sender: TObject);
  public
    Constructor Create(Model: TObject);
    Destructor Destroy; override;
    property MfRchRate: TModflowBoundaryDisplayTimeList read FMfRchRate;
    property MfRchLayer: TModflowBoundaryDisplayTimeList read FMfRchLayer;
    procedure InvalidateAllTimeLists; override;
    procedure InvalidateMfRchLayer(Sender: TObject);
  end;

  TEtsPackageSelection = class(TCustomTransientLayerPackageSelection)
  private
    FSegmentCount: integer;
    FEtsRateFractionLists: TList;
    FEtsDepthFractionLists: TList;
    FMfEtsEvapLayer: TModflowBoundaryDisplayTimeList;
    FMfEtsEvapRate: TModflowBoundaryDisplayTimeList;
    FMfEtsEvapDepth: TModflowBoundaryDisplayTimeList;
    FMfEtsEvapSurface: TModflowBoundaryDisplayTimeList;
    procedure SetSegmentCount(const Value: integer);
    procedure InitializeEtsDisplay(Sender: TObject);
    procedure GetMfEtsDepthUseList(Sender: TObject; NewUseList: TStringList);
    procedure GetMfEtsLayerUseList(Sender: TObject; NewUseList: TStringList);
    procedure GetMfEtsRateUseList(Sender: TObject; NewUseList: TStringList);
    procedure GetMfEtsSurfaceUseList(Sender: TObject; NewUseList: TStringList);
    procedure GetMfEtsDepthFractionUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfEtsRateFractionUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure UpdateEtsUseList(NewUseList: TStringList;
      ParamType: TParameterType; DataIndex: integer; const DisplayName: string);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TObject);
    Destructor Destroy; override;
    property MfEtsEvapRate: TModflowBoundaryDisplayTimeList
      read FMfEtsEvapRate;
    property MfEtsEvapSurface: TModflowBoundaryDisplayTimeList
      read FMfEtsEvapSurface;
    property MfEtsEvapDepth: TModflowBoundaryDisplayTimeList
      read FMfEtsEvapDepth;
    property MfEtsEvapLayer: TModflowBoundaryDisplayTimeList
      read FMfEtsEvapLayer;
    procedure InvalidateMfEtsEvapLayer(Sender: TObject);
    procedure InvalidateEtsDepthFractions(Sender: TObject);
    procedure InvalidateEtsRateFractions(Sender: TObject);
    procedure UpdateEtsSegmentCount;
    procedure InvalidateAllTimeLists; override;
  published
    property SegmentCount: integer read FSegmentCount
      write SetSegmentCount default 1;
  end;

  TResPackageSelection = class(TCustomTransientLayerPackageSelection)
  private
    FPrintStage: boolean;
    FTableStages: integer;
    procedure SetPrintStage(const Value: boolean);
    procedure SetTableStages(const Value: integer);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TObject);
  published
    property PrintStage: boolean read FPrintStage write SetPrintStage default True;
    property TableStages: integer read FTableStages write SetTableStages default 15;
  end;

  TLakePackageSelection = class(TModflowPackageSelection)
  private
    FPrintLakes: boolean;
    FNumberOfIterations: integer;
    FConvergenceCriterion: double;
    FTheta: double;
    FSurfDepth: TRealStorage;
    procedure SetConvergenceCriterion(const Value: double);
    procedure SetNumberOfIterations(const Value: integer);
    procedure SetPrintLakes(const Value: boolean);
    procedure SetTheta(const Value: double);
    procedure SetSurfDepth(const Value: TRealStorage);
  protected
    procedure SetIsSelected(const Value: boolean); override;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TObject);
    destructor Destroy; override;
  published
    property ConvergenceCriterion: double read FConvergenceCriterion write SetConvergenceCriterion;
    property NumberOfIterations: integer read FNumberOfIterations write SetNumberOfIterations default 100;
    property PrintLakes: boolean read FPrintLakes write SetPrintLakes default True;
    property Theta: double read FTheta write SetTheta;
    property SurfDepth: TRealStorage read FSurfDepth write SetSurfDepth;
  end;

  TSfrParamInstance = class(TOrderedItem)
  private
    FStartTime: double;
    FEndTime: double;
    FParameterInstance: string;
    FParameterName: string;
    procedure SetEndTime(const Value: double);
    procedure SetParameterInstance(const Value: string);
    procedure SetParameterName(const Value: string);
    procedure SetStartTime(const Value: double);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property ParameterName: string read FParameterName write SetParameterName;
    property ParameterInstance: string read FParameterInstance write SetParameterInstance;
    property StartTime: double read FStartTime write SetStartTime;
    property EndTime: double read FEndTime write SetEndTime;
  end;

  TSfrParamInstances = class(TOrderedCollection)
  private
    function GetItems(Index: integer): TSfrParamInstance;
    procedure SetItems(Index: integer; const Value: TSfrParamInstance);
  public
    constructor Create(Model: TObject);
    property Items[Index: integer]: TSfrParamInstance read GetItems write SetItems;
    function ParameterInstanceExists(const ParamName, InstaName: string): boolean;
  end;

  TPrintFlows = (pfNoPrint, pfListing, pfText);

  TSfrPackageSelection = class(TModflowPackageSelection)
  private
    FNstrail: integer;
    FNsfrsets: integer;
    FIsuzn: integer;
    FDleak: double;
    FIsfropt: integer;
    FPrintStreams: boolean;
    FParameterInstances: TSfrParamInstances;
    FAssignParameterInstances: boolean;
    FKinematicRouting: boolean;
    FTimeStepsForKinematicRouting: integer;
    FKinematicRoutingTolerance: double;
    FKinematicRoutingWeight: double;
    FMfSfrDownstreamHydraulicConductivity: TModflowBoundaryDisplayTimeList;
    FMfSfrWidthExponent: TModflowBoundaryDisplayTimeList;
    FMfSfrDiversionSegment: TModflowBoundaryDisplayTimeList;
    FMfSfrVerticalUnsatK: TModflowBoundaryDisplayTimeList;
    FMfSfrInitialWaterContent: TModflowBoundaryDisplayTimeList;
    FMfSfrUpstreamUnsatInitialWaterContent: TModflowBoundaryDisplayTimeList;
    FMfSfrSaturatedWaterContent: TModflowBoundaryDisplayTimeList;
    FMfSfrReachLength: TModflowBoundaryDisplayTimeList;
    FMfSfrSegmentNumber: TModflowBoundaryDisplayTimeList;
    FMfSfrUpstreamUnsaturatedWaterContent: TModflowBoundaryDisplayTimeList;
    FMfSfrDownstreamBrooksCorey: TModflowBoundaryDisplayTimeList;
    FMfSfrBankRoughness: TModflowBoundaryDisplayTimeList;
    FMfSfrUpstreamHydraulicConductivity: TModflowBoundaryDisplayTimeList;
    FMfSfrDepthCoefficient: TModflowBoundaryDisplayTimeList;
    FMfSfrDepthExponent: TModflowBoundaryDisplayTimeList;
    FMfSfrBrooksCorey: TModflowBoundaryDisplayTimeList;
    FMfSfrIcalc: TModflowBoundaryDisplayTimeList;
    FMfSfrUpstreamBrooksCorey: TModflowBoundaryDisplayTimeList;
    FMfSfrDownstreamThickness: TModflowBoundaryDisplayTimeList;
    FMfSfrDownstreamWidth: TModflowBoundaryDisplayTimeList;
    FMfSfrStreamTop: TModflowBoundaryDisplayTimeList;
    FMfSfrOutSegment: TModflowBoundaryDisplayTimeList;
    FMfSfrDownstreamElevation: TModflowBoundaryDisplayTimeList;
    FMfSfrDownstreamUnsatKz: TModflowBoundaryDisplayTimeList;
    FMfSfrUpstreamThickness: TModflowBoundaryDisplayTimeList;
    FMfSfrUpstreamWidth: TModflowBoundaryDisplayTimeList;
    FMfSfrStreamK: TModflowBoundaryDisplayTimeList;
    FMfSfrEvapotranspiration: TModflowBoundaryDisplayTimeList;
    FMfSfrReachNumber: TModflowBoundaryDisplayTimeList;
    FMfSfrDownstreamDepth: TModflowBoundaryDisplayTimeList;
    FMfSfrUpstreamElevation: TModflowBoundaryDisplayTimeList;
    FMfSfrIprior: TModflowBoundaryDisplayTimeList;
    FMfSfrUpstreamUnsatKz: TModflowBoundaryDisplayTimeList;
    FMfSfrPrecipitation: TModflowBoundaryDisplayTimeList;
    FMfSfrFlow: TModflowBoundaryDisplayTimeList;
    FMfSfrDownstreamUnsatInitialWaterContent: TModflowBoundaryDisplayTimeList;
    FMfSfrRunoff: TModflowBoundaryDisplayTimeList;
    FMfSfrStreamSlope: TModflowBoundaryDisplayTimeList;
    FMfSfrDownstreamUnsaturatedWaterContent: TModflowBoundaryDisplayTimeList;
    FMfSfrUpstreamDepth: TModflowBoundaryDisplayTimeList;
    FMfSfrWidthCoefficient: TModflowBoundaryDisplayTimeList;
    FMfSfrChannelRoughness: TModflowBoundaryDisplayTimeList;
    FMfSfrStreamThickness: TModflowBoundaryDisplayTimeList;
    FPrintFlows: TPrintFlows;
    FGageOverallBudget: boolean;
    procedure SetDleak(const Value: double);
    procedure SetIsfropt(const Value: integer);
    procedure SetIsuzn(const Value: integer);
    procedure SetNsfrsets(const Value: integer);
    procedure SetNstrail(const Value: integer);
    procedure SetPrintStreams(const Value: boolean);
    procedure SetParameterInstances(const Value: TSfrParamInstances);
    procedure SetKinematicRouting(const Value: boolean);
    procedure SetKinematicRoutingTolerance(const Value: double);
    procedure SetTimeStepsForKinematicRouting(const Value: integer);
    procedure SetKinematicRoutingWeight(const Value: double);
    procedure InitializeSfrDisplay(Sender: TObject);
    // @name is an empty procedure used for SFR segment numbers, reach numbers
    // and ICALC.  None of those can depend on any data set so the Use-List for
    // them should be empty.
    procedure GetMfSfrUseList(Sender: TObject; NewUseList: TStringList);
    procedure GetMfSfrReachLengthUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrReachUseList(DataIndex: integer; NewUseList: TStringList);
    function ModflowSfrSpatialVariationSelected(Sender: TObject): boolean;
    procedure GetMfSfrStreamTopUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrStreamSlopeUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrStreamThicknessUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrStreamKUseList(Sender: TObject; NewUseList: TStringList);
    function ModflowSfrUnsatSpatialVariationSelected(Sender: TObject): boolean;
    procedure GetMfSfrStreamSatWatContentUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrStreamInitialWatContentUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrBrooksCoreyUseList(Sender: TObject;
      NewUseList: TStringList);
    function ModflowSfrUnsatKzSpatialVariationSelected(
      Sender: TObject): boolean;
    procedure GetMfSfrUnsatKzUseList(Sender: TObject; NewUseList: TStringList);
    procedure GetMfSfrFlowUseList(Sender: TObject; NewUseList: TStringList);
    procedure GetMfSfrFlowItemUseList(DataIndex: integer;
      NewUseList: TStringList);
    procedure GetMfSfrRunoffUseList(Sender: TObject; NewUseList: TStringList);
    procedure GetMfSfrPrecipitationUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrEvapotranspirationUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrChannelRoughnessUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrChanelItemUseList(DataIndex: integer;
      NewUseList: TStringList);
    procedure GetMfSfrBankRoughnessUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrDepthCoefficientUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrEquationItemUseList(DataIndex: integer;
      NewUseList: TStringList);
    procedure GetMfSfrDepthExponentUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrWidthCoefficientUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrWidthExponentUseList(Sender: TObject;
      NewUseList: TStringList);
    function ModflowSfrUpstreamDownstreamUsed(Sender: TObject): boolean;
    procedure GetMfSfrUpstreamHydraulicConductivityUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrUpstreamItemUseList(DataIndex: integer;
      NewUseList: TStringList);
    procedure GetMfSfrDownstreamHydraulicConductivityUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrDownstreamItemUseList(DataIndex: integer;
      NewUseList: TStringList);
    procedure GetMfSfrUpstreamWidthUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrDownstreamWidthUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrUpstreamThicknessUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrDownstreamThicknessUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrUpstreamElevationUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrDownstreamElevationUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrUpstreamDepthUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrDownstreamDepthUseList(Sender: TObject;
      NewUseList: TStringList);
    function ModflowSfrUpstreamDownstreamUnsatUsed(Sender: TObject): boolean;
    procedure GetMfSfrUpstreamSatWatContentUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrUpstreamUnsatItemUseList(DataIndex: integer;
      NewUseList: TStringList);
    procedure GetMfSfrDownstreamSatWatContentUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrDownstreamUnsatItemUseList(DataIndex: integer;
      NewUseList: TStringList);
    procedure GetMfSfrUpstreamInitialWatContentUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrDownstreamInitialWatContentUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrUpstreamBrooksCoreyUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrDownstreamBrooksCoreyUseList(Sender: TObject;
      NewUseList: TStringList);
    function ModflowSfrUpstreamDownstreamUnsatKzUsed(Sender: TObject): boolean;
    procedure GetMfSfrUpstreamUnsatKzUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfSfrDownstreamUnsatKzUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure SetPrintFlows(const Value: TPrintFlows);
    procedure SetGageOverallBudget(const Value: boolean);
  protected
    procedure SetIsSelected(const Value: boolean); override;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TObject);
    destructor Destroy; override;
    Function StreamConstant: double;
    property AssignParameterInstances: boolean read FAssignParameterInstances
      write FAssignParameterInstances;
    property MfSfrSegmentNumber: TModflowBoundaryDisplayTimeList
      read FMfSfrSegmentNumber;
    property MfSfrReachNumber: TModflowBoundaryDisplayTimeList
      read FMfSfrReachNumber;
    property MfSfrIcalc: TModflowBoundaryDisplayTimeList read FMfSfrIcalc;
    property MfSfrReachLength: TModflowBoundaryDisplayTimeList
      read FMfSfrReachLength;
    property MfSfrStreamTop : TModflowBoundaryDisplayTimeList
      read FMfSfrStreamTop;
    property MfSfrStreamSlope : TModflowBoundaryDisplayTimeList
      read FMfSfrStreamSlope;
    property MfSfrStreamThickness : TModflowBoundaryDisplayTimeList
      read FMfSfrStreamThickness;
    property MfSfrStreamK : TModflowBoundaryDisplayTimeList
      read FMfSfrStreamK;
    property MfSfrSaturatedWaterContent : TModflowBoundaryDisplayTimeList
      read FMfSfrSaturatedWaterContent;
    property MfSfrInitialWaterContent : TModflowBoundaryDisplayTimeList
      read FMfSfrInitialWaterContent;
    property MfSfrBrooksCorey : TModflowBoundaryDisplayTimeList
      read FMfSfrBrooksCorey;
    property MfSfrVerticalUnsatK : TModflowBoundaryDisplayTimeList
      read FMfSfrVerticalUnsatK;
    property MfSfrOutSegment : TModflowBoundaryDisplayTimeList
      read FMfSfrOutSegment;
    property MfSfrDiversionSegment : TModflowBoundaryDisplayTimeList
      read FMfSfrDiversionSegment;
    property MfSfrIprior: TModflowBoundaryDisplayTimeList read FMfSfrIprior;
    property MfSfrFlow: TModflowBoundaryDisplayTimeList read FMfSfrFlow;
    property MfSfrRunoff: TModflowBoundaryDisplayTimeList read FMfSfrRunoff;
    property MfSfrPrecipitation: TModflowBoundaryDisplayTimeList
      read FMfSfrPrecipitation;
    property MfSfrEvapotranspiration: TModflowBoundaryDisplayTimeList
      read FMfSfrEvapotranspiration;
    property MfSfrChannelRoughness: TModflowBoundaryDisplayTimeList
      read FMfSfrChannelRoughness;
    property MfSfrBankRoughness: TModflowBoundaryDisplayTimeList
      read FMfSfrBankRoughness;
    property MfSfrDepthCoefficient: TModflowBoundaryDisplayTimeList
      read FMfSfrDepthCoefficient;
    property MfSfrDepthExponent: TModflowBoundaryDisplayTimeList
      read FMfSfrDepthExponent;
    property MfSfrWidthCoefficient: TModflowBoundaryDisplayTimeList
      read FMfSfrWidthCoefficient;
    property MfSfrWidthExponent: TModflowBoundaryDisplayTimeList
      read FMfSfrWidthExponent;
    property MfSfrUpstreamHydraulicConductivity: TModflowBoundaryDisplayTimeList
      read FMfSfrUpstreamHydraulicConductivity;
    property MfSfrDownstreamHydraulicConductivity:
      TModflowBoundaryDisplayTimeList
      read FMfSfrDownstreamHydraulicConductivity;
    property MfSfrUpstreamWidth: TModflowBoundaryDisplayTimeList
      read FMfSfrUpstreamWidth;
    property MfSfrDownstreamWidth: TModflowBoundaryDisplayTimeList
      read FMfSfrDownstreamWidth;
    property MfSfrUpstreamThickness: TModflowBoundaryDisplayTimeList
      read FMfSfrUpstreamThickness;
    property MfSfrDownstreamThickness: TModflowBoundaryDisplayTimeList
      read FMfSfrDownstreamThickness;
    property MfSfrUpstreamElevation: TModflowBoundaryDisplayTimeList
      read FMfSfrUpstreamElevation;
    property MfSfrDownstreamElevation: TModflowBoundaryDisplayTimeList
      read FMfSfrDownstreamElevation;
    property MfSfrUpstreamDepth: TModflowBoundaryDisplayTimeList
      read FMfSfrUpstreamDepth;
    property MfSfrDownstreamDepth: TModflowBoundaryDisplayTimeList
      read FMfSfrDownstreamDepth;
    property MfSfrUpstreamUnsaturatedWaterContent:
      TModflowBoundaryDisplayTimeList
      read FMfSfrUpstreamUnsaturatedWaterContent;
    property MfSfrDownstreamUnsaturatedWaterContent:
      TModflowBoundaryDisplayTimeList
      read FMfSfrDownstreamUnsaturatedWaterContent;
    property MfSfrUpstreamUnsatInitialWaterContent:
      TModflowBoundaryDisplayTimeList
      read FMfSfrUpstreamUnsatInitialWaterContent;
    property MfSfrDownstreamUnsatInitialWaterContent:
      TModflowBoundaryDisplayTimeList
      read FMfSfrDownstreamUnsatInitialWaterContent;
    property MfSfrUpstreamBrooksCorey: TModflowBoundaryDisplayTimeList
      read FMfSfrUpstreamBrooksCorey;
    property MfSfrDownstreamBrooksCorey: TModflowBoundaryDisplayTimeList
      read FMfSfrDownstreamBrooksCorey;
    property MfSfrUpstreamUnsatKz: TModflowBoundaryDisplayTimeList
      read FMfSfrUpstreamUnsatKz;
    property MfSfrDownstreamUnsatKz: TModflowBoundaryDisplayTimeList
      read FMfSfrDownstreamUnsatKz;
    procedure InvalidateMfSfrSegmentReachAndIcalc(Sender: TObject);
    procedure InvalidateAllTimeLists; override;
  published
    property Dleak: double read FDleak write SetDleak;
    property Isfropt: integer read FIsfropt write SetIsfropt;
    property Nstrail: integer read FNstrail write SetNstrail;
    property Isuzn: integer read FIsuzn write SetIsuzn;
    property Nsfrsets: integer read FNsfrsets write SetNsfrsets;
    // @name is for backwards compatibility.
    // PrintStreams is no longer used as of version 2.0.0.10.
    property PrintStreams: boolean read FPrintStreams
      write SetPrintStreams Stored False;
    property PrintFlows: TPrintFlows read FPrintFlows write SetPrintFlows
      default pfListing;
    property ParameterInstances: TSfrParamInstances read FParameterInstances
      write SetParameterInstances stored FIsSelected;
    property KinematicRouting: boolean read FKinematicRouting write SetKinematicRouting;
    property TimeStepsForKinematicRouting: integer
      read FTimeStepsForKinematicRouting write SetTimeStepsForKinematicRouting
      default 1;
    property KinematicRoutingTolerance: double read FKinematicRoutingTolerance
      write SetKinematicRoutingTolerance;
    property KinematicRoutingWeight: double read FKinematicRoutingWeight
      write SetKinematicRoutingWeight stored True;
    property GageOverallBudget: boolean read FGageOverallBudget
      write SetGageOverallBudget;
  end;

  TUzfPackageSelection = class(TCustomLayerPackageSelection)
  private
    FSimulateET: boolean;
    FNumberOfWaveSets: integer;
    FRouteDischargeToStreams: boolean;
    FVerticalKSource: integer;
    FNumberOfTrailingWaves: integer;
    FPrintSummary: integer;
    FDepthOfUndulations: double;
    FMfUzfInfiltration: TModflowBoundaryDisplayTimeList;
    FMfUzfExtinctionDepth: TModflowBoundaryDisplayTimeList;
    FMfUzfWaterContent: TModflowBoundaryDisplayTimeList;
    FMfUzfEtDemand: TModflowBoundaryDisplayTimeList;
    procedure SetNumberOfTrailingWaves(const Value: integer);
    procedure SetNumberOfWaveSets(const Value: integer);
    procedure SetRouteDischargeToStreams(const Value: boolean);
    procedure SetSimulateET(const Value: boolean);
    procedure SetVerticalKSource(const Value: integer);
    procedure SetPrintSummary(const Value: integer);
    procedure SetDepthOfUndulations(const Value: double);
    procedure InitializeUzfDisplay(Sender: TObject);
    procedure GetMfUzfInfiltrationUseList(Sender: TObject; NewUseList: TStringList);
    procedure GetMfUzfExtinctionDepthUseList(Sender: TObject;
      NewUseList: TStringList);
    function ModflowUztEtSimulated(Sender: TObject): boolean;
    procedure GetMfUzfEtDemandUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfUzfWaterContentUseList(Sender: TObject;
      NewUseList: TStringList);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TObject);
    Destructor Destroy; override;
    property MfUzfInfiltration: TModflowBoundaryDisplayTimeList
      read FMfUzfInfiltration;
    property MfUzfEtDemand: TModflowBoundaryDisplayTimeList
      read FMfUzfEtDemand;
    property MfUzfExtinctionDepth: TModflowBoundaryDisplayTimeList
      read FMfUzfExtinctionDepth;
    property MfUzfWaterContent: TModflowBoundaryDisplayTimeList 
      read FMfUzfWaterContent;
    procedure InvalidateAllTimeLists; override;
  published
    property VerticalKSource: integer read FVerticalKSource write SetVerticalKSource;
    property RouteDischargeToStreams: boolean read FRouteDischargeToStreams write SetRouteDischargeToStreams;
    property SimulateET: boolean read FSimulateET write SetSimulateET;
    property NumberOfTrailingWaves: integer read FNumberOfTrailingWaves write SetNumberOfTrailingWaves;
    property NumberOfWaveSets: integer read FNumberOfWaveSets write SetNumberOfWaveSets;
    property PrintSummary: integer read FPrintSummary write SetPrintSummary;
    property DepthOfUndulations: double read FDepthOfUndulations write SetDepthOfUndulations;
  end;

  THobPackageSelection = class(TModflowPackageSelection)
  private
    FDryHead: double;
    procedure SetDryHead(const Value: double);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TObject);
    procedure InitializeVariables;
  published
    property DryHead: double read FDryHead write SetDryHead;
  end;

  TSurfaceApplicationPosition = (sapInternal, sapVertical);


  TModpathSelection = class(TModflowPackageSelection)
  private
    FMaximumSize: integer;
    FRCH_Source: TSurfaceApplicationPosition;
    FEVT_Sink: TSurfaceApplicationPosition;
    FCompact: boolean;
    FBinary: boolean;
    FEndingTime: Real;
    FBeginningTime: Real;
    FReferenceTime: real;
    FOutputMode: TModpathOutputMode;
    FOutputTimes: TModpathTimes;
    FStopAfterMaxTime: boolean;
    FMaxTime: real;
    FTrackingDirection: TTrackingDirection;
    FWeakSink: TWeakSink;
    FWeakSinkThreshold: real;
    FStopInZone: boolean;
    FStopZoneNumber: integer;
    FEndpointWrite: TEndpointWrite;
    FComputeBudgetInAllCells: boolean;
    FErrorTolerance: real;
    FSummarize: boolean;
    FMakeBigBudgetFile: boolean;
    FTimeSeriesMaxCount: integer;
    FTimeSeriesInterval: double;
    FTimeSeriesMethod: TTimeSeriesMethod;
    FBackwardsTrackingReleaseTime: double;
    procedure SetMaximumSize(const Value: integer);
    procedure SetEVT_Sink(const Value: TSurfaceApplicationPosition);
    procedure SetRCH_Source(const Value: TSurfaceApplicationPosition);
    procedure SetBinary(const Value: boolean);
    procedure SetCompact(const Value: boolean);
    procedure SetBeginningTime(const Value: Real);
    procedure SetEndingTime(const Value: Real);
    procedure SetReferenceTime(const Value: real);
    procedure SetOutputMode(const Value: TModpathOutputMode);
    procedure SetOutputTimes(const Value: TModpathTimes);
    procedure SetStopAfterMaxTime(const Value: boolean);
    procedure SetMaxTime(const Value: real);
    procedure SetTrackingDirection(const Value: TTrackingDirection);
    procedure SetWeakSink(const Value: TWeakSink);
    procedure SetWeakSinkThreshold(const Value: real);
    procedure SetStopInZone(const Value: boolean);
    procedure SetStopZoneNumber(const Value: integer);
    procedure SetEndpointWrite(const Value: TEndpointWrite);
    procedure SetComputeBudgetInAllCells(const Value: boolean);
    procedure SetErrorTolerance(const Value: real);
    procedure SetSummarize(const Value: boolean);
    procedure SetMakeBigBudgetFile(const Value: boolean);
    procedure SetTimeSeriesInterval(const Value: double);
    procedure SetTimeSeriesMaxCount(const Value: integer);
    procedure SetTimeSeriesMethod(const Value: TTimeSeriesMethod);
    procedure SetBackwardsTrackingReleaseTime(const Value: double);
  public
    Constructor Create(Model: TObject);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure InitializeVariables;
    function ShouldCreateTimeFile: boolean;
  published
    property MaximumSize: integer read FMaximumSize write SetMaximumSize;
    property EVT_Sink: TSurfaceApplicationPosition
      read FEVT_Sink write SetEVT_Sink default sapVertical;
    property RCH_Source: TSurfaceApplicationPosition
      read FRCH_Source write SetRCH_Source default sapVertical;
    property Compact: boolean read FCompact write SetCompact;
    property Binary: boolean read FBinary write SetBinary;
    property BeginningTime: Real read FBeginningTime write SetBeginningTime;
    property EndingTime: Real read FEndingTime write SetEndingTime;
    property ReferenceTime: real read FReferenceTime write SetReferenceTime;
    property OutputMode: TModpathOutputMode read FOutputMode
      write SetOutputMode;
    property OutputTimes: TModpathTimes read FOutputTimes write SetOutputTimes;
    property StopAfterMaxTime: boolean read FStopAfterMaxTime
      write SetStopAfterMaxTime;
    property MaxTime: real read FMaxTime write SetMaxTime;
    property TrackingDirection: TTrackingDirection read FTrackingDirection
      write SetTrackingDirection;
    property WeakSink: TWeakSink read FWeakSink write SetWeakSink;
    property WeakSinkThreshold: real read FWeakSinkThreshold
      write SetWeakSinkThreshold;
    property StopInZone: boolean read FStopInZone write SetStopInZone;
    property StopZoneNumber: integer read FStopZoneNumber
      write SetStopZoneNumber;
    property EndpointWrite: TEndpointWrite read FEndpointWrite
      write SetEndpointWrite;
    property ComputeBudgetInAllCells: boolean read FComputeBudgetInAllCells
      write SetComputeBudgetInAllCells;
    property ErrorTolerance: real read FErrorTolerance write SetErrorTolerance;
    property Summarize: boolean read FSummarize write SetSummarize;
    property MakeBigBudgetFile: boolean read FMakeBigBudgetFile
      write SetMakeBigBudgetFile default True;
    // new properties
    property TimeSeriesMethod: TTimeSeriesMethod read FTimeSeriesMethod
      write SetTimeSeriesMethod;
    property TimeSeriesInterval: double read FTimeSeriesInterval
      write SetTimeSeriesInterval stored True;
    property TimeSeriesMaxCount: integer read FTimeSeriesMaxCount
      write SetTimeSeriesMaxCount;
    property BackwardsTrackingReleaseTime: double read FBackwardsTrackingReleaseTime write SetBackwardsTrackingReleaseTime;
  end;

  ZZoneItem = class(TOrderedItem)
  private
    FZoneNumber: integer;
    procedure SetZoneNumber(const Value: integer);
  public
    procedure Assign(Source: TPersistent); override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  published
    property ZoneNumber: integer read FZoneNumber write SetZoneNumber;
  end;

  TCompositeZone = class(TOrderedCollection)
  private
    FZoneName: string;
    function GetItem(Index: integer): ZZoneItem;
    procedure SetItem(Index: integer; const Value: ZZoneItem);
    procedure SetZoneName(Value: string);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Model: TObject);
    function IsSame(AnOrderedCollection: TOrderedCollection): boolean; override;
    property Items[Index: integer]: ZZoneItem read GetItem
      write SetItem; default;
  published
    property ZoneName: string read FZoneName write SetZoneName;
  end;

  TCompositeZoneItem = class(TOrderedItem)
  private
    FCompositeZone: TCompositeZone;
    procedure SetCompositeZone(const Value: TCompositeZone);
  public
    procedure Assign(Source: TPersistent); override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
  published
    property CompositeZone: TCompositeZone read FCompositeZone write SetCompositeZone;
  end;

  TCompositeZoneCollection = class(TOrderedCollection)
  private
    function GetItem(Index: integer): TCompositeZoneItem;
    procedure SetItem(Index: integer; const Value: TCompositeZoneItem);
  public
    constructor Create(Model: TObject);
    property Items[Index: integer]: TCompositeZoneItem read GetItem
      write SetItem; default;
  end;

  TZoneBudgetSelect = class(TModflowPackageSelection)
  private
    FCompositeZones: TCompositeZoneCollection;
    FExportZBLST: boolean;
    FExportCSV2: boolean;
    FExportCSV: boolean;
    procedure SetCompositeZones(const Value: TCompositeZoneCollection);
    procedure SetExportCSV(const Value: boolean);
    procedure SetExportCSV2(const Value: boolean);
    procedure SetExportZBLST(const Value: boolean);
  public
    procedure InitializeVariables;
    Constructor Create(Model: TObject);
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property CompositeZones: TCompositeZoneCollection read FCompositeZones write SetCompositeZones;
    property ExportZBLST: boolean read FExportZBLST write SetExportZBLST Stored True;
    property ExportCSV: boolean read FExportCSV write SetExportCSV Stored True;
    property ExportCSV2: boolean read FExportCSV2 write SetExportCSV2 Stored True;
  end;

  TMnw2PrintOption = (mpoNone, mpoIntermediate, mpoMost);

  TMultinodeWellSelection = class(TModflowPackageSelection)
  private
    FPrintOption: TMnw2PrintOption;
    FSummarizeByWell: Boolean;
    FCreateWellFile: Boolean;
    FSummarizeByNode: Boolean;
    FMfMnwWellRadius: TModflowBoundaryDisplayTimeList;
    FMfMnwCellToWellConductance: TModflowBoundaryDisplayTimeList;
    FMfMnwP: TModflowBoundaryDisplayTimeList;
    FMfMnwSkinK: TModflowBoundaryDisplayTimeList;
    FMfMnwSkinRadius: TModflowBoundaryDisplayTimeList;
    FMfMnwPartialPenetration: TModflowBoundaryDisplayTimeList;
    FMfMnwB: TModflowBoundaryDisplayTimeList;
    FMfMnwC: TModflowBoundaryDisplayTimeList;
    procedure SetPrintOption(const Value: TMnw2PrintOption);
    procedure SetCreateWellFile(const Value: Boolean);
    procedure SetSummarizeByNode(const Value: Boolean);
    procedure SetSummarizeByWell(const Value: Boolean);
    procedure InitializeMnw2Display(Sender: TObject);
    procedure GetMfMnwWellRadiusUseList(Sender: TObject; NewUseList: TStringList);
    procedure GetMfMnwSkinRadiusUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfMnwUseList(Sender: TObject; NewUseList: TStringList;
      DataIndex: integer);
    procedure GetMfMnwSkinKUseList(Sender: TObject; NewUseList: TStringList);
    procedure GetMfMnwBUseList(Sender: TObject; NewUseList: TStringList);
    procedure GetMfMnwCUseList(Sender: TObject; NewUseList: TStringList);
    procedure GetMfMnwPUseList(Sender: TObject; NewUseList: TStringList);
    procedure GetMfMnwCellToWellConductanceUseList(Sender: TObject;
      NewUseList: TStringList);
    procedure GetMfMnwPartialPenetrationUseList(Sender: TObject;
      NewUseList: TStringList);
  public
    Constructor Create(Model: TObject);
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property MfMnwWellRadius: TModflowBoundaryDisplayTimeList
      read FMfMnwWellRadius;
    property MfMnwSkinRadius: TModflowBoundaryDisplayTimeList
      read FMfMnwSkinRadius;
    property MfMnwSkinK: TModflowBoundaryDisplayTimeList read FMfMnwSkinK;
    property MfMnwB: TModflowBoundaryDisplayTimeList read FMfMnwB;
    property MfMnwC: TModflowBoundaryDisplayTimeList read FMfMnwC;
    property MfMnwP: TModflowBoundaryDisplayTimeList read FMfMnwP;
    property MfMnwCellToWellConductance: TModflowBoundaryDisplayTimeList
      read FMfMnwCellToWellConductance;
    property MfMnwPartialPenetration: TModflowBoundaryDisplayTimeList
      read FMfMnwPartialPenetration;
  published
    property PrintOption: TMnw2PrintOption read FPrintOption
      write SetPrintOption default mpoMost;
    property CreateWellFile: Boolean read FCreateWellFile write SetCreateWellFile;
    property SummarizeByWell: Boolean read FSummarizeByWell write SetSummarizeByWell;
    property SummarizeByNode: Boolean read FSummarizeByNode write SetSummarizeByNode;
  end;

  TSubPrintItem = class(TOrderedItem)
  private
    FPrintVerticalDisplacement: boolean;
    FSaveCompactionByInterbedSystem: boolean;
    FSaveCriticalHeadDelay: boolean;
    FStartTime: double;
    FPrintCompactionByInterbedSystem: boolean;
    FPrintCriticalHeadDelay: boolean;
    FSaveCriticalHeadNoDelay: boolean;
    FPrintDelayBudgets: boolean;
    FEndTime: double;
    FPrintCriticalHeadNoDelay: boolean;
    FSaveSubsidence: boolean;
    FSaveCompactionByModelLayer: boolean;
    FPrintSubsidence: boolean;
    FPrintCompactionByModelLayer: boolean;
    FSaveVerticalDisplacement: boolean;
    procedure SetEndTime(const Value: double);
    procedure SetPrintCompactionByInterbedSystem(const Value: boolean);
    procedure SetPrintCompactionByModelLayer(const Value: boolean);
    procedure SetPrintCriticalHeadDelay(const Value: boolean);
    procedure SetPrintCriticalHeadNoDelay(const Value: boolean);
    procedure SetPrintDelayBudgets(const Value: boolean);
    procedure SetPrintSubsidence(const Value: boolean);
    procedure SetPrintVerticalDisplacement(const Value: boolean);
    procedure SetSaveCompactionByInterbedSystem(const Value: boolean);
    procedure SetSaveCompactionByModelLayer(const Value: boolean);
    procedure SetSaveCriticalHeadDelay(const Value: boolean);
    procedure SetSaveCriticalHeadNoDelay(const Value: boolean);
    procedure SetSaveSubsidence(const Value: boolean);
    procedure SetStartTime(const Value: double);
    procedure SetSaveVerticalDisplacement(const Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  published
    property StartTime: double read FStartTime write SetStartTime;
    property EndTime: double read FEndTime write SetEndTime;
    // Ifl1
    property PrintSubsidence: boolean read FPrintSubsidence
      write SetPrintSubsidence;
    // Ifl2
    property SaveSubsidence: boolean read FSaveSubsidence
      write SetSaveSubsidence;
    // Ifl3
    property PrintCompactionByModelLayer: boolean
      read FPrintCompactionByModelLayer write SetPrintCompactionByModelLayer;
    // Ifl4
    property SaveCompactionByModelLayer: boolean
      read FSaveCompactionByModelLayer write SetSaveCompactionByModelLayer;
    // Ifl5
    property PrintCompactionByInterbedSystem: boolean
      read FPrintCompactionByInterbedSystem
      write SetPrintCompactionByInterbedSystem;
    // Ifl6
    property SaveCompactionByInterbedSystem: boolean
      read FSaveCompactionByInterbedSystem
      write SetSaveCompactionByInterbedSystem;
    // Ifl7
    property PrintVerticalDisplacement: boolean read FPrintVerticalDisplacement
      write SetPrintVerticalDisplacement;
    // Ifl8
    property SaveVerticalDisplacement: boolean read FSaveVerticalDisplacement
      write SetSaveVerticalDisplacement;
    // Ifl9
    property PrintCriticalHeadNoDelay: boolean read FPrintCriticalHeadNoDelay
      write SetPrintCriticalHeadNoDelay;
    // Ifl10
    property SaveCriticalHeadNoDelay: boolean read FSaveCriticalHeadNoDelay
      write SetSaveCriticalHeadNoDelay;
    // Ifl11
    property PrintCriticalHeadDelay: boolean read FPrintCriticalHeadDelay
      write SetPrintCriticalHeadDelay;
    // Ifl12
    property SaveCriticalHeadDelay: boolean read FSaveCriticalHeadDelay
      write SetSaveCriticalHeadDelay;
    // Ifl13
    property PrintDelayBudgets: boolean read FPrintDelayBudgets
      write SetPrintDelayBudgets;
  end;

  TSubPrintCollection = class(TOrderedCollection)
  private
    function GetItem(Index: integer): TSubPrintItem;
    procedure SetItem(Index: integer; const Value: TSubPrintItem);
  public
    constructor Create(Model: TObject);
    property Items[Index: integer]: TSubPrintItem read GetItem
      write SetItem; default;
    procedure ReportErrors;
  end;

  TSubPrintFormats = class(TGoPhastPersistent)
  private
    FDelayPreconsolidationHeadFormat: integer;
    FVerticalDisplacementFormat: integer;
    FNoDelayPreconsolidationHeadFormat: integer;
    FCompactionByInterbedSystemFormat: integer;
    FSubsidenceFormat: integer;
    FCompactionByModelLayerFormat: integer;
    procedure SetCompactionByInterbedSystemFormat(const Value: integer);
    procedure SetCompactionByModelLayerFormat(const Value: integer);
    procedure SetDelayPreconsolidationHeadFormat(const Value: integer);
    procedure SetNoDelayPreconsolidationHeadFormat(const Value: integer);
    procedure SetSubsidenceFormat(const Value: integer);
    procedure SetVerticalDisplacementFormat(const Value: integer);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property SubsidenceFormat : integer read FSubsidenceFormat
      write SetSubsidenceFormat;
    property CompactionByModelLayerFormat: integer
      read FCompactionByModelLayerFormat write SetCompactionByModelLayerFormat;
    property CompactionByInterbedSystemFormat: integer
      read FCompactionByInterbedSystemFormat
      write SetCompactionByInterbedSystemFormat;
    property VerticalDisplacementFormat: integer
      read FVerticalDisplacementFormat write SetVerticalDisplacementFormat;
    property NoDelayPreconsolidationHeadFormat: integer
      read FNoDelayPreconsolidationHeadFormat
      write SetNoDelayPreconsolidationHeadFormat;
    property DelayPreconsolidationHeadFormat: integer
      read FDelayPreconsolidationHeadFormat
      write SetDelayPreconsolidationHeadFormat;
  end;

  TSubBinaryOutputChoice = (sbocSingleFile, sbocMultipleFiles);

  TSubPackageSelection = class(TModflowPackageSelection)
  private
    FPrintChoices: TSubPrintCollection;
    FReadDelayRestartFileName: string;
    FAccelerationParameter2: double;
    FAccelerationParameter1: double;
    FMinIterations: integer;
    FNumberOfNodes: integer;
    FPrintFormats: TSubPrintFormats;
    FSaveDelayRestart: boolean;
    FSubBinaryOutputChoice: TSubBinaryOutputChoice;
    procedure SetPrintChoices(const Value: TSubPrintCollection);
    procedure SetAccelerationParameter1(const Value: double);
    procedure SetAccelerationParameter2(const Value: double);
    procedure SetMinIterations(const Value: integer);
    procedure SetNumberOfNodes(const Value: integer);
    procedure SetPrintFormats(const Value: TSubPrintFormats);
    procedure SetReadDelayRestartFileName(const Value: string);
    procedure SetSaveDelayRestart(const Value: boolean);
    procedure SetSubBinaryOutputChoice(const Value: TSubBinaryOutputChoice);
  public
    procedure Assign(Source: TPersistent); override;
    procedure InitializeVariables;
    Constructor Create(Model: TObject);
    Destructor Destroy; override;
  published
    property PrintFormats: TSubPrintFormats read FPrintFormats
      write SetPrintFormats;
    property PrintChoices: TSubPrintCollection read FPrintChoices
      write SetPrintChoices;
    // NN
    property NumberOfNodes: integer read FNumberOfNodes write SetNumberOfNodes;
    // AC1
    property AccelerationParameter1: double read FAccelerationParameter1
      write SetAccelerationParameter1;
    // AC2
    property AccelerationParameter2: double read FAccelerationParameter2
      write SetAccelerationParameter2;
    // ITMIN
    property MinIterations: integer read FMinIterations write SetMinIterations;
    // IDSAVE
    property SaveDelayRestart: boolean read FSaveDelayRestart
      write SetSaveDelayRestart;
    // IDREST
    property ReadDelayRestartFileName: string read FReadDelayRestartFileName
      write SetReadDelayRestartFileName;
    property BinaryOutputChoice: TSubBinaryOutputChoice
      read FSubBinaryOutputChoice write SetSubBinaryOutputChoice;
  end;

implementation

uses Math, Contnrs , PhastModelUnit, ModflowOptionsUnit,
  frmErrorsAndWarningsUnit, frmGridColorUnit, ModflowSfrParamIcalcUnit,
  frmContourDataUnit, ModflowWellWriterUnit, ModflowGHB_WriterUnit,
  ModflowDRN_WriterUnit, ModflowDRT_WriterUnit, ModflowRiverWriterUnit,
  ModflowCHD_WriterUnit, ModflowEVT_WriterUnit, ModflowEvtUnit, RbwParser,
  frmFormulaErrorsUnit, ModflowRCH_WriterUnit, ModflowRchUnit,
  ModflowETS_WriterUnit, ModflowEtsUnit, ModflowUzfWriterUnit, ModflowUzfUnit,
  ModflowSfrWriterUnit, ModflowSfrUnit, ModflowSfrReachUnit, ModflowSfrFlows, 
  ModflowSfrChannelUnit, ModflowSfrEquationUnit, ModflowSfrSegment, 
  ModflowSfrUnsatSegment, ModflowMNW2_WriterUnit, ModflowMnw2Unit;


{ TModflowPackageSelection }

procedure TModflowPackageSelection.AddTimeList(TimeList: TCustomTimeList);
begin
  Assert(FModel <> nil);
  (FModel as TPhastModel).AddTimeList(TimeList);
end;

procedure TModflowPackageSelection.Assign(Source: TPersistent);
var
  SourceItem: TModflowPackageSelection;
begin
  if Source is TModflowPackageSelection then
  begin
    SourceItem := TModflowPackageSelection(Source);
    Comments := SourceItem.Comments;
    IsSelected := SourceItem.IsSelected;
  end
  else
  begin
    inherited;
  end;
end;

constructor TModflowPackageSelection.Create(Model: TObject);
begin
  inherited Create;
  Assert((Model = nil) or (Model is TPhastModel));
  FModel := Model;
  FComments := TStringList.Create;
end;

destructor TModflowPackageSelection.Destroy;
begin
  FComments.Free;
  inherited;
end;

procedure TModflowPackageSelection.InvalidateAllTimeLists;
begin

end;

procedure TModflowPackageSelection.InvalidateModel;
begin
  if FModel <> nil then
  begin
    (FModel as TPhastModel).Invalidate;
  end;
end;

function TModflowPackageSelection.PackageUsed(Sender: TObject): boolean;
begin
  result := IsSelected;
end;

procedure TModflowPackageSelection.RemoveTimeList(TimeList: TCustomTimeList);
begin
  (FModel as TPhastModel).RemoveTimeList(TimeList);
end;

procedure TModflowPackageSelection.SetComments(const Value: TStrings);
begin
  FComments.Assign(Value);
  InvalidateModel;
end;

procedure TModflowPackageSelection.SetIsSelected(const Value: boolean);
begin
  if FIsSelected <> Value then
  begin            
    InvalidateModel;
    FIsSelected := Value;
    if FModel <> nil then
    begin
      UpdateFrmGridColor;
      UpdateFrmContourData;
    end;
  end;
end;

procedure TModflowPackageSelection.UpdateDisplayUseList(NewUseList: TStringList;
  ParamType: TParameterType; DataIndex: integer; const DisplayName: string);
begin
  (FModel as TPhastModel).UpdateDisplayUseList(NewUseList,
    ParamType, DataIndex, DisplayName);

end;

procedure TModflowPackageSelection.UpdateUseList(DataIndex: integer;
  NewUseList: TStringList; Item: TCustomModflowBoundaryItem);
var
  Formula: string;
  TempUseList: TStringList;
  VariableIndex: Integer;
  ScreenObject: TScreenObject;
  Parser: TRbwParser;
begin
  Parser := (FModel as TPhastModel).rpThreeDFormulaCompiler;
  Formula := Item.BoundaryFormula[DataIndex];
  try
    Parser.Compile(Formula);
  except on E: ErbwParserError do
    begin
      ScreenObject := Item.ScreenObject as TScreenObject;
      frmFormulaErrors.AddError(ScreenObject.Name, StrModflowSfrReachLength,
        Formula, E.Message);
      Formula := '0';
      Parser.Compile(Formula);
    end;
  end;
  TempUseList := Parser.CurrentExpression.VariablesUsed;
  for VariableIndex := 0 to TempUseList.Count - 1 do
  begin
    if NewUseList.IndexOf(TempUseList[VariableIndex]) < 0 then
    begin
      NewUseList.Add(TempUseList[VariableIndex]);
    end;
  end;
end;

procedure TCustomTransientLayerPackageSelection.UpdateWithElevationFormula(Formula: string;
  ScreenObject: TScreenObject; NewUseList: TStringList);
var
  TempUseList: TStringList;
  VariableIndex: Integer;
  Parser : TRbwParser;
begin
  Parser := (FModel as TPhastModel).rpTopFormulaCompiler;
  try
    Parser.Compile(Formula);
  except on E: ErbwParserError do
    begin
      frmFormulaErrors.AddError(ScreenObject.Name, 'Elevation formula',
        Formula, E.Message);
      Formula := '0';
      Parser.Compile(Formula);
    end;
  end;
  TempUseList := Parser.CurrentExpression.VariablesUsed;
  for VariableIndex := 0 to TempUseList.Count - 1 do
  begin
    if NewUseList.IndexOf(TempUseList[VariableIndex]) < 0 then
    begin
      NewUseList.Add(TempUseList[VariableIndex]);
    end;
  end;
end;

{ TPcgSelection }

procedure TPcgSelection.Assign(Source: TPersistent);
var
  PcgSource: TPcgSelection;
begin
  if Source is TPcgSelection then
  begin
    PcgSource := TPcgSelection(Source);
    MXITER := PcgSource.MXITER;
    ITER1 := PcgSource.ITER1;
    NPCOND := PcgSource.NPCOND;
    HCLOSE := PcgSource.HCLOSE;
    RCLOSE := PcgSource.RCLOSE;
    RELAX := PcgSource.RELAX;
    NBPOL := PcgSource.NBPOL;
    IPRPCG := PcgSource.IPRPCG;
    MUTPCG := PcgSource.MUTPCG;
    DAMPPCG := PcgSource.DAMPPCG;
    DAMPPCGT := PcgSource.DAMPPCGT;
  end;
  inherited;
end;

constructor TPcgSelection.Create(Model: TObject);
begin
  inherited;
  FHCLOSE := TRealStorage.Create;
  FRCLOSE := TRealStorage.Create;
  FRELAX := TRealStorage.Create;
  FDAMPPCG := TRealStorage.Create;
  FDAMPPCGT:= TRealStorage.Create;
  InitializeVariables;
end;

destructor TPcgSelection.Destroy;
begin
  FHCLOSE.Free;
  FRCLOSE.Free;
  FRELAX.Free;
  FDAMPPCG.Free;
  FDAMPPCGT.Free;
  inherited;
end;

procedure TPcgSelection.InitializeVariables;
begin
  SelectionType := stRadioButton;
  FIsSelected := True;
  FMXITER := 20;
  FITER1 := 30;
  FHCLOSE.Value := 0.001;
  FRCLOSE.Value := 0.001;
  FRELAX.Value := 1;
  FIPRPCG := 1;
  FDAMPPCG.Value := 1;
  FDAMPPCGT.Value := 1;
end;

procedure TPcgSelection.SetDAMPPCG(const Value: TRealStorage);
begin
  if FDAMPPCG.Value <> Value.Value then
  begin
    FDAMPPCG.Assign(Value);
    InvalidateModel;
  end;
end;

procedure TPcgSelection.SetDAMPPCGT(const Value: TRealStorage);
begin
  if FDAMPPCGT.Value <> Value.Value then
  begin
    FDAMPPCGT.Assign(Value);
    InvalidateModel;
  end;
end;

procedure TPcgSelection.SetHCLOSE(const Value: TRealStorage);
begin
  if FHCLOSE.Value <> Value.Value then
  begin
    FHCLOSE.Assign(Value);
    InvalidateModel;
  end;
end;

procedure TPcgSelection.SetIPRPCG(const Value: integer);
begin
  if FIPRPCG <> Value then
  begin
    FIPRPCG := Value;
    InvalidateModel;
  end;
end;

procedure TPcgSelection.SetITER1(const Value: integer);
begin
  if FITER1 <> Value then
  begin
    FITER1 := Value;
    InvalidateModel;
  end;
end;

procedure TPcgSelection.SetMUTPCG(const Value: TPcgPrintSelection);
begin
  if FMUTPCG <> Value then
  begin
    FMUTPCG := Value;
    InvalidateModel;
  end;
end;

procedure TPcgSelection.SetMXITER(const Value: integer);
begin
  if FMXITER <> Value then
  begin
    FMXITER := Value;
    InvalidateModel;
  end;
end;

procedure TPcgSelection.SetNBPOL(const Value: TPcgEstimateMaxEigenvalue);
begin
  if FNBPOL <> Value then
  begin
    FNBPOL := Value;
    InvalidateModel;
  end;
end;

procedure TPcgSelection.SetNPCOND(const Value: TPcgMethod);
begin
  if FNPCOND <> Value then
  begin
    FNPCOND := Value;
    InvalidateModel;
  end;
end;

procedure TPcgSelection.SetRCLOSE(const Value: TRealStorage);
begin
  if FRCLOSE.Value <> Value.Value then
  begin
    FRCLOSE.Assign(Value);
    InvalidateModel;
  end;
end;

procedure TPcgSelection.SetRELAX(const Value: TRealStorage);
begin
  if FRELAX.Value <> Value.Value then
  begin
    FRELAX.Assign(Value);
    InvalidateModel;
  end;
end;

{ TRchPackageSelection }

procedure TCustomTransientLayerPackageSelection.Assign(Source: TPersistent);
var
  SourceItem: TCustomTransientLayerPackageSelection;
begin
  if Source is TCustomTransientLayerPackageSelection then
  begin
    SourceItem := TCustomTransientLayerPackageSelection(Source);
    LayerOption := SourceItem.LayerOption;
    TimeVaryingLayers := SourceItem.TimeVaryingLayers;
  end;
  inherited;
end;

constructor TCustomTransientLayerPackageSelection.Create(Model: TObject);
begin
  inherited Create(Model);
  FLayerOption := loTop;
end;

function TCustomTransientLayerPackageSelection.GetTimeVaryingLayers: boolean;
begin
  if LayerOption = loSpecified then
  begin
    result := FTimeVaryingLayers
  end
  else
  begin
    result := False;
  end;
end;

procedure TCustomTransientLayerPackageSelection.SetTimeVaryingLayers(
  const Value: boolean);
begin
  if FTimeVaryingLayers <> Value then
  begin
    InvalidateModel;
    FTimeVaryingLayers := Value;
    if Assigned(OnLayerChoiceChange) then
    begin
      OnLayerChoiceChange(self);
    end;
  end;
end;

{ TRchPackageSelection }

constructor TRchPackageSelection.Create(Model: TObject);
begin
  inherited;
  if Model <> nil then
  begin
    OnLayerChoiceChange := (Model as TPhastModel).InvalidateMfRchLayer;

    FMfRchRate := TModflowBoundaryDisplayTimeList.Create(Model);
    MfRchRate.OnInitialize := InitializeRchDisplay;
    MfRchRate.OnGetUseList := GetMfRchRateUseList;
    MfRchRate.OnTimeListUsed := PackageUsed;
    MfRchRate.Name := StrMODFLOWRchRate;
    AddTimeList(MfRchRate);

    FMfRchLayer := TModflowBoundaryDisplayTimeList.Create(Model);
    MfRchLayer.OnInitialize := InitializeRchDisplay;
    MfRchLayer.OnGetUseList := GetMfRchLayerUseList;
    MfRchLayer.OnTimeListUsed := PackageUsed;
    MfRchLayer.Name := StrMODFLOWRchLayer;
    AddTimeList(MfRchLayer);
  end;
end;

destructor TRchPackageSelection.Destroy;
begin
  FMfRchRate.Free;
  FMfRchLayer.Free;
  inherited;
end;

procedure TRchPackageSelection.GetMfRchLayerUseList(Sender: TObject;
  NewUseList: TStringList);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Item: TRchLayerItem;
  ValueIndex: Integer;
  Boundary: TRchBoundary;
  LocalModel: TPhastModel;
begin
  LocalModel := FModel as TPhastModel;
  for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowRchBoundary;
    if (Boundary <> nil) and Boundary.Used then
    begin
      if TimeVaryingLayers then
      begin
        for ValueIndex := 0 to Boundary.RechargeLayers.Count -1 do
        begin
          Item := Boundary.RechargeLayers[ValueIndex] as TRchLayerItem;
          UpdateUseList(0, NewUseList, Item);
        end;
      end
      else
      begin
        case ScreenObject.ElevationCount of
          ecZero:
            begin
              // do nothing
            end;
          ecOne:
            begin
              UpdateWithElevationFormula(ScreenObject.ElevationFormula,
                ScreenObject, NewUseList);
            end;
          ecTwo:
            begin
              UpdateWithElevationFormula(ScreenObject.HigherElevationFormula,
                ScreenObject, NewUseList);
              UpdateWithElevationFormula(ScreenObject.LowerElevationFormula,
                ScreenObject, NewUseList);
            end;
          else Assert(False);
        end;
      end;
    end;
  end;
end;

procedure TRchPackageSelection.GetMfRchRateUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  UpdateDisplayUseList(NewUseList, ptRCH, 0, StrMODFLOWRchRate);
end;

procedure TRchPackageSelection.InitializeRchDisplay(Sender: TObject);
var
  RchWriter: TModflowRCH_Writer;
  List: TModflowBoundListOfTimeLists;
begin
  MfRchRate.CreateDataSets;
  if LayerOption = loSpecified then
  begin
    MfRchLayer.CreateDataSets;
  end
  else
  begin
    MfRchLayer.Clear;
    MfRchLayer.SetUpToDate(True);
  end;


  List := TModflowBoundListOfTimeLists.Create;
  RchWriter := TModflowRCH_Writer.Create(FModel as TPhastModel);
  try
    List.Add(MfRchRate);
    if LayerOption = loSpecified then
    begin
      List.Add(MfRchLayer);
    end
    else
    begin
      List.Add(nil);
    end;
    RchWriter.UpdateDisplay(List);
  finally
    RchWriter.Free;
    List.Free;
  end;
  MfRchRate.LabelAsSum;
end;

procedure TRchPackageSelection.InvalidateAllTimeLists;
begin
  inherited;
  if PackageUsed(FModel) then
  begin
    MfRchRate.Invalidate;
    MfRchLayer.Invalidate;
  end;
end;

procedure TRchPackageSelection.InvalidateMfRchLayer(Sender: TObject);
begin
  MfRchLayer.Invalidate;
end;

{ TEtsPackageSelection }

procedure TEtsPackageSelection.Assign(Source: TPersistent);
begin
  if Source is TEtsPackageSelection then
  begin
    SegmentCount := TEtsPackageSelection(Source).SegmentCount;
    if FModel <> nil then
    begin
      UpdateEtsSegmentCount;
    end;
  end;
  inherited;
end;

constructor TEtsPackageSelection.Create(Model: TObject);
begin
  inherited Create(Model);
  FSegmentCount := 1;
  if Model <> nil then
  begin
    FEtsRateFractionLists := TObjectList.Create;
    FEtsDepthFractionLists := TObjectList.Create;

    OnLayerChoiceChange := InvalidateMfEtsEvapLayer;

    FMfEtsEvapRate := TModflowBoundaryDisplayTimeList.Create(Model);
    MfEtsEvapRate.OnInitialize := InitializeEtsDisplay;
    MfEtsEvapRate.OnGetUseList := GetMfEtsRateUseList;
    MfEtsEvapRate.OnTimeListUsed := PackageUsed;
    MfEtsEvapRate.Name := StrMODFLOWEtsRate;
    AddTimeList(MfEtsEvapRate);

    FMfEtsEvapSurface := TModflowBoundaryDisplayTimeList.Create(Model);
    MfEtsEvapSurface.OnInitialize := InitializeEtsDisplay;
    MfEtsEvapSurface.OnGetUseList := GetMfEtsSurfaceUseList;
    MfEtsEvapSurface.OnTimeListUsed := PackageUsed;
    MfEtsEvapSurface.Name := StrMODFLOWEtsSurface;
    AddTimeList(MfEtsEvapSurface);

    FMfEtsEvapDepth := TModflowBoundaryDisplayTimeList.Create(Model);
    MfEtsEvapDepth.OnInitialize := InitializeEtsDisplay;
    MfEtsEvapDepth.OnGetUseList := GetMfEtsDepthUseList;
    MfEtsEvapDepth.OnTimeListUsed := PackageUsed;
    MfEtsEvapDepth.Name := StrMODFLOWEtsDepth;
    AddTimeList(MfEtsEvapDepth);

    FMfEtsEvapLayer := TModflowBoundaryDisplayTimeList.Create(Model);
    MfEtsEvapLayer.OnInitialize := InitializeEtsDisplay;
    MfEtsEvapLayer.OnGetUseList := GetMfEtsLayerUseList;
    MfEtsEvapLayer.OnTimeListUsed := PackageUsed;
    MfEtsEvapLayer.Name := StrMODFLOWEtsLayer;
    AddTimeList(MfEtsEvapLayer);

    UpdateEtsSegmentCount;
  end;
end;

destructor TEtsPackageSelection.Destroy;
begin
  FEtsRateFractionLists.Free;
  FEtsDepthFractionLists.Free;
  FMfEtsEvapLayer.Free;
  FMfEtsEvapRate.Free;
  FMfEtsEvapDepth.Free;
  FMfEtsEvapSurface.Free;
  inherited;
end;

procedure TEtsPackageSelection.GetMfEtsDepthFractionUseList(Sender: TObject;
  NewUseList: TStringList);
var
  Index: integer;
  DataSetName: string;
begin
  Index := FEtsRateFractionLists.IndexOf(Sender);
  DataSetName := StrMODFLOWEtsDepthFraction + IntToStr(Index+1);
  Index := Index*2+2;
  UpdateEtsUseList(NewUseList, ptETS, Index, DataSetName);
end;

procedure TEtsPackageSelection.GetMfEtsDepthUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  UpdateEtsUseList(NewUseList, ptETS, 2, StrMODFLOWEtsRate);
end;

procedure TEtsPackageSelection.GetMfEtsLayerUseList(Sender: TObject;
  NewUseList: TStringList);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Item: TEvtLayerItem;
  ValueIndex: Integer;
  Boundary: TEtsBoundary;
  LocalModel: TPhastModel;
begin
  LocalModel := FModel as TPhastModel;
  for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowEtsBoundary;
    if (Boundary <> nil) and Boundary.Used then
    begin
      if TimeVaryingLayers then
      begin
        for ValueIndex := 0 to Boundary.EvapotranspirationLayers.Count -1 do
        begin
          Item := Boundary.EvapotranspirationLayers[ValueIndex] as TEvtLayerItem;
          UpdateUseList(0, NewUseList, Item);
        end;
      end
      else
      begin
        case ScreenObject.ElevationCount of
          ecZero:
            begin
              // do nothing
            end;
          ecOne:
            begin
              UpdateWithElevationFormula(ScreenObject.ElevationFormula,
                ScreenObject, NewUseList);
            end;
          ecTwo:
            begin
              UpdateWithElevationFormula(ScreenObject.HigherElevationFormula,
                ScreenObject, NewUseList);
              UpdateWithElevationFormula(ScreenObject.LowerElevationFormula,
                ScreenObject, NewUseList);
            end;
          else Assert(False);
        end;
      end;
    end;
  end;
end;

procedure TEtsPackageSelection.GetMfEtsRateFractionUseList(Sender: TObject;
  NewUseList: TStringList);
var
  Index: integer;
  DataSetName: string;
begin
  Index := FEtsRateFractionLists.IndexOf(Sender);
  DataSetName := StrMODFLOWEtsRateFraction + IntToStr(Index+1);
  Index := Index*2+3;
  UpdateEtsUseList(NewUseList, ptETS, Index, DataSetName);
end;

procedure TEtsPackageSelection.GetMfEtsRateUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  UpdateDisplayUseList(NewUseList, ptETS, 0, StrMODFLOWEtsRate);
end;

procedure TEtsPackageSelection.GetMfEtsSurfaceUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  UpdateEtsUseList(NewUseList, ptETS, 1, StrMODFLOWEtsRate);
end;

procedure TEtsPackageSelection.InitializeEtsDisplay(Sender: TObject);
var
  EtsWriter: TModflowETS_Writer;
  List: TModflowBoundListOfTimeLists;
  Index: Integer;
  TimeList: TModflowBoundaryDisplayTimeList;
begin
  MfEtsEvapRate.CreateDataSets;
  MfEtsEvapSurface.CreateDataSets;
  MfEtsEvapDepth.CreateDataSets;
  if LayerOption = loSpecified then
  begin
    MfEtsEvapLayer.CreateDataSets;
  end
  else
  begin
    MfEtsEvapLayer.Clear;
    MfEtsEvapLayer.SetUpToDate(True);
  end;

  List := TModflowBoundListOfTimeLists.Create;
  EtsWriter := TModflowETS_Writer.Create(FModel as TPhastModel);
  try
    List.Add(MfEtsEvapRate);
    List.Add(MfEtsEvapSurface);
    List.Add(MfEtsEvapDepth);
    if LayerOption = loSpecified then
    begin
      List.Add(MfEtsEvapLayer);
    end
    else
    begin
      List.Add(nil);
    end;
    Assert(FEtsRateFractionLists.Count = FEtsDepthFractionLists.Count);
    for Index := 0 to FEtsRateFractionLists.Count - 1 do
    begin
      TimeList := FEtsDepthFractionLists[Index];
      TimeList.CreateDataSets;
      List.Add(TimeList);

      TimeList := FEtsRateFractionLists[Index];
      TimeList.CreateDataSets;
      List.Add(TimeList);
    end;
    EtsWriter.UpdateDisplay(List);
  finally
    EtsWriter.Free;
    List.Free;
  end;
  MfEtsEvapRate.LabelAsSum;
end;

procedure TEtsPackageSelection.InvalidateAllTimeLists;
begin
  inherited;
  if PackageUsed(nil) then
  begin
    MfEtsEvapDepth.Invalidate;
    InvalidateMfEtsEvapLayer(nil);
    MfEtsEvapRate.Invalidate;
    MfEtsEvapSurface.Invalidate;
    InvalidateEtsDepthFractions(nil);
    InvalidateEtsRateFractions(nil);
  end;
end;

procedure TEtsPackageSelection.InvalidateEtsDepthFractions(Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowBoundaryDisplayTimeList;
begin
  for Index := 0 to FEtsDepthFractionLists.Count - 1 do
  begin
    TimeList := FEtsRateFractionLists[Index];
    TimeList.Invalidate;
  end;
end;

procedure TEtsPackageSelection.InvalidateEtsRateFractions(Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowBoundaryDisplayTimeList;
begin
  for Index := 0 to FEtsRateFractionLists.Count - 1 do
  begin
    TimeList := FEtsRateFractionLists[Index];
    TimeList.Invalidate;
  end;
end;

procedure TEtsPackageSelection.InvalidateMfEtsEvapLayer(Sender: TObject);
begin
  MfEtsEvapLayer.Invalidate;
end;

procedure TEtsPackageSelection.SetSegmentCount(const Value: integer);
begin
  if FSegmentCount <> Value then
  begin
    InvalidateModel;
    FSegmentCount := Value;
  end;
end;

procedure TEtsPackageSelection.UpdateEtsSegmentCount;
var
  TimeList: TModflowBoundaryDisplayTimeList;
  Index: Integer;
begin
  if IsSelected then
  begin
    while FEtsRateFractionLists.Count >
      SegmentCount -1 do
    begin
      TimeList := FEtsRateFractionLists[FEtsRateFractionLists.Count-1];
      RemoveTimeList(TimeList);
      FEtsRateFractionLists.Delete(FEtsRateFractionLists.Count-1);

      TimeList := FEtsDepthFractionLists[FEtsDepthFractionLists.Count-1];
      RemoveTimeList(TimeList);
      FEtsDepthFractionLists.Delete(FEtsRateFractionLists.Count-1);
    end;
    while FEtsRateFractionLists.Count <
      SegmentCount -1 do
    begin
      TimeList := TModflowBoundaryDisplayTimeList.Create(FModel);
      AddTimeList(TimeList);
      FEtsRateFractionLists.Add(TimeList);
      TimeList.OnInitialize := InitializeEtsDisplay;
      TimeList.OnGetUseList := GetMfEtsRateFractionUseList;
      TimeList.Name := StrMODFLOWEtsRateFraction
        + IntToStr(FEtsRateFractionLists.Count);

      TimeList := TModflowBoundaryDisplayTimeList.Create(FModel);
      AddTimeList(TimeList);
      FEtsDepthFractionLists.Add(TimeList);
      TimeList.OnInitialize := InitializeEtsDisplay;
      TimeList.OnGetUseList := GetMfEtsDepthFractionUseList;
      TimeList.Name := StrMODFLOWEtsDepthFraction
        + IntToStr(FEtsDepthFractionLists.Count);;
    end;
  end
  else
  begin
    for Index := 0 to FEtsRateFractionLists.Count - 1 do
    begin
      TimeList := FEtsRateFractionLists[Index];
      RemoveTimeList(TimeList);
    end;
    for Index := 0 to FEtsDepthFractionLists.Count - 1 do
    begin
      TimeList := FEtsDepthFractionLists[Index];
      RemoveTimeList(TimeList);
    end;
    FEtsRateFractionLists.Clear;
    FEtsDepthFractionLists.Clear;
  end;
end;

procedure TEtsPackageSelection.UpdateEtsUseList(NewUseList: TStringList;
  ParamType: TParameterType; DataIndex: integer; const DisplayName: string);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Item: TCustomModflowBoundaryItem;
  ValueIndex: Integer;
  Boundary: TEtsBoundary;
  LocalModel: TPhastModel;
begin
  LocalModel := FModel as TPhastModel;
  for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowEtsBoundary;
    if (Boundary <> nil) and Boundary.Used then
    begin
      for ValueIndex := 0 to Boundary.EtsSurfDepthCollection.Count -1 do
      begin
        Item := Boundary.EtsSurfDepthCollection[ValueIndex];
        UpdateUseList(DataIndex, NewUseList, Item);
      end;
    end;
  end;
end;

{ TResPackageSelection }

procedure TResPackageSelection.Assign(Source: TPersistent);
var
  ResSource: TResPackageSelection;
begin
  if Source is TResPackageSelection then
  begin
    ResSource := TResPackageSelection(Source);
    PrintStage := ResSource.PrintStage;
    TableStages := ResSource.TableStages;
  end;
  inherited;
end;

constructor TResPackageSelection.Create(Model: TObject);
begin
  inherited;
  FPrintStage := True;
  FTableStages := 15;
end;

procedure TResPackageSelection.SetPrintStage(const Value: boolean);
begin
  if FPrintStage <> Value then
  begin
    InvalidateModel;
    FPrintStage := Value;
  end;
end;

procedure TResPackageSelection.SetTableStages(const Value: integer);
begin
  if FTableStages <> Value then
  begin
    InvalidateModel;
    FTableStages := Value;
  end;
end;

{ TLakePackageSelection }

procedure TLakePackageSelection.Assign(Source: TPersistent);
var
  Lake: TLakePackageSelection;
begin
  if Source is TLakePackageSelection then
  begin
    Lake := TLakePackageSelection(Source);
    ConvergenceCriterion := Lake.ConvergenceCriterion;
    NumberOfIterations := Lake.NumberOfIterations;
    SurfDepth := Lake.SurfDepth;
    PrintLakes := Lake.PrintLakes;
    Theta := Lake.Theta;
  end;
  inherited;
end;

constructor TLakePackageSelection.Create(Model: TObject);
begin
  inherited;
  FTheta := 0.5;
  FConvergenceCriterion := 0.00001;
  FNumberOfIterations := 100;
  FSurfDepth := TRealStorage.Create;
  FSurfDepth.Value := 0.2;
  FPrintLakes := True;
end;

destructor TLakePackageSelection.Destroy;
begin
  FSurfDepth.Free;
  inherited;
end;

procedure TLakePackageSelection.SetConvergenceCriterion(const Value: double);
begin
  if FConvergenceCriterion <> Value then
  begin
    InvalidateModel;
    FConvergenceCriterion := Value;
  end;
end;

procedure TLakePackageSelection.SetIsSelected(const Value: boolean);
begin
  inherited;
  DischargeRoutingUpdate;
end;

procedure TLakePackageSelection.SetNumberOfIterations(const Value: integer);
begin
  if FNumberOfIterations <> Value then
  begin
    InvalidateModel;
    FNumberOfIterations := Value;
  end;
end;

procedure TLakePackageSelection.SetPrintLakes(const Value: boolean);
begin
  if FPrintLakes <> Value then
  begin
    InvalidateModel;
    FPrintLakes := Value;
  end;
end;

procedure TLakePackageSelection.SetSurfDepth(const Value: TRealStorage);
begin
  if FSurfDepth.Value <> Value.Value then
  begin
    InvalidateModel;
    FSurfDepth.Assign(Value);
  end;
end;

procedure TLakePackageSelection.SetTheta(const Value: double);
begin
  if FTheta <> Value then
  begin
    InvalidateModel;
    FTheta := Value;
  end;
end;

{ TEvtPackageSelection }

constructor TEvtPackageSelection.Create(Model: TObject);
begin
  inherited;
  if Model <> nil then
  begin
    OnLayerChoiceChange := InvalidateMfEvtEvapLayer;

    FMfEvtEvapRate := TModflowBoundaryDisplayTimeList.Create(Model);
    MfEvtEvapRate.OnInitialize := InitializeEvtDisplay;
    MfEvtEvapRate.OnGetUseList := GetMfEvtRateUseList;
    MfEvtEvapRate.OnTimeListUsed := PackageUsed;
    MfEvtEvapRate.Name := StrMODFLOWEvtRate;
    AddTimeList(MfEvtEvapRate);

    FMfEvtEvapSurface := TModflowBoundaryDisplayTimeList.Create(Model);
    MfEvtEvapSurface.OnInitialize := InitializeEvtDisplay;
    MfEvtEvapSurface.OnGetUseList := GetMfEvtSurfaceUseList;
    MfEvtEvapSurface.OnTimeListUsed := PackageUsed;
    MfEvtEvapSurface.Name := StrMODFLOWEvtSurface;
    AddTimeList(MfEvtEvapSurface);

    FMfEvtEvapDepth := TModflowBoundaryDisplayTimeList.Create(Model);
    MfEvtEvapDepth.OnInitialize := InitializeEvtDisplay;
    MfEvtEvapDepth.OnGetUseList := GetMfEvtDepthUseList;
    MfEvtEvapDepth.OnTimeListUsed := PackageUsed;
    MfEvtEvapDepth.Name := StrMODFLOWEvtDepth;
    AddTimeList(MfEvtEvapDepth);

    FMfEvtEvapLayer := TModflowBoundaryDisplayTimeList.Create(Model);
    MfEvtEvapLayer.OnInitialize := InitializeEvtDisplay;
    MfEvtEvapLayer.OnGetUseList := GetMfEvtLayerUseList;
    MfEvtEvapLayer.OnTimeListUsed := PackageUsed;
    MfEvtEvapLayer.Name := StrMODFLOWEvtLayer;
    AddTimeList(MfEvtEvapLayer);
  end;
end;

destructor TEvtPackageSelection.Destroy;
begin
  FMfEvtEvapLayer.Free;
  FMfEvtEvapRate.Free;
  FMfEvtEvapSurface.Free;
  FMfEvtEvapDepth.Free;
  inherited;
end;

procedure TEvtPackageSelection.GetMfEvtDepthUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  UpdateEvtUseList(NewUseList, ptEVT, 1, StrMODFLOWEvtRate);
end;

procedure TEvtPackageSelection.GetMfEvtLayerUseList(Sender: TObject;
  NewUseList: TStringList);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Item: TEvtLayerItem;
  ValueIndex: Integer;
  Boundary: TEvtBoundary;
  LocalModel: TPhastModel;
begin
  LocalModel := FModel as TPhastModel;
  for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowEvtBoundary;
    if (Boundary <> nil) and Boundary.Used then
    begin
      if TimeVaryingLayers then
      begin
        for ValueIndex := 0 to Boundary.EvapotranspirationLayers.Count -1 do
        begin
          Item := Boundary.EvapotranspirationLayers[ValueIndex]
            as TEvtLayerItem;
          UpdateUseList(0, NewUseList, Item);
        end;
      end
      else
      begin
        case ScreenObject.ElevationCount of
          ecZero:
            begin
              // do nothing
            end;
          ecOne:
            begin
              UpdateWithElevationFormula(ScreenObject.ElevationFormula,
                ScreenObject, NewUseList);
            end;
          ecTwo:
            begin
              UpdateWithElevationFormula(ScreenObject.HigherElevationFormula,
                ScreenObject, NewUseList);
              UpdateWithElevationFormula(ScreenObject.LowerElevationFormula,
                ScreenObject, NewUseList);
            end;
          else Assert(False);
        end;
      end;
    end;
  end;
end;

procedure TEvtPackageSelection.GetMfEvtRateUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  UpdateDisplayUseList(NewUseList, ptEVT, 0, StrMODFLOWEvtRate);
end;

procedure TEvtPackageSelection.GetMfEvtSurfaceUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  UpdateEvtUseList(NewUseList, ptEVT, 0, StrMODFLOWEtsRate);
end;

procedure TEvtPackageSelection.InitializeEvtDisplay(Sender: TObject);
var
  EvtWriter: TModflowEVT_Writer;
  List: TModflowBoundListOfTimeLists;
begin
  MfEvtEvapRate.CreateDataSets;
  MfEvtEvapSurface.CreateDataSets;
  MfEvtEvapDepth.CreateDataSets;
  if LayerOption = loSpecified then
  begin
    MfEvtEvapLayer.CreateDataSets;
  end
  else
  begin
    MfEvtEvapLayer.Clear;
    MfEvtEvapLayer.SetUpToDate(True);
  end;


  List := TModflowBoundListOfTimeLists.Create;
  EvtWriter := TModflowEVT_Writer.Create(FModel as TPhastModel);
  try
    List.Add(MfEvtEvapRate);
    List.Add(MfEvtEvapSurface);
    List.Add(MfEvtEvapDepth);
    if LayerOption = loSpecified then
    begin
      List.Add(MfEvtEvapLayer);
    end
    else
    begin
      List.Add(nil);
    end;
    EvtWriter.UpdateDisplay(List);
  finally
    EvtWriter.Free;
    List.Free;
  end;
  MfEvtEvapRate.LabelAsSum;
end;

procedure TEvtPackageSelection.InvalidateAllTimeLists;
begin
  inherited;
  if PackageUsed(FModel) then
  begin
    MfEvtEvapDepth.Invalidate;
    MfEvtEvapLayer.Invalidate;
    MfEvtEvapRate.Invalidate;
    MfEvtEvapSurface.Invalidate;
  end;
end;

procedure TEvtPackageSelection.InvalidateMfEvtEvapLayer(Sender: TObject);
begin
  MfEvtEvapLayer.Invalidate;
end;

procedure TEvtPackageSelection.UpdateEvtUseList(NewUseList: TStringList;
  ParamType: TParameterType; DataIndex: integer; const DisplayName: string);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Item: TCustomModflowBoundaryItem;
  ValueIndex: Integer;
  Boundary: TEvtBoundary;
  LocalModel: TPhastModel;
begin
  LocalModel := FModel as TPhastModel;
  for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowEvtBoundary;
    if (Boundary <> nil) and Boundary.Used then
    begin
      for ValueIndex := 0 to Boundary.EvtSurfDepthCollection.Count -1 do
      begin
        Item := Boundary.EvtSurfDepthCollection[ValueIndex];
        UpdateUseList(DataIndex, NewUseList, Item);
      end;
    end;
  end;
end;

{ TSfrPackageSelection }

procedure TSfrPackageSelection.Assign(Source: TPersistent);
var
  Stream: TSfrPackageSelection;
begin
  if Source is TSfrPackageSelection then
  begin
    Stream := TSfrPackageSelection(Source);
    Dleak := Stream.Dleak;
    Isfropt := Stream.Isfropt;
    Nstrail := Stream.Nstrail;
    Isuzn := Stream.Isuzn;
    Nsfrsets := Stream.Nsfrsets;
    // PrintStreams has been replaced by PrintFlows.
//    PrintStreams := Stream.PrintStreams;
    PrintFlows := Stream.PrintFlows;
    KinematicRouting := Stream.KinematicRouting;
    GageOverallBudget := Stream.GageOverallBudget;
    if AssignParameterInstances then
    begin
      ParameterInstances := Stream.ParameterInstances;
    end;
  end;
  inherited;
end;

constructor TSfrPackageSelection.Create(Model: TObject);
begin
  inherited;
  Dleak := 0.0001;
  Isfropt := 0;
  Nstrail := 10;
  Isuzn := 10;
  Nsfrsets := 30;
  FKinematicRoutingTolerance := 1e-4;
  FPrintStreams := True;
  FPrintFlows := pfListing;
  FTimeStepsForKinematicRouting := 1;
  FKinematicRoutingWeight := 1;
  FParameterInstances := TSfrParamInstances.Create(Model);
  AssignParameterInstances := True;
  if Model <> nil then
  begin
    FMfSfrSegmentNumber := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrSegmentNumber.OnTimeListUsed := PackageUsed;
    FMfSfrSegmentNumber.OnInitialize := InitializeSfrDisplay;
    FMfSfrSegmentNumber.OnGetUseList := GetMfSfrUseList;
    FMfSfrSegmentNumber.Name := StrModflowSfrSegment;
    AddTimeList(FMfSfrSegmentNumber);

    FMfSfrReachNumber := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrReachNumber.OnTimeListUsed := PackageUsed;
    FMfSfrReachNumber.OnInitialize := InitializeSfrDisplay;
    FMfSfrReachNumber.OnGetUseList := GetMfSfrUseList;
    FMfSfrReachNumber.Name := StrModflowSfrReach;
    AddTimeList(FMfSfrReachNumber);

    FMfSfrIcalc := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrIcalc.OnTimeListUsed := PackageUsed;
    FMfSfrIcalc.OnInitialize := InitializeSfrDisplay;
    FMfSfrIcalc.OnGetUseList := GetMfSfrUseList;
    FMfSfrIcalc.Name := StrModflowSfrIcalc;
    AddTimeList(FMfSfrIcalc);

    FMfSfrReachLength := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrReachLength.OnTimeListUsed := PackageUsed;
    FMfSfrReachLength.OnInitialize := InitializeSfrDisplay;
    FMfSfrReachLength.OnGetUseList := GetMfSfrReachLengthUseList;
    FMfSfrReachLength.Name := StrModflowSfrReachLength;
    AddTimeList(FMfSfrReachLength);

    FMfSfrStreamTop := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrStreamTop.OnTimeListUsed := ModflowSfrSpatialVariationSelected;
    FMfSfrStreamTop.OnInitialize := InitializeSfrDisplay;
    FMfSfrStreamTop.OnGetUseList := GetMfSfrStreamTopUseList;
    FMfSfrStreamTop.Name := StrModflowSfrStreamTop;
    AddTimeList(FMfSfrStreamTop);

    FMfSfrStreamSlope := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrStreamSlope.OnTimeListUsed := ModflowSfrSpatialVariationSelected;
    FMfSfrStreamSlope.OnInitialize := InitializeSfrDisplay;
    FMfSfrStreamSlope.OnGetUseList := GetMfSfrStreamSlopeUseList;
    FMfSfrStreamSlope.Name := StrModflowSfrStreamSlope;
    AddTimeList(FMfSfrStreamSlope);

    FMfSfrStreamThickness := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrStreamThickness.OnTimeListUsed := ModflowSfrSpatialVariationSelected;
    FMfSfrStreamThickness.OnInitialize := InitializeSfrDisplay;
    FMfSfrStreamThickness.OnGetUseList := GetMfSfrStreamThicknessUseList;
    FMfSfrStreamThickness.Name := StrModflowSfrStreamThickness;
    AddTimeList(FMfSfrStreamThickness);

    FMfSfrStreamK := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrStreamK.OnTimeListUsed := ModflowSfrSpatialVariationSelected;
    FMfSfrStreamK.OnInitialize := InitializeSfrDisplay;
    FMfSfrStreamK.OnGetUseList := GetMfSfrStreamKUseList;
    FMfSfrStreamK.Name := StrModflowSfrStreamK;
    AddTimeList(FMfSfrStreamK);

    FMfSfrSaturatedWaterContent := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrSaturatedWaterContent.OnTimeListUsed :=
      ModflowSfrUnsatSpatialVariationSelected;
    FMfSfrSaturatedWaterContent.OnInitialize := InitializeSfrDisplay;
    FMfSfrSaturatedWaterContent.OnGetUseList :=  
      GetMfSfrStreamSatWatContentUseList;
    FMfSfrSaturatedWaterContent.Name := StrModflowSfrSatWatCont;
    AddTimeList(FMfSfrSaturatedWaterContent);

    FMfSfrInitialWaterContent := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrInitialWaterContent.OnTimeListUsed :=  
      ModflowSfrUnsatSpatialVariationSelected;
    FMfSfrInitialWaterContent.OnInitialize := InitializeSfrDisplay;
    FMfSfrInitialWaterContent.OnGetUseList :=  
      GetMfSfrStreamInitialWatContentUseList;
    FMfSfrInitialWaterContent.Name := StrModflowSfrInitWatCont;
    AddTimeList(FMfSfrInitialWaterContent);

    FMfSfrBrooksCorey := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrBrooksCorey.OnTimeListUsed := ModflowSfrUnsatSpatialVariationSelected;
    FMfSfrBrooksCorey.OnInitialize := InitializeSfrDisplay;
    FMfSfrBrooksCorey.OnGetUseList := GetMfSfrBrooksCoreyUseList;
    FMfSfrBrooksCorey.Name := StrModflowSfrBrooksCorey;
    AddTimeList(FMfSfrBrooksCorey);

    FMfSfrVerticalUnsatK := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrVerticalUnsatK.OnTimeListUsed :=
      ModflowSfrUnsatKzSpatialVariationSelected;
    FMfSfrVerticalUnsatK.OnInitialize := InitializeSfrDisplay;
    FMfSfrVerticalUnsatK.OnGetUseList := GetMfSfrUnsatKzUseList;
    FMfSfrVerticalUnsatK.Name := StrModflowSfrVertK;
    AddTimeList(FMfSfrVerticalUnsatK);

    FMfSfrOutSegment := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrOutSegment.OnTimeListUsed := PackageUsed;
    FMfSfrOutSegment.OnInitialize := InitializeSfrDisplay;
    FMfSfrOutSegment.OnGetUseList := GetMfSfrUseList;
    FMfSfrOutSegment.Name := StrModflowSfrDownstreamSegment;
    AddTimeList(FMfSfrOutSegment);

    FMfSfrDiversionSegment := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrDiversionSegment.OnTimeListUsed := PackageUsed;
    FMfSfrDiversionSegment.OnInitialize := InitializeSfrDisplay;
    FMfSfrDiversionSegment.OnGetUseList := GetMfSfrUseList;
    FMfSfrDiversionSegment.Name := StrModflowSfrDiversionSegment;
    AddTimeList(FMfSfrDiversionSegment);

    FMfSfrIprior := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrIprior.OnTimeListUsed := PackageUsed;
    FMfSfrIprior.OnInitialize := InitializeSfrDisplay;
    FMfSfrIprior.OnGetUseList := GetMfSfrUseList;
    FMfSfrIprior.Name := StrModflowSfrIprior;
    AddTimeList(FMfSfrIprior);

    FMfSfrFlow := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrFlow.OnTimeListUsed := PackageUsed;
    FMfSfrFlow.OnInitialize := InitializeSfrDisplay;
    FMfSfrFlow.OnGetUseList := GetMfSfrFlowUseList;
    FMfSfrFlow.Name := StrModflowSfrFlow;
    AddTimeList(FMfSfrFlow);

    FMfSfrRunoff := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrRunoff.OnTimeListUsed := PackageUsed;
    FMfSfrRunoff.OnInitialize := InitializeSfrDisplay;
    FMfSfrRunoff.OnGetUseList := GetMfSfrRunoffUseList;
    FMfSfrRunoff.Name := StrModflowSfrRunoff;
    AddTimeList(FMfSfrRunoff);

    FMfSfrPrecipitation := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrPrecipitation.OnTimeListUsed := PackageUsed;
    FMfSfrPrecipitation.OnInitialize := InitializeSfrDisplay;
    FMfSfrPrecipitation.OnGetUseList := GetMfSfrPrecipitationUseList;
    FMfSfrPrecipitation.Name := StrModflowSfrPrecipitation;
    AddTimeList(FMfSfrPrecipitation);

    FMfSfrEvapotranspiration := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrEvapotranspiration.OnTimeListUsed := PackageUsed;
    FMfSfrEvapotranspiration.OnInitialize := InitializeSfrDisplay;
    FMfSfrEvapotranspiration.OnGetUseList := GetMfSfrEvapotranspirationUseList;
    FMfSfrEvapotranspiration.Name := StrModflowSfrEvapotranspiration;
    AddTimeList(FMfSfrEvapotranspiration);

    FMfSfrChannelRoughness := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrChannelRoughness.OnTimeListUsed := PackageUsed;
    FMfSfrChannelRoughness.OnInitialize := InitializeSfrDisplay;
    FMfSfrChannelRoughness.OnGetUseList := GetMfSfrChannelRoughnessUseList;
    FMfSfrChannelRoughness.Name := StrModflowSfrChannelRoughness;
    AddTimeList(FMfSfrChannelRoughness);

    FMfSfrBankRoughness := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrBankRoughness.OnTimeListUsed := PackageUsed;
    FMfSfrBankRoughness.OnInitialize := InitializeSfrDisplay;
    FMfSfrBankRoughness.OnGetUseList := GetMfSfrBankRoughnessUseList;
    FMfSfrBankRoughness.Name := StrModflowSfrBankRoughness;
    AddTimeList(FMfSfrBankRoughness);

    FMfSfrDepthCoefficient := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrDepthCoefficient.OnTimeListUsed := PackageUsed;
    FMfSfrDepthCoefficient.OnInitialize := InitializeSfrDisplay;
    FMfSfrDepthCoefficient.OnGetUseList := GetMfSfrDepthCoefficientUseList;
    FMfSfrDepthCoefficient.Name := StrModflowSfrDepthCoefficient;
    AddTimeList(FMfSfrDepthCoefficient);

    FMfSfrDepthExponent := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrDepthExponent.OnTimeListUsed := PackageUsed;
    FMfSfrDepthExponent.OnInitialize := InitializeSfrDisplay;
    FMfSfrDepthExponent.OnGetUseList := GetMfSfrDepthExponentUseList;
    FMfSfrDepthExponent.Name := StrModflowSfrDepthExponent;
    AddTimeList(FMfSfrDepthExponent);

    FMfSfrWidthCoefficient := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrWidthCoefficient.OnTimeListUsed := PackageUsed;
    FMfSfrWidthCoefficient.OnInitialize := InitializeSfrDisplay;
    FMfSfrWidthCoefficient.OnGetUseList := GetMfSfrWidthCoefficientUseList;
    FMfSfrWidthCoefficient.Name := StrModflowSfrWidthCoefficient;
    AddTimeList(FMfSfrWidthCoefficient);

    FMfSfrWidthExponent := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrWidthExponent.OnTimeListUsed := PackageUsed;
    FMfSfrWidthExponent.OnInitialize := InitializeSfrDisplay;
    FMfSfrWidthExponent.OnGetUseList := GetMfSfrWidthExponentUseList;
    FMfSfrWidthExponent.Name := StrModflowSfrWidthExponent;
    AddTimeList(FMfSfrWidthExponent);

    FMfSfrUpstreamHydraulicConductivity :=  
      TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrUpstreamHydraulicConductivity.OnTimeListUsed :=  
      ModflowSfrUpstreamDownstreamUsed;
    FMfSfrUpstreamHydraulicConductivity.OnInitialize := InitializeSfrDisplay;
    FMfSfrUpstreamHydraulicConductivity.OnGetUseList :=
      GetMfSfrUpstreamHydraulicConductivityUseList;
    FMfSfrUpstreamHydraulicConductivity.Name :=
      StrModflowSfrUpstreamHydraulicConductivity;
    AddTimeList(FMfSfrUpstreamHydraulicConductivity);

    FMfSfrDownstreamHydraulicConductivity :=
      TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrDownstreamHydraulicConductivity.OnTimeListUsed :=
      ModflowSfrUpstreamDownstreamUsed;
    FMfSfrDownstreamHydraulicConductivity.OnInitialize := InitializeSfrDisplay;
    FMfSfrDownstreamHydraulicConductivity.OnGetUseList :=  
      GetMfSfrDownstreamHydraulicConductivityUseList;
    FMfSfrDownstreamHydraulicConductivity.Name :=  
      StrModflowSfrDownstreamHydraulicConductivity;
    AddTimeList(FMfSfrDownstreamHydraulicConductivity);

    FMfSfrUpstreamWidth := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrUpstreamWidth.OnTimeListUsed := PackageUsed;
    FMfSfrUpstreamWidth.OnInitialize := InitializeSfrDisplay;
    FMfSfrUpstreamWidth.OnGetUseList := GetMfSfrUpstreamWidthUseList;
    FMfSfrUpstreamWidth.Name := StrModflowSfrUpstreamWidth;
    AddTimeList(FMfSfrUpstreamWidth);

    FMfSfrDownstreamWidth := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrDownstreamWidth.OnTimeListUsed := PackageUsed;
    FMfSfrDownstreamWidth.OnInitialize := InitializeSfrDisplay;
    FMfSfrDownstreamWidth.OnGetUseList := GetMfSfrDownstreamWidthUseList;
    FMfSfrDownstreamWidth.Name := StrModflowSfrDownstreamWidth;
    AddTimeList(FMfSfrDownstreamWidth);

    FMfSfrUpstreamThickness := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrUpstreamThickness.OnTimeListUsed := ModflowSfrUpstreamDownstreamUsed;
    FMfSfrUpstreamThickness.OnInitialize := InitializeSfrDisplay;
    FMfSfrUpstreamThickness.OnGetUseList := GetMfSfrUpstreamThicknessUseList;
    FMfSfrUpstreamThickness.Name := StrModflowSfrUpstreamThickness;
    AddTimeList(FMfSfrUpstreamThickness);

    FMfSfrDownstreamThickness := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrDownstreamThickness.OnTimeListUsed := ModflowSfrUpstreamDownstreamUsed;
    FMfSfrDownstreamThickness.OnInitialize := InitializeSfrDisplay;
    FMfSfrDownstreamThickness.OnGetUseList := GetMfSfrDownstreamThicknessUseList;
    FMfSfrDownstreamThickness.Name := StrModflowSfrDownstreamThickness;
    AddTimeList(FMfSfrDownstreamThickness);

    FMfSfrUpstreamElevation := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrUpstreamElevation.OnTimeListUsed := ModflowSfrUpstreamDownstreamUsed;
    FMfSfrUpstreamElevation.OnInitialize := InitializeSfrDisplay;
    FMfSfrUpstreamElevation.OnGetUseList := GetMfSfrUpstreamElevationUseList;
    FMfSfrUpstreamElevation.Name := StrModflowSfrUpstreamElevation;
    AddTimeList(FMfSfrUpstreamElevation);

    FMfSfrDownstreamElevation := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrDownstreamElevation.OnTimeListUsed := ModflowSfrUpstreamDownstreamUsed;
    FMfSfrDownstreamElevation.OnInitialize := InitializeSfrDisplay;
    FMfSfrDownstreamElevation.OnGetUseList := GetMfSfrDownstreamElevationUseList;
    FMfSfrDownstreamElevation.Name := StrModflowSfrDownstreamElevation;
    AddTimeList(FMfSfrDownstreamElevation);

    FMfSfrUpstreamDepth := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrUpstreamDepth.OnTimeListUsed := PackageUsed;
    FMfSfrUpstreamDepth.OnInitialize := InitializeSfrDisplay;
    FMfSfrUpstreamDepth.OnGetUseList := GetMfSfrUpstreamDepthUseList;
    FMfSfrUpstreamDepth.Name := StrModflowSfrUpstreamDepth;
    AddTimeList(FMfSfrUpstreamDepth);

    FMfSfrDownstreamDepth := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrDownstreamDepth.OnTimeListUsed := PackageUsed;
    FMfSfrDownstreamDepth.OnInitialize := InitializeSfrDisplay;
    FMfSfrDownstreamDepth.OnGetUseList := GetMfSfrDownstreamDepthUseList;
    FMfSfrDownstreamDepth.Name := StrModflowSfrDownstreamDepth;
    AddTimeList(FMfSfrDownstreamDepth);

    FMfSfrUpstreamUnsaturatedWaterContent :=  
      TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrUpstreamUnsaturatedWaterContent.OnTimeListUsed :=  
      ModflowSfrUpstreamDownstreamUnsatUsed;
    FMfSfrUpstreamUnsaturatedWaterContent.OnInitialize := InitializeSfrDisplay;
    FMfSfrUpstreamUnsaturatedWaterContent.OnGetUseList :=
      GetMfSfrUpstreamSatWatContentUseList;
    FMfSfrUpstreamUnsaturatedWaterContent.Name :=
      StrModflowSfrUpstreamSaturatedWaterContent;
    AddTimeList(FMfSfrUpstreamUnsaturatedWaterContent);

    FMfSfrDownstreamUnsaturatedWaterContent :=
      TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrDownstreamUnsaturatedWaterContent.OnTimeListUsed :=
      ModflowSfrUpstreamDownstreamUnsatUsed;
    FMfSfrDownstreamUnsaturatedWaterContent.OnInitialize := InitializeSfrDisplay;
    FMfSfrDownstreamUnsaturatedWaterContent.OnGetUseList :=  
      GetMfSfrDownstreamSatWatContentUseList;
    FMfSfrDownstreamUnsaturatedWaterContent.Name :=  
      StrModflowSfrDownstreamSaturatedWaterContent;
    AddTimeList(FMfSfrDownstreamUnsaturatedWaterContent);

    FMfSfrUpstreamUnsatInitialWaterContent :=  
      TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrUpstreamUnsatInitialWaterContent.OnTimeListUsed :=  
      ModflowSfrUpstreamDownstreamUnsatUsed;
    FMfSfrUpstreamUnsatInitialWaterContent.OnInitialize := InitializeSfrDisplay;
    FMfSfrUpstreamUnsatInitialWaterContent.OnGetUseList :=
      GetMfSfrUpstreamInitialWatContentUseList;
    FMfSfrUpstreamUnsatInitialWaterContent.Name :=
      StrModflowSfrUpstreamInitialUnsaturatedWaterContent;
    AddTimeList(FMfSfrUpstreamUnsatInitialWaterContent);

    FMfSfrDownstreamUnsatInitialWaterContent :=
      TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrDownstreamUnsatInitialWaterContent.OnTimeListUsed :=
      ModflowSfrUpstreamDownstreamUnsatUsed;
    FMfSfrDownstreamUnsatInitialWaterContent.OnInitialize :=
      InitializeSfrDisplay;
    FMfSfrDownstreamUnsatInitialWaterContent.OnGetUseList :=
      GetMfSfrDownstreamInitialWatContentUseList;
    FMfSfrDownstreamUnsatInitialWaterContent.Name :=
      StrModflowSfrDownstreamInitialUnsaturatedWaterContent;
    AddTimeList(FMfSfrDownstreamUnsatInitialWaterContent);

    FMfSfrUpstreamBrooksCorey := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrUpstreamBrooksCorey.OnTimeListUsed :=  
      ModflowSfrUpstreamDownstreamUnsatUsed;
    FMfSfrUpstreamBrooksCorey.OnInitialize := InitializeSfrDisplay;
    FMfSfrUpstreamBrooksCorey.OnGetUseList := GetMfSfrUpstreamBrooksCoreyUseList;
    FMfSfrUpstreamBrooksCorey.Name := StrModflowSfrUpstreamBrooksCoreyExponent;
    AddTimeList(FMfSfrUpstreamBrooksCorey);

    FMfSfrDownstreamBrooksCorey := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrDownstreamBrooksCorey.OnTimeListUsed :=  
      ModflowSfrUpstreamDownstreamUnsatUsed;
    FMfSfrDownstreamBrooksCorey.OnInitialize := InitializeSfrDisplay;
    FMfSfrDownstreamBrooksCorey.OnGetUseList :=
      GetMfSfrDownstreamBrooksCoreyUseList;
    FMfSfrDownstreamBrooksCorey.Name :=
      StrModflowSfrDownstreamBrooksCoreyExponent;
    AddTimeList(FMfSfrDownstreamBrooksCorey);

    FMfSfrUpstreamUnsatKz := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrUpstreamUnsatKz.OnTimeListUsed :=  
      ModflowSfrUpstreamDownstreamUnsatKzUsed;
    FMfSfrUpstreamUnsatKz.OnInitialize := InitializeSfrDisplay;
    FMfSfrUpstreamUnsatKz.OnGetUseList := GetMfSfrUpstreamUnsatKzUseList;
    FMfSfrUpstreamUnsatKz.Name := StrModflowSfrUpstreamMaxUnsaturatedKz;
    AddTimeList(FMfSfrUpstreamUnsatKz);

    FMfSfrDownstreamUnsatKz := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfSfrDownstreamUnsatKz.OnTimeListUsed :=  
      ModflowSfrUpstreamDownstreamUnsatKzUsed;
    FMfSfrDownstreamUnsatKz.OnInitialize := InitializeSfrDisplay;
    FMfSfrDownstreamUnsatKz.OnGetUseList := GetMfSfrDownstreamUnsatKzUseList;
    FMfSfrDownstreamUnsatKz.Name := StrModflowSfrDownstreamMaxUnsaturatedKz;
    AddTimeList(FMfSfrDownstreamUnsatKz);
  end;
end;

destructor TSfrPackageSelection.Destroy;
begin
  FParameterInstances.Free;

  FMfSfrUpstreamUnsaturatedWaterContent.Free;
  FMfSfrDownstreamUnsaturatedWaterContent.Free;
  FMfSfrUpstreamUnsatInitialWaterContent.Free;
  FMfSfrDownstreamUnsatInitialWaterContent.Free;
  FMfSfrUpstreamBrooksCorey.Free;
  FMfSfrDownstreamBrooksCorey.Free;
  FMfSfrUpstreamUnsatKz.Free;
  FMfSfrDownstreamUnsatKz.Free;
  FMfSfrUpstreamDepth.Free;
  FMfSfrDownstreamDepth.Free;
  FMfSfrUpstreamElevation.Free;
  FMfSfrDownstreamElevation.Free;
  FMfSfrUpstreamThickness.Free;
  FMfSfrDownstreamThickness.Free;
  FMfSfrUpstreamWidth.Free;
  FMfSfrDownstreamWidth.Free;
  FMfSfrUpstreamHydraulicConductivity.Free;
  FMfSfrDownstreamHydraulicConductivity.Free;
  FMfSfrDepthCoefficient.Free;
  FMfSfrDepthExponent.Free;
  FMfSfrWidthCoefficient.Free;
  FMfSfrWidthExponent.Free;
  FMfSfrBankRoughness.Free;
  FMfSfrChannelRoughness.Free;
  FMfSfrEvapotranspiration.Free;
  FMfSfrPrecipitation.Free;
  FMfSfrRunoff.Free;
  FMfSfrFlow.Free;
  FMfSfrIprior.Free;
  FMfSfrDiversionSegment.Free;
  FMfSfrOutSegment.Free;
  FMfSfrVerticalUnsatK.Free;
  FMfSfrBrooksCorey.Free;
  FMfSfrInitialWaterContent.Free;
  FMfSfrSaturatedWaterContent.Free;
  FMfSfrStreamK.Free;
  FMfSfrStreamThickness.Free;
  FMfSfrStreamSlope.Free;
  FMfSfrStreamTop.Free;
  FMfSfrReachLength.Free;
  FMfSfrIcalc.Free;
  FMfSfrReachNumber.Free;
  FMfSfrSegmentNumber.Free;

  inherited;
end;

procedure TSfrPackageSelection.GetMfSfrBankRoughnessUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrChanelItemUseList(1, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrBrooksCoreyUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrReachUseList(7, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrChanelItemUseList(DataIndex: integer;
  NewUseList: TStringList);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Item: TSfrChannelItem;
  ValueIndex: Integer;
  Boundary: TSfrBoundary;
  LocalModel: TPhastModel;
begin
  LocalModel := FModel as TPhastModel;
  for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowSfrBoundary;
    if (Boundary <> nil) and Boundary.Used then
    begin
      for ValueIndex := 0 to Boundary.ChannelValues.Count -1 do
      begin
        Item := Boundary.ChannelValues[ValueIndex] as TSfrChannelItem;
        UpdateUseList(DataIndex, NewUseList, Item);
      end;
    end;
  end;
end;

procedure TSfrPackageSelection.GetMfSfrChannelRoughnessUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrChanelItemUseList(0, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrDepthCoefficientUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrEquationItemUseList(0, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrDepthExponentUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrEquationItemUseList(1, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrDownstreamBrooksCoreyUseList(
  Sender: TObject; NewUseList: TStringList);
begin
  GetMfSfrDownstreamUnsatItemUseList(2, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrDownstreamDepthUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrDownstreamItemUseList(4, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrDownstreamElevationUseList(
  Sender: TObject; NewUseList: TStringList);
begin
  GetMfSfrDownstreamItemUseList(2, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrDownstreamHydraulicConductivityUseList(
  Sender: TObject; NewUseList: TStringList);
begin
  GetMfSfrDownstreamItemUseList(0, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrDownstreamInitialWatContentUseList(
  Sender: TObject; NewUseList: TStringList);
begin
  GetMfSfrDownstreamUnsatItemUseList(1, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrDownstreamItemUseList(DataIndex: integer;
  NewUseList: TStringList);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Item: TSfrSegmentItem;
  ValueIndex: Integer;
  Boundary: TSfrBoundary;
  LocalModel: TPhastModel;
begin
  LocalModel := FModel as TPhastModel;
  for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowSfrBoundary;
    if (Boundary <> nil) and Boundary.Used then
    begin
      for ValueIndex := 0 to Boundary.DownstreamSegmentValues.Count -1 do
      begin
        Item := Boundary.DownstreamSegmentValues[ValueIndex]
          as TSfrSegmentItem;
        UpdateUseList(DataIndex, NewUseList, Item);
      end;
    end;
  end;
end;

procedure TSfrPackageSelection.GetMfSfrDownstreamSatWatContentUseList(
  Sender: TObject; NewUseList: TStringList);
begin
  GetMfSfrDownstreamUnsatItemUseList(0, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrDownstreamThicknessUseList(
  Sender: TObject; NewUseList: TStringList);
begin
  GetMfSfrDownstreamItemUseList(1, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrDownstreamUnsatItemUseList(
  DataIndex: integer; NewUseList: TStringList);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Item: TSfrUnsatSegmentItem;
  ValueIndex: Integer;
  Boundary: TSfrBoundary;
  LocalModel: TPhastModel;
begin
  LocalModel := FModel as TPhastModel;
  if (Isfropt >= 2) then
  begin
    for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
    begin
      ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      Boundary := ScreenObject.ModflowSfrBoundary;
      if (Boundary <> nil) and Boundary.Used then
      begin
        for ValueIndex := 0 to Boundary.DownstreamUnsatSegmentValues.Count -1 do
        begin
          Item := Boundary.DownstreamUnsatSegmentValues[ValueIndex]
            as TSfrUnsatSegmentItem;
          UpdateUseList(DataIndex, NewUseList, Item);
        end;
      end;
    end;
  end;
end;

procedure TSfrPackageSelection.GetMfSfrDownstreamUnsatKzUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrDownstreamUnsatItemUseList(3, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrDownstreamWidthUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrDownstreamItemUseList(3, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrEquationItemUseList(DataIndex: integer;
  NewUseList: TStringList);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Item: TSfrEquationItem;
  ValueIndex: Integer;
  Boundary: TSfrBoundary;
  LocalModel: TPhastModel;
begin
  LocalModel := FModel as TPhastModel;
  for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowSfrBoundary;
    if (Boundary <> nil) and Boundary.Used then
    begin
      for ValueIndex := 0 to Boundary.EquationValues.Count -1 do
      begin
        Item := Boundary.EquationValues[ValueIndex] as TSfrEquationItem;
        UpdateUseList(DataIndex, NewUseList, Item);
      end;
    end;
  end;
end;

procedure TSfrPackageSelection.GetMfSfrEvapotranspirationUseList(
  Sender: TObject; NewUseList: TStringList);
begin
  GetMfSfrFlowItemUseList(2, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrFlowItemUseList(DataIndex: integer;
  NewUseList: TStringList);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Item: TSfrSegmentFlowItem;
  ValueIndex: Integer;
  Boundary: TSfrBoundary;
  LocalModel: TPhastModel;
begin
  LocalModel := FModel as TPhastModel;
  for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowSfrBoundary;
    if (Boundary <> nil) and Boundary.Used then
    begin
      for ValueIndex := 0 to Boundary.SegmentFlows.Count -1 do
      begin
        Item := Boundary.SegmentFlows[ValueIndex] as TSfrSegmentFlowItem;
        UpdateUseList(DataIndex, NewUseList, Item);
      end;
    end;
  end;
end;

procedure TSfrPackageSelection.GetMfSfrFlowUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrFlowItemUseList(0, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrPrecipitationUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrFlowItemUseList(1, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrReachLengthUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrReachUseList(0, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrReachUseList(DataIndex: integer;
  NewUseList: TStringList);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Item: TSfrItem;
  ValueIndex: Integer;
  Boundary: TSfrBoundary;
  LocalModel: TPhastModel;
begin
  LocalModel := FModel as TPhastModel;
  for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowSfrBoundary;
    if (Boundary <> nil) and Boundary.Used then
    begin
      for ValueIndex := 0 to Boundary.Values.Count -1 do
      begin
        Item := Boundary.Values[ValueIndex] as TSfrItem;
        UpdateUseList(DataIndex, NewUseList, Item);
      end;
    end;
  end;
end;

procedure TSfrPackageSelection.GetMfSfrRunoffUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrFlowItemUseList(3, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrStreamInitialWatContentUseList(
  Sender: TObject; NewUseList: TStringList);
begin
  GetMfSfrReachUseList(6, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrStreamKUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrReachUseList(1, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrStreamSatWatContentUseList(
  Sender: TObject; NewUseList: TStringList);
begin
  GetMfSfrReachUseList(5, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrStreamSlopeUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrReachUseList(4, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrStreamThicknessUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrReachUseList(2, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrStreamTopUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrReachUseList(3, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrUnsatKzUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrReachUseList(8, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrUpstreamBrooksCoreyUseList(
  Sender: TObject; NewUseList: TStringList);
begin
  GetMfSfrUpstreamUnsatItemUseList(2, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrUpstreamDepthUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrUpstreamItemUseList(4, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrUpstreamElevationUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrUpstreamItemUseList(2, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrUpstreamHydraulicConductivityUseList(
  Sender: TObject; NewUseList: TStringList);
begin
  GetMfSfrUpstreamItemUseList(0, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrUpstreamInitialWatContentUseList(
  Sender: TObject; NewUseList: TStringList);
begin
  GetMfSfrUpstreamUnsatItemUseList(1, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrUpstreamItemUseList(DataIndex: integer;
  NewUseList: TStringList);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Item: TSfrSegmentItem;
  ValueIndex: Integer;
  Boundary: TSfrBoundary;
  LocalModel: TPhastModel;
begin
  LocalModel := FModel as TPhastModel;
  for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowSfrBoundary;
    if (Boundary <> nil) and Boundary.Used then
    begin
      for ValueIndex := 0 to Boundary.UpstreamSegmentValues.Count -1 do
      begin
        Item := Boundary.UpstreamSegmentValues[ValueIndex] as TSfrSegmentItem;
        UpdateUseList(DataIndex, NewUseList, Item);
      end;
    end;
  end;
end;

procedure TSfrPackageSelection.GetMfSfrUpstreamSatWatContentUseList(
  Sender: TObject; NewUseList: TStringList);
begin
  GetMfSfrUpstreamUnsatItemUseList(0, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrUpstreamThicknessUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrUpstreamItemUseList(1, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrUpstreamUnsatItemUseList(
  DataIndex: integer; NewUseList: TStringList);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Item: TSfrUnsatSegmentItem;
  ValueIndex: Integer;
  Boundary: TSfrBoundary;
  LocalModel: TPhastModel;
begin
  LocalModel := FModel as TPhastModel;
  if (Isfropt >= 2) then
  begin
    for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
    begin
      ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      Boundary := ScreenObject.ModflowSfrBoundary;
      if (Boundary <> nil) and Boundary.Used then
      begin
        for ValueIndex := 0 to Boundary.UpstreamUnsatSegmentValues.Count -1 do
        begin
          Item := Boundary.UpstreamUnsatSegmentValues[ValueIndex]
            as TSfrUnsatSegmentItem;
          UpdateUseList(DataIndex, NewUseList, Item);
        end;
      end;
    end;
  end;
end;

procedure TSfrPackageSelection.GetMfSfrUpstreamUnsatKzUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrUpstreamUnsatItemUseList(3, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrUpstreamWidthUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrUpstreamItemUseList(3, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  // do nothing.

  // The segment number, reach number and ICALC
  // can not depend on other data sets
end;

procedure TSfrPackageSelection.GetMfSfrWidthCoefficientUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrEquationItemUseList(2, NewUseList);
end;

procedure TSfrPackageSelection.GetMfSfrWidthExponentUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  GetMfSfrEquationItemUseList(3, NewUseList);
end;

procedure TSfrPackageSelection.InitializeSfrDisplay(Sender: TObject);
var
  SfrWriter: TModflowSFR_Writer;
  List: TModflowBoundListOfTimeLists;
  Index: Integer;
  DisplayList: TModflowBoundaryDisplayTimeList;
begin
  List := TModflowBoundListOfTimeLists.Create;
  SfrWriter := TModflowSFR_Writer.Create(FModel as TPhastModel);
  try
    List.Add(MfSfrSegmentNumber);
    List.Add(MfSfrReachNumber);
    List.Add(MfSfrIcalc);
    List.Add(MfSfrReachLength);
    List.Add(MfSfrStreamTop);
    List.Add(MfSfrStreamSlope);
    List.Add(MfSfrStreamThickness);
    List.Add(MfSfrStreamK);
    List.Add(MfSfrSaturatedWaterContent);
    List.Add(MfSfrInitialWaterContent);
    List.Add(MfSfrBrooksCorey);
    List.Add(MfSfrVerticalUnsatK);
    List.Add(MfSfrOutSegment);
    List.Add(MfSfrDiversionSegment);
    List.Add(MfSfrIprior);
    List.Add(MfSfrFlow);
    List.Add(MfSfrRunoff);
    List.Add(MfSfrPrecipitation);
    List.Add(MfSfrEvapotranspiration);
    List.Add(MfSfrChannelRoughness);
    List.Add(MfSfrBankRoughness);
    List.Add(MfSfrDepthCoefficient);
    List.Add(MfSfrDepthExponent);
    List.Add(MfSfrWidthCoefficient);
    List.Add(MfSfrWidthExponent);
    List.Add(MfSfrUpstreamHydraulicConductivity);
    List.Add(MfSfrDownstreamHydraulicConductivity);
    List.Add(MfSfrUpstreamWidth);
    List.Add(MfSfrDownstreamWidth);
    List.Add(MfSfrUpstreamThickness);
    List.Add(MfSfrDownstreamThickness);
    List.Add(MfSfrUpstreamElevation);
    List.Add(MfSfrDownstreamElevation);
    List.Add(MfSfrUpstreamDepth);
    List.Add(MfSfrDownstreamDepth);
    List.Add(MfSfrUpstreamUnsaturatedWaterContent);
    List.Add(MfSfrDownstreamUnsaturatedWaterContent);
    List.Add(MfSfrUpstreamUnsatInitialWaterContent);
    List.Add(MfSfrDownstreamUnsatInitialWaterContent);
    List.Add(MfSfrUpstreamBrooksCorey);
    List.Add(MfSfrDownstreamBrooksCorey);
    List.Add(MfSfrUpstreamUnsatKz);
    List.Add(MfSfrDownstreamUnsatKz);

    for Index := 0 to List.Count - 1 do
    begin
      DisplayList := List[Index];
      DisplayList.CreateDataSets;
    end;

    SfrWriter.UpdateDisplay(List);

    for Index := 0 to List.Count - 1 do
    begin
      DisplayList := List[Index];
      DisplayList.ComputeAverage;
    end;

  finally
    SfrWriter.Free;
    List.Free;
  end;
end;

procedure TSfrPackageSelection.InvalidateAllTimeLists;
begin
  inherited;
  if PackageUsed(nil) then
  begin
    MfSfrBankRoughness.Invalidate;
    MfSfrBrooksCorey.Invalidate;
    MfSfrUpstreamHydraulicConductivity.Invalidate;
    MfSfrDownstreamHydraulicConductivity.Invalidate;
    MfSfrDepthCoefficient.Invalidate;
    MfSfrDepthExponent.Invalidate;
    MfSfrDownstreamBrooksCorey.Invalidate;
    MfSfrDownstreamDepth.Invalidate;
    MfSfrDownstreamElevation.Invalidate;
    MfSfrDownstreamThickness.Invalidate;
    MfSfrDownstreamUnsatInitialWaterContent.Invalidate;
    MfSfrDownstreamUnsatKz.Invalidate;
    MfSfrDownstreamUnsaturatedWaterContent.Invalidate;
    MfSfrDownstreamWidth.Invalidate;
    MfSfrEvapotranspiration.Invalidate;
    MfSfrFlow.Invalidate;
    MfSfrInitialWaterContent.Invalidate;
    MfSfrIprior.Invalidate;
    MfSfrPrecipitation.Invalidate;
    MfSfrReachLength.Invalidate;
    MfSfrRunoff.Invalidate;
    MfSfrSaturatedWaterContent.Invalidate;
    InvalidateMfSfrSegmentReachAndIcalc(nil);
    MfSfrStreamK.Invalidate;
    MfSfrStreamSlope.Invalidate;
    MfSfrStreamThickness.Invalidate;
    MfSfrStreamTop.Invalidate;
    MfSfrUpstreamBrooksCorey.Invalidate;
    MfSfrUpstreamDepth.Invalidate;
    MfSfrUpstreamElevation.Invalidate;
    MfSfrUpstreamThickness.Invalidate;
    MfSfrUpstreamUnsatInitialWaterContent.Invalidate;
    MfSfrDownstreamUnsatKz.Invalidate;
    MfSfrDownstreamUnsaturatedWaterContent.Invalidate;
    MfSfrUpstreamWidth.Invalidate;
    MfSfrVerticalUnsatK.Invalidate;
    MfSfrWidthCoefficient.Invalidate;
    MfSfrWidthExponent.Invalidate;
  end;
end;

procedure TSfrPackageSelection.InvalidateMfSfrSegmentReachAndIcalc(
  Sender: TObject);
begin
  MfSfrSegmentNumber.Invalidate;
  MfSfrReachNumber.Invalidate;
  MfSfrIcalc.Invalidate;
  MfSfrOutSegment.Invalidate;
  MfSfrDiversionSegment.Invalidate;
  MfSfrChannelRoughness.Invalidate;
  MfSfrBankRoughness.Invalidate;
  MfSfrDepthCoefficient.Invalidate;
  MfSfrDepthExponent.Invalidate;
  MfSfrWidthCoefficient.Invalidate;
  MfSfrWidthExponent.Invalidate;
  MfSfrUpstreamHydraulicConductivity.Invalidate;
  MfSfrDownstreamHydraulicConductivity.Invalidate;
  MfSfrUpstreamWidth.Invalidate;
  MfSfrDownstreamWidth.Invalidate;
  MfSfrUpstreamThickness.Invalidate;
  MfSfrDownstreamThickness.Invalidate;
  MfSfrUpstreamElevation.Invalidate;
  MfSfrDownstreamElevation.Invalidate;
  MfSfrUpstreamDepth.Invalidate;
  MfSfrDownstreamDepth.Invalidate;
  MfSfrUpstreamUnsaturatedWaterContent.Invalidate;
  MfSfrDownstreamUnsaturatedWaterContent.Invalidate;
  MfSfrUpstreamUnsatInitialWaterContent.Invalidate;
  MfSfrDownstreamUnsatInitialWaterContent.Invalidate;
  MfSfrUpstreamBrooksCorey.Invalidate;
  MfSfrDownstreamBrooksCorey.Invalidate;
  MfSfrUpstreamUnsatKz.Invalidate;
  MfSfrDownstreamUnsatKz.Invalidate;
end;

function TSfrPackageSelection.ModflowSfrSpatialVariationSelected(
  Sender: TObject): boolean;
begin
  result := IsSelected and (Isfropt in [1,2,3]);
end;

function TSfrPackageSelection.ModflowSfrUnsatKzSpatialVariationSelected(
  Sender: TObject): boolean;
begin
  result := IsSelected and (Isfropt = 3);
end;

function TSfrPackageSelection.ModflowSfrUnsatSpatialVariationSelected(
  Sender: TObject): boolean;
begin
  result := IsSelected and (Isfropt in [2,3]);
end;

function TSfrPackageSelection.ModflowSfrUpstreamDownstreamUnsatKzUsed(
  Sender: TObject): boolean;
begin
  result := IsSelected and (Isfropt = 5);
end;

function TSfrPackageSelection.ModflowSfrUpstreamDownstreamUnsatUsed(
  Sender: TObject): boolean;
begin
  result := IsSelected and (Isfropt in [4,5]);
end;

function TSfrPackageSelection.ModflowSfrUpstreamDownstreamUsed(
  Sender: TObject): boolean;
begin
  result := IsSelected and (Isfropt in [0,4,5]);
end;

procedure TSfrPackageSelection.SetDleak(const Value: double);
begin
  if FDleak <> Value then
  begin
    InvalidateModel;
    FDleak := Value;
  end;
end;

procedure TSfrPackageSelection.SetGageOverallBudget(const Value: boolean);
begin
  if FGageOverallBudget <> Value then
  begin
    InvalidateModel;
    FGageOverallBudget := Value;
  end;
end;

procedure TSfrPackageSelection.SetIsfropt(const Value: integer);
var
  SelectionChanged: boolean;
  PhastModel: TPhastModel;
begin
  if FIsfropt <> Value then
  begin
    InvalidateModel;
    if FModel <> nil then
    begin
      SelectionChanged := (FIsfropt in [1,2,3]) <> (Value in [1,2,3]);
      PhastModel := nil;
      if SelectionChanged then
      begin
        PhastModel := FModel as TPhastModel;
        PhastModel.InvalidateMfSfrStreamTop(self);
        PhastModel.InvalidateMfSfrStreamSlope(self);
        PhastModel.InvalidateMfSfrStreamThickness(self);
        PhastModel.InvalidateMfSfrStreamK(self);
      end;
      SelectionChanged := (FIsfropt in [2,3]) <> (Value in [2,3]);
      if SelectionChanged then
      begin
        if PhastModel = nil then
        begin
          PhastModel := FModel as TPhastModel;
        end;
        PhastModel.InvalidateMfSfrSaturatedWaterContent(self);
        PhastModel.InvalidateMfSfrInitialWaterContent(self);
        PhastModel.InvalidateMfSfrBrooksCorey(self);
      end;
      SelectionChanged := (FIsfropt = 3) <> (Value = 3);
      if SelectionChanged then
      begin
        if PhastModel = nil then
        begin
          PhastModel := FModel as TPhastModel;
        end;
        PhastModel.InvalidateMfSfrVerticalUnsatK(self);
      end;
      SelectionChanged := (FIsfropt in [0,4,5]) <> (Value in [0,4,5]);
      if SelectionChanged then
      begin
        if PhastModel = nil then
        begin
          PhastModel := FModel as TPhastModel;
        end;
        PhastModel.InvalidateMfSfrUpstreamHydraulicConductivity(self);
        PhastModel.InvalidateMfSfrDownstreamHydraulicConductivity(self);
      end;
      SelectionChanged := (FIsfropt in [0,1,2,3]) <> (Value in [0,1,2,3]);
      if SelectionChanged then
      begin
        if PhastModel = nil then
        begin
          PhastModel := FModel as TPhastModel;
        end;
        PhastModel.InvalidateMfSfrUpstreamWidth(self);
        PhastModel.InvalidateMfSfrDownstreamWidth(self);
      end;
      SelectionChanged := (FIsfropt  <> Value);
      if SelectionChanged then
      begin
        if PhastModel = nil then
        begin
          PhastModel := FModel as TPhastModel;
        end;
        PhastModel.InvalidateMfSfrUpstreamThickness(self);
        PhastModel.InvalidateMfSfrDownstreamThickness(self);
        PhastModel.InvalidateMfSfrUpstreamElevation(self);
        PhastModel.InvalidateMfSfrDownstreamElevation(self);
      end;
      SelectionChanged := (FIsfropt in [4,5]) <> (Value in [4,5]);
      if SelectionChanged then
      begin
        if PhastModel = nil then
        begin
          PhastModel := FModel as TPhastModel;
        end;
        PhastModel.InvalidateMfSfrUpstreamUnsaturatedWaterContent(self);
        PhastModel.InvalidateMfSfrDownstreamUnsaturatedWaterContent(self);
        PhastModel.InvalidateMfSfrUpstreamUnsatInitialWaterContent(self);
        PhastModel.InvalidateMfSfrDownstreamUnsatInitialWaterContent(self);
        PhastModel.InvalidateMfSfrUpstreamBrooksCorey(self);
        PhastModel.InvalidateMfSfrDownstreamBrooksCorey(self);
      end;
      SelectionChanged := (FIsfropt = 5) <> (Value = 5);
      if SelectionChanged then
      begin
        if PhastModel = nil then
        begin
          PhastModel := FModel as TPhastModel;
        end;
        PhastModel.InvalidateMfSfrUpstreamUnsatKz(self);
        PhastModel.InvalidateMfSfrDownstreamUnsatKz(self);
      end;

    end;
    FIsfropt := Value;
  end;
end;

procedure TSfrPackageSelection.SetIsSelected(const Value: boolean);
begin
  inherited;
  DischargeRoutingUpdate;
end;

procedure TSfrPackageSelection.SetIsuzn(const Value: integer);
begin
  if FIsuzn <> Value then
  begin
    InvalidateModel;
    FIsuzn := Value;
  end;
end;

procedure TSfrPackageSelection.SetKinematicRouting(const Value: boolean);
begin
  if FKinematicRouting <> Value then
  begin
    InvalidateModel;
    FKinematicRouting := Value;
  end;
end;

procedure TSfrPackageSelection.SetKinematicRoutingTolerance(
  const Value: double);
begin
  if FKinematicRoutingTolerance <> Value then
  begin
    InvalidateModel;
    FKinematicRoutingTolerance := Value;
  end;
end;

procedure TSfrPackageSelection.SetKinematicRoutingWeight(const Value: double);
begin
  if FKinematicRoutingWeight <> Value then
  begin
    InvalidateModel;
    FKinematicRoutingWeight := Value;
  end;
end;

procedure TSfrPackageSelection.SetNsfrsets(const Value: integer);
begin
  if FNsfrsets <> Value then
  begin
    InvalidateModel;
    FNsfrsets := Value;
  end;
end;

procedure TSfrPackageSelection.SetNstrail(const Value: integer);
begin
  if FNstrail <> Value then
  begin
    InvalidateModel;
    FNstrail := Value;
  end;
end;

procedure TSfrPackageSelection.SetParameterInstances(
  const Value: TSfrParamInstances);
begin
  FParameterInstances.Assign(Value);
end;

procedure TSfrPackageSelection.SetPrintFlows(const Value: TPrintFlows);
begin
  if FPrintFlows <> Value then
  begin
    InvalidateModel;
    FPrintFlows := Value;
  end;
end;

procedure TSfrPackageSelection.SetPrintStreams(const Value: boolean);
begin
  if FPrintStreams <> Value then
  begin
    InvalidateModel;
    FPrintStreams := Value;
  end;
  if FPrintStreams then
  begin
    PrintFlows := pfNoPrint;
  end
  else
  begin
    PrintFlows := pfListing;
  end;
end;

procedure TSfrPackageSelection.SetTimeStepsForKinematicRouting(
  const Value: integer);
begin
  if FTimeStepsForKinematicRouting <> Value then
  begin
    InvalidateModel;
    FTimeStepsForKinematicRouting := Value;
  end;
end;

function TSfrPackageSelection.StreamConstant: double;
const
  SfrError = 'SFR Error';
var
  ModflowOptions: TModflowOptions;
  ErrorMessage: string;
begin
  result := 1;
  ModflowOptions := (FModel as TPhastModel).ModflowOptions;
  case ModflowOptions.LengthUnit of
    0: // undefined
      begin
        ErrorMessage :=
          'Length units for model are undefined';
        frmErrorsAndWarnings.AddError(SfrError, ErrorMessage);
      end;
    1: // feet
      begin
        result := 1 / 0.3048;
      end;
    2: // m
      begin
      end;
    3: // cm
      begin
        result := 1 / 100;
      end;
  else
    Assert(False);
  end;
  if result <> 1 then
  begin
    result := Power(result, 1 / 3);
  end;
  case ModflowOptions.TimeUnit of
    0: // Undefined
      begin
        ErrorMessage :=
          'Time units for model are undefined';
        frmErrorsAndWarnings.AddError(SfrError, ErrorMessage);
      end;
    1: // Seconds
      begin
      end;
    2: // Minutes
      begin
        result := result * 60;
      end;
    3: // Hours
      begin
        result := result * 60 * 60;
      end;
    4: // Days
      begin
        result := result * 60 * 60 * 24;
      end;
    5: // Years
      begin
        result := result * 60 * 60 * 24 * 365.25;
      end;
  else
    Assert(False);
  end;
end;

{ TSfrParamInstance }

procedure TSfrParamInstance.Assign(Source: TPersistent);
var
  AnotherItem: TSfrParamInstance;
begin
  AnotherItem := Source as TSfrParamInstance;
  StartTime := AnotherItem.StartTime;
  EndTime := AnotherItem.EndTime;
  ParameterName := AnotherItem.ParameterName;
  ParameterInstance := AnotherItem.ParameterInstance;
  inherited;

end;

function TSfrParamInstance.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TSfrParamInstance;
begin
  result := (AnotherItem is TSfrParamInstance);
  if result then
  begin
    Item := TSfrParamInstance(AnotherItem);
    result :=
      (StartTime = Item.StartTime) and
      (EndTime = Item.EndTime) and
      (ParameterName = Item.ParameterName) and
      (ParameterInstance = Item.ParameterInstance);
  end;
end;

procedure TSfrParamInstance.SetEndTime(const Value: double);
begin
  if FEndTime <> Value then
  begin
    InvalidateModel;
    FEndTime := Value;
  end;
end;

procedure TSfrParamInstance.SetParameterInstance(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ParamIndex: Integer;
  Item: TSfrParamIcalcItem;
begin
  if FParameterInstance <> Value then
  begin
    InvalidateModel;
    PhastModel := (Collection as TOrderedCollection).Model as TPhastModel;
    if PhastModel <> nil then
    begin
      for ScreenObjectIndex := 0 to PhastModel.ScreenObjectCount - 1 do
      begin
        ScreenObject := PhastModel.ScreenObjects[ScreenObjectIndex];
        if ScreenObject.ModflowSfrBoundary <> nil then
        begin
          for ParamIndex := 0 to ScreenObject.ModflowSfrBoundary.ParamIcalc.Count - 1 do
          begin
            Item := ScreenObject.ModflowSfrBoundary.ParamIcalc.Items[ParamIndex];
            if (Item.Param = FParameterName)
              and (Item.ParamInstance = FParameterInstance) then
            begin
              Item.ParamInstance := Value;
            end;
          end;
        end;
      end;
    end;
    FParameterInstance := Value;
  end;
end;

procedure TSfrParamInstance.SetParameterName(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObjectIndex: integer;
  ScreenObject: TScreenObject;
  ParamIndex: Integer;
  Item: TSfrParamIcalcItem;
begin
  if FParameterName <> Value then
  begin
    InvalidateModel;
    PhastModel := (Collection as TOrderedCollection).Model as TPhastModel;
    if PhastModel <> nil then
    begin
      for ScreenObjectIndex := 0 to PhastModel.ScreenObjectCount - 1 do
      begin
        ScreenObject := PhastModel.ScreenObjects[ScreenObjectIndex];
        if ScreenObject.ModflowSfrBoundary <> nil then
        begin
          for ParamIndex := 0 to ScreenObject.ModflowSfrBoundary.ParamIcalc.Count - 1 do
          begin
            Item := ScreenObject.ModflowSfrBoundary.ParamIcalc.Items[ParamIndex];
            if Item.Param = FParameterName then
            begin
              Item.Param := Value;
            end;
          end;
        end;
      end;
    end;
    FParameterName := Value;
  end;
end;

procedure TSfrParamInstance.SetStartTime(const Value: double);
begin
  if FStartTime <> Value then
  begin
    InvalidateModel;
    FStartTime := Value;
  end;
end;

{ TSfrParamInstances }

constructor TSfrParamInstances.Create(Model: TObject);
begin
  inherited Create(TSfrParamInstance, Model);
end;

function TSfrParamInstances.GetItems(Index: integer): TSfrParamInstance;
begin
  result := inherited Items[Index] as TSfrParamInstance
end;

function TSfrParamInstances.ParameterInstanceExists(const ParamName,
  InstaName: string): boolean;
var
  Index: Integer;
  Item: TSfrParamInstance;
begin
  result := False;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    if (Item.ParameterName = ParamName) and (Item.ParameterInstance = InstaName) then
    begin
      result := True;
      Exit;
    end;
  end;
end;

procedure TSfrParamInstances.SetItems(Index: integer;
  const Value: TSfrParamInstance);
begin
  inherited Items[Index] := Value;
end;

procedure TCustomLayerPackageSelection.Assign(Source: TPersistent);
var
  LayerSource: TCustomLayerPackageSelection;
begin
  if Source is TCustomLayerPackageSelection then
  begin
    LayerSource := TCustomLayerPackageSelection(Source);
    LayerOption := LayerSource.LayerOption;
  end;
  inherited;
end;

constructor TCustomLayerPackageSelection.Create(Model: TObject);
begin
  inherited;
  FLayerOption := loTop;
end;

procedure TCustomLayerPackageSelection.SetLayerOption(const Value: TLayerOption);
begin
  if FLayerOption <> Value then
  begin
    InvalidateModel;
    FLayerOption := Value;
    if Assigned(OnLayerChoiceChange) then
    begin
      OnLayerChoiceChange(self);
    end;
  end;
end;

{ TUzfPackageSelection }

procedure TUzfPackageSelection.Assign(Source: TPersistent);
var
  UZF: TUzfPackageSelection;
begin
  if Source is TUzfPackageSelection then
  begin
    UZF := TUzfPackageSelection(Source);
    VerticalKSource := UZF.VerticalKSource;
    RouteDischargeToStreams := UZF.RouteDischargeToStreams;
    SimulateET := UZF.SimulateET;
    NumberOfTrailingWaves := UZF.NumberOfTrailingWaves;
    NumberOfWaveSets := UZF.NumberOfWaveSets;
    PrintSummary := UZF.PrintSummary;
    DepthOfUndulations := UZF.DepthOfUndulations;
  end;
  inherited;
end;

constructor TUzfPackageSelection.Create(Model: TObject);
begin
  inherited;
  VerticalKSource := 1;
  RouteDischargeToStreams := True;
  SimulateET := True;
  NumberOfTrailingWaves := 15;
  NumberOfWaveSets:= 20;
  FDepthOfUndulations := 1;

  if Model <> nil then
  begin
    FMfUzfInfiltration := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfUzfInfiltration.OnInitialize := InitializeUzfDisplay;
    FMfUzfInfiltration.OnGetUseList := GetMfUzfInfiltrationUseList;
    FMfUzfInfiltration.OnTimeListUsed := PackageUsed;
    FMfUzfInfiltration.Name := StrUzfInfiltrationRate;
    AddTimeList(FMfUzfInfiltration);

    FMfUzfExtinctionDepth := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfUzfExtinctionDepth.OnInitialize := InitializeUzfDisplay;
    FMfUzfExtinctionDepth.OnGetUseList := GetMfUzfExtinctionDepthUseList;
    FMfUzfExtinctionDepth.OnTimeListUsed := ModflowUztEtSimulated;
    FMfUzfExtinctionDepth.Name := StrUzfExtinctionDepth;
    AddTimeList(FMfUzfExtinctionDepth);

    FMfUzfWaterContent := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfUzfWaterContent.OnInitialize := InitializeUzfDisplay;
    FMfUzfWaterContent.OnGetUseList := GetMfUzfWaterContentUseList;
    FMfUzfWaterContent.OnTimeListUsed := ModflowUztEtSimulated;
    FMfUzfWaterContent.Name := StrUzfWaterContent;
    AddTimeList(FMfUzfWaterContent);

    FMfUzfEtDemand := TModflowBoundaryDisplayTimeList.Create(Model);
    FMfUzfEtDemand.OnInitialize := InitializeUzfDisplay;
    FMfUzfEtDemand.OnGetUseList := GetMfUzfEtDemandUseList;
    FMfUzfEtDemand.OnTimeListUsed := ModflowUztEtSimulated;
    FMfUzfEtDemand.Name := StrUzfEtDemand;
    AddTimeList(FMfUzfEtDemand);
  end;

end;

destructor TUzfPackageSelection.Destroy;
begin
  FMfUzfInfiltration.Free;
  FMfUzfExtinctionDepth.Free;
  FMfUzfWaterContent.Free;
  FMfUzfEtDemand.Free;
  inherited;
end;

procedure TUzfPackageSelection.GetMfUzfExtinctionDepthUseList(Sender: TObject;
  NewUseList: TStringList);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Item: TCustomModflowBoundaryItem;
  ValueIndex: Integer;
  Boundary: TUzfBoundary;
  LocalModel: TPhastModel;
begin
  LocalModel := FModel as TPhastModel;
  for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowUzfBoundary;
    if (Boundary <> nil) and Boundary.Used then
    begin
      for ValueIndex := 0 to Boundary.ExtinctionDepth.Count -1 do
      begin
        Item := Boundary.ExtinctionDepth[ValueIndex];
        UpdateUseList(0, NewUseList, Item);
      end;
    end;
  end;
end;

procedure TUzfPackageSelection.GetMfUzfInfiltrationUseList(Sender: TObject;
  NewUseList: TStringList);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Item: TCustomModflowBoundaryItem;
  ValueIndex: Integer;
  Boundary: TUzfBoundary;
  LocalModel: TPhastModel;
begin
  LocalModel := FModel as TPhastModel;
  for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowUzfBoundary;
    if (Boundary <> nil) and Boundary.Used then
    begin
      for ValueIndex := 0 to Boundary.Values.Count -1 do
      begin
        Item := Boundary.Values[ValueIndex];
        UpdateUseList(0, NewUseList, Item);
      end;
    end;
  end;
end;

procedure TUzfPackageSelection.GetMfUzfWaterContentUseList(Sender: TObject;
  NewUseList: TStringList);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Item: TCustomModflowBoundaryItem;
  ValueIndex: Integer;
  Boundary: TUzfBoundary;
  LocalModel: TPhastModel;
begin
  LocalModel := FModel as TPhastModel;
  for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowUzfBoundary;
    if (Boundary <> nil) and Boundary.Used then
    begin
      for ValueIndex := 0 to Boundary.WaterContent.Count -1 do
      begin
        Item := Boundary.WaterContent[ValueIndex];
        UpdateUseList(0, NewUseList, Item);
      end;
    end;
  end;
end;

procedure TUzfPackageSelection.GetMfUzfEtDemandUseList(Sender: TObject;
  NewUseList: TStringList);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Item: TCustomModflowBoundaryItem;
  ValueIndex: Integer;
  Boundary: TUzfBoundary;
  LocalModel: TPhastModel;
begin
  LocalModel := FModel as TPhastModel;
  for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowUzfBoundary;
    if (Boundary <> nil) and Boundary.Used then
    begin
      for ValueIndex := 0 to Boundary.EvapotranspirationDemand.Count -1 do
      begin
        Item := Boundary.EvapotranspirationDemand[ValueIndex];
        UpdateUseList(0, NewUseList, Item);
      end;
    end;
  end;
end;

procedure TUzfPackageSelection.InitializeUzfDisplay(Sender: TObject);
var
  UzfWriter: TModflowUzfWriter;
  List: TModflowBoundListOfTimeLists;
begin
  MfUzfInfiltration.CreateDataSets;
  if SimulateET then
  begin
    MfUzfEtDemand.CreateDataSets;
    MfUzfExtinctionDepth.CreateDataSets;
    MfUzfWaterContent.CreateDataSets;
  end
  else
  begin
    MfUzfEtDemand.Clear;
    MfUzfEtDemand.SetUpToDate(True);

    MfUzfExtinctionDepth.Clear;
    MfUzfExtinctionDepth.SetUpToDate(True);

    MfUzfWaterContent.Clear;
    MfUzfWaterContent.SetUpToDate(True);
  end;

  List := TModflowBoundListOfTimeLists.Create;
  UzfWriter := TModflowUzfWriter.Create(FModel as TPhastModel);
  try
    List.Add(MfUzfInfiltration);
    if SimulateET then
    begin
      List.Add(MfUzfEtDemand);
      List.Add(MfUzfExtinctionDepth);
      List.Add(MfUzfWaterContent);
    end
    else
    begin
      List.Add(nil);
      List.Add(nil);
      List.Add(nil);
    end;
    UzfWriter.UpdateDisplay(List);
  finally
    UzfWriter.Free;
    List.Free;
  end;
end;

procedure TUzfPackageSelection.InvalidateAllTimeLists;
begin
  inherited;
  if PackageUsed(FModel) then
  begin
    MfUzfInfiltration.Invalidate;
    MfUzfEtDemand.Invalidate;
    MfUzfExtinctionDepth.Invalidate;
    MfUzfWaterContent.Invalidate;
  end;
end;

function TUzfPackageSelection.ModflowUztEtSimulated(Sender: TObject): boolean;
begin
  result := PackageUsed(Sender) and SimulateET;
end;

procedure TUzfPackageSelection.SetDepthOfUndulations(const Value: double);
begin
  if FDepthOfUndulations <> Value then
  begin
    InvalidateModel;
    FDepthOfUndulations := Value;
  end;
end;

procedure TUzfPackageSelection.SetNumberOfTrailingWaves(const Value: integer);
begin
  if FNumberOfTrailingWaves <> Value then
  begin
    InvalidateModel;
    FNumberOfTrailingWaves := Value;
  end;
end;

procedure TUzfPackageSelection.SetNumberOfWaveSets(const Value: integer);
begin
  if FNumberOfWaveSets <> Value then
  begin
    InvalidateModel;
    FNumberOfWaveSets := Value;
  end;
end;

procedure TUzfPackageSelection.SetPrintSummary(const Value: integer);
begin
  if FPrintSummary <> Value then
  begin
    InvalidateModel;
    Assert(Value in [0,1]);
    FPrintSummary := Value;
  end;
end;

procedure TUzfPackageSelection.SetRouteDischargeToStreams(const Value: boolean);
begin
  if FRouteDischargeToStreams <> Value then
  begin
    InvalidateModel;
    FRouteDischargeToStreams := Value;
  end;
end;

procedure TUzfPackageSelection.SetSimulateET(const Value: boolean);
begin
  if FSimulateET <> Value then
  begin
    InvalidateModel;
    FSimulateET := Value;
  end;
end;

procedure TUzfPackageSelection.SetVerticalKSource(const Value: integer);
begin
  if FVerticalKSource <> Value then
  begin
    InvalidateModel;
    FVerticalKSource := Value;
  end;
end;

procedure TModflowPackageSelection.DischargeRoutingUpdate;
begin
  if FModel <> nil then
  begin
    (FModel as TPhastModel).DischargeRoutingUpdate;
  end;
end;

{ TGmgPackageSelection }

procedure TGmgPackageSelection.Assign(Source: TPersistent);
var
  SourcePkg: TGmgPackageSelection;
begin
  if Source is TGmgPackageSelection then
  begin
    SourcePkg := TGmgPackageSelection(Source);
    RCLOSE := SourcePkg.RCLOSE;
    IITER := SourcePkg.IITER;
    HCLOSE := SourcePkg.HCLOSE;
    MXITER := SourcePkg.MXITER;
    DAMP := SourcePkg.DAMP;
    IADAMP := SourcePkg.IADAMP;
    IOUTGMG := SourcePkg.IOUTGMG;
    IUNITMHC := SourcePkg.IUNITMHC;
    ISM := SourcePkg.ISM;
    ISC := SourcePkg.ISC;
    DUP := SourcePkg.DUP;
    DLOW := SourcePkg.DLOW;
    CHGLIMIT := SourcePkg.CHGLIMIT;
    RELAX := SourcePkg.RELAX;
  end;
  inherited;
end;

constructor TGmgPackageSelection.Create(Model: TObject);
begin
  inherited;
  FCHGLIMIT:= TRealStorage.Create;
  FDUP:= TRealStorage.Create;
  FRELAX:= TRealStorage.Create;
  FRCLOSE:= TRealStorage.Create;
  FDLOW:= TRealStorage.Create;
  FDAMP:= TRealStorage.Create;
  FHCLOSE:= TRealStorage.Create;
  InitializeVariables;
end;

destructor TGmgPackageSelection.Destroy;
begin
  FCHGLIMIT.Free;
  FDUP.Free;
  FRELAX.Free;
  FRCLOSE.Free;
  FDLOW.Free;
  FDAMP.Free;
  FHCLOSE.Free;
  inherited;
end;

procedure TGmgPackageSelection.InitializeVariables;
begin
  FRCLOSE.Value := 1E-05;
  FIITER := 100;
  FMXITER := 100;
  FHCLOSE.Value := 1E-05;
  FDAMP.Value := 1;
  FIADAMP := 0;
  FIOUTGMG := 1;
  FISM := 0;
  FISC := 1;
  FDLOW.Value := 0.001;
  FDUP.Value := 0.7;
  FCHGLIMIT.Value := 0.001;
  FRELAX.Value := 1;
  FIUNITMHC := False;
end;

procedure TGmgPackageSelection.SetCHGLIMIT(const Value: TRealStorage);
begin
  if FCHGLIMIT.Value <> Value.Value then
  begin
    FCHGLIMIT.Assign(Value);
    InvalidateModel;
  end;
end;

procedure TGmgPackageSelection.SetDAMP(const Value: TRealStorage);
begin
  if FDAMP.Value <> Value.Value then
  begin
    FDAMP.Assign(Value);
    InvalidateModel;
  end;
end;

procedure TGmgPackageSelection.SetDLOW(const Value: TRealStorage);
begin
  if FDLOW.Value <> Value.Value then
  begin
    FDLOW.Assign(Value);
    InvalidateModel;
  end;
end;

procedure TGmgPackageSelection.SetDUP(const Value: TRealStorage);
begin
  if FDUP.Value <> Value.Value then
  begin
    FDUP.Assign(Value);
    InvalidateModel;
  end;
end;

procedure TGmgPackageSelection.SetHCLOSE(const Value: TRealStorage);
begin
  if FHCLOSE.Value <> Value.Value then
  begin
    FHCLOSE.Assign(Value);
    InvalidateModel;
  end;
end;

procedure TGmgPackageSelection.SetIADAMP(const Value: integer);
begin
  if FIADAMP <> Value then
  begin
    InvalidateModel;
    FIADAMP := Value;
  end;
end;

procedure TGmgPackageSelection.SetIITER(const Value: integer);
begin
  if FIITER <> Value then
  begin
    InvalidateModel;
    FIITER := Value;
  end;
end;

procedure TGmgPackageSelection.SetIOUTGMG(const Value: integer);
begin
  if FIOUTGMG <> Value then
  begin
    InvalidateModel;
    FIOUTGMG := Value;
  end;
end;

procedure TGmgPackageSelection.SetISC(const Value: integer);
begin
  if FISC <> Value then
  begin
    InvalidateModel;
    FISC := Value;
  end;
end;

procedure TGmgPackageSelection.SetISM(const Value: integer);
begin
  if FISM <> Value then
  begin
    InvalidateModel;
    FISM := Value;
  end;
end;

procedure TGmgPackageSelection.SetIUNITMHC(const Value: Boolean);
begin
  if FIUNITMHC <> Value then
  begin
    InvalidateModel;
    FIUNITMHC := Value;
  end;
end;

procedure TGmgPackageSelection.SetMXITER(const Value: integer);
begin
  if FMXITER <> Value then
  begin
    InvalidateModel;
    FMXITER := Value;
  end;
end;

procedure TGmgPackageSelection.SetRCLOSE(const Value: TRealStorage);
begin
  if FRCLOSE.Value <> Value.Value then
  begin
    FRCLOSE.Assign(Value);
    InvalidateModel;
  end;
end;

procedure TGmgPackageSelection.SetRELAX(const Value: TRealStorage);
begin
  if FRELAX.Value <> Value.Value then
  begin
    FRELAX.Assign(Value);
    InvalidateModel;
  end;
end;

{ TSIPPackageSelection }

procedure TSIPPackageSelection.Assign(Source: TPersistent);
var
  SourcePkg: TSIPPackageSelection;
begin
  if Source is TSIPPackageSelection then
  begin
    SourcePkg := TSIPPackageSelection(Source);
    MXITER := SourcePkg.MXITER;
    NPARM := SourcePkg.NPARM;
    ACCL := SourcePkg.ACCL;
    HCLOSE := SourcePkg.HCLOSE;
    IPCALC := SourcePkg.IPCALC;
    WSEED := SourcePkg.WSEED;
    IPRSIP := SourcePkg.IPRSIP;
  end;
  inherited;
end;

constructor TSIPPackageSelection.Create(Model: TObject);
begin
  inherited Create(Model);
  FACCL := TRealStorage.Create;
  FHCLOSE := TRealStorage.Create;
  FWSEED := TRealStorage.Create;
  InitializeVariables;
end;

destructor TSIPPackageSelection.Destroy;
begin
  FACCL.Free;
  FHCLOSE.Free;
  FWSEED.Free;
  inherited;
end;

procedure TSIPPackageSelection.InitializeVariables;
begin
  MXITER := 100;
  NPARM := 5;
  ACCL.Value := 1;
  HCLOSE.Value := 0.001;
  IPCALC := 1;
  WSEED.Value := 9999;
  IPRSIP := 999;
end;

procedure TSIPPackageSelection.SetACCL(const Value: TRealStorage);
begin
  if FACCL.Value <> Value.Value then
  begin
    FACCL.Assign(Value);
    InvalidateModel;
  end;
end;

procedure TSIPPackageSelection.SetHCLOSE(const Value: TRealStorage);
begin
  if FHCLOSE.Value <> Value.Value then
  begin
    FHCLOSE.Assign(Value);
    InvalidateModel;
  end;
end;

procedure TSIPPackageSelection.SetIPCALC(const Value: integer);
begin
  if FIPCALC <> Value then
  begin
    FIPCALC := Value;
    InvalidateModel;
  end;
end;

procedure TSIPPackageSelection.SetIPRSIP(const Value: integer);
begin
  if FIPRSIP <> Value then
  begin
    FIPRSIP := Value;
    InvalidateModel;
  end;
end;

procedure TSIPPackageSelection.SetMXITER(const Value: integer);
begin
  if FMXITER <> Value then
  begin
    FMXITER := Value;
    InvalidateModel;
  end;
end;

procedure TSIPPackageSelection.SetNPARM(const Value: integer);
begin
  if FNPARM <> Value then
  begin
    FNPARM := Value;
    InvalidateModel;
  end;
end;

procedure TSIPPackageSelection.SetWSEED(const Value: TRealStorage);
begin
  if FWSEED.Value <> Value.Value then
  begin
    FWSEED.Assign(Value);
    InvalidateModel;
  end;
end;

{ TDE4PackageSelection }

procedure TDE4PackageSelection.Assign(Source: TPersistent);
var
  SourcePkg: TDE4PackageSelection;
begin
  if Source is TDE4PackageSelection then
  begin
    SourcePkg := TDE4PackageSelection(Source);
    ITMX := SourcePkg.ITMX;
    MXUP := SourcePkg.MXUP;
    MXLOW := SourcePkg.MXLOW;
    MXBW := SourcePkg.MXBW;
    IFREQ := SourcePkg.IFREQ;
    MUTD4 := SourcePkg.MUTD4;
    ACCL := SourcePkg.ACCL;
    HCLOSE := SourcePkg.HCLOSE;
    IPRD4 := SourcePkg.IPRD4;
  end;
  inherited;
end;

constructor TDE4PackageSelection.Create(Model: TObject);
begin
  inherited;
  FHCLOSE := TRealStorage.Create;
  FACCL := TRealStorage.Create;
  InitializeVariables;
end;

destructor TDE4PackageSelection.Destroy;
begin
  FHCLOSE.Free;
  FACCL.Free;
  inherited;
end;

procedure TDE4PackageSelection.InitializeVariables;
begin
  ITMX := 5;
  MXUP := 0;
  MXLOW := 0;
  MXBW := 0;
  IFREQ := 3;
  MUTD4 := 0;
  ACCL.Value := 1;
  HCLOSE.Value := 0.001;
  IPRD4 := 1;
end;

procedure TDE4PackageSelection.SetACCL(const Value: TRealStorage);
begin
  if FACCL.Value <> Value.Value then
  begin
    FACCL.Assign(Value);
    InvalidateModel;
  end;
end;

procedure TDE4PackageSelection.SetHCLOSE(const Value: TRealStorage);
begin
  if FHCLOSE.Value <> Value.Value then
  begin
    FHCLOSE.Assign(Value);
    InvalidateModel;
  end;
end;

procedure TDE4PackageSelection.SetIFREQ(const Value: integer);
begin
  if FIFREQ <> Value then
  begin
    FIFREQ := Value;
    InvalidateModel;
  end;
end;

procedure TDE4PackageSelection.SetIPRD4(const Value: integer);
begin
  if FIPRD4 <> Value then
  begin
    FIPRD4 := Value;
    InvalidateModel;
  end;
end;

procedure TDE4PackageSelection.SetITMX(const Value: integer);
begin
  if FITMX <> Value then
  begin
    FITMX := Value;
    InvalidateModel;
  end;
end;

procedure TDE4PackageSelection.SetMUTD4(const Value: integer);
begin
  if FMUTD4 <> Value then
  begin
    FMUTD4 := Value;
    InvalidateModel;
  end;
end;

procedure TDE4PackageSelection.SetMXBW(const Value: integer);
begin
  if FMXBW <> Value then
  begin
    FMXBW := Value;
    InvalidateModel;
  end;
end;

procedure TDE4PackageSelection.SetMXLOW(const Value: integer);
begin
  if FMXLOW <> Value then
  begin
    FMXLOW := Value;
    InvalidateModel;
  end;
end;

procedure TDE4PackageSelection.SetMXUP(const Value: integer);
begin
  if FMXUP <> Value then
  begin
    FMXUP := Value;
    InvalidateModel;
  end;
end;

{ THobPackageSelection }

procedure THobPackageSelection.Assign(Source: TPersistent);
begin
  if Source is THobPackageSelection then
  begin
    DryHead := THobPackageSelection(Source).DryHead;
  end;
  inherited;
end;

constructor THobPackageSelection.Create(Model: TObject);
begin
  inherited Create(Model);
  InitializeVariables;
end;

procedure THobPackageSelection.InitializeVariables;
begin
  FDryHead := -1000000;
end;

procedure THobPackageSelection.SetDryHead(const Value: double);
begin
  if FDryHead <> Value then
  begin
    InvalidateModel;
    FDryHead := Value;
  end;
end;

{ TLpfSelection }

procedure TLpfSelection.Assign(Source: TPersistent);
var
  LpfSource: TLpfSelection;
begin
  if Source is TLpfSelection then
  begin
    LpfSource := TLpfSelection( Source);
    UseConstantCV := LpfSource.UseConstantCV;
    UseSaturatedThickness := LpfSource.UseSaturatedThickness;
    UseCvCorrection := LpfSource.UseCvCorrection;
    UseVerticalFlowCorrection := LpfSource.UseVerticalFlowCorrection;
  end;
  inherited;
end;

constructor TLpfSelection.Create(Model: TObject);
begin
  inherited;
  FUseCvCorrection := True;
  FUseVerticalFlowCorrection := True;
end;

procedure TLpfSelection.SetUseConstantCV(const Value: boolean);
begin
  if FUseConstantCV <> Value then
  begin
    InvalidateModel;
    FUseConstantCV := Value;
  end;
end;

procedure TLpfSelection.SetUseCvCorrection(const Value: boolean);
begin
  if FUseCvCorrection <> Value then
  begin
    InvalidateModel;
    FUseCvCorrection := Value;
  end;
end;

procedure TLpfSelection.SetUseSaturatedThickness(const Value: boolean);
begin
  if FUseSaturatedThickness <> Value then
  begin
    InvalidateModel;
    FUseSaturatedThickness := Value;
  end;
end;

procedure TLpfSelection.SetUseStorageCoefficient(const Value: boolean);
begin
  if FUseVerticalFlowCorrection <> Value then
  begin
    InvalidateModel;
    FUseStorageCoefficient := Value;
  end;
end;

procedure TLpfSelection.SetUseVerticalFlowCorrection(const Value: boolean);
begin
  if FUseVerticalFlowCorrection <> Value then
  begin
    InvalidateModel;
    FUseVerticalFlowCorrection := Value;
  end;
end;

{ TModpathSelection }

procedure TModpathSelection.Assign(Source: TPersistent);
var
  ModpathSource: TModpathSelection;
begin
  if Source is TModpathSelection then
  begin
    ModpathSource := TModpathSelection(Source);
    MaximumSize := ModpathSource.MaximumSize;
    EVT_Sink := ModpathSource.EVT_Sink;
    RCH_Source := ModpathSource.RCH_Source;
    Compact := ModpathSource.Compact;
    Binary := ModpathSource.Binary;
    BeginningTime := ModpathSource.BeginningTime;
    EndingTime := ModpathSource.EndingTime;
    ReferenceTime := ModpathSource.ReferenceTime;
    OutputMode := ModpathSource.OutputMode;
    OutputTimes := ModpathSource.OutputTimes;

    StopAfterMaxTime := ModpathSource.StopAfterMaxTime;
    MaxTime := ModpathSource.MaxTime;
    TrackingDirection := ModpathSource.TrackingDirection;
    WeakSink := ModpathSource.WeakSink;
    WeakSinkThreshold := ModpathSource.WeakSinkThreshold;
    StopInZone := ModpathSource.StopInZone;
    StopZoneNumber := ModpathSource.StopZoneNumber;
    EndpointWrite := ModpathSource.EndpointWrite;
    ComputeBudgetInAllCells := ModpathSource.ComputeBudgetInAllCells;
    ErrorTolerance := ModpathSource.ErrorTolerance;
    Summarize := ModpathSource.Summarize;
    MakeBigBudgetFile := ModpathSource.MakeBigBudgetFile;

    TimeSeriesMethod := ModpathSource.TimeSeriesMethod;
    TimeSeriesInterval := ModpathSource.TimeSeriesInterval;
    TimeSeriesMaxCount := ModpathSource.TimeSeriesMaxCount;
    BackwardsTrackingReleaseTime := ModpathSource.BackwardsTrackingReleaseTime;
  end;
  inherited;
end;

constructor TModpathSelection.Create(Model: TObject);
begin
  inherited;
  FOutputTimes := TModpathTimes.Create(Model as TComponent);
  FEVT_Sink := sapVertical;
  FRCH_Source := sapVertical;
  FMakeBigBudgetFile := True;
  FTimeSeriesInterval := 1;
end;

destructor TModpathSelection.Destroy;
begin
  FOutputTimes.Free;
  inherited;
end;

procedure TModpathSelection.InitializeVariables;
begin
  FMaximumSize := 0;
  FRCH_Source := sapVertical;
  FEVT_Sink := sapVertical;
  FCompact := False;
  FBinary := False;
end;

procedure TModpathSelection.SetBackwardsTrackingReleaseTime(
  const Value: double);
begin
  if FBackwardsTrackingReleaseTime <> Value then
  begin
    InvalidateModel;
    FBackwardsTrackingReleaseTime := Value;
  end;
end;

procedure TModpathSelection.SetBeginningTime(const Value: Real);
begin
  if FBeginningTime <> Value then
  begin
    InvalidateModel;
    FBeginningTime := Value;
  end;
end;

procedure TModpathSelection.SetBinary(const Value: boolean);
begin
  if FBinary <> Value then
  begin
    InvalidateModel;
    FBinary := Value;
  end;
end;

procedure TModpathSelection.SetCompact(const Value: boolean);
begin
  if FCompact <> Value then
  begin
    InvalidateModel;
    FCompact := Value;
  end;
end;

procedure TModpathSelection.SetComputeBudgetInAllCells(const Value: boolean);
begin
  if FComputeBudgetInAllCells <> Value then
  begin
    InvalidateModel;
    FComputeBudgetInAllCells := Value;
  end;
end;

procedure TModpathSelection.SetEndingTime(const Value: Real);
begin
  if FEndingTime <> Value then
  begin
    InvalidateModel;
    FEndingTime := Value;
  end;
end;

procedure TModpathSelection.SetEndpointWrite(const Value: TEndpointWrite);
begin
  if FEndpointWrite <> Value then
  begin
    InvalidateModel;
    FEndpointWrite := Value;
  end;
end;

procedure TModpathSelection.SetErrorTolerance(const Value: real);
begin
  if FErrorTolerance <> Value then
  begin
    InvalidateModel;
    FErrorTolerance := Value;
  end;
end;

procedure TModpathSelection.SetEVT_Sink(
  const Value: TSurfaceApplicationPosition);
begin
  if FEVT_Sink <> Value then
  begin
    InvalidateModel;
    FEVT_Sink := Value;
  end;
end;

procedure TModpathSelection.SetMakeBigBudgetFile(const Value: boolean);
begin
  if FMakeBigBudgetFile <> Value then
  begin
    InvalidateModel;
    FMakeBigBudgetFile := Value;
  end;
end;

procedure TModpathSelection.SetMaximumSize(const Value: integer);
begin
  if FMaximumSize <> Value then
  begin
    InvalidateModel;
    FMaximumSize := Value;
  end;
end;

procedure TModpathSelection.SetMaxTime(const Value: real);
begin
  if FMaxTime <> Value then
  begin
    InvalidateModel;
    FMaxTime := Value;
  end;
end;

procedure TModpathSelection.SetOutputMode(const Value: TModpathOutputMode);
begin
  if FOutputMode <> Value then
  begin
    InvalidateModel;
    FOutputMode := Value;
  end;
end;

procedure TModpathSelection.SetOutputTimes(const Value: TModpathTimes);
begin
  FOutputTimes.Assign(Value);
end;

procedure TModpathSelection.SetRCH_Source(
  const Value: TSurfaceApplicationPosition);
begin
  if FRCH_Source <> Value then
  begin
    InvalidateModel;
    FRCH_Source := Value;
  end;
end;

procedure TModpathSelection.SetReferenceTime(const Value: real);
begin
  if FReferenceTime <> Value then
  begin
    InvalidateModel;
    FReferenceTime := Value;
  end;
end;

procedure TModpathSelection.SetStopAfterMaxTime(const Value: boolean);
begin
  if FStopAfterMaxTime <> Value then
  begin
    InvalidateModel;
    FStopAfterMaxTime := Value;
  end;
end;

procedure TModpathSelection.SetStopInZone(const Value: boolean);
begin
  if FStopInZone <> Value then
  begin
    InvalidateModel;
    FStopInZone := Value;
  end;
end;

procedure TModpathSelection.SetStopZoneNumber(const Value: integer);
begin
  if FStopZoneNumber <> Value then
  begin
    InvalidateModel;
    FStopZoneNumber := Value;
  end;
end;

procedure TModpathSelection.SetSummarize(const Value: boolean);
begin
  if FSummarize <> Value then
  begin
    InvalidateModel;
    FSummarize := Value;
  end;
end;

procedure TModpathSelection.SetTimeSeriesInterval(const Value: double);
begin
  if FTimeSeriesInterval <> Value then
  begin
    InvalidateModel;
    FTimeSeriesInterval := Value;
  end;
end;

procedure TModpathSelection.SetTimeSeriesMaxCount(const Value: integer);
begin
  if FTimeSeriesMaxCount <> Value then
  begin
    InvalidateModel;
    FTimeSeriesMaxCount := Value;
  end;
end;

procedure TModpathSelection.SetTimeSeriesMethod(const Value: TTimeSeriesMethod);
begin
  if FTimeSeriesMethod <> Value then
  begin
    InvalidateModel;
    FTimeSeriesMethod := Value;
  end;
end;

procedure TModpathSelection.SetTrackingDirection(
  const Value: TTrackingDirection);
begin
  if FTrackingDirection <> Value then
  begin
    InvalidateModel;
    FTrackingDirection := Value;
  end;
end;

procedure TModpathSelection.SetWeakSink(const Value: TWeakSink);
begin
  if FWeakSink <> Value then
  begin
    InvalidateModel;
    FWeakSink := Value;
  end;
end;

procedure TModpathSelection.SetWeakSinkThreshold(const Value: real);
begin
  if FWeakSinkThreshold <> Value then
  begin
    InvalidateModel;
    FWeakSinkThreshold := Value;
  end;
end;

function TModpathSelection.ShouldCreateTimeFile: boolean;
begin
  result := (OutputMode in [mopPathline,mopTimeSeries])
      and (TimeSeriesMethod = tsmIndividual)
      and (OutputTimes.Count > 0)
end;

{ TWellPackage }

constructor TWellPackage.Create(Model: TObject);
begin
  inherited;
  if Model <> nil then
  begin
    FMfWellPumpage := TModflowBoundaryDisplayTimeList.Create(Model);
    MfWellPumpage.OnInitialize := InitializeMfWellPumpage;
    MfWellPumpage.OnGetUseList := GetMfWellUseList;
    MfWellPumpage.OnTimeListUsed := PackageUsed;
    MfWellPumpage.Name := StrMODFLOWWellPumping;
    AddTimeList(MfWellPumpage);
  end;
end;

destructor TWellPackage.Destroy;
begin
  FMfWellPumpage.Free;
  inherited;
end;

procedure TWellPackage.GetMfWellUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  UpdateDisplayUseList(NewUseList, ptQ, 0, 'Well Pumping Rate');
end;

procedure TWellPackage.InitializeMfWellPumpage(Sender: TObject);
var
  WellWriter: TModflowWEL_Writer;
  List: TModflowBoundListOfTimeLists;
begin
  MfWellPumpage.CreateDataSets;
  List := TModflowBoundListOfTimeLists.Create;
  WellWriter := TModflowWEL_Writer.Create(FModel as TPhastModel);
  try
    List.Add(MfWellPumpage);
    WellWriter.UpdateDisplay(List, [0]);
  finally
    WellWriter.Free;
    List.Free;
  end;
  MfWellPumpage.LabelAsSum;
end;

procedure TWellPackage.InvalidateAllTimeLists;
begin
  inherited;
  if PackageUsed(FModel) then
  begin
    InvalidateMfWellPumpage(FModel);
  end;
end;

procedure TWellPackage.InvalidateMfWellPumpage(Sender: TObject);
begin
  MfWellPumpage.Invalidate;
end;

{ TGhbPackage }

constructor TGhbPackage.Create(Model: TObject);
begin
  inherited;
  if Model <> nil then
  begin
    FMfGhbConductance := TModflowBoundaryDisplayTimeList.Create(FModel);
    MfGhbConductance.OnInitialize := InitializeGhbDisplay;
    MfGhbConductance.OnGetUseList := GetMfGhbConductanceUseList;
    MfGhbConductance.OnTimeListUsed := PackageUsed;
    MfGhbConductance.Name := StrMODFLOWGhbConductance;
    AddTimeList(MfGhbConductance);

    FMfGhbBoundaryHead := TModflowBoundaryDisplayTimeList.Create(FModel);
    MfGhbBoundaryHead.OnInitialize := InitializeGhbDisplay;
    MfGhbBoundaryHead.OnGetUseList := GetMfGhbBoundaryHeadUseList;
    MfGhbBoundaryHead.OnTimeListUsed := PackageUsed;
    MfGhbBoundaryHead.Name := StrMODFLOWGhbHead;
    AddTimeList(MfGhbBoundaryHead);
  end;
end;

destructor TGhbPackage.Destroy;
begin
  FMfGhbConductance.Free;
  FMfGhbBoundaryHead.Free;
  inherited;
end;

procedure TGhbPackage.GetMfGhbBoundaryHeadUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  UpdateDisplayUseList(NewUseList, ptGHB, 0, 'GHB Boundary Head');
end;

procedure TGhbPackage.GetMfGhbConductanceUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  UpdateDisplayUseList(NewUseList, ptGHB, 1, 'GHB Conductance');
end;

procedure TGhbPackage.InitializeGhbDisplay(Sender: TObject);
var
  GhbWriter: TModflowGHB_Writer;
  List: TModflowBoundListOfTimeLists;
begin
  MfGhbConductance.CreateDataSets;
  MfGhbBoundaryHead.CreateDataSets;

  List := TModflowBoundListOfTimeLists.Create;
  GhbWriter := TModflowGHB_Writer.Create(FModel as TPhastModel);
  try
    List.Add(MfGhbBoundaryHead);
    List.Add(MfGhbConductance);
    GhbWriter.UpdateDisplay(List, [1]);
  finally
    GhbWriter.Free;
    List.Free;
  end;
  MfGhbConductance.ComputeAverage;
  MfGhbBoundaryHead.ComputeAverage;
end;

procedure TGhbPackage.InvalidateAllTimeLists;
begin
  if PackageUsed(FModel) then
  begin
    MfGhbBoundaryHead.Invalidate;
    MfGhbConductance.Invalidate;
  end;
end;

{ TDrnPackage }

constructor TDrnPackage.Create(Model: TObject);
begin
  inherited;
  if Model <> nil then
  begin
    FMfDrnConductance := TModflowBoundaryDisplayTimeList.Create(Model);
    MfDrnConductance.OnInitialize := InitializeDrnDisplay;
    MfDrnConductance.OnGetUseList := GetMfDrnConductanceUseList;
    MfDrnConductance.OnTimeListUsed := PackageUsed;
    MfDrnConductance.Name := StrMODFLOWDrainConductance;
    AddTimeList(MfDrnConductance);

    FMfDrnElevation := TModflowBoundaryDisplayTimeList.Create(Model);
    MfDrnElevation.OnInitialize := InitializeDrnDisplay;
    MfDrnElevation.OnGetUseList := GetMfDrnElevationUseList;
    MfDrnElevation.OnTimeListUsed := PackageUsed;
    MfDrnElevation.Name := StrMODFLOWDrainElevation;
    AddTimeList(MfDrnElevation);
  end;
end;

destructor TDrnPackage.Destroy;
begin
  FMfDrnConductance.Free;
  FMfDrnElevation.Free;
  inherited;
end;

procedure TDrnPackage.GetMfDrnConductanceUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  UpdateDisplayUseList(NewUseList, ptDRN, 1, 'Drain Conductance');
end;

procedure TDrnPackage.GetMfDrnElevationUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  UpdateDisplayUseList(NewUseList, ptDRN, 0, 'Drain Elevation');
end;

procedure TDrnPackage.InitializeDrnDisplay(Sender: TObject);
var
  DrnWriter: TModflowDRN_Writer;
  List: TModflowBoundListOfTimeLists;
begin
  MfDrnConductance.CreateDataSets;
  MfDrnElevation.CreateDataSets;

  List := TModflowBoundListOfTimeLists.Create;
  DrnWriter := TModflowDRN_Writer.Create(FModel as TPhastModel);
  try
    List.Add(MfDrnElevation);
    List.Add(MfDrnConductance);
    DrnWriter.UpdateDisplay(List, [1]);
  finally
    DrnWriter.Free;
    List.Free;
  end;
  MfDrnConductance.ComputeAverage;
  MfDrnElevation.ComputeAverage;
end;

procedure TDrnPackage.InvalidateAllTimeLists;
begin
  if PackageUsed(FModel) then
  begin
    MfDrnElevation.Invalidate;
    MfDrnConductance.Invalidate;
  end;
end;

{ TDrtPackage }

constructor TDrtPackage.Create(Model: TObject);
begin
  inherited;
  if Model <> nil then
  begin
    FMfDrtConductance := TModflowBoundaryDisplayTimeList.Create(Model);
    MfDrtConductance.OnInitialize := InitializeDrtDisplay;
    MfDrtConductance.OnGetUseList := GetMfDrtConductanceUseList;
    MfDrtConductance.OnTimeListUsed := PackageUsed;
    MfDrtConductance.Name := StrMODFLOWDrainReturnConductance;
    AddTimeList(MfDrtConductance);

    FMfDrtElevation := TModflowBoundaryDisplayTimeList.Create(Model);
    MfDrtElevation.OnInitialize := InitializeDrtDisplay;
    MfDrtElevation.OnGetUseList := GetMfDrtElevationUseList;
    MfDrtElevation.OnTimeListUsed := PackageUsed;
    MfDrtElevation.Name := StrMODFLOWDrainReturnElevation;
    AddTimeList(MfDrtElevation);

    FMfDrtReturnFraction := TModflowBoundaryDisplayTimeList.Create(Model);
    MfDrtReturnFraction.OnInitialize := InitializeDrtDisplay;
    MfDrtReturnFraction.OnGetUseList := GetMfDrtReturnFractionUseList;
    MfDrtReturnFraction.OnTimeListUsed := PackageUsed;
    MfDrtReturnFraction.Name := StrMODFLOWDrainReturnFraction;
    AddTimeList(MfDrtReturnFraction);
  end;
end;

destructor TDrtPackage.Destroy;
begin
  FMfDrtReturnFraction.Free;
  FMfDrtElevation.Free;
  FMfDrtConductance.Free;
  inherited;
end;

procedure TDrtPackage.GetMfDrtConductanceUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  UpdateDisplayUseList(NewUseList, ptDRT, 1, 'Drain Return Conductance');
end;

procedure TDrtPackage.GetMfDrtElevationUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  UpdateDisplayUseList(NewUseList, ptDRT, 0, 'Drain Return Elevation');
end;

procedure TDrtPackage.GetMfDrtReturnFractionUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  UpdateDisplayUseList(NewUseList, ptDRT, 2, 'Drain Return Fraction');
end;

procedure TDrtPackage.InitializeDrtDisplay(Sender: TObject);
var
  DrtWriter: TModflowDRT_Writer;
  List: TModflowBoundListOfTimeLists;
begin
  MfDrtConductance.CreateDataSets;
  MfDrtElevation.CreateDataSets;
  MfDrtReturnFraction.CreateDataSets;

  List := TModflowBoundListOfTimeLists.Create;
  DrtWriter := TModflowDRT_Writer.Create(FModel as TPhastModel);
  try
    List.Add(MfDrtElevation);
    List.Add(MfDrtConductance);
    List.Add(MfDrtReturnFraction);
    DrtWriter.UpdateDisplay(List, [1]);
  finally
    DrtWriter.Free;
    List.Free;
  end;
  MfDrtConductance.ComputeAverage;
  MfDrtElevation.ComputeAverage;
  MfDrtReturnFraction.ComputeAverage;
end;

procedure TDrtPackage.InvalidateAllTimeLists;
begin
  inherited;
  if PackageUsed(FModel) then
  begin
    MfDrtConductance.Invalidate;
    MfDrtElevation.Invalidate;
    MfDrtReturnFraction.Invalidate;
  end;
end;

{ TRivPackage }

constructor TRivPackage.Create(Model: TObject);
begin
  inherited;
  if Model <> nil then
  begin
    FMfRivConductance := TModflowBoundaryDisplayTimeList.Create(Model);
    MfRivConductance.OnInitialize := InitializeRivDisplay;
    MfRivConductance.OnGetUseList := GetMfRivConductanceUseList;
    MfRivConductance.OnTimeListUsed := PackageUsed;
    MfRivConductance.Name := StrMODFLOWRiverConductance;
    AddTimeList(MfRivConductance);

    FMfRivStage := TModflowBoundaryDisplayTimeList.Create(Model);
    MfRivStage.OnInitialize := InitializeRivDisplay;
    MfRivStage.OnGetUseList := GetMfRivStageUseList;
    MfRivStage.OnTimeListUsed := PackageUsed;
    MfRivStage.Name := StrMODFLOWRiverStage;
    AddTimeList(MfRivStage);

    FMfRivBottom := TModflowBoundaryDisplayTimeList.Create(Model);
    MfRivBottom.OnInitialize := InitializeRivDisplay;
    MfRivBottom.OnGetUseList := GetMfRivBottomUseList;
    MfRivBottom.OnTimeListUsed := PackageUsed;
    MfRivBottom.Name := StrMODFLOWRiverBottom;
    AddTimeList(MfRivBottom);
  end;
end;

destructor TRivPackage.Destroy;
begin
  FMfRivConductance.Free;
  FMfRivBottom.Free;
  FMfRivStage.Free;
  inherited;
end;

procedure TRivPackage.GetMfRivBottomUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  UpdateDisplayUseList(NewUseList, ptRIV, 2, 'River Bottom Elevation');
end;

procedure TRivPackage.GetMfRivConductanceUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  UpdateDisplayUseList(NewUseList, ptRIV, 1, 'River Conductance');
end;

procedure TRivPackage.GetMfRivStageUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  UpdateDisplayUseList(NewUseList, ptRIV, 0, 'River Stage');
end;

procedure TRivPackage.InitializeRivDisplay(Sender: TObject);
var
  RivWriter: TModflowRIV_Writer;
  List: TModflowBoundListOfTimeLists;
begin
  MfRivConductance.CreateDataSets;
  MfRivStage.CreateDataSets;
  MfRivBottom.CreateDataSets;

  List := TModflowBoundListOfTimeLists.Create;
  RivWriter := TModflowRIV_Writer.Create(FModel as TPhastModel);
  try
    List.Add(MfRivStage);
    List.Add(MfRivConductance);
    List.Add(MfRivBottom);
    RivWriter.UpdateDisplay(List, [1]);
  finally
    RivWriter.Free;
    List.Free;
  end;
  MfRivConductance.ComputeAverage;
  MfRivStage.ComputeAverage;
  MfRivBottom.ComputeAverage;
end;

procedure TRivPackage.InvalidateAllTimeLists;
begin
  inherited;
  if PackageUsed(FModel) then
  begin
    MfRivConductance.Invalidate;
    MfRivStage.Invalidate;
    MfRivBottom.Invalidate;
  end;
end;

{ TChdPackage }

constructor TChdPackage.Create(Model: TObject);
begin
  inherited;
  if Model <> nil then
  begin
    FMfChdStartingHead := TModflowBoundaryDisplayTimeList.Create(Model);
    MfChdStartingHead.OnInitialize := InitializeChdDisplay;
    MfChdStartingHead.OnGetUseList := GetMfChdStartingHeadUseList;
    MfChdStartingHead.OnTimeListUsed := PackageUsed;
    MfChdStartingHead.Name := StrMODFLOWCHDStartingHead;
    AddTimeList(MfChdStartingHead);

    FMfChdEndingHead := TModflowBoundaryDisplayTimeList.Create(Model);
    MfChdEndingHead.OnInitialize := InitializeChdDisplay;
    MfChdEndingHead.OnGetUseList := GetMfChdEndingHeadUseList;
    MfChdEndingHead.OnTimeListUsed := PackageUsed;
    MfChdEndingHead.Name := StrMODFLOWCHDEndingHead;
    AddTimeList(MfChdEndingHead);
  end;
end;

destructor TChdPackage.Destroy;
begin
  FMfChdStartingHead.Free;
  FMfChdEndingHead.Free;
  inherited;
end;

procedure TChdPackage.GetMfChdEndingHeadUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  UpdateDisplayUseList(NewUseList, ptCHD, 1, 'CHD Ending Head');
end;

procedure TChdPackage.GetMfChdStartingHeadUseList(Sender: TObject;
  NewUseList: TStringList);
begin
  UpdateDisplayUseList(NewUseList, ptCHD, 0, 'CHD Starting Head');
end;

procedure TChdPackage.InitializeChdDisplay(Sender: TObject);
var
  ChdWriter: TModflowCHD_Writer;
  List: TModflowBoundListOfTimeLists;
begin
  MfChdStartingHead.CreateDataSets;
  MfChdEndingHead.CreateDataSets;

  List := TModflowBoundListOfTimeLists.Create;
  ChdWriter := TModflowCHD_Writer.Create(FModel as TPhastModel);
  try
    List.Add(MfChdStartingHead);
    List.Add(MfChdEndingHead);
    ChdWriter.UpdateDisplay(List, [0,1]);
  finally
    ChdWriter.Free;
    List.Free;
  end;
  MfChdStartingHead.LabelAsSum;
  MfChdEndingHead.LabelAsSum;
end;

procedure TChdPackage.InvalidateAllTimeLists;
begin
  inherited;
  if PackageUsed(FModel) then
  begin
    FMfChdStartingHead.Invalidate;
    FMfChdEndingHead.Invalidate;
  end;
end;

{ THufPackageSelection }

procedure THufPackageSelection.Assign(Source: TPersistent);
var
  HufPkg: THufPackageSelection;
begin
  if Source is THufPackageSelection then
  begin
    HufPkg := THufPackageSelection(Source);
    SaveHeads := HufPkg.SaveHeads;
    SaveFlows := HufPkg.SaveFlows;
    ReferenceChoice := HufPkg.ReferenceChoice;
  end;
  inherited;

end;

constructor THufPackageSelection.Create(Model: TObject);
begin
  inherited;
  InitializeVariables;
end;

procedure THufPackageSelection.InitializeVariables;
begin
  FSaveFlows := True;
  FSaveHeads := True;
  FReferenceChoice := hrcModelTop;
end;

procedure THufPackageSelection.SetReferenceChoice(
  const Value: THufReferenceChoice);
begin
  if FReferenceChoice <> Value then
  begin
    InvalidateModel;
    FReferenceChoice := Value;
  end;
end;

procedure THufPackageSelection.SetSaveFlows(const Value: boolean);
begin
  if FSaveFlows <> Value then
  begin
    InvalidateModel;
    FSaveFlows := Value;
  end;
end;

procedure THufPackageSelection.SetSaveHeads(const Value: boolean);
begin
  if FSaveHeads <> Value then
  begin
    InvalidateModel;
    FSaveHeads := Value;
  end;
end;

{ TMultinodeWellSelection }

procedure TMultinodeWellSelection.Assign(Source: TPersistent);
var
  MnwSource: TMultinodeWellSelection;
begin
  if Source is TMultinodeWellSelection then
  begin
    MnwSource := TMultinodeWellSelection(Source);
    PrintOption := MnwSource.PrintOption;
    CreateWellFile := MnwSource.CreateWellFile;
    SummarizeByWell := MnwSource.SummarizeByWell;
    SummarizeByNode := MnwSource.SummarizeByNode;
  end;
  inherited;
end;

constructor TMultinodeWellSelection.Create(Model: TObject);
begin
  inherited;
  FPrintOption := mpoMost;

  if Model <> nil then
  begin
    FMfMnwWellRadius := TModflowBoundaryDisplayTimeList.Create(Model);
    MfMnwWellRadius.OnInitialize := InitializeMnw2Display;
    MfMnwWellRadius.OnGetUseList := GetMfMnwWellRadiusUseList;
    MfMnwWellRadius.OnTimeListUsed := PackageUsed;
    MfMnwWellRadius.Name := StrWellRadius;
    AddTimeList(MfMnwWellRadius);

    FMfMnwSkinRadius := TModflowBoundaryDisplayTimeList.Create(Model);
    MfMnwSkinRadius.OnInitialize := InitializeMnw2Display;
    MfMnwSkinRadius.OnGetUseList := GetMfMnwSkinRadiusUseList;
    MfMnwSkinRadius.OnTimeListUsed := PackageUsed;
    MfMnwSkinRadius.Name := StrSkinRadius;
    AddTimeList(MfMnwSkinRadius);

    FMfMnwSkinK := TModflowBoundaryDisplayTimeList.Create(Model);
    MfMnwSkinK.OnInitialize := InitializeMnw2Display;
    MfMnwSkinK.OnGetUseList := GetMfMnwSkinKUseList;
    MfMnwSkinK.OnTimeListUsed := PackageUsed;
    MfMnwSkinK.Name := StrSkinK;
    AddTimeList(MfMnwSkinK);

    FMfMnwB := TModflowBoundaryDisplayTimeList.Create(Model);
    MfMnwB.OnInitialize := InitializeMnw2Display;
    MfMnwB.OnGetUseList := GetMfMnwBUseList;
    MfMnwB.OnTimeListUsed := PackageUsed;
    MfMnwB.Name := StrB;
    AddTimeList(MfMnwB);

    FMfMnwC := TModflowBoundaryDisplayTimeList.Create(Model);
    MfMnwC.OnInitialize := InitializeMnw2Display;
    MfMnwC.OnGetUseList := GetMfMnwCUseList;
    MfMnwC.OnTimeListUsed := PackageUsed;
    MfMnwC.Name := StrC;
    AddTimeList(MfMnwC);

    FMfMnwP := TModflowBoundaryDisplayTimeList.Create(Model);
    MfMnwP.OnInitialize := InitializeMnw2Display;
    MfMnwP.OnGetUseList := GetMfMnwPUseList;
    MfMnwP.OnTimeListUsed := PackageUsed;
    MfMnwP.Name := StrP;
    AddTimeList(MfMnwP);

    FMfMnwCellToWellConductance := TModflowBoundaryDisplayTimeList.Create(Model);
    MfMnwCellToWellConductance.OnInitialize := InitializeMnw2Display;
    MfMnwCellToWellConductance.OnGetUseList := GetMfMnwCellToWellConductanceUseList;
    MfMnwCellToWellConductance.OnTimeListUsed := PackageUsed;
    MfMnwCellToWellConductance.Name := StrCellToWellConductance;
    AddTimeList(MfMnwCellToWellConductance);

    FMfMnwPartialPenetration := TModflowBoundaryDisplayTimeList.Create(Model);
    MfMnwPartialPenetration.OnInitialize := InitializeMnw2Display;
    MfMnwPartialPenetration.OnGetUseList := GetMfMnwPartialPenetrationUseList;
    MfMnwPartialPenetration.OnTimeListUsed := PackageUsed;
    MfMnwPartialPenetration.Name := StrPartialPenetration;
    AddTimeList(MfMnwPartialPenetration);
  end;

end;

destructor TMultinodeWellSelection.Destroy;
begin
  FMfMnwPartialPenetration.Free;
  FMfMnwCellToWellConductance.Free;
  FMfMnwP.Free;
  FMfMnwC.Free;
  FMfMnwB.Free;
  FMfMnwSkinK.Free;
  FMfMnwSkinRadius.Free;
  FMfMnwWellRadius.Free;
  inherited;
end;

procedure TMultinodeWellSelection.GetMfMnwUseList(
  Sender: TObject; NewUseList: TStringList; DataIndex: integer);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Item: TCustomModflowBoundaryItem;
  ValueIndex: Integer;
  Boundary: TMnw2Boundary;
  PhastModel: TPhastModel;
begin
  PhastModel := FModel as TPhastModel;
  for ScreenObjectIndex := 0 to PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := PhastModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowMnw2Boundary;
    if (Boundary <> nil) and Boundary.Used then
    begin
      if Boundary.DataTypeUsed(DataIndex) then
      begin
        for ValueIndex := 0 to Boundary.Values.Count -1 do
        begin
          Item := Boundary.Values[ValueIndex];
          UpdateUseList(DataIndex, NewUseList, Item);
        end;
      end;
    end;
  end;
end;

procedure TMultinodeWellSelection.GetMfMnwWellRadiusUseList(
  Sender: TObject; NewUseList: TStringList);
begin
  GetMfMnwUseList(Sender, NewUseList, WellRadiusPosition);
end;

procedure TMultinodeWellSelection.GetMfMnwSkinRadiusUseList(
  Sender: TObject; NewUseList: TStringList);
begin
  GetMfMnwUseList(Sender, NewUseList, SkinRadiusPosition);
end;

procedure TMultinodeWellSelection.GetMfMnwSkinKUseList(
  Sender: TObject; NewUseList: TStringList);
begin
  GetMfMnwUseList(Sender, NewUseList, SkinKPosition);
end;

procedure TMultinodeWellSelection.GetMfMnwBUseList(
  Sender: TObject; NewUseList: TStringList);
begin
  GetMfMnwUseList(Sender, NewUseList, BPosition);
end;

procedure TMultinodeWellSelection.GetMfMnwCUseList(
  Sender: TObject; NewUseList: TStringList);
begin
  GetMfMnwUseList(Sender, NewUseList, CPosition);
end;

procedure TMultinodeWellSelection.GetMfMnwPUseList(
  Sender: TObject; NewUseList: TStringList);
begin
  GetMfMnwUseList(Sender, NewUseList, PPosition);
end;

procedure TMultinodeWellSelection.GetMfMnwCellToWellConductanceUseList(
  Sender: TObject; NewUseList: TStringList);
begin
  GetMfMnwUseList(Sender, NewUseList, CellToWellConductancePosition);
end;

procedure TMultinodeWellSelection.GetMfMnwPartialPenetrationUseList(
  Sender: TObject; NewUseList: TStringList);
begin
  GetMfMnwUseList(Sender, NewUseList, PartialPenetrationPosition);
end;

procedure TMultinodeWellSelection.InitializeMnw2Display(Sender: TObject);
var
  List: TModflowBoundListOfTimeLists;
  Mnw2Writer: TModflowMNW2_Writer;
  Index: Integer;
  TimeList: TModflowBoundaryDisplayTimeList;
begin
  List := TModflowBoundListOfTimeLists.Create;
  Mnw2Writer := TModflowMNW2_Writer.Create(FModel as TPhastModel);
  try
    List.Add(MfMnwWellRadius);
    List.Add(MfMnwSkinRadius);
    List.Add(MfMnwSkinK);
    List.Add(MfMnwB);
    List.Add(MfMnwC);
    List.Add(MfMnwP);
    List.Add(MfMnwCellToWellConductance);
    List.Add(MfMnwPartialPenetration);
    for Index := 0 to List.Count - 1 do
    begin
      TimeList := List[Index];
      TimeList.CreateDataSets;
    end;
    Mnw2Writer.UpdateDisplay(List);
    for Index := 0 to List.Count - 1 do
    begin
      TimeList := List[Index];
      TimeList.ComputeAverage;
    end;
  finally
    Mnw2Writer.Free;
    List.Free;
  end;
end;

procedure TMultinodeWellSelection.SetCreateWellFile(const Value: Boolean);
begin
  if FCreateWellFile <> Value then
  begin
    FCreateWellFile := Value;
    InvalidateModel;
  end;
end;

procedure TMultinodeWellSelection.SetPrintOption(const Value: TMnw2PrintOption);
begin
  if FPrintOption <> Value then
  begin
    FPrintOption := Value;
    InvalidateModel;
  end;
end;

procedure TMultinodeWellSelection.SetSummarizeByNode(const Value: Boolean);
begin
  if FSummarizeByNode <> Value then
  begin
    FSummarizeByNode := Value;
    InvalidateModel;
  end;
end;

procedure TMultinodeWellSelection.SetSummarizeByWell(const Value: Boolean);
begin
  if FSummarizeByWell <> Value then
  begin
    FSummarizeByWell := Value;
    InvalidateModel;
  end;
end;

{ TSubPrintItem }

procedure TSubPrintItem.Assign(Source: TPersistent);
var
  SourceItem: TSubPrintItem;
begin
  if Source is TSubPrintItem then
  begin
    SourceItem := TSubPrintItem(Source);
    StartTime := SourceItem.StartTime;
    EndTime := SourceItem.EndTime;
    PrintSubsidence := SourceItem.PrintSubsidence;
    SaveSubsidence := SourceItem.SaveSubsidence;
    PrintCompactionByModelLayer := SourceItem.PrintCompactionByModelLayer;
    SaveCompactionByModelLayer := SourceItem.SaveCompactionByModelLayer;
    PrintCompactionByInterbedSystem := SourceItem.PrintCompactionByInterbedSystem;
    SaveCompactionByInterbedSystem := SourceItem.SaveCompactionByInterbedSystem;
    PrintVerticalDisplacement := SourceItem.PrintVerticalDisplacement;
    SaveVerticalDisplacement := SourceItem.SaveVerticalDisplacement;
    PrintCriticalHeadNoDelay := SourceItem.PrintCriticalHeadNoDelay;
    SaveCriticalHeadNoDelay := SourceItem.SaveCriticalHeadNoDelay;
    PrintCriticalHeadDelay := SourceItem.PrintCriticalHeadDelay;
    SaveCriticalHeadDelay := SourceItem.SaveCriticalHeadDelay;
    PrintDelayBudgets := SourceItem.PrintDelayBudgets;
  end;
  inherited;

end;

function TSubPrintItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SourceItem: TSubPrintItem;
begin
  if AnotherItem is TSubPrintItem then
  begin
    SourceItem := TSubPrintItem(AnotherItem);
    result := (StartTime = SourceItem.StartTime)
      and (EndTime = SourceItem.EndTime)
      and (PrintSubsidence = SourceItem.PrintSubsidence)
      and (SaveSubsidence = SourceItem.SaveSubsidence)
      and (PrintCompactionByModelLayer = SourceItem.PrintCompactionByModelLayer)
      and (SaveCompactionByModelLayer = SourceItem.SaveCompactionByModelLayer)
      and (PrintCompactionByInterbedSystem = SourceItem.PrintCompactionByInterbedSystem)
      and (SaveCompactionByInterbedSystem = SourceItem.SaveCompactionByInterbedSystem)
      and (PrintVerticalDisplacement = SourceItem.PrintVerticalDisplacement)
      and (SaveVerticalDisplacement = SourceItem.SaveVerticalDisplacement)
      and (PrintCriticalHeadNoDelay = SourceItem.PrintCriticalHeadNoDelay)
      and (SaveCriticalHeadNoDelay = SourceItem.SaveCriticalHeadNoDelay)
      and (PrintCriticalHeadDelay = SourceItem.PrintCriticalHeadDelay)
      and (SaveCriticalHeadDelay = SourceItem.SaveCriticalHeadDelay)
      and (PrintDelayBudgets = SourceItem.PrintDelayBudgets);
  end
  else
  begin
    result := False;
  end;
end;

procedure TSubPrintItem.SetEndTime(const Value: double);
begin
  SetRealProperty(FEndTime, Value);
end;

procedure TSubPrintItem.SetPrintCompactionByInterbedSystem(
  const Value: boolean);
begin
  SetBooleanProperty(FPrintCompactionByInterbedSystem, Value);
end;

procedure TSubPrintItem.SetPrintCompactionByModelLayer(const Value: boolean);
begin
  SetBooleanProperty(FPrintCompactionByModelLayer, Value);
end;

procedure TSubPrintItem.SetPrintCriticalHeadDelay(const Value: boolean);
begin
  SetBooleanProperty(FPrintCriticalHeadDelay, Value);
end;

procedure TSubPrintItem.SetPrintCriticalHeadNoDelay(const Value: boolean);
begin
  SetBooleanProperty(FPrintCriticalHeadNoDelay, Value);
end;

procedure TSubPrintItem.SetPrintDelayBudgets(const Value: boolean);
begin
  SetBooleanProperty(FPrintDelayBudgets, Value);
end;

procedure TSubPrintItem.SetPrintSubsidence(const Value: boolean);
begin
  SetBooleanProperty(FPrintSubsidence, Value);
end;

procedure TSubPrintItem.SetPrintVerticalDisplacement(const Value: boolean);
begin
  SetBooleanProperty(FPrintVerticalDisplacement, Value);
end;

procedure TSubPrintItem.SetSaveCompactionByInterbedSystem(const Value: boolean);
begin
  SetBooleanProperty(FSaveCompactionByInterbedSystem, Value);
end;

procedure TSubPrintItem.SetSaveCompactionByModelLayer(const Value: boolean);
begin
  SetBooleanProperty(FSaveCompactionByModelLayer, Value);
end;

procedure TSubPrintItem.SetSaveCriticalHeadDelay(const Value: boolean);
begin
  SetBooleanProperty(FSaveCriticalHeadDelay, Value);
end;

procedure TSubPrintItem.SetSaveCriticalHeadNoDelay(const Value: boolean);
begin
  SetBooleanProperty(FSaveCriticalHeadNoDelay, Value);
end;

procedure TSubPrintItem.SetSaveSubsidence(const Value: boolean);
begin
  SetBooleanProperty(FSaveSubsidence, Value);
end;

procedure TSubPrintItem.SetSaveVerticalDisplacement(const Value: boolean);
begin
  SetBooleanProperty(FSaveVerticalDisplacement, Value);
end;

procedure TSubPrintItem.SetStartTime(const Value: double);
begin
  SetRealProperty(FStartTime, Value);
end;

{ TSubPrintCollection }

constructor TSubPrintCollection.Create(Model: TObject);
begin
  inherited Create(TSubPrintItem, Model);
end;

function TSubPrintCollection.GetItem(Index: integer): TSubPrintItem;
begin
  result := inherited Items[Index] as TSubPrintItem;
end;

procedure TSubPrintCollection.ReportErrors;
const
  ErrorRoot = 'In the Subsidence package, one or more starting time '
    + 'is after the ending time';
var
  Index: Integer;
  PrintChoice: TSubPrintItem;
begin
  for Index := 0 to Count -1 do
  begin
    PrintChoice := Items[Index];
    if PrintChoice.StartTime > PrintChoice.EndTime then
    begin
      frmErrorsAndWarnings.AddError(ErrorRoot,
        'StartingTime: ' + FloatToStr(PrintChoice.StartTime)
        + '; EndingTime: ' + FloatToStr(PrintChoice.EndTime));
    end;
  end;
end;

procedure TSubPrintCollection.SetItem(Index: integer;
  const Value: TSubPrintItem);
begin
  inherited Items[Index] := Value;
end;

{ TSubPackageSelection }

procedure TSubPackageSelection.Assign(Source: TPersistent);
var
  SubSource: TSubPackageSelection;
begin
  if Source is TSubPackageSelection then
  begin
    SubSource := TSubPackageSelection(Source);
    PrintFormats := SubSource.PrintFormats;
    PrintChoices := SubSource.PrintChoices;
    NumberOfNodes := SubSource.NumberOfNodes;
    AccelerationParameter1 := SubSource.AccelerationParameter1;
    AccelerationParameter2 := SubSource.AccelerationParameter2;
    MinIterations := SubSource.MinIterations;
    SaveDelayRestart := SubSource.SaveDelayRestart;
    ReadDelayRestartFileName := SubSource.ReadDelayRestartFileName;
    BinaryOutputChoice := SubSource.BinaryOutputChoice;

  end;
  inherited;

end;

constructor TSubPackageSelection.Create(Model: TObject);
begin
  inherited;
  InitializeVariables;
  FPrintChoices := TSubPrintCollection.Create(Model);
  FPrintFormats := TSubPrintFormats.Create(Model);
end;

destructor TSubPackageSelection.Destroy;
begin
  FPrintFormats.Free;
  FPrintChoices.Free;
  inherited;
end;

procedure TSubPackageSelection.InitializeVariables;
begin
  FNumberOfNodes := 10;
  FAccelerationParameter1 := 0;
  FAccelerationParameter2 := 1;
  FMinIterations := 5;
  FSaveDelayRestart := False;
  FReadDelayRestartFileName := '';
  FSubBinaryOutputChoice := sbocSingleFile;
end;

procedure TSubPackageSelection.SetAccelerationParameter1(const Value: double);
begin
  if FAccelerationParameter1 <> Value then
  begin
    FAccelerationParameter1 := Value;
    InvalidateModel;
  end;
end;

procedure TSubPackageSelection.SetAccelerationParameter2(const Value: double);
begin
  if FAccelerationParameter2 <> Value then
  begin
    FAccelerationParameter2 := Value;
    InvalidateModel;
  end;
end;

procedure TSubPackageSelection.SetMinIterations(const Value: integer);
begin
  if FMinIterations <> Value then
  begin
    FMinIterations := Value;
    InvalidateModel;
  end;
end;

procedure TSubPackageSelection.SetNumberOfNodes(const Value: integer);
begin
  if FNumberOfNodes <> Value then
  begin
    FNumberOfNodes := Value;
    InvalidateModel;
  end;
end;

procedure TSubPackageSelection.SetPrintChoices(
  const Value: TSubPrintCollection);
begin
  FPrintChoices.Assign(Value);
end;

procedure TSubPackageSelection.SetPrintFormats(const Value: TSubPrintFormats);
begin
  FPrintFormats.Assign(Value);
end;

procedure TSubPackageSelection.SetReadDelayRestartFileName(const Value: string);
begin
  if FReadDelayRestartFileName <> Value then
  begin
    FReadDelayRestartFileName := Value;
    InvalidateModel;
  end;
end;

procedure TSubPackageSelection.SetSaveDelayRestart(const Value: boolean);
begin
  if FSaveDelayRestart <> Value then
  begin
    FSaveDelayRestart := Value;
    InvalidateModel;
  end;
end;

procedure TSubPackageSelection.SetSubBinaryOutputChoice(
  const Value: TSubBinaryOutputChoice);
begin
  if FSubBinaryOutputChoice <> Value then
  begin
    FSubBinaryOutputChoice := Value;
    InvalidateModel;
  end;
end;

{ TSubPrintFormats }

procedure TSubPrintFormats.Assign(Source: TPersistent);
var
  SubPrintSource: TSubPrintFormats;
begin
  if Source is TSubPrintFormats then
  begin
    SubPrintSource := TSubPrintFormats(Source);
    SubsidenceFormat := SubPrintSource.SubsidenceFormat;
    CompactionByModelLayerFormat := SubPrintSource.CompactionByModelLayerFormat;
    CompactionByInterbedSystemFormat :=
      SubPrintSource.CompactionByInterbedSystemFormat;
    VerticalDisplacementFormat := SubPrintSource.VerticalDisplacementFormat;
    NoDelayPreconsolidationHeadFormat :=
      SubPrintSource.NoDelayPreconsolidationHeadFormat;
    DelayPreconsolidationHeadFormat :=
      SubPrintSource.DelayPreconsolidationHeadFormat;
  end
  else
  begin
    inherited;
  end;
end;

procedure TSubPrintFormats.SetCompactionByInterbedSystemFormat(
  const Value: integer);
begin
  SetIntegerProperty(FCompactionByInterbedSystemFormat, Value);
end;

procedure TSubPrintFormats.SetCompactionByModelLayerFormat(
  const Value: integer);
begin
  SetIntegerProperty(FCompactionByModelLayerFormat, Value);
end;

procedure TSubPrintFormats.SetDelayPreconsolidationHeadFormat(
  const Value: integer);
begin
  SetIntegerProperty(FDelayPreconsolidationHeadFormat, Value);
end;

procedure TSubPrintFormats.SetNoDelayPreconsolidationHeadFormat(
  const Value: integer);
begin
  SetIntegerProperty(FNoDelayPreconsolidationHeadFormat, Value);
end;

procedure TSubPrintFormats.SetSubsidenceFormat(const Value: integer);
begin
  SetIntegerProperty(FSubsidenceFormat, Value);
end;

procedure TSubPrintFormats.SetVerticalDisplacementFormat(const Value: integer);
begin
  SetIntegerProperty(FVerticalDisplacementFormat, Value);
end;

{ ZZoneItem }

procedure ZZoneItem.Assign(Source: TPersistent);
begin
  if Source is ZZoneItem then
  begin
    ZoneNumber := ZZoneItem(Source).ZoneNumber;
  end;
  inherited;
end;

function ZZoneItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := AnotherItem is ZZoneItem;
  if result then
  begin
    result := ZoneNumber = ZZoneItem(AnotherItem).ZoneNumber;
  end;
end;

procedure ZZoneItem.SetZoneNumber(const Value: integer);
begin
  if FZoneNumber <> Value then
  begin
    FZoneNumber := Value;
    InvalidateModel;
  end;
end;

{ TCompositeZone }

procedure TCompositeZone.Assign(Source: TPersistent);
begin
  if Source is TCompositeZone then
  begin
    ZoneName := TCompositeZone(Source).ZoneName;
  end;
  inherited;
end;

constructor TCompositeZone.Create(Model: TObject);
begin
  inherited Create(ZZoneItem, Model);
end;

function TCompositeZone.GetItem(Index: integer): ZZoneItem;
begin
  result := inherited Items[Index] as ZZoneItem;
end;

function TCompositeZone.IsSame(
  AnOrderedCollection: TOrderedCollection): boolean;
begin
  result := AnOrderedCollection is TCompositeZone;
  if result then
  begin
    result := ZoneName = TCompositeZone(AnOrderedCollection).ZoneName;
  end;
  if result then
  begin
    result := inherited IsSame(AnOrderedCollection);
  end;
end;

procedure TCompositeZone.SetItem(Index: integer; const Value: ZZoneItem);
begin
  inherited Items[Index] := Value;
end;

procedure TCompositeZone.SetZoneName(Value: string);
begin
  Value := ValidName(Value);
  if FZoneName <> Value then
  begin
    FZoneName := Value;
    InvalidateModel;
  end;
end;

{ TCompositeZoneItem }

procedure TCompositeZoneItem.Assign(Source: TPersistent);
begin
  if Source is TCompositeZoneItem then
  begin
    CompositeZone := TCompositeZoneItem(Source).CompositeZone;
  end;
  inherited;
end;

constructor TCompositeZoneItem.Create(Collection: TCollection);
begin
  inherited;
  FCompositeZone:= TCompositeZone.Create(Model);
end;

destructor TCompositeZoneItem.Destroy;
begin
  FCompositeZone.Free;
  inherited;
end;

function TCompositeZoneItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := AnotherItem is TCompositeZoneItem;
  if result then
  begin
    result := TCompositeZoneItem(AnotherItem).CompositeZone.
      IsSame(CompositeZone);
  end;
end;

procedure TCompositeZoneItem.SetCompositeZone(const Value: TCompositeZone);
begin
  FCompositeZone.Assign(Value);
end;

{ TCompositeZoneCollection }

constructor TCompositeZoneCollection.Create(Model: TObject);
begin
  inherited Create(TCompositeZoneItem, Model);
end;

function TCompositeZoneCollection.GetItem(Index: integer): TCompositeZoneItem;
begin
  result := inherited Items[Index] as TCompositeZoneItem;
end;

procedure TCompositeZoneCollection.SetItem(Index: integer;
  const Value: TCompositeZoneItem);
begin
  inherited Items[Index] := Value;
end;

{ TZoneBudgetSelect }

procedure TZoneBudgetSelect.Assign(Source: TPersistent);
var
  SourceBudget: TZoneBudgetSelect;
begin
  if Source is TZoneBudgetSelect then
  begin
    SourceBudget := TZoneBudgetSelect(Source);
    CompositeZones := SourceBudget.CompositeZones;
    ExportZBLST := SourceBudget.ExportZBLST;
    ExportCSV := SourceBudget.ExportCSV;
    ExportCSV2 := SourceBudget.ExportCSV2;
  end;
  inherited;
end;

constructor TZoneBudgetSelect.Create(Model: TObject);
begin
  inherited;
  FCompositeZones := TCompositeZoneCollection.Create(Model);
  InitializeVariables;
end;

destructor TZoneBudgetSelect.Destroy;
begin
  FCompositeZones.Free;
  inherited;
end;

procedure TZoneBudgetSelect.InitializeVariables;
begin
  FExportZBLST := True;
  FExportCSV2 := True;
  FExportCSV := True;
  FCompositeZones.Clear
end;

procedure TZoneBudgetSelect.SetCompositeZones(
  const Value: TCompositeZoneCollection);
begin
  FCompositeZones.Assign(Value);
end;

procedure TZoneBudgetSelect.SetExportCSV(const Value: boolean);
begin
  if FExportCSV <> Value then
  begin
    FExportCSV := Value;
    InvalidateModel;
  end;
end;

procedure TZoneBudgetSelect.SetExportCSV2(const Value: boolean);
begin
  if FExportCSV2 <> Value then
  begin
    FExportCSV2 := Value;
    InvalidateModel;
  end;
end;

procedure TZoneBudgetSelect.SetExportZBLST(const Value: boolean);
begin
  if FExportZBLST <> Value then
  begin
    FExportZBLST := Value;
    InvalidateModel;
  end;
end;

end.
