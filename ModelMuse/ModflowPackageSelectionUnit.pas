unit ModflowPackageSelectionUnit;

interface

uses SysUtils, Classes, GoPhastTypes, OrderedCollectionUnit, DataSetUnit,
  ModpathParticleUnit, ModflowBoundaryDisplayUnit, ScreenObjectUnit,
  ModflowBoundaryUnit, Mt3dmsChemSpeciesUnit;

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
    FModel: TBaseModel;
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
    // @name is used when determining what data sets or global variables are
    // used when evaluating the formula for a MODFLOW boundary condition.
    procedure UpdateDisplayUseList(NewUseList: TStringList;
      ParamType: TParameterType; DataIndex: integer; const DisplayName: string);
    procedure UpdateUseList(DataIndex: integer; NewUseList: TStringList;
      Item: TCustomModflowBoundaryItem);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel);
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
    procedure InitializeVariables; virtual;
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
    Constructor Create(Model: TBaseModel);
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
    Constructor Create(Model: TBaseModel);
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
    Constructor Create(Model: TBaseModel);
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
    Constructor Create(Model: TBaseModel);
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
    Constructor Create(Model: TBaseModel);
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
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
    property MfChdStartingHead: TModflowBoundaryDisplayTimeList
      read FMfChdStartingHead;
    property MfChdEndingHead: TModflowBoundaryDisplayTimeList
      read FMfChdEndingHead;
    procedure InvalidateAllTimeLists; override;
  end;

  TCustomTransientArrayItem = class(TCollectionItem)
  private
    FFileName: string;
    FUniform: boolean;
    FArrayName: string;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property ArrayName: string read FArrayName write FArrayName;
    property FileName: string read FFileName write FFileName;
    property Uniform: boolean read FUniform write FUniform;
  end;

  TTransientMultItem = class(TCustomTransientArrayItem)
  private
    FUniformValue: double;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property UniformValue: double read FUniformValue write FUniformValue;
  end;

  TTransientZoneItem = class(TCustomTransientArrayItem)
  private
    FUniformValue: integer;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property UniformValue: integer read FUniformValue write FUniformValue;
  end;

  TTransientMultCollection = class(TCollection)
  private
    function GetItem(Index: integer): TTransientMultItem;
    procedure SetItem(Index: integer; const Value: TTransientMultItem);
  public
    Constructor Create;
    function Add: TTransientMultItem;
    property Items[Index: integer]: TTransientMultItem
      read GetItem write SetItem; default;
  end;

  TTransientZoneCollection = class(TCollection)
  private
    function GetItem(Index: integer): TTransientZoneItem;
    procedure SetItem(Index: integer; const Value: TTransientZoneItem);
  public
    Constructor Create;
    function Add: TTransientZoneItem;
    property Items[Index: integer]: TTransientZoneItem
      read GetItem write SetItem; default;
  end;

  TLpfSelection = class(TModflowPackageSelection)
  private
    FUseSaturatedThickness: boolean;
    FUseConstantCV: boolean;
    FUseCvCorrection: boolean;
    FUseVerticalFlowCorrection: boolean;
    FUseStorageCoefficient: boolean;
    FMultZoneArraysExported: boolean;
    procedure SetUseConstantCV(const Value: boolean);
    procedure SetUseCvCorrection(const Value: boolean);
    procedure SetUseSaturatedThickness(const Value: boolean);
    procedure SetUseVerticalFlowCorrection(const Value: boolean);
    procedure SetUseStorageCoefficient(const Value: boolean);
  public
    procedure InitializeVariables; override;
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    property MultZoneArraysExported: boolean read FMultZoneArraysExported
      write FMultZoneArraysExported;
  published
    property UseConstantCV: boolean read FUseConstantCV write SetUseConstantCV;
    property UseSaturatedThickness: boolean read FUseSaturatedThickness
      write SetUseSaturatedThickness;
    property UseCvCorrection: boolean read FUseCvCorrection
      write SetUseCvCorrection default True;
    property UseVerticalFlowCorrection: boolean read FUseVerticalFlowCorrection
      write SetUseVerticalFlowCorrection default True;
    property UseStorageCoefficient: boolean read FUseStorageCoefficient
      write SetUseStorageCoefficient;
  end;

  THDryPrintOption = (hpoDontPrintHdry, hpoPrintHdry);

  TUpwPackageSelection = class(TModflowPackageSelection)
  private
    FHDryPrintOption: THDryPrintOption;
    procedure SetHDryPrintOption(const Value: THDryPrintOption);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    procedure InitializeVariables; override;
  published
    property HDryPrintOption: THDryPrintOption read FHDryPrintOption
      write SetHDryPrintOption stored True;
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
    Constructor Create(Model: TBaseModel);
    procedure InitializeVariables; override;
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
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
    procedure InitializeVariables; override;
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
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
    procedure InitializeVariables; override;
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
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
    procedure InitializeVariables; override;
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
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
    procedure InitializeVariables; override;
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

  TNewtonSolverMethod = (nsmGmres, nsmChiMD);
  TNewtonOption = (noSimple, noModerate, noComplex, noSpecified);
  TNewtonIluMethod = (nimDropTol, nimKOrder);
  TNewtonAccelMethod = (namCongGrad, namOthoMin, namBiCgstab);
  TNewtonOrderingMethod = (nomRCM, nomMinimumOrdering);
  TNewtonApplyReducedPrecondition = (narpDontApply, narpApply);
  TNewtonUseDropTolerance = (nudtDontUse, nudtUse);

  TNwtPackageSelection = class(TModflowPackageSelection)
  private
    FMaxGmresRestarts: integer;
    FStopTolerance: TRealStorage;
    FMomementumCoefficient: TRealStorage;
    FLevel: integer;
    FOrderingMethod: TNewtonOrderingMethod;
    FDropTolerancePreconditioning: TRealStorage;
    FCorrectForCellBottom: integer;
    FFillLimit: integer;
    FOption: TNewtonOption;
    FSolverMethod: TNewtonSolverMethod;
    FMaxBackIterations: integer;
    FDBDKappa: TRealStorage;
    FApplyReducedPrecondition: TNewtonApplyReducedPrecondition;
    FIluMethod: TNewtonIluMethod;
    FInnerHeadClosureCriterion: TRealStorage;
    FDBDGamma: TRealStorage;
    FFluxTolerance: TRealStorage;
    FResidReducConv: TRealStorage;
    FAccelMethod: TNewtonAccelMethod;
    FMaxOuterIterations: integer;
    FMaxIterInner: integer;
    FUseDropTolerance: TNewtonUseDropTolerance;
    FDBDTheta: TRealStorage;
    FHeadTolerance: TRealStorage;
    FFillLevel: integer;
    FPrintFlag: integer;
    FMaxInnerIterations: integer;
    FNumberOfOrthogonalizations: integer;
    FThicknessFactor: TRealStorage;
    FBackReduce: TRealStorage;
    FBackTol: TRealStorage;
    FBackFlag: integer;
    FContinueNWT: Boolean;
    procedure SetRealProperty(Field, NewValue: TRealStorage);
    procedure SetIntegerProperty(var Field: integer; NewValue: integer);
    procedure SetBooleanProperty(var Field: Boolean; NewValue: Boolean);
    procedure SetAccelMethod(const Value: TNewtonAccelMethod);
    procedure SetApplyReducedPrecondition(
      const Value: TNewtonApplyReducedPrecondition);
    procedure SetBackFlag(const Value: integer);
    procedure SetBackReduce(const Value: TRealStorage);
    procedure SetBackTol(const Value: TRealStorage);
    procedure SetCorrectForCellBottom(const Value: integer);
    procedure SetDBDGamma(const Value: TRealStorage);
    procedure SetDBDKappa(const Value: TRealStorage);
    procedure SetDBDTheta(const Value: TRealStorage);
    procedure SetDropTolerancePreconditioning(const Value: TRealStorage);
    procedure SetFillLevel(const Value: integer);
    procedure SetFillLimit(const Value: integer);
    procedure SetFluxTolerance(const Value: TRealStorage);
    procedure SetHeadTolerance(const Value: TRealStorage);
    procedure SetIluMethod(const Value: TNewtonIluMethod);
    procedure SetInnerHeadClosureCriterion(const Value: TRealStorage);
    procedure SetLevel(const Value: integer);
    procedure SetMaxBackIterations(const Value: integer);
    procedure SetMaxGmresRestarts(const Value: integer);
    procedure SetMaxInnerIterations(const Value: integer);
    procedure SetMaxIterInner(const Value: integer);
    procedure SetMaxOuterIterations(const Value: integer);
    procedure SetMomementumCoefficient(const Value: TRealStorage);
    procedure SetNumberOfOrthogonalizations(const Value: integer);
    procedure SetOption(const Value: TNewtonOption);
    procedure SetOrderingMethod(const Value: TNewtonOrderingMethod);
    procedure SetPrintFlag(const Value: integer);
    procedure SetResidReducConv(const Value: TRealStorage);
    procedure SetSolverMethod(const Value: TNewtonSolverMethod);
    procedure SetStopTolerance(const Value: TRealStorage);
    procedure SetThicknessFactor(const Value: TRealStorage);
    procedure SetUseDropTolerance(const Value: TNewtonUseDropTolerance);
    procedure SetContinueNWT(const Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
    procedure InitializeVariables; override;
  published
    // @name is HEADTOL.
    property HeadTolerance: TRealStorage read FHeadTolerance write SetHeadTolerance;
    // @name is FLUXTOL.
    property FluxTolerance: TRealStorage read FFluxTolerance write SetFluxTolerance;
    // @name is MAXITEROUT.
    property MaxOuterIterations: integer read FMaxOuterIterations write SetMaxOuterIterations stored True;
    // @name is THICKFACT.
    // range = 0 to 1;
    property ThicknessFactor: TRealStorage read FThicknessFactor write SetThicknessFactor;
    // @name is LINMETH.
    property SolverMethod: TNewtonSolverMethod read FSolverMethod write SetSolverMethod stored True;
    // @name is IPRNWT.
    property PrintFlag: integer read FPrintFlag write SetPrintFlag stored True;
    // @name is IBOTAV.
    // Zero or one.
    Property CorrectForCellBottom: integer read FCorrectForCellBottom write SetCorrectForCellBottom stored True;
    // @name is OPTIONS.
    property Option: TNewtonOption read FOption write SetOption stored True;
    property DBDTheta: TRealStorage read FDBDTheta write SetDBDTheta;
    property DBDKappa: TRealStorage read FDBDKappa write SetDBDKappa;
    property DBDGamma: TRealStorage read FDBDGamma write SetDBDGamma;
    // @name is MOMFACT.
    property MomementumCoefficient: TRealStorage read FMomementumCoefficient write SetMomementumCoefficient;
    // Zero or one.
    property BackFlag: integer read FBackFlag write SetBackFlag stored True;
    // @name is MAXBACKITER.
    property MaxBackIterations: integer read FMaxBackIterations write SetMaxBackIterations stored True;
    // @name is BACKTOL.
    // range = 1 to 2;
    property BackTol: TRealStorage read FBackTol write SetBackTol;
    // @name is BACKREDUCE.
    property BackReduce: TRealStorage read FBackReduce write SetBackReduce;
    // @name is MAXITINNER.
    property MaxIterInner: integer read FMaxIterInner write SetMaxIterInner stored True;
    // @name is ILUMETHOD.
    property IluMethod: TNewtonIluMethod read FIluMethod
      write SetIluMethod stored True;
    // @name is LEVFILL is the fill limit for ILUMETHOD = 1
    property FillLimit: integer read FFillLimit write SetFillLimit stored True;
    // @name is LEVFILL is the level of fill for ILUMETHOD = 2
    property FillLevel: integer read FFillLevel write SetFillLevel stored True;
    // @name is STOPTOL.
    property StopTolerance: TRealStorage read FStopTolerance
      write SetStopTolerance;
    // @name is MSDR.
    property MaxGmresRestarts: integer read FMaxGmresRestarts
      write SetMaxGmresRestarts stored True;
    // @name is IACL.
    property AccelMethod: TNewtonAccelMethod read FAccelMethod
      write SetAccelMethod stored True;
    // @name is NORDER.
    property OrderingMethod: TNewtonOrderingMethod read FOrderingMethod
      write SetOrderingMethod stored True;
    // @name is LEVEL.
    property Level: integer read FLevel write SetLevel stored True;
    // @name is NORTH.
    property NumberOfOrthogonalizations: integer
      read FNumberOfOrthogonalizations write SetNumberOfOrthogonalizations
      stored True;
    // @name is IREDSYS
    property ApplyReducedPrecondition: TNewtonApplyReducedPrecondition
      read FApplyReducedPrecondition write SetApplyReducedPrecondition
      stored True;
    // @name is RRCTOLS.
    property ResidReducConv: TRealStorage read FResidReducConv
      write SetResidReducConv;
    // @name is IDROPTOL.
    // IDROPTOL <> 0 means to use the drop tolerance.
    property UseDropTolerance: TNewtonUseDropTolerance read FUseDropTolerance
      write SetUseDropTolerance stored True;
    // @name is EPSRN.
    property DropTolerancePreconditioning: TRealStorage
      read FDropTolerancePreconditioning write SetDropTolerancePreconditioning;
    // @name is HCLOSEXMD.
    property InnerHeadClosureCriterion: TRealStorage
      read FInnerHeadClosureCriterion write SetInnerHeadClosureCriterion;
    // @name is MXITERXMD.
    property MaxInnerIterations: integer read FMaxInnerIterations
      write SetMaxInnerIterations stored True;
    // @name is the GOFAIL OPTION in NWT now renamed CONTINUE.
    property GoFail: Boolean read FContinueNWT write SetContinueNWT stored False;
    // @name is the CONTINUE OPTION in NWT.
    property ContinueNWT: Boolean read FContinueNWT write SetContinueNWT;
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
    Constructor Create(Model: TBaseModel);
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
    FMultiplierArrayNames: TTransientMultCollection;
    FZoneArrayNames: TTransientZoneCollection;
    procedure UpdateWithElevationFormula(Formula: string;
      ScreenObject: TScreenObject; NewUseList: TStringList);
    procedure SetMultiplierArrayNames(const Value: TTransientMultCollection);
    procedure SetZoneArrayNames(const Value: TTransientZoneCollection);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel);
    destructor Destroy; override;
  published
    property TimeVaryingLayers: boolean read GetTimeVaryingLayers
      write SetTimeVaryingLayers;
    property MultiplierArrayNames: TTransientMultCollection read FMultiplierArrayNames
      write SetMultiplierArrayNames;
    property ZoneArrayNames: TTransientZoneCollection read FZoneArrayNames
      write SetZoneArrayNames;
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
    Constructor Create(Model: TBaseModel);
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
    FAssignmentMethod: TUpdateMethod;
    procedure GetMfRchRateUseList(Sender: TObject; NewUseList: TStringList);
    procedure GetMfRchLayerUseList(Sender: TObject; NewUseList: TStringList);
    procedure InitializeRchDisplay(Sender: TObject);
    procedure SetAssignmentMethod(const Value: TUpdateMethod);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
    property MfRchRate: TModflowBoundaryDisplayTimeList read FMfRchRate;
    property MfRchLayer: TModflowBoundaryDisplayTimeList read FMfRchLayer;
    procedure InvalidateAllTimeLists; override;
    procedure InvalidateMfRchLayer(Sender: TObject);
  published
    property AssignmentMethod: TUpdateMethod read FAssignmentMethod
      write SetAssignmentMethod Stored True;
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
    Constructor Create(Model: TBaseModel);
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
    Constructor Create(Model: TBaseModel);
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
    Constructor Create(Model: TBaseModel);
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
    constructor Create(Model: TBaseModel);
    property Items[Index: integer]: TSfrParamInstance read GetItems write SetItems;
    function ParameterInstanceExists(const ParamName, InstaName: string): boolean;
    procedure DeleteInstancesOfParameter(const ParamName: string);
    procedure UpdateParamName(const OldParamName, NewParamName: string);
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
    FUseGsflowFormat: boolean;
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
    procedure SetUseGsflowFormat(const Value: boolean);
  protected
    procedure SetIsSelected(const Value: boolean); override;
  public
    procedure InitializeVariables; override;
    procedure ComputeAverages(List: TModflowBoundListOfTimeLists);
    procedure GetDisplayLists(List: TModflowBoundListOfTimeLists);
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel);
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
    property UseGsflowFormat: boolean read FUseGsflowFormat write SetUseGsflowFormat;
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
    FAssignmentMethod: TUpdateMethod;
    FSpecifyResidualWaterContent: boolean;
    FCalulateSurfaceLeakage: boolean;
    FSpecifyInitialWaterContent: boolean;
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
    procedure SetAssignmentMethod(const Value: TUpdateMethod);
    procedure SetCalulateSurfaceLeakage(const Value: boolean);
    procedure SetSpecifyInitialWaterContent(const Value: boolean);
    procedure SetSpecifyResidualWaterContent(const Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel);
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
    procedure InitializeVariables; override;
  published
    property VerticalKSource: integer read FVerticalKSource write SetVerticalKSource;
    property RouteDischargeToStreams: boolean read FRouteDischargeToStreams write SetRouteDischargeToStreams;
    property SimulateET: boolean read FSimulateET write SetSimulateET;
    property NumberOfTrailingWaves: integer read FNumberOfTrailingWaves write SetNumberOfTrailingWaves;
    property NumberOfWaveSets: integer read FNumberOfWaveSets write SetNumberOfWaveSets;
    // IFTUNIT
    property PrintSummary: integer read FPrintSummary write SetPrintSummary;
    property DepthOfUndulations: double read FDepthOfUndulations write SetDepthOfUndulations;
    property AssignmentMethod: TUpdateMethod read FAssignmentMethod
      write SetAssignmentMethod stored True;
    // SPECIFYTHTR
    property SpecifyResidualWaterContent: boolean
      read FSpecifyResidualWaterContent write SetSpecifyResidualWaterContent;
    // SPECIFYTHTI
    property SpecifyInitialWaterContent: boolean
      read FSpecifyInitialWaterContent write SetSpecifyInitialWaterContent;
    // inverse of NOSURFLEAK
    property CalulateSurfaceLeakage: boolean read FCalulateSurfaceLeakage
      write SetCalulateSurfaceLeakage default True;
  end;

  THobPackageSelection = class(TModflowPackageSelection)
  private
    FDryHead: double;
    procedure SetDryHead(const Value: double);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel);
    procedure InitializeVariables; override;
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
  protected
    procedure SetIsSelected(const Value: boolean); override;
  public
    Constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure InitializeVariables; override;
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
    constructor Create(Model: TBaseModel);
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
    constructor Create(Model: TBaseModel);
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
    procedure InitializeVariables; override;
    Constructor Create(Model: TBaseModel);
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
    Constructor Create(Model: TBaseModel);
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

  TCustomPrintItem = class(TOrderedItem)
  private
    FStartTime: double;
    FEndTime: double;
    procedure SetStartTime(const Value: double);
    procedure SetEndTime(const Value: double);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property StartTime: double read FStartTime write SetStartTime;
    property EndTime: double read FEndTime write SetEndTime;
  end;

  TSubPrintItem = class(TCustomPrintItem)
  private
    FPrintVerticalDisplacement: boolean;
    FSaveCompactionByInterbedSystem: boolean;
    FSaveCriticalHeadDelay: boolean;
    FPrintCompactionByInterbedSystem: boolean;
    FPrintCriticalHeadDelay: boolean;
    FSaveCriticalHeadNoDelay: boolean;
    FPrintDelayBudgets: boolean;
    FPrintCriticalHeadNoDelay: boolean;
    FSaveSubsidence: boolean;
    FSaveCompactionByModelLayer: boolean;
    FPrintSubsidence: boolean;
    FPrintCompactionByModelLayer: boolean;
    FSaveVerticalDisplacement: boolean;
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
    procedure SetSaveVerticalDisplacement(const Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  published
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
    constructor Create(Model: TBaseModel);
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
    procedure InitializeVariables;
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
    procedure InitializeVariables; override;
    Constructor Create(Model: TBaseModel);
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

  TSwtPrintItem = class(TCustomPrintItem)
  private
    FSaveDeltaGeostaticStress: boolean;
    FPrintGeostaticStress: boolean;
    FPrintVerticalDisplacement: boolean;
    FSavePreconsolidationStress: boolean;
    FSaveCompactionByInterbedSystem: boolean;
    FPrintVoidRatio: boolean;
    FPrintDeltaEffectiveStress: boolean;
    FPrintDeltaGeostaticStress: boolean;
    FSaveDeltaPreconsolidationStress: boolean;
    FPrintPreconsolidationStress: boolean;
    FPrintCompactionByInterbedSystem: boolean;
    FPrintDeltaPreconsolidationStress: boolean;
    FSaveThicknessCompressibleSediments: boolean;
    FSaveSubsidence: boolean;
    FSaveEffectiveStress: boolean;
    FSaveCompactionByModelLayer: boolean;
    FSaveLayerCenterElevation: boolean;
    FPrintThicknessCompressibleSediments: boolean;
    FSaveGeostaticStress: boolean;
    FSaveVerticalDisplacement: boolean;
    FSaveVoidRatio: boolean;
    FPrintSubsidence: boolean;
    FSaveDeltaEffectiveStress: boolean;
    FPrintEffectiveStress: boolean;
    FPrintCompactionByModelLayer: boolean;
    FPrintLayerCenterElevation: boolean;
    procedure SetPrintCompactionByInterbedSystem(const Value: boolean);
    procedure SetPrintCompactionByModelLayer(const Value: boolean);
    procedure SetPrintDeltaEffectiveStress(const Value: boolean);
    procedure SetPrintDeltaGeostaticStress(const Value: boolean);
    procedure SetPrintDeltaPreconsolidationStress(const Value: boolean);
    procedure SetPrintEffectiveStress(const Value: boolean);
    procedure SetPrintGeostaticStress(const Value: boolean);
    procedure SetPrintLayerCenterElevation(const Value: boolean);
    procedure SetPrintPreconsolidationStress(const Value: boolean);
    procedure SetPrintSubsidence(const Value: boolean);
    procedure SetPrintThicknessCompressibleSediments(const Value: boolean);
    procedure SetPrintVerticalDisplacement(const Value: boolean);
    procedure SetPrintVoidRatio(const Value: boolean);
    procedure SetSaveCompactionByInterbedSystem(const Value: boolean);
    procedure SetSaveCompactionByModelLayer(const Value: boolean);
    procedure SetSaveDeltaEffectiveStress(const Value: boolean);
    procedure SetSaveDeltaGeostaticStress(const Value: boolean);
    procedure SetSaveDeltaPreconsolidationStress(const Value: boolean);
    procedure SetSaveEffectiveStress(const Value: boolean);
    procedure SetSaveGeostaticStress(const Value: boolean);
    procedure SetSaveLayerCenterElevation(const Value: boolean);
    procedure SetSavePreconsolidationStress(const Value: boolean);
    procedure SetSaveSubsidence(const Value: boolean);
    procedure SetSaveThicknessCompressibleSediments(const Value: boolean);
    procedure SetSaveVerticalDisplacement(const Value: boolean);
    procedure SetSaveVoidRatio(const Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  published
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
    property PrintPreconsolidationStress: boolean
      read FPrintPreconsolidationStress write SetPrintPreconsolidationStress;
    // Ifl10
    property SavePreconsolidationStress: boolean
      read FSavePreconsolidationStress write SetSavePreconsolidationStress;
    // Ifl11
    property PrintDeltaPreconsolidationStress: boolean
      read FPrintDeltaPreconsolidationStress
      write SetPrintDeltaPreconsolidationStress;
    // Ifl12
    property SaveDeltaPreconsolidationStress: boolean
      read FSaveDeltaPreconsolidationStress
      write SetSaveDeltaPreconsolidationStress;
    // Ifl13
    property PrintGeostaticStress: boolean read FPrintGeostaticStress
      write SetPrintGeostaticStress;
    // Ifl14
    property SaveGeostaticStress: boolean read FSaveGeostaticStress
      write SetSaveGeostaticStress;
    // Ifl15
    property PrintDeltaGeostaticStress: boolean read FPrintDeltaGeostaticStress
      write SetPrintDeltaGeostaticStress;
    // Ifl16
    property SaveDeltaGeostaticStress: boolean read FSaveDeltaGeostaticStress
      write SetSaveDeltaGeostaticStress;
    // Ifl17
    property PrintEffectiveStress: boolean read FPrintEffectiveStress
      write SetPrintEffectiveStress;
    // Ifl18
    property SaveEffectiveStress: boolean read FSaveEffectiveStress
      write SetSaveEffectiveStress;
    // Ifl19
    property PrintDeltaEffectiveStress: boolean read FPrintDeltaEffectiveStress
      write SetPrintDeltaEffectiveStress;
    // Ifl20
    property SaveDeltaEffectiveStress: boolean read FSaveDeltaEffectiveStress
      write SetSaveDeltaEffectiveStress;
    // Ifl21
    property PrintVoidRatio: boolean read FPrintVoidRatio
      write SetPrintVoidRatio;
    // Ifl22
    property SaveVoidRatio: boolean read FSaveVoidRatio write SetSaveVoidRatio;
    // Ifl23
    property PrintThicknessCompressibleSediments: boolean
      read FPrintThicknessCompressibleSediments
      write SetPrintThicknessCompressibleSediments;
    // Ifl24
    property SaveThicknessCompressibleSediments: boolean
      read FSaveThicknessCompressibleSediments
      write SetSaveThicknessCompressibleSediments;
    // Ifl25
    property PrintLayerCenterElevation: boolean read FPrintLayerCenterElevation
      write SetPrintLayerCenterElevation;
    // Ifl26
    property SaveLayerCenterElevation: boolean read FSaveLayerCenterElevation
      write SetSaveLayerCenterElevation;
  end;

  TSwtPrintCollection = class(TOrderedCollection)
  private
    function GetItem(Index: integer): TSwtPrintItem;
    procedure SetItem(Index: integer; const Value: TSwtPrintItem);
  public
    constructor Create(Model: TBaseModel);
    property Items[Index: integer]: TSwtPrintItem read GetItem
      write SetItem; default;
    procedure ReportErrors;
  end;

  TSwtInitialPrint = class(TGoPhastPersistent)
  private
    FPrintInitialLayerCenterElevations: boolean;
    FPrintInitialEquivalentStorageProperties: boolean;
    FInitialEquivalentStoragePropertiesFormat: integer;
    FPrintInitialGeostaticStress: boolean;
    FInitialLayerCenterElevationFormat: integer;
    FInitialGeostaticStressFormat: integer;
    FPrintInitialPreconsolidationStress: boolean;
    FInitialPreconsolidationStressFormat: integer;
    FPrintInitialEffectiveStress: boolean;
    FInitialEffectiveStressFormat: integer;
    procedure SetInitialEquivalentStoragePropertiesFormat(const Value: integer);
    procedure SetInitialGeostaticStressFormat(const Value: integer);
    procedure SetInitialLayerCenterElevationFormat(const Value: integer);
    procedure SetInitialPreconsolidationStressFormat(const Value: integer);
    procedure SetPrintInitialEquivalentStorageProperties(const Value: boolean);
    procedure SetPrintInitialGeostaticStress(const Value: boolean);
    procedure SetPrintInitialLayerCenterElevations(const Value: boolean);
    procedure SetPrintInitialPreconsolidationStress(const Value: boolean);
    procedure SetInitialEffectiveStressFormat(const Value: integer);
    procedure SetPrintInitialEffectiveStress(const Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
    procedure InitializeVariables;
  published
    // IZCFL
    property PrintInitialLayerCenterElevations: boolean
      read FPrintInitialLayerCenterElevations
      write SetPrintInitialLayerCenterElevations;
    // IZCFM
    property InitialLayerCenterElevationFormat: integer
      read FInitialLayerCenterElevationFormat
      write SetInitialLayerCenterElevationFormat;
    // IGLFL
    property PrintInitialGeostaticStress: boolean
      read FPrintInitialGeostaticStress write SetPrintInitialGeostaticStress;
    // IGLFM
    property InitialGeostaticStressFormat: integer
      read FInitialGeostaticStressFormat write SetInitialGeostaticStressFormat;
    // IESTFL
    property PrintInitialEffectiveStress: boolean
      read FPrintInitialEffectiveStress write SetPrintInitialEffectiveStress;
    // IESTFM
    property InitialEffectiveStressFormat: integer
      read FInitialEffectiveStressFormat write SetInitialEffectiveStressFormat;
    // IPCSFL
    property PrintInitialPreconsolidationStress: boolean
      read FPrintInitialPreconsolidationStress
      write SetPrintInitialPreconsolidationStress;
    // IPCSFM
    property InitialPreconsolidationStressFormat: integer
      read FInitialPreconsolidationStressFormat
      write SetInitialPreconsolidationStressFormat;
    // ISTFL
    property PrintInitialEquivalentStorageProperties: boolean
      read FPrintInitialEquivalentStorageProperties
      write SetPrintInitialEquivalentStorageProperties;
    // ISTFM
    property InitialEquivalentStoragePropertiesFormat: integer
      read FInitialEquivalentStoragePropertiesFormat
      write SetInitialEquivalentStoragePropertiesFormat;
  end;

  TSwtPrintFormats = class(TGoPhastPersistent)
  private
    FVerticalDisplacementFormat: integer;
    FCompactionByInterbedSystemFormat: integer;
    FSubsidenceFormat: integer;
    FCompactionByModelLayerFormat: integer;
    FThicknessCompressibleSediments: integer;
    FEffectiveStress: integer;
    FLayerCenterElevation: integer;
    FGeostaticStress: integer;
    FVoidRatio: integer;
    FDeltaEffectiveStress: integer;
    FDeltaGeostaticStress: integer;
    FPreconsolidationStress: integer;
    FDeltaPreconsolidationStress: integer;
    procedure SetCompactionByInterbedSystemFormat(const Value: integer);
    procedure SetCompactionByModelLayerFormat(const Value: integer);
    procedure SetSubsidenceFormat(const Value: integer);
    procedure SetVerticalDisplacementFormat(const Value: integer);
    procedure SetDeltaEffectiveStress(const Value: integer);
    procedure SetDeltaGeostaticStress(const Value: integer);
    procedure SetDeltaPreconsolidationStress(const Value: integer);
    procedure SetEffectiveStress(const Value: integer);
    procedure SetGeostaticStress(const Value: integer);
    procedure SetLayerCenterElevation(const Value: integer);
    procedure SetPreconsolidationStress(const Value: integer);
    procedure SetThicknessCompressibleSediments(const Value: integer);
    procedure SetVoidRatio(const Value: integer);
  public
    procedure Assign(Source: TPersistent); override;
    procedure InitializeVariables;
  published
    // Ifm1
    property SubsidenceFormat : integer read FSubsidenceFormat
      write SetSubsidenceFormat;
    // Ifm2
    property CompactionByModelLayerFormat: integer
      read FCompactionByModelLayerFormat write SetCompactionByModelLayerFormat;
    // Ifm3
    property CompactionByInterbedSystemFormat: integer
      read FCompactionByInterbedSystemFormat
      write SetCompactionByInterbedSystemFormat;
    // Ifm4
    property VerticalDisplacementFormat: integer
      read FVerticalDisplacementFormat write SetVerticalDisplacementFormat;
    // Ifm5
    property PreconsolidationStress: integer read FPreconsolidationStress
      write SetPreconsolidationStress;
    // Ifm6
    property DeltaPreconsolidationStress: integer
      read FDeltaPreconsolidationStress write SetDeltaPreconsolidationStress;
    // Ifm7
    property GeostaticStress: integer read FGeostaticStress
      write SetGeostaticStress;
    // Ifm8
    property DeltaGeostaticStress: integer read FDeltaGeostaticStress
      write SetDeltaGeostaticStress;
    // Ifm9
    property EffectiveStress: integer read FEffectiveStress
      write SetEffectiveStress;
    // Ifm10
    property DeltaEffectiveStress: integer read FDeltaEffectiveStress
      write SetDeltaEffectiveStress;
    // Ifm11
    property VoidRatio: integer read FVoidRatio write SetVoidRatio;
    // Ifm12
    property ThicknessCompressibleSediments: integer
      read FThicknessCompressibleSediments
      write SetThicknessCompressibleSediments;
    // Ifm13
    property LayerCenterElevation: integer read FLayerCenterElevation
      write SetLayerCenterElevation;
  end;

  TThickResponse = (trConstant, trVariable);
  TVoidRatioResponse = (vrrConstant, vrrVariable);
  TPreconsolidationSource = (pcSpecified, pcOffsets);
  TCompressionSource = (csCompressionReComp, csSpecificStorage);

  TSwtPackageSelection = class(TModflowPackageSelection)
  private
    FInitialPrint: TSwtInitialPrint;
    FThickResponse: TThickResponse;
    FPrintChoices: TSwtPrintCollection;
    FCompressionSource: TCompressionSource;
    FPreconsolidationSource: TPreconsolidationSource;
    FVoidRatioResponse: TVoidRatioResponse;
    FPrintFormats: TSwtPrintFormats;
    FSubBinaryOutputChoice: TSubBinaryOutputChoice;
    procedure SetCompressionSource(const Value: TCompressionSource);
    procedure SetInitialPrint(const Value: TSwtInitialPrint);
    procedure SetPreconsolidationSource(const Value: TPreconsolidationSource);
    procedure SetPrintChoices(const Value: TSwtPrintCollection);
    procedure SetPrintFormats(const Value: TSwtPrintFormats);
    procedure SetThickResponse(const Value: TThickResponse);
    procedure SetVoidRatioResponse(const Value: TVoidRatioResponse);
    procedure SetSubBinaryOutputChoice(const Value: TSubBinaryOutputChoice);
  public
    procedure Assign(Source: TPersistent); override;
    procedure InitializeVariables; override;
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
  published
    // Data set 1, ITHK
    property ThickResponse: TThickResponse read FThickResponse
      write SetThickResponse;
    // Data set 1, IVOID
    property VoidRatioResponse: TVoidRatioResponse read FVoidRatioResponse
      write SetVoidRatioResponse;
    // Data set 1, ISTPCS
    property PreconsolidationSource: TPreconsolidationSource
      read FPreconsolidationSource write SetPreconsolidationSource
      default pcSpecified;
    // Data set 1, ICRCC
    property CompressionSource: TCompressionSource read FCompressionSource
      write SetCompressionSource;
    //  Data set 16
    property PrintFormats: TSwtPrintFormats read FPrintFormats
      write SetPrintFormats;
    // data set 17
    property PrintChoices: TSwtPrintCollection read FPrintChoices
      write SetPrintChoices;
    // Data set 3
    property InitialPrint: TSwtInitialPrint read FInitialPrint
      write SetInitialPrint;
    property BinaryOutputChoice: TSubBinaryOutputChoice
      read FSubBinaryOutputChoice write SetSubBinaryOutputChoice;
  end;

  THydPackageSelection = class(TModflowPackageSelection)
  private
    FStoredHYDNOH: TRealStorage;
    procedure SetStoredHYDNOH(const Value: TRealStorage);
    function GetHYDNOH: double;
    procedure SetHYDNOH(const Value: double);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
    property HYDNOH: double read GetHYDNOH write SetHYDNOH;
    procedure InitializeVariables; override;
  published
    property StoredHYDNOH: TRealStorage read FStoredHYDNOH write SetStoredHYDNOH;
  end;

  TMt3dBasic = class(TModflowPackageSelection)
  private
    FMinimumSaturatedFraction: TRealStorage;
    FInactiveConcentration: TRealStorage;
    FMassUnit: TStringStorage;
    procedure SetStoredInactiveConcentration(const Value: TRealStorage);
    procedure SetStoredMassUnit(const Value: TStringStorage);
    procedure SetStoredMinimumSaturatedFraction(const Value: TRealStorage);
    function GetInactiveConcentration: double;
    function GetMassUnit: string;
    function GetMinimumSaturatedFraction: double;
    procedure SetInactiveConcentration(const Value: double);
    procedure SetMassUnit(const Value: string);
    procedure SetMinimumSaturatedFraction(const Value: double);
    procedure Changed(Sender: TObject);
    procedure UpdateDataSets;
  protected
    procedure SetIsSelected(const Value: boolean); override;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
    procedure InitializeVariables; override;
    property MassUnit: string read GetMassUnit write SetMassUnit;
    property InactiveConcentration: double read GetInactiveConcentration
      write SetInactiveConcentration;
    property MinimumSaturatedFraction: double read GetMinimumSaturatedFraction
      write SetMinimumSaturatedFraction;
  published
    property StoredMassUnit: TStringStorage read FMassUnit
      write SetStoredMassUnit;
    property StoredInactiveConcentration: TRealStorage
      read FInactiveConcentration write SetStoredInactiveConcentration;
    property StoredMinimumSaturatedFraction: TRealStorage
      read FMinimumSaturatedFraction write SetStoredMinimumSaturatedFraction;
  end;

  TGcgPreconditioner = (gpJacobi, gpSSOR, gpCholesky);
  TDispersionTensorTreatment = (dtcLump, dtcFull);

  TMt3dmsGCGSolverPackage = class(TModflowPackageSelection)
  private
    FPrintoutInterval: integer;
    FDispersionTensorChoice: TDispersionTensorTreatment;
    FPreconditionerChoice: TGcgPreconditioner;
    FMaxOuterIterations: integer;
    FStoredRelaxationFactor: TRealStorage;
    FStoredConvergenceCriterion: TRealStorage;
    FMaxInnerIterations: integer;
    procedure SetDispersionTensorChoice(const Value: TDispersionTensorTreatment);
    procedure SetMaxInnerIterations(const Value: integer);
    procedure SetMaxOuterIterations(const Value: integer);
    procedure SetPreconditionerChoice(const Value: TGcgPreconditioner);
    procedure SetPrintoutInterval(const Value: integer);
    procedure SetStoredConvergenceCriterion(const Value: TRealStorage);
    procedure SetStoredRelaxationFactor(const Value: TRealStorage);
    procedure Changed(Sender: TObject);
    function GetConvergenceCriterion: double;
    function GetRelaxationFactor: double;
    procedure SetConvergenceCriterion(const Value: double);
    procedure SetRelaxationFactor(const Value: double);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
    procedure InitializeVariables; override;
    // ACCL
    property RelaxationFactor: double read GetRelaxationFactor
      write SetRelaxationFactor;
    // CCLOSE
    property ConvergenceCriterion: double read GetConvergenceCriterion
      write SetConvergenceCriterion;
  published
    // MXITER
    property MaxOuterIterations: integer read FMaxOuterIterations
      write SetMaxOuterIterations stored True;
    // ITER1
    property MaxInnerIterations: integer read FMaxInnerIterations
      write SetMaxInnerIterations stored True;
    // ISOLVE
    property PreconditionerChoice: TGcgPreconditioner read FPreconditionerChoice
      write SetPreconditionerChoice stored True;
    // NCRS
    property DispersionTensorChoice: TDispersionTensorTreatment
      read FDispersionTensorChoice write SetDispersionTensorChoice stored True;
    // ACCL
    property StoredRelaxationFactor: TRealStorage read FStoredRelaxationFactor
      write SetStoredRelaxationFactor stored True;
    // CCLOSE
    property StoredConvergenceCriterion: TRealStorage
      read FStoredConvergenceCriterion write SetStoredConvergenceCriterion stored True;
    // IPRGCG
    property PrintoutInterval: integer read FPrintoutInterval
      write SetPrintoutInterval stored True;
  end;

  TAdvectionSolution = (asUltimate, asStandard, asMoc, asMmoc, asHmoc);
  TWeightingScheme = (wsUpstream, wsCentral);
  TParticleTrackMethod = (ptmEuler, ptmRungeKutta, ptmHybrid);
  TParticlePlacementMethod = (ppmRandom, ppmFixed);

  TMt3dmsAdvection = class(TModflowPackageSelection)
  private
    FStoredRelCelConcGrad: TRealStorage;
    FStoredCriticalConcGradient: TRealStorage;
    FSinkNumberOfParticlePlanes: integer;
    FSinkParticlePlacementMethod: TParticlePlacementMethod;
    FNumberOfParticlePlanes: integer;
    FParticlePlacementMethod: TParticlePlacementMethod;
    FWeightingScheme: TWeightingScheme;
    FMaxParticlesPerCell: integer;
    FLowGradientParticleCount: integer;
    FAdvectionSolution: TAdvectionSolution;
    FStoredCourant: TRealStorage;
    FHighGradientParticleCount: integer;
    FStoredConcWeight: TRealStorage;
    FMaximumParticles: integer;
    FSinkParticleCount: integer;
    FMinParticlePerCell: integer;
    FParticleTrackMethod: TParticleTrackMethod;
    function GetConcWeight: double;
    function GetCourant: double;
    function GetCriticalConcGradient: double;
    function GetRelCelConcGrad: double;
    procedure SetAdvectionSolution(const Value: TAdvectionSolution);
    procedure SetConcWeight(const Value: double);
    procedure SetCourant(const Value: double);
    procedure SetCriticalConcGradient(const Value: double);
    procedure SetHighGradientParticleCount(const Value: integer);
    procedure SetLowGradientParticleCount(const Value: integer);
    procedure SetMaximumParticles(const Value: integer);
    procedure SetMaxParticlesPerCell(const Value: integer);
    procedure SetMinParticlePerCell(const Value: integer);
    procedure SetNumberOfParticlePlanes(const Value: integer);
    procedure SetParticlePlacementMethod(const Value: TParticlePlacementMethod);
    procedure SetParticleTrackMethod(const Value: TParticleTrackMethod);
    procedure SetRelCelConcGrad(const Value: double);
    procedure SetSinkNumberOfParticlePlanes(const Value: integer);
    procedure SetSinkParticleCount(const Value: integer);
    procedure SetSinkParticlePlacementMethod(
      const Value: TParticlePlacementMethod);
    procedure SetStoredConcWeight(const Value: TRealStorage);
    procedure SetStoredCourant(const Value: TRealStorage);
    procedure SetStoredCriticalConcGradient(const Value: TRealStorage);
    procedure SetStoredRelCelConcGrad(const Value: TRealStorage);
    procedure SetWeightingScheme(const Value: TWeightingScheme);
    procedure Changed(Sender: TObject);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
    procedure InitializeVariables; override;
    // PERCEL
    property Courant: double read GetCourant write SetCourant;
    // WD
    property ConcWeight: double read GetConcWeight write SetConcWeight;
    // DCEPS
    property RelCelConcGrad: double read GetRelCelConcGrad
      write SetRelCelConcGrad;
    // DCHMOC
    property CriticalConcGradient: double
      read GetCriticalConcGradient write SetCriticalConcGradient;
  published
    // MIXELM
    property AdvectionSolution: TAdvectionSolution
      read FAdvectionSolution write SetAdvectionSolution stored True;
    // PERCEL
    property StoredCourant: TRealStorage
      read FStoredCourant write SetStoredCourant stored True;
    // MXPART
    property MaximumParticles: integer
      read FMaximumParticles write SetMaximumParticles stored True;
    // NADVFD
    property WeightingScheme: TWeightingScheme
      read FWeightingScheme write SetWeightingScheme stored True;
    // ITRACK
    property ParticleTrackMethod: TParticleTrackMethod
      read FParticleTrackMethod write SetParticleTrackMethod stored True;
    // WD is a concentration weighting factor
    property StoredConcWeight: TRealStorage
      read FStoredConcWeight write SetStoredConcWeight;
    // DCEPS
    property StoredRelCelConcGrad: TRealStorage
      read FStoredRelCelConcGrad write SetStoredRelCelConcGrad stored True;
    // NPLANE
    property ParticlePlacementMethod: TParticlePlacementMethod
      read FParticlePlacementMethod write SetParticlePlacementMethod stored True;
    // NPLANE
    property NumberOfParticlePlanes: integer
      read FNumberOfParticlePlanes write SetNumberOfParticlePlanes stored True;
    // NPL
    property LowGradientParticleCount: integer
      read FLowGradientParticleCount write SetLowGradientParticleCount stored True;
    // NPH
    property HighGradientParticleCount: integer
      read FHighGradientParticleCount write SetHighGradientParticleCount stored True;
    // NPMIN
    property MinParticlePerCell: integer
      read FMinParticlePerCell write SetMinParticlePerCell stored True;
    // NPMAX
    property MaxParticlesPerCell: integer
      read FMaxParticlesPerCell write SetMaxParticlesPerCell stored True;
    // NLSINK
    property SinkParticlePlacementMethod: TParticlePlacementMethod
      read FSinkParticlePlacementMethod write SetSinkParticlePlacementMethod stored True;
    // NLSINK
    property SinkNumberOfParticlePlanes: integer
      read FSinkNumberOfParticlePlanes write SetSinkNumberOfParticlePlanes stored True;
    // NPSINK
    property SinkParticleCount: integer
      read FSinkParticleCount write SetSinkParticleCount stored True;
    // DCHMOC
    property StoredCriticalConcGradient: TRealStorage
      read FStoredCriticalConcGradient write SetStoredCriticalConcGradient;
  end;

  TMt3dmsDispersion = class(TModflowPackageSelection)
  private
    FMultiDifussion: boolean;
    procedure SetMultiDifussion(const Value: boolean);
    procedure UpdateDataSets;
  protected
    procedure SetIsSelected(const Value: boolean); override;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel);
    procedure InitializeVariables; override;
  published
    property MultiDifussion: boolean read FMultiDifussion
      write SetMultiDifussion;
  end;

  TMt3dmsSourceSinkMixing = class(TModflowPackageSelection)
  private
    FConcentrations: TModflowBoundaryDisplayTimeList;
    procedure InitializeConcentrationDisplay(Sender: TObject);
    procedure GetConcentrationUseList(Sender: TObject; NewUseList: TStringList);
  public
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
    property Concentrations: TModflowBoundaryDisplayTimeList
      read FConcentrations;
  end;

  TSorptionChoice = (scNone, scLinear, scFreundlich, scLangmuir, scFirstOrderKinetic,
    scDualDomainNoSorption, scDualDomainWithSorption);
  TKineticChoice = (kcNone, kcFirstOrder, kcZeroOrder);
  TOtherInitialConcChoice = (oicDontUse, oicUse);

  TMt3dmsChemReaction = class(TModflowPackageSelection)
  private
    FSorptionChoice: TSorptionChoice;
    FOtherInitialConcChoice: TOtherInitialConcChoice;
    FKineticChoice: TKineticChoice;
    procedure SetKineticChoice(const Value: TKineticChoice);
    procedure SetOtherInitialConcChoice(const Value: TOtherInitialConcChoice);
    procedure SetSorptionChoice(const Value: TSorptionChoice);
    procedure UpdateDataSets;
  protected
    procedure SetIsSelected(const Value: boolean); override;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel);
    procedure InitializeVariables; override;
  published
    // ISOTHM
    property SorptionChoice: TSorptionChoice read FSorptionChoice
      write SetSorptionChoice stored True;
    // IREACT
    property KineticChoice: TKineticChoice read FKineticChoice
      write SetKineticChoice stored True;
    // IGETSC
    property OtherInitialConcChoice: TOtherInitialConcChoice
      read FOtherInitialConcChoice write SetOtherInitialConcChoice stored True;
  end;

  TConcObsResult = (corConc, corConcResid);
  TMassFluxObsResult = (mfoMassFlux, mfoMassFluxResid);
  TTransformType = (ltNoConversion, ltLogConverion);
  TInterpolateObs = (ioNoInterpolation, ioBilinear);
  TSaveBinary = (sbNoSave, sbSave);

  TMt3dmsTransportObservations = class(TModflowPackageSelection)
  private
    FInterpolateObs: TInterpolateObs;
    FSaveBinary: TSaveBinary;
    FStoredConcScaleFactor: TRealStorage;
    FStoredFluxScaleFactor: TRealStorage;
    FConcObsResult: TConcObsResult;
    FTransformType: TTransformType;
    FMassFluxObsResult: TMassFluxObsResult;
    function GetConcScaleFactor: double;
    procedure SetConcObsResult(const Value: TConcObsResult);
    procedure SetConcScaleFactor(const Value: double);
    procedure SetInterpolateObs(const Value: TInterpolateObs);
    procedure SetSaveBinary(const Value: TSaveBinary);
    procedure SetStoredConcScaleFactor(const Value: TRealStorage);
    procedure SetTransformType(const Value: TTransformType);
    procedure Changed(Sender: TObject);
    procedure SetStoredFluxScaleFactor(const Value: TRealStorage);
    function GetFluxScaleFactor: double;
    procedure SetFluxScaleFactor(const Value: double);
    procedure SetMassFluxObsResult(const Value: TMassFluxObsResult);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    procedure InitializeVariables; override;
    // CScale
    property ConcScaleFactor: double read GetConcScaleFactor
      write SetConcScaleFactor;
    // FScale
    property FluxScaleFactor: double read GetFluxScaleFactor
      write SetFluxScaleFactor;
  published
    // inSaveObs
    property SaveBinary: TSaveBinary read FSaveBinary write SetSaveBinary stored True;
    // CScale
    property StoredConcScaleFactor: TRealStorage read FStoredConcScaleFactor
      write SetStoredConcScaleFactor stored True;
    // iOutCobs
    property ConcObsResult: TConcObsResult read FConcObsResult
      write SetConcObsResult stored True;
    // iConcLOG
    property TransformType: TTransformType read FTransformType
      write SetTransformType stored True;
    // iConcINTP
    property InterpolateObs: TInterpolateObs read FInterpolateObs
      write SetInterpolateObs stored True;
    // FScale
    property StoredFluxScaleFactor: TRealStorage read FStoredFluxScaleFactor
      write SetStoredFluxScaleFactor;
    // iOutFlux
    property MassFluxObsResult: TMassFluxObsResult read FMassFluxObsResult
      write SetMassFluxObsResult stored True;
  end;


implementation

uses Math, Contnrs , PhastModelUnit, ModflowOptionsUnit,
  frmErrorsAndWarningsUnit, ModflowSfrParamIcalcUnit,
  ModflowWellWriterUnit, ModflowGHB_WriterUnit,
  ModflowDRN_WriterUnit, ModflowDRT_WriterUnit, ModflowRiverWriterUnit,
  ModflowCHD_WriterUnit, ModflowEVT_WriterUnit, ModflowEvtUnit, RbwParser,
  frmFormulaErrorsUnit, ModflowRCH_WriterUnit, ModflowRchUnit,
  ModflowETS_WriterUnit, ModflowEtsUnit, ModflowUzfWriterUnit, ModflowUzfUnit,
  ModflowSfrWriterUnit, ModflowSfrUnit, ModflowSfrReachUnit, ModflowSfrFlows,
  ModflowSfrChannelUnit, ModflowSfrEquationUnit, ModflowSfrSegment,
  ModflowSfrUnsatSegment, ModflowMNW2_WriterUnit, ModflowMnw2Unit,
  LayerStructureUnit, ModflowSubsidenceDefUnit, frmGridValueUnit,
  frmGoPhastUnit, CustomModflowWriterUnit, frmDisplayDataUnit,
  Mt3dmsSsmWriterUnit, Mt3dmsChemUnit;

resourcestring
  StrLengthUnitsForMod = 'Length units for model are undefined';
  SfrError = 'SFR Error';
  StrTimeUnitsForModel = 'Time units for model are undefined';
  StrInTheSubsidencePa = 'In the Subsidence package, one or more starting ti' +
  'me is after the ending time';
  StrInTheSubsidenceAn = 'In the Subsidence and Aquifer-System Compaction Pa' +
  'ckage for Water-Table Aquifers, one or more starting time is after the en' +
  'ding time';
  StrStartingTime0g = 'StartingTime: %:0g; EndingTime: %1:g';


{ TModflowPackageSelection }

procedure TModflowPackageSelection.AddTimeList(TimeList: TCustomTimeList);
begin
  Assert(FModel <> nil);
  (FModel as TCustomModel).AddTimeList(TimeList);
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

constructor TModflowPackageSelection.Create(Model: TBaseModel);
begin
  inherited Create;
  Assert((Model = nil) or (Model is TCustomModel));
  FModel := Model;
  FComments := TStringList.Create;
end;

destructor TModflowPackageSelection.Destroy;
begin
  FComments.Free;
  inherited;
end;

procedure TModflowPackageSelection.InitializeVariables;
begin
  IsSelected := False;
  Comments.Clear;
end;

procedure TModflowPackageSelection.InvalidateAllTimeLists;
begin

end;

procedure TModflowPackageSelection.InvalidateModel;
begin
  if FModel <> nil then
  begin
    FModel.Invalidate;
  end;
end;

function TModflowPackageSelection.PackageUsed(Sender: TObject): boolean;
begin
  result := IsSelected;
end;

procedure TModflowPackageSelection.RemoveTimeList(TimeList: TCustomTimeList);
begin
  (FModel as TCustomModel).RemoveTimeList(TimeList);
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
      InvalidateAllTimeLists;
      UpdateFrmDisplayData;
//      UpdateFrmContourData;
      UpdateFrmGridValue;
    end;
  end;
end;

procedure TModflowPackageSelection.UpdateDisplayUseList(NewUseList: TStringList;
  ParamType: TParameterType; DataIndex: integer; const DisplayName: string);
begin
  (FModel as TCustomModel).UpdateDisplayUseList(NewUseList,
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
  Parser := (FModel as TCustomModel).rpThreeDFormulaCompiler;
  Formula := Item.BoundaryFormula[DataIndex];
  try
    Parser.Compile(Formula);
  except on E: ErbwParserError do
    begin
      ScreenObject := Item.ScreenObject as TScreenObject;
      frmFormulaErrors.AddFormulaError(ScreenObject.Name, StrModflowSfrReachLength,
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
  Parser := (FModel as TCustomModel).rpTopFormulaCompiler;
  try
    Parser.Compile(Formula);
  except on E: ErbwParserError do
    begin
      frmFormulaErrors.AddFormulaError(ScreenObject.Name, 'Elevation formula',
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

constructor TPcgSelection.Create(Model: TBaseModel);
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
  inherited;
  IsSelected := True;
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

constructor TCustomTransientLayerPackageSelection.Create(Model: TBaseModel);
begin
  inherited Create(Model);
  FZoneArrayNames := TTransientZoneCollection.Create;
  FMultiplierArrayNames := TTransientMultCollection.Create;
  FLayerOption := loTop;
end;

destructor TCustomTransientLayerPackageSelection.Destroy;
begin
  FMultiplierArrayNames.Free;
  FZoneArrayNames.Free;
  inherited;
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

procedure TRchPackageSelection.Assign(Source: TPersistent);
begin
  if Source is TRchPackageSelection then
  begin
    AssignmentMethod := TRchPackageSelection(Source).AssignmentMethod;
  end;
  inherited;
end;

constructor TRchPackageSelection.Create(Model: TBaseModel);
begin
  inherited;
//  FAssignmentMethod := umAdd;
  if Model <> nil then
  begin
    OnLayerChoiceChange := (Model as TCustomModel).InvalidateMfRchLayer;

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
  LocalModel: TCustomModel;
begin
  LocalModel := FModel as TCustomModel;
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
  RchWriter := TModflowRCH_Writer.Create(FModel as TCustomModel, etDisplay);
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
//  if PackageUsed(FModel) then
  begin
    MfRchRate.Invalidate;
    MfRchLayer.Invalidate;
  end;
end;

procedure TRchPackageSelection.InvalidateMfRchLayer(Sender: TObject);
begin
  MfRchLayer.Invalidate;
end;

procedure TRchPackageSelection.SetAssignmentMethod(const Value: TUpdateMethod);
begin
  if FAssignmentMethod <> Value then
  begin
    FAssignmentMethod := Value;
    InvalidateModel;
    if FModel <> nil then
    begin
      MfRchRate.Invalidate;
    end;
  end;
end;
{ TEtsPackageSelection }

procedure TEtsPackageSelection.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TEtsPackageSelection then
  begin
    SegmentCount := TEtsPackageSelection(Source).SegmentCount;
    if FModel <> nil then
    begin
      UpdateEtsSegmentCount;
    end;
  end;
end;

constructor TEtsPackageSelection.Create(Model: TBaseModel);
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
  LocalModel: TCustomModel;
begin
  LocalModel := FModel as TCustomModel;
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
  EtsWriter := TModflowETS_Writer.Create(FModel as TCustomModel, etDisplay);
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
//  if PackageUsed(nil) then
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
  LocalModel: TCustomModel;
begin
  LocalModel := FModel as TCustomModel;
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

constructor TResPackageSelection.Create(Model: TBaseModel);
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

constructor TLakePackageSelection.Create(Model: TBaseModel);
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

constructor TEvtPackageSelection.Create(Model: TBaseModel);
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
  LocalModel: TCustomModel;
begin
  LocalModel := FModel as TCustomModel;
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
  EvtWriter := TModflowEVT_Writer.Create(FModel as TCustomModel, etDisplay);
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
//  if PackageUsed(FModel) then
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
  LocalModel: TCustomModel;
begin
  LocalModel := FModel as TCustomModel;
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
    UseGsflowFormat := Stream.UseGsflowFormat;
    if AssignParameterInstances then
    begin
      ParameterInstances := Stream.ParameterInstances;
    end;
  end;
  inherited;
end;

constructor TSfrPackageSelection.Create(Model: TBaseModel);
begin
  inherited;
  InitializeVariables;
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
  LocalModel: TCustomModel;
begin
  LocalModel := FModel as TCustomModel;
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
  LocalModel: TCustomModel;
begin
  LocalModel := FModel as TCustomModel;
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
  LocalModel: TCustomModel;
begin
  LocalModel := FModel as TCustomModel;
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
  LocalModel: TCustomModel;
begin
  LocalModel := FModel as TCustomModel;
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
  LocalModel: TCustomModel;
begin
  LocalModel := FModel as TCustomModel;
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
  LocalModel: TCustomModel;
begin
  LocalModel := FModel as TCustomModel;
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
  LocalModel: TCustomModel;
begin
  LocalModel := FModel as TCustomModel;
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
  LocalModel: TCustomModel;
begin
  LocalModel := FModel as TCustomModel;
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

procedure TSfrPackageSelection.ComputeAverages(List: TModflowBoundListOfTimeLists);
var
  Index: Integer;
  DisplayList: TModflowBoundaryDisplayTimeList;
begin
  for Index := 0 to List.Count - 1 do
  begin
    DisplayList := List[Index];
    DisplayList.ComputeAverage;
  end;
end;

procedure TSfrPackageSelection.InitializeSfrDisplay(Sender: TObject);
var
  SfrWriter: TModflowSFR_Writer;
  List: TModflowBoundListOfTimeLists;
begin
  List := TModflowBoundListOfTimeLists.Create;
  SfrWriter := TModflowSFR_Writer.Create(FModel as TCustomModel, etDisplay);
  try
    GetDisplayLists(List);

    SfrWriter.UpdateDisplay(List);

    ComputeAverages(List);
  finally
    SfrWriter.Free;
    List.Free;
  end;
end;

procedure TSfrPackageSelection.InvalidateAllTimeLists;
begin
  inherited;
//  if PackageUsed(nil) then
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

procedure TSfrPackageSelection.InitializeVariables;
begin
  inherited;
  Dleak := 0.0001;
  Isfropt := 0;
  Nstrail := 10;
  Isuzn := 10;
  Nsfrsets := 30;
  FKinematicRoutingTolerance := 0.0001;
  FPrintStreams := True;
  FPrintFlows := pfListing;
  FTimeStepsForKinematicRouting := 1;
  FKinematicRoutingWeight := 1;
  GageOverallBudget := False;
  UseGsflowFormat := False;
  KinematicRouting := False;
end;

procedure TSfrPackageSelection.GetDisplayLists(List: TModflowBoundListOfTimeLists);
var
  Index: Integer;
  DisplayList: TModflowBoundaryDisplayTimeList;
begin
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
  PhastModel: TCustomModel;
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
        PhastModel := FModel as TCustomModel;
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
          PhastModel := FModel as TCustomModel;
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
          PhastModel := FModel as TCustomModel;
        end;
        PhastModel.InvalidateMfSfrVerticalUnsatK(self);
      end;
      SelectionChanged := (FIsfropt in [0,4,5]) <> (Value in [0,4,5]);
      if SelectionChanged then
      begin
        if PhastModel = nil then
        begin
          PhastModel := FModel as TCustomModel;
        end;
        PhastModel.InvalidateMfSfrUpstreamHydraulicConductivity(self);
        PhastModel.InvalidateMfSfrDownstreamHydraulicConductivity(self);
      end;
      SelectionChanged := (FIsfropt in [0,1,2,3]) <> (Value in [0,1,2,3]);
      if SelectionChanged then
      begin
        if PhastModel = nil then
        begin
          PhastModel := FModel as TCustomModel;
        end;
        PhastModel.InvalidateMfSfrUpstreamWidth(self);
        PhastModel.InvalidateMfSfrDownstreamWidth(self);
      end;
      SelectionChanged := (FIsfropt  <> Value);
      if SelectionChanged then
      begin
        if PhastModel = nil then
        begin
          PhastModel := FModel as TCustomModel;
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
          PhastModel := FModel as TCustomModel;
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
          PhastModel := FModel as TCustomModel;
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

procedure TSfrPackageSelection.SetUseGsflowFormat(const Value: boolean);
begin
  if FUseGsflowFormat <> Value then
  begin
    InvalidateModel;
    FUseGsflowFormat := Value;
  end;
end;

function TSfrPackageSelection.StreamConstant: double;
var
  ModflowOptions: TModflowOptions;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(FModel, SfrError);
  result := 1;
  ModflowOptions := (FModel as TCustomModel).ModflowOptions;
  case ModflowOptions.LengthUnit of
    0: // undefined
      begin
        frmErrorsAndWarnings.AddError(FModel, SfrError, StrLengthUnitsForMod);
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
//        ErrorMessage :=
//          StrTimeUnitsForModel;
        frmErrorsAndWarnings.AddError(FModel, SfrError, StrTimeUnitsForModel);
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
  // if Assign is updated, update IsSame too.
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
  LocalModel: TCustomModel;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ParamIndex: Integer;
  Item: TSfrParamIcalcItem;
begin
  if FParameterInstance <> Value then
  begin
    InvalidateModel;
    LocalModel := (Collection as TOrderedCollection).Model as TCustomModel;
    if LocalModel <> nil then
    begin
      for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
      begin
        ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
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
  LocalModel: TCustomModel;
  ScreenObjectIndex: integer;
  ScreenObject: TScreenObject;
  ParamIndex: Integer;
  Item: TSfrParamIcalcItem;
  NewName: string;
begin
  NewName := CorrectParamName(Value);
  if FParameterName <> NewName then
  begin
    InvalidateModel;
    LocalModel := (Collection as TOrderedCollection).Model as TCustomModel;
    if LocalModel <> nil then
    begin
      for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
      begin
        ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
        if ScreenObject.ModflowSfrBoundary <> nil then
        begin
          for ParamIndex := 0 to ScreenObject.ModflowSfrBoundary.ParamIcalc.Count - 1 do
          begin
            Item := ScreenObject.ModflowSfrBoundary.ParamIcalc.Items[ParamIndex];
            if Item.Param = FParameterName then
            begin
              Item.Param := NewName;
            end;
          end;
        end;
      end;
    end;
    FParameterName := NewName;
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

constructor TSfrParamInstances.Create(Model: TBaseModel);
begin
  inherited Create(TSfrParamInstance, Model);
end;

procedure TSfrParamInstances.DeleteInstancesOfParameter(
  const ParamName: string);
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    if Items[Index].ParameterName = ParamName then
    begin
      Delete(Index);
    end;
  end;
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

procedure TSfrParamInstances.UpdateParamName(const OldParamName,
  NewParamName: string);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    if Items[Index].ParameterName = OldParamName then
    begin
      Items[Index].ParameterName := NewParamName;
    end;
  end;
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

constructor TCustomLayerPackageSelection.Create(Model: TBaseModel);
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
    AssignmentMethod := UZF.AssignmentMethod;
    SpecifyResidualWaterContent := UZF.SpecifyResidualWaterContent;
    SpecifyInitialWaterContent := UZF.SpecifyInitialWaterContent;
    CalulateSurfaceLeakage := UZF.CalulateSurfaceLeakage;
  end;
  inherited;
end;

constructor TUzfPackageSelection.Create(Model: TBaseModel);
begin
  inherited;
  InitializeVariables;

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
  LocalModel: TCustomModel;
begin
  LocalModel := FModel as TCustomModel;
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
  LocalModel: TCustomModel;
begin
  LocalModel := FModel as TCustomModel;
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
  LocalModel: TCustomModel;
begin
  LocalModel := FModel as TCustomModel;
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
  LocalModel: TCustomModel;
begin
  LocalModel := FModel as TCustomModel;
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
  UzfWriter := TModflowUzfWriter.Create(FModel as TCustomModel, etDisplay);
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

procedure TUzfPackageSelection.InitializeVariables;
begin
  inherited;
  VerticalKSource := 1;
  RouteDischargeToStreams := True;
  SimulateET := True;
  NumberOfTrailingWaves := 15;
  NumberOfWaveSets:= 20;
  FDepthOfUndulations := 1;
  FSpecifyResidualWaterContent := False;
  FSpecifyInitialWaterContent := False;
  FCalulateSurfaceLeakage := True;
end;

procedure TUzfPackageSelection.InvalidateAllTimeLists;
begin
  inherited;
//  if PackageUsed(FModel) then
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

procedure TUzfPackageSelection.SetAssignmentMethod(const Value: TUpdateMethod);
begin
  if FAssignmentMethod <> Value then
  begin
    FAssignmentMethod := Value;
    InvalidateModel;
    if FModel <> nil then
    begin
      MfUzfInfiltration.Invalidate;
    end;
  end;
end;

procedure TUzfPackageSelection.SetCalulateSurfaceLeakage(const Value: boolean);
begin
  FCalulateSurfaceLeakage := Value;
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

procedure TUzfPackageSelection.SetSpecifyInitialWaterContent(
  const Value: boolean);
begin
  FSpecifyInitialWaterContent := Value;
end;

procedure TUzfPackageSelection.SetSpecifyResidualWaterContent(
  const Value: boolean);
begin
  FSpecifyResidualWaterContent := Value;
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
    (FModel as TCustomModel).DischargeRoutingUpdate;
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

constructor TGmgPackageSelection.Create(Model: TBaseModel);
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
  inherited;
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

constructor TSIPPackageSelection.Create(Model: TBaseModel);
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
  inherited;
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

constructor TDE4PackageSelection.Create(Model: TBaseModel);
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
  inherited;
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

constructor THobPackageSelection.Create(Model: TBaseModel);
begin
  inherited Create(Model);
  InitializeVariables;
end;

procedure THobPackageSelection.InitializeVariables;
begin
  inherited;
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
    UseStorageCoefficient := LpfSource.UseStorageCoefficient;
  end;
  inherited;
end;

constructor TLpfSelection.Create(Model: TBaseModel);
begin
  inherited;
  InitializeVariables;
end;

destructor TLpfSelection.Destroy;
begin
  inherited;
end;

procedure TLpfSelection.InitializeVariables;
begin
  inherited;
  IsSelected := True;
  FUseCvCorrection := True;
  FUseVerticalFlowCorrection := True;
  FUseSaturatedThickness := False;
  FUseConstantCV := False;
  FUseStorageCoefficient := False;
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
  if FUseStorageCoefficient <> Value then
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

constructor TModpathSelection.Create(Model: TBaseModel);
begin
  inherited;
  FOutputTimes := TModpathTimes.Create(Model);
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
  inherited;
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

procedure TModpathSelection.SetIsSelected(const Value: boolean);
begin
  inherited;
  if FModel <> nil then
  begin
    (FModel as TCustomModel).DataArrayManager.UpdateClassifications;
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

constructor TWellPackage.Create(Model: TBaseModel);
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
  WellWriter := TModflowWEL_Writer.Create(FModel as TCustomModel, etDisplay);
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
//  if PackageUsed(FModel) then
  begin
    InvalidateMfWellPumpage(FModel);
  end;
end;

procedure TWellPackage.InvalidateMfWellPumpage(Sender: TObject);
begin
  MfWellPumpage.Invalidate;
end;

{ TGhbPackage }

constructor TGhbPackage.Create(Model: TBaseModel);
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
  GhbWriter := TModflowGHB_Writer.Create(FModel as TCustomModel, etDisplay);
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
//  if PackageUsed(FModel) then
  begin
    MfGhbBoundaryHead.Invalidate;
    MfGhbConductance.Invalidate;
  end;
end;

{ TDrnPackage }

constructor TDrnPackage.Create(Model: TBaseModel);
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
  UpdateDisplayUseList(NewUseList, ptDRN, 0, StrDrainElevation);
end;

procedure TDrnPackage.InitializeDrnDisplay(Sender: TObject);
var
  DrnWriter: TModflowDRN_Writer;
  List: TModflowBoundListOfTimeLists;
begin
  MfDrnConductance.CreateDataSets;
  MfDrnElevation.CreateDataSets;

  List := TModflowBoundListOfTimeLists.Create;
  DrnWriter := TModflowDRN_Writer.Create(FModel as TCustomModel, etDisplay);
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
//  if PackageUsed(FModel) then
  begin
    MfDrnElevation.Invalidate;
    MfDrnConductance.Invalidate;
  end;
end;

{ TDrtPackage }

constructor TDrtPackage.Create(Model: TBaseModel);
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
  DrtWriter := TModflowDRT_Writer.Create(FModel as TCustomModel, etDisplay);
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
//  if PackageUsed(FModel) then
  begin
    MfDrtConductance.Invalidate;
    MfDrtElevation.Invalidate;
    MfDrtReturnFraction.Invalidate;
  end;
end;

{ TRivPackage }

constructor TRivPackage.Create(Model: TBaseModel);
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
  UpdateDisplayUseList(NewUseList, ptRIV, 0, StrRiverStage);
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
  RivWriter := TModflowRIV_Writer.Create(FModel as TCustomModel, etDisplay);
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
//  if PackageUsed(FModel) then
  begin
    MfRivConductance.Invalidate;
    MfRivStage.Invalidate;
    MfRivBottom.Invalidate;
  end;
end;

{ TChdPackage }

constructor TChdPackage.Create(Model: TBaseModel);
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
  ChdWriter := TModflowCHD_Writer.Create(FModel as TCustomModel, etDisplay);
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
//  if PackageUsed(FModel) then
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

constructor THufPackageSelection.Create(Model: TBaseModel);
begin
  inherited;
  InitializeVariables;
end;

procedure THufPackageSelection.InitializeVariables;
begin
  inherited;
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

constructor TMultinodeWellSelection.Create(Model: TBaseModel);
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
  PhastModel: TCustomModel;
begin
  PhastModel := FModel as TCustomModel;
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
  Mnw2Writer := TModflowMNW2_Writer.Create(FModel as TCustomModel, etDisplay);
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
  // if Assign is updated, update IsSame too.
  if Source is TSubPrintItem then
  begin
    SourceItem := TSubPrintItem(Source);
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
    Result := inherited;
    if Result then
    begin
      SourceItem := TSubPrintItem(AnotherItem);
      result := (PrintSubsidence = SourceItem.PrintSubsidence)
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
    end;
  end
  else
  begin
    result := False;
  end;
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

{ TSubPrintCollection }
constructor TSubPrintCollection.Create(Model: TBaseModel);
begin
  inherited Create(TSubPrintItem, Model);
end;

function TSubPrintCollection.GetItem(Index: integer): TSubPrintItem;
begin
  result := inherited Items[Index] as TSubPrintItem;
end;

procedure TSubPrintCollection.ReportErrors;
var
  Index: Integer;
  PrintChoice: TSubPrintItem;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(frmGoPhast.PhastModel, StrInTheSubsidencePa);
  for Index := 0 to Count -1 do
  begin
    PrintChoice := Items[Index];
    if PrintChoice.StartTime > PrintChoice.EndTime then
    begin
      frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel, StrInTheSubsidencePa,
        Format(StrStartingTime0g, [PrintChoice.StartTime, PrintChoice.EndTime]));
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

constructor TSubPackageSelection.Create(Model: TBaseModel);
begin
  inherited;
  FPrintChoices := TSubPrintCollection.Create(Model);
  FPrintFormats := TSubPrintFormats.Create(Model);
  InitializeVariables;
end;

destructor TSubPackageSelection.Destroy;
begin
  FPrintFormats.Free;
  FPrintChoices.Free;
  inherited;
end;

procedure TSubPackageSelection.InitializeVariables;
begin
  inherited;
  FNumberOfNodes := 10;
  FAccelerationParameter1 := 0;
  FAccelerationParameter2 := 1;
  FMinIterations := 5;
  FSaveDelayRestart := False;
  FReadDelayRestartFileName := '';
  FSubBinaryOutputChoice := sbocSingleFile;
  PrintFormats.InitializeVariables;
  PrintChoices.Clear;
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
var
  LayerStructure: TLayerStructure;
  LayerIndex: Integer;
  LayerGroup: TLayerGroup;
  Index: Integer;
  DelayItem: TSubDelayBedLayerItem;
begin
  if FReadDelayRestartFileName <> Value then
  begin
    FReadDelayRestartFileName := Value;
    InvalidateModel;
    if FModel <> nil then
    begin
      // ensure that related data sets have been created.
      LayerStructure := (FModel as TCustomModel).LayerStructure;
      for LayerIndex := 0 to LayerStructure.Count - 1 do
      begin
        LayerGroup := LayerStructure[LayerIndex];
        for Index := 0 to LayerGroup.SubDelayBedLayers.Count - 1 do
        begin
          DelayItem := LayerGroup.SubDelayBedLayers[Index];
          DelayItem.InterbedStartingHeadDataArrayName :=
            DelayItem.InterbedStartingHeadDataArrayName;
          DelayItem.InterbedPreconsolidationHeadDataArrayName :=
            DelayItem.InterbedPreconsolidationHeadDataArrayName;
        end;
      end;
    end;
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

procedure TSubPrintFormats.InitializeVariables;
begin
  SubsidenceFormat := 0;
  CompactionByModelLayerFormat := 0;
  CompactionByInterbedSystemFormat := 0;
  VerticalDisplacementFormat := 0;
  NoDelayPreconsolidationHeadFormat := 0;
  DelayPreconsolidationHeadFormat := 0;
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
  // if Assign is updated, update IsSame too.
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
  // if Assign is updated, update IsSame too.
  if Source is TCompositeZone then
  begin
    ZoneName := TCompositeZone(Source).ZoneName;
  end;
  inherited;
end;

constructor TCompositeZone.Create(Model: TBaseModel);
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
  // if Assign is updated, update IsSame too.
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

constructor TCompositeZoneCollection.Create(Model: TBaseModel);
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

constructor TZoneBudgetSelect.Create(Model: TBaseModel);
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
  inherited;
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

{ TSwtPrintFormats }

procedure TSwtPrintFormats.Assign(Source: TPersistent);
var
  SwtPrintSource: TSwtPrintFormats;
begin
  if Source is TSwtPrintFormats then
  begin
    SwtPrintSource := TSwtPrintFormats(Source);
    SubsidenceFormat := SwtPrintSource.SubsidenceFormat;
    CompactionByModelLayerFormat := SwtPrintSource.CompactionByModelLayerFormat;
    CompactionByInterbedSystemFormat :=
      SwtPrintSource.CompactionByInterbedSystemFormat;
    VerticalDisplacementFormat := SwtPrintSource.VerticalDisplacementFormat;
    PreconsolidationStress := SwtPrintSource.PreconsolidationStress;
    DeltaPreconsolidationStress := SwtPrintSource.DeltaPreconsolidationStress;
    GeostaticStress := SwtPrintSource.GeostaticStress;
    DeltaGeostaticStress := SwtPrintSource.DeltaGeostaticStress;
    EffectiveStress := SwtPrintSource.EffectiveStress;
    DeltaEffectiveStress := SwtPrintSource.DeltaEffectiveStress;
    VoidRatio := SwtPrintSource.VoidRatio;
    ThicknessCompressibleSediments := SwtPrintSource.ThicknessCompressibleSediments;
    LayerCenterElevation := SwtPrintSource.LayerCenterElevation;
  end
  else
  begin
    inherited;
  end;
end;

procedure TSwtPrintFormats.InitializeVariables;
begin
  SubsidenceFormat := 0;
  CompactionByModelLayerFormat := 0;
  CompactionByInterbedSystemFormat := 0;
  VerticalDisplacementFormat := 0;
  PreconsolidationStress := 0;
  DeltaPreconsolidationStress := 0;
  GeostaticStress := 0;
  DeltaGeostaticStress := 0;
  EffectiveStress := 0;
  DeltaEffectiveStress := 0;
  VoidRatio := 0;
  ThicknessCompressibleSediments := 0;
  LayerCenterElevation := 0;
end;

procedure TSwtPrintFormats.SetCompactionByInterbedSystemFormat(
  const Value: integer);
begin
  SetIntegerProperty(FCompactionByInterbedSystemFormat, Value);
end;

procedure TSwtPrintFormats.SetCompactionByModelLayerFormat(
  const Value: integer);
begin
  SetIntegerProperty(FCompactionByModelLayerFormat, Value);
end;

procedure TSwtPrintFormats.SetDeltaEffectiveStress(const Value: integer);
begin
  SetIntegerProperty(FDeltaEffectiveStress, Value);
end;

procedure TSwtPrintFormats.SetDeltaGeostaticStress(const Value: integer);
begin
  SetIntegerProperty(FDeltaGeostaticStress, Value);
end;

procedure TSwtPrintFormats.SetDeltaPreconsolidationStress(const Value: integer);
begin
  SetIntegerProperty(FDeltaPreconsolidationStress, Value);
end;

procedure TSwtPrintFormats.SetEffectiveStress(const Value: integer);
begin
  SetIntegerProperty(FEffectiveStress, Value);
end;

procedure TSwtPrintFormats.SetGeostaticStress(const Value: integer);
begin
  SetIntegerProperty(FGeostaticStress, Value);
end;

procedure TSwtPrintFormats.SetLayerCenterElevation(const Value: integer);
begin
  SetIntegerProperty(FLayerCenterElevation, Value);
end;

procedure TSwtPrintFormats.SetPreconsolidationStress(const Value: integer);
begin
  SetIntegerProperty(FPreconsolidationStress, Value);
end;

procedure TSwtPrintFormats.SetSubsidenceFormat(const Value: integer);
begin
  SetIntegerProperty(FSubsidenceFormat, Value);
end;

procedure TSwtPrintFormats.SetThicknessCompressibleSediments(
  const Value: integer);
begin
  SetIntegerProperty(FThicknessCompressibleSediments, Value);
end;

procedure TSwtPrintFormats.SetVerticalDisplacementFormat(const Value: integer);
begin
  SetIntegerProperty(FVerticalDisplacementFormat, Value);
end;

procedure TSwtPrintFormats.SetVoidRatio(const Value: integer);
begin
  SetIntegerProperty(FVoidRatio, Value);
end;

{ TSwtPrintItem }

procedure TSwtPrintItem.Assign(Source: TPersistent);
var
  SourceItem: TSwtPrintItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TSwtPrintItem then
  begin
    SourceItem := TSwtPrintItem(Source);
    PrintSubsidence := SourceItem.PrintSubsidence;
    SaveSubsidence := SourceItem.SaveSubsidence;
    PrintCompactionByModelLayer := SourceItem.PrintCompactionByModelLayer;
    SaveCompactionByModelLayer := SourceItem.SaveCompactionByModelLayer;
    PrintCompactionByInterbedSystem := SourceItem.PrintCompactionByInterbedSystem;
    SaveCompactionByInterbedSystem := SourceItem.SaveCompactionByInterbedSystem;
    PrintVerticalDisplacement := SourceItem.PrintVerticalDisplacement;
    SaveVerticalDisplacement := SourceItem.SaveVerticalDisplacement;
    PrintPreconsolidationStress := SourceItem.PrintPreconsolidationStress;
    SavePreconsolidationStress := SourceItem.SavePreconsolidationStress;
    PrintDeltaPreconsolidationStress := SourceItem.PrintDeltaPreconsolidationStress;
    SaveDeltaPreconsolidationStress := SourceItem.SaveDeltaPreconsolidationStress;
    PrintGeostaticStress := SourceItem.PrintGeostaticStress;
    SaveGeostaticStress := SourceItem.SaveGeostaticStress;
    PrintDeltaGeostaticStress := SourceItem.PrintDeltaGeostaticStress;
    SaveDeltaGeostaticStress := SourceItem.SaveDeltaGeostaticStress;
    PrintEffectiveStress := SourceItem.PrintEffectiveStress;
    SaveEffectiveStress := SourceItem.SaveEffectiveStress;
    PrintDeltaEffectiveStress := SourceItem.PrintDeltaEffectiveStress;
    SaveDeltaEffectiveStress := SourceItem.SaveDeltaEffectiveStress;
    PrintVoidRatio := SourceItem.PrintVoidRatio;
    SaveVoidRatio := SourceItem.SaveVoidRatio;
    PrintThicknessCompressibleSediments := SourceItem.PrintThicknessCompressibleSediments;
    SaveThicknessCompressibleSediments := SourceItem.SaveThicknessCompressibleSediments;
    PrintLayerCenterElevation := SourceItem.PrintLayerCenterElevation;
    SaveLayerCenterElevation := SourceItem.SaveLayerCenterElevation;
  end;
  inherited;
end;

function TSwtPrintItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SourceItem: TSwtPrintItem;
begin
  if AnotherItem is TSwtPrintItem then
  begin
    result := inherited;
    if result then
    begin

      SourceItem := TSwtPrintItem(AnotherItem);
      result := (PrintSubsidence = SourceItem.PrintSubsidence)
        and (SaveSubsidence = SourceItem.SaveSubsidence)
        and (PrintCompactionByModelLayer = SourceItem.PrintCompactionByModelLayer)
        and (SaveCompactionByModelLayer = SourceItem.SaveCompactionByModelLayer)
        and (PrintCompactionByInterbedSystem = SourceItem.PrintCompactionByInterbedSystem)
        and (SaveCompactionByInterbedSystem = SourceItem.SaveCompactionByInterbedSystem)
        and (PrintVerticalDisplacement = SourceItem.PrintVerticalDisplacement)
        and (SaveVerticalDisplacement = SourceItem.SaveVerticalDisplacement)
        and (PrintPreconsolidationStress = SourceItem.PrintPreconsolidationStress)
        and (SavePreconsolidationStress = SourceItem.SavePreconsolidationStress)
        and (PrintDeltaPreconsolidationStress = SourceItem.PrintDeltaPreconsolidationStress)
        and (SaveDeltaPreconsolidationStress = SourceItem.SaveDeltaPreconsolidationStress)
        and (PrintGeostaticStress = SourceItem.PrintGeostaticStress)
        and (SaveGeostaticStress = SourceItem.SaveGeostaticStress)
        and (PrintDeltaGeostaticStress = SourceItem.PrintDeltaGeostaticStress)
        and (SaveDeltaGeostaticStress = SourceItem.SaveDeltaGeostaticStress)
        and (PrintEffectiveStress = SourceItem.PrintEffectiveStress)
        and (SaveEffectiveStress = SourceItem.SaveEffectiveStress)
        and (PrintDeltaEffectiveStress = SourceItem.PrintDeltaEffectiveStress)
        and (SaveDeltaEffectiveStress = SourceItem.SaveDeltaEffectiveStress)
        and (PrintVoidRatio = SourceItem.PrintVoidRatio)
        and (SaveVoidRatio = SourceItem.SaveVoidRatio)
        and (PrintThicknessCompressibleSediments = SourceItem.PrintThicknessCompressibleSediments)
        and (SaveThicknessCompressibleSediments = SourceItem.SaveThicknessCompressibleSediments)
        and (PrintLayerCenterElevation = SourceItem.PrintLayerCenterElevation)
        and (SaveLayerCenterElevation = SourceItem.SaveLayerCenterElevation)
    end;
  end
  else
  begin
    result := False;
  end;
end;

procedure TSwtPrintItem.SetPrintCompactionByInterbedSystem(
  const Value: boolean);
begin
  SetBooleanProperty(FPrintCompactionByInterbedSystem, Value);
end;

procedure TSwtPrintItem.SetPrintCompactionByModelLayer(const Value: boolean);
begin
  SetBooleanProperty(FPrintCompactionByModelLayer, Value);
end;

procedure TSwtPrintItem.SetPrintDeltaEffectiveStress(const Value: boolean);
begin
  SetBooleanProperty(FPrintDeltaEffectiveStress, Value);
end;

procedure TSwtPrintItem.SetPrintDeltaGeostaticStress(const Value: boolean);
begin
  SetBooleanProperty(FPrintDeltaGeostaticStress, Value);
end;

procedure TSwtPrintItem.SetPrintDeltaPreconsolidationStress(
  const Value: boolean);
begin
  SetBooleanProperty(FPrintDeltaPreconsolidationStress, Value);
end;

procedure TSwtPrintItem.SetPrintEffectiveStress(const Value: boolean);
begin
  SetBooleanProperty(FPrintEffectiveStress, Value);
end;

procedure TSwtPrintItem.SetPrintGeostaticStress(const Value: boolean);
begin
  SetBooleanProperty(FPrintGeostaticStress, Value);
end;

procedure TSwtPrintItem.SetPrintLayerCenterElevation(const Value: boolean);
begin
  SetBooleanProperty(FPrintLayerCenterElevation, Value);
end;

procedure TSwtPrintItem.SetPrintPreconsolidationStress(const Value: boolean);
begin
  SetBooleanProperty(FPrintPreconsolidationStress, Value);
end;

procedure TSwtPrintItem.SetPrintSubsidence(const Value: boolean);
begin
  SetBooleanProperty(FPrintSubsidence, Value);
end;

procedure TSwtPrintItem.SetPrintThicknessCompressibleSediments(
  const Value: boolean);
begin
  SetBooleanProperty(FPrintThicknessCompressibleSediments, Value);
end;

procedure TSwtPrintItem.SetPrintVerticalDisplacement(const Value: boolean);
begin
  SetBooleanProperty(FPrintVerticalDisplacement, Value);
end;

procedure TSwtPrintItem.SetPrintVoidRatio(const Value: boolean);
begin
  SetBooleanProperty(FPrintVoidRatio, Value);
end;

procedure TSwtPrintItem.SetSaveCompactionByInterbedSystem(const Value: boolean);
begin
  SetBooleanProperty(FSaveCompactionByInterbedSystem, Value);
end;

procedure TSwtPrintItem.SetSaveCompactionByModelLayer(const Value: boolean);
begin
  SetBooleanProperty(FSaveCompactionByModelLayer, Value);
end;

procedure TSwtPrintItem.SetSaveDeltaEffectiveStress(const Value: boolean);
begin
  SetBooleanProperty(FSaveDeltaEffectiveStress, Value);
end;

procedure TSwtPrintItem.SetSaveDeltaGeostaticStress(const Value: boolean);
begin
  SetBooleanProperty(FSaveDeltaGeostaticStress, Value);
end;

procedure TSwtPrintItem.SetSaveDeltaPreconsolidationStress(
  const Value: boolean);
begin
  SetBooleanProperty(FSaveDeltaPreconsolidationStress, Value);
end;

procedure TSwtPrintItem.SetSaveEffectiveStress(const Value: boolean);
begin
  SetBooleanProperty(FSaveEffectiveStress, Value);
end;

procedure TSwtPrintItem.SetSaveGeostaticStress(const Value: boolean);
begin
  SetBooleanProperty(FSaveGeostaticStress, Value);
end;

procedure TSwtPrintItem.SetSaveLayerCenterElevation(const Value: boolean);
begin
  SetBooleanProperty(FSaveLayerCenterElevation, Value);
end;

procedure TSwtPrintItem.SetSavePreconsolidationStress(const Value: boolean);
begin
  SetBooleanProperty(FSavePreconsolidationStress, Value);
end;

procedure TSwtPrintItem.SetSaveSubsidence(const Value: boolean);
begin
  SetBooleanProperty(FSaveSubsidence, Value);
end;

procedure TSwtPrintItem.SetSaveThicknessCompressibleSediments(
  const Value: boolean);
begin
  SetBooleanProperty(FSaveThicknessCompressibleSediments, Value);
end;

procedure TSwtPrintItem.SetSaveVerticalDisplacement(const Value: boolean);
begin
  SetBooleanProperty(FSaveVerticalDisplacement, Value);
end;

procedure TSwtPrintItem.SetSaveVoidRatio(const Value: boolean);
begin
  SetBooleanProperty(FSaveVoidRatio, Value);
end;

{ TSwtPrintCollection }

constructor TSwtPrintCollection.Create(Model: TBaseModel);
begin
  inherited Create(TSwtPrintItem, Model);
end;

function TSwtPrintCollection.GetItem(Index: integer): TSwtPrintItem;
begin
  result := inherited Items[Index] as TSwtPrintItem;
end;

procedure TSwtPrintCollection.ReportErrors;
var
  Index: Integer;
  PrintChoice: TSwtPrintItem;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(frmGoPhast.PhastModel, StrInTheSubsidenceAn);
  for Index := 0 to Count -1 do
  begin
    PrintChoice := Items[Index];
    if PrintChoice.StartTime > PrintChoice.EndTime then
    begin
      frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel, StrInTheSubsidenceAn,
        Format(StrStartingTime0g,
          [PrintChoice.StartTime, PrintChoice.EndTime]));
    end;
  end;
end;

procedure TSwtPrintCollection.SetItem(Index: integer;
  const Value: TSwtPrintItem);
begin
  inherited Items[Index] := Value;
end;

{ TSwtInitialPrint }

procedure TSwtInitialPrint.Assign(Source: TPersistent);
var
  SwtInitialPrintSource: TSwtInitialPrint;
begin
  if Source is TSwtInitialPrint then
  begin
    SwtInitialPrintSource := TSwtInitialPrint(Source);
    PrintInitialLayerCenterElevations :=
      SwtInitialPrintSource.PrintInitialLayerCenterElevations;
    InitialLayerCenterElevationFormat :=
      SwtInitialPrintSource.InitialLayerCenterElevationFormat;
    PrintInitialGeostaticStress :=
      SwtInitialPrintSource.PrintInitialGeostaticStress;
    InitialGeostaticStressFormat :=
      SwtInitialPrintSource.InitialGeostaticStressFormat;
    PrintInitialEffectiveStress :=
      SwtInitialPrintSource.PrintInitialEffectiveStress;
    InitialEffectiveStressFormat :=
      SwtInitialPrintSource.InitialEffectiveStressFormat;
    PrintInitialPreconsolidationStress :=
      SwtInitialPrintSource.PrintInitialPreconsolidationStress;
    InitialPreconsolidationStressFormat :=
      SwtInitialPrintSource.InitialPreconsolidationStressFormat;
    PrintInitialEquivalentStorageProperties :=
      SwtInitialPrintSource.PrintInitialEquivalentStorageProperties;
    InitialEquivalentStoragePropertiesFormat :=
      SwtInitialPrintSource.InitialEquivalentStoragePropertiesFormat;
  end
  else
  begin
    inherited;
  end;
end;

procedure TSwtInitialPrint.InitializeVariables;
begin
  PrintInitialLayerCenterElevations := False;
  InitialLayerCenterElevationFormat := 0;

  PrintInitialGeostaticStress := False;
  InitialGeostaticStressFormat := 0;

  PrintInitialEffectiveStress := False;
  InitialEffectiveStressFormat := 0;

  PrintInitialPreconsolidationStress := False;
  InitialPreconsolidationStressFormat := 0;

  PrintInitialEquivalentStorageProperties := False;
  InitialEquivalentStoragePropertiesFormat := 0;
end;

procedure TSwtInitialPrint.SetInitialEffectiveStressFormat(
  const Value: integer);
begin
  SetIntegerProperty(FInitialEffectiveStressFormat, Value);
end;

procedure TSwtInitialPrint.SetInitialEquivalentStoragePropertiesFormat(
  const Value: integer);
begin
  SetIntegerProperty(FInitialEquivalentStoragePropertiesFormat, Value);
end;

procedure TSwtInitialPrint.SetInitialGeostaticStressFormat(
  const Value: integer);
begin
  SetIntegerProperty(FInitialGeostaticStressFormat, Value);
end;

procedure TSwtInitialPrint.SetInitialLayerCenterElevationFormat(
  const Value: integer);
begin
  SetIntegerProperty(FInitialLayerCenterElevationFormat, Value);
end;

procedure TSwtInitialPrint.SetInitialPreconsolidationStressFormat(
  const Value: integer);
begin
  SetIntegerProperty(FInitialPreconsolidationStressFormat, Value);
end;

procedure TSwtInitialPrint.SetPrintInitialEffectiveStress(const Value: boolean);
begin
  SetBooleanProperty(FPrintInitialEffectiveStress, Value);
end;

procedure TSwtInitialPrint.SetPrintInitialEquivalentStorageProperties(
  const Value: boolean);
begin
  SetBooleanProperty(FPrintInitialEquivalentStorageProperties, Value);
end;

procedure TSwtInitialPrint.SetPrintInitialGeostaticStress(const Value: boolean);
begin
  SetBooleanProperty(FPrintInitialGeostaticStress, Value);
end;

procedure TSwtInitialPrint.SetPrintInitialLayerCenterElevations(
  const Value: boolean);
begin
  SetBooleanProperty(FPrintInitialLayerCenterElevations, Value);
end;

procedure TSwtInitialPrint.SetPrintInitialPreconsolidationStress(
  const Value: boolean);
begin
  SetBooleanProperty(FPrintInitialPreconsolidationStress, Value);
end;

{ TSwtPackageSelection }

procedure TSwtPackageSelection.Assign(Source: TPersistent);
var
  SwtSource: TSwtPackageSelection;
begin
  if Source is TSwtPackageSelection then
  begin
    SwtSource := TSwtPackageSelection(Source);
    ThickResponse := SwtSource.ThickResponse;
    VoidRatioResponse := SwtSource.VoidRatioResponse;
    PreconsolidationSource := SwtSource.PreconsolidationSource;
    CompressionSource := SwtSource.CompressionSource;
    PrintFormats := SwtSource.PrintFormats;
    PrintChoices := SwtSource.PrintChoices;
    InitialPrint := SwtSource.InitialPrint;
    BinaryOutputChoice := SwtSource.BinaryOutputChoice;
  end;
  inherited;
end;

constructor TSwtPackageSelection.Create(Model: TBaseModel);
begin
  inherited;
  FPrintChoices := TSwtPrintCollection.Create(Model);
  FInitialPrint := TSwtInitialPrint.Create(Model);
  FPrintFormats := TSwtPrintFormats.Create(Model);
  InitializeVariables;
end;

destructor TSwtPackageSelection.Destroy;
begin
  FPrintFormats.Free;
  FInitialPrint.Free;
  FPrintChoices.Free;
  inherited;
end;

procedure TSwtPackageSelection.InitializeVariables;
begin
  inherited;
  FThickResponse := trConstant;
  FCompressionSource := csSpecificStorage;
  FPreconsolidationSource := pcSpecified;
  FVoidRatioResponse := vrrConstant;
  FSubBinaryOutputChoice := sbocSingleFile;
  PrintFormats.InitializeVariables;
  PrintChoices.Clear;
  InitialPrint.InitializeVariables;
end;

procedure TSwtPackageSelection.SetCompressionSource(
  const Value: TCompressionSource);
var
  LayerStructure: TLayerStructure;
  LayerIndex: Integer;
  LayerGroup: TLayerGroup;
  Index: Integer;
  WtItem: TSwtWaterTableItem;
begin
  if FCompressionSource <> Value then
  begin
    FCompressionSource := Value;
    InvalidateModel;
    if FModel <> nil then
    begin
      // ensure that related data sets have been created.
      LayerStructure := (FModel as TCustomModel).LayerStructure;
      if LayerStructure <> nil then
      begin
        for LayerIndex := 0 to LayerStructure.Count - 1 do
        begin
          LayerGroup := LayerStructure[LayerIndex];
          for Index := 0 to LayerGroup.WaterTableLayers.Count - 1 do
          begin
            WtItem := LayerGroup.WaterTableLayers[Index];
            WtItem.WaterTableInitialElasticSkeletalSpecificStorageDataArrayName :=
              WtItem.WaterTableInitialElasticSkeletalSpecificStorageDataArrayName;
            WtItem.WaterTableInitialInelasticSkeletalSpecificStorageDataArrayName :=
              WtItem.WaterTableInitialInelasticSkeletalSpecificStorageDataArrayName;
            WtItem.WaterTableRecompressionIndexDataArrayName :=
              WtItem.WaterTableRecompressionIndexDataArrayName;
            WtItem.WaterTableCompressionIndexDataArrayName :=
              WtItem.WaterTableCompressionIndexDataArrayName;
          end;
        end;
      end;
    end;
  end;
end;

procedure TSwtPackageSelection.SetInitialPrint(const Value: TSwtInitialPrint);
begin
  FInitialPrint.Assign(Value);
end;

procedure TSwtPackageSelection.SetPreconsolidationSource(
  const Value: TPreconsolidationSource);
begin
  if FPreconsolidationSource <> Value then
  begin
    FPreconsolidationSource := Value;
    InvalidateModel;
  end;
end;

procedure TSwtPackageSelection.SetPrintChoices(
  const Value: TSwtPrintCollection);
begin
  FPrintChoices.Assign(Value);
end;

procedure TSwtPackageSelection.SetPrintFormats(const Value: TSwtPrintFormats);
begin
  FPrintFormats.Assign(Value);
end;

procedure TSwtPackageSelection.SetSubBinaryOutputChoice(
  const Value: TSubBinaryOutputChoice);
begin
  if FSubBinaryOutputChoice <> Value then
  begin
    FSubBinaryOutputChoice := Value;
    InvalidateModel;
  end;
end;

procedure TSwtPackageSelection.SetThickResponse(const Value: TThickResponse);
begin
  if FThickResponse <> Value then
  begin
    FThickResponse := Value;
    InvalidateModel;
  end;
end;

procedure TSwtPackageSelection.SetVoidRatioResponse(
  const Value: TVoidRatioResponse);
begin
  if FVoidRatioResponse <> Value then
  begin
    FVoidRatioResponse := Value;
    InvalidateModel;
  end;
end;

procedure TCustomPrintItem.SetStartTime(const Value: double);
begin
  SetRealProperty(FStartTime, Value);
end;

procedure TCustomPrintItem.Assign(Source: TPersistent);
var
  SourceItem: TCustomPrintItem;
begin
  inherited;
  // if Assign is updated, update IsSame too.
  if Source is TCustomPrintItem then
  begin
    SourceItem := TCustomPrintItem(Source);
    StartTime := SourceItem.StartTime;
    EndTime := SourceItem.EndTime;
  end;
end;

function TCustomPrintItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SourceItem: TCustomPrintItem;
begin
  Result := false;
  if AnotherItem is TCustomPrintItem then
  begin
    SourceItem := TCustomPrintItem(AnotherItem);
    result := (StartTime = SourceItem.StartTime)
      and (EndTime = SourceItem.EndTime)
  end;
end;

procedure TCustomPrintItem.SetEndTime(const Value: double);
begin
  SetRealProperty(FEndTime, Value);
end;

{ THydPackageSelection }

procedure THydPackageSelection.Assign(Source: TPersistent);
begin
  if Source is THydPackageSelection then
  begin
    StoredHYDNOH := THydPackageSelection(Source).StoredHYDNOH;
  end;
  inherited;
end;

constructor THydPackageSelection.Create(Model: TBaseModel);
begin
  inherited;
  FStoredHYDNOH := TRealStorage.Create;
  InitializeVariables;
end;

destructor THydPackageSelection.Destroy;
begin
  FStoredHYDNOH.Free;
  inherited;
end;

procedure THydPackageSelection.InitializeVariables;
begin
  inherited;
  HYDNOH := -1E+20;
end;

function THydPackageSelection.GetHYDNOH: double;
begin
  result := StoredHYDNOH.Value;
end;

procedure THydPackageSelection.SetHYDNOH(const Value: double);
begin
  if StoredHYDNOH.Value <> Value then
  begin
    StoredHYDNOH.Value := Value;
    InvalidateModel;
  end;
end;

procedure THydPackageSelection.SetStoredHYDNOH(const Value: TRealStorage);
begin
  if FStoredHYDNOH.Value <> Value.Value then
  begin
    FStoredHYDNOH.Assign(Value);
    InvalidateModel;
  end;
end;

{ TUpwPackageSelection }

procedure TUpwPackageSelection.Assign(Source: TPersistent);
begin
  if Source is TUpwPackageSelection then
  begin
    HDryPrintOption := TUpwPackageSelection(Source).HDryPrintOption;
  end;
  inherited;

end;

constructor TUpwPackageSelection.Create(Model: TBaseModel);
begin
  inherited;
  InitializeVariables;
end;

destructor TUpwPackageSelection.Destroy;
begin
  inherited;
end;

procedure TUpwPackageSelection.InitializeVariables;
begin
  inherited;
  FHDryPrintOption := hpoPrintHdry;
end;

procedure TUpwPackageSelection.SetHDryPrintOption(
  const Value: THDryPrintOption);
begin
  if FHDryPrintOption <> Value then
  begin
    InvalidateModel;
    FHDryPrintOption := Value;
  end;
end;

{ TNwtPackageSelection }

procedure TNwtPackageSelection.Assign(Source: TPersistent);
var
  SourceNwt: TNwtPackageSelection;
begin
  if Source is TNwtPackageSelection then
  begin
    SourceNwt := TNwtPackageSelection(Source);
    HeadTolerance := SourceNwt.HeadTolerance;
    FluxTolerance := SourceNwt.FluxTolerance;
    MaxOuterIterations := SourceNwt.MaxOuterIterations;
    ThicknessFactor := SourceNwt.ThicknessFactor;
    SolverMethod := SourceNwt.SolverMethod;
    PrintFlag := SourceNwt.PrintFlag;
    CorrectForCellBottom := SourceNwt.CorrectForCellBottom;
    Option := SourceNwt.Option;
    DBDTheta := SourceNwt.DBDTheta;
    DBDKappa := SourceNwt.DBDKappa;
    DBDGamma := SourceNwt.DBDGamma;
    MomementumCoefficient := SourceNwt.MomementumCoefficient;
    BackFlag := SourceNwt.BackFlag;
    MaxBackIterations := SourceNwt.MaxBackIterations;
    BackTol := SourceNwt.BackTol;
    BackReduce := SourceNwt.BackReduce;
    MaxIterInner := SourceNwt.MaxIterInner;
    IluMethod := SourceNwt.IluMethod;
    FillLimit := SourceNwt.FillLimit;
    FillLevel := SourceNwt.FillLevel;
    StopTolerance := SourceNwt.StopTolerance;
    MaxGmresRestarts := SourceNwt.MaxGmresRestarts;
    AccelMethod := SourceNwt.AccelMethod;
    OrderingMethod := SourceNwt.OrderingMethod;
    Level := SourceNwt.Level;
    NumberOfOrthogonalizations := SourceNwt.NumberOfOrthogonalizations;
    ApplyReducedPrecondition := SourceNwt.ApplyReducedPrecondition;
    ResidReducConv := SourceNwt.ResidReducConv;
    UseDropTolerance := SourceNwt.UseDropTolerance;
    DropTolerancePreconditioning := SourceNwt.DropTolerancePreconditioning;
    InnerHeadClosureCriterion := SourceNwt.InnerHeadClosureCriterion;
    MaxInnerIterations := SourceNwt.MaxInnerIterations;
    ContinueNWT := SourceNwt.ContinueNWT;
  end;
  inherited;
end;

constructor TNwtPackageSelection.Create(Model: TBaseModel);
begin
  inherited;
  FStopTolerance := TRealStorage.Create;
  FMomementumCoefficient := TRealStorage.Create;
  FDropTolerancePreconditioning := TRealStorage.Create;
  FDBDKappa := TRealStorage.Create;
  FInnerHeadClosureCriterion := TRealStorage.Create;
  FDBDGamma := TRealStorage.Create;
  FFluxTolerance := TRealStorage.Create;
  FResidReducConv := TRealStorage.Create;
  FDBDTheta := TRealStorage.Create;
  FHeadTolerance := TRealStorage.Create;
  FThicknessFactor := TRealStorage.Create;
  FBackReduce := TRealStorage.Create;
  FBackTol := TRealStorage.Create;
  InitializeVariables;
end;

destructor TNwtPackageSelection.Destroy;
begin
  FBackTol.Free;
  FBackReduce.Free;
  FThicknessFactor.Free;
  FHeadTolerance.Free;
  FDBDTheta.Free;
  FResidReducConv.Free;
  FFluxTolerance.Free;
  FDBDGamma.Free;
  FInnerHeadClosureCriterion.Free;
  FDBDKappa.Free;
  FDropTolerancePreconditioning.Free;
  FMomementumCoefficient.Free;
  FStopTolerance.Free;
  inherited;
end;

procedure TNwtPackageSelection.InitializeVariables;
begin
  inherited;
  HeadTolerance.Value := 1e-4;
  // The recomended value is 500 with units of m and days
  // The default units in ModelMuse are m and seconds
  // 0.006 is approximately equal to 500/24/3600
  FluxTolerance.Value := 0.06;
  MaxOuterIterations := 100;
  ThicknessFactor.Value := 0.00001;
  SolverMethod := nsmChiMD;
  PrintFlag := 1;
  CorrectForCellBottom := 0;
  Option := noSimple;
  DBDTheta.Value := 0.7;
  DBDKappa.Value := 0.0001;
  DBDGamma.Value := 0;
  MomementumCoefficient.Value := 0.1;
  BackFlag := 0;
  MaxBackIterations := 50;
  BackTol.Value := 1.5;
  BackReduce.Value := 0.9;
  MaxIterInner := 50;
  IluMethod := nimKOrder;
  FillLimit := 7;
  FillLevel := 1;
  StopTolerance.Value := 1e-10;
  MaxGmresRestarts := 10;
  AccelMethod := namBiCgstab;
  OrderingMethod := nomMinimumOrdering;
  Level := 1;
  NumberOfOrthogonalizations := 2;
  ApplyReducedPrecondition := narpDontApply;
  ResidReducConv.Value := 0;
  UseDropTolerance := nudtUse;
  DropTolerancePreconditioning.Value := 1e-3;
  InnerHeadClosureCriterion.Value := 1e-4;
  MaxInnerIterations := 50;
end;

procedure TNwtPackageSelection.SetAccelMethod(const Value: TNewtonAccelMethod);
begin
  if FAccelMethod <> Value then
  begin
    FAccelMethod := Value;
    InvalidateModel;
  end;
end;

procedure TNwtPackageSelection.SetApplyReducedPrecondition(
  const Value: TNewtonApplyReducedPrecondition);
begin
  if FApplyReducedPrecondition <> Value then
  begin
    FApplyReducedPrecondition := Value;
    InvalidateModel;
  end;
end;

procedure TNwtPackageSelection.SetBackFlag(const Value: integer);
begin
  SetIntegerProperty(FBackFlag , Value);
end;

procedure TNwtPackageSelection.SetBackReduce(const Value: TRealStorage);
begin
  SetRealProperty(FBackReduce , Value);
end;

procedure TNwtPackageSelection.SetBackTol(const Value: TRealStorage);
begin
  SetRealProperty(FBackTol , Value);
end;

procedure TNwtPackageSelection.SetBooleanProperty(var Field: Boolean;
  NewValue: Boolean);
begin
  if Field <> NewValue then
  begin
    Field := NewValue;
    InvalidateModel;
  end;
end;

procedure TNwtPackageSelection.SetCorrectForCellBottom(const Value: integer);
begin
  SetIntegerProperty(FCorrectForCellBottom , Value);
end;

procedure TNwtPackageSelection.SetDBDGamma(const Value: TRealStorage);
begin
  SetRealProperty(FDBDGamma , Value);
end;

procedure TNwtPackageSelection.SetDBDKappa(const Value: TRealStorage);
begin
  SetRealProperty(FDBDKappa , Value);
end;

procedure TNwtPackageSelection.SetDBDTheta(const Value: TRealStorage);
begin
  SetRealProperty(FDBDTheta , Value);
end;

procedure TNwtPackageSelection.SetDropTolerancePreconditioning(
  const Value: TRealStorage);
begin
  SetRealProperty(FDropTolerancePreconditioning , Value);
end;

procedure TNwtPackageSelection.SetFillLevel(const Value: integer);
begin
  SetIntegerProperty(FFillLevel , Value);
end;

procedure TNwtPackageSelection.SetFillLimit(const Value: integer);
begin
  SetIntegerProperty(FFillLimit , Value);
end;

procedure TNwtPackageSelection.SetFluxTolerance(const Value: TRealStorage);
begin
  SetRealProperty(FFluxTolerance , Value);
end;

procedure TNwtPackageSelection.SetContinueNWT(const Value: Boolean);
begin
  SetBooleanProperty(FContinueNWT, Value);
end;

procedure TNwtPackageSelection.SetHeadTolerance(const Value: TRealStorage);
begin
  SetRealProperty(FHeadTolerance , Value);
end;

procedure TNwtPackageSelection.SetIluMethod(const Value: TNewtonIluMethod);
begin
  if FIluMethod <> Value then
  begin
    FIluMethod := Value;
    InvalidateModel;
  end;
end;

procedure TNwtPackageSelection.SetInnerHeadClosureCriterion(
  const Value: TRealStorage);
begin
  SetRealProperty(FInnerHeadClosureCriterion , Value);
end;

procedure TNwtPackageSelection.SetIntegerProperty(var Field: integer;
  NewValue: integer);
begin
  if Field <> NewValue then
  begin
    Field := NewValue;
    InvalidateModel;
  end;
end;

procedure TNwtPackageSelection.SetLevel(const Value: integer);
begin
  SetIntegerProperty(FLevel , Value);
end;

procedure TNwtPackageSelection.SetMaxBackIterations(const Value: integer);
begin
  SetIntegerProperty(FMaxBackIterations , Value);
end;

procedure TNwtPackageSelection.SetMaxGmresRestarts(const Value: integer);
begin
  SetIntegerProperty(FMaxGmresRestarts , Value);
end;

procedure TNwtPackageSelection.SetMaxInnerIterations(const Value: integer);
begin
  SetIntegerProperty(FMaxInnerIterations , Value);
end;

procedure TNwtPackageSelection.SetMaxIterInner(const Value: integer);
begin
  SetIntegerProperty(FMaxIterInner , Value);
end;

procedure TNwtPackageSelection.SetMaxOuterIterations(const Value: integer);
begin
  SetIntegerProperty(FMaxOuterIterations , Value);
end;

procedure TNwtPackageSelection.SetMomementumCoefficient(
  const Value: TRealStorage);
begin
  SetRealProperty(FMomementumCoefficient , Value);
end;

procedure TNwtPackageSelection.SetNumberOfOrthogonalizations(
  const Value: integer);
begin
  SetIntegerProperty(FNumberOfOrthogonalizations , Value);
end;

procedure TNwtPackageSelection.SetOption(const Value: TNewtonOption);
begin
  if FOption <> Value then
  begin
    FOption := Value;
    InvalidateModel;
  end;
end;

procedure TNwtPackageSelection.SetOrderingMethod(
  const Value: TNewtonOrderingMethod);
begin
  if FOrderingMethod <> Value then
  begin
    FOrderingMethod := Value;
    InvalidateModel;
  end;
end;

procedure TNwtPackageSelection.SetPrintFlag(const Value: integer);
begin
  SetIntegerProperty(FPrintFlag , Value);
end;

procedure TNwtPackageSelection.SetRealProperty(Field, NewValue: TRealStorage);
begin
  if Field.Value <> NewValue.Value then
  begin
    Field.Assign(NewValue);
    InvalidateModel;
  end;
end;

procedure TNwtPackageSelection.SetResidReducConv(const Value: TRealStorage);
begin
  SetRealProperty(FResidReducConv , Value);
end;

procedure TNwtPackageSelection.SetSolverMethod(
  const Value: TNewtonSolverMethod);
begin
  if FSolverMethod <> Value then
  begin
    FSolverMethod := Value;
    InvalidateModel;
  end;
end;

procedure TNwtPackageSelection.SetStopTolerance(const Value: TRealStorage);
begin
  SetRealProperty(FStopTolerance , Value);
end;

procedure TNwtPackageSelection.SetThicknessFactor(const Value: TRealStorage);
begin
  SetRealProperty(FThicknessFactor , Value);
end;

procedure TNwtPackageSelection.SetUseDropTolerance(
  const Value: TNewtonUseDropTolerance);
begin
  if FUseDropTolerance <> Value then
  begin
    FUseDropTolerance := Value;
    InvalidateModel;
  end;
end;

procedure TCustomTransientLayerPackageSelection.SetMultiplierArrayNames
  (const Value: TTransientMultCollection);
begin
  FMultiplierArrayNames.Assign(Value);
end;

procedure TCustomTransientLayerPackageSelection.SetZoneArrayNames
  (const Value: TTransientZoneCollection);
begin
  FZoneArrayNames.Assign(Value);
end;

{ TCustomTransientArrayItem }

procedure TCustomTransientArrayItem.Assign(Source: TPersistent);
var
  TransSource: TCustomTransientArrayItem;
begin
  if Source is TCustomTransientArrayItem then
  begin
    TransSource := TCustomTransientArrayItem(Source);
    ArrayName := TransSource.ArrayName;
    FileName := TransSource.FileName;
    Uniform := TransSource.Uniform;
  end
  else
  begin
    inherited;
  end;
end;

{ TTransientMultItem }

procedure TTransientMultItem.Assign(Source: TPersistent);
var
  TransSource: TTransientMultItem;
begin
  if Source is TTransientMultItem then
  begin
    TransSource := TTransientMultItem(Source);
    UniformValue := TransSource.UniformValue
  end;
  inherited;
end;

{ TTransientZoneItem }

procedure TTransientZoneItem.Assign(Source: TPersistent);
var
  TransSource: TTransientZoneItem;
begin
  if Source is TTransientZoneItem then
  begin
    TransSource := TTransientZoneItem(Source);
    UniformValue := TransSource.UniformValue
  end;
  inherited;
end;

{ TTransientMultCollection }

function TTransientMultCollection.Add: TTransientMultItem;
begin
  result := inherited Add as TTransientMultItem;
end;

constructor TTransientMultCollection.Create;
begin
  inherited Create(TTransientMultItem);
end;

function TTransientMultCollection.GetItem(Index: integer): TTransientMultItem;
begin
  result := inherited Items[Index] as TTransientMultItem
end;

procedure TTransientMultCollection.SetItem(Index: integer;
  const Value: TTransientMultItem);
begin
  inherited Items[Index] := Value;
end;

{ TTransientZoneCollection }

function TTransientZoneCollection.Add: TTransientZoneItem;
begin
  result := inherited Add as TTransientZoneItem;
end;

constructor TTransientZoneCollection.Create;
begin
  inherited Create(TTransientZoneItem);
end;

function TTransientZoneCollection.GetItem(Index: integer): TTransientZoneItem;
begin
  result := inherited Items[Index] as TTransientZoneItem;
end;

procedure TTransientZoneCollection.SetItem(Index: integer;
  const Value: TTransientZoneItem);
begin
  inherited Items[Index] := Value;
end;

{ TMt3dBasic }

procedure TMt3dBasic.Assign(Source: TPersistent);
var
  BasicSource: TMt3dBasic;
begin
  if Source is TMt3dBasic then
  begin
    BasicSource := TMt3dBasic(Source);
    StoredMinimumSaturatedFraction :=
      BasicSource.StoredMinimumSaturatedFraction;
    StoredInactiveConcentration := BasicSource.StoredInactiveConcentration;
    StoredMassUnit := BasicSource.StoredMassUnit;
  end;
  inherited;
end;

procedure TMt3dBasic.Changed(Sender: TObject);
begin
  InvalidateModel;
end;

constructor TMt3dBasic.Create(Model: TBaseModel);
begin
  inherited;
  FMinimumSaturatedFraction := TRealStorage.Create;
  FMinimumSaturatedFraction.OnChange := Changed;

  FInactiveConcentration := TRealStorage.Create;
  FInactiveConcentration.OnChange := Changed;

  FMassUnit := TStringStorage.Create;
  FMassUnit.OnChange := Changed;

  InitializeVariables;
end;

destructor TMt3dBasic.Destroy;
begin
  FMassUnit.Free;
  FInactiveConcentration.Free;
  FMinimumSaturatedFraction.Free;
  inherited;
end;

function TMt3dBasic.GetInactiveConcentration: double;
begin
  result := StoredInactiveConcentration.Value;
end;

function TMt3dBasic.GetMassUnit: string;
begin
  result := StoredMassUnit.Value;
end;

function TMt3dBasic.GetMinimumSaturatedFraction: double;
begin
  result := StoredMinimumSaturatedFraction.Value;
end;

procedure TMt3dBasic.InitializeVariables;
begin
  inherited;
  IsSelected := False;
  MassUnit := 'g';
  InactiveConcentration := -1e30;
  MinimumSaturatedFraction := 0.01;
end;

procedure TMt3dBasic.SetInactiveConcentration(const Value: double);
begin
  StoredInactiveConcentration.Value := Value;
end;

procedure TMt3dBasic.SetIsSelected(const Value: boolean);
begin
  inherited;
  if FModel <> nil then
  begin
    (FModel as TCustomModel).DataArrayManager.UpdateClassifications;
  end;
  UpdateDataSets;
end;

procedure TMt3dBasic.SetStoredInactiveConcentration(const Value: TRealStorage);
begin
  FInactiveConcentration.Assign(Value)
end;

procedure TMt3dBasic.SetStoredMassUnit(const Value: TStringStorage);
begin
  FMassUnit.Assign(Value)
end;

procedure TMt3dBasic.SetStoredMinimumSaturatedFraction(const Value: TRealStorage);
begin
  FMinimumSaturatedFraction.Assign(Value)
end;

procedure TMt3dBasic.UpdateDataSets;
var
  LocalModel: TCustomModel;
begin
  LocalModel := FModel as TCustomModel;
  if (LocalModel <> nil) and IsSelected then
  begin
    LocalModel.UpdateMt3dmsChemDataSets
//    MobileComponents.UpdateDataArrays;
//    ImMobileComponents.UpdateDataArrays;
    // update diffusion data sets.
  end;
end;

procedure TMt3dBasic.SetMassUnit(const Value: string);
begin
  StoredMassUnit.Value := Value;
end;

procedure TMt3dBasic.SetMinimumSaturatedFraction(const Value: double);
begin
  StoredMinimumSaturatedFraction.Value := Value;
end;

{ TMt3dmsGCGSolverPackage }

procedure TMt3dmsGCGSolverPackage.Assign(Source: TPersistent);
var
  SourceSolver: TMt3dmsGCGSolverPackage;
begin
  if Source is TMt3dmsGCGSolverPackage then
  begin
    SourceSolver := TMt3dmsGCGSolverPackage(Source);
    RelaxationFactor := SourceSolver.RelaxationFactor;
    ConvergenceCriterion := SourceSolver.ConvergenceCriterion;
    MaxOuterIterations := SourceSolver.MaxOuterIterations;
    MaxInnerIterations := SourceSolver.MaxInnerIterations;
    PreconditionerChoice := SourceSolver.PreconditionerChoice;
    DispersionTensorChoice := SourceSolver.DispersionTensorChoice;
    PrintoutInterval := SourceSolver.PrintoutInterval;
  end;
  inherited;

end;

procedure TMt3dmsGCGSolverPackage.Changed;
begin
  InvalidateModel;
end;

constructor TMt3dmsGCGSolverPackage.Create(Model: TBaseModel);
begin
  inherited;
  FStoredRelaxationFactor := TRealStorage.Create;
  FStoredRelaxationFactor.OnChange := Changed;
  FStoredConvergenceCriterion := TRealStorage.Create;
  FStoredConvergenceCriterion.OnChange := Changed;
  InitializeVariables;
end;

destructor TMt3dmsGCGSolverPackage.Destroy;
begin
  FStoredRelaxationFactor.Free;
  FStoredConvergenceCriterion.Free;
  inherited;
end;

function TMt3dmsGCGSolverPackage.GetConvergenceCriterion: double;
begin
  result := StoredConvergenceCriterion.Value;
end;

function TMt3dmsGCGSolverPackage.GetRelaxationFactor: double;
begin
  result := StoredRelaxationFactor.Value;
end;

procedure TMt3dmsGCGSolverPackage.InitializeVariables;
begin
  inherited;
  IsSelected := False;
  FStoredRelaxationFactor.Value := 1;
  FStoredConvergenceCriterion.Value := 1e-6;
  FMaxOuterIterations := 1;
  FMaxInnerIterations := 200;
  FPreconditionerChoice := gpCholesky;
  FDispersionTensorChoice := dtcLump;
  FPrintoutInterval := 1;
end;

procedure TMt3dmsGCGSolverPackage.SetConvergenceCriterion(const Value: double);
begin
  StoredConvergenceCriterion.Value := Value;
end;

procedure TMt3dmsGCGSolverPackage.SetDispersionTensorChoice(
  const Value: TDispersionTensorTreatment);
begin
  if FDispersionTensorChoice <> Value then
  begin
    FDispersionTensorChoice := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsGCGSolverPackage.SetMaxInnerIterations(const Value: integer);
begin
  if FMaxInnerIterations <> Value then
  begin
    FMaxInnerIterations := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsGCGSolverPackage.SetMaxOuterIterations(const Value: integer);
begin
  if FMaxOuterIterations <> Value then
  begin
    FMaxOuterIterations := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsGCGSolverPackage.SetPreconditionerChoice(
  const Value: TGcgPreconditioner);
begin
  if FPreconditionerChoice <> Value then
  begin
    FPreconditionerChoice := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsGCGSolverPackage.SetPrintoutInterval(const Value: integer);
begin
  if FPrintoutInterval <> Value then
  begin
    FPrintoutInterval := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsGCGSolverPackage.SetRelaxationFactor(const Value: double);
begin
  StoredRelaxationFactor.Value := Value;
end;

procedure TMt3dmsGCGSolverPackage.SetStoredConvergenceCriterion(
  const Value: TRealStorage);
begin
  FStoredConvergenceCriterion.Assign(Value);
end;

procedure TMt3dmsGCGSolverPackage.SetStoredRelaxationFactor(
  const Value: TRealStorage);
begin
  FStoredRelaxationFactor.Assign(Value);
end;

{ TMt3dmsAdvection }

procedure TMt3dmsAdvection.Assign(Source: TPersistent);
var
  AdvSource: TMt3dmsAdvection;
begin
  if Source is TMt3dmsAdvection then
  begin
    AdvSource := TMt3dmsAdvection(Source);
    Courant := AdvSource.Courant;
    ConcWeight := AdvSource.ConcWeight;
    RelCelConcGrad := AdvSource.RelCelConcGrad;
    CriticalConcGradient := AdvSource.CriticalConcGradient;
    AdvectionSolution := AdvSource.AdvectionSolution;
    MaximumParticles := AdvSource.MaximumParticles;
    WeightingScheme := AdvSource.WeightingScheme;
    ParticleTrackMethod := AdvSource.ParticleTrackMethod;
    StoredConcWeight := AdvSource.StoredConcWeight;
    StoredRelCelConcGrad := AdvSource.StoredRelCelConcGrad;
    ParticlePlacementMethod := AdvSource.ParticlePlacementMethod;
    NumberOfParticlePlanes := AdvSource.NumberOfParticlePlanes;
    LowGradientParticleCount := AdvSource.LowGradientParticleCount;
    HighGradientParticleCount := AdvSource.HighGradientParticleCount;
    MinParticlePerCell := AdvSource.MinParticlePerCell;
    MaxParticlesPerCell := AdvSource.MaxParticlesPerCell;
    SinkParticlePlacementMethod := AdvSource.SinkParticlePlacementMethod;
    SinkNumberOfParticlePlanes := AdvSource.SinkNumberOfParticlePlanes;
    SinkParticleCount := AdvSource.SinkParticleCount;
  end;
  inherited;
end;

procedure TMt3dmsAdvection.Changed(Sender: TObject);
begin
  InvalidateModel;
end;

constructor TMt3dmsAdvection.Create(Model: TBaseModel);
begin
  inherited;
  FStoredCourant := TRealStorage.Create;
  FStoredCourant.OnChange := Changed;

  FStoredConcWeight := TRealStorage.Create;
  FStoredConcWeight.OnChange := Changed;

  FStoredRelCelConcGrad := TRealStorage.Create;
  FStoredRelCelConcGrad.OnChange := Changed;

  FStoredCriticalConcGradient := TRealStorage.Create;
  FStoredCriticalConcGradient.OnChange := Changed;

  InitializeVariables;
end;

destructor TMt3dmsAdvection.Destroy;
begin
  FStoredCriticalConcGradient.Free;
  FStoredRelCelConcGrad.Free;
  FStoredConcWeight.Free;
  FStoredCourant.Free;
  inherited;
end;

function TMt3dmsAdvection.GetConcWeight: double;
begin
  result := StoredConcWeight.Value;
end;

function TMt3dmsAdvection.GetCourant: double;
begin
  result := StoredCourant.Value;
end;

function TMt3dmsAdvection.GetCriticalConcGradient: double;
begin
  result := StoredCriticalConcGradient.Value;
end;

function TMt3dmsAdvection.GetRelCelConcGrad: double;
begin
  result := StoredRelCelConcGrad.Value
end;

procedure TMt3dmsAdvection.InitializeVariables;
begin
  inherited;
  Courant := 1;
  ConcWeight := 0.5;
  RelCelConcGrad := 1e-5;
  CriticalConcGradient := 0.01;
  AdvectionSolution := asUltimate;
  MaximumParticles := 75000;
  WeightingScheme  := wsUpstream;
  ParticleTrackMethod := ptmHybrid;
  ParticlePlacementMethod := ppmRandom;
  NumberOfParticlePlanes := 1;
  LowGradientParticleCount := 0;
  HighGradientParticleCount := 10;
  MinParticlePerCell := 2;
  MaxParticlesPerCell := 20;
  SinkParticlePlacementMethod := ppmRandom;
  SinkNumberOfParticlePlanes := 1;
  SinkParticleCount := 10;
end;

procedure TMt3dmsAdvection.SetAdvectionSolution(
  const Value: TAdvectionSolution);
begin
  if FAdvectionSolution <> Value then
  begin
    FAdvectionSolution := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsAdvection.SetConcWeight(const Value: double);
begin
  StoredConcWeight.Value := Value;
end;

procedure TMt3dmsAdvection.SetCourant(const Value: double);
begin
  StoredCourant.Value := Value;
end;

procedure TMt3dmsAdvection.SetCriticalConcGradient(const Value: double);
begin
  StoredCriticalConcGradient.Value := Value;
end;

procedure TMt3dmsAdvection.SetHighGradientParticleCount(const Value: integer);
begin
  if FHighGradientParticleCount <> Value then
  begin
    FHighGradientParticleCount := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsAdvection.SetLowGradientParticleCount(const Value: integer);
begin
  if FLowGradientParticleCount <> Value then
  begin
    FLowGradientParticleCount := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsAdvection.SetMaximumParticles(const Value: integer);
begin
  if FMaximumParticles <> Value then
  begin
    FMaximumParticles := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsAdvection.SetMaxParticlesPerCell(const Value: integer);
begin
  if FMaxParticlesPerCell <> Value then
  begin
    FMaxParticlesPerCell := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsAdvection.SetMinParticlePerCell(const Value: integer);
begin
  if FMinParticlePerCell <> Value then
  begin
    FMinParticlePerCell := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsAdvection.SetNumberOfParticlePlanes(const Value: integer);
begin
  if FNumberOfParticlePlanes <> Value then
  begin
    FNumberOfParticlePlanes := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsAdvection.SetParticlePlacementMethod(
  const Value: TParticlePlacementMethod);
begin
  if FParticlePlacementMethod <> Value then
  begin
    FParticlePlacementMethod := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsAdvection.SetParticleTrackMethod(
  const Value: TParticleTrackMethod);
begin
  if FParticleTrackMethod <> Value then
  begin
    FParticleTrackMethod := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsAdvection.SetRelCelConcGrad(const Value: double);
begin
  StoredRelCelConcGrad.Value := Value;
end;

procedure TMt3dmsAdvection.SetSinkNumberOfParticlePlanes(const Value: integer);
begin
  if FSinkNumberOfParticlePlanes <> Value then
  begin
    FSinkNumberOfParticlePlanes := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsAdvection.SetSinkParticleCount(const Value: integer);
begin
  if FSinkParticleCount <> Value then
  begin
    FSinkParticleCount := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsAdvection.SetSinkParticlePlacementMethod(
  const Value: TParticlePlacementMethod);
begin
  if FSinkParticlePlacementMethod <> Value then
  begin
    FSinkParticlePlacementMethod := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsAdvection.SetStoredConcWeight(const Value: TRealStorage);
begin
  FStoredConcWeight.Assign(Value);
end;

procedure TMt3dmsAdvection.SetStoredCourant(const Value: TRealStorage);
begin
  FStoredCourant.Assign(Value);
end;

procedure TMt3dmsAdvection.SetStoredCriticalConcGradient(
  const Value: TRealStorage);
begin
  FStoredCriticalConcGradient.Assign(Value);
end;

procedure TMt3dmsAdvection.SetStoredRelCelConcGrad(const Value: TRealStorage);
begin
  FStoredRelCelConcGrad.Assign(Value);
end;

procedure TMt3dmsAdvection.SetWeightingScheme(const Value: TWeightingScheme);
begin
  if FWeightingScheme <> Value then
  begin
    FWeightingScheme := Value;
    InvalidateModel;
  end;
end;

{ TMt3dmsDispersion }

procedure TMt3dmsDispersion.Assign(Source: TPersistent);
begin
  if Source is TMt3dmsDispersion then
  begin
    MultiDifussion := TMt3dmsDispersion(Source).MultiDifussion;
  end;
  inherited;
end;

constructor TMt3dmsDispersion.Create(Model: TBaseModel);
begin
  inherited;
  InitializeVariables;
end;

procedure TMt3dmsDispersion.InitializeVariables;
begin
  inherited;
  MultiDifussion := False;
end;

procedure TMt3dmsDispersion.SetIsSelected(const Value: boolean);
begin
  inherited;
  UpdateDataSets;
end;

procedure TMt3dmsDispersion.SetMultiDifussion(const Value: boolean);
begin
  if FMultiDifussion <> Value then
  begin
    FMultiDifussion := Value;
    InvalidateModel;
    UpdateDataSets;
  end;
end;

procedure TMt3dmsDispersion.UpdateDataSets;
var
  LocalModel: TCustomModel;
begin
  LocalModel := FModel as TCustomModel;
  if (LocalModel <> nil) and IsSelected then
  begin
    LocalModel.MobileComponents.UpdateDataArrays;
    // update diffusion data sets.
  end;
end;

{ TMt3dmsChemReaction }

procedure TMt3dmsChemReaction.Assign(Source: TPersistent);
var
  React: TMt3dmsChemReaction;
begin
  if Source is TMt3dmsChemReaction then
  begin
    React := TMt3dmsChemReaction(Source);
    SorptionChoice := React.SorptionChoice;
    KineticChoice := React.KineticChoice;
    OtherInitialConcChoice := React.OtherInitialConcChoice;
  end;
  inherited;
end;

constructor TMt3dmsChemReaction.Create(Model: TBaseModel);
begin
  inherited;
  InitializeVariables;
end;

procedure TMt3dmsChemReaction.InitializeVariables;
begin
  inherited;
  SorptionChoice := scLinear;
  KineticChoice := kcNone;
  OtherInitialConcChoice := oicDontUse;
end;

procedure TMt3dmsChemReaction.SetIsSelected(const Value: boolean);
begin
  inherited;
  UpdateDataSets;
end;

procedure TMt3dmsChemReaction.SetKineticChoice(const Value: TKineticChoice);
begin
  if FKineticChoice <> Value then
  begin
    FKineticChoice := Value;
    InvalidateModel;
    UpdateDataSets;
  end;
end;

procedure TMt3dmsChemReaction.SetOtherInitialConcChoice(
  const Value: TOtherInitialConcChoice);
begin
  if FOtherInitialConcChoice <> Value then
  begin
    FOtherInitialConcChoice := Value;
    InvalidateModel;
    UpdateDataSets;
  end;
end;

procedure TMt3dmsChemReaction.SetSorptionChoice(const Value: TSorptionChoice);
begin
  if FSorptionChoice <> Value then
  begin
    FSorptionChoice := Value;
    InvalidateModel;
    UpdateDataSets;
  end;
end;

procedure TMt3dmsChemReaction.UpdateDataSets;
var
  LocalModel: TCustomModel;
begin
  LocalModel := FModel as TCustomModel;
  if (LocalModel <> nil) and IsSelected then
  begin
    LocalModel.ModflowPackages.Mt3dBasic.UpdateDataSets;
    // update diffusion data sets.
  end;
end;

{ TMt3dmsTransportObservations }

procedure TMt3dmsTransportObservations.Assign(Source: TPersistent);
var
  TransObsSource: TMt3dmsTransportObservations;
begin
  if Source is TMt3dmsTransportObservations then
  begin
    TransObsSource := TMt3dmsTransportObservations(Source);
    ConcScaleFactor := TransObsSource.ConcScaleFactor;
    SaveBinary := TransObsSource.SaveBinary;
    ConcObsResult := TransObsSource.ConcObsResult;
    TransformType := TransObsSource.TransformType;
    InterpolateObs := TransObsSource.InterpolateObs;
    FluxScaleFactor := TransObsSource.FluxScaleFactor;
    MassFluxObsResult := TransObsSource.MassFluxObsResult;

  end;
  inherited;
end;

procedure TMt3dmsTransportObservations.Changed(Sender: TObject);
begin
  InvalidateModel;
end;

constructor TMt3dmsTransportObservations.Create(Model: TBaseModel);
begin
  inherited;
  FStoredConcScaleFactor := TRealStorage.Create;
  FStoredConcScaleFactor.OnChange := Changed;
  FStoredFluxScaleFactor := TRealStorage.Create;
  FStoredFluxScaleFactor.OnChange := Changed;
  InitializeVariables;
end;

destructor TMt3dmsTransportObservations.Destroy;
begin
  FStoredFluxScaleFactor.Free;
  FStoredConcScaleFactor.Free;
  inherited;
end;

function TMt3dmsTransportObservations.GetConcScaleFactor: double;
begin
  Result := StoredConcScaleFactor.Value;
end;

function TMt3dmsTransportObservations.GetFluxScaleFactor: double;
begin
  Result := StoredFluxScaleFactor.Value;
end;

procedure TMt3dmsTransportObservations.InitializeVariables;
begin
  inherited;
  ConcScaleFactor := 1;
  FluxScaleFactor := 1;
  SaveBinary := sbSave;
  ConcObsResult := corConcResid;
  TransformType := ltNoConversion;
  InterpolateObs := ioBilinear;
  MassFluxObsResult := mfoMassFluxResid;
end;

procedure TMt3dmsTransportObservations.SetConcObsResult(
  const Value: TConcObsResult);
begin
  if FConcObsResult <> Value then
  begin
    FConcObsResult := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsTransportObservations.SetConcScaleFactor(const Value: double);
begin
  StoredConcScaleFactor.Value := Value;
end;

procedure TMt3dmsTransportObservations.SetFluxScaleFactor(const Value: double);
begin
  StoredFluxScaleFactor.Value := Value;
end;

procedure TMt3dmsTransportObservations.SetInterpolateObs(
  const Value: TInterpolateObs);
begin
  if FInterpolateObs <> Value then
  begin
    FInterpolateObs := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsTransportObservations.SetMassFluxObsResult(
  const Value: TMassFluxObsResult);
begin
  FMassFluxObsResult := Value;
end;

procedure TMt3dmsTransportObservations.SetSaveBinary(const Value: TSaveBinary);
begin
  if FSaveBinary <> Value then
  begin
    FSaveBinary := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dmsTransportObservations.SetStoredConcScaleFactor(
  const Value: TRealStorage);
begin
  FStoredConcScaleFactor.Assign(Value);
end;

procedure TMt3dmsTransportObservations.SetStoredFluxScaleFactor(
  const Value: TRealStorage);
begin
  FStoredFluxScaleFactor.Assign(Value);
end;

procedure TMt3dmsTransportObservations.SetTransformType(
  const Value: TTransformType);
begin
  if FTransformType <> Value then
  begin
    FTransformType := Value;
    InvalidateModel;
  end;
end;

{ TMt3dmsSourceSinkMixing }

constructor TMt3dmsSourceSinkMixing.Create(Model: TBaseModel);
begin
  inherited;
  if Model <> nil then
  begin
    FConcentrations := TModflowBoundaryDisplayTimeList.Create(Model);
    Concentrations.OnInitialize := InitializeConcentrationDisplay;
    Concentrations.OnGetUseList := GetConcentrationUseList;
    Concentrations.OnTimeListUsed := PackageUsed;
    Concentrations.Name := StrMT3DMSSSMConcentra;
    AddTimeList(Concentrations);
  end;
end;

destructor TMt3dmsSourceSinkMixing.Destroy;
begin
  FConcentrations.Free;
  inherited;
end;

procedure TMt3dmsSourceSinkMixing.GetConcentrationUseList(Sender: TObject;
  NewUseList: TStringList);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Item: TCustomModflowBoundaryItem;
  ValueIndex: Integer;
  PhastModel: TCustomModel;
  Boundary: TMt3dmsConcBoundary;
begin
  PhastModel := FModel as TCustomModel;
  for ScreenObjectIndex := 0 to PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := PhastModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.Mt3dmsConcBoundary;
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

procedure TMt3dmsSourceSinkMixing.InitializeConcentrationDisplay(Sender: TObject);
var
  SsmWriter: TMt3dmsSsmWriter;
  List: TModflowBoundListOfTimeLists;
begin
  FConcentrations.CreateDataSets;

  List := TModflowBoundListOfTimeLists.Create;
  SsmWriter := TMt3dmsSsmWriter.Create(FModel as TCustomModel, etDisplay);
  try
    List.Add(FConcentrations);
    SsmWriter.UpdateDisplay(List);
  finally
    SsmWriter.Free;
    List.Free;
  end;
  FConcentrations.ComputeAverage;
end;

end.
