unit ModflowMnw2Unit;

interface

uses Classes, ModflowBoundaryUnit, FormulaManagerUnit, OrderedCollectionUnit,
  RbwParser, ModflowCellUnit, ZLib, GoPhastTypes, RealListUnit,
  SubscriptionUnit, SysUtils, Contnrs, DataSetUnit;

type
  TMnwLimitMethod = (mlmNoMinimum, mlmRate, mlmFraction);
  TMnwLossType = (mltNone, mltThiem, mltSkin, mltEquation, mtlSpecify);

  TMnw2Record = record
    Cell: TCellLocation;
    WellRadius: double;
    SkinRadius: double;
    SkinK: double;
    B: double;
    C: double;
    P: double;
    CellToWellConductance: double;
    PartialPenetration: double;
    WellRadiusAnnotation: string;
    SkinRadiusAnnotation: string;
    SkinKAnnotation: string;
    BAnnotation: string;
    CAnnotation: string;
    PAnnotation: string;
    CellToWellConductanceAnnotation: string;
    PartialPenetrationAnnotation: string;
    procedure Cache(Comp: TCompressionStream);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
  end;

  TMnw2Array = array of TMnw2Record;

  TMnw2Storage = class(TCustomBoundaryStorage)
  private
    FMnw2Array: TMnw2Array;
    function GetMnw2Array: TMnw2Array;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property Mnw2Array: TMnw2Array read GetMnw2Array;
  end;

  TMnw2TimeItem = class(TCustomModflowBoundaryItem)
  private
    FLimitMethod: TMnwLimitMethod;
    FPumpingRate: TFormulaObject;
    FReactivationPumpingRate: TFormulaObject;
    FLimitingWaterLevel: TFormulaObject;
    FInactivationPumpingRate: TFormulaObject;
    FHeadCapacityMultiplier: TFormulaObject;
    FHlim: double;
    FQdes: double;
    FCapMult: double;
    FQfrcmn: double;
    FQfrcmx: double;
    function GetHeadCapacityMultiplier: string;
    function GetInactivationPumpingRate: string;
    function GetLimitingWaterLevel: string;
    function GetPumpingRate: string;
    function GetReactivationPumpingRate: string;
    procedure SetHeadCapacityMultiplier(const Value: string);
    procedure SetInactivationPumpingRate(const Value: string);
    procedure SetLimitingWaterLevel(const Value: string);
    procedure SetLimitMethod(const Value: TMnwLimitMethod);
    procedure SetPumpingRate(const Value: string);
    procedure SetReactivationPumpingRate(const Value: string);
    function GetHeadCapacityMultiplierValue: double;
    function GetInactivationPumpingRateValue: double;
    function GetLimitingWaterLevelValue: double;
    function GetPumpingRateValue: double;
    function GetReactivationPumpingRateValue: double;
    function GetBoundaryValue(Index: integer): double;
    procedure SetBoundaryValue(Index: integer; const Value: double);
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    function GetBoundaryFormula(Index: integer): string; override;
    procedure SetBoundaryFormula(Index: integer; const Value: string);
      override;
    function BoundaryFormulaCount: integer; override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
    procedure Evaluate;
    property PumpingRateValue: double read GetPumpingRateValue;
    property HeadCapacityMultiplierValue: double
      read GetHeadCapacityMultiplierValue;
    property LimitingWaterLevelValue: double
      read GetLimitingWaterLevelValue;
    property InactivationPumpingRateValue: double
      read GetInactivationPumpingRateValue;
    property ReactivationPumpingRateValue: double
      read GetReactivationPumpingRateValue;
    property BoundaryValue[Index: integer]: double read GetBoundaryValue write SetBoundaryValue;
  published
    // QDes
    property PumpingRate: string read GetPumpingRate write SetPumpingRate;
    // CapMult
    property HeadCapacityMultiplier: string read GetHeadCapacityMultiplier
      write SetHeadCapacityMultiplier;
    // Hlim
    property LimitingWaterLevel: string read GetLimitingWaterLevel
      write SetLimitingWaterLevel;
    // Qfrcmn
    property InactivationPumpingRate: string read GetInactivationPumpingRate
      write SetInactivationPumpingRate;
    // Qfrcmx
    property ReactivationPumpingRate: string read GetReactivationPumpingRate
      write SetReactivationPumpingRate;
    // QCut
    property LimitMethod: TMnwLimitMethod read FLimitMethod
      write SetLimitMethod;
  end;

  TMnw2TimeCollection = class(TCustomNonSpatialBoundColl)
  protected
    class function ItemClass: TMF_BoundItemClass; override;
  public
    procedure Evaluate;
    function IndexOfContainedStartTime(AStartTime: double): integer;
  end;

  TMnw2SpatialItem = class(TCustomModflowBoundaryItem)
  private
    FB: TFormulaObject;
    FC: TFormulaObject;
    FCellToWellConductance: TFormulaObject;
    FP: TFormulaObject;
    FPartialPenetration: TFormulaObject;
    FSkinK: TFormulaObject;
    FSkinRadius: TFormulaObject;
    FWellRadius: TFormulaObject;
    function GetB: string;
    function GetC: string;
    function GetCellToWellConductance: string;
    function GetP: string;
    function GetPartialPenetration: string;
    function GetSkinK: string;
    function GetSkinRadius: string;
    function GetWellRadius: string;
    procedure SetB(const Value: string);
    procedure SetC(const Value: string);
    procedure SetCellToWellConductance(const Value: string);
    procedure SetP(const Value: string);
    procedure SetPartialPenetration(const Value: string);
    procedure SetSkinK(const Value: string);
    procedure SetSkinRadius(const Value: string);
    procedure SetWellRadius(const Value: string);
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    function GetBoundaryFormula(Index: integer): string; override;
    procedure SetBoundaryFormula(Index: integer; const Value: string);
      override;
    function BoundaryFormulaCount: integer; override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property WellRadius: string read GetWellRadius write SetWellRadius;
    property SkinRadius: string read GetSkinRadius write SetSkinRadius;
    property SkinK: string read GetSkinK write SetSkinK;
    property B: string read GetB write SetB;
    property C: string read GetC write SetC;
    property P: string read GetP write SetP;
    property CellToWellConductance: string read GetCellToWellConductance
      write SetCellToWellConductance;
    property PartialPenetration: string read GetPartialPenetration
      write SetPartialPenetration;
  end;

//  TMnw2SpatialCollection = class(TCustomMF_ListBoundColl)
  TMnw2SpatialCollection = class(TCustomMF_ArrayBoundColl)
  private
    FWellRadiusData: TModflowTimeList;
    FSkinRadiusData: TModflowTimeList;
    FSkinKData: TModflowTimeList;
    FBData: TModflowTimeList;
    FCData: TModflowTimeList;
    FPData: TModflowTimeList;
    FCellToWellConductanceData: TModflowTimeList;
    FPartialPenetrationData: TModflowTimeList;
    procedure InvalidateWellRadiusData(Sender: TObject);
    procedure InvalidateSkinRadiusData(Sender: TObject);
    procedure InvalidateSkinKData(Sender: TObject);
    procedure InvalidateBData(Sender: TObject);
    procedure InvalidateCData(Sender: TObject);
    procedure InvalidatePData(Sender: TObject);
    procedure InvalidateCellToWellConductanceData(Sender: TObject);
    procedure InvalidatePartialPenetrationData(Sender: TObject);
    function GetPhastModel: TComponent;
    procedure InvalidateWellRadius;
    procedure InvalidateSkinRadius;
    procedure InvalidateSkinK;
    procedure InvalidateB;
    procedure InvalidateC;
    procedure InvalidateP;
    procedure InvalidateCellToWellConductance;
    procedure InvalidatePartialPenetration;
  protected
    procedure AssignCellValues(DataSets: TList; ItemIndex: Integer);
      override;
    procedure InitializeTimeLists(ListOfTimeLists: TList); override;
//    procedure AssignCellLocation(BoundaryStorage: TCustomBoundaryStorage;
//      ACellList: TObject); override;
//    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
//      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
//      Variables, DataSets: TList); override;
//    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
//      override;
    procedure AddSpecificBoundary; override;

    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TMF_BoundItemClass; override;
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer); override;
    property PhastModel: TComponent read GetPhastModel;
  public
    // @name creates an instance of @classname
    constructor Create(Boundary: TModflowBoundary; Model,
      ScreenObject: TObject); override;
    // @name destroys the current instance of @classname.
    // Do not call @name; call Free instead.
    destructor Destroy; override;
  end;

  TLiftItem = class(TOrderedItem)
  private
    FLift: double;
    FQ: double;
    procedure SetLift(const Value: double);
    procedure SetQ(const Value: double);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Lift: double read FLift write SetLift;
    property Q: double read FQ write SetQ;
  end;

  TLiftCollection = class(TEnhancedOrderedCollection)
  public
    constructor Create(Model: TObject);
    procedure Sort;
    procedure Assign(Source: TPersistent); override;
  end;

  TMnw2_Cell = class(TValueCell)
  private
    Values: TMnw2Record;
    StressPeriod: integer;
    function GetB: double;
    function GetBAnnotation: string;
    function GetC: double;
    function GetCAnnotation: string;
    function GetCellToWellConductance: double;
    function GetCellToWellConductanceAnnotation: string;
    function GetP: double;
    function GetPAnnotation: string;
    function GetPartialPenetration: double;
    function GetPartialPenetrationAnnotation: string;
    function GetSkinK: double;
    function GetSkinKAnnotation: string;
    function GetSkinRadius: double;
    function GetSkinRadiusAnnotation: string;
    function GetWellRadius: double;
    function GetWellRadiusAnnotation: string;
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
    property WellRadius: double read GetWellRadius;
    property SkinRadius: double read GetSkinRadius;
    property SkinK: double read GetSkinK;
    property B: double read GetB;
    property C: double read GetC;
    property P: double read GetP;
    property CellToWellConductance: double read GetCellToWellConductance;
    property PartialPenetration: double read GetPartialPenetration;
    property WellRadiusAnnotation: string read GetWellRadiusAnnotation;
    property SkinRadiusAnnotation: string read GetSkinRadiusAnnotation;
    property SkinKAnnotation: string read GetSkinKAnnotation;
    property BAnnotation: string read GetBAnnotation;
    property CAnnotation: string read GetCAnnotation;
    property PAnnotation: string read GetPAnnotation;
    property CellToWellConductanceAnnotation: string
      read GetCellToWellConductanceAnnotation;
    property PartialPenetrationAnnotation: string
      read GetPartialPenetrationAnnotation;
  end;

  TTargetCell = Class(TGoPhastPersistent)
  private
    FLay: integer;
    FCol: integer;
    FRow: integer;
    procedure SetCol(const Value: integer);
    procedure SetLay(const Value: integer);
    procedure SetRow(const Value: integer);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Col: integer read FCol write SetCol;
    property Row: integer read FRow write SetRow;
    property Lay: integer read FLay write SetLay;
  End;

  TTargetLocation = Class(TGoPhastPersistent)
  private
    FZ: real;
    FX: real;
    FY: real;
    procedure SetX(const Value: real);
    procedure SetY(const Value: real);
    procedure SetZ(const Value: real);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property X: real read FX write SetX;
    property Y: real read FY write SetY;
    property Z: real read FZ write SetZ;
  End;

  TTargetObject = class(TGoPhastPersistent)
  private
    FObjectName: string;
    FScreenObject: TObject;
    procedure SetObjectName(const Value: string);
    function GetObjectName: string;
    procedure SetScreenObject(const Value: TObject);
    function GetScreenObject: TObject;
    function ValidScreenObject(AScreenObject: TObject): boolean;
  public
    procedure Assign(Source: TPersistent); override;
    property ScreenObject: TObject read GetScreenObject
      write SetScreenObject;
  published
    property ObjectName: string read GetObjectName write SetObjectName;
  end;

  TTargetType = (ttNone, ttCell, ttLocation, ttObject);

  TTarget = class(TGoPhastPersistent)
  private
    FTargetType: TTargetType;
    FTargetObject: TTargetObject;
    FTargetCell: TTargetCell;
    FTargetLocation: TTargetLocation;
    procedure SetTargetCell(const Value: TTargetCell);
    procedure SetTargetLocation(const Value: TTargetLocation);
    procedure SetTargetObject(const Value: TTargetObject);
    procedure SetTargetType(const Value: TTargetType);
    function StoreTargetObject: Boolean;
    function StoreTargetCell: Boolean;
    function StoreTargetLocation: Boolean;
  public
    Constructor Create(Model: TObject);
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property TargetType: TTargetType read FTargetType write SetTargetType;
    property TargetObject: TTargetObject read FTargetObject
      write SetTargetObject stored StoreTargetObject;
    property TargetLocation: TTargetLocation read FTargetLocation
      write SetTargetLocation stored StoreTargetLocation;
    property TargetCell: TTargetCell read FTargetCell write SetTargetCell
      stored StoreTargetCell;
  end;

  TVerticalScreen = class(TCustomModflowBoundaryItem)
  private
    FSkinRadius: TFormulaObject;
    FB: TFormulaObject;
    FC: TFormulaObject;
    FZTop: double;
    FCellToWellConductance: TFormulaObject;
    FP: TFormulaObject;
    FWellRadius: TFormulaObject;
    FSkinK: TFormulaObject;
    FZBottom: double;
    procedure SetB(const Value: string);
    procedure SetC(const Value: string);
    procedure SetCellToWellConductance(const Value: string);
    procedure SetP(const Value: string);
    procedure SetSkinK(const Value: string);
    procedure SetSkinRadius(const Value: string);
    procedure SetWellRadius(const Value: string);
    procedure SetZBottom(const Value: double);
    procedure SetZTop(const Value: double);
    function GetB: string;
    function GetC: string;
    function GetCellToWellConductance: string;
    function GetP: string;
    function GetSkinK: string;
    function GetSkinRadius: string;
    function GetWellRadius: string;
  protected
    function BoundaryFormulaCount: integer; override;
    function GetBoundaryFormula(Index: integer): string; override;
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function ScreenObject: TObject;
    procedure CreateFormulaObjects; override;
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property ZTop: double read FZTop write SetZTop;
    property ZBottom: double read FZBottom write SetZBottom;
    property WellRadius: string read GetWellRadius write SetWellRadius;
    property SkinRadius: string read GetSkinRadius write SetSkinRadius;
    property SkinK: string read GetSkinK write SetSkinK;
    property B: string read GetB write SetB;
    property C: string read GetC write SetC;
    property P: string read GetP write SetP;
    property CellToWellConductance: string read GetCellToWellConductance
      write SetCellToWellConductance;
  end;

  TVerticalScreenCollection = class(TCustomNonSpatialBoundColl)
  protected
    class function ItemClass: TMF_BoundItemClass; override;
  public
    procedure Sort;
  end;

  TMnw2Boundary = class(TModflowBoundary)
  private
    FTimeValues: TMnw2TimeCollection;
    FWellTolerance: double;
    FReferenceHead: double;
    FPumpElevation: double;
    FLiftAtMaxRate: double;
    FWellID: string;
    FLossType: TMnwLossType;
    FAdjustPumping: boolean;
    FConstrainPumping: boolean;
    FPartialPenetrationCorrection: boolean;
    FMaximumLift: double;
    FSpecifyPump: boolean;
    FLiftValues: TLiftCollection;
    FPumpCellTarget: TTarget;
    FSaveExternalFlows: boolean;
    FSaveInternalFlows: boolean;
    FVerticalScreens: TVerticalScreenCollection;
    FSaveMnwiInfo: boolean;
    procedure SetTimeValues(const Value: TMnw2TimeCollection);
    procedure SetAdjustPumping(const Value: boolean);
    procedure SetConstrainPumping(const Value: boolean);
    procedure SetLiftAtMaxRate(const Value: double);
    procedure SetLossType(const Value: TMnwLossType);
    procedure SetMaximumLift(const Value: double);
    procedure SetPartialPenetrationCorrection(const Value: boolean);
    procedure SetPumpElevation(const Value: double);
    procedure SetReferenceHead(const Value: double);
    procedure SetSpecifyPump(const Value: boolean);
    procedure SetWellID(const Value: string);
    procedure SetWellTolerance(const Value: double);
    procedure SetLiftValues(const Value: TLiftCollection);
    procedure SetPumpCellTarget(const Value: TTarget);
    function GetWellID: string;
    procedure SetSaveExternalFlows(const Value: boolean);
    procedure SetSaveInternalFlows(const Value: boolean);
    procedure SetVerticalScreens(const Value: TVerticalScreenCollection);
    procedure SetSaveMnwiInfo(const Value: boolean);
  protected
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass;
      override;
    procedure AddBoundaryTimes(BoundCol: TCustomNonSpatialBoundColl;
      Times: TRealList); override;
  public
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList);
      override;
    procedure InvalidateDisplay; override;
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model, ScreenObject: TObject);
    destructor Destroy; override;
    function Used: boolean; override;
    function ConstantConstraints: boolean;
    function TargetCellLocation: TCellLocation;
    procedure Clear; override;
    function DataTypeUsed(DataIndex: integer): boolean;
  published
    property TimeValues: TMnw2TimeCollection read FTimeValues
      write SetTimeValues;
    property LiftValues: TLiftCollection read FLiftValues write SetLiftValues;
    property WellID: string read GetWellID write SetWellID;
    property LossType: TMnwLossType read FLossType write SetLossType
      default mltThiem;
    // PUMPLOC
    property SpecifyPump: boolean read FSpecifyPump write SetSpecifyPump;
    // Zpump
    property PumpElevation: double read FPumpElevation write SetPumpElevation;
    // Qlimit
    property ConstrainPumping: boolean read FConstrainPumping
      write SetConstrainPumping;
    // PPFLAG
    property PartialPenetrationCorrection: boolean
      read FPartialPenetrationCorrection write SetPartialPenetrationCorrection;
    // PUMPCAP
    property AdjustPumping: boolean read FAdjustPumping write SetAdjustPumping;
    // Hlift
    property ReferenceHead: double read FReferenceHead write SetReferenceHead;
    // LIFTq0
    property MaximumLift: double read FMaximumLift write SetMaximumLift;
    // LIFTqmax
    property LiftAtMaxRate: double read FLiftAtMaxRate write SetLiftAtMaxRate;
    // HWtol
    property WellTolerance: double read FWellTolerance write SetWellTolerance;
    property PumpCellTarget: TTarget read FPumpCellTarget
      write SetPumpCellTarget;
    property SaveMnwiInfo: boolean read FSaveMnwiInfo write SetSaveMnwiInfo;
    property SaveExternalFlows: boolean read FSaveExternalFlows
      write SetSaveExternalFlows;
    property SaveInternalFlows: boolean read FSaveInternalFlows
      write SetSaveInternalFlows;
    property VerticalScreens: TVerticalScreenCollection read FVerticalScreens
      write SetVerticalScreens;
  end;

const
  PumpingRatePosition = 0;
  HeadCapacityMultiplierPosition = 1;
  LimitingWaterLevelPosition = 2;
  InactivationPumpingRatePosition = 3;
  ReactivationPumpingRatePosition = 4;

const
  WellRadiusPosition = 0;
  SkinRadiusPosition = 1;
  SkinKPosition = 2;
  BPosition = 3;
  CPosition = 4;
  PPosition = 5;
  CellToWellConductancePosition = 6;
  PartialPenetrationPosition = 7;

implementation

uses
  frmGoPhastUnit, ScreenObjectUnit, PhastModelUnit,
  ModflowGridUnit, frmFormulaErrorsUnit, Math, SparseDataSets, SparseArrayUnit;

{ TMnw2Item }

procedure TMnw2TimeItem.Assign(Source: TPersistent);
var
  Mnw: TMnw2TimeItem;
  Index: integer;
begin
  if Source is TMnw2TimeItem then
  begin
    Mnw := TMnw2TimeItem(Source);
    for Index := 0 to BoundaryFormulaCount - 1 do
    begin
      BoundaryFormula[Index] := Mnw.BoundaryFormula[Index];
    end;
    LimitMethod := Mnw.LimitMethod;
  end;
  inherited;
end;

procedure TMnw2TimeItem.AssignObserverEvents(Collection: TCollection);
begin
  // do nothing.
end;

function TMnw2TimeItem.BoundaryFormulaCount: integer;
begin
  result := 5;
end;

procedure TMnw2TimeItem.CreateFormulaObjects;
begin
  FPumpingRate := CreateFormulaObject(dso3D);
  FReactivationPumpingRate := CreateFormulaObject(dso3D);
  FLimitingWaterLevel := CreateFormulaObject(dso3D);
  FInactivationPumpingRate := CreateFormulaObject(dso3D);
  FHeadCapacityMultiplier := CreateFormulaObject(dso3D);
end;

destructor TMnw2TimeItem.Destroy;
var
  Index: integer;
begin
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    BoundaryFormula[Index] := '0';
  end;
  inherited;
end;

procedure TMnw2TimeItem.Evaluate;
var
  FormulaIndex: Integer;
  Formula: string;
  Compiler: TRbwParser;
  ObjectName: string;
  ErrorMessage: string;
  Expression: TExpression;
begin
// Set the following
//    FHlim: double;
//    FQdes: double;
//    FCapMult: double;
//    FQfrcmn: double;
//    FQfrcmx: double;

  Compiler := (Model as TPhastModel).rpThreeDFormulaCompiler;
  for FormulaIndex := 0 to BoundaryFormulaCount - 1 do
  begin
    Formula := BoundaryFormula[FormulaIndex];
    if Formula = '' then
    begin
      Formula := '0';
    end;
    try
      Compiler.Compile(Formula);
    except on E: ERbwParserError do
      begin
        ObjectName := (ScreenObject as TScreenObject).Name;
        case FormulaIndex of
          PumpingRatePosition:
            ErrorMessage := 'Qdes';
          HeadCapacityMultiplierPosition:
            ErrorMessage := 'CapMult';
          LimitingWaterLevelPosition:
            ErrorMessage := 'Hlim';
          InactivationPumpingRatePosition:
            ErrorMessage := 'Qfrcmn';
          ReactivationPumpingRatePosition:
            ErrorMessage := 'Qfrcmx';
          else Assert(False);
        end;
        ErrorMessage := 'Error in formula for ' + ErrorMessage
          + ' in MNW2 data';

        frmFormulaErrors.AddError(ObjectName, '', Formula, ErrorMessage);
        Formula := '0';
        BoundaryFormula[FormulaIndex] := Formula;
        Compiler.Compile(Formula);
      end;
    end;
    Expression := Compiler.CurrentExpression;
    if not (Expression.ResultType in [rdtDouble, rdtInteger]) then
    begin
      ObjectName := (ScreenObject as TScreenObject).Name;
      case FormulaIndex of
        PumpingRatePosition:
          ErrorMessage := 'Qdes';
        HeadCapacityMultiplierPosition:
          ErrorMessage := 'CapMult';
        LimitingWaterLevelPosition:
          ErrorMessage := 'Hlim';
        InactivationPumpingRatePosition:
          ErrorMessage := 'Qfrcmn';
        ReactivationPumpingRatePosition:
          ErrorMessage := 'Qfrcmx';
        else Assert(False);
      end;
      ErrorMessage := 'Incorrect result type in formula for ' + ErrorMessage
        + ' in MNW2 data';

      frmFormulaErrors.AddError(ObjectName, '', Formula, ErrorMessage);
      Formula := '0';
      BoundaryFormula[FormulaIndex] := Formula;
      Compiler.Compile(Formula);
      Expression := Compiler.CurrentExpression;
    end;
    Expression.Evaluate;
    BoundaryValue[FormulaIndex] := Expression.DoubleResult;
  end;
end;

function TMnw2TimeItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    PumpingRatePosition: result := PumpingRate;
    HeadCapacityMultiplierPosition: result := HeadCapacityMultiplier;
    LimitingWaterLevelPosition: result := LimitingWaterLevel;
    InactivationPumpingRatePosition: result := InactivationPumpingRate;
    ReactivationPumpingRatePosition: result := ReactivationPumpingRate;
    else Assert(False);
  end;
end;

function TMnw2TimeItem.GetBoundaryValue(Index: integer): double;
begin
  result := 0;
  case Index of
    PumpingRatePosition:
      result := FQdes;
    HeadCapacityMultiplierPosition:
      result := FCapMult;
    LimitingWaterLevelPosition:
      result := FHlim;
    InactivationPumpingRatePosition:
      result := FQfrcmn;
    ReactivationPumpingRatePosition:
      result := FQfrcmx;
    else Assert(False);
  end;
end;

function TMnw2TimeItem.GetHeadCapacityMultiplier: string;
begin
  Result := FHeadCapacityMultiplier.Formula;
  ResetItemObserver(HeadCapacityMultiplierPosition);
end;

function TMnw2TimeItem.GetHeadCapacityMultiplierValue: double;
begin
  result := FCapMult;
end;

function TMnw2TimeItem.GetInactivationPumpingRate: string;
begin
  Result := FInactivationPumpingRate.Formula;
  ResetItemObserver(InactivationPumpingRatePosition);
end;

function TMnw2TimeItem.GetInactivationPumpingRateValue: double;
begin
  result := FQfrcmn;
end;

function TMnw2TimeItem.GetLimitingWaterLevel: string;
begin
  Result := FLimitingWaterLevel.Formula;
  ResetItemObserver(LimitingWaterLevelPosition);
end;

function TMnw2TimeItem.GetLimitingWaterLevelValue: double;
begin
  result := FHlim
end;

procedure TMnw2TimeItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FPumpingRate then
  begin
    List.Add(FObserverList[PumpingRatePosition]);
  end;
  if Sender = FReactivationPumpingRate then
  begin
    List.Add(FObserverList[ReactivationPumpingRatePosition]);
  end;
  if Sender = FLimitingWaterLevel then
  begin
    List.Add(FObserverList[LimitingWaterLevelPosition]);
  end;
  if Sender = FInactivationPumpingRate then
  begin
    List.Add(FObserverList[InactivationPumpingRatePosition]);
  end;
  if Sender = FHeadCapacityMultiplier then
  begin
    List.Add(FObserverList[HeadCapacityMultiplierPosition]);
  end;
end;

function TMnw2TimeItem.GetPumpingRate: string;
begin
  Result := FPumpingRate.Formula;
  ResetItemObserver(PumpingRatePosition);
end;

function TMnw2TimeItem.GetPumpingRateValue: double;
begin
  result := FQdes;
end;

function TMnw2TimeItem.GetReactivationPumpingRate: string;
begin
  Result := FReactivationPumpingRate.Formula;
  ResetItemObserver(ReactivationPumpingRatePosition);
end;

function TMnw2TimeItem.GetReactivationPumpingRateValue: double;
begin
  result := FQfrcmx;
end;

function TMnw2TimeItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TMnw2TimeItem;
  Index: integer;
begin
  result := (AnotherItem is TMnw2TimeItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TMnw2TimeItem(AnotherItem);
    for Index := 0 to BoundaryFormulaCount - 1 do
    begin
      result := BoundaryFormula[Index] = Item.BoundaryFormula[Index];
      if not result then
      begin
        Exit;
      end;
    end;
    result := LimitMethod = Item.LimitMethod;
  end;
end;

procedure TMnw2TimeItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FPumpingRate,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FReactivationPumpingRate,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FLimitingWaterLevel,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FInactivationPumpingRate,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FHeadCapacityMultiplier,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);
end;

procedure TMnw2TimeItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    PumpingRatePosition:
      PumpingRate := Value;
    HeadCapacityMultiplierPosition:
      HeadCapacityMultiplier := Value;
    LimitingWaterLevelPosition:
      LimitingWaterLevel := Value;
    InactivationPumpingRatePosition:
      InactivationPumpingRate := Value;
    ReactivationPumpingRatePosition:
      ReactivationPumpingRate := Value;
    else Assert(False);
  end;
end;

procedure TMnw2TimeItem.SetBoundaryValue(Index: integer; const Value: double);
begin
  case Index of
    PumpingRatePosition:
      FQdes := Value;
    HeadCapacityMultiplierPosition:
      FCapMult := Value;
    LimitingWaterLevelPosition:
      FHlim := Value;
    InactivationPumpingRatePosition:
      FQfrcmn := Value;
    ReactivationPumpingRatePosition:
      FQfrcmx := Value;
    else Assert(False);
  end;
end;

procedure TMnw2TimeItem.SetHeadCapacityMultiplier(const Value: string);
begin
  UpdateFormula(Value, HeadCapacityMultiplierPosition, FHeadCapacityMultiplier);
end;

procedure TMnw2TimeItem.SetInactivationPumpingRate(const Value: string);
begin
  UpdateFormula(Value, InactivationPumpingRatePosition,
    FInactivationPumpingRate);
end;

procedure TMnw2TimeItem.SetLimitingWaterLevel(const Value: string);
begin
  UpdateFormula(Value, LimitingWaterLevelPosition, FLimitingWaterLevel);
end;

procedure TMnw2TimeItem.SetLimitMethod(const Value: TMnwLimitMethod);
begin
  if FLimitMethod <> Value then
  begin
    FLimitMethod := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2TimeItem.SetPumpingRate(const Value: string);
begin
  UpdateFormula(Value, PumpingRatePosition, FPumpingRate);
end;

procedure TMnw2TimeItem.SetReactivationPumpingRate(const Value: string);
begin
  UpdateFormula(Value, ReactivationPumpingRatePosition,
    FReactivationPumpingRate);
end;

{ TMnw2Boundary }

procedure TMnw2SpatialCollection.AddSpecificBoundary;
begin
  AddBoundary(TMnw2Storage.Create);
end;

//function TMnw2SpatialCollection.AdjustedFormula(FormulaIndex,
//  ItemIndex: integer): string;
//var
//  Item: TMnw2SpatialItem;
//begin
//  Item := Items[ItemIndex] as TMnw2SpatialItem;
//  result := Item.BoundaryFormula[FormulaIndex];
//end;

//procedure TMnw2SpatialCollection.AssignCellList(Expression: TExpression;
//  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
//  BoundaryFunctionIndex: integer; Variables, DataSets: TList);
//var
//  Mnw2Storage: TMnw2Storage;
//  CellList: TCellAssignmentList;
//  Index: Integer;
//  ACell: TCellAssignment;
//begin
//  Assert(BoundaryFunctionIndex in [WellRadiusPosition, SkinRadiusPosition,
//    SkinKPosition, BPosition, CPosition, PPosition,
//    CellToWellConductancePosition, PartialPenetrationPosition]);
//  Assert(Expression <> nil);
//
//  Mnw2Storage := BoundaryStorage as TMnw2Storage;
//  CellList := ACellList as TCellAssignmentList;
//  for Index := 0 to CellList.Count - 1 do
//  begin
//    ACell := CellList[Index];
//    UpdataRequiredData(DataSets, Variables, ACell);
//    // 2. update locations
//    Expression.Evaluate;
//    with Mnw2Storage.Mnw2Array[Index] do
//    begin
//      case BoundaryFunctionIndex of
//        WellRadiusPosition:
//          begin
//            WellRadius := Expression.DoubleResult;
//            WellRadiusAnnotation := ACell.Annotation;
//          end;
//        SkinRadiusPosition:
//          begin
//            SkinRadius := Expression.DoubleResult;
//            SkinRadiusAnnotation := ACell.Annotation;
//          end;
//        SkinKPosition:
//          begin
//            SkinK := Expression.DoubleResult;
//            SkinKAnnotation := ACell.Annotation;
//          end;
//        BPosition:
//          begin
//            B := Expression.DoubleResult;
//            BAnnotation := ACell.Annotation;
//          end;
//        CPosition:
//          begin
//            C := Expression.DoubleResult;
//            CAnnotation := ACell.Annotation;
//          end;
//        PPosition:
//          begin
//            P := Expression.DoubleResult;
//            PAnnotation := ACell.Annotation;
//          end;
//        CellToWellConductancePosition:
//          begin
//            CellToWellConductance := Expression.DoubleResult;
//            CellToWellConductanceAnnotation := ACell.Annotation;
//          end;
//        PartialPenetrationPosition:
//          begin
//            PartialPenetration := Expression.DoubleResult;
//            PartialPenetrationAnnotation := ACell.Annotation;
//          end;
//        else
//          Assert(False);
//      end;
//    end;
//  end;
//end;

//procedure TMnw2SpatialCollection.AssignCellLocation(
//  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
//var
//  Mnw2Storage: TMnw2Storage;
//  CellList: TCellAssignmentList;
//  Index: Integer;
//  ACell: TCellAssignment;
//begin
//  Mnw2Storage := BoundaryStorage as TMnw2Storage;
//  CellList := ACellList as TCellAssignmentList;
//  for Index := 0 to CellList.Count - 1 do
//  begin
//    ACell := CellList[Index];
//    with Mnw2Storage.Mnw2Array[Index] do
//    begin
//      Cell.Layer := ACell.Layer;
//      Cell.Row := ACell.Row;
//      Cell.Column := ACell.Column;
//      Cell.Section := ACell.Section;
//    end;
//  end;
//end;

procedure TMnw2SpatialCollection.AssignCellValues(DataSets: TList;
  ItemIndex: Integer);
var
  WellRadiusArray: TDataArray;
  SkinRadiusArray: TDataArray;
  SkinKArray: TDataArray;
  BArray: TDataArray;
  CArray: TDataArray;
  CellToWellConductanceArray: TDataArray;
  PartialPenetrationArray: TDataArray;
  BoundaryStorage: TMnw2Storage;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  BoundaryIndex: Integer;
  PArray: TDataArray;
  LocalScreenObject: TScreenObject;
  SegmentIndex: Integer;
  Segment: TCellElementSegment;
  PriorCol, PriorRow, PriorLayer: integer;
  LocalModel: TPhastModel;
  Boundary: TMnw2Boundary;
  LossType: TMnwLossType;
  IsValue: Boolean;
  UsedCells: T3DSparseIntegerArray;
begin
  LocalModel := Model as TPhastModel;
  PriorCol := -1;
  PriorRow := -1;
  PriorLayer := -1;
  Boundary := BoundaryGroup as TMnw2Boundary;
  LossType := Boundary.LossType;

  if LossType in [mltThiem, mltSkin, mltEquation] then
  begin
    WellRadiusArray := DataSets[0];
  end
  else
  begin
    WellRadiusArray := nil;
  end;

  if LossType = mltSkin then
  begin
    SkinRadiusArray := DataSets[1];
    SkinKArray := DataSets[2];
  end
  else
  begin
    SkinRadiusArray := nil;
    SkinKArray := nil;
  end;

  if LossType = mltEquation then
  begin
    BArray := DataSets[3];
    CArray := DataSets[4];
    PArray := DataSets[5];
  end
  else
  begin
    BArray := nil;
    CArray := nil;
    PArray := nil;
  end;

  if LossType = mtlSpecify then
  begin
    CellToWellConductanceArray := DataSets[6];
  end
  else
  begin
    CellToWellConductanceArray := nil;
  end;

  if Boundary.PartialPenetrationCorrection then
  begin
    PartialPenetrationArray := DataSets[7];
  end
  else
  begin
    PartialPenetrationArray := nil;
  end;




//  LastBoundaryIndex := -1;
  BoundaryIndex := -1;
  BoundaryStorage := Boundaries[ItemIndex] as TMnw2Storage;

  UsedCells := T3DSparseIntegerArray.Create(SPASmall);
  try
    LocalScreenObject := ScreenObject as TScreenObject;
    for SegmentIndex := 0 to LocalScreenObject.Segments.Count - 1 do
    begin
      Segment := LocalScreenObject.Segments[SegmentIndex];
      ColIndex := Segment.Col;
      RowIndex := Segment.Row;
      LayerIndex := Segment.Layer;
      if not LocalModel.LayerStructure.IsLayerSimulated(LayerIndex) then
      begin
        Continue;
      end;

      IsValue := False;
      if WellRadiusArray <> nil then
      begin
        IsValue := WellRadiusArray.IsValue[LayerIndex, RowIndex, ColIndex];
      end;
      if SkinRadiusArray <> nil then
      begin
        IsValue := SkinRadiusArray.IsValue[LayerIndex, RowIndex, ColIndex];
      end;
      if BArray <> nil then
      begin
        IsValue := BArray.IsValue[LayerIndex, RowIndex, ColIndex];
      end;
      if CellToWellConductanceArray <> nil then
      begin
        IsValue := CellToWellConductanceArray.IsValue[LayerIndex, RowIndex, ColIndex];
      end;
      if PartialPenetrationArray <> nil then
      begin
        IsValue := PartialPenetrationArray.IsValue[LayerIndex, RowIndex, ColIndex];
      end;

      if not IsValue then
      begin
        Continue;
      end;
      if (ColIndex = PriorCol)
        and (RowIndex = PriorRow)
        and (LayerIndex = PriorLayer) then
      begin
        Continue
      end;

      if UsedCells.IsValue[LayerIndex, RowIndex, ColIndex] then
      begin
        Continue
      end;
      UsedCells.Items[LayerIndex, RowIndex, ColIndex] := 1;

      Inc(BoundaryIndex);
      PriorCol := Segment.Col;
      PriorRow := Segment.Row;
      PriorLayer := Segment.Layer;


  //    if WellRadiusArray.IsValue[LayerIndex, RowIndex, ColIndex] then
      begin
  //      BoundaryStorage := Boundaries[ItemIndex] as TMnw2Storage;
        Assert(BoundaryIndex < Length(BoundaryStorage.Mnw2Array));
        with BoundaryStorage.Mnw2Array[BoundaryIndex] do
        begin
          Cell.Layer := LayerIndex;
          Cell.Row := RowIndex;
          Cell.Column := ColIndex;
  //        Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
          if WellRadiusArray <> nil then
          begin
            WellRadius := WellRadiusArray.
              RealData[LayerIndex, RowIndex, ColIndex];
            WellRadiusAnnotation := WellRadiusArray.
              Annotation[LayerIndex, RowIndex, ColIndex];
          end;

          if SkinRadiusArray <> nil then
          begin
            SkinRadius := SkinRadiusArray.
              RealData[LayerIndex, RowIndex, ColIndex];
            SkinRadiusAnnotation := SkinRadiusArray.
              Annotation[LayerIndex, RowIndex, ColIndex];
          end;
          if SkinKArray <> nil then
          begin
            SkinK := SkinKArray.
              RealData[LayerIndex, RowIndex, ColIndex];
            SkinKAnnotation := SkinKArray.
              Annotation[LayerIndex, RowIndex, ColIndex];
          end;

          if BArray <> nil then
          begin
            B := BArray.
              RealData[LayerIndex, RowIndex, ColIndex];
            BAnnotation := BArray.
              Annotation[LayerIndex, RowIndex, ColIndex];
          end;
          if CArray <> nil then
          begin
            C := CArray.
              RealData[LayerIndex, RowIndex, ColIndex];
            CAnnotation := CArray.
              Annotation[LayerIndex, RowIndex, ColIndex];
          end;
          if PArray <> nil then
          begin
            P := PArray.
              RealData[LayerIndex, RowIndex, ColIndex];
            PAnnotation := PArray.
              Annotation[LayerIndex, RowIndex, ColIndex];
          end;

          if CellToWellConductanceArray <> nil then
          begin
            CellToWellConductance := CellToWellConductanceArray.
              RealData[LayerIndex, RowIndex, ColIndex];
            CellToWellConductanceAnnotation := CellToWellConductanceArray.
              Annotation[LayerIndex, RowIndex, ColIndex];
          end;

          if PartialPenetrationArray <> nil then
          begin
            PartialPenetration := PartialPenetrationArray.
              RealData[LayerIndex, RowIndex, ColIndex];
            PartialPenetrationAnnotation := PartialPenetrationArray.
              Annotation[LayerIndex, RowIndex, ColIndex];
          end;
        end;
      end;
    end;
  finally
    UsedCells.Free;
  end;
  if WellRadiusArray <> nil then
  begin
    WellRadiusArray.CacheData;
  end;
  if SkinRadiusArray <> nil then
  begin
    SkinRadiusArray.CacheData;
  end;
  if SkinKArray <> nil then
  begin
    SkinKArray.CacheData;
  end;
  if BArray <> nil then
  begin
    BArray.CacheData;
  end;
  if CArray <> nil then
  begin
    CArray.CacheData;
  end;
  if PArray <> nil then
  begin
    PArray.CacheData;
  end;
  if CellToWellConductanceArray <> nil then
  begin
    CellToWellConductanceArray.CacheData;
  end;
  if PartialPenetrationArray <> nil then
  begin
    PartialPenetrationArray.CacheData;
  end;
  BoundaryStorage.CacheData;
end;

constructor TMnw2SpatialCollection.Create(Boundary: TModflowBoundary; Model,
  ScreenObject: TObject);
begin
  inherited;
  FWellRadiusData := TModflowTimeList.Create(Model);
  FSkinRadiusData := TModflowTimeList.Create(Model);
  FSkinKData := TModflowTimeList.Create(Model);
  FBData := TModflowTimeList.Create(Model);
  FCData := TModflowTimeList.Create(Model);
  FPData := TModflowTimeList.Create(Model);
  FCellToWellConductanceData := TModflowTimeList.Create(Model);
  FPartialPenetrationData := TModflowTimeList.Create(Model);
end;

destructor TMnw2SpatialCollection.Destroy;
begin
  FPartialPenetrationData.Free;
  FCellToWellConductanceData.Free;
  FPData.Free;
  FCData.Free;
  FBData.Free;
  FSkinKData.Free;
  FSkinRadiusData.Free;
  FWellRadiusData.Free;
  inherited;
end;

procedure TMnw2SpatialCollection.InvalidatePartialPenetration;
var
  LocalPhastModel: TPhastModel;
begin
  LocalPhastModel := PhastModel as TPhastModel;
  if LocalPhastModel <> nil then
  begin
    LocalPhastModel.ModflowPackages.Mnw2Package.MfMnwPartialPenetration.Invalidate;
  end;
end;

procedure TMnw2SpatialCollection.InvalidateCellToWellConductance;
var
  LocalPhastModel: TPhastModel;
begin
  LocalPhastModel := PhastModel as TPhastModel;
  if LocalPhastModel <> nil then
  begin
    LocalPhastModel.ModflowPackages.Mnw2Package.MfMnwCellToWellConductance.Invalidate;
  end;
end;

procedure TMnw2SpatialCollection.InvalidateP;
var
  LocalPhastModel: TPhastModel;
begin
  LocalPhastModel := PhastModel as TPhastModel;
  if LocalPhastModel <> nil then
  begin
    LocalPhastModel.ModflowPackages.Mnw2Package.MfMnwP.Invalidate;
  end;
end;

procedure TMnw2SpatialCollection.InvalidateC;
var
  LocalPhastModel: TPhastModel;
begin
  LocalPhastModel := PhastModel as TPhastModel;
  if LocalPhastModel <> nil then
  begin
    LocalPhastModel.ModflowPackages.Mnw2Package.MfMnwC.Invalidate;
  end;
end;

procedure TMnw2SpatialCollection.InitializeTimeLists(ListOfTimeLists: TList);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TMnw2SpatialItem;
  Boundary: TMnw2Boundary;
  ScreenObject: TScreenObject;
  ItemUsed: boolean;
  LossType: TMnwLossType;
begin
  SetLength(BoundaryValues, Count);

  Boundary := BoundaryGroup as TMnw2Boundary;
  LossType := Boundary.LossType;
  ScreenObject := Boundary.ScreenObject as TScreenObject;

  ItemUsed := LossType  in [mltThiem, mltSkin, mltEquation];
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TMnw2SpatialItem;
    BoundaryValues[Index].Time := Item.StartTime;
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.WellRadius;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  FWellRadiusData.Initialize(BoundaryValues, ScreenObject, alAll);

  ItemUsed := LossType  = mltSkin;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TMnw2SpatialItem;
    BoundaryValues[Index].Time := Item.StartTime;
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.SkinRadius;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  FSkinRadiusData.Initialize(BoundaryValues, ScreenObject, alAll);

  ItemUsed := LossType  = mltSkin;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TMnw2SpatialItem;
    BoundaryValues[Index].Time := Item.StartTime;
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.SkinK;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  FSkinKData.Initialize(BoundaryValues, ScreenObject, alAll);

  ItemUsed := LossType  = mltEquation;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TMnw2SpatialItem;
    BoundaryValues[Index].Time := Item.StartTime;
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.B;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  FBData.Initialize(BoundaryValues, ScreenObject,
    alAll);

  ItemUsed := LossType  = mltEquation;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TMnw2SpatialItem;
    BoundaryValues[Index].Time := Item.StartTime;
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.C;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  FCData.Initialize(BoundaryValues, ScreenObject,
    alAll);

  ItemUsed := LossType  = mltEquation;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TMnw2SpatialItem;
    BoundaryValues[Index].Time := Item.StartTime;
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.P;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  FPData.Initialize(BoundaryValues, ScreenObject,
    alAll);

  ItemUsed := LossType  = mtlSpecify;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TMnw2SpatialItem;
    BoundaryValues[Index].Time := Item.StartTime;
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.CellToWellConductance;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  FCellToWellConductanceData.Initialize(BoundaryValues, ScreenObject,
    alAll);


  ItemUsed := Boundary.PartialPenetrationCorrection;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TMnw2SpatialItem;
    BoundaryValues[Index].Time := Item.StartTime;
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.PartialPenetration;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  FPartialPenetrationData.Initialize(BoundaryValues, ScreenObject,
    alAll);



  Assert(FWellRadiusData.Count = Count);
  Assert(FSkinRadiusData.Count = Count);
  Assert(FSkinKData.Count = Count);
  Assert(FBData.Count = Count);
  Assert(FCData.Count = Count);
  Assert(FPData.Count = Count);
  Assert(FCellToWellConductanceData.Count = Count);
  Assert(FPartialPenetrationData.Count = Count);
  ClearBoundaries;
  SetBoundaryCapacity(FWellRadiusData.Count);
  for TimeIndex := 0 to FWellRadiusData.Count - 1 do
  begin
    AddBoundary(TMnw2Storage.Create);
  end;
  ListOfTimeLists.Add(FWellRadiusData);
  ListOfTimeLists.Add(FSkinRadiusData);
  ListOfTimeLists.Add(FSkinKData);
  ListOfTimeLists.Add(FBData);
  ListOfTimeLists.Add(FCData);
  ListOfTimeLists.Add(FPData);
  ListOfTimeLists.Add(FCellToWellConductanceData);
  ListOfTimeLists.Add(FPartialPenetrationData);
end;

procedure TMnw2SpatialCollection.InvalidateB;
var
  LocalPhastModel: TPhastModel;
begin
  LocalPhastModel := PhastModel as TPhastModel;
  if LocalPhastModel <> nil then
  begin
    LocalPhastModel.ModflowPackages.Mnw2Package.MfMnwB.Invalidate;
  end;
end;

procedure TMnw2SpatialCollection.InvalidateSkinK;
var
  LocalPhastModel: TPhastModel;
begin
  LocalPhastModel := PhastModel as TPhastModel;
  if LocalPhastModel <> nil then
  begin
    LocalPhastModel.ModflowPackages.Mnw2Package.MfMnwSkinK.Invalidate;
  end;
end;

procedure TMnw2SpatialCollection.InvalidateSkinRadius;
var
  LocalPhastModel: TPhastModel;
begin
  LocalPhastModel := PhastModel as TPhastModel;
  if LocalPhastModel <> nil then
  begin
    LocalPhastModel.ModflowPackages.Mnw2Package.MfMnwSkinRadius.Invalidate;
  end;
end;

procedure TMnw2SpatialCollection.InvalidateWellRadius;
var
  LocalPhastModel: TPhastModel;
begin
  LocalPhastModel := PhastModel as TPhastModel;
  if LocalPhastModel <> nil then
  begin
    LocalPhastModel.ModflowPackages.Mnw2Package.MfMnwWellRadius.Invalidate;
  end;
end;

procedure TMnw2SpatialCollection.InvalidateBData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FBData.Invalidate;
    InvalidateB;
  end;
end;

procedure TMnw2SpatialCollection.InvalidateCData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FCData.Invalidate;
    InvalidateC;
  end;
end;

procedure TMnw2SpatialCollection.InvalidateCellToWellConductanceData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FCellToWellConductanceData.Invalidate;
    InvalidateCellToWellConductance;
  end;
end;

procedure TMnw2SpatialCollection.InvalidatePartialPenetrationData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FPartialPenetrationData.Invalidate;
    InvalidatePartialPenetration;
  end;
end;

procedure TMnw2SpatialCollection.InvalidatePData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FPData.Invalidate;
    InvalidateP;
  end;
end;

procedure TMnw2SpatialCollection.InvalidateSkinKData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FSkinKData.Invalidate;
    InvalidateSkinK;
  end;
end;

procedure TMnw2SpatialCollection.InvalidateSkinRadiusData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FSkinRadiusData.Invalidate;
    InvalidateSkinRadius;
  end;
end;

function TMnw2SpatialCollection.GetPhastModel: TComponent;
var
  OwnerScreenObject: TScreenObject;
begin
  result := nil;
  if ScreenObject <> nil then
  begin
    OwnerScreenObject := ScreenObject as TScreenObject;
    if OwnerScreenObject.Model <> nil then
    begin
      result := OwnerScreenObject.Model;
    end;
  end;
end;

procedure TMnw2SpatialCollection.InvalidateWellRadiusData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FWellRadiusData.Invalidate;
    InvalidateWellRadius;
  end;
end;

class function TMnw2SpatialCollection.ItemClass: TMF_BoundItemClass;
begin
  result := TMnw2SpatialItem;
end;

procedure TMnw2SpatialCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer);
begin
  SetLength((Boundaries[ItemIndex] as TMnw2Storage).FMnw2Array, BoundaryCount);
  inherited;
end;

{ TMnw2Boundary }

procedure TMnw2Boundary.AddBoundaryTimes(BoundCol: TCustomNonSpatialBoundColl;
  Times: TRealList);
var
  Index: Integer;
  TimeItem: TMnw2TimeItem;
begin
  for Index := 0 to TimeValues.Count - 1 do
  begin
    TimeItem := TimeValues[Index] as TMnw2TimeItem;
    Times.AddUnique(TimeItem.StartTime);
    Times.AddUnique(TimeItem.EndTime);
  end;
end;

procedure TMnw2Boundary.Assign(Source: TPersistent);
var
  SourceMnw2: TMnw2Boundary;
begin
  if Source is TMnw2Boundary then
  begin
    SourceMnw2 := TMnw2Boundary(Source);
    TimeValues := SourceMnw2.TimeValues;
    LiftValues := SourceMnw2.LiftValues;
    WellID := SourceMnw2.WellID;
    LossType := SourceMnw2.LossType;
    SpecifyPump := SourceMnw2.SpecifyPump;
    PumpElevation := SourceMnw2.PumpElevation;
    PumpElevation := SourceMnw2.PumpElevation;
    ConstrainPumping := SourceMnw2.ConstrainPumping;
    PartialPenetrationCorrection := SourceMnw2.PartialPenetrationCorrection;
    AdjustPumping := SourceMnw2.AdjustPumping;
    ReferenceHead := SourceMnw2.ReferenceHead;
    MaximumLift := SourceMnw2.MaximumLift;
    LiftAtMaxRate := SourceMnw2.LiftAtMaxRate;
    WellTolerance := SourceMnw2.WellTolerance;
    PumpCellTarget := SourceMnw2.PumpCellTarget;
    SaveMnwiInfo := SourceMnw2.SaveMnwiInfo;
    SaveExternalFlows := SourceMnw2.SaveExternalFlows;
    SaveInternalFlows := SourceMnw2.SaveInternalFlows;
    VerticalScreens := SourceMnw2.VerticalScreens;
  end;
  inherited;
end;

procedure TMnw2Boundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList);
var
  Cell: TMnw2_Cell;
  BoundaryValues: TMnw2Record;
  BoundaryIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TMnw2Storage;
begin
  LocalBoundaryStorage := BoundaryStorage as TMnw2Storage;
  if 0 < ValueTimeList.Count then
  begin
    Cells := ValueTimeList[0];
  end
  else
  begin
    Cells := TValueCellList.Create(TMnw2_Cell);
    ValueTimeList.Add(Cells);
  end;
  // Check if the stress period is completely enclosed within the times
  // of the LocalBoundaryStorage;
//  Cells.CheckRestore;
  for BoundaryIndex := 0 to Length(LocalBoundaryStorage.Mnw2Array) - 1 do
  begin
    BoundaryValues := LocalBoundaryStorage.Mnw2Array[BoundaryIndex];
    Cell := TMnw2_Cell.Create;
    Assert(ScreenObject <> nil);
    Cell.IFace := (ScreenObject as TScreenObject).IFace;
    Cells.Add(Cell);
    Cell.StressPeriod := 0;
    Cell.Values := BoundaryValues;
    Cell.ScreenObject := ScreenObject;
  end;
  Cells.Cache;
  LocalBoundaryStorage.CacheData;
end;

class function TMnw2Boundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TMnw2SpatialCollection;
end;

procedure TMnw2Boundary.Clear;
begin
  inherited;
  FLossType := mltThiem;
  TimeValues.Clear;
  LiftValues.Clear;
  VerticalScreens.Clear;
end;

function TMnw2Boundary.DataTypeUsed(DataIndex: integer): boolean;
begin
  result := False;
  case DataIndex of
    WellRadiusPosition:
      begin
        result := LossType in [mltThiem, mltSkin, mltEquation];
      end;
    SkinRadiusPosition, SkinKPosition:
      begin
        result := LossType = mltSkin;
      end;
    BPosition, CPosition, PPosition:
      begin
        result := LossType = mltEquation;
      end;
    CellToWellConductancePosition:
      begin
        result := LossType = mtlSpecify;
      end;
    PartialPenetrationPosition:
      begin
        result := PartialPenetrationCorrection;
      end;
    else Assert(False);
  end;
end;



function TMnw2Boundary.ConstantConstraints: boolean;
var
  Index: Integer;
  Item: TMnw2TimeItem;
  FirstItem: TMnw2TimeItem;
begin
  result := True;

  Assert(TimeValues.Count > 0);
  FirstItem := TimeValues.Items[0] as TMnw2TimeItem;
  for Index := 0 to TimeValues.Count - 1 do
  begin
    Item := TimeValues.Items[Index] as TMnw2TimeItem;
    result := (Item.FHlim = FirstItem.FHlim)
      and (Item.LimitMethod = FirstItem.LimitMethod);
    if not result then
    begin
      Exit;
    end;
    if FirstItem.LimitMethod <> mlmNoMinimum then
    begin
      result := (Item.FQfrcmn = FirstItem.FQfrcmn)
        and (Item.FQfrcmx = FirstItem.FQfrcmx);
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

constructor TMnw2Boundary.Create(Model, ScreenObject: TObject);
begin
  inherited;
  FLossType := mltThiem;
  FTimeValues := TMnw2TimeCollection.Create(self, Model, ScreenObject);
  FLiftValues := TLiftCollection.Create(Model);
  FPumpCellTarget := TTarget.Create(Model);
  FVerticalScreens := TVerticalScreenCollection.Create(self, Model, ScreenObject);
end;

destructor TMnw2Boundary.Destroy;
begin
  FVerticalScreens.Free;
  FPumpCellTarget.Free;
  FLiftValues.Free;
  FTimeValues.Free;
  inherited;
end;

procedure TMnw2Boundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList);
var
  ValueIndex: Integer;
  BoundaryStorage: TMnw2Storage;
//  ValueCount: Integer;
begin
  EvaluateArrayBoundaries;
//  EvaluateListBoundaries;
//  ValueCount := 0;
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex] as TMnw2Storage;
      AssignCells(BoundaryStorage, ValueTimeList);
//      Inc(ValueCount);
    end;
  end;
  TimeValues.Evaluate;
end;

function TMnw2Boundary.GetWellID: string;
begin
  if Length(FWellID) > 20 then
  begin
    SetLength(FWellID, 20);
  end;
  result := FWellID;
end;

procedure TMnw2Boundary.InvalidateDisplay;
begin
  inherited;
  // need to finish this
  Assert(False);
end;

procedure TMnw2Boundary.SetAdjustPumping(const Value: boolean);
begin
  if FAdjustPumping <> Value then
  begin
    FAdjustPumping := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2Boundary.SetConstrainPumping(const Value: boolean);
begin
  if FConstrainPumping <> Value then
  begin
    FConstrainPumping := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2Boundary.SetLiftAtMaxRate(const Value: double);
begin
  if FLiftAtMaxRate <> Value then
  begin
    FLiftAtMaxRate := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2Boundary.SetLiftValues(const Value: TLiftCollection);
begin
  FLiftValues.Assign(Value);
end;

procedure TMnw2Boundary.SetLossType(const Value: TMnwLossType);
var
  SpatialCollection: TMnw2SpatialCollection;
begin
  if FLossType <> Value then
  begin
    FLossType := Value;
    InvalidateModel;
    SpatialCollection := Values as TMnw2SpatialCollection;
    SpatialCollection.InvalidateWellRadius;
    SpatialCollection.InvalidateSkinRadius;
    SpatialCollection.InvalidateSkinK;
    SpatialCollection.InvalidateB;
    SpatialCollection.InvalidateC;
    SpatialCollection.InvalidateP;
    SpatialCollection.InvalidateCellToWellConductance;
  end;
end;

procedure TMnw2Boundary.SetMaximumLift(const Value: double);
begin
  if FMaximumLift <> Value then
  begin
    FMaximumLift := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2Boundary.SetPartialPenetrationCorrection(const Value: boolean);
var
  SpatialCollection: TMnw2SpatialCollection;
begin
  if FPartialPenetrationCorrection <> Value then
  begin
    FPartialPenetrationCorrection := Value;
    InvalidateModel;
    SpatialCollection := Values as TMnw2SpatialCollection;
    SpatialCollection.InvalidatePartialPenetration;
  end;
end;

procedure TMnw2Boundary.SetPumpCellTarget(const Value: TTarget);
begin
  FPumpCellTarget.Assign(Value);
end;

procedure TMnw2Boundary.SetPumpElevation(const Value: double);
begin
  if FPumpElevation <> Value then
  begin
    FPumpElevation := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2Boundary.SetReferenceHead(const Value: double);
begin
  if FReferenceHead <> Value then
  begin
    FReferenceHead := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2Boundary.SetSaveExternalFlows(const Value: boolean);
begin
  if FSaveExternalFlows <> Value then
  begin
    FSaveExternalFlows := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2Boundary.SetSaveInternalFlows(const Value: boolean);
begin
  if FSaveInternalFlows <> Value then
  begin
    FSaveInternalFlows := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2Boundary.SetSaveMnwiInfo(const Value: boolean);
begin
  if FSaveMnwiInfo <> Value then
  begin
    FSaveMnwiInfo := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2Boundary.SetSpecifyPump(const Value: boolean);
begin
  if FSpecifyPump <> Value then
  begin
    FSpecifyPump := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2Boundary.SetTimeValues(const Value: TMnw2TimeCollection);
begin
  FTimeValues.Assign(Value);
end;

procedure TMnw2Boundary.SetVerticalScreens(
  const Value: TVerticalScreenCollection);
begin
  FVerticalScreens.Assign(Value);
end;

procedure TMnw2Boundary.SetWellID(const Value: string);
begin
  if FWellID <> Value then
  begin
    FWellID := Value;
    InvalidateModel;
  end;
end;

procedure TMnw2Boundary.SetWellTolerance(const Value: double);
begin
  if FWellTolerance <> Value then
  begin
    FWellTolerance := Value;
    InvalidateModel;
  end;
end;

function TMnw2Boundary.TargetCellLocation: TCellLocation;
var
  ScreenObject: TScreenObject;
  X, Y, Z: double;
  Model: TPhastModel;
  Grid: TModflowGrid;
begin
  case PumpCellTarget.TargetType of
    ttNone:
      begin
        result.Layer := 0;
        result.Row := 1;
        result.Column := 1;
      end;
    ttObject:
      begin
        ScreenObject := PumpCellTarget.TargetObject.ScreenObject as TScreenObject;
        if ScreenObject = nil then
        begin
          result.Layer := 0;
          result.Row := 1;
          result.Column := 1;
        end
        else
        begin
          result := ScreenObject.SingleCellLocation;
        end;
      end;
    ttLocation:
      begin
        Model := PhastModel as TPhastModel;
        Grid := Model.ModflowGrid;
        X := PumpCellTarget.TargetLocation.X;
        Y := PumpCellTarget.TargetLocation.Y;
        Z := PumpCellTarget.TargetLocation.Z;
        if (X < Grid.ColumnPosition[Grid.ColumnCount]) and
          (Y > Grid.RowPosition[Grid.RowCount]) then
        begin
          result.Column := Grid.GetContainingColumn(X);
          result.Row := Grid.GetContainingRow(Y);
          if (result.Column >= 0) and (result.Row >= 0) then
          begin
            GetLayerFromZ(Z, Result, Grid, Model);
          end
          else
          begin
            result.Layer := 0;
            result.Row := 1;
            result.Column := 1;
          end;
        end
        else
        begin
          result.Layer := 0;
          result.Row := 1;
          result.Column := 1;
        end;
      end;
    ttCell:
      begin
        result.Layer := PumpCellTarget.TargetCell.Lay;
        result.Row := PumpCellTarget.TargetCell.Row;
        result.Column := PumpCellTarget.TargetCell.Col;
      end;
    else Assert(False);
  end;
end;

function TMnw2Boundary.Used: boolean;
begin
  result := TimeValues.Count > 0;
end;

{ TMnw2SpatialItem }

procedure TMnw2SpatialItem.Assign(Source: TPersistent);
var
  SpatialSource: TMnw2SpatialItem;
begin
  if Source is TMnw2SpatialItem then
  begin
    SpatialSource := TMnw2SpatialItem(Source);
    WellRadius := SpatialSource.WellRadius;
    SkinRadius := SpatialSource.SkinRadius;
    SkinK := SpatialSource.SkinK;
    B := SpatialSource.B;
    C := SpatialSource.C;
    P := SpatialSource.P;
    CellToWellConductance := SpatialSource.CellToWellConductance;
    PartialPenetration := SpatialSource.PartialPenetration;
  end;
  inherited;

end;

procedure TMnw2SpatialItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TMnw2SpatialCollection;
  WellRadiusObserver: TObserver;
  SkinRadiusObserver: TObserver;
  SkinKObserver: TObserver;
  BObserver: TObserver;
  CObserver: TObserver;
  PObserver: TObserver;
  CellToWellConductanceObserver: TObserver;
  PartialPenetrationObserver: TObserver;
begin
  ParentCollection := Collection as TMnw2SpatialCollection;

  WellRadiusObserver := FObserverList[WellRadiusPosition];
  WellRadiusObserver.OnUpToDateSet := ParentCollection.InvalidateWellRadiusData;

  SkinRadiusObserver := FObserverList[SkinRadiusPosition];
  SkinRadiusObserver.OnUpToDateSet := ParentCollection.InvalidateSkinRadiusData;

  SkinKObserver := FObserverList[SkinKPosition];
  SkinKObserver.OnUpToDateSet := ParentCollection.InvalidateSkinKData;

  BObserver := FObserverList[BPosition];
  BObserver.OnUpToDateSet := ParentCollection.InvalidateBData;

  CObserver := FObserverList[CPosition];
  CObserver.OnUpToDateSet := ParentCollection.InvalidateCData;

  PObserver := FObserverList[PPosition];
  PObserver.OnUpToDateSet := ParentCollection.InvalidatePData;

  CellToWellConductanceObserver := FObserverList[CellToWellConductancePosition];
  CellToWellConductanceObserver.OnUpToDateSet :=
    ParentCollection.InvalidateCellToWellConductanceData;

  PartialPenetrationObserver :=
    FObserverList[PartialPenetrationPosition];
  PartialPenetrationObserver.OnUpToDateSet :=
    ParentCollection.InvalidatePartialPenetrationData;
end;

function TMnw2SpatialItem.BoundaryFormulaCount: integer;
begin
  result := 8;
end;

procedure TMnw2SpatialItem.CreateFormulaObjects;
begin
  FB := CreateFormulaObject(dso3D);
  FC := CreateFormulaObject(dso3D);
  FCellToWellConductance := CreateFormulaObject(dso3D);
  FP := CreateFormulaObject(dso3D);
  FPartialPenetration := CreateFormulaObject(dso3D);
  FSkinK := CreateFormulaObject(dso3D);
  FSkinRadius := CreateFormulaObject(dso3D);
  FWellRadius := CreateFormulaObject(dso3D);

end;

function TMnw2SpatialItem.GetB: string;
begin
  Result := FB.Formula;
  ResetItemObserver(BPosition);
end;

function TMnw2SpatialItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    WellRadiusPosition:
      result := WellRadius;
    SkinRadiusPosition:
      result := SkinRadius;
    SkinKPosition:
      result := SkinK;
    BPosition:
      result := B;
    CPosition:
      result := C;
    PPosition:
      result := P;
    CellToWellConductancePosition:
      result := CellToWellConductance;
    PartialPenetrationPosition:
      result := PartialPenetration;
    else Assert(False);
  end;
end;

function TMnw2SpatialItem.GetC: string;
begin
  Result := FC.Formula;
  ResetItemObserver(CPosition);
end;

function TMnw2SpatialItem.GetCellToWellConductance: string;
begin
  Result := FCellToWellConductance.Formula;
  ResetItemObserver(CellToWellConductancePosition);
end;

function TMnw2SpatialItem.GetP: string;
begin
  Result := FP.Formula;
  ResetItemObserver(PPosition);
end;

function TMnw2SpatialItem.GetPartialPenetration: string;
begin
  Result := FPartialPenetration.Formula;
  ResetItemObserver(PartialPenetrationPosition);
end;

procedure TMnw2SpatialItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FB then
  begin
    List.Add(FObserverList[BPosition]);
  end;
  if Sender = FC then
  begin
    List.Add(FObserverList[CPosition]);
  end;
  if Sender = FCellToWellConductance then
  begin
    List.Add(FObserverList[CellToWellConductancePosition]);
  end;
  if Sender = FP then
  begin
    List.Add(FObserverList[PPosition]);
  end;
  if Sender = FPartialPenetration then
  begin
    List.Add(FObserverList[PartialPenetrationPosition]);
  end;
  if Sender = FSkinK then
  begin
    List.Add(FObserverList[SkinKPosition]);
  end;
  if Sender = FSkinRadius then
  begin
    List.Add(FObserverList[SkinRadiusPosition]);
  end;
  if Sender = FWellRadius then
  begin
    List.Add(FObserverList[WellRadiusPosition]);
  end;
end;

function TMnw2SpatialItem.GetSkinK: string;
begin
  Result := FSkinK.Formula;
  ResetItemObserver(SkinKPosition);
end;

function TMnw2SpatialItem.GetSkinRadius: string;
begin
  Result := FSkinRadius.Formula;
  ResetItemObserver(SkinRadiusPosition);
end;

function TMnw2SpatialItem.GetWellRadius: string;
begin
  Result := FWellRadius.Formula;
  ResetItemObserver(WellRadiusPosition);
end;

function TMnw2SpatialItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TMnw2SpatialItem;
  Index: integer;
begin
  result := (AnotherItem is TMnw2SpatialItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TMnw2SpatialItem(AnotherItem);
    for Index := 0 to BoundaryFormulaCount - 1 do
    begin
      result := BoundaryFormula[Index] = Item.BoundaryFormula[Index];
      if not result then
      begin
        Exit;
      end;
    end;
  end;

end;

procedure TMnw2SpatialItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FB,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FC,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FCellToWellConductance,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FP,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FPartialPenetration,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FSkinK,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FSkinRadius,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FWellRadius,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);

end;

procedure TMnw2SpatialItem.SetB(const Value: string);
begin
  UpdateFormula(Value, BPosition, FB);
end;

procedure TMnw2SpatialItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    WellRadiusPosition:
      WellRadius := Value;
    SkinRadiusPosition:
      SkinRadius := Value;
    SkinKPosition:
      SkinK := Value;
    BPosition:
      B := Value;
    CPosition:
      C := Value;
    PPosition:
      P := Value;
    CellToWellConductancePosition:
      CellToWellConductance := Value;
    PartialPenetrationPosition:
      PartialPenetration := Value;
    else Assert(False);
  end;
end;

procedure TMnw2SpatialItem.SetC(const Value: string);
begin
  UpdateFormula(Value, CPosition, FC);
end;

procedure TMnw2SpatialItem.SetCellToWellConductance(const Value: string);
begin
  UpdateFormula(Value, CellToWellConductancePosition, FCellToWellConductance);
end;

procedure TMnw2SpatialItem.SetP(const Value: string);
begin
  UpdateFormula(Value, PPosition, FP);
end;

procedure TMnw2SpatialItem.SetPartialPenetration(const Value: string);
begin
  UpdateFormula(Value, PartialPenetrationPosition, FPartialPenetration);
end;

procedure TMnw2SpatialItem.SetSkinK(const Value: string);
begin
  UpdateFormula(Value, SkinKPosition, FSkinK);
end;

procedure TMnw2SpatialItem.SetSkinRadius(const Value: string);
begin
  UpdateFormula(Value, SkinRadiusPosition, FSkinRadius);
end;

procedure TMnw2SpatialItem.SetWellRadius(const Value: string);
begin
  UpdateFormula(Value, WellRadiusPosition, FWellRadius);
end;

{ TMnw2Record }

procedure TMnw2Record.Cache(Comp: TCompressionStream);
begin
  WriteCompCell(Comp, Cell);

  WriteCompReal(Comp, WellRadius);
  WriteCompReal(Comp, SkinRadius);
  WriteCompReal(Comp, SkinK);
  WriteCompReal(Comp, B);
  WriteCompReal(Comp, C);
  WriteCompReal(Comp, P);
  WriteCompReal(Comp, CellToWellConductance);
  WriteCompReal(Comp, PartialPenetration);

  WriteCompString(Comp, WellRadiusAnnotation);
  WriteCompString(Comp, SkinRadiusAnnotation);
  WriteCompString(Comp, SkinKAnnotation);
  WriteCompString(Comp, BAnnotation);
  WriteCompString(Comp, CAnnotation);
  WriteCompString(Comp, PAnnotation);
  WriteCompString(Comp, CellToWellConductanceAnnotation);
  WriteCompString(Comp, PartialPenetrationAnnotation);
end;

procedure TMnw2Record.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);

  WellRadius := ReadCompReal(Decomp);
  SkinRadius := ReadCompReal(Decomp);
  SkinK := ReadCompReal(Decomp);
  B := ReadCompReal(Decomp);
  C := ReadCompReal(Decomp);
  P := ReadCompReal(Decomp);
  CellToWellConductance := ReadCompReal(Decomp);
  PartialPenetration := ReadCompReal(Decomp);

  WellRadiusAnnotation := ReadCompString(Decomp, Annotations);
  SkinRadiusAnnotation := ReadCompString(Decomp, Annotations);
  SkinKAnnotation := ReadCompString(Decomp, Annotations);
  BAnnotation := ReadCompString(Decomp, Annotations);
  CAnnotation := ReadCompString(Decomp, Annotations);
  PAnnotation := ReadCompString(Decomp, Annotations);
  CellToWellConductanceAnnotation := ReadCompString(Decomp, Annotations);
  PartialPenetrationAnnotation := ReadCompString(Decomp, Annotations);
end;

{ TMnw2Storage }

procedure TMnw2Storage.Clear;
begin
  SetLength(FMnw2Array, 0);
  FCleared := True;
end;

function TMnw2Storage.GetMnw2Array: TMnw2Array;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FMnw2Array;
end;

procedure TMnw2Storage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FMnw2Array, Count);
  for Index := 0 to Count - 1 do
  begin
    FMnw2Array[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TMnw2Storage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
begin
  Count := Length(FMnw2Array);
  Compressor.Write(Count, SizeOf(Count));
  for Index := 0 to Count - 1 do
  begin
    FMnw2Array[Index].Cache(Compressor);
  end;
end;

{ TMnw2TimeCollection }

procedure TMnw2TimeCollection.Evaluate;
var
  Index: Integer;
  Item: TMnw2TimeItem;
begin
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TMnw2TimeItem;
    Item.Evaluate;
  end;
end;

function TMnw2TimeCollection.IndexOfContainedStartTime(
  AStartTime: double): integer;
var
  Index: Integer;
  Item: TMnw2TimeItem;
begin
  result := -1;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TMnw2TimeItem;
    if (Item.StartTime <= AStartTime) and (Item.EndTime > AStartTime) then
    begin
      result := Index;
      Exit;
    end;
  end;
end;

class function TMnw2TimeCollection.ItemClass: TMF_BoundItemClass;
begin
  result := TMnw2TimeItem;
end;

{ TMnw2_Cell }

procedure TMnw2_Cell.Cache(Comp: TCompressionStream);
begin
  inherited;
  Values.Cache(Comp);
  WriteCompInt(Comp, StressPeriod);
end;

function TMnw2_Cell.GetB: double;
begin
  result := Values.B;
end;

function TMnw2_Cell.GetBAnnotation: string;
begin
  result := Values.BAnnotation;
end;

function TMnw2_Cell.GetC: double;
begin
  result := Values.C;
end;

function TMnw2_Cell.GetCAnnotation: string;
begin
  result := Values.CAnnotation;
end;

function TMnw2_Cell.GetCellToWellConductance: double;
begin
  result := Values.CellToWellConductance;
end;

function TMnw2_Cell.GetCellToWellConductanceAnnotation: string;
begin
  result := Values.CellToWellConductanceAnnotation;
end;

function TMnw2_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TMnw2_Cell.GetIntegerAnnotation(Index: integer): string;
begin
  result := '';
  Assert(False);
end;

function TMnw2_Cell.GetIntegerValue(Index: integer): integer;
begin
  result := 0;
  Assert(False);
end;

function TMnw2_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TMnw2_Cell.GetP: double;
begin
  result := Values.P;
end;

function TMnw2_Cell.GetPAnnotation: string;
begin
  result := Values.PAnnotation;
end;

function TMnw2_Cell.GetPartialPenetration: double;
begin
  result := Values.PartialPenetration;
end;

function TMnw2_Cell.GetPartialPenetrationAnnotation: string;
begin
  result := Values.PartialPenetrationAnnotation;
end;

function TMnw2_Cell.GetRealAnnotation(Index: integer): string;
begin
  result := '';
  case Index of
    WellRadiusPosition: result := WellRadiusAnnotation;
    SkinRadiusPosition: result := SkinRadiusAnnotation;
    SkinKPosition: result := SkinKAnnotation;
    BPosition: result := BAnnotation;
    CPosition: result := CAnnotation;
    PPosition: result := PAnnotation;
    CellToWellConductancePosition: result := CellToWellConductanceAnnotation;
    PartialPenetrationPosition: result := PartialPenetrationAnnotation;
    else Assert(False);
  end;
end;

function TMnw2_Cell.GetRealValue(Index: integer): double;
begin
  result := 0;
  case Index of
    WellRadiusPosition: result := WellRadius;
    SkinRadiusPosition: result := SkinRadius;
    SkinKPosition: result := SkinK;
    BPosition: result := B;
    CPosition: result := C;
    PPosition: result := P;
    CellToWellConductancePosition: result := CellToWellConductance;
    PartialPenetrationPosition: result := PartialPenetration;
    else Assert(False);
  end;
end;

function TMnw2_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TMnw2_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TMnw2_Cell.GetSkinK: double;
begin
  result := Values.SkinK;
end;

function TMnw2_Cell.GetSkinKAnnotation: string;
begin
  result := Values.SkinKAnnotation;
end;

function TMnw2_Cell.GetSkinRadius: double;
begin
  result := Values.SkinRadius;
end;

function TMnw2_Cell.GetSkinRadiusAnnotation: string;
begin
  result := Values.SkinRadiusAnnotation;
end;

function TMnw2_Cell.GetWellRadius: double;
begin
  result := Values.WellRadius;
end;

function TMnw2_Cell.GetWellRadiusAnnotation: string;
begin
  result := Values.WellRadiusAnnotation;
end;

procedure TMnw2_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

{ TLiftItem }

procedure TLiftItem.Assign(Source: TPersistent);
var
  SourceLift: TLiftItem;
begin
  if Source is TLiftItem then
  begin
    SourceLift := TLiftItem(Source);
    Lift := SourceLift.Lift;
    Q := SourceLift.Q;
  end;
  inherited;
end;

function TLiftItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherLift: TLiftItem;
begin
  result := (AnotherItem is TLiftItem);
  if result then
  begin
    OtherLift := TLiftItem(AnotherItem);
    result :=
      (Lift = OtherLift.Lift)
      and (Q = OtherLift.Q);
  end;
end;

procedure TLiftItem.SetLift(const Value: double);
begin
  if FLift <> Value then
  begin
    FLift := Value;
    InvalidateModel;
  end;
end;

procedure TLiftItem.SetQ(const Value: double);
begin
  if FQ <> Value then
  begin
    FQ := Value;
    InvalidateModel;
  end;
end;

{ TLiftCollection }

procedure TLiftCollection.Assign(Source: TPersistent);
begin
  inherited;
  Sort;
end;

constructor TLiftCollection.Create(Model: TObject);
begin
  inherited Create(TLiftItem, Model);
end;

function CompareLiftItems(Item1, Item2: Pointer): Integer;
var
  LiftItem1: TLiftItem;
  LiftItem2: TLiftItem;
begin
  LiftItem1 := Item1;
  LiftItem2 := Item2;
  result := Sign(LiftItem2.Lift - LiftItem1.Lift);
end;

procedure TLiftCollection.Sort;
var
  List: TList;
  Index: Integer;
  Item: TLiftItem;
begin
  List := TList.Create;
  try
    for Index := 0 to Count - 1 do
    begin
      List.Add(Items[Index]);
    end;
    List.Sort(CompareLiftItems);
    for Index := 0 to List.Count - 1 do
    begin
      Item := List[Index];
      Item.Index := Index;
    end;
  finally
    List.Free;
  end;
end;

{ TTargetCell }

procedure TTargetCell.Assign(Source: TPersistent);
var
  TC: TTargetCell;
begin
  if Source is TTargetCell then
  begin
    TC := TTargetCell(Source);
    Col := TC.Col;
    Row := TC.Row;
    Lay := TC.Lay;
  end
  else
  begin
    inherited;
  end;
end;

procedure TTargetCell.SetCol(const Value: integer);
begin
  if FCol <> Value then
  begin
    FCol := Value;
    InvalidateModel;
  end;
end;

procedure TTargetCell.SetLay(const Value: integer);
begin
  if FLay <> Value then
  begin
    FLay := Value;
    InvalidateModel;
  end;
end;

procedure TTargetCell.SetRow(const Value: integer);
begin
  if FRow <> Value then
  begin
    FRow := Value;
    InvalidateModel;
  end;
end;

{ TTargetLocation }

procedure TTargetLocation.Assign(Source: TPersistent);
var
  TL: TTargetLocation;
begin
  if Source is TTargetLocation then
  begin
    TL := TTargetLocation(Source);
    X := TL.X;
    Y := TL.Y;
    Z := TL.Z;
  end
  else
  begin
    inherited;
  end;
end;

procedure TTargetLocation.SetX(const Value: real);
begin
  if FX <> Value then
  begin
    FX := Value;
    InvalidateModel;
  end;
end;

procedure TTargetLocation.SetY(const Value: real);
begin
  if FY <> Value then
  begin
    FY := Value;
    InvalidateModel;
  end;
end;

procedure TTargetLocation.SetZ(const Value: real);
begin
  if FZ <> Value then
  begin
    FZ := Value;
    InvalidateModel;
  end;
end;

{ TTargetObject }

procedure TTargetObject.Assign(Source: TPersistent);
var
  TargetObject: TTargetObject;
begin
  if Source is TTargetObject then
  begin
    TargetObject := TTargetObject(Source);
    ScreenObject := TargetObject.ScreenObject;
    ObjectName := TargetObject.ObjectName;
  end
  else
  begin
    inherited;
  end;
end;

function TTargetObject.GetObjectName: string;
var
  ScreenObject: TScreenObject;
begin
  if FScreenObject = nil then
  begin
    result := FObjectName;
  end
  else
  begin
    if ValidScreenObject(FScreenObject) then
    begin
      ScreenObject := FScreenObject as TScreenObject;
      result := ScreenObject.Name;
    end
    else
    begin
      result := '';
    end;
  end;
end;

function TTargetObject.GetScreenObject: TObject;
begin
  if ValidScreenObject(FScreenObject) then
  begin
    result := FScreenObject;
  end
  else
  begin
    result := nil;
  end;
end;

procedure TTargetObject.SetObjectName(const Value: string);
begin
  if FObjectName <> Value then
  begin
    FObjectName := Value;
    InvalidateModel;
  end;
end;

procedure TTargetObject.SetScreenObject(const Value: TObject);
begin
  Assert((Value = nil) or (Value is TScreenObject));
  FScreenObject := Value;
  if FScreenObject = nil then
  begin
    ObjectName := '';
  end
  else
  begin
    ObjectName := TScreenObject(Value).Name;
  end;
end;

function TTargetObject.ValidScreenObject(AScreenObject: TObject): boolean;
var
  ScreenObject: TScreenObject;
begin
  result := (AScreenObject <> nil);
  if result then
  begin
    ScreenObject := AScreenObject as TScreenObject;
    result :=  (ScreenObject.Count= 1)
      and (ScreenObject.ElevationCount = ecOne)
      and not ScreenObject.Deleted;
  end;
end;

{ TTarget }

procedure TTarget.Assign(Source: TPersistent);
var
  SourceTarget: TTarget;
begin
  if Source is TTarget then
  begin
    SourceTarget := TTarget(Source);
    TargetType := SourceTarget.TargetType;
    TargetObject := SourceTarget.TargetObject;
    TargetLocation := SourceTarget.TargetLocation;
    TargetCell := SourceTarget.TargetCell;
  end
  else
  begin
    inherited;
  end;
end;

constructor TTarget.Create(Model: TObject);
begin
  FTargetObject:= TTargetObject.Create(Model);
  FTargetCell:= TTargetCell.Create(Model);
  FTargetLocation:= TTargetLocation.Create(Model);
  FTargetType := ttNone;
end;

destructor TTarget.Destroy;
begin
  FTargetLocation.Free;
  FTargetCell.Free;
  FTargetObject.Free;
  inherited;
end;

procedure TTarget.SetTargetCell(const Value: TTargetCell);
begin
  FTargetCell.Assign(Value);
end;

procedure TTarget.SetTargetLocation(const Value: TTargetLocation);
begin
  FTargetLocation.Assign(Value);
end;

procedure TTarget.SetTargetObject(const Value: TTargetObject);
begin
  FTargetObject.Assign(Value);
end;

procedure TTarget.SetTargetType(const Value: TTargetType);
begin
  FTargetType := Value;
end;

function TTarget.StoreTargetCell: Boolean;
begin
  result := TargetType = ttCell;
end;

function TTarget.StoreTargetLocation: Boolean;
begin
  result := TargetType = ttLocation;
end;

function TTarget.StoreTargetObject: Boolean;
begin
  result := TargetType = ttObject;
end;

{ TVerticalScreen }

procedure TVerticalScreen.Assign(Source: TPersistent);
var
  SourceScreen: TVerticalScreen;
begin
  if Source is TVerticalScreen then
  begin
    SourceScreen := TVerticalScreen(Source);
    ZTop := SourceScreen.ZTop;
    ZBottom := SourceScreen.ZBottom;
    WellRadius := SourceScreen.WellRadius;
    SkinRadius := SourceScreen.SkinRadius;
    SkinK := SourceScreen.SkinK;
    B := SourceScreen.B;
    C := SourceScreen.C;
    P := SourceScreen.P;
    CellToWellConductance := SourceScreen.CellToWellConductance;
  end;
  inherited;
end;

procedure TVerticalScreen.AssignObserverEvents(Collection: TCollection);
begin

end;

function TVerticalScreen.BoundaryFormulaCount: integer;
begin
  result := 7;
end;

procedure TVerticalScreen.CreateFormulaObjects;
begin
  FB := CreateFormulaObject(dso3D);
  FC := CreateFormulaObject(dso3D);
  FCellToWellConductance := CreateFormulaObject(dso3D);
  FP := CreateFormulaObject(dso3D);
  FSkinK := CreateFormulaObject(dso3D);
  FSkinRadius := CreateFormulaObject(dso3D);
  FWellRadius := CreateFormulaObject(dso3D);
end;

function TVerticalScreen.GetB: string;
begin
  Result := FB.Formula;
  ResetItemObserver(BPosition);
end;

function TVerticalScreen.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    WellRadiusPosition:
      result := WellRadius;
    SkinRadiusPosition:
      result := SkinRadius;
    SkinKPosition:
      result := SkinK;
    BPosition:
      result := B;
    CPosition:
      result := C;
    PPosition:
      result := P;
    CellToWellConductancePosition:
      result := CellToWellConductance;
    else Assert(False);
  end;
end;

function TVerticalScreen.GetC: string;
begin
  Result := FC.Formula;
  ResetItemObserver(CPosition);
end;

function TVerticalScreen.GetCellToWellConductance: string;
begin
  Result := FCellToWellConductance.Formula;
  ResetItemObserver(CellToWellConductancePosition);
end;

function TVerticalScreen.GetP: string;
begin
  Result := FP.Formula;
  ResetItemObserver(PPosition);
end;

procedure TVerticalScreen.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FB then
  begin
    List.Add(FObserverList[BPosition]);
  end;
  if Sender = FC then
  begin
    List.Add(FObserverList[CPosition]);
  end;
  if Sender = FCellToWellConductance then
  begin
    List.Add(FObserverList[CellToWellConductancePosition]);
  end;
  if Sender = FP then
  begin
    List.Add(FObserverList[PPosition]);
  end;
  if Sender = FSkinK then
  begin
    List.Add(FObserverList[SkinKPosition]);
  end;
  if Sender = FSkinRadius then
  begin
    List.Add(FObserverList[SkinRadiusPosition]);
  end;
  if Sender = FWellRadius then
  begin
    List.Add(FObserverList[WellRadiusPosition]);
  end;
end;

function TVerticalScreen.GetSkinK: string;
begin
  Result := FSkinK.Formula;
  ResetItemObserver(SkinKPosition);
end;

function TVerticalScreen.GetSkinRadius: string;
begin
  Result := FSkinRadius.Formula;
  ResetItemObserver(SkinRadiusPosition);
end;

function TVerticalScreen.GetWellRadius: string;
begin
  Result := FWellRadius.Formula;
  ResetItemObserver(WellRadiusPosition);
end;

function TVerticalScreen.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherScreen: TVerticalScreen;
begin
  result := (AnotherItem is TVerticalScreen);
  if result then
  begin
    OtherScreen := TVerticalScreen(AnotherItem);
    result :=
      (ZTop = OtherScreen.ZTop)
      and (ZBottom = OtherScreen.ZBottom)
      and (WellRadius = OtherScreen.WellRadius)
      and (SkinRadius = OtherScreen.SkinRadius)
      and (SkinK = OtherScreen.SkinK)
      and (B = OtherScreen.B)
      and (C = OtherScreen.C)
      and (P = OtherScreen.P)
      and (CellToWellConductance = OtherScreen.CellToWellConductance)
  end;
end;

procedure TVerticalScreen.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FB,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FC,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FCellToWellConductance,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FP,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FSkinK,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FSkinRadius,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);

  frmGoPhast.PhastModel.FormulaManager.Remove(FWellRadius,
    GlobalRemoveModflowBoundarySubscription,
    GlobalRestoreModflowBoundarySubscription, self);
end;

function TVerticalScreen.ScreenObject: TObject;
begin
  result := (Collection as TVerticalScreenCollection).ScreenObject;
end;

procedure TVerticalScreen.SetB(const Value: string);
begin
  UpdateFormula(Value, BPosition, FB);
end;

procedure TVerticalScreen.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    WellRadiusPosition:
      WellRadius := Value;
    SkinRadiusPosition:
      SkinRadius := Value;
    SkinKPosition:
      SkinK := Value;
    BPosition:
      B := Value;
    CPosition:
      C := Value;
    PPosition:
      P := Value;
    CellToWellConductancePosition:
      CellToWellConductance := Value;
    else Assert(False);
  end;
end;

procedure TVerticalScreen.SetC(const Value: string);
begin
  UpdateFormula(Value, CPosition, FC);
end;

procedure TVerticalScreen.SetCellToWellConductance(const Value: string);
begin
  UpdateFormula(Value, CellToWellConductancePosition, FCellToWellConductance);
end;

procedure TVerticalScreen.SetP(const Value: string);
begin
  UpdateFormula(Value, PPosition, FP);
end;

procedure TVerticalScreen.SetSkinK(const Value: string);
begin
  UpdateFormula(Value, SkinKPosition, FSkinK);
end;

procedure TVerticalScreen.SetSkinRadius(const Value: string);
begin
  UpdateFormula(Value, SkinRadiusPosition, FSkinRadius);
end;

procedure TVerticalScreen.SetWellRadius(const Value: string);
begin
  UpdateFormula(Value, WellRadiusPosition, FWellRadius);
end;

procedure TVerticalScreen.SetZBottom(const Value: double);
begin
  if FZBottom <> Value then
  begin
    FZBottom := Value;
    InvalidateModel;
  end;
end;

procedure TVerticalScreen.SetZTop(const Value: double);
begin
  if FZTop <> Value then
  begin
    FZTop := Value;
    InvalidateModel;
  end;
end;

{ TVerticalScreenCollection }

function CompareVerticalScreenItems(Item1, Item2: Pointer): Integer;
var
  VerticalScreen1: TVerticalScreen;
  VerticalScreen2: TVerticalScreen;
begin
  VerticalScreen1 := Item1;
  VerticalScreen2 := Item2;
  result := Sign(VerticalScreen2.ZTop - VerticalScreen1.ZTop);
  if result = 0 then
  begin
    result := Sign(VerticalScreen2.ZBottom - VerticalScreen1.ZBottom);
  end;
end;

class function TVerticalScreenCollection.ItemClass: TMF_BoundItemClass;
begin
  result := TVerticalScreen;
end;

procedure TVerticalScreenCollection.Sort;
var
  List: TList;
  Index: Integer;
  Item: TLiftItem;
begin
  List := TList.Create;
  try
    for Index := 0 to Count - 1 do
    begin
      List.Add(Items[Index]);
    end;
    List.Sort(CompareVerticalScreenItems);
    for Index := 0 to List.Count - 1 do
    begin
      Item := List[Index];
      Item.Index := Index;
    end;
  finally
    List.Free;
  end;
end;

end.
