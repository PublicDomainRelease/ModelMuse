unit ModflowLakUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, ModflowCellUnit, DataSetUnit, FormulaManagerUnit,
  SubscriptionUnit;

type
  TLakRecord = record
    Cell: TCellLocation;
    MinimumStage: double;
    MaximumStage: double;
    Precipitation: double;
    Evaporation: double;
    OverlandRunoff: double;
    Withdrawal: double;
    MinimumStageAnnotation: string;
    MaximumStageAnnotation: string;
    PrecipitationAnnotation: string;
    EvaporationAnnotation: string;
    OverlandRunoffAnnotation: string;
    WithdrawalAnnotation: string;
    procedure Cache(Comp: TCompressionStream);
    procedure Restore(Decomp: TDecompressionStream);
  end;

  TLakArray = array of TLakRecord;

  TLakStorage = class(TCustomBoundaryStorage)
  private
    FLakArray: TLakArray;
    function GetLakArray: TLakArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property LakArray: TLakArray read GetLakArray;
  end;

  // @name represents a MODFLOW Drain boundary for one time interval.
  // @name is stored by @link(TResCollection).
  TLakItem = class(TCustomModflowBoundaryItem)
  private
    FMaximumStage: TFormulaObject;
    FPrecipitation: TFormulaObject;
    FMinimumStage: TFormulaObject;
    FWithdrawal: TFormulaObject;
    FOverlandRunoff: TFormulaObject;
    FEvaporation: TFormulaObject;
    procedure SetEvaporation(const Value: string);
    procedure SetMaximumStage(const Value: string);
    procedure SetMinimumStage(const Value: string);
    procedure SetOverlandRunoff(const Value: string);
    procedure SetPrecipitation(const Value: string);
    procedure SetWithdrawal(const Value: string);
    function ConvertString(Const AString: string): double;
    function GetEvaporation: string;
    function GetMaximumStage: string;
    function GetMinimumStage: string;
    function GetOverlandRunoff: string;
    function GetPrecipitation: string;
    function GetWithdrawal: string;
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    // See @link(TCustomModflowBoundaryItem.BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(TCustomModflowBoundaryItem.BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    // @name checks whether AnotherItem is the same as the current @classname.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function BoundaryFormulaCount: integer; override;
  public
    function SSMN: double;
    function SSMX: double;
    function PRCPLK: double;
    function EVAPLK: double;
    function RNF: double;
    function WTHDRW: double;
    Destructor Destroy; override;
  published
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    property MinimumStage: string read GetMinimumStage write SetMinimumStage;
    property MaximumStage: string read GetMaximumStage write SetMaximumStage;
    property Precipitation: string read GetPrecipitation write SetPrecipitation;
    property Evaporation: string read GetEvaporation write SetEvaporation;
    property OverlandRunoff: string read GetOverlandRunoff write SetOverlandRunoff;
    property Withdrawal: string read GetWithdrawal write SetWithdrawal;
  end;

  TLak_Cell = class(TValueCell)
  private
    Values: TLakRecord;
    StressPeriod: integer;
    function GetEvaporation: double;
    function GetMaximumStage: double;
    function GetMinimumStage: double;
    function GetOverlandRunoff: double;
    function GetPrecipitation: double;
    function GetWithdrawal: double;
    function GetEvaporationAnnotation: string;
    function GetMaximumStageAnnotation: string;
    function GetMinimumStageAnnotation: string;
    function GetOverlandRunoffAnnotation: string;
    function GetPrecipitationAnnotation: string;
    function GetWithdrawalAnnotation: string;
  protected
    function GetColumn: integer; override;
    function GetLayer: integer; override;
    function GetRow: integer; override;
    function GetIntegerValue(Index: integer): integer; override;
    function GetRealValue(Index: integer): double; override;
    function GetRealAnnotation(Index: integer): string; override;
    function GetIntegerAnnotation(Index: integer): string; override;
    procedure Cache(Comp: TCompressionStream); override;
    procedure Restore(Decomp: TDecompressionStream); override;
    function GetSection: integer; override;
  public
    property MinimumStage: double read GetMinimumStage;
    property MaximumStage: double read GetMaximumStage;
    property Precipitation: double read GetPrecipitation;
    property Evaporation: double read GetEvaporation;
    property OverlandRunoff: double read GetOverlandRunoff;
    property Withdrawal: double read GetWithdrawal;
    property MinimumStageAnnotation: string read GetMinimumStageAnnotation;
    property MaximumStageAnnotation: string read GetMaximumStageAnnotation;
    property PrecipitationAnnotation: string read GetPrecipitationAnnotation;
    property EvaporationAnnotation: string read GetEvaporationAnnotation;
    property OverlandRunoffAnnotation: string read GetOverlandRunoffAnnotation;
    property WithdrawalAnnotation: string read GetWithdrawalAnnotation;
  end;


  // @name represents MODFLOW lake boundaries
  // for a series of time intervals.
  TLakCollection = class(TCustomMF_ArrayBoundColl)
  private
    // @name is used to compute the Elevations for a series of
    // Drain Boundaries over a series of time intervals.
    FMinimumStageData: TModflowTimeList;
    FMaximumStageData: TModflowTimeList;
    FPrecipitationData: TModflowTimeList;
    FEvaporationData: TModflowTimeList;
    FOverlandRunoffData: TModflowTimeList;
    FWithdrawalData: TModflowTimeList;
    procedure InvalidateMinStageData(Sender: TObject);
    procedure InvalidateMaxStageData(Sender: TObject);
    procedure InvalidatePrecipData(Sender: TObject);
    procedure InvalidateEvapData(Sender: TObject);
    procedure InvalidateRunoffData(Sender: TObject);
    procedure InvalidateWithdrawalData(Sender: TObject);
  protected
    procedure AddSpecificBoundary; override;
    // See @link(TCustomMF_ArrayBoundColl.AssignCellValues
    // TCustomMF_ArrayBoundColl.AssignCellValues)
    procedure AssignCellValues(DataSets: TList; ItemIndex: Integer); override;
    // See @link(TCustomMF_ArrayBoundColl.InitializeTimeLists
    // TCustomMF_ArrayBoundColl.InitializeTimeLists)
    procedure InitializeTimeLists(ListOfTimeLists: TList); override;
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TMF_BoundItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TResStorage.ResArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer); override;
  public
    // @name creates an instance of @classname
    constructor Create(Boundary: TModflowBoundary; Model,
      ScreenObject: TObject); override;
    // @name destroys the current instance of @classname.
    // Do not call @name; call Free instead.
    destructor Destroy; override;
  end;

  TLakBoundary = class(TModflowBoundary)
  private
    FSill: double;
    FInitialStage: double;
    FCenterLake: integer;
    FLakeID: integer;
    FSubLakes: TList;
    FTrueLakeID: integer;
    FFluxCondGage: boolean;
    FStandardGage: boolean;
    FDeltaGage: boolean;
    FGage4: boolean;
    procedure SetCenterLake(const Value: integer);
    procedure SetInitialStage(const Value: double);
    procedure SetSill(const Value: double);
    procedure SetLakeID(const Value: integer);
    function GetSubLakeCount: integer;
    function GetSubLake(Index: Integer): TObject;
    procedure SetDeltaGage(const Value: boolean);
    procedure SetFluxCondGage(const Value: boolean);
    procedure SetStandardGage(const Value: boolean);
    function GetOutType: integer;
    procedure SetGage4(const Value: boolean);
  protected
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList);
      override;
    Constructor Create(Model, ScreenObject: TObject);
    // @name destroys the current instance of @classname.  Do not call
    // @name directly.  Call Free instead.
    Destructor Destroy; override;
    property SubLakeCount: integer read GetSubLakeCount;
    property SubLakes[Index: integer]: TObject read GetSubLake;
    procedure ClearSubLakes;
    procedure AddSubLake(Lake: TObject);
    property TrueLakeID: integer read FTrueLakeID write FTrueLakeID;
    procedure DeleteSubLake(Index: integer);
    property OutType: integer read GetOutType;
  published
    property InitialStage: double read FInitialStage write SetInitialStage;
    property CenterLake: integer read FCenterLake write SetCenterLake;
    property Sill: double read FSill write SetSill;
    property LakeID: integer read FLakeID write SetLakeID;
    property StandardGage: boolean read FStandardGage write SetStandardGage;
    property FluxCondGage: boolean read FFluxCondGage write SetFluxCondGage;
    property DeltaGage: boolean read FDeltaGage write SetDeltaGage;
    property Gage4: boolean read FGage4 write SetGage4;
  end;

implementation

uses RbwParser, ScreenObjectUnit, PhastModelUnit, ModflowTimeUnit,
  ModflowTransientListParameterUnit, TempFiles, GoPhastTypes, 
  frmFormulaErrorsUnit, frmGoPhastUnit;

const
  MinimumStagePosition = 0;
  MaximumStagePosition = 1;
  PrecipitationPosition = 2;
  EvaporationPosition = 3;
  OverlandRunoffPosition = 4;
  WithdrawalPosition = 5;

{ TLakItem }

procedure TLakItem.Assign(Source: TPersistent);
var
  Lak: TLakItem;
begin
  if Source is TLakItem then
  begin
    Lak := TLakItem(Source);
    MinimumStage := Lak.MinimumStage;
    MaximumStage := Lak.MaximumStage;
    Precipitation := Lak.Precipitation;
    Evaporation := Lak.Evaporation;
    OverlandRunoff := Lak.OverlandRunoff;
    Withdrawal := Lak.Withdrawal;
  end;
  inherited;
end;

procedure TLakItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TLakCollection;
  MinStageObserver: TObserver;
  MaxStageObserver: TObserver;
  PrecipObserver: TObserver;
  EvapObserver: TObserver;
  RunoffObserver: TObserver;
  WithdrawalObserver: TObserver;
begin
  ParentCollection := Collection as TLakCollection;
  MinStageObserver := FObserverList[MinimumStagePosition];
  MinStageObserver.OnUpToDateSet := ParentCollection.InvalidateMinStageData;
  MaxStageObserver := FObserverList[MaximumStagePosition];
  MaxStageObserver.OnUpToDateSet := ParentCollection.InvalidateMaxStageData;
  PrecipObserver := FObserverList[PrecipitationPosition];
  PrecipObserver.OnUpToDateSet := ParentCollection.InvalidatePrecipData;
  EvapObserver := FObserverList[EvaporationPosition];
  EvapObserver.OnUpToDateSet := ParentCollection.InvalidateEvapData;
  RunoffObserver := FObserverList[OverlandRunoffPosition];
  RunoffObserver.OnUpToDateSet := ParentCollection.InvalidateRunoffData;
  WithdrawalObserver := FObserverList[WithdrawalPosition];
  WithdrawalObserver.OnUpToDateSet := ParentCollection.InvalidateWithdrawalData;
end;

function TLakItem.BoundaryFormulaCount: integer;
begin
  result := 6;
end;

function TLakItem.ConvertString(const AString: string): double;
var
  Compiler: TRbwParser;
  Model: TPhastModel;
  OrderedCollection: TOrderedCollection;
  TempFormula: string;
begin
  OrderedCollection := Collection as TOrderedCollection;
  Model := OrderedCollection.Model as TPhastModel;
  Compiler := Model.rpThreeDFormulaCompiler;

  TempFormula := AString;
  Compiler.Compile(TempFormula);
  Compiler.CurrentExpression.Evaluate;
  result := Compiler.CurrentExpression.DoubleResult;
end;

procedure TLakItem.CreateFormulaObjects;
begin
  inherited;
  FMinimumStage := CreateFormulaObject(dso3D);
  FMaximumStage := CreateFormulaObject(dso3D);
  FPrecipitation := CreateFormulaObject(dso3D);
  FEvaporation := CreateFormulaObject(dso3D);
  FOverlandRunoff := CreateFormulaObject(dso3D);
  FWithdrawal := CreateFormulaObject(dso3D);
end;

destructor TLakItem.Destroy;
begin
  MinimumStage := '0';
  MaximumStage := '0';
  Precipitation := '0';
  Evaporation := '0';
  OverlandRunoff := '0';
  Withdrawal := '0';
  inherited;
end;

function TLakItem.EVAPLK: double;
var
  LocalScreenObject: TScreenObject;
begin
  try
    result := ConvertString(Evaporation);
  except on E: ERbwParserError do
    begin
      LocalScreenObject := ScreenObject as TScreenObject;
      frmFormulaErrors.AddError(LocalScreenObject.Name,
        '(evaporation for the Lake package)',
        Evaporation, E.Message);
      Evaporation := '0.';
      result := ConvertString(Evaporation);
    end;
  end;
end;

function TLakItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    MinimumStagePosition: result := MinimumStage;
    MaximumStagePosition: result := MaximumStage;
    PrecipitationPosition: result := Precipitation;
    EvaporationPosition: result := Evaporation;
    OverlandRunoffPosition: result := OverlandRunoff;
    WithdrawalPosition: result := Withdrawal;
    else Assert(False);
  end;
end;

function TLakItem.GetEvaporation: string;
begin
  Result := FEvaporation.Formula;
  ResetItemObserver(EvaporationPosition);
end;

function TLakItem.GetMaximumStage: string;
begin
  Result := FMaximumStage.Formula;
  ResetItemObserver(MaximumStagePosition);
end;

function TLakItem.GetMinimumStage: string;
begin
  Result := FMinimumStage.Formula;
  ResetItemObserver(MinimumStagePosition);
end;

function TLakItem.GetOverlandRunoff: string;
begin
  Result := FOverlandRunoff.Formula;
  ResetItemObserver(OverlandRunoffPosition);
end;

function TLakItem.GetPrecipitation: string;
begin
  Result := FPrecipitation.Formula;
  ResetItemObserver(PrecipitationPosition);
end;

procedure TLakItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FMinimumStage then
  begin
    List.Add(FObserverList[MinimumStagePosition]);
  end;
  if Sender = FMaximumStage then
  begin
    List.Add(FObserverList[MaximumStagePosition]);
  end;
  if Sender = FPrecipitation then
  begin
    List.Add(FObserverList[PrecipitationPosition]);
  end;
  if Sender = FEvaporation then
  begin
    List.Add(FObserverList[EvaporationPosition]);
  end;
  if Sender = FOverlandRunoff then
  begin
    List.Add(FObserverList[OverlandRunoffPosition]);
  end;
  if Sender = FWithdrawal then
  begin
    List.Add(FObserverList[WithdrawalPosition]);
  end;
end;

function TLakItem.GetWithdrawal: string;
begin
  Result := FWithdrawal.Formula;
  ResetItemObserver(WithdrawalPosition);
end;

function TLakItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TLakItem;
begin
  result := (AnotherItem is TLakItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TLakItem(AnotherItem);
    result :=
      (Item.MinimumStage = MinimumStage)
      and (Item.MaximumStage = MaximumStage)
      and (Item.Precipitation = Precipitation)
      and (Item.Evaporation = Evaporation)
      and (Item.OverlandRunoff = OverlandRunoff)
      and (Item.Withdrawal = Withdrawal);
  end;
end;

function TLakItem.PRCPLK: double;
var
  LocalScreenObject: TScreenObject;
begin
  try
    result := ConvertString(Precipitation);
  except on E: ERbwParserError do
    begin
      LocalScreenObject := ScreenObject as TScreenObject;
      frmFormulaErrors.AddError(LocalScreenObject.Name,
        '(precipitation for the Lake package)',
        Precipitation, E.Message);
      Precipitation := '0.';
      result := ConvertString(Precipitation);
    end;
  end;
end;

procedure TLakItem.RemoveFormulaObjects;
begin
  inherited;
  frmGoPhast.PhastModel.FormulaManager.Remove(FWithdrawal,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FOverlandRunoff,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FEvaporation,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FPrecipitation,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FMaximumStage,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FMinimumStage,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
end;

function TLakItem.RNF: double;
var
  LocalScreenObject: TScreenObject;
begin
  try
    result := ConvertString(OverlandRunoff);
  except on E: ERbwParserError do
    begin
      LocalScreenObject := ScreenObject as TScreenObject;
      frmFormulaErrors.AddError(LocalScreenObject.Name,
        '(runoff for the Lake package)',
        OverlandRunoff, E.Message);
      OverlandRunoff := '0.';
      result := ConvertString(OverlandRunoff);
    end;
  end;
end;

procedure TLakItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  inherited;
  case Index of
    MinimumStagePosition: MinimumStage := Value;
    MaximumStagePosition: MaximumStage := Value;
    PrecipitationPosition: Precipitation := Value;
    EvaporationPosition: Evaporation := Value;
    OverlandRunoffPosition: OverlandRunoff := Value;
    WithdrawalPosition: Withdrawal := Value;
    else Assert(False);
  end;
end;

procedure TLakItem.SetEvaporation(const Value: string);
begin
  UpdateFormula(Value, EvaporationPosition, FEvaporation);
end;

procedure TLakItem.SetMaximumStage(const Value: string);
begin
//  MinimumStagePosition = 0;
//  MaximumStagePosition = 1;
//  PrecipitationPosition = 2;
//  EvaporationPosition = 3;
//  OverlandRunoffPosition = 4;
//  WithdrawalPosition = 5;

//    FMaximumStage: TFormulaObject;
//    FPrecipitation: TFormulaObject;
//    FMinimumStage: TFormulaObject;
//    FWithdrawal: TFormulaObject;
//    FOverlandRunoff: TFormulaObject;
//    FEvaporation: TFormulaObject;
  UpdateFormula(Value, MaximumStagePosition, FMaximumStage);
end;

procedure TLakItem.SetMinimumStage(const Value: string);
begin
  UpdateFormula(Value, MinimumStagePosition, FMinimumStage);
end;

procedure TLakItem.SetOverlandRunoff(const Value: string);
begin
  UpdateFormula(Value, OverlandRunoffPosition, FOverlandRunoff);
end;

procedure TLakItem.SetPrecipitation(const Value: string);
begin
  UpdateFormula(Value, PrecipitationPosition, FPrecipitation);
end;

procedure TLakItem.SetWithdrawal(const Value: string);
begin
  UpdateFormula(Value, WithdrawalPosition, FWithdrawal);
end;

function TLakItem.SSMN: double;
var
  LocalScreenObject: TScreenObject;
begin
  try
    result := ConvertString(MinimumStage);
  except on E: ERbwParserError do
    begin
      LocalScreenObject := ScreenObject as TScreenObject;
      frmFormulaErrors.AddError(LocalScreenObject.Name,
        '(minimum stage for the Lake package)',
        MinimumStage, E.Message);
      MinimumStage := '0.';
      result := ConvertString(MinimumStage);
    end;
  end;
end;

function TLakItem.SSMX: double;
var
  LocalScreenObject: TScreenObject;
begin
  try
    result := ConvertString(MaximumStage);
  except on E: ERbwParserError do
    begin
      LocalScreenObject := ScreenObject as TScreenObject;
      frmFormulaErrors.AddError(LocalScreenObject.Name,
        '(maximum stage for the Lake package)',
        MaximumStage, E.Message);
      MaximumStage := '0.';
      result := ConvertString(MaximumStage);
    end;
  end;
end;

function TLakItem.WTHDRW: double;
var
  LocalScreenObject: TScreenObject;
begin
  try
  result := ConvertString(Withdrawal);
  except on E: ERbwParserError do
    begin
      LocalScreenObject := ScreenObject as TScreenObject;
      frmFormulaErrors.AddError(LocalScreenObject.Name,
        '(withdrawal for the Lake package)',
        Withdrawal, E.Message);
      Withdrawal := '0.';
      result := ConvertString(Withdrawal);
    end;
  end;
end;

{ TLak_Cell }

procedure TLak_Cell.Cache(Comp: TCompressionStream);
begin
  inherited;
  Values.Cache(Comp);
  WriteCompInt(Comp, StressPeriod);
end;

function TLak_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TLak_Cell.GetEvaporation: double;
begin
  result := Values.Evaporation;
end;

function TLak_Cell.GetEvaporationAnnotation: string;
begin
  result := Values.EvaporationAnnotation;
end;

function TLak_Cell.GetIntegerAnnotation(Index: integer): string;
begin
  result := '';
  Assert(False);
end;

function TLak_Cell.GetIntegerValue(Index: integer): integer;
begin
  result := 0;
  Assert(False);
end;

function TLak_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TLak_Cell.GetMaximumStage: double;
begin
  result := Values.MaximumStage;
end;

function TLak_Cell.GetMaximumStageAnnotation: string;
begin
  result := Values.MaximumStageAnnotation;
end;

function TLak_Cell.GetMinimumStage: double;
begin
  result := Values.MinimumStage;
end;

function TLak_Cell.GetMinimumStageAnnotation: string;
begin
  result := Values.MinimumStageAnnotation;
end;

function TLak_Cell.GetOverlandRunoff: double;
begin
  result := Values.OverlandRunoff;
end;

function TLak_Cell.GetOverlandRunoffAnnotation: string;
begin
  result := Values.OverlandRunoffAnnotation;
end;

function TLak_Cell.GetPrecipitation: double;
begin
  result := Values.Precipitation;
end;

function TLak_Cell.GetPrecipitationAnnotation: string;
begin
  result := Values.PrecipitationAnnotation;
end;

function TLak_Cell.GetRealAnnotation(Index: integer): string;
begin
  result := '';
  case Index of
    0: result := Values.MinimumStageAnnotation;
    1: result := Values.MaximumStageAnnotation;
    2: result := Values.PrecipitationAnnotation;
    3: result := Values.EvaporationAnnotation;
    4: result := Values.OverlandRunoffAnnotation;
    5: result := Values.WithdrawalAnnotation;
    else Assert(False);
  end;
end;

function TLak_Cell.GetRealValue(Index: integer): double;
begin
  result := 0;
  case Index of
    0: result := Values.MinimumStage;
    1: result := Values.MaximumStage;
    2: result := Values.Precipitation;
    3: result := Values.Evaporation;
    4: result := Values.OverlandRunoff;
    5: result := Values.Withdrawal;
    else Assert(False);
  end;
end;

function TLak_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TLak_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TLak_Cell.GetWithdrawal: double;
begin
  result := Values.Withdrawal;
end;

function TLak_Cell.GetWithdrawalAnnotation: string;
begin
  result := Values.WithdrawalAnnotation;
end;

procedure TLak_Cell.Restore(Decomp: TDecompressionStream);
begin
  inherited;
  Values.Restore(Decomp); 
  StressPeriod := ReadCompInt(Decomp);
end;

{ TLakCollection }

procedure TLakCollection.AddSpecificBoundary;
begin
  AddBoundary(TLakStorage.Create);
end;

procedure TLakCollection.AssignCellValues(DataSets: TList;
  ItemIndex: Integer);
var
  MinimumStageArray: TDataArray;
  MaximumStageArray: TDataArray;
  PrecipitationArray: TDataArray;
  EvaporationArray: TDataArray;
  OverlandRunoffArray: TDataArray;
  WithdrawalArray: TDataArray;
  Boundary: TLakStorage;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  BoundaryIndex: Integer;
  LocalModel: TPhastModel;
  LayerMin: Integer;
  RowMin: Integer;
  ColMin: Integer;
  LayerMax: Integer;
  RowMax: Integer;
  ColMax: Integer;
begin
  LocalModel := Model as TPhastModel;
  BoundaryIndex := 0;
  MinimumStageArray := DataSets[0];
  MaximumStageArray := DataSets[1];
  PrecipitationArray := DataSets[2];
  EvaporationArray := DataSets[3];
  OverlandRunoffArray := DataSets[4];
  WithdrawalArray := DataSets[5];
  Boundary := Boundaries[ItemIndex] as TLakStorage;
  MinimumStageArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
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
            if MinimumStageArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              Assert(MaximumStageArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(PrecipitationArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(EvaporationArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(OverlandRunoffArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(WithdrawalArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              with Boundary.LakArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                MinimumStage := MinimumStageArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                MinimumStageAnnotation := MinimumStageArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];

                MaximumStage := MaximumStageArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                MaximumStageAnnotation := MaximumStageArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];

                Precipitation := PrecipitationArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                PrecipitationAnnotation := PrecipitationArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];

                Evaporation := EvaporationArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                EvaporationAnnotation := EvaporationArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];

                OverlandRunoff := OverlandRunoffArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                OverlandRunoffAnnotation := OverlandRunoffArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];

                Withdrawal := WithdrawalArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                WithdrawalAnnotation := WithdrawalArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  MinimumStageArray.CacheData;
  MaximumStageArray.CacheData;
  PrecipitationArray.CacheData;
  EvaporationArray.CacheData;
  OverlandRunoffArray.CacheData;
  WithdrawalArray.CacheData;
  Boundary.CacheData;
end;

constructor TLakCollection.Create(Boundary: TModflowBoundary; Model,
  ScreenObject: TObject);
begin
  inherited Create(Boundary, Model, ScreenObject);
  FMinimumStageData := TModflowTimeList.Create(Model);
  FMaximumStageData := TModflowTimeList.Create(Model);
  FPrecipitationData := TModflowTimeList.Create(Model);
  FEvaporationData := TModflowTimeList.Create(Model);
  FOverlandRunoffData := TModflowTimeList.Create(Model);
  FWithdrawalData := TModflowTimeList.Create(Model);

  FMinimumStageData.NonParamDescription := 'Minimum stage';
  FMinimumStageData.ParamDescription := ' minimum stage';

  FMaximumStageData.NonParamDescription := 'Maximum stage';
  FMaximumStageData.ParamDescription := ' maximum stage';

  FPrecipitationData.NonParamDescription := 'Precipitaton';
  FPrecipitationData.ParamDescription := ' precipitaton';

  FEvaporationData.NonParamDescription := 'Evaporation';
  FEvaporationData.ParamDescription := ' evaporation';

  FOverlandRunoffData.NonParamDescription := 'Overland runoff';
  FOverlandRunoffData.ParamDescription := ' overland runoff';
  
  FWithdrawalData.NonParamDescription := 'Withdrawal';
  FWithdrawalData.ParamDescription := ' withdrawal';

  FMinimumStageData.DataType := rdtDouble;
  FMaximumStageData.DataType := rdtDouble;
  FPrecipitationData.DataType := rdtDouble;
  FEvaporationData.DataType := rdtDouble;
  FOverlandRunoffData.DataType := rdtDouble;
  FWithdrawalData.DataType := rdtDouble;

  AddTimeList(FMinimumStageData);
  AddTimeList(FMaximumStageData);
  AddTimeList(FPrecipitationData);
  AddTimeList(FEvaporationData);
  AddTimeList(FOverlandRunoffData);
  AddTimeList(FWithdrawalData);
end;

destructor TLakCollection.Destroy;
begin
  FMinimumStageData.Free;
  FMaximumStageData.Free;
  FPrecipitationData.Free;
  FEvaporationData.Free;
  FOverlandRunoffData.Free;
  FWithdrawalData.Free;

  inherited;
end;

procedure TLakCollection.InitializeTimeLists(ListOfTimeLists: TList);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TLakItem;
  Boundary: TLakBoundary;
  ScreenObject: TScreenObject;
begin
  Boundary := BoundaryGroup as TLakBoundary;
  ScreenObject := Boundary.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TLakItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.MinimumStage;
  end;
  FMinimumStageData.Initialize(BoundaryValues, ScreenObject);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TLakItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.MaximumStage;
  end;
  FMaximumStageData.Initialize(BoundaryValues, ScreenObject);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TLakItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.Precipitation;
  end;
  FPrecipitationData.Initialize(BoundaryValues, ScreenObject);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TLakItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.Evaporation;
  end;
  FEvaporationData.Initialize(BoundaryValues, ScreenObject);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TLakItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.OverlandRunoff;
  end;
  FOverlandRunoffData.Initialize(BoundaryValues, ScreenObject);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TLakItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.Withdrawal;
  end;
  FWithdrawalData.Initialize(BoundaryValues, ScreenObject);

  Assert(FMinimumStageData.Count = Count);
  Assert(FMaximumStageData.Count = Count);
  Assert(FPrecipitationData.Count = Count);
  Assert(FEvaporationData.Count = Count);
  Assert(FOverlandRunoffData.Count = Count);
  Assert(FWithdrawalData.Count = Count);

  ClearBoundaries;
  SetBoundaryCapacity(FMinimumStageData.Count);
  for TimeIndex := 0 to FMinimumStageData.Count - 1 do
  begin
    AddBoundary(TLakStorage.Create);
  end;

  ListOfTimeLists.Add(FMinimumStageData);
  ListOfTimeLists.Add(FMaximumStageData);
  ListOfTimeLists.Add(FPrecipitationData);
  ListOfTimeLists.Add(FEvaporationData);
  ListOfTimeLists.Add(FOverlandRunoffData);
  ListOfTimeLists.Add(FWithdrawalData);
end;

procedure TLakCollection.InvalidateEvapData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FEvaporationData.Invalidate;
  end;
end;

procedure TLakCollection.InvalidateMaxStageData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FMaximumStageData.Invalidate;
  end;
end;

procedure TLakCollection.InvalidateMinStageData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FMinimumStageData.Invalidate;
  end;
end;

procedure TLakCollection.InvalidatePrecipData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FPrecipitationData.Invalidate;
  end;
end;

procedure TLakCollection.InvalidateRunoffData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FOverlandRunoffData.Invalidate;
  end;
end;

procedure TLakCollection.InvalidateWithdrawalData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FWithdrawalData.Invalidate;
  end;
end;

class function TLakCollection.ItemClass: TMF_BoundItemClass;
begin
  result := TLakItem;
end;

procedure TLakCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer);
begin
  SetLength((Boundaries[ItemIndex] as TLakStorage).FLakArray, BoundaryCount);
  inherited;
end;

{ TLakBoundary }

procedure TLakBoundary.AddSubLake(Lake: TObject);
begin
  Assert(Lake is TScreenObject);
  FSubLakes.Add(Lake);
end;

procedure TLakBoundary.Assign(Source: TPersistent);
var
  Lake: TLakBoundary;
begin
  if Source is TLakBoundary then
  begin
    Lake := TLakBoundary(Source);
    InitialStage := Lake.InitialStage;
    CenterLake := Lake.CenterLake;
    Sill := Lake.Sill;
    LakeID := Lake.LakeID;
    StandardGage := Lake.StandardGage;
    FluxCondGage := Lake.FluxCondGage;
    DeltaGage := Lake.DeltaGage;
    Gage4 := Lake.Gage4;
  end;
  inherited;
end;

procedure TLakBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList);
var
  Cell: TLak_Cell;
  BoundaryValues: TLakRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TLakStorage;
begin
  LocalBoundaryStorage := BoundaryStorage as TLakStorage;
  for TimeIndex := 0 to
    (PhastModel as TPhastModel).ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TLak_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := (PhastModel as TPhastModel).ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime <= LocalBoundaryStorage.EndingTime) then
    begin
      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.LakArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.LakArray[BoundaryIndex];
        Cell := TLak_Cell.Create;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TLakBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TLakCollection;
end;

procedure TLakBoundary.ClearSubLakes;
begin
  FSubLakes.Clear;
end;

constructor TLakBoundary.Create(Model, ScreenObject: TObject);
begin
  inherited;
  FSubLakes:= TList.Create;
end;

procedure TLakBoundary.DeleteSubLake(Index: integer);
begin
  FSubLakes.Delete(Index);
end;

destructor TLakBoundary.Destroy;
begin
  FSubLakes.Free;
  inherited;
end;

procedure TLakBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList);
var
  ValueIndex: Integer;
  BoundaryStorage: TLakStorage;
begin
  EvaluateArrayBoundaries;
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    BoundaryStorage := Values.Boundaries[ValueIndex] as TLakStorage;
    AssignCells(BoundaryStorage, ValueTimeList);
  end;
end;

function TLakBoundary.GetOutType: integer;
begin
  if StandardGage then
  begin
    result := 0;
    if FluxCondGage then
    begin
      result := 1;
    end;
    if DeltaGage then
    begin
      result := result + 2;;
    end;
  end
  else
  begin
    result := -1;
  end;
end;

function TLakBoundary.GetSubLake(Index: Integer): TObject;
begin
  result := FSubLakes[Index];
end;

function TLakBoundary.GetSubLakeCount: integer;
begin
  result := FSubLakes.Count;
end;

procedure TLakBoundary.SetCenterLake(const Value: integer);
begin
  if FCenterLake <> Value then
  begin
    FCenterLake := Value;
    InvalidateModel;
  end;
end;

procedure TLakBoundary.SetDeltaGage(const Value: boolean);
begin
  if FDeltaGage <> Value then
  begin
    FDeltaGage := Value;
    InvalidateModel;
  end;
end;

procedure TLakBoundary.SetFluxCondGage(const Value: boolean);
begin
  if FFluxCondGage <> Value then
  begin
    FFluxCondGage := Value;
    InvalidateModel;
  end;
end;

procedure TLakBoundary.SetGage4(const Value: boolean);
begin
  if FGage4 <> Value then
  begin
    FGage4 := Value;
    InvalidateModel;
  end;
end;

procedure TLakBoundary.SetInitialStage(const Value: double);
begin
  if FInitialStage <> Value then
  begin
    FInitialStage := Value;
    InvalidateModel;
  end;
end;

procedure TLakBoundary.SetLakeID(const Value: integer);
begin
  if FLakeID <> Value then
  begin
    FLakeID := Value;
    InvalidateModel;
    if (ScreenObject <> nil)
        and (ScreenObject as TScreenObject).CanInvalidateModel
        and (PhastModel <> nil) then
    begin
      (PhastModel as TPhastModel).DischargeRoutingUpdate;
    end;
  end;
end;

procedure TLakBoundary.SetSill(const Value: double);
begin
  if FSill <> Value then
  begin
    FSill := Value;
    InvalidateModel;
  end;
end;

procedure TLakBoundary.SetStandardGage(const Value: boolean);
begin
  if FStandardGage <> Value then
  begin
    FStandardGage := Value;
    InvalidateModel;
  end;
end;

{ TLakRecord }

procedure TLakRecord.Cache(Comp: TCompressionStream);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, MinimumStage);
  WriteCompReal(Comp, MaximumStage);
  WriteCompReal(Comp, Precipitation);
  WriteCompReal(Comp, Evaporation);
  WriteCompReal(Comp, OverlandRunoff);
  WriteCompReal(Comp, Withdrawal);

  WriteCompString(Comp, MinimumStageAnnotation);
  WriteCompString(Comp, MaximumStageAnnotation);
  WriteCompString(Comp, PrecipitationAnnotation);
  WriteCompString(Comp, EvaporationAnnotation);
  WriteCompString(Comp, OverlandRunoffAnnotation);
  WriteCompString(Comp, WithdrawalAnnotation);
end;

procedure TLakRecord.Restore(Decomp: TDecompressionStream);
begin
  Cell := ReadCompCell(Decomp);
  MinimumStage := ReadCompReal(Decomp);
  MaximumStage := ReadCompReal(Decomp);
  Precipitation := ReadCompReal(Decomp);
  Evaporation := ReadCompReal(Decomp);
  OverlandRunoff := ReadCompReal(Decomp);
  Withdrawal := ReadCompReal(Decomp);

  MinimumStageAnnotation := ReadCompString(Decomp);
  MaximumStageAnnotation := ReadCompString(Decomp);
  PrecipitationAnnotation := ReadCompString(Decomp);
  EvaporationAnnotation := ReadCompString(Decomp);
  OverlandRunoffAnnotation := ReadCompString(Decomp);
  WithdrawalAnnotation := ReadCompString(Decomp);
end;

{ TLakStorage }
procedure TLakStorage.Clear;
begin
  SetLength(FLakArray, 0);
  FCleared := True;
end;

procedure TLakStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
begin
  Count := Length(FLakArray);
  Compressor.Write(Count, SizeOf(Count));
  for Index := 0 to Count - 1 do
  begin
    FLakArray[Index].Cache(Compressor);
  end;
end;

procedure TLakStorage.Restore(DecompressionStream: TDecompressionStream);
var
  Count: Integer;
  Index: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FLakArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FLakArray[Index].Restore(DecompressionStream);
  end;
end;

function TLakStorage.GetLakArray: TLakArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FLakArray;
end;

end.
