unit ModflowResUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, ModflowCellUnit, DataSetUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets;

type
  TResRecord = record
    Cell: TCellLocation;
    ResID: integer;
    StartingTime: double;
    EndingTime: double;
    ResIDAnnotation: string;
    procedure Cache(Comp: TCompressionStream);
    procedure Restore(Decomp: TDecompressionStream);
  end;

  TResArray = array of TResRecord;

  TResStorage = class(TCustomBoundaryStorage)
  private
    FResArray: TResArray;
    function GetResArray: TResArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property ResArray: TResArray read GetResArray;
  end;

  // @name represents a MODFLOW reservoir boundary for one time interval.
  // @name is stored by @link(TResCollection).
  TResItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(StartHead).
    FStartHead: TFormulaObject;
    // See @link(EndHead).
    FEndHead: TFormulaObject;
    // See @link(StartHead).
    procedure SetStartHead(const Value: string);
    // See @link(EndHead).
    procedure SetEndHead(const Value: string);
    function GetEndHead: string;
    function GetStartHead: string;
  protected
    // See @link(TCustomModflowBoundaryItem.BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(TCustomModflowBoundaryItem.BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    // @name checks whether AnotherItem is the same as the current @classname.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function BoundaryFormulaCount: integer; override;
  public
    Destructor Destroy; override;
  published
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    // @name is the formula used to set the elevation
    // of this boundary.
    property StartHead: string read GetStartHead write SetStartHead;
    // @name is the formula used to set the EndHead
    // or the EndHead multiplier of this boundary.
    property EndHead: string read GetEndHead write SetEndHead;
  end;

  TRes_Cell = class(TValueCell)
  private
    Values: TResRecord;
    StressPeriod: integer;
    function GetResID: integer;
    function GetResIdAnnotation: string;
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
    property ResID: integer read GetResID;
    property ResIdAnnotation: string read GetResIdAnnotation;
  end;


  // @name represents MODFLOW Reservoir boundaries
  // for a series of time intervals.
  TResCollection = class(TCustomMF_ArrayBoundColl)
  private
    FResIDData: TModflowTimeList;
    FEndHeadData: TModflowTimeList;
    procedure InvalidateStartHeadData(Sender: TObject);
    procedure InvalidateEndHeadData(Sender: TObject);
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

  TResBoundary = class(TModflowBoundary)
  private
    FResId: integer;
    procedure SetResId(const Value: integer);
  protected
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
  public
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList);
      override;
    property ResId: integer read FResId write SetResId;
  end;

implementation

uses RbwParser, ScreenObjectUnit, ModflowTimeUnit, PhastModelUnit, TempFiles, 
  GoPhastTypes, frmGoPhastUnit;

const
  StartPosition = 0;
  EndPosition = 1;

{ TResItem }

procedure TResItem.Assign(Source: TPersistent);
var
  Res: TResItem;
begin
  if Source is TResItem then
  begin
    Res := TResItem(Source);
    StartHead := Res.StartHead;
    EndHead := Res.EndHead;
  end;
  inherited;
end;

procedure TResItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TResCollection;
  StartObserver: TObserver;
  EndObserver: TObserver;
begin
  ParentCollection := Collection as TResCollection;
  StartObserver := FObserverList[StartPosition];
  StartObserver.OnUpToDateSet := ParentCollection.InvalidateStartHeadData;
  EndObserver := FObserverList[EndPosition];
  EndObserver.OnUpToDateSet := ParentCollection.InvalidateEndHeadData;
end;

function TResItem.BoundaryFormulaCount: integer;
begin
  result := 2;
end;

procedure TResItem.CreateFormulaObjects;
begin
  inherited;
  FStartHead := CreateFormulaObject(dso3D);
  FEndHead := CreateFormulaObject(dso3D);
end;

destructor TResItem.Destroy;
begin
  StartHead := '0';
  EndHead := '0';
  inherited;
end;

function TResItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    StartPosition: result := StartHead;
    EndPosition: result := EndHead;
    else Assert(False);
  end;
end;

function TResItem.GetEndHead: string;
begin
  Result := FEndHead.Formula;
  ResetItemObserver(EndPosition);
end;

procedure TResItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FStartHead then
  begin
    List.Add(FObserverList[StartPosition]);
  end;
  if Sender = FEndHead then
  begin
    List.Add(FObserverList[EndPosition]);
  end;
end;

function TResItem.GetStartHead: string;
begin
  Result := FStartHead.Formula;
  ResetItemObserver(StartPosition);
end;

function TResItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TResItem;
begin
  result := (AnotherItem is TResItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TResItem(AnotherItem);
    result := (Item.StartHead = StartHead)
      and (Item.EndHead = EndHead);
  end;
end;

procedure TResItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FEndHead,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStartHead,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
end;

procedure TResItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    StartPosition: StartHead := Value;
    EndPosition: EndHead := Value;
    else Assert(False);
  end;
end;

procedure TResItem.SetEndHead(const Value: string);
begin
  UpdateFormula(Value, EndPosition, FEndHead);
end;

procedure TResItem.SetStartHead(const Value: string);
begin
  UpdateFormula(Value, StartPosition, FStartHead);
end;

{ TResCollection }

procedure TResCollection.AddSpecificBoundary;
begin
  AddBoundary(TResStorage.Create);
end;

procedure TResCollection.AssignCellValues(DataSets: TList;
  ItemIndex: Integer);
var
  ResIDArray: TDataArray;
  Boundary: TResStorage;
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
  ResIDArray := DataSets[0];
  Boundary := Boundaries[ItemIndex] as TResStorage;
  ResIDArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
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
            if ResIDArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              with Boundary.ResArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                ResID := ResIDArray.
                  IntegerData[LayerIndex, RowIndex, ColIndex];
                ResIDAnnotation := ResIDArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  ResIDArray.CacheData;
  Boundary.CacheData;
end;

constructor TResCollection.Create(Boundary: TModflowBoundary; Model,
  ScreenObject: TObject);
begin
  inherited Create(Boundary, Model, ScreenObject);
  FResIDData := TModflowTimeList.Create(Model);
  FEndHeadData := TModflowTimeList.Create(Model);

  FResIDData.NonParamDescription := 'Starting stage';
  FResIDData.ParamDescription := ' starting stage';
  FEndHeadData.NonParamDescription := 'Ending stage';
  FEndHeadData.ParamDescription := ' ending stage';

  FResIDData.DataType := rdtInteger;

  AddTimeList(FResIDData);
  AddTimeList(FEndHeadData);
end;

destructor TResCollection.Destroy;
begin
  FResIDData.Free;
  FEndHeadData.Free;
  inherited;
end;

procedure TResCollection.InitializeTimeLists(ListOfTimeLists: TList);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TResItem;
  Boundary: TResBoundary;
  ScreenObject: TScreenObject;
begin
  Boundary := BoundaryGroup as TResBoundary;
  ScreenObject := Boundary.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TResItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := IntToStr(Boundary.ResID);
  end;
  FResIDData.Initialize(BoundaryValues, ScreenObject);

  Assert(FResIDData.Count = Count);
  ClearBoundaries;
  SetBoundaryCapacity(FResIDData.Count);
  for TimeIndex := 0 to FResIDData.Count - 1 do
  begin
    AddBoundary(TResStorage.Create);
  end;
  ListOfTimeLists.Add(FResIDData);
end;

procedure TResCollection.InvalidateEndHeadData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FEndHeadData.Invalidate;
  end;
end;

procedure TResCollection.InvalidateStartHeadData(Sender: TObject);
begin
  // do nothing?
end;

class function TResCollection.ItemClass: TMF_BoundItemClass;
begin
  result := TResItem;
end;

procedure TResCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer);
begin
  SetLength((Boundaries[ItemIndex] as TResStorage).FResArray, BoundaryCount);
  inherited;
end;

{ TResBoundary }

procedure TResBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList);
var
  Cell: TRes_Cell;
  BoundaryValues: TResRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TResStorage;
begin
  LocalBoundaryStorage := BoundaryStorage as TResStorage;
  for TimeIndex := 0 to
    (PhastModel as TPhastModel).ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TRes_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := (PhastModel as TPhastModel).ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime <= LocalBoundaryStorage.EndingTime) then
    begin
      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.ResArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.ResArray[BoundaryIndex];
        Cell := TRes_Cell.Create;
        Assert(ScreenObject <> nil);
        Cell.IFace := (ScreenObject as TScreenObject).IFace;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TResBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TResCollection;
end;

procedure TResBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList);
var
  ValueIndex: Integer;
  BoundaryStorage: TResStorage;
begin
  EvaluateArrayBoundaries;
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    BoundaryStorage := Values.Boundaries[ValueIndex] as TResStorage;
    AssignCells(BoundaryStorage, ValueTimeList);
  end;
end;

procedure TResBoundary.SetResId(const Value: integer);
begin
  if FResId <> Value then
  begin
    FResId := Value;
    InvalidateModel;
  end;
end;

{ TRes_Cell }

procedure TRes_Cell.Cache(Comp: TCompressionStream);
begin
  inherited;
  Values.Cache(Comp);
  WriteCompInt(Comp, StressPeriod);
end;

function TRes_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TRes_Cell.GetIntegerAnnotation(Index: integer): string;
begin
  result := '';
  case Index of
    0: result := ResIDAnnotation;
    else Assert(False);
  end;
end;

function TRes_Cell.GetIntegerValue(Index: integer): integer;
begin
  result := -1;
  case Index of
    0: result := ResID;
    else Assert(False);
  end;
end;

function TRes_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TRes_Cell.GetRealAnnotation(Index: integer): string;
begin
  result := '';
  Assert(False);
end;

function TRes_Cell.GetRealValue(Index: integer): double;
begin
  result := 0;
  Assert(False);
end;

function TRes_Cell.GetResID: integer;
begin
  result := Values.ResID;
end;

function TRes_Cell.GetResIdAnnotation: string;
begin
  result := Values.ResIDAnnotation;
end;

function TRes_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TRes_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

procedure TRes_Cell.Restore(Decomp: TDecompressionStream);
begin
  inherited;
  Values.Restore(Decomp); 
  StressPeriod := ReadCompInt(Decomp);
end;

{ TResRecord }

procedure TResRecord.Cache(Comp: TCompressionStream);
begin
  WriteCompCell(Comp, Cell);
  WriteCompInt(Comp, ResID);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompString(Comp, ResIDAnnotation);
end;

procedure TResRecord.Restore(Decomp: TDecompressionStream);
begin
  Cell := ReadCompCell(Decomp);
  ResID := ReadCompInt(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  ResIDAnnotation := ReadCompString(Decomp);
end;

{ TResStorage }

procedure TResStorage.Clear;
begin
  SetLength(FResArray, 0);
  FCleared := True;
end;

procedure TResStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
begin
  Count := Length(FResArray);
  Compressor.Write(Count, SizeOf(Count));
  for Index := 0 to Count - 1 do
  begin
    FResArray[Index].Cache(Compressor);
  end;
end;

procedure TResStorage.Restore(DecompressionStream: TDecompressionStream);
var
  Count: Integer;
  Index: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FResArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FResArray[Index].Restore(DecompressionStream);
  end;
end;

function TResStorage.GetResArray: TResArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FResArray;
end;

end.
