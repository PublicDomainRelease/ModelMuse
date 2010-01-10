unit ModflowConstantHeadBoundaryUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, RbwParser;

type

  // @name stores data for one CHD cell in a time increment defined by
  // @link(StartingTime) and @link(EndingTime).
  // The @link(StartingTime) and @link(EndingTime) may or may not be
  // the starting and ending time of stress periods.
  //  @longcode(
  //  TChdRecord = record
  //    Cell: TCellLocation;
  //    StartingHead: double;
  //    EndingHead: double;
  //    StartingTime: double;
  //    EndingTime: double;
  //    StartAnnotation: string;
  //    EndAnnotation: string;
  //  end;
  //  )
  // @member(Cell Cell is the cell to which this boundary applies.)
  // @member(StartingHead StartingHead is the specified head
  //   for this boundary at @link(StartingTime).)
  // @member(EndingHead EndingHead is the specified head
  //   for this boundary at @link(EndingTime).)
  // @member(StartingTime StartingTime is when this boundary
  //   first begins to apply.)
  // @member(EndingTime EndingTime is when this boundary ceases to apply.)
  // @member(StartAnnotation StartAnnotation tells how
  //  @link(StartingHead) was assigned.)
  // @member(EndAnnotation EndAnnotation tells how
  //  @link(EndingHead) was assigned.)
  TChdRecord = record
    Cell: TCellLocation;
    StartingHead: double;
    EndingHead: double;
    StartingTime: double;
    EndingTime: double;
    StartAnnotation: string;
    EndAnnotation: string;
    procedure Cache(Comp: TCompressionStream);
    procedure Restore(Decomp: TDecompressionStream);
  end;

  TChdArray = array of TChdRecord;

  // @name represents a MODFLOW Constant-Head boundary for one time interval.
  // @name is stored by TChdCollection.
  TChdItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(EndHead).
    FEndHead: TFormulaObject;
    // See @link(StartHead).
    FStartHead: TFormulaObject;
    // See @link(EndHead).
    procedure SetEndHead(const Value: string);
    // See @link(StartHead).
    procedure SetStartHead(const Value: string);
    function GetEndHead: string;
    function GetStartHead: string;
  protected
    procedure RemoveFormulaObjects; override;
    procedure CreateFormulaObjects; override;
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    function GetBoundaryFormula(Index: integer): string; override;
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    // @name checks whether AnotherItem is the same as the current @classname.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure InvalidateModel; override;
    function BoundaryFormulaCount: integer; override;
  public
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    Destructor Destroy; override;
  published
    // @name is the formula used to set the ending head
    // or the ending head multiplier of this boundary.
    property EndHead: string read GetEndHead write SetEndHead;
    // @name is the formula used to set the starting head
    // or the starting head multiplier of this boundary.
    property StartHead: string read GetStartHead write SetStartHead;
  end;

  TCHD_Cell = class(TValueCell)
  private
    Values: TChdRecord;
    StressPeriod: integer;
    function GetEndingHead: double;
    function GetStartingHead: double;
    function GetEndingHeadAnnotation: string;
    function GetStartingHeadAnnotation: string;
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
    property StartingHead: double read GetStartingHead;
    property EndingHead: double read GetEndingHead;
    property StartingHeadAnnotation: string read GetStartingHeadAnnotation;
    property EndingHeadAnnotation: string read GetEndingHeadAnnotation;
  end;


  // @name represents the MODFLOW Constant-Head boundaries associated with
  // a single @link(TScreenObject).
  //See also TChdCollection in implementation section
  // @seealso(TModflowParameters)
  TChdBoundary = class(TModflowParamBoundary)
  protected
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList);  override;        
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    class function ModflowParamItemClass: TModflowParamItemClass; override;
  public
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList);
      override;
    procedure InvalidateDisplay; override;
  end;

implementation

uses PhastModelUnit, ScreenObjectUnit, ModflowTimeUnit, TempFiles,
  GoPhastTypes, frmGoPhastUnit, GIS_Functions;

const
  StartHeadPosition = 0;
  EndHeadPosition = 1;

type
  TChdStorage = class(TCustomBoundaryStorage)
  private
    FChdArray: TChdArray;
    function GetChdArray: TChdArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property ChdArray: TChdArray read GetChdArray;
  end;

  // @name represents MODFLOW Constant-Head boundaries
  // for a series of time intervals.
  TChdCollection = class(TCustomMF_ListBoundColl)
  private
    FStartData: TModflowTimeList;
    FEndData: TModflowTimeList;
  protected
    procedure InvalidateStartData(Sender: TObject);
    procedure InvalidateEndData(Sender: TObject);
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer); override;
//    procedure AssignCellValues(DataSets: TList;
//      Sections: T3DSparseIntegerArray; ItemIndex: Integer); override;
//    procedure InitializeTimeLists(ListOfTimeLists: TList); override;

    // @name returns @link(TChdItem).
    class function ItemClass: TMF_BoundItemClass; override;
    procedure AddSpecificBoundary; override;
    procedure AssignCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList); override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
      override;
  public
    constructor Create(Boundary: TModflowBoundary;
      Model, ScreenObject: TObject); override;
    destructor Destroy; override;
  end;

  // Each @name stores a @link(TChdCollection).
  // @classname is stored by @link(TModflowParameters).
  TChdParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;


{ TChdItem }

procedure TChdItem.Assign(Source: TPersistent);
var
  Chd: TChdItem;
begin
  if Source is TChdItem then
  begin
    Chd := TChdItem(Source);
    StartHead := Chd.StartHead;
    EndHead := Chd.EndHead;
  end;
  inherited;
end;

procedure TChdItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FStartHead,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FEndHead,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
end;

procedure TChdItem.CreateFormulaObjects;
begin
  FStartHead := CreateFormulaObject(dso3D);
  FEndHead := CreateFormulaObject(dso3D);
end;

destructor TChdItem.Destroy;
begin
  StartHead := '0';
  EndHead := '0';
  inherited;
end;

procedure TChdItem.AssignObserverEvents(Collection: TCollection);
var
  EndObserver: TObserver;
  StartObserver: TObserver;
  ParentCollection: TChdCollection;
begin
  ParentCollection := Collection as TChdCollection;
  StartObserver := FObserverList[StartHeadPosition];
  StartObserver.OnUpToDateSet := ParentCollection.InvalidateStartData;
  EndObserver := FObserverList[EndHeadPosition];
  EndObserver.OnUpToDateSet := ParentCollection.InvalidateEndData;
end;

function TChdItem.BoundaryFormulaCount: integer;
begin
  result := 2;
end;

function TChdItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    StartHeadPosition: result := StartHead;
    EndHeadPosition: result := EndHead;
    else Assert(False);
  end;
end;

function TChdItem.GetEndHead: string;
begin
  Result := FEndHead.Formula;
  ResetItemObserver(EndHeadPosition);
end;

procedure TChdItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FStartHead then
  begin
    List.Add(FObserverList[StartHeadPosition]);
  end;
  if Sender = FEndHead then
  begin
    List.Add(FObserverList[EndHeadPosition]);
  end;
end;

function TChdItem.GetStartHead: string;
begin
  Result := FStartHead.Formula;
  ResetItemObserver(StartHeadPosition);
end;

procedure TChdItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState) then
  begin
    PhastModel.InvalidateMfChdStartingHead(self);
    PhastModel.InvalidateMfChdEndingHead(self);
  end;
end;

function TChdItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TChdItem;
begin
  result := (AnotherItem is TChdItem) and inherited IsSame(AnotherItem);;
  if result then
  begin
    Item := TChdItem(AnotherItem);
    result := (Item.EndHead = EndHead)
      and (Item.StartHead = StartHead);
  end;
end;

{ TChdParamItem }
procedure TChdItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  inherited;
  case Index of
    StartHeadPosition: StartHead := Value;
    EndHeadPosition: EndHead := Value;
    else Assert(False);
  end;
end;

procedure TChdItem.SetEndHead(const Value: string);
begin
  UpdateFormula(Value, EndHeadPosition, FEndHead);
end;

procedure TChdItem.SetStartHead(const Value: string);
begin
  UpdateFormula(Value, StartHeadPosition, FStartHead);
end;

{ TChdParamItem }

class function TChdParamItem.BoundaryClass: TMF_BoundCollClass;
begin
  result := TChdCollection;
end;

{ TChdCollection }

constructor TChdCollection.Create(Boundary: TModflowBoundary;
  Model, ScreenObject: TObject);
begin
  inherited Create(Boundary, Model, ScreenObject);
  FStartData := TModflowTimeList.Create(Model);
  FEndData := TModflowTimeList.Create(Model);
  FStartData.NonParamDescription := 'Starting head';
  FStartData.ParamDescription := ' starting head multiplier';
  FEndData.NonParamDescription := 'Ending head';
  FEndData.ParamDescription := ' ending head multiplier';

  if Model <> nil then
  begin
    FStartData.OnInvalidate := (Model as TPhastModel).InvalidateMfChdStartingHead;
    FEndData.OnInvalidate := (Model as TPhastModel).InvalidateMfChdEndingHead;
  end;

  AddTimeList(FStartData);
  AddTimeList(FEndData);
end;

destructor TChdCollection.Destroy;
begin
  FStartData.Free;
  FEndData.Free;
  inherited;
end;

procedure TChdCollection.AddSpecificBoundary;
begin
  AddBoundary(TChdStorage.Create);
end;

function TChdCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Item: TChdItem;
begin
  Item := Items[ItemIndex] as TChdItem;
  result := Item.BoundaryFormula[FormulaIndex];
end;

procedure TChdCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList);
var
  ChdStorage: TChdStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  Assert(Expression <> nil);

  ChdStorage := BoundaryStorage as TChdStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdataRequiredData(DataSets, Variables, ACell);
    Expression.Evaluate;
    with ChdStorage.ChdArray[Index] do
    begin
      case BoundaryFunctionIndex of
        0:
          begin
            StartingHead := Expression.DoubleResult;
            StartAnnotation := ACell.Annotation;
          end;
        1:
          begin
            EndingHead := Expression.DoubleResult;
            EndAnnotation := ACell.Annotation;
          end;
        else Assert(False);
      end;
    end;
  end;
end;

procedure TChdCollection.AssignCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  ChdStorage: TChdStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  ChdStorage := BoundaryStorage as TChdStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    with ChdStorage.ChdArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

procedure TChdCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer);
begin
  SetLength((Boundaries[ItemIndex] as TChdStorage).FChdArray, BoundaryCount);
  inherited;
end;

procedure TChdCollection.InvalidateEndData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FEndData.Invalidate;
  end;
end;

procedure TChdCollection.InvalidateStartData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FStartData.Invalidate;
  end;
end;

class function TChdCollection.ItemClass: TMF_BoundItemClass;
begin
  result := TChdItem;
end;

{ TChdBoundary }

procedure TChdBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList);
const
  FormatString =
    'Assigned by interpolation between the starting head of %f at '
    + 't = %f (%s) and the ending head of %f at t = %f (%s).';
var
  Cell: TCHD_Cell;
  BoundaryValues: TChdRecord;
  BoundaryIndex: Integer;
  EndHeadFactor: Double;
  StartFormatString: string;
  StartHeadFactor: Double;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TChdStorage;
begin
  LocalBoundaryStorage := BoundaryStorage as TChdStorage;
  for TimeIndex := 0 to
    (PhastModel as TPhastModel).ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TCHD_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := (PhastModel as TPhastModel).
      ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime <= LocalBoundaryStorage.EndingTime) then
    begin
      Cells.CheckRestore;
      // The starting head for each cell will be
      // StartHeadFactor * StartingHead + (1-StartHeadFactor)*EndingHead
      // The ending head for each cell will be
      // EndHeadFactor * StartingHead + (1-EndHeadFactor)*EndingHead
      if StressPeriod.StartTime = LocalBoundaryStorage.StartingTime then
      begin
        StartHeadFactor := 1;
        StartFormatString := '';
      end
      else
      begin
        StartHeadFactor := 1 -
          (StressPeriod.StartTime - LocalBoundaryStorage.StartingTime)
          / (LocalBoundaryStorage.EndingTime
          - LocalBoundaryStorage.StartingTime);
      end;
      if StressPeriod.EndTime = LocalBoundaryStorage.EndingTime then
      begin
        EndHeadFactor := 0;
      end
      else
      begin
        EndHeadFactor := 1 -
          (StressPeriod.EndTime - LocalBoundaryStorage.StartingTime)
          / (LocalBoundaryStorage.EndingTime
          - LocalBoundaryStorage.StartingTime);
      end;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.ChdArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.ChdArray[BoundaryIndex];
        Cell := TCHD_Cell.Create;
        Assert(ScreenObject <> nil);
        Cell.IFace := (ScreenObject as TScreenObject).IFace;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values.Cell := BoundaryValues.Cell;
        Cell.Values.StartingHead :=
          StartHeadFactor * BoundaryValues.StartingHead
          + (1 - StartHeadFactor) * BoundaryValues.EndingHead;
        Cell.Values.EndingHead :=
          EndHeadFactor * BoundaryValues.StartingHead
          + (1 - EndHeadFactor) * BoundaryValues.EndingHead;
        if StartHeadFactor = 1 then
        begin
          Cell.Values.StartAnnotation := BoundaryValues.StartAnnotation;
        end
        else
        begin
          Cell.Values.StartAnnotation := Format(FormatString,
            [BoundaryValues.StartingHead, BoundaryValues.StartingTime,
            BoundaryValues.StartAnnotation, BoundaryValues.EndingHead,
            BoundaryValues.EndingTime, BoundaryValues.EndAnnotation]);
        end;
        if EndHeadFactor = 0 then
        begin
          Cell.Values.EndAnnotation := BoundaryValues.EndAnnotation;
        end
        else
        begin
          Cell.Values.EndAnnotation := Format(FormatString,
            [BoundaryValues.StartingHead, BoundaryValues.StartingTime,
            BoundaryValues.StartAnnotation, BoundaryValues.EndingHead,
            BoundaryValues.EndingTime, BoundaryValues.EndAnnotation]);
        end;
        Cell.ScreenObject := ScreenObject;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TChdBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TChdCollection;
end;

procedure TChdBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList);
var
  ValueIndex: Integer;
  BoundaryStorage: TCustomBoundaryStorage;
  ParamIndex: Integer;
  Param: TChdParamItem;
  Times: TList;
  Position: integer;
  ParamName: string;
begin
  EvaluateListBoundaries;
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    BoundaryStorage := Values.Boundaries[ValueIndex] as TChdStorage;
    AssignCells(BoundaryStorage, ValueTimeList);
  end;
  for ParamIndex := 0 to Parameters.Count - 1 do
  begin
    Param := Parameters[ParamIndex] as TChdParamItem;
    ParamName := Param.Param.ParamName;
    Position := ParamList.IndexOf(ParamName);
    if Position < 0 then
    begin
      Times := TObjectList.Create;
      ParamList.AddObject(ParamName, Times);
    end
    else
    begin
      Times := ParamList.Objects[Position] as TList;
    end;
    for ValueIndex := 0 to Param.Param.Count - 1 do
    begin
      BoundaryStorage := Param.Param.Boundaries[ValueIndex];
      AssignCells(BoundaryStorage, Times);
    end;
  end;
end;

procedure TChdBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (PhastModel <> nil) then
  begin
    Model := PhastModel as TPhastModel;
    Model.InvalidateMfChdStartingHead(self);
    Model.InvalidateMfChdEndingHead(self);
  end;
end;

class function TChdBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := TChdParamItem;
end;

{ TCHD_Cell }

procedure TCHD_Cell.Cache(Comp: TCompressionStream);
begin
  inherited;
  Values.Cache(Comp);
  WriteCompInt(Comp, StressPeriod);
end;

function TCHD_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TCHD_Cell.GetEndingHead: double;
begin
  result := Values.EndingHead;
end;

function TCHD_Cell.GetEndingHeadAnnotation: string;
begin
  result := Values.EndAnnotation;
end;

function TCHD_Cell.GetIntegerAnnotation(Index: integer): string;
begin
  result := '';
  Assert(False);
end;

function TCHD_Cell.GetIntegerValue(Index: integer): integer;
begin
  result := 0;
  Assert(False);
end;

function TCHD_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TCHD_Cell.GetRealAnnotation(Index: integer): string;
begin
  result := '';
  case Index of
    StartHeadPosition: result := StartingHeadAnnotation;
    EndHeadPosition: result := EndingHeadAnnotation;
    else Assert(False);
  end;
end;

function TCHD_Cell.GetRealValue(Index: integer): double;
begin
  result := 0;
  case Index of
    StartHeadPosition: result := StartingHead;
    EndHeadPosition: result := EndingHead;
    else Assert(False);
  end;
end;

function TCHD_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TCHD_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TCHD_Cell.GetStartingHead: double;
begin
  result := Values.StartingHead;
end;

function TCHD_Cell.GetStartingHeadAnnotation: string;
begin
  result := Values.StartAnnotation;
end;

procedure TCHD_Cell.Restore(Decomp: TDecompressionStream);
begin
  inherited;
  Values.Restore(Decomp);
  StressPeriod := ReadCompInt(Decomp);
end;

{ TChdRecord }

procedure TChdRecord.Cache(Comp: TCompressionStream);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, StartingHead);
  WriteCompReal(Comp, EndingHead);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompString(Comp, StartAnnotation);
  WriteCompString(Comp, EndAnnotation);
end;

procedure TChdRecord.Restore(Decomp: TDecompressionStream);
begin
  Cell := ReadCompCell(Decomp);
  StartingHead := ReadCompReal(Decomp);
  EndingHead := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  StartAnnotation := ReadCompString(Decomp);
  EndAnnotation := ReadCompString(Decomp);
end;

{ TChdStorage }

procedure TChdStorage.Clear;
begin
  SetLength(FChdArray, 0);
  FCleared := True;
end;

procedure TChdStorage.Store(Compressor: TCompressionStream);
var
  Count: Integer;
  Index: Integer;
begin
  Count := Length(FChdArray);
  Compressor.Write(Count, SizeOf(Count));
  for Index := 0 to Count - 1 do
  begin
    FChdArray[Index].Cache(Compressor);
  end;
end;

procedure TChdStorage.Restore(DecompressionStream: TDecompressionStream);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FChdArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FChdArray[Index].Restore(DecompressionStream);
  end;
end;

function TChdStorage.GetChdArray: TChdArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FChdArray;

end;

end.