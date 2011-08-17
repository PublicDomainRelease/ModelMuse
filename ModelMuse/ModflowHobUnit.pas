unit ModflowHobUnit;

interface

uses ZLib, SysUtils, Classes, Contnrs, GoPhastTypes, ModflowBoundaryUnit,
  OrderedCollectionUnit, DataSetUnit, ModflowCellUnit;

type
  THobRecord = record
    Cell: TCellLocation;
    Head: double;
    Time: double;
    HeadAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList); 
    procedure RecordStrings(Strings: TStringList);
  end;

  TMultiObsMethod = (momAllHeads, momHeadAndDrawdown);

  { TODO : Mark observations as "observations", "predictions", or "omitted". }
  // @name represents a MODFLOW General Head boundary for one time interval.
  // @name is stored by @link(TGhbCollection).
  THobItem = class(TOrderedItem)
  private
    FHead: double;
    FTime: double;
    FStatFlag: TStatFlag;
    FStatistic: double;
    FComment: string;
    procedure SetHead(const Value: double);
    procedure SetTime(const Value: double);
    procedure SetStatFlag(const Value: TStatFlag);
    procedure SetStatistic(const Value: double);
    procedure SetComment(const Value: string);
    function GetStatFlag: TStatFlag;
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure InvalidateModel; override;
  published
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent); override;
    // @name is the formula used to set the observed head
    // of this boundary.
    property Head: double read FHead write SetHead;
    // @name indicates the time of this observation.
    property Time: double read FTime write SetTime;
    property Statistic: double read FStatistic write SetStatistic;
    property StatFlag: TStatFlag read GetStatFlag write SetStatFlag;
    property Comment: string read FComment write SetComment;
  end;

  THobBoundary = class;
  TObservationTimeList = class;

  TObsTimesModelLink = class(TObject)
  private
    FObsTimes: TObservationTimeList;
    FModel: TBaseModel;
  public
    Constructor Create(AModel: TBaseModel);
    Destructor Destroy; override;
  end;

  TObsTimesModelLinkList = class(TObject)
  private
    // @name is actually a TObjectList.
    FList: TList;
    function GetLink(AModel: TBaseModel): TObsTimesModelLink;
  public
    property Links[AModel: TBaseModel]: TObsTimesModelLink read GetLink;
    Constructor Create;
    Destructor Destroy; override;
    procedure RemoveLink(AModel: TBaseModel);
  end;

  // @name represents MODFLOW Head observations
  // for a series of times.
  THobCollection = class(TEnhancedOrderedCollection)
  private
    FBoundary: THobBoundary;
    FObservationHeads: TObservationTimeList;
    FObsTimesModelLinkList: TObsTimesModelLinkList;
    FObservationRowOffset: double;
    FObservationColumnOffset: double;
    FScreenObject: TObject;
    function GetHobItems(Index: integer): THobItem;
    function GetObservationHeads(AModel: TBaseModel): TObservationTimeList;
  protected
    procedure InvalidateModel; override;
  public
    procedure RemoveModelLink(AModel: TBaseModel);
    property ScreenObject: TObject read FScreenObject;
    // @name creates an instance of @classname
    constructor Create(Boundary: THobBoundary; Model: TBaseModel;
      ScreenObject: TObject);
    procedure EvaluateHeadObservations(AModel: TBaseModel);
    // @name destroys the current instance of @classname.
    // Do not call @name; call Free instead.
    destructor Destroy; override;
    property HobItems[Index: integer]: THobItem read GetHobItems;
    property ObservationRowOffset: double read FObservationRowOffset;
    property ObservationColumnOffset: double read FObservationColumnOffset;
    property ObservationHeads[AModel: TBaseModel]: TObservationTimeList
      read GetObservationHeads;
    function CountObservationTimes(StartTime, EndTime: double): integer;
  end;

  THob_Cell = class(TValueCell)
  private
    Values: THobRecord;
    function GetHead: double;
    function GetTime: double;
    function GetHeadAnnotation: string;
  protected
    function GetColumn: integer; override;
    function GetLayer: integer; override;
    function GetRow: integer; override;
    procedure SetColumn(const Value: integer); override;
    procedure SetLayer(const Value: integer); override;
    procedure SetRow(const Value: integer); override;
    function GetIntegerValue(Index: integer; AModel: TBaseModel): integer; override;
    function GetRealValue(Index: integer; AModel: TBaseModel): double; override;
    function GetRealAnnotation(Index: integer; AModel: TBaseModel): string; override;
    function GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string; override;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList); override;
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList); override;
    function GetSection: integer; override;
    procedure RecordStrings(Strings: TStringList); override;
  public
    property Head: double read GetHead;
    property Time: double read GetTime;
    property HeadAnnotation: string read GetHeadAnnotation;
  end;

  TObsCellList = class(TObject)
  private
    FList: TList;
    function GetCount: integer;
    function GetItem(Index: integer): THob_Cell;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure Add(Cell: THob_Cell);
    property Count: integer read GetCount;
    property Items[Index: integer]: THob_Cell read GetItem; default;
  end;

  TMultiHeadItem = class(TOrderedItem)
  private
    FLayer: integer;
    FProportion: double;
    FUsed: boolean;
    procedure SetLayer(const Value: integer);
    procedure SetProportion(const Value: double);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    property Used: boolean read FUsed write FUsed;
    procedure Assign(Source: TPersistent); override;
  published
    property Layer: integer read FLayer write SetLayer;
    property Proportion: double read FProportion write SetProportion;
  end;

  TMultiHeadCollection = class(TEnhancedOrderedCollection)
  private
    FBoundary: THobBoundary;
    FScreenObject: TObject;
    function GetMultiHeadItem(Index: integer): TMultiHeadItem;
  public
    constructor Create(Boundary: THobBoundary; Model: TBaseModel;
      ScreenObject: TObject);
    property MultiHeadItems[Index: integer]: TMultiHeadItem read GetMultiHeadItem; default;
  end;

  // @name represents the MODFLOW Head observations associated with
  // a single @link(TScreenObject).
  THobBoundary = class(TModflowScreenObjectProperty)
  private
    FValues: THobCollection;
    FObservationName: string;
    FLayerFractions: TMultiHeadCollection;
    FMultiObsMethod: TMultiObsMethod;
    FPurpose: TObservationPurpose;
    procedure SetValues(const Value: THobCollection);
    procedure SetObservationName(Value: string);
    procedure SetLayerFractions(const Value: TMultiHeadCollection);
    function GetCellList(Index: integer): TObsCellList;
    procedure SetMultiObsMethod(const Value: TMultiObsMethod);
    function GetCellListCount: integer;
    procedure SetPurpose(const Value: TObservationPurpose);
  public
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    // @name destroys the current instance of @classname.  Do not call
    // @name directly.  Call Free instead.
    Destructor Destroy; override;
    // @name checks that the Purpose parameter matches @link(Purpose)
    // and, if so, calls @link(THobCollection.EvaluateHeadObservations
    // Values.EvaluateHeadObservations)
    procedure EvaluateHeadObservations(Purpose: TObservationPurpose;
      AModel: TBaseModel);
    function Used: boolean; override;
    procedure Clear; virtual;
    property CellLists[Index: integer]: TObsCellList read GetCellList;
    property CellListCount: integer read GetCellListCount;
    function GetItemObsName(Item: THobItem): string;
    procedure RemoveModelLink(AModel: TBaseModel);
  published
    property ObservationName: string read FObservationName write SetObservationName;
    // @name stores the MODFLOW boundaries that are NOT
    // associated with parameters.
    property Values: THobCollection read FValues write SetValues;
    property LayerFractions: TMultiHeadCollection read FLayerFractions
      write SetLayerFractions;
    // ITT
    property MultiObsMethod: TMultiObsMethod read FMultiObsMethod
      write SetMultiObsMethod default momHeadAndDrawdown;
    property Purpose: TObservationPurpose read FPurpose
      write SetPurpose;
  end;

  // @name is used to store a series of @link(TDataArray)s for head
  // observations in MODFLOW.
  TObservationTimeList = class(TCustomTimeList)
  private
    // See @link(OnInvalidate).
    FOnInvalidate: TNotifyEvent;
    FCellList: TList;
    function GetCellList(Index: integer): TObsCellList;
  protected
    // @name calls the inherited @link(TCustomTimeList.SetUpToDate)
    // and then calls @link(OnInvalidate) if @link(OnInvalidate) is assigned.
    procedure SetUpToDate(const Value: boolean); override;
  public
    procedure Clear; override;
    constructor Create(Model: TBaseModel);
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;

    // @name takes the times and formulas in BoundaryValues and uses them
    // to determine the locations and values for those times.  These
    // locations and values are stored in @link(TRealSparseDataSet)s
    // accessed through @link(TCustomTimeList.Items Items).
    procedure Initialize(ObservationValues: THobCollection;
      ScreenObject: TObject; UseLgrEdgeCells: boolean; AModel: TBaseModel); reintroduce;
    // If assigned, @name is called with @link(UpToDate) is set to False.
    property OnInvalidate: TNotifyEvent read FOnInvalidate write FOnInvalidate;
    property CellLists[Index: integer]: TObsCellList read GetCellList; default;
  end;

resourcestring
  StrHeadObservationsError = 'Head observations can only be defined using ' +
    'objects with a single vertex.  The following objects need to be fixed.';

const
  StrHobout = '.hob_out';

implementation

uses RbwParser, ScreenObjectUnit, PhastModelUnit, ModflowGridUnit, FastGEO,
  SubscriptionUnit, RealListUnit, frmErrorsAndWarningsUnit;

resourcestring
  StrErrorObject0s = 'Error; Object = %0:s Duplicate Times = %1:s';

{ THob_Cell }

procedure THob_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
end;

function THob_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column
end;

function THob_Cell.GetHead: double;
begin
  result := Values.Head;
end;

function THob_Cell.GetHeadAnnotation: string;
begin
  result := Values.HeadAnnotation;
end;

function THob_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function THob_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function THob_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function THob_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    0: result := HeadAnnotation;
    else Assert(False);
  end;
end;

function THob_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    0: result := Head;
    else Assert(False);
  end;
end;

function THob_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function THob_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function THob_Cell.GetTime: double;
begin
  result := Values.Time;
end;

procedure THob_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure THob_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
end;

procedure THob_Cell.SetColumn(const Value: integer);
begin
  Values.Cell.Column := Value;
end;

procedure THob_Cell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure THob_Cell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
end;

{ THobBoundary }

procedure THobBoundary.Assign(Source: TPersistent);
var
  HobSource: THobBoundary;
begin
  if Source is THobBoundary then
  begin
    HobSource := THobBoundary(Source);
    Values := HobSource.Values;
    LayerFractions := HobSource.LayerFractions;
    ObservationName := HobSource.ObservationName;
    MultiObsMethod := HobSource.MultiObsMethod;
    Purpose := HobSource.Purpose;
  end
  else
  begin
    inherited;
  end;
end;

procedure THobBoundary.Clear;
begin
  Values.Clear;
end;

constructor THobBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
//  Assert((ScreenObject = nil) or (ScreenObject is TScreenObject));
//  FScreenObject := ScreenObject;
//  Assert((Model = nil) or (Model is TPhastModel));
//  FPhastModel := Model;
  FValues:= THobCollection.Create(self, Model,
    ScreenObject);
  FLayerFractions := TMultiHeadCollection.Create(self, Model,
    ScreenObject);
  FMultiObsMethod := momHeadAndDrawdown;
  FPurpose := ofObserved;
end;

destructor THobBoundary.Destroy;
begin
  FLayerFractions.Free;
  FValues.Free;
  inherited;
end;

procedure THobBoundary.EvaluateHeadObservations(Purpose: TObservationPurpose;
  AModel: TBaseModel);
begin
  if self.Purpose = Purpose then
  begin
    Values.EvaluateHeadObservations(AModel);
  end;
end;

function THobBoundary.GetCellList(Index: integer): TObsCellList;
begin
  result := Values.FObservationHeads.CellLists[Index];
end;

function THobBoundary.GetCellListCount: integer;
begin
  result := Values.FObservationHeads.FCellList.Count;
end;


function THobBoundary.GetItemObsName(Item: THobItem): string;
begin
  Assert(Item.Collection = Values);
  if Values.Count = 1 then
  begin
    result := ObservationName;
  end
  else
  begin
    result := ObservationName + '_' + IntToStr(Item.Index + 1);
    if Length(result) > 12 then
    begin
      result := ObservationName + IntToStr(Item.Index + 1);
    end;
    if Length(result) > 12 then
    begin
      // The GUI is designed to prevent this from ever being required.
      SetLength(result, 12);
    end;
  end;
end;

procedure THobBoundary.RemoveModelLink(AModel: TBaseModel);
begin
  Values.RemoveModelLink(AModel);
end;

procedure THobBoundary.SetLayerFractions(const Value: TMultiHeadCollection);
begin
  FLayerFractions.Assign(Value);
end;

procedure THobBoundary.SetMultiObsMethod(const Value: TMultiObsMethod);
begin
  if FMultiObsMethod <> Value then
  begin
    InvalidateModel;
    FMultiObsMethod := Value;
  end;
end;

procedure THobBoundary.SetPurpose(const Value: TObservationPurpose);
begin
  if FPurpose <> Value then
  begin
    InvalidateModel;
    FPurpose := Value;
  end;
end;

procedure THobBoundary.SetObservationName(Value: string);
begin
  Value := Trim(Value);
  Value := StringReplace(Value, ' ', '_', [rfReplaceAll]);
  Value := StringReplace(Value, '"', '', [rfReplaceAll]);
  Value := StringReplace(Value, '''', '', [rfReplaceAll]);
  if Length(Value) > 12 then
  begin
    Value := Copy(Value, 1, 12);
  end;
  if FObservationName <> Value then
  begin
    InvalidateModel;
    FObservationName := Value;
  end;
end;

procedure THobBoundary.SetValues(const Value: THobCollection);
begin
  FValues.Assign(Value);
end;

function THobBoundary.Used: boolean;
begin
  result := FValues.Count > 0;
end;

{ THobItem }

procedure THobItem.Assign(Source: TPersistent);
var
  SourceItem: THobItem;
begin
  if Source is THobItem then
  begin
    SourceItem := THobItem(Source);
    Head := SourceItem.Head;
    Time := SourceItem.Time;
    Statistic := SourceItem.Statistic;
    StatFlag := SourceItem.StatFlag;
    Comment := SourceItem.Comment;
  end;
  inherited;
end;

function THobItem.GetStatFlag: TStatFlag;
var
  LocalCollection: THobCollection;
begin
  result := FStatFlag;
  LocalCollection := Collection as THobCollection;
  if (LocalCollection.FBoundary.Purpose = ofPredicted)
    and (result > stStandardDev) then
  begin
    result := stVariance;
  end;
end;

procedure THobItem.InvalidateModel;
begin
  (Collection as THobCollection).InvalidateModel;
end;

function THobItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: THobItem;
begin
  result := AnotherItem is THobItem;
  if result then
  begin
    Item := THobItem(AnotherItem);
    result := (Item.Head = Head)
      and (Item.Time = Time)
      and (Item.Statistic = Statistic)
      and (Item.StatFlag = StatFlag)
      and (Item.Comment = Comment);
  end;
end;

procedure THobItem.SetComment(const Value: string);
begin
  if FComment <> Value then
  begin
    FComment := Value;
    InvalidateModel;
  end;
end;

procedure THobItem.SetHead(const Value: double);
begin
  if FHead <> Value then
  begin
    FHead := Value;
    InvalidateModel;
  end;
end;

procedure THobItem.SetStatFlag(const Value: TStatFlag);
begin
  if FStatFlag <> Value then
  begin
    FStatFlag := Value;
    InvalidateModel;
  end;
end;

procedure THobItem.SetStatistic(const Value: double);
begin
  if FStatistic <> Value then
  begin
    FStatistic := Value;
    InvalidateModel;
  end;
end;

procedure THobItem.SetTime(const Value: double);
begin
  if FTime <> Value then
  begin
    FTime := Value;
    InvalidateModel;
  end;
end;

{ THobCollection }

function THobCollection.CountObservationTimes(StartTime,
  EndTime: double): integer;
var
  Index: Integer;
  Item: THobItem;
begin
  result := 0;
  for Index := 0 to Count - 1 do
  begin
    Item := HobItems[Index];
    if (StartTime <= Item.Time) and (Item.Time <= EndTime) then
    begin
      Inc(result);
    end;
  end;
end;

constructor THobCollection.Create(Boundary: THobBoundary; Model: TBaseModel;
  ScreenObject: TObject);
begin
  inherited Create(THobItem, Model);
  FBoundary := Boundary;
  Assert((ScreenObject = nil) or (ScreenObject is TScreenObject));
  FScreenObject := ScreenObject;
  FObsTimesModelLinkList := TObsTimesModelLinkList.Create;

//  FObservationHeads:= TObservationTimeList.Create(Model);

//  if Model <> nil then
//  begin
//    FObservationHeads.OnInvalidate :=
//      (Model as TPhastModel).InvalidateMfHobHeads;
//  end;

end;

destructor THobCollection.Destroy;
begin
  FObsTimesModelLinkList.Free;
//  FObservationHeads.Free;
  inherited;
end;

procedure THobCollection.EvaluateHeadObservations(AModel: TBaseModel);
var
  LocalScreenObject: TScreenObject;
  LocalModel: TCustomModel;
  Grid: TModflowGrid;
  ObservationPoint: TPoint2D;
  Row: Integer;
  Column: Integer;
  Width: Real;
  Center: Real;
  CellList: TObsCellList;
  Cell : THob_Cell;
begin
  ObservationHeads[AModel].Initialize(self, ScreenObject, True, AModel);

  if FObservationHeads.FCellList.Count > 0 then
  begin
    CellList := FObservationHeads.FCellList[0];
    if CellList.Count > 0 then
    begin
      Assert(ScreenObject <> nil);
      LocalScreenObject := ScreenObject as TScreenObject;
      Assert(LocalScreenObject.ViewDirection = vdTop);
      Assert(Model <> nil);
      LocalModel := AModel as TCustomModel;
      Grid := LocalModel.ModflowGrid;
      Assert(Grid <> nil);

      if LocalScreenObject.Count > 1 then
      begin
        FObservationRowOffset := -1000;
        FObservationColumnOffset := -1000;
        frmErrorsAndWarnings.AddError(FObservationHeads.Model,
          StrHeadObservationsError, LocalScreenObject.Name)
      end
      else
      begin
        Assert(LocalScreenObject.Count = 1);
        ObservationPoint := Grid.RotateFromRealWorldCoordinatesToGridCoordinates(
          LocalScreenObject.Points[0]);

        Cell := CellList[0];
        Row := Cell.Row;
        Column := Cell.Column;

        Width := Grid.RowWidth[Row];
        Center := Grid.RowCenter(Row);
        FObservationRowOffset := -(ObservationPoint.y - Center)/Width;

        Width := Grid.ColumnWidth[Column];
        Center := Grid.ColumnCenter(Column);
        FObservationColumnOffset := (ObservationPoint.x - Center)/Width;
      end;
    end;
  end;
end;

function THobCollection.GetHobItems(Index: integer): THobItem;
begin
  result := Items[Index] as THobItem;
end;

function THobCollection.GetObservationHeads(
  AModel: TBaseModel): TObservationTimeList;
begin
  FObservationHeads := FObsTimesModelLinkList.Links[AModel].FObsTimes;
  result := FObservationHeads;
end;

procedure THobCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  FBoundary.InvalidateModel;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState) then
  begin
    PhastModel.InvalidateMfHobHeads(self);
  end;
end;

procedure THobCollection.RemoveModelLink(AModel: TBaseModel);
begin
  FObsTimesModelLinkList.RemoveLink(AModel);
end;

{ TObservationTimeList }

procedure TObservationTimeList.Clear;
begin
  inherited;
  FCellList.Clear;
end;

constructor TObservationTimeList.Create(Model: TBaseModel);
begin
  inherited;
  FCellList := TObjectList.Create;
end;

destructor TObservationTimeList.Destroy;
begin
  FCellList.Free;
  inherited;
end;

function TObservationTimeList.GetCellList(Index: integer): TObsCellList;
begin
  result := FCellList[Index];
end;

procedure TObservationTimeList.Initialize(ObservationValues: THobCollection;
  ScreenObject: TObject; UseLgrEdgeCells: boolean; AModel: TBaseModel);
const
  ErrorRoot = 'Error: Duplicate head observation times';
  EarlyTimeWarning = 'Head observation times earlier than the beginning of the first stress period will be ignored.';
  LateTimeWarning = 'Head observation times later than the end of the last stress period will be ignored.';
var
  LocalScreenObject: TScreenObject;
  Index: Integer;
  Time: double;
  DataArray: TCustomSparseDataSet;
  LocalModel: TCustomModel;
  Grid: TModflowGrid;
  Value: double;
  StoredUpToDate: boolean;
  CellList: TObsCellList;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Cell: THob_Cell;
  Times: TRealList;
  DuplicateTimes: string;
  EarliestAllowedTime: double;
  LatestAllowedTime: double;
  EarlyTimes: string;
  LateTimes: string;
  ActiveDataSet: TDataArray;
begin
  if UpToDate then
    Exit;

  LocalScreenObject := ScreenObject as TScreenObject;
  Assert(LocalScreenObject <> nil);
  LocalModel := AModel as TCustomModel;
  EarliestAllowedTime := LocalModel.ModflowFullStressPeriods[0].StartTime;
  LatestAllowedTime := LocalModel.ModflowFullStressPeriods[
    LocalModel.ModflowFullStressPeriods.Count-1].EndTime;
  Assert(LocalModel <> nil);
  StoredUpToDate := LocalModel.UpToDate;
  Times := TRealList.Create;
  try
    DuplicateTimes := '';
    EarlyTimes := '';
    LateTimes := '';
    Times.Sorted := True;
    Clear;
    Grid := LocalModel.ModflowGrid;
    Assert(Grid <> nil);

    for Index := 0 to ObservationValues.Count - 1 do
    begin
      CellList := TObsCellList.Create;
      FCellList.Add(CellList);
      Time := ObservationValues.HobItems[Index].Time;
      if Times.IndexOf(Time) >= 0 then
      begin
        DuplicateTimes := DuplicateTimes + ' ' + FloatToStr(Time);
        Continue;
      end;

      if Time < EarliestAllowedTime then
      begin
        EarlyTimes := EarlyTimes + ' ' + FloatToStr(Time);
        Continue;
      end;

      if Time > LatestAllowedTime then
      begin
        LateTimes := LateTimes + ' ' + FloatToStr(Time);
        Continue;
      end;

      Times.Add(Time);
      Value := ObservationValues.HobItems[Index].Head;
      DataArray := TRealSparseDataSet.Create(LocalModel);
      Add(Time, DataArray);
      DataArray.EvaluatedAt := eaBlocks;
      DataArray.Orientation := dso3D;
      DataArray.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
        Grid.ColumnCount);

      ActiveDataSet := LocalModel.DataArrayManager.GetDataSetByName(rsActive);
      ActiveDataSet.Initialize;

      LocalScreenObject.AssignNumericValueToDataSet(Grid, DataArray, Value,
        Model);
      for LayerIndex := 0 to DataArray.LayerCount - 1 do
      begin
        for RowIndex := 0 to DataArray.RowCount - 1 do
        begin
          for ColIndex := 0 to DataArray.ColumnCount - 1 do
          begin
            if DataArray.IsValue[LayerIndex, RowIndex,ColIndex]
              and ActiveDataSet.BooleanData[LayerIndex, RowIndex,ColIndex] then
            begin
              Cell := THob_Cell.Create;
              CellList.Add(Cell);
              Cell.Values.Cell.Layer := LayerIndex;
              Cell.Values.Cell.Row := RowIndex;
              Cell.Values.Cell.Column := ColIndex;
              Cell.Values.Head := DataArray.RealData[LayerIndex, RowIndex,ColIndex];
              Cell.Values.HeadAnnotation := DataArray.Annotation[LayerIndex, RowIndex,ColIndex];
              Cell.Values.Time := Time;
            end;
          end;
        end;
      end;
    end;
    if DuplicateTimes <> '' then
    begin
      DuplicateTimes := Format(StrErrorObject0s,
        [LocalScreenObject.Name, DuplicateTimes]);
//      DuplicateTimes := 'Error; Object = ' + LocalScreenObject.Name +
//        ' Duplicate Times = ' +  DuplicateTimes;
      frmErrorsAndWarnings.AddError(Model, ErrorRoot, DuplicateTimes);
    end;
    if EarlyTimes <> '' then
    begin
      EarlyTimes := 'Error; Object = ' + LocalScreenObject.Name +
        ' Early Times = ' +  EarlyTimes;
      frmErrorsAndWarnings.AddWarning(Model, EarlyTimeWarning, EarlyTimes);
    end;
    if LateTimes <> '' then
    begin
      LateTimes := 'Error; Object = ' + LocalScreenObject.Name +
        ' Late Times = ' +  LateTimes;
      frmErrorsAndWarnings.AddWarning(Model, LateTimeWarning, LateTimes);
    end;
  finally
    LocalModel.UpToDate := StoredUpToDate;
    Times.Free;
  end;
end;

procedure TObservationTimeList.SetUpToDate(const Value: boolean);
begin
  inherited;
  if not Value then
  begin
    if Assigned(OnInvalidate) then
    begin
      OnInvalidate(Self);
    end;
  end;
end;

{ TMultiHeadItem }

procedure TMultiHeadItem.Assign(Source: TPersistent);
var
  SourceItem: TMultiHeadItem;
begin
  if Source is TMultiHeadItem then
  begin
    SourceItem := TMultiHeadItem(Source);
    Layer := SourceItem.Layer;
    Proportion := SourceItem.Proportion;
  end;
  inherited;
end;

function TMultiHeadItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TMultiHeadItem;
begin
  result := AnotherItem is TMultiHeadItem;
  if result then
  begin
    Item := TMultiHeadItem(AnotherItem);
    result := (Item.Layer = Layer)
      and (Item.Proportion = Proportion);
  end;
end;

procedure TMultiHeadItem.SetLayer(const Value: integer);
begin
  if FLayer <> Value then
  begin
    InvalidateModel;
    FLayer := Value;
  end;
end;

procedure TMultiHeadItem.SetProportion(const Value: double);
begin
  if FProportion <> Value then
  begin
    InvalidateModel;
    FProportion := Value;
  end;
end;

{ TMultiHeadCollection }

constructor TMultiHeadCollection.Create(Boundary: THobBoundary; Model: TBaseModel;
  ScreenObject: TObject);
begin
  inherited Create(TMultiHeadItem, Model);
  FBoundary := Boundary;
  Assert((ScreenObject = nil) or (ScreenObject is TScreenObject));
  FScreenObject := ScreenObject;
end;

function TMultiHeadCollection.GetMultiHeadItem(Index: integer): TMultiHeadItem;
begin
  result := Items[Index] as TMultiHeadItem;
end;

{ TObsCellList }

procedure TObsCellList.Add(Cell: THob_Cell);
begin
  FList.Add(Cell);
end;

constructor TObsCellList.Create;
begin
  inherited;
  FList := TObjectList.Create;
end;

destructor TObsCellList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TObsCellList.GetCount: integer;
begin
  result := FList.Count;
end;

function TObsCellList.GetItem(Index: integer): THob_Cell;
begin
  result := FList[Index];
end;

{ THobRecord }

procedure THobRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, Head);
  WriteCompReal(Comp, Time);
  WriteCompInt(Comp, Strings.IndexOf(HeadAnnotation));
//  WriteCompString(Comp, HeadAnnotation);
end;

procedure THobRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(HeadAnnotation);
end;

procedure THobRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  Head := ReadCompReal(Decomp);
  Time := ReadCompReal(Decomp);
//  HeadAnnotation := ReadCompString(Decomp, Annotations);
  HeadAnnotation := Annotations[ReadCompInt(Decomp)];
end;

{ TFObsTimesModelLink }

constructor TObsTimesModelLink.Create(AModel: TBaseModel);
begin
  FModel := AModel;
  FObsTimes := TObservationTimeList.Create(FModel);
end;

destructor TObsTimesModelLink.Destroy;
begin
  FObsTimes.Free;
  inherited;
end;

{ TObsTimesModelLinkList }

constructor TObsTimesModelLinkList.Create;
begin
  FList := TObjectList.Create;
end;

destructor TObsTimesModelLinkList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TObsTimesModelLinkList.GetLink(
  AModel: TBaseModel): TObsTimesModelLink;
var
  ModelIndex: Integer;
  Item: TObsTimesModelLink;
begin
  for ModelIndex := 0 to FList.Count - 1 do
  begin
    Item := FList[ModelIndex];
    if Item.FModel = AModel then
    begin
      result := Item;
      Exit;
    end;
  end;
  result := TObsTimesModelLink.Create(AModel);
  FList.Add(result);
  if AModel <> nil then
  begin
    result.FObsTimes.OnInvalidate :=
      (AModel as TCustomModel).InvalidateMfHobHeads;
  end;
end;

procedure TObsTimesModelLinkList.RemoveLink(AModel: TBaseModel);
var
  Index: Integer;
  ALink: TObsTimesModelLink;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    ALink := FList[Index];
    if ALink.FModel = AModel then
    begin
      FList.Delete(Index);
      Break;
    end;
  end;
end;

end.

