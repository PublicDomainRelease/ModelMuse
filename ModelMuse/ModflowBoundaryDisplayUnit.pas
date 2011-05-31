unit ModflowBoundaryDisplayUnit;

interface

uses Windows, SysUtils, Classes, DataSetUnit, SparseDataSets, ZLib,
  GoPhastTypes;

type
  TOnGetUseList = procedure (Sender: TObject;
    NewUseList: TStringList) of object;

  TModflowBoundaryDisplayDataArray = class(TRealSparseDataSet)
  private
    FCount: T3DSparseIntegerArray;
    function GetCellCount(Layer, Row, Column: integer): integer;
    procedure SetCellCount(Layer, Row, Column: integer; const Value: integer);
  protected
    procedure Clear; override;
    procedure SetDimensions(const SetToZero: boolean); override;
    procedure SetUpToDate(const Value: boolean); override;
    procedure StoreData(Stream: TStream); override;
    procedure ReadData(DecompressionStream: TDecompressionStream); override;
  public
    procedure AddDataArray(DataArray: TDataArray);
    procedure AddDataValue(const DataAnnotation: string; DataValue: Double;
      ColIndex, RowIndex, LayerIndex: Integer);
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure ComputeAverage;
    procedure LabelAsSum;
    property CellCount[Layer, Row, Column: integer]: integer read GetCellCount
      Write SetCellCount;
  end;

  TModflowBoundaryDisplayTimeList = class(TCustomTimeList)
  private
    FOnInitialize: TNotifyEvent;
    FOnGetUseList: TOnGetUseList;
    procedure SetOnInitialize(const Value: TNotifyEvent);
  protected
    FUseList: TStringList;
    procedure CreateNewDataSets; virtual;
  public
    procedure Initialize; override;
    property OnInitialize: TNotifyEvent read FOnInitialize
      write SetOnInitialize;
    property OnGetUseList: TOnGetUseList read FOnGetUseList
      write FOnGetUseList;
    // @name creates an instance of @classname.
    constructor Create(Model: TBaseModel);
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;
    procedure ComputeAverage;
    procedure LabelAsSum;
    procedure CreateDataSets;
    // See @link(TCustomTimeList.SetUpToDate TCustomTimeList.SetUpToDate).
    // @name is public in @classname instead of protected as in
    // @link(TCustomTimeList).
    procedure SetUpToDate(const Value: boolean); override;
  end;

  THobDisplayTimeList = class(TModflowBoundaryDisplayTimeList)
  protected
    procedure CreateNewDataSets; override;
  end;

  TModflowBoundListOfTimeLists = class(TObject)
  private
    FList: TList;
    function GetCount: integer;
    function GetItem(Index: integer): TModflowBoundaryDisplayTimeList;
  published
  public
    procedure Add(Item: TModflowBoundaryDisplayTimeList);
    property Count: integer read GetCount;
    property Items[Index: integer]: TModflowBoundaryDisplayTimeList
      read GetItem; default;
    Constructor Create;
    destructor Destroy; override;
  end;

implementation

uses SparseArrayUnit, PhastModelUnit, frmGoPhastUnit,
  ModflowTimeUnit, SubscriptionUnit, RealListUnit, ScreenObjectUnit,
  ModflowHobUnit, TempFiles, IntListUnit, CustomModflowWriterUnit, 
  frmProgressUnit;

{ TModflowBoundaryDisplayDataArray }

procedure TModflowBoundaryDisplayDataArray.AddDataArray(DataArray: TDataArray);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  DataValue: Double;
  DataAnnotation: string;
begin
  for LayerIndex := 0 to DataArray.LayerCount - 1 do
  begin
    for RowIndex := 0 to DataArray.RowCount - 1 do
    begin
      for ColIndex := 0 to DataArray.ColumnCount - 1 do
      begin
        if DataArray.IsValue[LayerIndex, RowIndex, ColIndex] then
        begin
          DataValue := DataArray.RealData[LayerIndex, RowIndex, ColIndex];
          DataAnnotation :=
            DataArray.Annotation[LayerIndex, RowIndex, ColIndex];
          AddDataValue(DataAnnotation, DataValue,
            ColIndex, RowIndex, LayerIndex);
        end;
      end;
    end;
  end;
end;

procedure TModflowBoundaryDisplayDataArray.Clear;
begin
  inherited;
  FCount.Clear;
end;

procedure TModflowBoundaryDisplayDataArray.ComputeAverage;
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  for LayerIndex := 0 to LayerCount - 1 do
  begin
    for RowIndex := 0 to RowCount - 1 do
    begin
      for ColIndex := 0 to ColumnCount - 1 do
      begin
        if IsValue[LayerIndex, RowIndex, ColIndex] then
        begin
          if FCount[LayerIndex, RowIndex, ColIndex] > 1 then
          begin
            RealData[LayerIndex, RowIndex, ColIndex] :=
              RealData[LayerIndex, RowIndex, ColIndex]
              / FCount[LayerIndex, RowIndex, ColIndex];
            Annotation[LayerIndex, RowIndex, ColIndex] :=
              'Average of:' + EndOfLine +
              Annotation[LayerIndex, RowIndex, ColIndex];
            FDataCached := False;
          end;
        end;
      end;
    end;
  end;
end;

procedure TModflowBoundaryDisplayDataArray.AddDataValue(
  const DataAnnotation: string; DataValue: Double;
  ColIndex, RowIndex, LayerIndex: Integer);
begin
  // The way the annotations are handled here is related to the
  // fact that strings are reference counted variables.
  // A new string will be allocated each time a string is changed
  // but if a string is just copied all that really happens is that
  // the reference count of the string is increased.  Thus, it
  // saves memory to change a string only when it really needs to be
  // changed and to just copy it whenever possible.
  if IsValue[LayerIndex, RowIndex, ColIndex] then
  begin
    if Annotation[LayerIndex, RowIndex, ColIndex] = StrNoValueAssigned then
    begin
      // If the array has been previously set to a
      // default value of zero,
      // just replace the old annotaion with the new annotation.
      Annotation[LayerIndex, RowIndex, ColIndex] := DataAnnotation;
    end
    else
    begin
      // One or more values has been previously assigned
      // to this cell.
      // Each value will need to be included as part of the
      // annotation.
      if FCount[LayerIndex, RowIndex, ColIndex] = 1 then
      begin
        // One value has been previously assigned to this cell.
        // The value associated with that annotation was not part
        // of the original annotation so the previous value
        // needs to be included along with the original annotation.
        Annotation[LayerIndex, RowIndex, ColIndex] :=
          FloatToStr(RealData[LayerIndex, RowIndex, ColIndex])  + ' '
          + Annotation[LayerIndex, RowIndex, ColIndex] + EndOfLine
          + FloatToStr(DataValue) + ' ' + DataAnnotation;
      end
      else
      begin
        // Include the new value and new annotation in
        // the annotation for the cell.
        Annotation[LayerIndex, RowIndex, ColIndex] :=
          Annotation[LayerIndex, RowIndex, ColIndex] + EndOfLine
          + FloatToStr(DataValue) + ' ' + DataAnnotation;
      end;
    end;
    RealData[LayerIndex, RowIndex, ColIndex] :=
      RealData[LayerIndex, RowIndex, ColIndex] + DataValue;
    FCount[LayerIndex, RowIndex, ColIndex] :=
      FCount[LayerIndex, RowIndex, ColIndex] + 1;
  end
  else
  begin
    // No previous value has been assigned to this cell.
    // Just make a copy of the annotation.
    RealData[LayerIndex, RowIndex, ColIndex] := DataValue;
    Annotation[LayerIndex, RowIndex, ColIndex] := DataAnnotation;
    FCount[LayerIndex, RowIndex, ColIndex] := 1;
  end;
end;

constructor TModflowBoundaryDisplayDataArray.Create(AnOwner: TComponent);
begin
  inherited;
  FDataCached := False;
  FCount:= T3DSparseIntegerArray.Create(SPASmall);
end;

destructor TModflowBoundaryDisplayDataArray.Destroy;
begin
  inherited;
  FCount.Free;
end;

function TModflowBoundaryDisplayDataArray.GetCellCount(Layer, Row,
  Column: integer): integer;
begin
  result := FCount[Layer, Row, Column];
end;

procedure TModflowBoundaryDisplayDataArray.LabelAsSum;
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  for LayerIndex := 0 to LayerCount - 1 do
  begin
    for RowIndex := 0 to RowCount - 1 do
    begin
      for ColIndex := 0 to ColumnCount - 1 do
      begin
        if IsValue[LayerIndex, RowIndex, ColIndex] then
        begin
          if FCount[LayerIndex, RowIndex, ColIndex] > 1 then
          begin
            Annotation[LayerIndex, RowIndex, ColIndex] :=
              'Sum of:' + EndOfLine
              + Annotation[LayerIndex, RowIndex, ColIndex];
            FDataCached := False;
          end;
        end;
      end;
    end;
  end;
end;

procedure TModflowBoundaryDisplayDataArray.ReadData(
  DecompressionStream: TDecompressionStream);
var
  Count: Integer;
  LayerArray: array of Integer;
  RowArray: array of Integer;
  ColumnArray: array of Integer;
  IntegerValues: array of Integer;
  Index: Integer;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  IntValue: Integer;
begin
  inherited ReadData(DecompressionStream);
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(LayerArray, Count);
  SetLength(RowArray, Count);
  SetLength(ColumnArray, Count);
  SetLength(IntegerValues, Count);
  if Count > 0 then
  begin
    DecompressionStream.Read(LayerArray[0], Count*SizeOf(integer));
    DecompressionStream.Read(RowArray[0], Count*SizeOf(integer));
    DecompressionStream.Read(ColumnArray[0], Count*SizeOf(integer));
    DecompressionStream.Read(IntegerValues[0], Count*SizeOf(integer));

    for Index := 0 to Count - 1 do
    begin
      LayerIndex := LayerArray[Index];
      RowIndex := RowArray[Index];
      ColIndex := ColumnArray[Index];
      IntValue := IntegerValues[Index];
      CellCount[LayerIndex, RowIndex, ColIndex] := IntValue;
    end;
  end;

end;

procedure TModflowBoundaryDisplayDataArray.SetCellCount(Layer, Row,
  Column: integer; const Value: integer);
begin
  FCount[Layer, Row, Column] := Value;
end;

procedure TModflowBoundaryDisplayDataArray.SetDimensions(
  const SetToZero: boolean);
begin
  inherited;
  FCount.Clear;
end;

procedure TModflowBoundaryDisplayDataArray.SetUpToDate(const Value: boolean);
begin
  if Value and not UpToDate then
  begin
    FDataCached := False;
    FDimensionsChanged := False;
  end;
  inherited;
end;

procedure TModflowBoundaryDisplayDataArray.StoreData(
  Stream: TStream);
var
  Count: Integer;
  LayerLimit: Integer;
  RowLimit: Integer;
  ColLimit: Integer;
  LayerMin: Integer;
  RowMin: Integer;
  ColMin: Integer;
  LayerArray: array of Integer;
  RowArray: array of Integer;
  ColumnArray: array of Integer;
  IntegerValues: array of Integer;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  inherited StoreData(Stream);
  Count := 0;
  CountValues(LayerLimit, RowLimit, ColLimit, Count);
  GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
    LayerLimit, RowLimit, ColLimit);
  if Count > 0 then
  begin
    SetLength(LayerArray, Count);
    SetLength(RowArray, Count);
    SetLength(ColumnArray, Count);
    SetLength(IntegerValues, Count);
    Count := 0;
    for LayerIndex := LayerMin to LayerLimit do
    begin
      for RowIndex := RowMin to RowLimit do
      begin
        for ColIndex := ColMin to ColLimit do
        begin
          if IsValue[LayerIndex, RowIndex, ColIndex] then
          begin
            LayerArray[Count] := LayerIndex;
            RowArray[Count] := RowIndex;
            ColumnArray[Count] := ColIndex;
            IntegerValues[Count] := CellCount[LayerIndex, RowIndex, ColIndex];
            Inc(Count);
          end;
        end;
      end;
    end;
    Stream.Write(Count, SizeOf(Count));
    Stream.Write(LayerArray[0], Count*SizeOf(integer));
    Stream.Write(RowArray[0], Count*SizeOf(integer));
    Stream.Write(ColumnArray[0], Count*SizeOf(integer));
    Stream.Write(IntegerValues[0], Count*SizeOf(integer));

  end;

end;

{ TModflowBoundaryDisplayTimeList }

procedure TModflowBoundaryDisplayTimeList.ComputeAverage;
var
  TimeIndex: Integer;
  DataArray: TModflowBoundaryDisplayDataArray;
begin
  for TimeIndex := 0 to Count - 1 do
  begin
    DataArray := Items[TimeIndex] as TModflowBoundaryDisplayDataArray;
    DataArray.ComputeAverage;
    DataArray.CacheData;
  end;
end;

constructor TModflowBoundaryDisplayTimeList.Create(Model: TBaseModel);
begin
  inherited;
  FUseList := TStringList.Create;
  FUseList.Sorted := True;
  Orientation := dso3D;
  Direction := dso3D;
end;

destructor TModflowBoundaryDisplayTimeList.Destroy;
begin
  FUseList.Free;
  inherited;
end;

procedure TModflowBoundaryDisplayTimeList.Initialize;
var
  LocalModel: TCustomModel;
  TimeIndex: Integer;
  Index: Integer;
begin
  If UpToDate then Exit;
  Assert(Assigned(OnGetUseList));

  frmProgressMM.ShouldContinue := True;
  frmProgressMM.btnAbort.Visible := False;
  if not frmProgressMM.Visible then
  begin
    frmProgressMM.Caption := 'Progress';
  end;
  frmProgressMM.Show;
  LocalModel := Model as TCustomModel;
  LocalModel.UpdateModflowFullStressPeriods;
  TimeIndex := LocalModel.ModflowFullStressPeriods.
    FindStressPeriod(LocalModel.ThreeDDisplayTime);
  if TimeIndex < 0 then
  begin
    TimeIndex := 0;
  end;

  LocalModel.ModflowFullStressPeriods.BeginUpdate;
  try
    for Index := LocalModel.ModflowFullStressPeriods.Count - 1 downto 0 do
    begin
      if Index <> TimeIndex then
      begin
        LocalModel.ModflowFullStressPeriods.Delete(Index);
      end;
    end;
  finally
    LocalModel.ModflowFullStressPeriods.EndUpdate;
  end;

  Assert(Assigned(OnInitialize));
  OnInitialize(self);
  SetUpToDate(True);
  frmProgressMM.Hide;
end;

procedure TModflowBoundaryDisplayTimeList.LabelAsSum;
var
  TimeIndex: Integer;
  DataArray: TModflowBoundaryDisplayDataArray;
begin
  for TimeIndex := 0 to Count - 1 do
  begin
    DataArray := Items[TimeIndex] as TModflowBoundaryDisplayDataArray;
    DataArray.LabelAsSum;
    DataArray.CacheData;
  end;
end;

procedure TModflowBoundaryDisplayTimeList.CreateDataSets;
var
  ObservedItem: TObserver;
  Index: Integer;
  DataArray: TModflowBoundaryDisplayDataArray;
  TimeIndex: Integer;
  LocalModel: TCustomModel;
begin
  LocalModel := Model as TCustomModel;
  for TimeIndex := 0 to Count - 1 do
  begin
    DataArray := Items[TimeIndex] as TModflowBoundaryDisplayDataArray;
    for Index := 0 to FUseList.Count - 1 do
    begin
      ObservedItem := LocalModel.GetObserverByName(FUseList[Index]);
      if ObservedItem <> nil then
      begin
        ObservedItem.StopsTalkingTo(DataArray);
      end;
    end;
  end;
  Clear;
  FUseList.Clear;
  CreateNewDataSets;
end;

procedure TModflowBoundaryDisplayTimeList.SetOnInitialize(
  const Value: TNotifyEvent);
begin
  if Addr(FOnInitialize) <> Addr(Value) then
  begin
    FOnInitialize := Value;
    Invalidate;
  end;
end;

procedure TModflowBoundaryDisplayTimeList.SetUpToDate(const Value: boolean);
begin
  inherited;
  // do nothing
end;

procedure TModflowBoundaryDisplayTimeList.CreateNewDataSets;
var
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Index: Integer;
  LocalModel: TCustomModel;
  ObservedItem: TObserver;
  DataArray: TModflowBoundaryDisplayDataArray;
begin
  LocalModel := Model as TCustomModel;
  FUseList.Sorted := True;
  OnGetUseList(self, FUseList);
  for TimeIndex := 0 to LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    DataArray := TModflowBoundaryDisplayDataArray.Create(LocalModel);
    DataArray.Orientation := dso3D;
    DataArray.EvaluatedAt := eaBlocks;
    DataArray.Limits := Limits;
    Add(StressPeriod.StartTime, DataArray);
    DataArray.UpdateDimensions(LocalModel.ModflowGrid.LayerCount,
      LocalModel.ModflowGrid.RowCount, LocalModel.ModflowGrid.ColumnCount);
    for Index := 0 to FUseList.Count - 1 do
    begin
      ObservedItem := LocalModel.GetObserverByName(FUseList[Index]);
      Assert(ObservedItem <> nil);
      ObservedItem.TalksTo(DataArray);
    end;
  end;
end;

{ TModflowBoundListOfTimeLists }

procedure TModflowBoundListOfTimeLists.Add(
  Item: TModflowBoundaryDisplayTimeList);
begin
  FList.Add(Item);
end;

constructor TModflowBoundListOfTimeLists.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TModflowBoundListOfTimeLists.Destroy;
begin
  FList.Free;
  inherited;
end;

function TModflowBoundListOfTimeLists.GetCount: integer;
begin
  result := FList.Count
end;

function TModflowBoundListOfTimeLists.GetItem(
  Index: integer): TModflowBoundaryDisplayTimeList;
begin
  result := FList[Index];
end;

procedure THobDisplayTimeList.CreateNewDataSets;
var
  Times: TRealList;
  LocalModel: TCustomModel;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  TimeIndex: Integer;
  Obs: THobBoundary;
  Item: THobItem;
  DataArray: TModflowBoundaryDisplayDataArray;
  Index: Integer;
  ObservedItem: TObserver;
begin
  LocalModel := Model as TCustomModel;
  FUseList.Sorted := True;
  OnGetUseList(self, FUseList);
  Times := TRealList.Create;
  try
    Times.Sorted := True;
    for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
    begin
      ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      if (ScreenObject.ModflowHeadObservations <> nil)
        and ScreenObject.ModflowHeadObservations.Used then
      begin
        Obs := ScreenObject.ModflowHeadObservations;
        for TimeIndex := 0 to Obs.Values.Count - 1 do
        begin
          Item := Obs.Values.HobItems[TimeIndex];
          Times.AddUnique(Item.Time);
        end;
      end;
    end;
    for TimeIndex := 0 to Times.Count - 1 do
    begin
      DataArray := TModflowBoundaryDisplayDataArray.Create(LocalModel);
      DataArray.Orientation := dso3D;
      DataArray.EvaluatedAt := eaBlocks;
      Add(Times[TimeIndex], DataArray);
      DataArray.UpdateDimensions(LocalModel.ModflowGrid.LayerCount,
        LocalModel.ModflowGrid.RowCount, LocalModel.ModflowGrid.ColumnCount);
      for Index := 0 to FUseList.Count - 1 do
      begin
        ObservedItem := LocalModel.GetObserverByName(FUseList[Index]);
        Assert(ObservedItem <> nil);
        ObservedItem.TalksTo(DataArray);
      end;
    end;
  finally
    Times.Free;
  end;

end;

end.
