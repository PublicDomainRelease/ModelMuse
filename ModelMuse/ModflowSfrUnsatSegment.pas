unit ModflowSfrUnsatSegment;

interface

uses Windows, Classes, SysUtils, ZLib, RbwParser, GoPhastTypes,
  OrderedCollectionUnit, ModflowCellUnit, ModflowBoundaryUnit,
  FormulaManagerUnit, SubscriptionUnit, SparseDataSets;

type
  TSfrUnsatSegmentRecord = record
    Cell: TCellLocation;
    SaturatedWaterContent: double;
    InitialWaterContent: double;
    BrooksCoreyExponent: double;
    VerticalSaturatedK: double;
    StartingTime: double;
    EndingTime: double;
    BrooksCoreyExponentAnnotation: string;
    SaturatedWaterContentAnnotation: string;
    InitialWaterContentAnnotation: string;
    VerticalSaturatedKAnnotation: string;
    procedure Cache(Comp: TCompressionStream);
    procedure Restore(Decomp: TDecompressionStream);
  end;

  TSrfUnsatSegmentArray = array of TSfrUnsatSegmentRecord;

  TSfrUnsatSegmentStorage = class(TCustomBoundaryStorage)
  private
    FSrfUnsatSegmentArray: TSrfUnsatSegmentArray;
    function GetSrfUnsatSegmentArray: TSrfUnsatSegmentArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property SrfUnsatSegmentArray: TSrfUnsatSegmentArray
      read GetSrfUnsatSegmentArray;
  end;

  // @name represents a MODFLOW Streamflow Routing boundary for one time interval.
  // @name is stored by @link(TSfrUnsatSegmentCollection).
  TSfrUnsatSegmentItem = class(TCustomModflowBoundaryItem)
  private
    FInitialWaterContent: TFormulaObject;
    FSaturatedWaterContent: TFormulaObject;
    FBrooksCoreyExponent: TFormulaObject;
    FVerticalSaturatedK: TFormulaObject;
    procedure SetBrooksCoreyExponent(const Value: string);
    procedure SetSaturatedWaterContent(const Value: string);
    procedure SetInitialWaterContent(const Value: string);
    procedure SetVerticalSaturatedK(const Value: string);
    function GetBrooksCoreyExponent: string;
    function GetInitialWaterContent: string;
    function GetSaturatedWaterContent: string;
    function GetVerticalSaturatedK: string;
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
    procedure InvalidateModel; override;
    function BoundaryFormulaCount: integer; override;
  public
    constructor Create(Collection: TCollection); override;
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    Destructor Destroy; override;
  published
    property BrooksCoreyExponent: string read GetBrooksCoreyExponent write SetBrooksCoreyExponent;
    property InitialWaterContent: string read GetInitialWaterContent write SetInitialWaterContent;
    property SaturatedWaterContent: string read GetSaturatedWaterContent write SetSaturatedWaterContent;
    property VerticalSaturatedK: string read GetVerticalSaturatedK write SetVerticalSaturatedK;
  end;

  // @name represents MODFLOW Streamflow Routing boundaries
  // for a series of time intervals.
  TSfrUnsatSegmentCollection = class(TCustomMF_ArrayBoundColl)
  private
    // @name is used to compute the hydraulic conductivity for a series of
    // Streamflow Routing Boundaries over a series of time intervals.
    FBrooksCoreyExponentData: TModflowTimeList;
    // @name is used to compute the streambed thickness for a series of
    // Streamflow Routing Boundaries over a series of time intervals.
    FInitialWaterContentData: TModflowTimeList;
    // @name is used to compute the streambed elevation for a series of
    // Streamflow Routing Boundaries over a series of time intervals.
    FSaturatedWaterContentData: TModflowTimeList;
    // @name is used to compute the stream slope for a series of
    // Streamflow Routing Boundaries over a series of time intervals.
    FVerticalSaturatedKData: TModflowTimeList;
    FAssignmentLocation: TAssignmentLocation;
    procedure InvalidateSaturatedWaterContentData(Sender: TObject);
    procedure InvalidateInitialWaterContentData(Sender: TObject);
    procedure InvalidateBrooksCoreyExponentData(Sender: TObject);
    procedure InvalidateVerticalSaturatedKData(Sender: TObject);
  protected
    procedure AddSpecificBoundary; override;
    // See @link(TCustomMF_ArrayBoundColl.AssignCellValues
    // TCustomMF_ArrayBoundColl.AssignCellValues)
    procedure AssignCellValues(DataSets: TList;ItemIndex: Integer); override;
    // See @link(TCustomMF_ArrayBoundColl.InitializeTimeLists
    // TCustomMF_ArrayBoundColl.InitializeTimeLists)
    procedure InitializeTimeLists(ListOfTimeLists: TList); override;
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TMF_BoundItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TRivStorage.RivArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer); override;
  public
    property AssignmentLocation: TAssignmentLocation read FAssignmentLocation
      write FAssignmentLocation;
    // @name creates an instance of @classname
    constructor Create(Boundary: TModflowBoundary; Model,
      ScreenObject: TObject); override;
    // @name destroys the current instance of @classname.
    // Do not call @name; call Free instead.
    destructor Destroy; override;
  end;


  TSfrUnsatSegment_Cell = class(TValueCell)
  private
    FValues: TSfrUnsatSegmentRecord;
    FStressPeriod: integer;
    function GetInitialWaterContent: double;
    function GetBrooksCoreyExponent: double;
    function GetSaturatedWaterContent: double;
    function GetBrooksCoreyExponentAnnotation: string;
    function GetSaturatedWaterContentAnnotation: string;
    function GetInitialWaterContentAnnotation: string;
    function GetVerticalSaturatedK: double;
    function GetVerticalSaturatedKAnnotation: string;
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
  public
    property Values: TSfrUnsatSegmentRecord read FValues write FValues;
    property StressPeriod: integer read FStressPeriod write FStressPeriod;
    property BrooksCoreyExponent: double read GetBrooksCoreyExponent;
    property SaturatedWaterContent: double read GetSaturatedWaterContent;
    property InitialWaterContent: double read GetInitialWaterContent;
    property VerticalSaturatedK: double read GetVerticalSaturatedK;
    property BrooksCoreyExponentAnnotation: string read GetBrooksCoreyExponentAnnotation;
    property SaturatedWaterContentAnnotation: string read GetSaturatedWaterContentAnnotation;
    property InitialWaterContentAnnotation: string read GetInitialWaterContentAnnotation;
    property VerticalSaturatedKAnnotation: string read GetVerticalSaturatedKAnnotation;
  end;

implementation

uses Contnrs, DataSetUnit, ScreenObjectUnit, ModflowTimeUnit, PhastModelUnit,
  ModflowSfrUnit, TempFiles, frmGoPhastUnit;

const
  SaturatedWaterContentPosition = 0;
  InitialWaterContentPosition = 1;
  BrooksCoreyExponentPosition = 2;
  VerticalSaturatedKPosition = 3;

{ TSfrUnsatSegmentItem }

procedure TSfrUnsatSegmentItem.Assign(Source: TPersistent);
var
  Sfr: TSfrUnsatSegmentItem;
begin
  if Source is TSfrUnsatSegmentItem then
  begin
    Sfr := TSfrUnsatSegmentItem(Source);
    BrooksCoreyExponent := Sfr.BrooksCoreyExponent;
    InitialWaterContent := Sfr.InitialWaterContent;
    SaturatedWaterContent := Sfr.SaturatedWaterContent;
    VerticalSaturatedK := Sfr.VerticalSaturatedK;
  end;
  inherited;
end;

procedure TSfrUnsatSegmentItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TSfrUnsatSegmentCollection;
  SaturatedWaterContentObserver: TObserver;
  InitialWaterContentObserver: TObserver;
  BrooksCoreyExponentObserver: TObserver;
  VerticalSaturatedKObserver: TObserver;
begin
  ParentCollection := Collection as TSfrUnsatSegmentCollection;
  SaturatedWaterContentObserver := FObserverList[SaturatedWaterContentPosition];
  SaturatedWaterContentObserver.OnUpToDateSet := ParentCollection.InvalidateSaturatedWaterContentData;
  InitialWaterContentObserver := FObserverList[InitialWaterContentPosition];
  InitialWaterContentObserver.OnUpToDateSet := ParentCollection.InvalidateInitialWaterContentData;
  BrooksCoreyExponentObserver := FObserverList[BrooksCoreyExponentPosition];
  BrooksCoreyExponentObserver.OnUpToDateSet := ParentCollection.InvalidateBrooksCoreyExponentData;
  VerticalSaturatedKObserver := FObserverList[VerticalSaturatedKPosition];
  VerticalSaturatedKObserver.OnUpToDateSet := ParentCollection.InvalidateVerticalSaturatedKData;
end;

function TSfrUnsatSegmentItem.BoundaryFormulaCount: integer;
begin
  result := 4;
end;

constructor TSfrUnsatSegmentItem.Create(Collection: TCollection);
begin
  inherited;
  SaturatedWaterContent := '0.3';
  InitialWaterContent := '0.2';
  BrooksCoreyExponent := '3.5';
  VerticalSaturatedK := 'Kx';
end;

procedure TSfrUnsatSegmentItem.CreateFormulaObjects;
begin
  FSaturatedWaterContent := CreateFormulaObject(dso3D);
  FInitialWaterContent := CreateFormulaObject(dso3D);
  FBrooksCoreyExponent := CreateFormulaObject(dso3D);
  FVerticalSaturatedK := CreateFormulaObject(dso3D);
end;

destructor TSfrUnsatSegmentItem.Destroy;
var
  Index: integer;
begin
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    BoundaryFormula[Index] := '0';
  end;
  inherited;
end;

function TSfrUnsatSegmentItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    SaturatedWaterContentPosition:
      result := SaturatedWaterContent;
    InitialWaterContentPosition:
      result := InitialWaterContent;
    BrooksCoreyExponentPosition:
      result := BrooksCoreyExponent;
    VerticalSaturatedKPosition:
      result := VerticalSaturatedK;
    else Assert(False);
  end;
end;

function TSfrUnsatSegmentItem.GetBrooksCoreyExponent: string;
begin
  Result := FBrooksCoreyExponent.Formula;
  ResetItemObserver(BrooksCoreyExponentPosition);
end;

function TSfrUnsatSegmentItem.GetInitialWaterContent: string;
begin
  Result := FInitialWaterContent.Formula;
  ResetItemObserver(InitialWaterContentPosition);
end;

procedure TSfrUnsatSegmentItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FSaturatedWaterContent then
  begin
    List.Add(FObserverList[SaturatedWaterContentPosition]);
  end;
  if Sender = FInitialWaterContent then
  begin
    List.Add(FObserverList[InitialWaterContentPosition]);
  end;
  if Sender = FBrooksCoreyExponent then
  begin
    List.Add(FObserverList[BrooksCoreyExponentPosition]);
  end;
  if Sender = FVerticalSaturatedK then
  begin
    List.Add(FObserverList[VerticalSaturatedKPosition]);
  end;
end;

function TSfrUnsatSegmentItem.GetSaturatedWaterContent: string;
begin
  Result := FSaturatedWaterContent.Formula;
  ResetItemObserver(SaturatedWaterContentPosition);
end;

function TSfrUnsatSegmentItem.GetVerticalSaturatedK: string;
begin
  Result := FVerticalSaturatedK.Formula;
  ResetItemObserver(VerticalSaturatedKPosition);
end;

procedure TSfrUnsatSegmentItem.InvalidateModel;
begin
  inherited;

end;

function TSfrUnsatSegmentItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TSfrUnsatSegmentItem;
begin
  result := (AnotherItem is TSfrUnsatSegmentItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TSfrUnsatSegmentItem(AnotherItem);
    result := (Item.BrooksCoreyExponent = BrooksCoreyExponent)
      and (Item.InitialWaterContent = InitialWaterContent)
      and (Item.SaturatedWaterContent = SaturatedWaterContent)
      and (Item.VerticalSaturatedK = VerticalSaturatedK);
  end;
end;

procedure TSfrUnsatSegmentItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FVerticalSaturatedK,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FBrooksCoreyExponent,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FInitialWaterContent,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FSaturatedWaterContent,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
end;

procedure TSfrUnsatSegmentItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    SaturatedWaterContentPosition:
      SaturatedWaterContent := Value;
    InitialWaterContentPosition:
      InitialWaterContent := Value;
    BrooksCoreyExponentPosition:
      BrooksCoreyExponent := Value;
    VerticalSaturatedKPosition:
      VerticalSaturatedK := Value;
    else Assert(False);
  end;
end;


procedure TSfrUnsatSegmentItem.SetBrooksCoreyExponent(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FBrooksCoreyExponent.Formula <> Value then
  begin
    UpdateFormula(Value, BrooksCoreyExponentPosition, FBrooksCoreyExponent);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState) then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        if Collection = (ScreenObj as TScreenObject).
          ModflowSfrBoundary.UpstreamUnsatSegmentValues then
        begin
          PhastModel.InvalidateMfSfrUpstreamBrooksCorey(self);
        end
        else
        begin
          Assert(Collection = (ScreenObj as TScreenObject).
            ModflowSfrBoundary.DownstreamUnsatSegmentValues);
          PhastModel.InvalidateMfSfrDownstreamBrooksCorey(self);
        end;
      end;
    end;
  end;
end;

procedure TSfrUnsatSegmentItem.SetSaturatedWaterContent(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FSaturatedWaterContent.Formula <> Value then
  begin
    UpdateFormula(Value, SaturatedWaterContentPosition, FSaturatedWaterContent);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState) then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        if Collection = (ScreenObj as TScreenObject).
          ModflowSfrBoundary.UpstreamUnsatSegmentValues then
        begin
          PhastModel.InvalidateMfSfrUpstreamUnsaturatedWaterContent(self);
        end
        else
        begin
          Assert(Collection = (ScreenObj as TScreenObject).
            ModflowSfrBoundary.DownstreamUnsatSegmentValues);
          PhastModel.InvalidateMfSfrDownstreamUnsaturatedWaterContent(self);
        end;
      end;
    end;
  end;
end;

procedure TSfrUnsatSegmentItem.SetInitialWaterContent(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FInitialWaterContent.Formula <> Value then
  begin
    UpdateFormula(Value, InitialWaterContentPosition, FInitialWaterContent);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState) then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        if Collection = (ScreenObj as TScreenObject).
          ModflowSfrBoundary.UpstreamUnsatSegmentValues then
        begin
          PhastModel.InvalidateMfSfrUpstreamUnsatInitialWaterContent(self);
        end
        else
        begin
          Assert(Collection = (ScreenObj as TScreenObject).
            ModflowSfrBoundary.DownstreamUnsatSegmentValues);
          PhastModel.InvalidateMfSfrDownstreamUnsatInitialWaterContent(self);
        end;
      end;
    end;
  end;
end;

procedure TSfrUnsatSegmentItem.SetVerticalSaturatedK(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FVerticalSaturatedK.Formula <> Value then
  begin
    UpdateFormula(Value, VerticalSaturatedKPosition, FVerticalSaturatedK);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState) then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        if Collection = (ScreenObj as TScreenObject).
          ModflowSfrBoundary.UpstreamUnsatSegmentValues then
        begin
          PhastModel.InvalidateMfSfrUpstreamUnsatKz(self);
        end
        else
        begin
          Assert(Collection = (ScreenObj as TScreenObject).
            ModflowSfrBoundary.DownstreamUnsatSegmentValues);
          PhastModel.InvalidateMfSfrDownstreamUnsatKz(self);
        end;
      end;
    end;
  end;
end;

{ TSfrUnsatSegmentCollection }

procedure TSfrUnsatSegmentCollection.AddSpecificBoundary;
begin
  AddBoundary(TSfrUnsatSegmentStorage.Create);
end;

procedure TSfrUnsatSegmentCollection.AssignCellValues(DataSets: TList;
  ItemIndex: Integer);
var
  BrooksCoreyExponentArray: TDataArray;
  InitialWaterContentArray: TDataArray;
  SaturatedWaterContentArray: TDataArray;
  VerticalSaturatedKArray: TDataArray;
  Boundary: TSfrUnsatSegmentStorage;
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
  SaturatedWaterContentArray := DataSets[0];
  InitialWaterContentArray := DataSets[1];
  BrooksCoreyExponentArray := DataSets[2];
  VerticalSaturatedKArray := DataSets[3];
  Boundary := Boundaries[ItemIndex] as TSfrUnsatSegmentStorage;

  SaturatedWaterContentArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
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
            if SaturatedWaterContentArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              Assert(InitialWaterContentArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(BrooksCoreyExponentArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(VerticalSaturatedKArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(BoundaryIndex < Length(Boundary.SrfUnsatSegmentArray));
              with Boundary.SrfUnsatSegmentArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                BrooksCoreyExponent := BrooksCoreyExponentArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                BrooksCoreyExponentAnnotation := BrooksCoreyExponentArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                InitialWaterContent := InitialWaterContentArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                InitialWaterContentAnnotation := InitialWaterContentArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                SaturatedWaterContent := SaturatedWaterContentArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                SaturatedWaterContentAnnotation := SaturatedWaterContentArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                VerticalSaturatedK := VerticalSaturatedKArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                VerticalSaturatedKAnnotation := VerticalSaturatedKArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  SaturatedWaterContentArray.CacheData;
  InitialWaterContentArray.CacheData;
  BrooksCoreyExponentArray.CacheData;
  VerticalSaturatedKArray.CacheData;
  Boundary.CacheData;
end;


constructor TSfrUnsatSegmentCollection.Create(Boundary: TModflowBoundary; Model,
  ScreenObject: TObject);
begin
  inherited Create(Boundary, Model, ScreenObject);
  FBrooksCoreyExponentData := TModflowTimeList.Create(Model);
  FInitialWaterContentData := TModflowTimeList.Create(Model);
  FSaturatedWaterContentData := TModflowTimeList.Create(Model);
  FVerticalSaturatedKData := TModflowTimeList.Create(Model);

  FInitialWaterContentData.NonParamDescription := 'Initial water content';
  FInitialWaterContentData.ParamDescription := ' initial water content';
  FSaturatedWaterContentData.NonParamDescription := 'Saturated water content';
  FSaturatedWaterContentData.ParamDescription := ' saturated water content';
  FBrooksCoreyExponentData.NonParamDescription := 'Brooks-Corey exponent';
  FBrooksCoreyExponentData.ParamDescription := ' Brooks-Corey exponent';
  FVerticalSaturatedKData.NonParamDescription := 'Maximum vertical K';
  FVerticalSaturatedKData.ParamDescription := ' maximum vertical K';

  if Model <> nil then
  begin
//    FInitialWaterContentData.OnInvalidate := (Model as TPhastModel).InvalidateMfRivStage;
//    FSaturatedWaterContentData.OnInvalidate := (Model as TPhastModel).InvalidateMfRivConductance;
//    FBrooksCoreyExponentData.OnInvalidate := (Model as TPhastModel).InvalidateMfRivBottom;
//    FVerticalSaturatedKData.OnInvalidate := (Model as TPhastModel).InvalidateMfRivBottom;
  end;

  AddTimeList(FSaturatedWaterContentData);
  AddTimeList(FInitialWaterContentData);
  AddTimeList(FBrooksCoreyExponentData);
  AddTimeList(FVerticalSaturatedKData);
end;

destructor TSfrUnsatSegmentCollection.Destroy;
begin
  FInitialWaterContentData.Free;
  FSaturatedWaterContentData.Free;
  FBrooksCoreyExponentData.Free;
  FVerticalSaturatedKData.Free;
  inherited;
end;

procedure TSfrUnsatSegmentCollection.InitializeTimeLists(ListOfTimeLists: TList);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TSfrUnsatSegmentItem;
  Boundary: TSfrBoundary;
  ScreenObject: TScreenObject;
  ISFROPT: integer;
  ICALC: integer;
  ItemUsed: boolean;
begin
  ISFROPT := (Model as TPhastModel).ModflowPackages.SfrPackage.Isfropt;

  SetLength(BoundaryValues, Count);
  Boundary := BoundaryGroup as TSfrBoundary;
  ScreenObject := Boundary.ScreenObject as TScreenObject;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSfrUnsatSegmentItem;
    BoundaryValues[Index].Time := Item.StartTime;
    Assert(ScreenObject.ModflowSfrBoundary <> nil);
    ICALC := ScreenObject.ModflowSfrBoundary.ParamIcalc.ICalc(Item.StartTime);
    ItemUsed := (ISFROPT  in [4,5]) and (ICALC in [1,2]) and (Index = 0);
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.BrooksCoreyExponent;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  FBrooksCoreyExponentData.Initialize(BoundaryValues, ScreenObject,
    AssignmentLocation);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSfrUnsatSegmentItem;
    BoundaryValues[Index].Time := Item.StartTime;
    Assert(ScreenObject.ModflowSfrBoundary <> nil);
    ICALC := ScreenObject.ModflowSfrBoundary.ParamIcalc.ICalc(Item.StartTime);
    ItemUsed := (ISFROPT  in [4,5]) and (ICALC in [1,2]) and (Index = 0);
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.InitialWaterContent;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  FInitialWaterContentData.Initialize(BoundaryValues, ScreenObject,
    AssignmentLocation);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSfrUnsatSegmentItem;
    BoundaryValues[Index].Time := Item.StartTime;
    Assert(ScreenObject.ModflowSfrBoundary <> nil);
    ICALC := ScreenObject.ModflowSfrBoundary.ParamIcalc.ICalc(Item.StartTime);
    ItemUsed := (ISFROPT  in [4,5]) and (ICALC in [1,2]) and (Index = 0);
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.SaturatedWaterContent;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  FSaturatedWaterContentData.Initialize(BoundaryValues, ScreenObject,
    AssignmentLocation);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSfrUnsatSegmentItem;
    BoundaryValues[Index].Time := Item.StartTime;
    Assert(ScreenObject.ModflowSfrBoundary <> nil);
    ICALC := ScreenObject.ModflowSfrBoundary.ParamIcalc.ICalc(Item.StartTime);
    ItemUsed := (ISFROPT  = 5) and (ICALC in [1,2]) and (Index = 0);
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.VerticalSaturatedK;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  FVerticalSaturatedKData.Initialize(BoundaryValues, ScreenObject,
    AssignmentLocation);

  Assert(FBrooksCoreyExponentData.Count = Count);
  Assert(FInitialWaterContentData.Count = Count);
  Assert(FSaturatedWaterContentData.Count = Count);
  Assert(FVerticalSaturatedKData.Count = Count);
  ClearBoundaries;
  SetBoundaryCapacity(FBrooksCoreyExponentData.Count);
  for TimeIndex := 0 to FBrooksCoreyExponentData.Count - 1 do
  begin
    AddBoundary(TSfrUnsatSegmentStorage.Create);
  end;
  ListOfTimeLists.Add(FSaturatedWaterContentData);
  ListOfTimeLists.Add(FInitialWaterContentData);
  ListOfTimeLists.Add(FBrooksCoreyExponentData);
  ListOfTimeLists.Add(FVerticalSaturatedKData);
end;

procedure TSfrUnsatSegmentCollection.InvalidateBrooksCoreyExponentData(
  Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FBrooksCoreyExponentData.Invalidate;
  end;
end;

procedure TSfrUnsatSegmentCollection.InvalidateInitialWaterContentData(
  Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FInitialWaterContentData.Invalidate;
  end;
end;

procedure TSfrUnsatSegmentCollection.InvalidateSaturatedWaterContentData(
  Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FSaturatedWaterContentData.Invalidate;
  end;
end;

procedure TSfrUnsatSegmentCollection.InvalidateVerticalSaturatedKData(
  Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FVerticalSaturatedKData.Invalidate;
  end;
end;

class function TSfrUnsatSegmentCollection.ItemClass: TMF_BoundItemClass;
begin
  result := TSfrUnsatSegmentItem;
end;

procedure TSfrUnsatSegmentCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer);
begin
  SetLength((Boundaries[ItemIndex] as TSfrUnsatSegmentStorage).
    FSrfUnsatSegmentArray, BoundaryCount);
  inherited;
end;

{ TSfrUnsatSegment_Cell }

function TSfrUnsatSegment_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

procedure TSfrUnsatSegment_Cell.Cache(Comp: TCompressionStream);
begin
  inherited;
  Values.Cache(Comp);
  WriteCompInt(Comp, StressPeriod);
end;

function TSfrUnsatSegment_Cell.GetBrooksCoreyExponent: double;
begin
  result := Values.BrooksCoreyExponent;
end;

function TSfrUnsatSegment_Cell.GetBrooksCoreyExponentAnnotation: string;
begin
  result := Values.BrooksCoreyExponentAnnotation;
end;

function TSfrUnsatSegment_Cell.GetIntegerAnnotation(Index: integer): string;
begin
  result := '';
  Assert(False);
end;

function TSfrUnsatSegment_Cell.GetIntegerValue(Index: integer): integer;
begin
  result := 0;
  Assert(False);
end;

function TSfrUnsatSegment_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TSfrUnsatSegment_Cell.GetRealAnnotation(Index: integer): string;
begin
  result := '';
  case Index of
    0: result := SaturatedWaterContentAnnotation;
    1: result := InitialWaterContentAnnotation;
    2: result := BrooksCoreyExponentAnnotation;
    3: result := VerticalSaturatedKAnnotation;
    else Assert(False);
  end;
end;

function TSfrUnsatSegment_Cell.GetRealValue(Index: integer): double;
begin
  result := 0;
  case Index of
    0: result := SaturatedWaterContent;
    1: result := InitialWaterContent;
    2: result := BrooksCoreyExponent;
    3: result := VerticalSaturatedK;
    else Assert(False);
  end;
end;

function TSfrUnsatSegment_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TSfrUnsatSegment_Cell.GetSaturatedWaterContent: double;
begin
  result := Values.SaturatedWaterContent;
end;

function TSfrUnsatSegment_Cell.GetSaturatedWaterContentAnnotation: string;
begin
  result := Values.SaturatedWaterContentAnnotation;
end;

function TSfrUnsatSegment_Cell.GetInitialWaterContent: double;
begin
  result := Values.InitialWaterContent;
end;

function TSfrUnsatSegment_Cell.GetInitialWaterContentAnnotation: string;
begin
  result := Values.InitialWaterContentAnnotation;
end;

function TSfrUnsatSegment_Cell.GetVerticalSaturatedK: double;
begin
  result := Values.VerticalSaturatedK;
end;

function TSfrUnsatSegment_Cell.GetVerticalSaturatedKAnnotation: string;
begin
  result := Values.VerticalSaturatedKAnnotation;
end;
procedure TSfrUnsatSegment_Cell.Restore(Decomp: TDecompressionStream);
begin
  inherited;
  Values.Restore(Decomp);
  StressPeriod := ReadCompInt(Decomp);
end;

{ TSfrUnsatSegmentRecord }

procedure TSfrUnsatSegmentRecord.Cache(Comp: TCompressionStream);
begin
  WriteCompCell(Comp, Cell);

  WriteCompReal(Comp, SaturatedWaterContent);
  WriteCompReal(Comp, InitialWaterContent);
  WriteCompReal(Comp, BrooksCoreyExponent);
  WriteCompReal(Comp, VerticalSaturatedK);

  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);

  WriteCompString(Comp, BrooksCoreyExponentAnnotation);
  WriteCompString(Comp, SaturatedWaterContentAnnotation);
  WriteCompString(Comp, InitialWaterContentAnnotation);
  WriteCompString(Comp, VerticalSaturatedKAnnotation);

end;

procedure TSfrUnsatSegmentRecord.Restore(Decomp: TDecompressionStream);
begin
  Cell := ReadCompCell(Decomp);
  SaturatedWaterContent := ReadCompReal(Decomp);
  InitialWaterContent := ReadCompReal(Decomp);
  BrooksCoreyExponent := ReadCompReal(Decomp);
  VerticalSaturatedK := ReadCompReal(Decomp);

  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);

  BrooksCoreyExponentAnnotation := ReadCompString(Decomp);
  SaturatedWaterContentAnnotation := ReadCompString(Decomp);
  InitialWaterContentAnnotation := ReadCompString(Decomp);
  VerticalSaturatedKAnnotation := ReadCompString(Decomp);
end;

{ TSfrUnsatSegmentStorage }

procedure TSfrUnsatSegmentStorage.Clear;
begin
  SetLength(FSrfUnsatSegmentArray, 0);
  FCleared := True;
end;

procedure TSfrUnsatSegmentStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
begin
  Count := Length(FSrfUnsatSegmentArray);
  Compressor.Write(Count, SizeOf(Count));
  for Index := 0 to Count - 1 do
  begin
    FSrfUnsatSegmentArray[Index].Cache(Compressor);
  end;
end;

procedure TSfrUnsatSegmentStorage.Restore(DecompressionStream: TDecompressionStream);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FSrfUnsatSegmentArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FSrfUnsatSegmentArray[Index].Restore(DecompressionStream);
  end;
end;

function TSfrUnsatSegmentStorage.GetSrfUnsatSegmentArray: TSrfUnsatSegmentArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FSrfUnsatSegmentArray;
end;

end.