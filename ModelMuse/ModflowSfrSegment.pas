unit ModflowSfrSegment;

interface

uses Windows, Classes, SysUtils, ZLib, RbwParser, GoPhastTypes,
  OrderedCollectionUnit, ModflowCellUnit, ModflowBoundaryUnit,
  FormulaManagerUnit, SubscriptionUnit, SparseDataSets;

type
  TSfrSegmentRecord = record
    Cell: TCellLocation;
    StreambedElevation: double;
    StreamBedThickness: double;
    HydraulicConductivity: double;
    StreamWidth: double;
    StreamDepth: double;
    StartingTime: double;
    EndingTime: double;
    HydraulicConductivityAnnotation: string;
    StreambedElevationAnnotation: string;
    StreamBedThicknessAnnotation: string;
    StreamWidthAnnotation: string;
    StreamDepthAnnotation: string;
    procedure Cache(Comp: TCompressionStream);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
  end;

  TSrfSegmentArray = array of TSfrSegmentRecord;

  TSfrSegmentStorage = class(TCustomBoundaryStorage)
  private
    FSrfSegmentArray: TSrfSegmentArray;
    function GetSrfSegmentArray: TSrfSegmentArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property SrfSegmentArray: TSrfSegmentArray read GetSrfSegmentArray;
  end;

  // @name represents a MODFLOW Streamflow Routing boundary for one time interval.
  // @name is stored by @link(TSfrSegmentCollection).
  TSfrSegmentItem = class(TCustomModflowBoundaryItem)
  private
    FStreamBedThickness: TFormulaObject;
    FStreambedElevation: TFormulaObject;
    FHydraulicConductivity: TFormulaObject;
    FStreamWidth: TFormulaObject;
    FStreamDepth: TFormulaObject;
    procedure SetHydraulicConductivity(const Value: string);
    procedure SetStreambedElevation(const Value: string);
    procedure SetStreamBedThickness(const Value: string);
    procedure SetStreamDepth(const Value: string);
    procedure SetStreamWidth(const Value: string);
    function GetHydraulicConductivity: string;
    function GetStreambedElevation: string;
    function GetStreamBedThickness: string;
    function GetStreamDepth: string;
    function GetStreamWidth: string;
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
    property HydraulicConductivity: string read GetHydraulicConductivity write SetHydraulicConductivity;
    property StreamBedThickness: string read GetStreamBedThickness write SetStreamBedThickness;
    property StreambedElevation: string read GetStreambedElevation write SetStreambedElevation;
    property StreamWidth: string read GetStreamWidth write SetStreamWidth;
    property StreamDepth: string read GetStreamDepth write SetStreamDepth;
  end;

  // @name represents MODFLOW Streamflow Routing boundaries
  // for a series of time intervals.
  TSfrSegmentCollection = class(TCustomMF_ArrayBoundColl)
  private
    // @name is used to compute the hydraulic conductivity for a series of
    // Streamflow Routing Boundaries over a series of time intervals.
    FHydraulicConductivityData: TModflowTimeList;
    // @name is used to compute the streambed thickness for a series of
    // Streamflow Routing Boundaries over a series of time intervals.
    FStreamBedThicknessData: TModflowTimeList;
    // @name is used to compute the streambed elevation for a series of
    // Streamflow Routing Boundaries over a series of time intervals.
    FStreamBedElevationData: TModflowTimeList;
    // @name is used to compute the stream slope for a series of
    // Streamflow Routing Boundaries over a series of time intervals.
    FStreamWidthData: TModflowTimeList;
    FStreamDepthData: TModflowTimeList;
    FAssignmentLocation: TAssignmentLocation;
    procedure InvalidateHydraulicConductivityData(Sender: TObject);
    procedure InvalidateStreamBedThicknessData(Sender: TObject);
    procedure InvalidateStreambedElevationData(Sender: TObject);
    procedure InvalidateStreamWidthData(Sender: TObject);
    procedure InvalidateStreamDepthData(Sender: TObject);
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

  TSfrSegmentParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;

  TSfrSegment_Cell = class(TValueCell)
  private
    FValues: TSfrSegmentRecord;
    FStressPeriod: integer;
    function GetStreamBedThickness: double;
    function GetHydraulicConductivity: double;
    function GetStreambedElevation: double;
    function GetHydraulicConductivityAnnotation: string;
    function GetStreambedElevationAnnotation: string;
    function GetStreamBedThicknessAnnotation: string;
    function GetStreamDepth: double;
    function GetStreamDepthAnnotation: string;
    function GetStreamWidth: double;
    function GetStreamWidthAnnotation: string;
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
  public
    property Values: TSfrSegmentRecord read FValues write FValues;
    property StressPeriod: integer read FStressPeriod write FStressPeriod;
    property HydraulicConductivity: double read GetHydraulicConductivity;
    property StreambedElevation: double read GetStreambedElevation;
    property StreamBedThickness: double read GetStreamBedThickness;
    property StreamWidth: double read GetStreamWidth;
    property StreamDepth: double read GetStreamDepth;
    property HydraulicConductivityAnnotation: string read GetHydraulicConductivityAnnotation;
    property StreambedElevationAnnotation: string read GetStreambedElevationAnnotation;
    property StreamBedThicknessAnnotation: string read GetStreamBedThicknessAnnotation;
    property StreamWidthAnnotation: string read GetStreamWidthAnnotation;
    property StreamDepthAnnotation: string read GetStreamDepthAnnotation;
  end;

implementation

uses Contnrs, DataSetUnit, ScreenObjectUnit, ModflowTimeUnit, PhastModelUnit,
  ModflowSfrUnit, TempFiles, frmGoPhastUnit;

const
  HydraulicConductivityPosition = 0;
  StreamBedThicknessPosition = 1;
  StreambedElevationPosition = 2;
  StreamWidthPosition = 3;
  StreamDepthPosition = 4;

{ TSfrSegmentItem }

procedure TSfrSegmentItem.Assign(Source: TPersistent);
var
  Sfr: TSfrSegmentItem;
begin
  if Source is TSfrSegmentItem then
  begin
    Sfr := TSfrSegmentItem(Source);
    HydraulicConductivity := Sfr.HydraulicConductivity;
    StreamBedThickness := Sfr.StreamBedThickness;
    StreambedElevation := Sfr.StreambedElevation;
    StreamWidth := Sfr.StreamWidth;
    StreamDepth := Sfr.StreamDepth;
  end;
  inherited;
end;

procedure TSfrSegmentItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TSfrSegmentCollection;
  HydraulicConductivityObserver: TObserver;
  StreamBedThicknessObserver: TObserver;
  StreambedElevationObserver: TObserver;
  StreamWidthObserver: TObserver;
  StreamDepthObserver: TObserver;
begin
  ParentCollection := Collection as TSfrSegmentCollection;
  HydraulicConductivityObserver := FObserverList[HydraulicConductivityPosition];
  HydraulicConductivityObserver.OnUpToDateSet := ParentCollection.InvalidateHydraulicConductivityData;
  StreamBedThicknessObserver := FObserverList[StreamBedThicknessPosition];
  StreamBedThicknessObserver.OnUpToDateSet := ParentCollection.InvalidateStreamBedThicknessData;
  StreambedElevationObserver := FObserverList[StreambedElevationPosition];
  StreambedElevationObserver.OnUpToDateSet := ParentCollection.InvalidateStreambedElevationData;
  StreamWidthObserver := FObserverList[StreamWidthPosition];
  StreamWidthObserver.OnUpToDateSet := ParentCollection.InvalidateStreamWidthData;
  StreamDepthObserver := FObserverList[StreamDepthPosition];
  StreamDepthObserver.OnUpToDateSet := ParentCollection.InvalidateStreamDepthData;
end;

function TSfrSegmentItem.BoundaryFormulaCount: integer;
begin
  result := 5;
end;

constructor TSfrSegmentItem.Create(Collection: TCollection);
begin
  inherited;
  StreamBedThickness := '1';
  StreambedElevation := '0';
  HydraulicConductivity := 'Kx';
  StreamWidth := '10';
  StreamDepth := '1';
end;

procedure TSfrSegmentItem.CreateFormulaObjects;
begin
  FHydraulicConductivity := CreateFormulaObject(dso3D);
  FStreamBedThickness := CreateFormulaObject(dso3D);
  FStreambedElevation := CreateFormulaObject(dso3D);
  FStreamWidth := CreateFormulaObject(dso3D);
  FStreamDepth := CreateFormulaObject(dso3D);
end;

destructor TSfrSegmentItem.Destroy;
var
  Index: integer;
begin
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    BoundaryFormula[Index] := '0';
  end;
  inherited;
end;

function TSfrSegmentItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    HydraulicConductivityPosition:
      result := HydraulicConductivity;
    StreamBedThicknessPosition:
      result := StreamBedThickness;
    StreambedElevationPosition:
      result := StreambedElevation;
    StreamWidthPosition:
      result := StreamWidth;
    StreamDepthPosition:
      result := StreamDepth;
    else Assert(False);
  end;
end;

function TSfrSegmentItem.GetHydraulicConductivity: string;
begin
  Result := FHydraulicConductivity.Formula;
  ResetItemObserver(HydraulicConductivityPosition);
end;

procedure TSfrSegmentItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FHydraulicConductivity then
  begin
    List.Add(FObserverList[HydraulicConductivityPosition]);
  end;
  if Sender = FStreamBedThickness then
  begin
    List.Add(FObserverList[StreamBedThicknessPosition]);
  end;
  if Sender = FStreambedElevation then
  begin
    List.Add(FObserverList[StreambedElevationPosition]);
  end;
  if Sender = FStreamWidth then
  begin
    List.Add(FObserverList[StreamWidthPosition]);
  end;
  if Sender = FStreamDepth then
  begin
    List.Add(FObserverList[StreamDepthPosition]);
  end;
end;

function TSfrSegmentItem.GetStreambedElevation: string;
begin
  Result := FStreambedElevation.Formula;
  ResetItemObserver(StreambedElevationPosition);
end;

function TSfrSegmentItem.GetStreamBedThickness: string;
begin
  Result := FStreamBedThickness.Formula;
  ResetItemObserver(StreamBedThicknessPosition);
end;

function TSfrSegmentItem.GetStreamDepth: string;
begin
  Result := FStreamDepth.Formula;
  ResetItemObserver(StreamDepthPosition);
end;

function TSfrSegmentItem.GetStreamWidth: string;
begin
  Result := FStreamWidth.Formula;
  ResetItemObserver(StreamWidthPosition);
end;

procedure TSfrSegmentItem.InvalidateModel;
begin
  inherited;

end;

function TSfrSegmentItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TSfrSegmentItem;
begin
  result := (AnotherItem is TSfrSegmentItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TSfrSegmentItem(AnotherItem);
    result := (Item.HydraulicConductivity = HydraulicConductivity)
      and (Item.StreamBedThickness = StreamBedThickness)
      and (Item.StreambedElevation = StreambedElevation)
      and (Item.StreamWidth = StreamWidth)
      and (Item.StreamDepth = StreamDepth);
  end;
end;

procedure TSfrSegmentItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FStreamDepth,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStreamWidth,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStreambedElevation,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStreamBedThickness,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FHydraulicConductivity,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
end;

procedure TSfrSegmentItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    HydraulicConductivityPosition:
      HydraulicConductivity := Value;
    StreamBedThicknessPosition:
      StreamBedThickness := Value;
    StreambedElevationPosition:
      StreambedElevation := Value;
    StreamWidthPosition:
      StreamWidth := Value;
    StreamDepthPosition:
      StreamDepth := Value;
    else Assert(False);
  end;
end;


procedure TSfrSegmentItem.SetHydraulicConductivity(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FHydraulicConductivity.Formula <> Value then
  begin
    UpdateFormula(Value, HydraulicConductivityPosition, FHydraulicConductivity);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState) then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        if Collection = (ScreenObj as TScreenObject).
          ModflowSfrBoundary.UpstreamSegmentValues then
        begin
          PhastModel.InvalidateMfSfrUpstreamHydraulicConductivity(self);
        end
        else
        begin
          Assert(Collection = (ScreenObj as TScreenObject).
            ModflowSfrBoundary.DownstreamSegmentValues);
          PhastModel.InvalidateMfSfrDownstreamHydraulicConductivity(self);
        end;
      end;
    end;
  end;
end;

procedure TSfrSegmentItem.SetStreambedElevation(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FStreambedElevation.Formula <> Value then
  begin
    UpdateFormula(Value, StreambedElevationPosition, FStreambedElevation);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState) then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        if Collection = (ScreenObj as TScreenObject).
          ModflowSfrBoundary.UpstreamSegmentValues then
        begin
          PhastModel.InvalidateMfSfrUpstreamElevation(self);
        end
        else
        begin
          Assert(Collection = (ScreenObj as TScreenObject).
            ModflowSfrBoundary.DownstreamSegmentValues);
          PhastModel.InvalidateMfSfrDownstreamElevation(self);
        end;
      end;
    end;
  end;
end;

procedure TSfrSegmentItem.SetStreamBedThickness(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FStreamBedThickness.Formula <> Value then
  begin
    UpdateFormula(Value, StreamBedThicknessPosition, FStreamBedThickness);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState) then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        if Collection = (ScreenObj as TScreenObject).
          ModflowSfrBoundary.UpstreamSegmentValues then
        begin
          PhastModel.InvalidateMfSfrUpstreamThickness(self);
        end
        else
        begin
          Assert(Collection = (ScreenObj as TScreenObject).
            ModflowSfrBoundary.DownstreamSegmentValues);
          PhastModel.InvalidateMfSfrDownstreamThickness(self);
        end;
      end;
    end;
  end;
end;

procedure TSfrSegmentItem.SetStreamDepth(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FStreamDepth.Formula <> Value then
  begin
    UpdateFormula(Value, StreamDepthPosition, FStreamDepth);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState) then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        if Collection = (ScreenObj as TScreenObject).
          ModflowSfrBoundary.UpstreamSegmentValues then
        begin
          PhastModel.InvalidateMfSfrUpstreamDepth(self);
        end
        else
        begin
          Assert(Collection = (ScreenObj as TScreenObject).
            ModflowSfrBoundary.DownstreamSegmentValues);
          PhastModel.InvalidateMfSfrDownstreamDepth(self);
        end;
      end;
    end;
  end;
end;

procedure TSfrSegmentItem.SetStreamWidth(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FStreamWidth.Formula <> Value then
  begin
    UpdateFormula(Value, StreamWidthPosition, FStreamWidth);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState) then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        if Collection = (ScreenObj as TScreenObject).
          ModflowSfrBoundary.UpstreamSegmentValues then
        begin
          PhastModel.InvalidateMfSfrUpstreamWidth(self);
        end
        else
        begin
          Assert(Collection = (ScreenObj as TScreenObject).
            ModflowSfrBoundary.DownstreamSegmentValues);
          PhastModel.InvalidateMfSfrDownstreamWidth(self);
        end;
      end;
    end;
  end;
end;

{ TSfrSegmentCollection }

procedure TSfrSegmentCollection.AddSpecificBoundary;
begin
  AddBoundary(TSfrSegmentStorage.Create);
end;

procedure TSfrSegmentCollection.AssignCellValues(DataSets: TList;
  ItemIndex: Integer);
var
  HydraulicConductivityArray: TDataArray;
  StreamBedThicknessArray: TDataArray;
  StreambedElevationArray: TDataArray;
  StreamWidthArray: TDataArray;
  Boundary: TSfrSegmentStorage;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  BoundaryIndex: Integer;
  StreamDepthArray: TDataArray;
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
  HydraulicConductivityArray := DataSets[HydraulicConductivityPosition];
  StreamBedThicknessArray := DataSets[StreamBedThicknessPosition];
  StreambedElevationArray := DataSets[StreambedElevationPosition];
  StreamWidthArray := DataSets[StreamWidthPosition];
  StreamDepthArray := DataSets[StreamDepthPosition];
  Boundary := Boundaries[ItemIndex] as TSfrSegmentStorage;

  HydraulicConductivityArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
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
            if HydraulicConductivityArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              Assert(StreamBedThicknessArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(StreambedElevationArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(StreamWidthArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(StreamDepthArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(BoundaryIndex < Length(Boundary.SrfSegmentArray));
              with Boundary.SrfSegmentArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                HydraulicConductivity := HydraulicConductivityArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                HydraulicConductivityAnnotation := HydraulicConductivityArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                StreamBedThickness := StreamBedThicknessArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                StreamBedThicknessAnnotation := StreamBedThicknessArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                StreambedElevation := StreambedElevationArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                StreambedElevationAnnotation := StreambedElevationArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                StreamWidth := StreamWidthArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                StreamWidthAnnotation := StreamWidthArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                StreamDepth := StreamDepthArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                StreamDepthAnnotation := StreamDepthArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  HydraulicConductivityArray.CacheData;
  StreamBedThicknessArray.CacheData;
  StreambedElevationArray.CacheData;
  StreamWidthArray.CacheData;
  StreamDepthArray.CacheData;
  Boundary.CacheData;
end;


constructor TSfrSegmentCollection.Create(Boundary: TModflowBoundary; Model,
  ScreenObject: TObject);
begin
  inherited Create(Boundary, Model, ScreenObject);
  FHydraulicConductivityData := TModflowTimeList.Create(Model);
  FStreamBedThicknessData := TModflowTimeList.Create(Model);
  FStreamBedElevationData := TModflowTimeList.Create(Model);
  FStreamWidthData := TModflowTimeList.Create(Model);
  FStreamDepthData := TModflowTimeList.Create(Model);

  FStreamBedThicknessData.NonParamDescription := 'Streambed thickness';
  FStreamBedThicknessData.ParamDescription := ' streambed thickness';
  FStreamBedElevationData.NonParamDescription := 'Streambed elevation';
  FStreamBedElevationData.ParamDescription := ' streambed elevation';
  FHydraulicConductivityData.NonParamDescription := 'Hydraulic conductivity';
  FHydraulicConductivityData.ParamDescription := ' Hydraulic conductivity multiplier';
  FStreamWidthData.NonParamDescription := 'Stream width';
  FStreamWidthData.ParamDescription := ' stream width';

  FStreamDepthData.NonParamDescription := 'Stream depth';
  FStreamDepthData.ParamDescription := ' stream depth';

  if Model <> nil then
  begin
//    FStreamBedThicknessData.OnInvalidate := (Model as TPhastModel).InvalidateMfRivStage;
//    FStreamBedElevationData.OnInvalidate := (Model as TPhastModel).InvalidateMfRivConductance;
//    FHydraulicConductivityData.OnInvalidate := (Model as TPhastModel).InvalidateMfRivBottom;
//    FStreamWidthData.OnInvalidate := (Model as TPhastModel).InvalidateMfRivBottom;
  end;

  AddTimeList(FHydraulicConductivityData);
  AddTimeList(FStreamBedThicknessData);
  AddTimeList(FStreamBedElevationData);
  AddTimeList(FStreamWidthData);
  AddTimeList(FStreamDepthData);
end;

destructor TSfrSegmentCollection.Destroy;
begin
  FStreamBedThicknessData.Free;
  FStreamBedElevationData.Free;
  FHydraulicConductivityData.Free;
  FStreamWidthData.Free;
  FStreamDepthData.Free;
  inherited;
end;

procedure TSfrSegmentCollection.InitializeTimeLists(ListOfTimeLists: TList);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TSfrSegmentItem;
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
    Item := Items[Index] as TSfrSegmentItem;
    BoundaryValues[Index].Time := Item.StartTime;
    ItemUsed := ISFROPT  in [0,4,5];
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.HydraulicConductivity;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  FHydraulicConductivityData.Initialize(BoundaryValues, ScreenObject,
    AssignmentLocation);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSfrSegmentItem;
    BoundaryValues[Index].Time := Item.StartTime;
    ItemUsed := ISFROPT in [0,4,5];
    if ItemUsed then
    begin
      if ISFROPT in [4,5] then
      begin
        Assert(ScreenObject.ModflowSfrBoundary <> nil);
        ICALC := ScreenObject.ModflowSfrBoundary.ParamIcalc.ICalc(Item.StartTime);
        if ICALC in [1,2] then
        begin
//          ItemUsed := Index = 0;
        end
        else
        begin
          ItemUsed := True;
        end;
      end;
    end;
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.StreamBedThickness;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  FStreamBedThicknessData.Initialize(BoundaryValues, ScreenObject,
    AssignmentLocation);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSfrSegmentItem;
    BoundaryValues[Index].Time := Item.StartTime;
    ItemUsed := ISFROPT in [0,4,5];
    if ItemUsed then
    begin
      if ISFROPT in [4,5] then
      begin
        Assert(ScreenObject.ModflowSfrBoundary <> nil);
        ICALC := ScreenObject.ModflowSfrBoundary.ParamIcalc.ICalc(Item.StartTime);
        if ICALC in [1,2] then
        begin
          ItemUsed := Index = 0;
        end
        else
        begin
          ItemUsed := True;
        end;
      end;
    end;
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.StreamBedElevation;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  FStreamBedElevationData.Initialize(BoundaryValues, ScreenObject,
    AssignmentLocation);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSfrSegmentItem;
    BoundaryValues[Index].Time := Item.StartTime;
    Assert(ScreenObject.ModflowSfrBoundary <> nil);
    ICALC := ScreenObject.ModflowSfrBoundary.ParamIcalc.ICalc(Item.StartTime);
    ItemUsed := ICALC < 2;
    if ItemUsed then
    begin
      if (ISFROPT > 1) and (ICALC = 1) then
      begin
        ItemUsed := Index = 0;
      end;
    end;
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.StreamWidth;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  FStreamWidthData.Initialize(BoundaryValues, ScreenObject,
    AssignmentLocation);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSfrSegmentItem;
    BoundaryValues[Index].Time := Item.StartTime;
    Assert(ScreenObject.ModflowSfrBoundary <> nil);
    ICALC := ScreenObject.ModflowSfrBoundary.ParamIcalc.ICalc(Item.StartTime);
    ItemUsed := ICALC = 0;
    if ItemUsed then
    begin
      BoundaryValues[Index].Formula := Item.StreamDepth;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
  end;
  FStreamDepthData.Initialize(BoundaryValues, ScreenObject,
    AssignmentLocation);


  Assert(FHydraulicConductivityData.Count = Count);
  Assert(FStreamBedThicknessData.Count = Count);
  Assert(FStreamBedElevationData.Count = Count);
  Assert(FStreamWidthData.Count = Count);
  Assert(FStreamDepthData.Count = Count);
  ClearBoundaries;
  SetBoundaryCapacity(FHydraulicConductivityData.Count);
  for TimeIndex := 0 to FHydraulicConductivityData.Count - 1 do
  begin
    AddBoundary(TSfrSegmentStorage.Create);
  end;
  ListOfTimeLists.Add(FHydraulicConductivityData);
  ListOfTimeLists.Add(FStreamBedThicknessData);
  ListOfTimeLists.Add(FStreamBedElevationData);
  ListOfTimeLists.Add(FStreamWidthData);
  ListOfTimeLists.Add(FStreamDepthData);
end;

procedure TSfrSegmentCollection.InvalidateHydraulicConductivityData(
  Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FHydraulicConductivityData.Invalidate;
  end;
end;

procedure TSfrSegmentCollection.InvalidateStreambedElevationData(
  Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FStreamBedElevationData.Invalidate;
  end;
end;

procedure TSfrSegmentCollection.InvalidateStreamBedThicknessData(
  Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FStreamBedThicknessData.Invalidate;
  end;
end;

procedure TSfrSegmentCollection.InvalidateStreamDepthData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FStreamDepthData.Invalidate;
  end;
end;

procedure TSfrSegmentCollection.InvalidateStreamWidthData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FStreamWidthData.Invalidate;
  end;
end;

class function TSfrSegmentCollection.ItemClass: TMF_BoundItemClass;
begin
  result := TSfrSegmentItem;
end;

procedure TSfrSegmentCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer);
begin
  SetLength((Boundaries[ItemIndex] as TSfrSegmentStorage).FSrfSegmentArray,
    BoundaryCount);
  inherited;
end;

{ TSfrSegmentParamItem }

class function TSfrSegmentParamItem.BoundaryClass: TMF_BoundCollClass;
begin
  result := TSfrSegmentCollection;
end;

{ TSfrSegment_Cell }

procedure TSfrSegment_Cell.Cache(Comp: TCompressionStream);
begin
  inherited;
  Values.Cache(Comp);
  WriteCompInt(Comp, StressPeriod);
end;

function TSfrSegment_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TSfrSegment_Cell.GetHydraulicConductivity: double;
begin
  result := Values.HydraulicConductivity;
end;

function TSfrSegment_Cell.GetHydraulicConductivityAnnotation: string;
begin
  result := Values.HydraulicConductivityAnnotation;
end;

function TSfrSegment_Cell.GetIntegerAnnotation(Index: integer): string;
begin
  result := '';
  Assert(False);
end;

function TSfrSegment_Cell.GetIntegerValue(Index: integer): integer;
begin
  result := 0;
  Assert(False);
end;

function TSfrSegment_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TSfrSegment_Cell.GetRealAnnotation(Index: integer): string;
begin
  result := '';
  case Index of
    HydraulicConductivityPosition: result := HydraulicConductivityAnnotation;
    StreamBedThicknessPosition: result := StreamBedThicknessAnnotation;
    StreambedElevationPosition: result := StreambedElevationAnnotation;
    StreamWidthPosition: result := StreamWidthAnnotation;
    StreamDepthPosition: result := StreamDepthAnnotation;
    else Assert(False);
  end;
end;

function TSfrSegment_Cell.GetRealValue(Index: integer): double;
begin
  result := 0;
  case Index of
    HydraulicConductivityPosition: result := HydraulicConductivity;
    StreamBedThicknessPosition: result := StreamBedThickness;
    StreambedElevationPosition: result := StreambedElevation;
    StreamWidthPosition: result := StreamWidth;
    StreamDepthPosition: result := StreamDepth;
    else Assert(False);
  end;
end;

function TSfrSegment_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TSfrSegment_Cell.GetStreambedElevation: double;
begin
  result := Values.StreambedElevation;
end;

function TSfrSegment_Cell.GetStreambedElevationAnnotation: string;
begin
  result := Values.StreambedElevationAnnotation;
end;

function TSfrSegment_Cell.GetStreamBedThickness: double;
begin
  result := Values.StreamBedThickness;
end;

function TSfrSegment_Cell.GetStreamBedThicknessAnnotation: string;
begin
  result := Values.StreamBedThicknessAnnotation;
end;

function TSfrSegment_Cell.GetStreamDepth: double;
begin
  result := Values.StreamDepth;
end;

function TSfrSegment_Cell.GetStreamDepthAnnotation: string;
begin
  result := Values.StreamDepthAnnotation;
end;

function TSfrSegment_Cell.GetStreamWidth: double;
begin
  result := Values.StreamWidth;
end;

function TSfrSegment_Cell.GetStreamWidthAnnotation: string;
begin
  result := Values.StreamWidthAnnotation;
end;

procedure TSfrSegment_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

{ TSfrSegmentRecord }

procedure TSfrSegmentRecord.Cache(Comp: TCompressionStream);
begin
  WriteCompCell(Comp, Cell);

  WriteCompReal(Comp, StreambedElevation);
  WriteCompReal(Comp, StreamBedThickness);
  WriteCompReal(Comp, HydraulicConductivity);
  WriteCompReal(Comp, StreamWidth);
  WriteCompReal(Comp, StreamDepth);

  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);

  WriteCompString(Comp, HydraulicConductivityAnnotation);
  WriteCompString(Comp, StreambedElevationAnnotation);
  WriteCompString(Comp, StreamBedThicknessAnnotation);
  WriteCompString(Comp, StreamWidthAnnotation);
  WriteCompString(Comp, StreamDepthAnnotation);
end;

procedure TSfrSegmentRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  StreambedElevation := ReadCompReal(Decomp);
  StreamBedThickness := ReadCompReal(Decomp);
  HydraulicConductivity := ReadCompReal(Decomp);
  StreamWidth := ReadCompReal(Decomp);
  StreamDepth := ReadCompReal(Decomp);

  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);

  HydraulicConductivityAnnotation := ReadCompString(Decomp, Annotations);
  StreambedElevationAnnotation := ReadCompString(Decomp, Annotations);
  StreamBedThicknessAnnotation := ReadCompString(Decomp, Annotations);
  StreamWidthAnnotation := ReadCompString(Decomp, Annotations);
  StreamDepthAnnotation := ReadCompString(Decomp, Annotations);
end;

{ TSfrSegmentStorage }

procedure TSfrSegmentStorage.Clear;
begin
  SetLength(FSrfSegmentArray, 0);
  FCleared := True;
end;

procedure TSfrSegmentStorage.Store(Compressor: TCompressionStream);
var
  Count: Integer;
  Index: Integer;
begin
  Count := Length(FSrfSegmentArray);
  Compressor.Write(Count, SizeOf(Count));
  for Index := 0 to Count - 1 do
  begin
    FSrfSegmentArray[Index].Cache(Compressor);
  end;
end;

procedure TSfrSegmentStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Count: Integer;
  Index: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FSrfSegmentArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FSrfSegmentArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TSfrSegmentStorage.GetSrfSegmentArray: TSrfSegmentArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FSrfSegmentArray;
end;

end.
