unit ModflowSfrReachUnit;

interface

uses Windows, ZLib, SysUtils, Classes, RbwParser, OrderedCollectionUnit,
  ModflowCellUnit, ModflowBoundaryUnit, DataSetUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets;

type
  TSfrRecord = record
    Cell: TCellLocation;
    ReachLength: double;
    StreamSlope: double;
    StreambedElevation: double;
    StreamBedThickness: double;
    HydraulicConductivity: double;
    SaturatedWaterContent: double;
    InitialWaterContent: double;
    BrooksCoreyExponent: double;
    VerticalK: double;
    StartingTime: double;
    EndingTime: double;
    ReachLengthAnnotation: string;
    StreamSlopeAnnotation: string;
    HydraulicConductivityAnnotation: string;
    StreambedElevationAnnotation: string;
    StreamBedThicknessAnnotation: string;
    SaturatedWaterContentAnnotation: string;
    InitialWaterContentAnnotation: string;
    BrooksCoreyExponentAnnotation: string;
    VerticalKAnnotation: string;
    procedure Cache(Comp: TCompressionStream);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
  end;

  TSrfArray = array of TSfrRecord;

  TSfrStorage = class(TCustomBoundaryStorage)
  private
    FSfrArray: TSrfArray;
    function GetSfrArray: TSrfArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property SfrArray: TSrfArray read GetSfrArray;
  end;

  // @name represents a MODFLOW Streamflow Routing boundary for one time interval.
  // @name is stored by @link(TSfrCollection).
  TSfrItem = class(TCustomModflowBoundaryItem)
  private
    FStreamBedThickness: TFormulaObject;
    FStreambedElevation: TFormulaObject;
    FHydraulicConductivity: TFormulaObject;
    FStreamSlope: TFormulaObject;
    FBrooksCoreyExponent: TFormulaObject;
    FVerticalK: TFormulaObject;
    FInitialWaterContent: TFormulaObject;
    FSaturatedWaterContent: TFormulaObject;
    FReachLength: TFormulaObject;
    procedure SetHydraulicConductivity(const Value: string);
    procedure SetStreambedElevation(const Value: string);
    procedure SetStreamBedThickness(const Value: string);
    procedure SetStreamSlope(const Value: string);
    procedure SetBrooksCoreyExponent(const Value: string);
    procedure SetInitialWaterContent(const Value: string);
    procedure SetSaturatedWaterContent(const Value: string);
    procedure SetVerticalK(const Value: string);
    procedure SetReachLength(const Value: string);
    function ScreenObject: TObject;
    function GetBrooksCoreyExponent: string;
    function GetHydraulicConductivity: string;
    function GetInitialWaterContent: string;
    function GetReachLength: string;
    function GetSaturatedWaterContent: string;
    function GetStreambedElevation: string;
    function GetStreamBedThickness: string;
    function GetStreamSlope: string;
    function GetVerticalK: string;
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
    property ReachLength: string read GetReachLength write SetReachLength;
    property HydraulicConductivity: string read GetHydraulicConductivity
      write SetHydraulicConductivity;
    property StreamBedThickness: string read GetStreamBedThickness
      write SetStreamBedThickness;
    property StreambedElevation: string read GetStreambedElevation
      write SetStreambedElevation;
    property StreamSlope: string read GetStreamSlope write SetStreamSlope;
    property SaturatedWaterContent: string read GetSaturatedWaterContent
      write SetSaturatedWaterContent;
    property InitialWaterContent: string read GetInitialWaterContent
      write SetInitialWaterContent;
    property BrooksCoreyExponent: string read GetBrooksCoreyExponent
      write SetBrooksCoreyExponent;
    property VerticalK: string read GetVerticalK write SetVerticalK;
  end;

  // @name represents MODFLOW Streamflow Routing boundaries
  // for a series of time intervals.
  TSfrCollection = class(TCustomMF_ArrayBoundColl)
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
    FStreamSlopeData: TModflowTimeList;
    FSaturatedWaterContent: TModflowTimeList;
    FInitialWaterContent: TModflowTimeList;
    FBrooksCoreyExponent: TModflowTimeList;
    FVerticalK: TModflowTimeList;
    FReachLength: TModflowTimeList;
    procedure InvalidateReachLengthData(Sender: TObject);
    procedure InvalidateHydraulicConductivityData(Sender: TObject);
    procedure InvalidateBedThicknessData(Sender: TObject);
    procedure InvalidateStreambedElevationData(Sender: TObject);
    procedure InvalidateStreamSlopeData(Sender: TObject);
    procedure InvalidateSaturatedWaterContentData(Sender: TObject);
    procedure InvalidateInitialWaterContentData(Sender: TObject);
    procedure InvalidateBrooksCoreyExponentData(Sender: TObject);
    procedure InvalidateVerticalKData(Sender: TObject);
  protected
    procedure AddSpecificBoundary; override;
    procedure CountBoundaryCells(var BoundaryCount: Integer;
      DataArray1: TDataArray; DataSets: TList); override;
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
    // @name creates an instance of @classname
    constructor Create(Boundary: TModflowBoundary; Model,
      ScreenObject: TObject); override;
    // @name destroys the current instance of @classname.
    // Do not call @name; call Free instead.
    destructor Destroy; override;
  end;

  TSfr_Cell = class(TValueCell)
  private
    FValues: TSfrRecord;
    FStressPeriod: integer;
    function GetStreamBedThickness: double;
    function GetHydraulicConductivity: double;
    function GetStreambedElevation: double;
    function GetHydraulicConductivityAnnotation: string;
    function GetStreambedElevationAnnotation: string;
    function GetStreamBedThicknessAnnotation: string;
    function GetStreamSlope: double;
    function GetStreamSlopeAnnotation: string;
    function GetBrooksCoreyExponent: double;
    function GetBrooksCoreyExponentAnnotation: string;
    function GetInitialWaterContent: double;
    function GetInitialWaterContentAnnotation: string;
    function GetSaturatedWaterContent: double;
    function GetSaturatedWaterContentAnnotation: string;
    function GetVerticalK: double;
    function GetVerticalKAnnotation: string;
    function GetReachLength: double;
    function GetReachLengthAnnotation: string;
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
    property Values: TSfrRecord read FValues write FValues;
    property StressPeriod: integer read FStressPeriod write FStressPeriod;
    property HydraulicConductivity: double read GetHydraulicConductivity;
    property StreambedElevation: double read GetStreambedElevation;
    property StreamBedThickness: double read GetStreamBedThickness;
    property StreamSlope: double read GetStreamSlope;
    property HydraulicConductivityAnnotation: string read GetHydraulicConductivityAnnotation;
    property StreambedElevationAnnotation: string read GetStreambedElevationAnnotation;
    property StreamBedThicknessAnnotation: string read GetStreamBedThicknessAnnotation;
    property StreamSlopeAnnotation: string read GetStreamSlopeAnnotation;

    property SaturatedWaterContent: double read GetSaturatedWaterContent;
    property InitialWaterContent: double read GetInitialWaterContent;
    property BrooksCoreyExponent: double read GetBrooksCoreyExponent;
    property VerticalK: double read GetVerticalK;
    property ReachLength: double read GetReachLength;

    property ReachLengthAnnotation: string read GetReachLengthAnnotation;
    property SaturatedWaterContentAnnotation: string read GetSaturatedWaterContentAnnotation;
    property InitialWaterContentAnnotation: string read GetInitialWaterContentAnnotation;
    property BrooksCoreyExponentAnnotation: string read GetBrooksCoreyExponentAnnotation;
    property VerticalKAnnotation: string read GetVerticalKAnnotation;
  end;

implementation

uses Contnrs, ScreenObjectUnit, ModflowTimeUnit, PhastModelUnit,
  ModflowSfrUnit, TempFiles, GoPhastTypes, frmGoPhastUnit;

const
  ReachLengthPosition = 0;
  HydraulicConductivityPosition = 1;
  StreamBedThicknessPosition = 2;
  StreambedElevationPosition = 3;
  StreamSlopePosition = 4;
  SaturatedWaterContentPosition = 5;
  InitialWaterContentPosition = 6;
  BrooksCoreyExponentPosition = 7;
  VerticalKPosition = 8;

{ TSfrItem }

procedure TSfrItem.Assign(Source: TPersistent);
var
  Sfr: TSfrItem;
begin
  if Source is TSfrItem then
  begin
    Sfr := TSfrItem(Source);
    ReachLength := Sfr.ReachLength;
    HydraulicConductivity := Sfr.HydraulicConductivity;
    StreamBedThickness := Sfr.StreamBedThickness;
    StreambedElevation := Sfr.StreambedElevation;
    StreamSlope := Sfr.StreamSlope;
    SaturatedWaterContent := Sfr.SaturatedWaterContent;
    InitialWaterContent := Sfr.InitialWaterContent;
    BrooksCoreyExponent := Sfr.BrooksCoreyExponent;
    VerticalK := Sfr.VerticalK;
  end;
  inherited;
end;

procedure TSfrItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TSfrCollection;
  ReachLengthObserver: TObserver;
  HydraulicConductivityObserver: TObserver;
  BedThicknessObserver: TObserver;
  StreambedElevationObserver: TObserver;
  StreamSlopeObserver: TObserver;
  SaturatedWaterContentObserver: TObserver;
  InitialWaterContentObserver: TObserver;
  BrooksCoreyExponentObserver: TObserver;
  VerticalKObserver: TObserver;
begin
  ParentCollection := Collection as TSfrCollection;
  ReachLengthObserver := FObserverList[ReachLengthPosition];
  ReachLengthObserver.OnUpToDateSet := ParentCollection.InvalidateReachLengthData;
  HydraulicConductivityObserver := FObserverList[HydraulicConductivityPosition];
  HydraulicConductivityObserver.OnUpToDateSet := ParentCollection.InvalidateHydraulicConductivityData;
  BedThicknessObserver := FObserverList[StreamBedThicknessPosition];
  BedThicknessObserver.OnUpToDateSet := ParentCollection.InvalidateBedThicknessData;
  StreambedElevationObserver  := FObserverList[StreambedElevationPosition];
  StreambedElevationObserver.OnUpToDateSet := ParentCollection.InvalidateStreambedElevationData;
  StreamSlopeObserver := FObserverList[StreamSlopePosition];
  StreamSlopeObserver.OnUpToDateSet := ParentCollection.InvalidateStreamSlopeData;
  SaturatedWaterContentObserver := FObserverList[SaturatedWaterContentPosition];
  SaturatedWaterContentObserver.OnUpToDateSet := ParentCollection.InvalidateSaturatedWaterContentData;
  InitialWaterContentObserver := FObserverList[InitialWaterContentPosition];
  InitialWaterContentObserver.OnUpToDateSet := ParentCollection.InvalidateInitialWaterContentData;
  BrooksCoreyExponentObserver := FObserverList[BrooksCoreyExponentPosition];
  BrooksCoreyExponentObserver.OnUpToDateSet := ParentCollection.InvalidateBrooksCoreyExponentData;
  VerticalKObserver := FObserverList[VerticalKPosition];
  VerticalKObserver.OnUpToDateSet := ParentCollection.InvalidateVerticalKData;
end;

function TSfrItem.BoundaryFormulaCount: integer;
begin
  result := 9;
end;

constructor TSfrItem.Create(Collection: TCollection);
begin
  inherited;
  ReachLength := StrObjectIntersectLength;
  StreambedElevation := '0';
  HydraulicConductivity := '0';
  StreamSlope := '0';
  BrooksCoreyExponent := '0';
  VerticalK := '0';
  InitialWaterContent := '0';
  SaturatedWaterContent := '0';
end;

procedure TSfrItem.CreateFormulaObjects;
begin
  FReachLength := CreateFormulaObject(dso3D);
  FHydraulicConductivity := CreateFormulaObject(dso3D);
  FStreamBedThickness := CreateFormulaObject(dso3D);
  FStreambedElevation := CreateFormulaObject(dso3D);
  FStreamSlope := CreateFormulaObject(dso3D);
  FSaturatedWaterContent := CreateFormulaObject(dso3D);
  FInitialWaterContent := CreateFormulaObject(dso3D);
  FBrooksCoreyExponent := CreateFormulaObject(dso3D);
  FVerticalK := CreateFormulaObject(dso3D);
end;

destructor TSfrItem.Destroy;
var
  Index: integer;
begin
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    BoundaryFormula[Index] := '0';
  end;
  inherited;
end;

function TSfrItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    0:
      result := ReachLength;
    1:
      result := HydraulicConductivity;
    2:
      result := StreamBedThickness;
    3:
      result := StreambedElevation;
    4:
      result := StreamSlope;
    5:
      result := SaturatedWaterContent;
    6:
      result := InitialWaterContent;
    7:
      result := BrooksCoreyExponent;
    8:
      result := VerticalK;
    else Assert(False);
  end;
end;

function TSfrItem.GetBrooksCoreyExponent: string;
begin
  Result := FBrooksCoreyExponent.Formula;
  ResetItemObserver(BrooksCoreyExponentPosition);
end;

function TSfrItem.GetHydraulicConductivity: string;
begin
  Result := FHydraulicConductivity.Formula;
  ResetItemObserver(HydraulicConductivityPosition);
end;

function TSfrItem.GetInitialWaterContent: string;
begin
  Result := FInitialWaterContent.Formula;
  ResetItemObserver(InitialWaterContentPosition);
end;

procedure TSfrItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FReachLength then
  begin
    List.Add(FObserverList[ReachLengthPosition]);
  end;
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
  if Sender = FStreamSlope then
  begin
    List.Add(FObserverList[StreamSlopePosition]);
  end;
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
  if Sender = FVerticalK then
  begin
    List.Add(FObserverList[VerticalKPosition]);
  end;
end;

function TSfrItem.GetReachLength: string;
begin
  Result := FReachLength.Formula;
  ResetItemObserver(ReachLengthPosition);
end;

function TSfrItem.GetSaturatedWaterContent: string;
begin
  Result := FSaturatedWaterContent.Formula;
  ResetItemObserver(SaturatedWaterContentPosition);
end;

function TSfrItem.GetStreambedElevation: string;
begin
  Result := FStreambedElevation.Formula;
  ResetItemObserver(StreambedElevationPosition);
end;

function TSfrItem.GetStreamBedThickness: string;
begin
  Result := FStreamBedThickness.Formula;
  ResetItemObserver(StreamBedThicknessPosition);
end;

function TSfrItem.GetStreamSlope: string;
begin
  Result := FStreamSlope.Formula;
  ResetItemObserver(StreamSlopePosition);
end;

function TSfrItem.GetVerticalK: string;
begin
  Result := FVerticalK.Formula;
  ResetItemObserver(VerticalKPosition);
end;

procedure TSfrItem.InvalidateModel;
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState) then
  begin
    ScreenObj := ScreenObject;
    if (ScreenObj <> nil)
      and (ScreenObj as TScreenObject).CanInvalidateModel then
    begin
      PhastModel.InvalidateMfSfrSegmentReachAndIcalc(self);
    end;
  end;
end;

function TSfrItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TSfrItem;
begin
  result := (AnotherItem is TSfrItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TSfrItem(AnotherItem);
    result :=
      (Item.ReachLength = ReachLength)
      and (Item.HydraulicConductivity = HydraulicConductivity)
      and (Item.StreamBedThickness = StreamBedThickness)
      and (Item.StreambedElevation = StreambedElevation)
      and (Item.StreamSlope = StreamSlope)
      and (Item.SaturatedWaterContent = SaturatedWaterContent)
      and (Item.InitialWaterContent = InitialWaterContent)
      and (Item.BrooksCoreyExponent = BrooksCoreyExponent)
      and (Item.VerticalK = VerticalK);
  end;
end;

procedure TSfrItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FVerticalK,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FBrooksCoreyExponent,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FInitialWaterContent,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FSaturatedWaterContent,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStreamSlope,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStreambedElevation,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStreamBedThickness,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FHydraulicConductivity,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FReachLength,
    GlobalRemoveModflowBoundarySubscription, GlobalRestoreModflowBoundarySubscription, self);
end;

function TSfrItem.ScreenObject: TObject;
var
  SfrCollection: TSfrCollection;
begin
  SfrCollection := Collection as TSfrCollection;
  Assert(SfrCollection <> nil);
  result := SfrCollection.ScreenObject;
end;

procedure TSfrItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    0:
      ReachLength := Value;
    1:
      HydraulicConductivity := Value;
    2:
      StreamBedThickness := Value;
    3:
      StreambedElevation := Value;
    4:
      StreamSlope := Value;
    5:
      SaturatedWaterContent := Value;
    6:
      InitialWaterContent := Value;
    7:
      BrooksCoreyExponent := Value;
    8:
      VerticalK := Value;
    else Assert(False);
  end;
end;

procedure TSfrItem.SetBrooksCoreyExponent(const Value: string);
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
        PhastModel.InvalidateMfSfrBrooksCorey(self);
      end;
    end;
  end;
end;

procedure TSfrItem.SetHydraulicConductivity(const Value: string);
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
        PhastModel.InvalidateMfSfrStreamK(self);
      end;
    end;
  end;
end;

procedure TSfrItem.SetInitialWaterContent(const Value: string);
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
        PhastModel.InvalidateMfSfrInitialWaterContent(self);
      end;
    end;
  end;
end;

procedure TSfrItem.SetReachLength(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FReachLength.Formula <> Value then
  begin
    UpdateFormula(Value, ReachLengthPosition, FReachLength);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState) then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateMfSfrReachLength(self);
      end;
    end;
  end;
end;

procedure TSfrItem.SetSaturatedWaterContent(const Value: string);
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
        PhastModel.InvalidateMfSfrSaturatedWaterContent(self);
      end;
    end;
  end;
end;

procedure TSfrItem.SetStreambedElevation(const Value: string);
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
        PhastModel.InvalidateMfSfrStreamTop(self);
      end;
    end;
  end;
end;

procedure TSfrItem.SetStreamBedThickness(const Value: string);
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
        PhastModel.InvalidateMfSfrStreamThickness(self);
      end;
    end;
  end;
end;

procedure TSfrItem.SetStreamSlope(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FStreamSlope.Formula <> Value then
  begin
    UpdateFormula(Value, StreamSlopePosition, FStreamSlope);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState) then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateMfSfrStreamSlope(self);
      end;
    end;
  end;
end;

procedure TSfrItem.SetVerticalK(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FVerticalK.Formula <> Value then
  begin
    UpdateFormula(Value, VerticalKPosition, FVerticalK);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState) then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateMfSfrVerticalUnsatK(self);
      end;
    end;
  end;
end;

{ TSfrCollection }

procedure TSfrCollection.AddSpecificBoundary;
begin
  AddBoundary(TSfrStorage.Create);
end;

procedure TSfrCollection.AssignCellValues(DataSets: TList;
  ItemIndex: Integer);
var
  ReachLengthArray: TDataArray;
  HydraulicConductivityArray: TDataArray;
  StreamBedThicknessArray: TDataArray;
  StreambedElevationArray: TDataArray;
  StreamSlopeArray: TDataArray;
  Boundary: TSfrStorage;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  BoundaryIndex: Integer;
  SaturatedWaterContentArray: TDataArray;
  InitialWaterContentArray: TDataArray;
  BrooksCoreyExponentArray: TDataArray;
  VerticalKArray: TDataArray;
  ISFROPT: integer;
  LocalScreenObject: TScreenObject;
  SegmentIndex: Integer;
  Segment: TCellElementSegment;
  PriorCol, PriorRow, PriorLayer: integer;
  LocalModel: TPhastModel;
begin
  LocalModel := Model as TPhastModel;
  PriorCol := -1;
  PriorRow := -1;
  PriorLayer := -1;
  ISFROPT := (Model as TPhastModel).ModflowPackages.SfrPackage.Isfropt;
  ReachLengthArray := DataSets[0];
  if ISFROPT in [1,2,3] then
  begin
    HydraulicConductivityArray := DataSets[1];
    StreamBedThicknessArray := DataSets[2];
    StreambedElevationArray := DataSets[3];
    StreamSlopeArray := DataSets[4];
    if ISFROPT in [2,3] then
    begin
      SaturatedWaterContentArray := DataSets[5];
      InitialWaterContentArray := DataSets[6];
      BrooksCoreyExponentArray := DataSets[7];
      if ISFROPT = 3 then
      begin
        VerticalKArray := DataSets[8];
      end
      else
      begin
        VerticalKArray := nil;
      end;
    end
    else
    begin
      SaturatedWaterContentArray := nil;
      InitialWaterContentArray := nil;
      BrooksCoreyExponentArray := nil;
      VerticalKArray := nil;
    end;
  end
  else
  begin
    HydraulicConductivityArray := nil;
    StreamBedThicknessArray := nil;
    StreambedElevationArray := nil;
    StreamSlopeArray := nil;
    SaturatedWaterContentArray := nil;
    InitialWaterContentArray := nil;
    BrooksCoreyExponentArray := nil;
    VerticalKArray := nil;
  end;

//  LastBoundaryIndex := -1;
  BoundaryIndex := -1;
  Boundary := Boundaries[ItemIndex] as TSfrStorage;

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
    if not ReachLengthArray.IsValue[LayerIndex, RowIndex, ColIndex] then
    begin
      Continue;
    end;
    if (ColIndex = PriorCol)
      and (RowIndex = PriorRow)
      and (LayerIndex = PriorLayer) then
    begin
      Continue
    end;
    Inc(BoundaryIndex);
    PriorCol := Segment.Col;
    PriorRow := Segment.Row;
    PriorLayer := Segment.Layer;

    Assert(BoundaryIndex < Length(Boundary.SfrArray));
    if ISFROPT in [1,2,3] then
    begin
      if ReachLengthArray.IsValue[LayerIndex, RowIndex, ColIndex] then
      begin
        Assert(HydraulicConductivityArray.IsValue[LayerIndex, RowIndex, ColIndex]);
        Assert(StreamBedThicknessArray.IsValue[LayerIndex, RowIndex, ColIndex]);
        Assert(StreambedElevationArray.IsValue[LayerIndex, RowIndex, ColIndex]);
        Assert(StreamSlopeArray.IsValue[LayerIndex, RowIndex, ColIndex]);
        if ISFROPT in [2,3] then
        begin
          Assert(SaturatedWaterContentArray.IsValue[LayerIndex, RowIndex, ColIndex]);
          Assert(InitialWaterContentArray.IsValue[LayerIndex, RowIndex, ColIndex]);
          Assert(BrooksCoreyExponentArray.IsValue[LayerIndex, RowIndex, ColIndex]);
        end;
        if ISFROPT = 3 then
        begin
          Assert(VerticalKArray.IsValue[LayerIndex, RowIndex, ColIndex]);
        end;
      end;
    end;
    if ReachLengthArray.IsValue[LayerIndex, RowIndex, ColIndex] then
    begin
//      Boundary := Boundaries[ItemIndex] as TSfrStorage;
      Assert(BoundaryIndex < Length(Boundary.SfrArray));
      with Boundary.SfrArray[BoundaryIndex] do
      begin
        Cell.Layer := LayerIndex;
        Cell.Row := RowIndex;
        Cell.Column := ColIndex;
//        Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
        ReachLength := ReachLengthArray.
          RealData[LayerIndex, RowIndex, ColIndex];
        ReachLengthAnnotation := ReachLengthArray.
          Annotation[LayerIndex, RowIndex, ColIndex];
        if ISFROPT in [1,2,3] then
        begin
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
          StreamSlope := StreamSlopeArray.
            RealData[LayerIndex, RowIndex, ColIndex];
          StreamSlopeAnnotation := StreamSlopeArray.
            Annotation[LayerIndex, RowIndex, ColIndex];
        end;

        if ISFROPT in [2,3] then
        begin
          SaturatedWaterContent := SaturatedWaterContentArray.
            RealData[LayerIndex, RowIndex, ColIndex];
          SaturatedWaterContentAnnotation := SaturatedWaterContentArray.
            Annotation[LayerIndex, RowIndex, ColIndex];
          InitialWaterContent := InitialWaterContentArray.
            RealData[LayerIndex, RowIndex, ColIndex];
          InitialWaterContentAnnotation := InitialWaterContentArray.
            Annotation[LayerIndex, RowIndex, ColIndex];

          BrooksCoreyExponent := BrooksCoreyExponentArray.
            RealData[LayerIndex, RowIndex, ColIndex];
          BrooksCoreyExponentAnnotation := BrooksCoreyExponentArray.
            Annotation[LayerIndex, RowIndex, ColIndex];
        end;
        if ISFROPT = 3 then
        begin
          VerticalK := VerticalKArray.
            RealData[LayerIndex, RowIndex, ColIndex];
          VerticalKAnnotation := VerticalKArray.
            Annotation[LayerIndex, RowIndex, ColIndex];
        end;
      end;
    end;
  end;
  ReachLengthArray.CacheData;
  if HydraulicConductivityArray <> nil then
  begin
    HydraulicConductivityArray.CacheData;
  end;
  if StreamBedThicknessArray <> nil then
  begin
    StreamBedThicknessArray.CacheData;
  end;
  if StreambedElevationArray <> nil then
  begin
    StreambedElevationArray.CacheData;
  end;
  if StreamSlopeArray <> nil then
  begin
    StreamSlopeArray.CacheData;
  end;
  if SaturatedWaterContentArray <> nil then
  begin
    SaturatedWaterContentArray.CacheData;
  end;
  if InitialWaterContentArray <> nil then
  begin
    InitialWaterContentArray.CacheData;
  end;
  if BrooksCoreyExponentArray <> nil then
  begin
    BrooksCoreyExponentArray.CacheData;
  end;
  if VerticalKArray <> nil then
  begin
    VerticalKArray.CacheData;
  end;
  Boundary.CacheData;
end;

procedure TSfrCollection.CountBoundaryCells(var BoundaryCount: Integer;
  DataArray1: TDataArray; DataSets: TList);
var
  DSIndex: Integer;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  DataArray2: TDataArray;
  SO: TScreenObject;
  Index: Integer;
  Segment: TCellElementSegment;
  PriorCol, PriorRow, PriorLayer: integer;
  LocalModel: TPhastModel;
begin
  LocalModel := Model as TPhastModel;
  BoundaryCount := 0;
  PriorCol := -1;
  PriorRow := -1;
  PriorLayer := -1;

  SO := ScreenObject as TScreenObject;
  for Index := 0 to SO.Segments.Count - 1 do
  begin
    Segment := SO.Segments[Index];
    LayerIndex := Segment.Layer;
    if not LocalModel.LayerStructure.IsLayerSimulated(LayerIndex) then
    begin
      Continue;
    end;
    RowIndex := Segment.Row;
    ColIndex := Segment.Col;
    if DataArray1.IsValue[LayerIndex, RowIndex, ColIndex]
      and ((LayerIndex <> PriorLayer)
      or (RowIndex <> PriorRow)
      or (ColIndex <> PriorCol)) then
    begin
      for DSIndex := 1 to DataSets.Count - 1 do
      begin
        DataArray2 := DataSets[DSIndex];
        Assert(DataArray2.IsValue[LayerIndex, RowIndex, ColIndex]);
      end;
      Inc(BoundaryCount);
      PriorLayer := Segment.Layer;
      PriorRow := Segment.Row;
      PriorCol := Segment.Col;
    end;
  end;
end;

constructor TSfrCollection.Create(Boundary: TModflowBoundary; Model,
  ScreenObject: TObject);
begin
  inherited Create(Boundary, Model, ScreenObject);
  FReachLength := TModflowTimeList.Create(Model);
  FHydraulicConductivityData := TModflowTimeList.Create(Model);
  FStreamBedThicknessData := TModflowTimeList.Create(Model);
  FStreamBedElevationData := TModflowTimeList.Create(Model);
  FStreamSlopeData := TModflowTimeList.Create(Model);
  FSaturatedWaterContent := TModflowTimeList.Create(Model);
  FInitialWaterContent := TModflowTimeList.Create(Model);
  FBrooksCoreyExponent := TModflowTimeList.Create(Model);
  FVerticalK := TModflowTimeList.Create(Model);

  FReachLength.NonParamDescription := 'Reach length';
  FReachLength.ParamDescription := ' reach length';

  FStreamBedThicknessData.NonParamDescription := 'Streambed thickness';
  FStreamBedThicknessData.ParamDescription := ' streambed thickness';
  FStreamBedElevationData.NonParamDescription := 'Streambed elevation';
  FStreamBedElevationData.ParamDescription := ' streambed elevation';
  FHydraulicConductivityData.NonParamDescription := 'Hydraulic conductivity';
  FHydraulicConductivityData.ParamDescription := ' Hydraulic conductivity multiplier';
  FStreamSlopeData.NonParamDescription := 'Slope';
  FStreamSlopeData.ParamDescription := ' slope';

  FSaturatedWaterContent.NonParamDescription := 'Saturated water content';
  FSaturatedWaterContent.ParamDescription := ' saturated water content';
  FInitialWaterContent.NonParamDescription := 'Initial water content';
  FInitialWaterContent.ParamDescription := ' initial water content';
  FBrooksCoreyExponent.NonParamDescription := 'Brooks-Corey exponent';
  FBrooksCoreyExponent.ParamDescription := ' Brooks-Corey exponent';
  FVerticalK.NonParamDescription := 'Maximum vertical unsaturated hydraulic conductivity';
  FVerticalK.ParamDescription := ' maximum vertical unsaturated hydraulic conductivity';

  if Model <> nil then
  begin
//    FStreamBedThicknessData.OnInvalidate := (Model as TPhastModel).InvalidateMfRivStage;
//    FStreamBedElevationData.OnInvalidate := (Model as TPhastModel).InvalidateMfRivConductance;
//    FHydraulicConductivityData.OnInvalidate := (Model as TPhastModel).InvalidateMfRivBottom;
//    FStreamSlopeData.OnInvalidate := (Model as TPhastModel).InvalidateMfRivBottom;
  end;


  AddTimeList(FReachLength);
  AddTimeList(FHydraulicConductivityData);
  AddTimeList(FStreamBedThicknessData);
  AddTimeList(FStreamBedElevationData);
  AddTimeList(FStreamSlopeData);
  AddTimeList(FSaturatedWaterContent);
  AddTimeList(FInitialWaterContent);
  AddTimeList(FBrooksCoreyExponent);
  AddTimeList(FVerticalK);
end;

destructor TSfrCollection.Destroy;
begin
  FReachLength.Free;
  FHydraulicConductivityData.Free;
  FStreamBedThicknessData.Free;
  FStreamBedElevationData.Free;
  FStreamSlopeData.Free;
  FSaturatedWaterContent.Free;
  FInitialWaterContent.Free;
  FBrooksCoreyExponent.Free;
  FVerticalK.Free;
  inherited;
end;

procedure TSfrCollection.InitializeTimeLists(ListOfTimeLists: TList);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TSfrItem;
  Boundary: TSfrBoundary;
  ScreenObject: TScreenObject;
  ISFROPT: integer;
begin
  ISFROPT := (Model as TPhastModel).ModflowPackages.SfrPackage.Isfropt;

  Assert(Count = 1);
  SetLength(BoundaryValues, Count);

  Boundary := BoundaryGroup as TSfrBoundary;
  ScreenObject := Boundary.ScreenObject as TScreenObject;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSfrItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.ReachLength;
  end;
  FReachLength.Initialize(BoundaryValues, ScreenObject);
  Assert(FReachLength.Count = Count);

  if ISFROPT in [1,2,3] then
  begin
    for Index := 0 to Count - 1 do
    begin
      Item := Items[Index] as TSfrItem;
      BoundaryValues[Index].Time := Item.StartTime;
      BoundaryValues[Index].Formula := Item.HydraulicConductivity;
    end;
    FHydraulicConductivityData.Initialize(BoundaryValues, ScreenObject);
    Assert(FHydraulicConductivityData.Count = Count);

    for Index := 0 to Count - 1 do
    begin
      Item := Items[Index] as TSfrItem;
      BoundaryValues[Index].Time := Item.StartTime;
      BoundaryValues[Index].Formula := Item.StreamBedThickness;
    end;
    FStreamBedThicknessData.Initialize(BoundaryValues, ScreenObject);
    Assert(FStreamBedThicknessData.Count = Count);

    for Index := 0 to Count - 1 do
    begin
      Item := Items[Index] as TSfrItem;
      BoundaryValues[Index].Time := Item.StartTime;
      BoundaryValues[Index].Formula := Item.StreamBedElevation;
    end;
    FStreamBedElevationData.Initialize(BoundaryValues, ScreenObject);
    Assert(FStreamBedElevationData.Count = Count);

    for Index := 0 to Count - 1 do
    begin
      Item := Items[Index] as TSfrItem;
      BoundaryValues[Index].Time := Item.StartTime;
      BoundaryValues[Index].Formula := Item.StreamSlope;
    end;
    FStreamSlopeData.Initialize(BoundaryValues, ScreenObject);
    Assert(FStreamSlopeData.Count = Count);

    if ISFROPT in [2,3] then
    begin
      for Index := 0 to Count - 1 do
      begin
        Item := Items[Index] as TSfrItem;
        BoundaryValues[Index].Time := Item.StartTime;
        BoundaryValues[Index].Formula := Item.SaturatedWaterContent;
      end;
      FSaturatedWaterContent.Initialize(BoundaryValues, ScreenObject);
      Assert(FSaturatedWaterContent.Count = Count);

      for Index := 0 to Count - 1 do
      begin
        Item := Items[Index] as TSfrItem;
        BoundaryValues[Index].Time := Item.StartTime;
        BoundaryValues[Index].Formula := Item.InitialWaterContent;
      end;
      FInitialWaterContent.Initialize(BoundaryValues, ScreenObject);
      Assert(FInitialWaterContent.Count = Count);

      for Index := 0 to Count - 1 do
      begin
        Item := Items[Index] as TSfrItem;
        BoundaryValues[Index].Time := Item.StartTime;
        BoundaryValues[Index].Formula := Item.BrooksCoreyExponent;
      end;
      FBrooksCoreyExponent.Initialize(BoundaryValues, ScreenObject);
      Assert(FBrooksCoreyExponent.Count = Count);

      if ISFROPT = 3 then
      begin
        for Index := 0 to Count - 1 do
        begin
          Item := Items[Index] as TSfrItem;
          BoundaryValues[Index].Time := Item.StartTime;
          BoundaryValues[Index].Formula := Item.VerticalK;
        end;
        FVerticalK.Initialize(BoundaryValues, ScreenObject);
        Assert(FVerticalK.Count = Count);
      end;
    end;
  end;


  ClearBoundaries;
  SetBoundaryCapacity(FReachLength.Count);
  for TimeIndex := 0 to FReachLength.Count - 1 do
  begin
    AddBoundary(TSfrStorage.Create);
  end;
  ListOfTimeLists.Add(FReachLength);
  if ISFROPT in [1,2,3] then
  begin
    ListOfTimeLists.Add(FHydraulicConductivityData);
    ListOfTimeLists.Add(FStreamBedThicknessData);
    ListOfTimeLists.Add(FStreamBedElevationData);
    ListOfTimeLists.Add(FStreamSlopeData);
    if ISFROPT in [2,3] then
    begin
      ListOfTimeLists.Add(FSaturatedWaterContent);
      ListOfTimeLists.Add(FInitialWaterContent);
      ListOfTimeLists.Add(FBrooksCoreyExponent);
      if ISFROPT = 3 then
      begin
        ListOfTimeLists.Add(FVerticalK);
      end;
    end;
  end;
end;

procedure TSfrCollection.InvalidateBedThicknessData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FStreamBedThicknessData.Invalidate;
  end;
end;

procedure TSfrCollection.InvalidateBrooksCoreyExponentData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FBrooksCoreyExponent.Invalidate;
  end;
end;

procedure TSfrCollection.InvalidateHydraulicConductivityData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FHydraulicConductivityData.Invalidate;
  end;
end;

procedure TSfrCollection.InvalidateInitialWaterContentData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FInitialWaterContent.Invalidate;
  end;
end;

procedure TSfrCollection.InvalidateReachLengthData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FReachLength.Invalidate;
  end;
end;

procedure TSfrCollection.InvalidateSaturatedWaterContentData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FSaturatedWaterContent.Invalidate;
  end;
end;

procedure TSfrCollection.InvalidateStreambedElevationData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FStreamBedElevationData.Invalidate;
  end;
end;

procedure TSfrCollection.InvalidateStreamSlopeData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FStreamSlopeData.Invalidate;
  end;
end;

procedure TSfrCollection.InvalidateVerticalKData(Sender: TObject);
begin
  if not (Sender as TObserver).UpToDate then
  begin
    FVerticalK.Invalidate;
  end;
end;

class function TSfrCollection.ItemClass: TMF_BoundItemClass;
begin
  result := TSfrItem;
end;

procedure TSfrCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer);
begin
  SetLength((Boundaries[ItemIndex] as TSfrStorage).FSfrArray, BoundaryCount);
  inherited;
end;

{ TSfr_Cell }

procedure TSfr_Cell.Cache(Comp: TCompressionStream);
begin
  inherited;
  Values.Cache(Comp);
  WriteCompInt(Comp, StressPeriod);
end;

function TSfr_Cell.GetBrooksCoreyExponent: double;
begin
  result := Values.BrooksCoreyExponent;
end;

function TSfr_Cell.GetBrooksCoreyExponentAnnotation: string;
begin
  result := Values.BrooksCoreyExponentAnnotation;
end;

function TSfr_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TSfr_Cell.GetHydraulicConductivity: double;
begin
  result := Values.HydraulicConductivity;
end;

function TSfr_Cell.GetHydraulicConductivityAnnotation: string;
begin
  result := Values.HydraulicConductivityAnnotation;
end;

function TSfr_Cell.GetInitialWaterContent: double;
begin
  result := Values.InitialWaterContent;
end;

function TSfr_Cell.GetInitialWaterContentAnnotation: string;
begin
  result := Values.InitialWaterContentAnnotation;
end;

function TSfr_Cell.GetIntegerAnnotation(Index: integer): string;
begin
  result := '';
  Assert(False);
end;

function TSfr_Cell.GetIntegerValue(Index: integer): integer;
begin
  result := 0;
  Assert(False);
end;

function TSfr_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TSfr_Cell.GetReachLength: double;
begin
  result := Values.ReachLength;
end;

function TSfr_Cell.GetReachLengthAnnotation: string;
begin
  result := Values.ReachLengthAnnotation;
end;

function TSfr_Cell.GetRealAnnotation(Index: integer): string;
begin
  result := '';
  case Index of
    0: result := ReachLengthAnnotation;
    1: result := HydraulicConductivityAnnotation;
    2: result := StreambedElevationAnnotation;
    3: result := StreamBedThicknessAnnotation;
    4: result := StreamSlopeAnnotation;
    5: result := SaturatedWaterContentAnnotation;
    6: result := InitialWaterContentAnnotation;
    7: result := BrooksCoreyExponentAnnotation;
    8: result := VerticalKAnnotation;
    else Assert(False);
  end;
end;

function TSfr_Cell.GetRealValue(Index: integer): double;
begin
  result := 0;
  case Index of
    0: result := ReachLength;
    1: result := HydraulicConductivity;
    2: result := StreambedElevation;
    3: result := StreamBedThickness;
    4: result := StreamSlope;
    5: result := SaturatedWaterContent;
    6: result := InitialWaterContent;
    7: result := BrooksCoreyExponent;
    8: result := VerticalK;
    else Assert(False);
  end;
end;

function TSfr_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TSfr_Cell.GetSaturatedWaterContent: double;
begin
  result := Values.SaturatedWaterContent;
end;

function TSfr_Cell.GetSaturatedWaterContentAnnotation: string;
begin
  result := Values.SaturatedWaterContentAnnotation;
end;

function TSfr_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TSfr_Cell.GetStreambedElevation: double;
begin
  result := Values.StreambedElevation;
end;

function TSfr_Cell.GetStreambedElevationAnnotation: string;
begin
  result := Values.StreambedElevationAnnotation;
end;

function TSfr_Cell.GetStreamBedThickness: double;
begin
  result := Values.StreamBedThickness;
end;

function TSfr_Cell.GetStreamBedThicknessAnnotation: string;
begin
  result := Values.StreamBedThicknessAnnotation;
end;


function TSfr_Cell.GetStreamSlope: double;
begin
  result := Values.StreamSlope;
end;

function TSfr_Cell.GetStreamSlopeAnnotation: string;
begin
  result := Values.StreamSlopeAnnotation;
end;

function TSfr_Cell.GetVerticalK: double;
begin
  result := Values.VerticalK;
end;

function TSfr_Cell.GetVerticalKAnnotation: string;
begin
  result := Values.VerticalKAnnotation;
end;

procedure TSfr_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

{ TSfrRecord }

procedure TSfrRecord.Cache(Comp: TCompressionStream);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, ReachLength);
  WriteCompReal(Comp, StreamSlope);
  WriteCompReal(Comp, StreambedElevation);
  WriteCompReal(Comp, StreamBedThickness);

  WriteCompReal(Comp, HydraulicConductivity);
  WriteCompReal(Comp, SaturatedWaterContent);
  WriteCompReal(Comp, InitialWaterContent);
  WriteCompReal(Comp, BrooksCoreyExponent);
  WriteCompReal(Comp, VerticalK);

  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);

  WriteCompString(Comp, ReachLengthAnnotation);
  WriteCompString(Comp, StreamSlopeAnnotation);
  WriteCompString(Comp, StreambedElevationAnnotation);
  WriteCompString(Comp, StreamBedThicknessAnnotation);

  WriteCompString(Comp, HydraulicConductivityAnnotation);
  WriteCompString(Comp, SaturatedWaterContentAnnotation);
  WriteCompString(Comp, InitialWaterContentAnnotation);
  WriteCompString(Comp, BrooksCoreyExponentAnnotation);
  WriteCompString(Comp, VerticalKAnnotation);
end;

procedure TSfrRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  ReachLength := ReadCompReal(Decomp);
  StreamSlope := ReadCompReal(Decomp);
  StreambedElevation := ReadCompReal(Decomp);
  StreamBedThickness := ReadCompReal(Decomp);

  HydraulicConductivity := ReadCompReal(Decomp);
  SaturatedWaterContent := ReadCompReal(Decomp);
  InitialWaterContent := ReadCompReal(Decomp);
  BrooksCoreyExponent := ReadCompReal(Decomp);
  VerticalK := ReadCompReal(Decomp);

  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);

  ReachLengthAnnotation := ReadCompString(Decomp, Annotations);
  StreamSlopeAnnotation := ReadCompString(Decomp, Annotations);
  StreambedElevationAnnotation := ReadCompString(Decomp, Annotations);
  StreamBedThicknessAnnotation := ReadCompString(Decomp, Annotations);

  HydraulicConductivityAnnotation := ReadCompString(Decomp, Annotations);
  SaturatedWaterContentAnnotation := ReadCompString(Decomp, Annotations);
  InitialWaterContentAnnotation := ReadCompString(Decomp, Annotations);
  BrooksCoreyExponentAnnotation := ReadCompString(Decomp, Annotations);
  VerticalKAnnotation := ReadCompString(Decomp, Annotations);


end;

{ TSfrStorage }

procedure TSfrStorage.Clear;
begin
  SetLength(FSfrArray, 0);
  FCleared := True;
end;

procedure TSfrStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
begin
  Count := Length(FSfrArray);
  Compressor.Write(Count, SizeOf(Count));
  for Index := 0 to Count - 1 do
  begin
    FSfrArray[Index].Cache(Compressor);
  end;
end;

procedure TSfrStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FSfrArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FSfrArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TSfrStorage.GetSfrArray: TSrfArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FSfrArray;
end;

end.
