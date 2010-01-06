unit ModflowSfrUnit;

interface

uses Classes, RbwParser, RealListUnit, OrderedCollectionUnit, ModflowCellUnit,
  ModflowBoundaryUnit, ModflowSfrReachUnit, ModflowSfrChannelUnit, GoPhastTypes,
  ModflowSfrSegment, ModflowSfrUnsatSegment, ModflowSfrTable, ModflowSfrFlows,
  ModflowSfrEquationUnit, ModflowSfrParamIcalcUnit;

type
  TGageLocation = (glNone, glFirst, glLast, glAll);
  // @name represents the MODFLOW Stream Flow Routing boundaries associated with
  // a single @link(TScreenObject).
  //
  // @link(TModflowBoundary.Values) is a @link(TSfrCollection)
  // and represents the stream properties at each reach.
  //
  // @seealso(TSfrCollection)
  TSfrBoundary = class(TModflowBoundary)
  private
    FSegementNumber: integer;
    FChannelValues: TSfrChannelCollection;
    FUpstreamSegmentValues: TSfrSegmentCollection;
    FDownstreamSegmentValues: TSfrSegmentCollection;
    FDownstreamUnsatSegmentValues: TSfrUnsatSegmentCollection;
    FUpstreamUnsatSegmentValues: TSfrUnsatSegmentCollection;
    FTableCollection: TSfrTableCollection;
    FSegmentFlows: TSfrSegmentFlowCollection;
    FEquationValues: TSfrEquationCollection;
    FIFSROPT: integer;
    FParamIcalc: TSfrParamIcalcCollection;
    FGage0: boolean;
    FGage1: boolean;
    FGage2: boolean;
    FGage3: boolean;
    FGage5: boolean;
    FGage6: boolean;
    FGage7: boolean;
    FGageLocation: TGageLocation;
    procedure SetSegementNumber(const Value: integer);
    procedure SetChannelValues(const Value: TSfrChannelCollection);
    procedure SetUpstreamSegmentValues(const Value: TSfrSegmentCollection);
    procedure SetDownstreamSegmentValues(const Value: TSfrSegmentCollection);
    procedure SetDownstreamUnsatSegmentValues(
      const Value: TSfrUnsatSegmentCollection);
    procedure SetUpstreamUnsatSegmentValues(
      const Value: TSfrUnsatSegmentCollection);
    procedure SetTableCollection(const Value: TSfrTableCollection);
    procedure SetSegmentFlows(const Value: TSfrSegmentFlowCollection);
    procedure SetEquationValues(const Value: TSfrEquationCollection);
    function GetISFROPT: integer;
    procedure SetParamIcalc(const Value: TSfrParamIcalcCollection);
    procedure InvalidateDisplayTimeLists;
    procedure SetGage0(const Value: boolean);
    procedure SetGage1(const Value: boolean);
    procedure SetGage2(const Value: boolean);
    procedure SetGage5(const Value: boolean);
    procedure SetGage6(const Value: boolean);
    procedure SetGage7(const Value: boolean);
    procedure SetGage3(const Value: boolean);
    function GetOutTypes: TByteSet;
    procedure SetGageLocation(const Value: TGageLocation);
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TSfr_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
  public
    procedure InvalidateSegmentNumberArray;
    procedure Assign(Source: TPersistent); override;
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TRivStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW SFR parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TSfrStorage) in @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList);
      override;
    procedure InvalidateDisplay; override;
    Constructor Create(Model, ScreenObject: TObject);
    destructor Destroy; override;
    procedure EvaluateArrayBoundaries; override;
    function Used: boolean; override;
    property ISFROPT: integer read GetISFROPT write FIFSROPT;
    procedure UpdateTimes(Times: TRealList); override;
    procedure Clear; override;
    property OutTypes: TByteSet read GetOutTypes;
  published
    property SegementNumber: integer read FSegementNumber
      write SetSegementNumber;
    property ChannelValues: TSfrChannelCollection read FChannelValues
      write SetChannelValues;
    property UpstreamSegmentValues: TSfrSegmentCollection
      read FUpstreamSegmentValues write SetUpstreamSegmentValues;
    property DownstreamSegmentValues: TSfrSegmentCollection
      read FDownstreamSegmentValues write SetDownstreamSegmentValues;
    property UpstreamUnsatSegmentValues: TSfrUnsatSegmentCollection
      read FUpstreamUnsatSegmentValues write SetUpstreamUnsatSegmentValues;
    property DownstreamUnsatSegmentValues: TSfrUnsatSegmentCollection
      read FDownstreamUnsatSegmentValues write SetDownstreamUnsatSegmentValues;
    property TableCollection: TSfrTableCollection read FTableCollection
      write SetTableCollection;
    property SegmentFlows: TSfrSegmentFlowCollection read FSegmentFlows
      write SetSegmentFlows;
    property EquationValues: TSfrEquationCollection read FEquationValues
      write SetEquationValues;
    property ParamIcalc: TSfrParamIcalcCollection read FParamIcalc
      write SetParamIcalc;
    property Gage0: boolean read FGage0 write SetGage0;
    property Gage1: boolean read FGage1 write SetGage1;
    property Gage2: boolean read FGage2 write SetGage2;
    property Gage3: boolean read FGage3 write SetGage3;
    property Gage5: boolean read FGage5 write SetGage5;
    property Gage6: boolean read FGage6 write SetGage6;
    property Gage7: boolean read FGage7 write SetGage7;
    property GageLocation: TGageLocation read FGageLocation write SetGageLocation;
  end;

const
  StrIncompleteSFRData = 'Incomplete SFR data';

implementation

uses Contnrs, DataSetUnit, ScreenObjectUnit, ModflowTimeUnit, PhastModelUnit;

{ TSfrBoundary }

procedure TSfrBoundary.Assign(Source: TPersistent);
var
  Sfr: TSfrBoundary;
begin
  if Source is TSfrBoundary then
  begin
    Sfr := TSfrBoundary(Source);
    if Used <> Sfr.Used then
    begin
      if (PhastModel <> nil) and (ScreenObject <> nil)
        and (ScreenObject as TScreenObject).CanInvalidateModel  then
      begin
        InvalidateDisplayTimeLists;
      end;
    end;
    ISFROPT := Sfr.ISFROPT;
    SegementNumber := Sfr.SegementNumber;

    ChannelValues := Sfr.ChannelValues;
    UpstreamSegmentValues := Sfr.UpstreamSegmentValues;
    DownstreamSegmentValues := Sfr.DownstreamSegmentValues;
    UpstreamUnsatSegmentValues := Sfr.UpstreamUnsatSegmentValues;
    DownstreamUnsatSegmentValues := Sfr.DownstreamUnsatSegmentValues;
    TableCollection := Sfr.TableCollection;
    SegmentFlows := Sfr.SegmentFlows;
    EquationValues := Sfr.EquationValues;
    ParamIcalc := Sfr.ParamIcalc;
    Gage0 := Sfr.Gage0;
    Gage1 := Sfr.Gage1;
    Gage2 := Sfr.Gage2;
    Gage3 := Sfr.Gage3;
    Gage5 := Sfr.Gage5;
    Gage6 := Sfr.Gage6;
    Gage7 := Sfr.Gage7;
    GageLocation := Sfr.GageLocation;
  end;
  inherited;
end;

procedure TSfrBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList);
var
  Cell: TSfr_Cell;
  BoundaryValues: TSfrRecord;
  BoundaryIndex: Integer;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TSfrStorage;
begin
  LocalBoundaryStorage := BoundaryStorage as TSfrStorage;
  TimeIndex := 0;
  if TimeIndex < ValueTimeList.Count then
  begin
    Cells := ValueTimeList[TimeIndex];
  end
  else
  begin
    Cells := TValueCellList.Create(TSfr_Cell);
    ValueTimeList.Add(Cells);
  end;

  Cells.CheckRestore;
  for BoundaryIndex := 0 to Length(LocalBoundaryStorage.SfrArray) - 1 do
  begin
    BoundaryValues := LocalBoundaryStorage.SfrArray[BoundaryIndex];
    Cell := TSfr_Cell.Create;
    Assert(ScreenObject <> nil);
    Cell.IFace := (ScreenObject as TScreenObject).IFace;
    Cells.Add(Cell);
    Cell.StressPeriod := TimeIndex;
    Cell.Values := BoundaryValues;
  end;
  Cells.Cache;
  LocalBoundaryStorage.CacheData;
end;

class function TSfrBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TSfrCollection;
end;

procedure TSfrBoundary.Clear;
begin
  inherited;
  ChannelValues.Clear;
  UpstreamSegmentValues.Clear;
  DownstreamSegmentValues.Clear;
  UpstreamUnsatSegmentValues.Clear;
  DownstreamUnsatSegmentValues.Clear;
  TableCollection.Clear;
  SegmentFlows.Clear;
  EquationValues.Clear;
  ParamIcalc.Clear;
end;

constructor TSfrBoundary.Create(Model, ScreenObject: TObject);
begin
  inherited;
  if Model <> nil then
  begin
    ISFROPT := (Model as TPhastModel).ModflowPackages.SfrPackage.Isfropt;
  end;
  FParamIcalc := TSfrParamIcalcCollection.Create(self, Model, ScreenObject);
  FChannelValues := TSfrChannelCollection.Create(self, Model, ScreenObject);
  FUpstreamSegmentValues := TSfrSegmentCollection.Create(self, Model, ScreenObject);
  FUpstreamSegmentValues.AssignmentLocation := alFirstVertex;
  FDownstreamSegmentValues := TSfrSegmentCollection.Create(self, Model, ScreenObject);
  FDownstreamSegmentValues.AssignmentLocation := alLastVertex;
  FUpstreamUnsatSegmentValues := TSfrUnsatSegmentCollection.Create(self, Model, ScreenObject);
  FUpstreamUnsatSegmentValues.AssignmentLocation := alFirstVertex;
  FDownstreamUnsatSegmentValues := TSfrUnsatSegmentCollection.Create(self, Model, ScreenObject);
  FDownstreamUnsatSegmentValues.AssignmentLocation := alLastVertex;
  FTableCollection := TSfrTableCollection.Create(self, Model, ScreenObject);
  FSegmentFlows := TSfrSegmentFlowCollection.Create(self, Model, ScreenObject);
  FEquationValues := TSfrEquationCollection.Create(self, Model, ScreenObject);
end;

destructor TSfrBoundary.Destroy;
begin
  FEquationValues.Free;
  FSegmentFlows.Free;
  FTableCollection.Free;
  FDownstreamUnsatSegmentValues.Free;
  FUpstreamUnsatSegmentValues.Free;
  FDownstreamSegmentValues.Free;
  FUpstreamSegmentValues.Free;
  FChannelValues.Free;
  FParamIcalc.Free;
  inherited;
end;

procedure TSfrBoundary.EvaluateArrayBoundaries;
begin
  inherited;
  ChannelValues.EvaluateBoundaries;
  UpstreamSegmentValues.EvaluateArrayBoundaries;
  DownstreamSegmentValues.EvaluateArrayBoundaries;
  UpstreamUnsatSegmentValues.EvaluateArrayBoundaries;
  DownstreamUnsatSegmentValues.EvaluateArrayBoundaries;
  EquationValues.EvaluateBoundaries;
  TableCollection.EvaluateBoundaries;
  SegmentFlows.EvaluateBoundaries;
end;

procedure TSfrBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList);
var
  ValueIndex: Integer;
  BoundaryStorage: TSfrStorage;
begin
  EvaluateArrayBoundaries;
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    BoundaryStorage := Values.Boundaries[ValueIndex] as TSfrStorage;
    AssignCells(BoundaryStorage, ValueTimeList);
  end;
end;

function TSfrBoundary.GetISFROPT: integer;
begin
  if PhastModel <> nil then
  begin
    result := (PhastModel as TPhastModel).ModflowPackages.SfrPackage.Isfropt;
  end
  else
  begin
    Result := FIFSROPT;
  end;
end;

function TSfrBoundary.GetOutTypes: TByteSet;
begin
  result := [];
  if Gage0 then
  begin
    Include(result, 0);
  end;
  if Gage1 then
  begin
    Include(result, 1);
    Exclude(result, 0);
  end;
  if Gage2 then
  begin
    Include(result, 2);
    Exclude(result, 0);
  end;
  if Gage3 then
  begin
    Include(result, 3);
    Exclude(result, 0);
  end;
  if result = [1,2,3] then
  begin
    result := [4];
  end;
  if Gage5 then
  begin
    Include(result, 5);
  end;
  if Gage6 then
  begin
    Include(result, 6);
  end;
  if Gage7 then
  begin
    Include(result, 7);
  end;
end;

procedure TSfrBoundary.InvalidateDisplay;
begin
  inherited;
  if Used and (PhastModel <> nil) then
  begin
    InvalidateDisplayTimeLists;
  end;
end;

procedure TSfrBoundary.SetChannelValues(const Value: TSfrChannelCollection);
begin
  FChannelValues.Assign(Value);
end;

procedure TSfrBoundary.SetDownstreamSegmentValues(
  const Value: TSfrSegmentCollection);
begin
  FDownstreamSegmentValues.Assign(Value);
end;

procedure TSfrBoundary.SetDownstreamUnsatSegmentValues(
  const Value: TSfrUnsatSegmentCollection);
begin
  FDownstreamUnsatSegmentValues.Assign(Value);
end;

procedure TSfrBoundary.SetEquationValues(const Value: TSfrEquationCollection);
begin
  FEquationValues.Assign(Value);
end;

procedure TSfrBoundary.SetGage0(const Value: boolean);
begin
  if FGage0 <> Value then
  begin
    InvalidateModel;
    FGage0 := Value;
  end;
end;

procedure TSfrBoundary.SetGage1(const Value: boolean);
begin
  if FGage1 <> Value then
  begin
    InvalidateModel;
    FGage1 := Value;
  end;
end;

procedure TSfrBoundary.SetGage2(const Value: boolean);
begin
  if FGage2 <> Value then
  begin
    InvalidateModel;
    FGage2 := Value;
  end;
end;

procedure TSfrBoundary.SetGage3(const Value: boolean);
begin
  if FGage3 <> Value then
  begin
    InvalidateModel;
    FGage3 := Value;
  end;
end;

procedure TSfrBoundary.SetGage5(const Value: boolean);
begin
  if FGage5 <> Value then
  begin
    InvalidateModel;
    FGage5 := Value;
  end;
end;

procedure TSfrBoundary.SetGage6(const Value: boolean);
begin
  if FGage6 <> Value then
  begin
    InvalidateModel;
    FGage6 := Value;
  end;
end;

procedure TSfrBoundary.SetGage7(const Value: boolean);
begin
  if FGage7 <> Value then
  begin
    InvalidateModel;
    FGage7 := Value;
  end;
end;

procedure TSfrBoundary.SetGageLocation(const Value: TGageLocation);
begin
  if FGageLocation <> Value then
  begin
    InvalidateModel;
    FGageLocation := Value;
  end;
end;

procedure TSfrBoundary.SetParamIcalc(const Value: TSfrParamIcalcCollection);
begin
  FParamIcalc.Assign(Value);
end;

procedure TSfrBoundary.SetSegementNumber(const Value: integer);
begin
  if FSegementNumber <> Value then
  begin
    InvalidateModel;
    InvalidateSegmentNumberArray;
    FSegementNumber := Value;
    if (ScreenObject <> nil)
        and (ScreenObject as TScreenObject).CanInvalidateModel
        and (PhastModel <> nil) then
    begin
      (PhastModel as TPhastModel).DischargeRoutingUpdate;
    end;
  end;
end;

procedure TSfrBoundary.SetSegmentFlows(const Value: TSfrSegmentFlowCollection);
begin
  FSegmentFlows.Assign(Value);
end;

procedure TSfrBoundary.SetTableCollection(const Value: TSfrTableCollection);
begin
  FTableCollection.Assign(Value);
end;

procedure TSfrBoundary.SetUpstreamSegmentValues(const Value: TSfrSegmentCollection);
begin
  FUpstreamSegmentValues.Assign(Value);
end;

procedure TSfrBoundary.SetUpstreamUnsatSegmentValues(
  const Value: TSfrUnsatSegmentCollection);
begin
  FUpstreamUnsatSegmentValues.Assign(Value);
end;

procedure TSfrBoundary.UpdateTimes(Times: TRealList);
begin
  inherited;
  AddBoundaryTimes(ChannelValues, Times);
  AddBoundaryTimes(UpstreamSegmentValues, Times);
  AddBoundaryTimes(DownstreamSegmentValues, Times);
  AddBoundaryTimes(UpstreamUnsatSegmentValues, Times);
  AddBoundaryTimes(DownstreamUnsatSegmentValues, Times);
  AddBoundaryTimes(TableCollection, Times);
  AddBoundaryTimes(SegmentFlows, Times);
  AddBoundaryTimes(EquationValues, Times);
end;

procedure TSfrBoundary.InvalidateDisplayTimeLists;
var
  Model: TPhastModel;
begin
  Model := PhastModel as TPhastModel;
  Model.InvalidateMfSfrSegmentReachAndIcalc(self);
  Model.InvalidateMfSfrIprior(self);
  Model.InvalidateMfSfrVerticalUnsatK(self);
  Model.InvalidateMfSfrReachLength(self);
  Model.InvalidateMfSfrStreamTop(self);
  Model.InvalidateMfSfrStreamSlope(self);
  Model.InvalidateMfSfrStreamThickness(self);
  Model.InvalidateMfSfrStreamK(self);
  Model.InvalidateMfSfrSaturatedWaterContent(self);
  Model.InvalidateMfSfrInitialWaterContent(self);
  Model.InvalidateMfSfrBrooksCorey(self);
  Model.InvalidateMfSfrFlow(self);
  Model.InvalidateMfSfrRunoff(self);
  Model.InvalidateMfSfrPrecipitation(self);
  Model.InvalidateMfSfrEvapotranspiration(self);
  Model.InvalidateMfSfrChannelRoughness(self);
  Model.InvalidateMfSfrBankRoughness(self);
  Model.InvalidateMfSfrDepthCoefficient(self);
  Model.InvalidateMfSfrDepthExponent(self);
  Model.InvalidateMfSfrWidthCoefficient(self);
  Model.InvalidateMfSfrWidthExponent(self);
  Model.InvalidateMfSfrUpstreamHydraulicConductivity(self);
  Model.InvalidateMfSfrDownstreamHydraulicConductivity(self);
  Model.InvalidateMfSfrUpstreamWidth(self);
  Model.InvalidateMfSfrDownstreamWidth(self);
  Model.InvalidateMfSfrUpstreamThickness(self);
  Model.InvalidateMfSfrDownstreamThickness(self);
  Model.InvalidateMfSfrUpstreamElevation(self);
  Model.InvalidateMfSfrDownstreamElevation(self);
  Model.InvalidateMfSfrUpstreamDepth(self);
  Model.InvalidateMfSfrDownstreamDepth(self);
  Model.InvalidateMfSfrUpstreamUnsaturatedWaterContent(self);
  Model.InvalidateMfSfrDownstreamUnsaturatedWaterContent(self);
  Model.InvalidateMfSfrUpstreamUnsatInitialWaterContent(self);
  Model.InvalidateMfSfrDownstreamUnsatInitialWaterContent(self);
  Model.InvalidateMfSfrUpstreamBrooksCorey(self);
  Model.InvalidateMfSfrDownstreamBrooksCorey(self);
  Model.InvalidateMfSfrUpstreamUnsatKz(self);
  Model.InvalidateMfSfrDownstreamUnsatKz(self);
end;

procedure TSfrBoundary.InvalidateSegmentNumberArray;
begin
  if (ScreenObject <> nil)
    and (ScreenObject as TScreenObject).CanInvalidateModel
    and (PhastModel <> nil) then
  begin
    (PhastModel as TPhastModel).InvalidateMfSfrSegmentReachAndIcalc(self);
  end;
end;

function TSfrBoundary.Used: boolean;
var
  LocalISFROPT: integer;
begin
  LocalISFROPT := ISFROPT;
  result := ((LocalISFROPT in [1,2,3]) and inherited Used)
    or ChannelValues.Used
    or ((LocalISFROPT in [0,4,5]) and
      UpstreamSegmentValues.Used and DownstreamSegmentValues.Used)
    or ((LocalISFROPT in [4,5]) and
      UpstreamUnsatSegmentValues.Used and DownstreamUnsatSegmentValues.Used)
    or TableCollection.Used
    or SegmentFlows.Used
    or EquationValues.Used;
end;

end.

