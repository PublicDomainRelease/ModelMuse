unit ModflowSfrWriterUnit;

interface

uses Windows, Types, SysUtils, Classes, Contnrs, CustomModflowWriterUnit,
  ModflowPackageSelectionUnit, PhastModelUnit, ScreenObjectUnit,
  ModflowSfrParamIcalcUnit, ModflowSfrUnit, ModflowSfrSegment,
  ModflowSfrUnsatSegment, ModflowBoundaryDisplayUnit, ModflowCellUnit;


type
//  EInvalidTime = class(Exception)
//    Constructor Create(AnObject: TObject); overload;
//  end;

  TSegment = class(TObject)
  private
    FReaches: TValueCellList;
    FScreenObject: TScreenObject;
    FHasTributaries: boolean;
    FNewSegmentNumber: integer;
    function GetReach(Index: integer): TValueCell;
    function GetReachCount: integer;
  public
    function OriginalSegmentNumber: integer;
    property NewSegmentNumber: integer read FNewSegmentNumber;
    Destructor Destroy; override;
    function OriginalDownStreamSegmentNumbers: TIntegerDynArray;
    function OriginalDiversionSegmentNumbers: TIntegerDynArray;
    property Reaches[Index: integer]: TValueCell read GetReach;
    property ReachCount: integer read GetReachCount;
  end;

  TModflowSFR_Writer = class(TCustomPackageWriter)
  private
    FNameOfFile: string;
    FValues: TList;
    FSegments: TList;
    FLakes: TList;
    ISFROPT: integer;
    NSFRPAR: integer;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSets3and4;
    procedure WriteDataSet4b6a(StartTime: double;
      Segment: TSegment; ParamScreenObjectItem: TSfrParamIcalcItem;
      SfrBoundary: TSfrBoundary; DataSet4B: boolean);
    procedure WriteDataSet4c6b(Parameter: Boolean;
      SfrBoundary: TSfrBoundary; ParamScreenObjectItem: TSfrParamIcalcItem;
      StartTime: double; StressPeriodIndex: integer);
    procedure WriteSegmentValues(StressPeriodIndex: Integer;
      Parameter: Boolean; UpOrDownStreamValues: TSfrSegmentStorage; upstream: Boolean;
      var CommentLine: string; var ValuesWriten: boolean;
      ParamScreenObjectItem: TSfrParamIcalcItem);
    procedure WriteUnsatSegmentValues(upstream: Boolean;
      var CommentLine: string; var ValuesWriten: boolean;
      UnsatUpstreamValues: TSfrUnsatSegmentStorage);
    procedure WriteDataSet4d6c(Parameter: Boolean; SfrBoundary: TSfrBoundary;
      ParamScreenObjectItem: TSfrParamIcalcItem;
      StartTime: double; StressPeriodIndex: integer);
    procedure WriteDataSet4e6d(Parameter: Boolean; SfrBoundary: TSfrBoundary;
      ParamScreenObjectItem: TSfrParamIcalcItem; StressPeriod: integer);
    procedure WriteDataSet4f6e(Parameter: Boolean; SfrBoundary: TSfrBoundary;
      ParamScreenObjectItem: TSfrParamIcalcItem; StartTime: double);
    procedure WriteDataSets5to7;
    function FindConvertedSegment(OriginalSegmentNumber: integer): integer;
    procedure WriteGages(var StartUnitNumber: integer; Lines: TStrings);
    function GetSegment(Index: integer): TSegment;
    function GetSegmentCount: integer;
    procedure TestBedElevations;
  protected
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
    procedure SortSegments;
    procedure Evaluate;
  public
    property Segments[Index: integer]: TSegment read GetSegment;
    property SegmentCount: integer read GetSegmentCount;
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string;
      var StartUnitNumber: integer; Lines: TStrings);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
  end;

implementation

uses ModflowUnitNumbers, OrderedCollectionUnit, frmErrorsAndWarningsUnit,
  ModflowSfrReachUnit, ModflowTransientListParameterUnit, ModflowSfrTable,
  ModflowSfrFlows, ModflowSfrChannelUnit, ModflowSfrEquationUnit,
  ModflowTimeUnit, frmProgressUnit, IntListUnit, GoPhastTypes, Forms, 
  ModflowBoundaryUnit, Dialogs;

resourcestring
  StrInvalidStartingTim = 'Invalid starting time or missing data for the '
    + 'first time step in the following objects';
  StrSFRLinkagesBetween = 'SFR linkages between grids is unsupported';

const
  StrOneOrMoreSFRStre = 'One or more SFR stream segments have slopes that '
    + 'are zero or negative.';
  StrDownstreamOutOfOrder = 'In the SFR package, the following objects '
    + 'define streams that flow into streams with lower segment numbers.  '
    + 'Because the stream segments are in strict numerical order, ModelMuse '
    + 'will not renumber them. However, you may wish to check the order of the '
    + 'stream segments.';
  StrDiversionOutOfOrder = 'In the SFR package, the following objects define'
    + ' streams that divert flow from streams with higher segment numbers.  ' 
    + 'Because the stream segments are in strict numerical order, ModelMuse '
    + 'will not renumber them. However, you may wish to check the order of the '
    + 'stream segments.';
  StrLakeDownstreamError = 'In the SFR package, the following objects '
    + 'define streams that flow into lakes even though the lake package is ' +
      'not in use.';
  StrLakeDiversionError = 'In the SFR package, the following objects '
    + 'define streams that divert flow from lakes even though the lake ' +
      'package is not in use.';

const
  StrSegmentNumber = 'Segment Number in ';
  StrReachNumber = 'Reach Number in ';
  SfrICalcNumber = 'ICALC in ';
  StrDownstreamSegmentNumber = 'Outflow Segment Number in ';
  StrDiversionSegmentNumber = 'Diversion Segment Number in ';
  StrIprior = 'IPRIOR in ';

const
  DupErrorCategory = 'Duplicate SFR segment numbers';
  CircularCategory = 'The following SFR segments circle back on themselves.';
  NoSegmentsWarning = 'One or more objects do not define segments '
    + 'in the SFR package because they do not intersect the grid.';
  UnsatError = 'One or more objects do not define unsaturated flow properties '
    + 'in the SFR package.';

{ TModflowSFR_Writer }

function CompareSegments(Item1, Item2: Pointer): Integer;
var
  Segment1, Segment2: TSegment;
begin
  Segment1 := Item1;
  Segment2 := Item2;
  result := Segment1.OriginalSegmentNumber - Segment2.OriginalSegmentNumber;
end;

constructor TModflowSFR_Writer.Create(Model: TCustomModel; EvaluationType: TEvaluationType);
begin
  inherited;
  FValues := TObjectList.Create;
  FSegments := TObjectList.Create;
end;

destructor TModflowSFR_Writer.Destroy;
begin
  FSegments.Free;
  FValues.Free;
  FLakes.Free;
  inherited;
end;

procedure TModflowSFR_Writer.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TSfrBoundary;
  Dummy: TStringList;
  Segment: TSegment;
  Index: Integer;
  Item: TCustomModflowBoundaryItem;
  StartTime: Double;
  EndTime: Double;
begin

  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidStartingTim);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrIncompleteSFRData);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, DupErrorCategory);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, CircularCategory);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, ChannelRoughnessError);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, BankRoughnessError);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, NoSegmentsWarning);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, UnsatError);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrOneOrMoreSFRStre);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrDownstreamOutOfOrder);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrDiversionOutOfOrder);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrLakeDownstreamError);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrLakeDiversionError);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrSFRLinkagesBetween);

  if Model is TChildModel then
  begin
    frmErrorsAndWarnings.AddWarning(Model, StrSFRLinkagesBetween,
      'ModelMuse does not currently support linking streams between parent '
      + 'and child grids. You will need to modify the SFR files manually if '
      + 'want the streams to be linked.');
  end;

  StartTime := Model.ModflowStressPeriods[0].StartTime;
  EndTime := Model.ModflowStressPeriods[
    Model.ModflowStressPeriods.Count-1].EndTime;

  frmProgressMM.AddMessage('Evaluating SFR Package data.');
  ISFROPT := (Package as TSfrPackageSelection).Isfropt;
  Dummy := TStringList.Create;
  try
    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      if not ScreenObject.UsedModels.UsesModel(Model) then
      begin
        Continue;
      end;
      Boundary := ScreenObject.ModflowSfrBoundary;
      if (Boundary = nil) or not Boundary.Used then
      begin
        Continue;
      end;
      frmProgressMM.AddMessage('    Evaluating ' + ScreenObject.Name);
      Assert(Boundary.Values.Count = 1);
      Item := Boundary.Values[0];
      Item.StartTime := StartTime;
      Item.EndTime := EndTime;

      Boundary.GetCellValues(FValues, Dummy, Model);
      if (FValues.Count >= 1) then
      begin
        Assert(FValues.Count = 1);
        Segment := TSegment.Create;
        Segment.FReaches := FValues.Extract(FValues[0]);
        if Segment.FReaches.Count > 0 then
        begin
          FSegments.Add(Segment);
          Segment.FScreenObject := ScreenObject;
        end
        else
        begin
          frmErrorsAndWarnings.AddWarning(Model,
            NoSegmentsWarning, ScreenObject.Name);
          Segment.Free;
        end;
      end
      else
      begin
      end;
    end;
  finally
    Dummy.Free;
  end;
  SortSegments;
  for Index := 0 to FSegments.Count - 1 do
  begin
    Segment := FSegments[Index];
    Segment.FNewSegmentNumber := Index + 1;
  end;
  TestBedElevations;
end;

class function TModflowSFR_Writer.Extension: string;
begin
  result := '.sfr';
end;

function TModflowSFR_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.SfrPackage;
end;

procedure TModflowSFR_Writer.SortSegments;
const
  DupNumbersError = 'The SFR segments defined by %0:s and %1:s have '
    + 'the same segment number: %2:d.';
  CircularError = 'Object: %0:s; Segment Number: %1:d.';
var
  SortedSegments: TList;
  Segment, Segment1, Segment2: TSegment;
  Index: Integer;
  Error: string;
  SegmentsFound: boolean;
  AnotherSegment: TSegment;
  SegmentNumberArray: TIntegerDynArray;
  OutIndex: integer;
  DownStreamOutOfOrder: TList;
  DiversionOutOfOrder: TList;
  WarningIndex: Integer;
  AScreenObject: TScreenObject;
  LakeDownstreamError: TList;
  LakeDiversionError: TList;
  function FindSegment(Number: integer): TSegment;
  var
    Index: Integer;
    Segment: TSegment;
  begin
    result := nil;
    for Index := 0 to FSegments.Count - 1 do
    begin
      Segment := FSegments[Index];
      if (Segment <> nil) and (Segment.OriginalSegmentNumber = Number) then
      begin
        result := Segment;
        Exit;
      end;
    end;
  end;
begin
  // sort the segments in order of their original segment number.
  FSegments.Sort(CompareSegments);

  Error := '';
  for Index := 0 to FSegments.Count - 2 do
  begin
    Segment1 := FSegments[Index];
    Segment2 := FSegments[Index+1];
    if Segment1.OriginalSegmentNumber = Segment2.OriginalSegmentNumber then
    begin
      Error := Format(DupNumbersError, [Segment1.FScreenObject.Name,
        Segment2.FScreenObject.Name, Segment1.OriginalSegmentNumber]);
      frmErrorsAndWarnings.AddError(Model, DupErrorCategory, Error);
    end;
  end;

  if not Model.ModflowPackages.LakPackage.IsSelected then
  begin
    LakeDownstreamError := TList.Create;
    LakeDiversionError := TList.Create;
    try
      for Index := 0 to FSegments.Count - 1 do
      begin
        Segment := FSegments[Index];
        SegmentNumberArray := Segment.OriginalDownStreamSegmentNumbers;
        for OutIndex := 0 to Length(SegmentNumberArray) - 1 do
        begin
          if (SegmentNumberArray[OutIndex] < 0) then
          begin
            LakeDownstreamError.Add(Segment.FScreenObject);
            break;
          end;
        end;

        SegmentNumberArray := Segment.OriginalDiversionSegmentNumbers;
        for OutIndex := 0 to Length(SegmentNumberArray) - 1 do
        begin
          if (SegmentNumberArray[OutIndex] < 0) then
          begin
            LakeDiversionError.Add(Segment.FScreenObject);
            break;
          end;
        end;
      end;
      for WarningIndex := 0 to LakeDownstreamError.Count - 1 do
      begin
        AScreenObject := LakeDownstreamError[WarningIndex];
        frmErrorsAndWarnings.AddError(Model,
          StrLakeDownstreamError, AScreenObject.Name);
      end;
      for WarningIndex := 0 to LakeDiversionError.Count - 1 do
      begin
        AScreenObject := LakeDiversionError[WarningIndex];
        frmErrorsAndWarnings.AddError(Model,
          StrLakeDiversionError, AScreenObject.Name);
      end;
    finally
      LakeDiversionError.Free;
      LakeDownstreamError.Free;
    end;
  end;

//  UnsortedList := nil;
  // don't renumber the segments if they are already in
  // numerical order.
  if (Error = '') and (FSegments.Count > 0) then
  begin
    Segment1 := FSegments[0];
    if Segment1.OriginalSegmentNumber = 1 then
    begin
      Segment2 := FSegments[FSegments.Count - 1];
      if Segment2.OriginalSegmentNumber = FSegments.Count then
      begin
//        OrderOK := True;
        DownStreamOutOfOrder := TList.Create;
        DiversionOutOfOrder := TList.Create;
        try
          for Index := 0 to FSegments.Count - 1 do
          begin
            Segment := FSegments[Index];
            SegmentNumberArray := Segment.OriginalDownStreamSegmentNumbers;
            for OutIndex := 0 to Length(SegmentNumberArray) - 1 do
            begin
              if (SegmentNumberArray[OutIndex] > 0) then
              begin
                if (SegmentNumberArray[OutIndex] > FSegments.Count)
                or (SegmentNumberArray[OutIndex] < Segment.OriginalSegmentNumber) then
                begin
//                  OrderOK := False;
                  if DownStreamOutOfOrder.IndexOf(Segment.FScreenObject) < 0 then
                  begin
                    DownStreamOutOfOrder.Add(Segment.FScreenObject)
                  end;
                end;
              end;
            end;
            SegmentNumberArray := Segment.OriginalDiversionSegmentNumbers;
            for OutIndex := 0 to Length(SegmentNumberArray) - 1 do
            begin
              if (SegmentNumberArray[OutIndex] > 0) then
              begin
                if (SegmentNumberArray[OutIndex] > FSegments.Count)
                or (SegmentNumberArray[OutIndex] > Segment.OriginalSegmentNumber) then
                begin
//                  OrderOK := False;
                  if DiversionOutOfOrder.IndexOf(Segment.FScreenObject) < 0 then
                  begin
                    DiversionOutOfOrder.Add(Segment.FScreenObject)
                  end;
                end;
              end;
            end;
          end;
          for WarningIndex := 0 to DownStreamOutOfOrder.Count - 1 do
          begin
            AScreenObject := DownStreamOutOfOrder[WarningIndex];
            frmErrorsAndWarnings.AddWarning(Model,
              StrDownstreamOutOfOrder, AScreenObject.Name);
          end;
          for WarningIndex := 0 to DiversionOutOfOrder.Count - 1 do
          begin
            AScreenObject := DiversionOutOfOrder[WarningIndex];
            frmErrorsAndWarnings.AddWarning(Model,
              StrDiversionOutOfOrder, AScreenObject.Name);
          end;
        finally
          DownStreamOutOfOrder.Free;
          DiversionOutOfOrder.Free;
        end;
//        if OrderOK then
//        begin
          Exit;
//          UnsortedList := TObjectList.Create;
//          TObjectList(UnsortedList).OwnsObjects := True;
//          UnsortedList.Capacity := FSegments.Count;
//          for Index := 0 to FSegments.Count - 1 do
//          begin
//            UnsortedList.Add(FSegments[Index]);
//          end;
//        end;
//        Exit;
      end;
    end;
  end;

  (FSegments as TObjectList).OwnsObjects := False;

  SortedSegments := TObjectList.Create;
  SortedSegments.Capacity := FSegments.Count;

  repeat
    SegmentsFound := False;
    for Index := 0 to FSegments.Count - 1 do
    begin
      Segment := FSegments[Index];
      Segment.FHasTributaries := False;
    end;

    for Index := 0 to FSegments.Count - 1 do
    begin
      Segment := FSegments[Index];
      SegmentNumberArray := Segment.OriginalDownStreamSegmentNumbers;
      for OutIndex := 0 to Length(SegmentNumberArray) - 1 do
      begin
        Segment := FindSegment(SegmentNumberArray[OutIndex]);
        if Segment <> nil then
        begin
          Segment.FHasTributaries := True;
        end;
      end;
    end;

    for Index := 0 to FSegments.Count - 1 do
    begin
      Segment := FSegments[Index];
      SegmentNumberArray := Segment.OriginalDiversionSegmentNumbers;
      for OutIndex := 0 to Length(SegmentNumberArray) - 1 do
      begin
        AnotherSegment := FindSegment(SegmentNumberArray[OutIndex]);
        if AnotherSegment <> nil then
        begin
          Segment.FHasTributaries := True;
          break;
        end;
      end;
    end;

    for Index := 0 to FSegments.Count - 1 do
    begin
      Segment := FSegments[Index];
      if not Segment.FHasTributaries then
      begin
        SortedSegments.Add(Segment);
        FSegments[Index] := nil;
        SegmentsFound := True;
      end;
    end;

    FSegments.Pack;
  until (not SegmentsFound) or (FSegments.Count = 0);

  if FSegments.Count > 0 then
  begin
    for Index := 0 to FSegments.Count - 1 do
    begin
      Segment := FSegments[Index];
      Error := Format(CircularError, [Segment.FScreenObject.Name,
        Segment.OriginalSegmentNumber]);
      frmErrorsAndWarnings.AddError(Model, CircularCategory, Error);
    end;
  end;
//  if UnsortedList = nil then
//  begin
    (FSegments as TObjectList).OwnsObjects := True;
    FSegments.Free;

    FSegments := SortedSegments;
//  end
//  else
//  begin
//    (SortedSegments as TObjectList).OwnsObjects := False;
//    SortedSegments.Free;
//    FSegments.Free;
//    FSegments := UnsortedList;
//  end;
end;

procedure TModflowSFR_Writer.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  SegmentNumberTimes: TModflowBoundaryDisplayTimeList;
  ReachNumberTimes: TModflowBoundaryDisplayTimeList;
  SegmentIndex: Integer;
  Segment: TSegment;
  ReachIndex: Integer;
  Reach: TSfr_Cell;
  TimeIndex: Integer;
  DataArray: TModflowBoundaryDisplayDataArray;
  TimeListIndex: Integer;
  DisplayTimeList: TModflowBoundaryDisplayTimeList;
  Boundary: TSfrBoundary;
  Item: TSfrParamIcalcItem;
  ICalcTimeList: TModflowBoundaryDisplayTimeList;
  SegmentComment, ReachComment, ICALC_Comment, DownstreamComment: string;
  ReachLengthList: TModflowBoundaryDisplayTimeList;
  StreamElevationList: TModflowBoundaryDisplayTimeList;
  StreamSlopeList: TModflowBoundaryDisplayTimeList;
  StreamThicknessList: TModflowBoundaryDisplayTimeList;
  StreamKList: TModflowBoundaryDisplayTimeList;
  SatWatContent: TModflowBoundaryDisplayTimeList;
  InitWatContent: TModflowBoundaryDisplayTimeList;
  BrooksCorey: TModflowBoundaryDisplayTimeList;
  UnSatKz: TModflowBoundaryDisplayTimeList;
  OutSeg: TModflowBoundaryDisplayTimeList;
  DiversionSeg: TModflowBoundaryDisplayTimeList;
  ADisplayList: TModflowBoundaryDisplayTimeList;
  Index: Integer;
  DiversionComment, IpriorComment: string;
  IpriorList: TModflowBoundaryDisplayTimeList;
  FlowList: TModflowBoundaryDisplayTimeList;
  FlowRecord: TSfrSegmentFlowRecord;
  RunOffList: TModflowBoundaryDisplayTimeList;
  PrecipitationList: TModflowBoundaryDisplayTimeList;
  EvapotranspirationList: TModflowBoundaryDisplayTimeList;
  ChannelRoughnessList: TModflowBoundaryDisplayTimeList;
  ChannelRecord: TSfrChannelRecord;
  BankRoughnessList: TModflowBoundaryDisplayTimeList;
  DepthCoefficientList: TModflowBoundaryDisplayTimeList;
  DepthExponentList: TModflowBoundaryDisplayTimeList;
  WidthCoefficientList: TModflowBoundaryDisplayTimeList;
  WidthExponentList: TModflowBoundaryDisplayTimeList;
  EquationRecord: TSfrEquationRecord;
  UpstreamHydraulicConductivityList: TModflowBoundaryDisplayTimeList;
  DownstreamHydraulicConductivityList: TModflowBoundaryDisplayTimeList;
  UpstreamValues: TSfrSegmentStorage;
  UpstreamRecord: TSfrSegmentRecord;
  DownstreamValues: TSfrSegmentStorage;
  DownstreamRecord: TSfrSegmentRecord;
  UpstreamWidthList: TModflowBoundaryDisplayTimeList;
  DownstreamWidthList: TModflowBoundaryDisplayTimeList;
  UpstreamThicknessList: TModflowBoundaryDisplayTimeList;
  DownstreamThicknessList: TModflowBoundaryDisplayTimeList;
  UpstreamElevationList: TModflowBoundaryDisplayTimeList;
  DownstreamElevationList: TModflowBoundaryDisplayTimeList;
  UpstreamDepthList: TModflowBoundaryDisplayTimeList;
  DownstreamDepthList: TModflowBoundaryDisplayTimeList;

  UpstreamUnSatWatContList: TModflowBoundaryDisplayTimeList;
  DownstreamUnSatWatContList: TModflowBoundaryDisplayTimeList;
  UpstreamUnSatInitWatContList: TModflowBoundaryDisplayTimeList;
  DownstreamUnSatInitWatContList: TModflowBoundaryDisplayTimeList;
  UpstreamBrooksCoreyList: TModflowBoundaryDisplayTimeList;
  DownstreamBrooksCoreyList: TModflowBoundaryDisplayTimeList;
  UpstreamUnSatKzList: TModflowBoundaryDisplayTimeList;
  DownstreamUnSatKzList: TModflowBoundaryDisplayTimeList;
  UpstreamUnsatValues: TSfrUnsatSegmentStorage;
  UpstreamUnsatRecord: TSfrUnsatSegmentRecord;
  DownstreamUnsatValues: TSfrUnsatSegmentStorage;
  DownstreamUnsatRecord: TSfrUnsatSegmentRecord;
  Param: TModflowTransientListParameter;
  KAnnotation: string;
  ErrorObject: TScreenObject;
  function WidthValueUsed: boolean;
  begin
    result := True;
    case ISFROPT of
      0,1:
        begin
          result := Item.ICalc <= 1;
        end;
      2,3,4,5:
        begin
          if Item.ICalc <= 0 then
          begin
            result := True;
          end
          else if Item.ICalc = 1 then
          begin
            result := TimeIndex = 0;
          end
          else
          begin
            result := False;
          end;
        end;
      else
        Assert(False);
    end;
  end;
  function ThicknessElevUsed: boolean;
  begin
    result := True;
    case ISFROPT of
      0,1:
        begin
          result := True;
        end;
      2,3:
        begin
          result := False;
        end;
      4,5:
        begin
          if Item.ICalc <= 0 then
          begin
            result := True;
          end
          else if Item.ICalc in [1,2] then
          begin
            result := TimeIndex = 0;
          end
          else
          begin
            result := True;
          end;
        end;
      else
        Assert(False);
    end;
  end;
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;

  Evaluate;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  SegmentNumberTimes := TimeLists[0];
  ReachNumberTimes := TimeLists[1];
  ICalcTimeList := TimeLists[2];
  ReachLengthList := TimeLists[3];
  StreamElevationList := TimeLists[4];
  StreamSlopeList := TimeLists[5];
  StreamThicknessList := TimeLists[6];
  StreamKList := TimeLists[7];
  SatWatContent := TimeLists[8];
  InitWatContent := TimeLists[9];
  BrooksCorey := TimeLists[10];
  UnSatKz := TimeLists[11];
  OutSeg := TimeLists[12];
  DiversionSeg := TimeLists[13];
  IpriorList := TimeLists[14];
  FlowList := TimeLists[15];
  RunOffList := TimeLists[16];
  PrecipitationList := TimeLists[17];
  EvapotranspirationList := TimeLists[18];
  ChannelRoughnessList := TimeLists[19];
  BankRoughnessList := TimeLists[20];
  DepthCoefficientList := TimeLists[21];
  DepthExponentList := TimeLists[22];
  WidthCoefficientList := TimeLists[23];
  WidthExponentList := TimeLists[24];
  UpstreamHydraulicConductivityList := TimeLists[25];
  DownstreamHydraulicConductivityList := TimeLists[26];
  UpstreamWidthList := TimeLists[27];
  DownstreamWidthList := TimeLists[28];
  UpstreamThicknessList := TimeLists[29];
  DownstreamThicknessList := TimeLists[30];
  UpstreamElevationList := TimeLists[31];
  DownstreamElevationList := TimeLists[32];
  UpstreamDepthList := TimeLists[33];
  DownstreamDepthList := TimeLists[34];
  UpstreamUnSatWatContList := TimeLists[35];
  DownstreamUnSatWatContList := TimeLists[36];
  UpstreamUnSatInitWatContList := TimeLists[37];
  DownstreamUnSatInitWatContList := TimeLists[38];
  UpstreamBrooksCoreyList := TimeLists[39];
  DownstreamBrooksCoreyList := TimeLists[40];
  UpstreamUnSatKzList := TimeLists[41];
  DownstreamUnSatKzList := TimeLists[42];

  // check that all the time lists contain the same number of times
  // as the first one.
  for Index := 1 to TimeLists.Count - 1 do
  begin
    ADisplayList := TimeLists[Index];
    Assert(SegmentNumberTimes.Count = ADisplayList.Count);
  end;

  for SegmentIndex := 0 to FSegments.Count - 1 do
  begin
    Segment := FSegments[SegmentIndex];
    Boundary := Segment.FScreenObject.ModflowSfrBoundary;

    SegmentComment := StrSegmentNumber + Segment.FScreenObject.Name;
    ReachComment := StrReachNumber + Segment.FScreenObject.Name;
    ICALC_Comment := SfrICalcNumber + Segment.FScreenObject.Name;
    DownstreamComment := StrDownstreamSegmentNumber + Segment.FScreenObject.Name;
    DiversionComment := StrDiversionSegmentNumber + Segment.FScreenObject.Name;
    IpriorComment := StrIprior + Segment.FScreenObject.Name;

    for ReachIndex := 0 to Segment.FReaches.Count - 1 do
    begin
      Reach := Segment.FReaches[ReachIndex] as TSfr_Cell;
      for TimeIndex := 0 to SegmentNumberTimes.Count - 1 do
      begin
        DataArray := SegmentNumberTimes[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(SegmentComment, Segment.OriginalSegmentNumber,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
      for TimeIndex := 0 to OutSeg.Count - 1 do
      begin
        Item := Boundary.ParamIcalc.GetItemByStartTime(
          OutSeg.Times[TimeIndex]);
        if Item <> nil then
        begin
          DataArray := OutSeg[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(DownstreamComment,
            Item.OutflowSegment,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
      end;
      for TimeIndex := 0 to DiversionSeg.Count - 1 do
      begin
        Item := Boundary.ParamIcalc.GetItemByStartTime(
          DiversionSeg.Times[TimeIndex]);
        if Item <> nil then
        begin
          DataArray := DiversionSeg[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(DiversionComment,
            Item.DiversionSegment,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
      end;
      for TimeIndex := 0 to IpriorList.Count - 1 do
      begin
        Item := Boundary.ParamIcalc.GetItemByStartTime(
          IpriorList.Times[TimeIndex]);
        if Item <> nil then
        begin
          if Item.DiversionSegment <> 0 then
          begin
            DataArray := IpriorList[TimeIndex]
              as TModflowBoundaryDisplayDataArray;
            DataArray.AddDataValue(IpriorComment,
              Item.IPRIOR,
              Reach.Column, Reach.Row, Reach.Layer);
          end;
        end;
      end;
      for TimeIndex := 0 to ReachNumberTimes.Count - 1 do
      begin
        DataArray := ReachNumberTimes[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(ReachComment, ReachIndex + 1,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
      for TimeIndex := 0 to ICalcTimeList.Count - 1 do
      begin
        Item := Boundary.ParamIcalc.GetItemByStartTime(
          ICalcTimeList.Times[TimeIndex]);
        if Item <> nil then
        begin
          DataArray := ICalcTimeList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(ICALC_Comment, Item.ICalc,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
      end;
      for TimeIndex := 0 to ReachLengthList.Count - 1 do
      begin
        DataArray := ReachLengthList[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataArray.AddDataValue(Reach.ReachLengthAnnotation, Reach.ReachLength,
          Reach.Column, Reach.Row, Reach.Layer);
      end;
      if ISFROPT in [1,2,3] then
      begin
        for TimeIndex := 0 to StreamElevationList.Count - 1 do
        begin
          DataArray := StreamElevationList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(Reach.StreambedElevationAnnotation,
            Reach.StreambedElevation,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
        for TimeIndex := 0 to StreamSlopeList.Count - 1 do
        begin
          DataArray := StreamSlopeList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(Reach.StreamSlopeAnnotation,
            Reach.StreamSlope,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
        for TimeIndex := 0 to StreamThicknessList.Count - 1 do
        begin
          DataArray := StreamThicknessList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(Reach.StreamBedThicknessAnnotation,
            Reach.StreamBedThickness,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
        for TimeIndex := 0 to StreamKList.Count - 1 do
        begin
          DataArray := StreamKList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(Reach.HydraulicConductivityAnnotation,
            Reach.HydraulicConductivity,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
      end;
      if ISFROPT in [2,3] then
      begin
        for TimeIndex := 0 to SatWatContent.Count - 1 do
        begin
          DataArray := SatWatContent[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(Reach.SaturatedWaterContentAnnotation,
            Reach.SaturatedWaterContent,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
        for TimeIndex := 0 to InitWatContent.Count - 1 do
        begin
          DataArray := InitWatContent[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(Reach.InitialWaterContentAnnotation,
            Reach.InitialWaterContent,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
        for TimeIndex := 0 to BrooksCorey.Count - 1 do
        begin
          DataArray := BrooksCorey[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(Reach.BrooksCoreyExponentAnnotation,
            Reach.BrooksCoreyExponent,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
      end;
      if ISFROPT = 3 then
      begin
        for TimeIndex := 0 to UnSatKz.Count - 1 do
        begin
          DataArray := UnSatKz[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(Reach.VerticalKAnnotation,
            Reach.VerticalK,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
      end;
      for TimeIndex := 0 to FlowList.Count - 1 do
      begin
        Item := Boundary.ParamIcalc.GetItemByStartTime(
          FlowList.Times[TimeIndex]);
        if Item <> nil then
        begin
          FlowRecord := Boundary.SegmentFlows.
            GetFlowValuesFromTime(FlowList.Times[TimeIndex]);
          DataArray := FlowList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(FlowRecord.FlowAnnotation,
            FlowRecord.Flow,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
      end;
      for TimeIndex := 0 to RunOffList.Count - 1 do
      begin
        Item := Boundary.ParamIcalc.GetItemByStartTime(
          RunOffList.Times[TimeIndex]);
        if Item <> nil then
        begin
          FlowRecord := Boundary.SegmentFlows.
            GetFlowValuesFromTime(RunOffList.Times[TimeIndex]);
          DataArray := RunOffList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(FlowRecord.RunnoffAnnotation,
            FlowRecord.Runnoff,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
      end;
      for TimeIndex := 0 to PrecipitationList.Count - 1 do
      begin
        Item := Boundary.ParamIcalc.GetItemByStartTime(
          PrecipitationList.Times[TimeIndex]);
        if Item <> nil then
        begin
          FlowRecord := Boundary.SegmentFlows.
            GetFlowValuesFromTime(PrecipitationList.Times[TimeIndex]);
          DataArray := PrecipitationList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(FlowRecord.PrecipitationAnnotation,
            FlowRecord.Precipitation,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
      end;
      for TimeIndex := 0 to EvapotranspirationList.Count - 1 do
      begin
        Item := Boundary.ParamIcalc.GetItemByStartTime(
          EvapotranspirationList.Times[TimeIndex]);
        if Item <> nil then
        begin
          FlowRecord := Boundary.SegmentFlows.
            GetFlowValuesFromTime(EvapotranspirationList.Times[TimeIndex]);
          DataArray := EvapotranspirationList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(FlowRecord.EvapotranspirationAnnotation,
            FlowRecord.Evapotranspiration,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
      end;
      for TimeIndex := 0 to ChannelRoughnessList.Count - 1 do
      begin
        Item := Boundary.ParamIcalc.GetItemByStartTime(
          ChannelRoughnessList.Times[TimeIndex]);
        if (Item <> nil) and (Item.ICalc in [1,2]) then
        begin
          ChannelRecord := Boundary.ChannelValues.
            GetChannelTimeValuesFromTime(Model, ChannelRoughnessList.Times[TimeIndex]);
          DataArray := ChannelRoughnessList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(ChannelRecord.ChannelRoughnessAnnotation,
            ChannelRecord.ChannelRoughness,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
      end;
      for TimeIndex := 0 to BankRoughnessList.Count - 1 do
      begin
        Item := Boundary.ParamIcalc.GetItemByStartTime(
          BankRoughnessList.Times[TimeIndex]);
        if  (Item <> nil) and (Item.ICalc = 2) then
        begin
          ChannelRecord := Boundary.ChannelValues.
            GetChannelTimeValuesFromTime(Model, BankRoughnessList.Times[TimeIndex]);
          DataArray := BankRoughnessList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(ChannelRecord.BankRoughnessAnnotation,
            ChannelRecord.BankRoughness,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
      end;
      for TimeIndex := 0 to DepthCoefficientList.Count - 1 do
      begin
        Item := Boundary.ParamIcalc.GetItemByStartTime(
          DepthCoefficientList.Times[TimeIndex]);
        if  (Item <> nil) and (Item.ICalc = 3) then
        begin
          EquationRecord := Boundary.EquationValues.
            GetEquationTimeValuesFromTime(DepthCoefficientList.Times[TimeIndex]);
          DataArray := DepthCoefficientList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(EquationRecord.DepthCoefficientAnnotation,
            EquationRecord.DepthCoefficient,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
      end;
      for TimeIndex := 0 to DepthExponentList.Count - 1 do
      begin
        Item := Boundary.ParamIcalc.GetItemByStartTime(
          DepthExponentList.Times[TimeIndex]);
        if  (Item <> nil) and (Item.ICalc = 3) then
        begin
          EquationRecord := Boundary.EquationValues.
            GetEquationTimeValuesFromTime(DepthExponentList.Times[TimeIndex]);
          DataArray := DepthExponentList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(EquationRecord.DepthExponentAnnotation,
            EquationRecord.DepthExponent,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
      end;
      for TimeIndex := 0 to WidthCoefficientList.Count - 1 do
      begin
        Item := Boundary.ParamIcalc.GetItemByStartTime(
          WidthCoefficientList.Times[TimeIndex]);
        if  (Item <> nil) and (Item.ICalc = 3) then
        begin
          EquationRecord := Boundary.EquationValues.
            GetEquationTimeValuesFromTime(WidthCoefficientList.Times[TimeIndex]);
          DataArray := WidthCoefficientList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(EquationRecord.WidthCoefficientAnnotation,
            EquationRecord.WidthCoefficient,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
      end;
      for TimeIndex := 0 to WidthExponentList.Count - 1 do
      begin
        Item := Boundary.ParamIcalc.GetItemByStartTime(
          WidthExponentList.Times[TimeIndex]);
        if  (Item <> nil) and (Item.ICalc = 3) then
        begin
          EquationRecord := Boundary.EquationValues.
            GetEquationTimeValuesFromTime(WidthExponentList.Times[TimeIndex]);
          DataArray := WidthExponentList[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArray.AddDataValue(EquationRecord.WidthExponentAnnotation,
            EquationRecord.WidthExponent,
            Reach.Column, Reach.Row, Reach.Layer);
        end;
      end;
      if (ISFROPT in [0,4,5])  then
      begin
        if (ReachIndex = 0) then
        begin
          for TimeIndex := 0 to UpstreamHydraulicConductivityList.Count - 1 do
          begin
            Item := Boundary.ParamIcalc.GetItemByStartTime(
              DepthExponentList.Times[TimeIndex]);
            if (Item <> nil) then
            begin
              Param := nil;
              if Item.Param <> '' then
              begin
                Param := Model.ModflowTransientParameters.GetParamByName(Item.Param);
              end;
              UpstreamValues := Boundary.UpstreamSegmentValues.
                GetBoundaryByStartTime(UpstreamHydraulicConductivityList.Times[TimeIndex])
                as TSfrSegmentStorage;
              if UpstreamValues = nil then
              begin
                ErrorObject := Boundary.ScreenObject as TScreenObject;
                frmErrorsAndWarnings.AddError(Model,
                  'Invalid starting time in the following objects', ErrorObject.Name);
                Continue;
              end;
              UpstreamRecord := UpstreamValues.SrfSegmentArray[0];
              DataArray := UpstreamHydraulicConductivityList[TimeIndex]
                as TModflowBoundaryDisplayDataArray;
              if Param = nil then
              begin
                DataArray.AddDataValue(UpstreamRecord.HydraulicConductivityAnnotation,
                  UpstreamRecord.HydraulicConductivity,
                  Reach.Column, Reach.Row, Reach.Layer);
              end
              else
              begin
                KAnnotation := UpstreamRecord.HydraulicConductivityAnnotation
                  + ' multiplied by Parameter: ' + Item.Param;
                DataArray.AddDataValue(
                  KAnnotation,
                  UpstreamRecord.HydraulicConductivity * Param.Value,
                  Reach.Column, Reach.Row, Reach.Layer);
              end;
            end;
          end;
        end;
        if (ReachIndex = Segment.FReaches.Count - 1) then
        begin
          for TimeIndex := 0 to DownstreamHydraulicConductivityList.Count - 1 do
          begin
            Item := Boundary.ParamIcalc.GetItemByStartTime(
              DepthExponentList.Times[TimeIndex]);
            if (Item <> nil) then
            begin
              Param := nil;
              if Item.Param <> '' then
              begin
                Param := Model.ModflowTransientParameters.GetParamByName(Item.Param);
              end;
              DownstreamValues := Boundary.DownstreamSegmentValues.
                GetBoundaryByStartTime(DownstreamHydraulicConductivityList.Times[TimeIndex])
                as TSfrSegmentStorage;
              if DownstreamValues = nil then
              begin
                ErrorObject := Boundary.ScreenObject as TScreenObject;
                frmErrorsAndWarnings.AddError(Model,
                  StrInvalidStartingTim, ErrorObject.Name);
                Continue;
//                  raise EInvalidTime.Create(Boundary.ScreenObject);
              end;
              DownstreamRecord := DownstreamValues.SrfSegmentArray[0];
              DataArray := DownstreamHydraulicConductivityList[TimeIndex]
                as TModflowBoundaryDisplayDataArray;
              if Param = nil then
              begin
                DataArray.AddDataValue(DownstreamRecord.HydraulicConductivityAnnotation,
                  DownstreamRecord.HydraulicConductivity,
                  Reach.Column, Reach.Row, Reach.Layer);
              end
              else
              begin
                KAnnotation := DownstreamRecord.HydraulicConductivityAnnotation
                  + ' multiplied by Parameter: ' + Item.Param;
                DataArray.AddDataValue(
                  KAnnotation,
                  DownstreamRecord.HydraulicConductivity * Param.Value,
                  Reach.Column, Reach.Row, Reach.Layer);
              end;
            end;
          end;
        end;
      end;

      if (ReachIndex = 0) then
      begin
        for TimeIndex := 0 to UpstreamWidthList.Count - 1 do
        begin
          Item := Boundary.ParamIcalc.GetItemByStartTime(
            UpstreamWidthList.Times[TimeIndex]);
          if (Item <> nil) and WidthValueUsed then
          begin
            UpstreamValues := Boundary.UpstreamSegmentValues.
              GetBoundaryByStartTime(UpstreamWidthList.Times[TimeIndex])
              as TSfrSegmentStorage;
            if UpstreamValues = nil then
            begin
              ErrorObject := Boundary.ScreenObject as TScreenObject;
              frmErrorsAndWarnings.AddError(Model,
                StrInvalidStartingTim, ErrorObject.Name);
              Continue;
//                raise EInvalidTime.Create(Boundary.ScreenObject);
            end;
            UpstreamRecord := UpstreamValues.SrfSegmentArray[0];
            DataArray := UpstreamWidthList[TimeIndex]
              as TModflowBoundaryDisplayDataArray;
            DataArray.AddDataValue(UpstreamRecord.StreamWidthAnnotation,
              UpstreamRecord.StreamWidth,
              Reach.Column, Reach.Row, Reach.Layer);
          end;
        end;
      end;
      if (ReachIndex = Segment.FReaches.Count - 1) then
      begin
        for TimeIndex := 0 to DownstreamWidthList.Count - 1 do
        begin
          Item := Boundary.ParamIcalc.GetItemByStartTime(
            DownstreamWidthList.Times[TimeIndex]);
          if (Item <> nil) and WidthValueUsed then
          begin
            DownstreamValues := Boundary.DownstreamSegmentValues.
              GetBoundaryByStartTime(DownstreamWidthList.Times[TimeIndex])
              as TSfrSegmentStorage;
            if DownstreamValues = nil then
            begin
              ErrorObject := Boundary.ScreenObject as TScreenObject;
              frmErrorsAndWarnings.AddError(Model,
                StrInvalidStartingTim, ErrorObject.Name);
              Continue;
//                raise EInvalidTime.Create(Boundary.ScreenObject);
            end;
            DownstreamRecord := DownstreamValues.SrfSegmentArray[0];
            DataArray := DownstreamWidthList[TimeIndex]
              as TModflowBoundaryDisplayDataArray;
            DataArray.AddDataValue(DownstreamRecord.StreamWidthAnnotation,
              DownstreamRecord.StreamWidth,
              Reach.Column, Reach.Row, Reach.Layer);
          end;
        end;
      end;
      if ISFROPT in [0,4,5] then
      begin
        if (ReachIndex = 0) then
        begin
          for TimeIndex := 0 to UpstreamThicknessList.Count - 1 do
          begin
            Item := Boundary.ParamIcalc.GetItemByStartTime(
              UpstreamThicknessList.Times[TimeIndex]);
            if (Item <> nil) and ThicknessElevUsed then
            begin
              UpstreamValues := Boundary.UpstreamSegmentValues.
                GetBoundaryByStartTime(UpstreamThicknessList.Times[TimeIndex])
                as TSfrSegmentStorage;
              if UpstreamValues = nil then
              begin
                ErrorObject := Boundary.ScreenObject as TScreenObject;
                frmErrorsAndWarnings.AddError(Model,
                  StrInvalidStartingTim, ErrorObject.Name);
                Continue;
//                  raise EInvalidTime.Create(Boundary.ScreenObject);
              end;
              UpstreamRecord := UpstreamValues.SrfSegmentArray[0];
              DataArray := UpstreamThicknessList[TimeIndex]
                as TModflowBoundaryDisplayDataArray;
              DataArray.AddDataValue(UpstreamRecord.StreamBedThicknessAnnotation,
                UpstreamRecord.StreamBedThickness,
                Reach.Column, Reach.Row, Reach.Layer);
            end;
          end;
        end;
        if (ReachIndex = Segment.FReaches.Count - 1) then
        begin
          for TimeIndex := 0 to DownstreamThicknessList.Count - 1 do
          begin
            Item := Boundary.ParamIcalc.GetItemByStartTime(
              DownstreamThicknessList.Times[TimeIndex]);
            if (Item <> nil) and ThicknessElevUsed then
            begin
              DownstreamValues := Boundary.DownstreamSegmentValues.
                GetBoundaryByStartTime(DownstreamThicknessList.Times[TimeIndex])
                as TSfrSegmentStorage;
              if DownstreamValues = nil then
              begin
                ErrorObject := Boundary.ScreenObject as TScreenObject;
                frmErrorsAndWarnings.AddError(Model,
                  StrInvalidStartingTim, ErrorObject.Name);
                Continue;
//                  raise EInvalidTime.Create(Boundary.ScreenObject);
              end;
              DownstreamRecord := DownstreamValues.SrfSegmentArray[0];
              DataArray := DownstreamThicknessList[TimeIndex]
                as TModflowBoundaryDisplayDataArray;
              DataArray.AddDataValue(DownstreamRecord.StreamBedThicknessAnnotation,
                DownstreamRecord.StreamBedThickness,
                Reach.Column, Reach.Row, Reach.Layer);
            end;
          end;
        end;
      end;
      if ISFROPT in [0,4,5] then
      begin
        if (ReachIndex = 0) then
        begin
          for TimeIndex := 0 to UpstreamElevationList.Count - 1 do
          begin
            Item := Boundary.ParamIcalc.GetItemByStartTime(
              UpstreamElevationList.Times[TimeIndex]);
            if (Item <> nil) and ThicknessElevUsed then
            begin
              UpstreamValues := Boundary.UpstreamSegmentValues.
                GetBoundaryByStartTime(UpstreamElevationList.Times[TimeIndex])
                as TSfrSegmentStorage;
              if UpstreamValues = nil then
              begin
                ErrorObject := Boundary.ScreenObject as TScreenObject;
                frmErrorsAndWarnings.AddError(Model,
                  StrInvalidStartingTim, ErrorObject.Name);
                Continue;
//                  raise EInvalidTime.Create(Boundary.ScreenObject);
              end;
              UpstreamRecord := UpstreamValues.SrfSegmentArray[0];
              DataArray := UpstreamElevationList[TimeIndex]
                as TModflowBoundaryDisplayDataArray;
              DataArray.AddDataValue(UpstreamRecord.StreambedElevationAnnotation,
                UpstreamRecord.StreambedElevation,
                Reach.Column, Reach.Row, Reach.Layer);
            end;
          end;
        end;
        if (ReachIndex = Segment.FReaches.Count - 1) then
        begin
          for TimeIndex := 0 to DownstreamElevationList.Count - 1 do
          begin
            Item := Boundary.ParamIcalc.GetItemByStartTime(
              DownstreamElevationList.Times[TimeIndex]);
            if (Item <> nil) and ThicknessElevUsed then
            begin
              DownstreamValues := Boundary.DownstreamSegmentValues.
                GetBoundaryByStartTime(DownstreamElevationList.Times[TimeIndex])
                as TSfrSegmentStorage;
              if DownstreamValues = nil then
              begin
                ErrorObject := Boundary.ScreenObject as TScreenObject;
                frmErrorsAndWarnings.AddError(Model,
                  StrInvalidStartingTim, ErrorObject.Name);
                Continue;
//                  raise EInvalidTime.Create(Boundary.ScreenObject);
              end;
              DownstreamRecord := DownstreamValues.SrfSegmentArray[0];
              DataArray := DownstreamElevationList[TimeIndex]
                as TModflowBoundaryDisplayDataArray;
              DataArray.AddDataValue(DownstreamRecord.StreambedElevationAnnotation,
                DownstreamRecord.StreambedElevation,
                Reach.Column, Reach.Row, Reach.Layer);
            end;
          end;
        end;
      end;
      if (ReachIndex = 0) then
      begin
        for TimeIndex := 0 to UpstreamDepthList.Count - 1 do
        begin
          Item := Boundary.ParamIcalc.GetItemByStartTime(
            UpstreamDepthList.Times[TimeIndex]);
          if (Item <> nil) and (Item.ICalc <= 0) then
          begin
            UpstreamValues := Boundary.UpstreamSegmentValues.
              GetBoundaryByStartTime(UpstreamDepthList.Times[TimeIndex])
              as TSfrSegmentStorage;
            if UpstreamValues = nil then
            begin
              ErrorObject := Boundary.ScreenObject as TScreenObject;
              frmErrorsAndWarnings.AddError(Model,
                StrInvalidStartingTim, ErrorObject.Name);
              Continue;
//                raise EInvalidTime.Create(Boundary.ScreenObject);
            end;
            UpstreamRecord := UpstreamValues.SrfSegmentArray[0];
            DataArray := UpstreamDepthList[TimeIndex]
              as TModflowBoundaryDisplayDataArray;
            DataArray.AddDataValue(UpstreamRecord.StreamDepthAnnotation,
              UpstreamRecord.StreamDepth,
              Reach.Column, Reach.Row, Reach.Layer);
          end;
        end;
      end;
      if (ReachIndex = Segment.FReaches.Count - 1) then
      begin
        for TimeIndex := 0 to DownstreamDepthList.Count - 1 do
        begin
          Item := Boundary.ParamIcalc.GetItemByStartTime(
            DownstreamDepthList.Times[TimeIndex]);
          if (Item <> nil) and (Item.ICalc <= 0) then
          begin
            DownstreamValues := Boundary.DownstreamSegmentValues.
              GetBoundaryByStartTime(DownstreamDepthList.Times[TimeIndex])
              as TSfrSegmentStorage;
            if DownstreamValues = nil then
            begin
              ErrorObject := Boundary.ScreenObject as TScreenObject;
              frmErrorsAndWarnings.AddError(Model,
                StrInvalidStartingTim, ErrorObject.Name);
              Continue;
//                raise EInvalidTime.Create(Boundary.ScreenObject);
            end;
            DownstreamRecord := DownstreamValues.SrfSegmentArray[0];
            DataArray := DownstreamDepthList[TimeIndex]
              as TModflowBoundaryDisplayDataArray;
            DataArray.AddDataValue(DownstreamRecord.StreamDepthAnnotation,
              DownstreamRecord.StreamDepth,
              Reach.Column, Reach.Row, Reach.Layer);
          end;
        end;
      end;
      if ISFROPT in [4,5] then
      begin
        if (ReachIndex = 0) then
        begin
          for TimeIndex := 0 to UpstreamUnSatWatContList.Count - 1 do
          begin
            Item := Boundary.ParamIcalc.GetItemByStartTime(
              UpstreamUnSatWatContList.Times[TimeIndex]);
            if (Item <> nil) and (Item.ICalc in [1,2]) then
            begin
              if Boundary.UpstreamUnsatSegmentValues.
                BoundaryCount = 0 then
              begin
                frmErrorsAndWarnings.AddError(Model, UnsatError,
                  Segment.FScreenObject.Name);
              end
              else
              begin
                UpstreamUnsatValues := Boundary.UpstreamUnsatSegmentValues.
                  Boundaries[0] as TSfrUnsatSegmentStorage;
                UpstreamUnsatRecord := UpstreamUnsatValues.SrfUnsatSegmentArray[0];
                DataArray := UpstreamUnSatWatContList[TimeIndex]
                  as TModflowBoundaryDisplayDataArray;
                DataArray.AddDataValue(UpstreamUnsatRecord.SaturatedWaterContentAnnotation,
                  UpstreamUnsatRecord.SaturatedWaterContent,
                  Reach.Column, Reach.Row, Reach.Layer);
              end;
            end;
          end;
        end;
        if (ReachIndex = Segment.FReaches.Count - 1) then
        begin
          for TimeIndex := 0 to DownstreamUnSatWatContList.Count - 1 do
          begin
            Item := Boundary.ParamIcalc.GetItemByStartTime(
              DownstreamUnSatWatContList.Times[TimeIndex]);
            if (Item <> nil) and (Item.ICalc in [1,2]) then
            begin
              if Boundary.DownstreamUnsatSegmentValues.
                BoundaryCount = 0 then
              begin
                frmErrorsAndWarnings.AddError(Model, UnsatError,
                  Segment.FScreenObject.Name);
              end
              else
              begin
                DownstreamUnsatValues := Boundary.DownstreamUnsatSegmentValues.
                  Boundaries[0] as TSfrUnsatSegmentStorage;
                DownstreamUnsatRecord := DownstreamUnsatValues.SrfUnsatSegmentArray[0];
                DataArray := DownstreamUnSatWatContList[TimeIndex]
                  as TModflowBoundaryDisplayDataArray;
                DataArray.AddDataValue(DownstreamUnsatRecord.SaturatedWaterContentAnnotation,
                  DownstreamUnsatRecord.SaturatedWaterContent,
                  Reach.Column, Reach.Row, Reach.Layer);
              end;
            end;
          end;
        end;
      end;
      if ISFROPT in [4,5] then
      begin
        if (ReachIndex = 0) then
        begin
          for TimeIndex := 0 to UpstreamUnSatInitWatContList.Count - 1 do
          begin
            Item := Boundary.ParamIcalc.GetItemByStartTime(
              UpstreamUnSatInitWatContList.Times[TimeIndex]);
            if (Item <> nil) and (Item.ICalc in [1,2]) then
            begin
              if Boundary.UpstreamUnsatSegmentValues.
                BoundaryCount = 0 then
              begin
                frmErrorsAndWarnings.AddError(Model, UnsatError,
                  Segment.FScreenObject.Name);
              end
              else
              begin
                UpstreamUnsatValues := Boundary.UpstreamUnsatSegmentValues.
                  Boundaries[0] as TSfrUnsatSegmentStorage;
                UpstreamUnsatRecord := UpstreamUnsatValues.SrfUnsatSegmentArray[0];
                DataArray := UpstreamUnSatInitWatContList[TimeIndex]
                  as TModflowBoundaryDisplayDataArray;
                DataArray.AddDataValue(UpstreamUnsatRecord.InitialWaterContentAnnotation,
                  UpstreamUnsatRecord.InitialWaterContent,
                  Reach.Column, Reach.Row, Reach.Layer);
              end;
            end;
          end;
        end;
        if (ReachIndex = Segment.FReaches.Count - 1) then
        begin
          for TimeIndex := 0 to DownstreamUnSatInitWatContList.Count - 1 do
          begin
            Item := Boundary.ParamIcalc.GetItemByStartTime(
              DownstreamUnSatInitWatContList.Times[TimeIndex]);
            if (Item <> nil) and (Item.ICalc in [1,2]) then
            begin
              if Boundary.DownstreamUnsatSegmentValues.
                BoundaryCount = 0 then
              begin
                frmErrorsAndWarnings.AddError(Model, UnsatError,
                  Segment.FScreenObject.Name);
              end
              else
              begin
                DownstreamUnsatValues := Boundary.DownstreamUnsatSegmentValues.
                  Boundaries[0] as TSfrUnsatSegmentStorage;
                DownstreamUnsatRecord := DownstreamUnsatValues.SrfUnsatSegmentArray[0];
                DataArray := DownstreamUnSatInitWatContList[TimeIndex]
                  as TModflowBoundaryDisplayDataArray;
                DataArray.AddDataValue(DownstreamUnsatRecord.InitialWaterContentAnnotation,
                  DownstreamUnsatRecord.InitialWaterContent,
                  Reach.Column, Reach.Row, Reach.Layer);
              end;
            end;
          end;
        end;
      end;
      if ISFROPT in [4,5] then
      begin
        if (ReachIndex = 0) then
        begin
          for TimeIndex := 0 to UpstreamBrooksCoreyList.Count - 1 do
          begin
            Item := Boundary.ParamIcalc.GetItemByStartTime(
              UpstreamBrooksCoreyList.Times[TimeIndex]);
            if (Item <> nil) and (Item.ICalc in [1,2]) then
            begin
              if Boundary.UpstreamUnsatSegmentValues.
                BoundaryCount = 0 then
              begin
                frmErrorsAndWarnings.AddError(Model, UnsatError,
                  Segment.FScreenObject.Name);
              end
              else
              begin
                UpstreamUnsatValues := Boundary.UpstreamUnsatSegmentValues.
                  Boundaries[0] as TSfrUnsatSegmentStorage;
                UpstreamUnsatRecord := UpstreamUnsatValues.SrfUnsatSegmentArray[0];
                DataArray := UpstreamBrooksCoreyList[TimeIndex]
                  as TModflowBoundaryDisplayDataArray;
                DataArray.AddDataValue(UpstreamUnsatRecord.BrooksCoreyExponentAnnotation,
                  UpstreamUnsatRecord.BrooksCoreyExponent,
                  Reach.Column, Reach.Row, Reach.Layer);
              end;
            end;
          end;
        end;
        if (ReachIndex = Segment.FReaches.Count - 1) then
        begin
          for TimeIndex := 0 to DownstreamBrooksCoreyList.Count - 1 do
          begin
            Item := Boundary.ParamIcalc.GetItemByStartTime(
              DownstreamBrooksCoreyList.Times[TimeIndex]);
            if (Item <> nil) and (Item.ICalc in [1,2]) then
            begin
              if Boundary.DownstreamUnsatSegmentValues.
                BoundaryCount = 0 then
              begin
                frmErrorsAndWarnings.AddError(Model, UnsatError,
                  Segment.FScreenObject.Name);
              end
              else
              begin
                DownstreamUnsatValues := Boundary.DownstreamUnsatSegmentValues.
                  Boundaries[0] as TSfrUnsatSegmentStorage;
                DownstreamUnsatRecord := DownstreamUnsatValues.SrfUnsatSegmentArray[0];
                DataArray := DownstreamBrooksCoreyList[TimeIndex]
                  as TModflowBoundaryDisplayDataArray;
                DataArray.AddDataValue(DownstreamUnsatRecord.BrooksCoreyExponentAnnotation,
                  DownstreamUnsatRecord.BrooksCoreyExponent,
                  Reach.Column, Reach.Row, Reach.Layer);
              end;
            end;
          end;
        end;
      end;
      if ISFROPT = 5 then
      begin
        if (ReachIndex = 0) then
        begin
          for TimeIndex := 0 to UpstreamUnSatKzList.Count - 1 do
          begin
            Item := Boundary.ParamIcalc.GetItemByStartTime(
              UpstreamUnSatKzList.Times[TimeIndex]);
            if (Item <> nil) and (Item.ICalc in [1,2]) then
            begin
              if Boundary.UpstreamUnsatSegmentValues.
                BoundaryCount = 0 then
              begin
                frmErrorsAndWarnings.AddError(Model, UnsatError,
                  Segment.FScreenObject.Name);
              end
              else
              begin
                UpstreamUnsatValues := Boundary.UpstreamUnsatSegmentValues.
                  Boundaries[0] as TSfrUnsatSegmentStorage;
                UpstreamUnsatRecord := UpstreamUnsatValues.SrfUnsatSegmentArray[0];
                DataArray := UpstreamUnSatKzList[TimeIndex]
                  as TModflowBoundaryDisplayDataArray;
                DataArray.AddDataValue(UpstreamUnsatRecord.VerticalSaturatedKAnnotation,
                  UpstreamUnsatRecord.VerticalSaturatedK,
                  Reach.Column, Reach.Row, Reach.Layer);
              end;
            end;
          end;
        end;
        if (ReachIndex = Segment.FReaches.Count - 1) then
        begin
          for TimeIndex := 0 to DownstreamUnSatKzList.Count - 1 do
          begin
            Item := Boundary.ParamIcalc.GetItemByStartTime(
              DownstreamUnSatKzList.Times[TimeIndex]);
            if (Item <> nil) and (Item.ICalc in [1,2]) then
            begin
              if Boundary.DownstreamUnsatSegmentValues.
                BoundaryCount = 0 then
              begin
                frmErrorsAndWarnings.AddError(Model, UnsatError,
                  Segment.FScreenObject.Name);
              end
              else
              begin
                DownstreamUnsatValues := Boundary.DownstreamUnsatSegmentValues.
                  Boundaries[0] as TSfrUnsatSegmentStorage;
                DownstreamUnsatRecord := DownstreamUnsatValues.SrfUnsatSegmentArray[0];
                DataArray := DownstreamUnSatKzList[TimeIndex]
                  as TModflowBoundaryDisplayDataArray;
                DataArray.AddDataValue(DownstreamUnsatRecord.VerticalSaturatedKAnnotation,
                  DownstreamUnsatRecord.VerticalSaturatedK,
                  Reach.Column, Reach.Row, Reach.Layer);
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  // Mark all the data arrays and time lists as up to date.
  for TimeListIndex := 0 to TimeLists.Count - 1 do
  begin
    DisplayTimeList := TimeLists[TimeListIndex];
    for TimeIndex := 0 to DisplayTimeList.Count - 1 do
    begin
      DataArray := DisplayTimeList[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      DataArray.UpToDate := True;
    end;
    DisplayTimeList.SetUpToDate(True);
  end;
end;

procedure TModflowSFR_Writer.TestBedElevations;
var
  DownstreamElev: Double;
  UpstreamElev: Double;
  WriteValue: Boolean;
  DownstreamValues: TSfrSegmentStorage;
  UpstreamValues: TSfrSegmentStorage;
  ParamIcalcItem: TSfrParamIcalcItem;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  PriorDownstreamValues: TSfrSegmentStorage;
  PriorUpstreamValues: TSfrSegmentStorage;
  Index: Integer;
  Segment: TSegment;
  Boundary: TSfrBoundary;
  ErrorObject: TScreenObject;
begin
  for Index := 0 to FSegments.Count - 1 do
  begin
    Segment := FSegments[Index];
    Boundary := Segment.FScreenObject.ModflowSfrBoundary;
    PriorUpstreamValues := nil;
    PriorDownstreamValues := nil;
    for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
    begin
      StressPeriod := Model.ModflowFullStressPeriods[TimeIndex];
      ParamIcalcItem := Boundary.ParamIcalc.GetItemByStartTime(StressPeriod.StartTime);
      UpstreamValues := Boundary.UpstreamSegmentValues.
        GetBoundaryByStartTime(StressPeriod.StartTime) as TSfrSegmentStorage;
      if UpstreamValues = nil then
      begin
        ErrorObject := Boundary.ScreenObject as TScreenObject;
        frmErrorsAndWarnings.AddError(Model,
          StrInvalidStartingTim, ErrorObject.Name);
        Continue;
//        raise EInvalidTime.Create(Boundary.ScreenObject);
      end;
      DownstreamValues := Boundary.DownstreamSegmentValues.
        GetBoundaryByStartTime(StressPeriod.StartTime) as TSfrSegmentStorage;
      if DownstreamValues = nil then
      begin
        ErrorObject := Boundary.ScreenObject as TScreenObject;
        frmErrorsAndWarnings.AddError(Model,
          StrInvalidStartingTim, ErrorObject.Name);
        Continue;
//        raise EInvalidTime.Create(Boundary.ScreenObject);
      end;
//      Assert(UpstreamValues <> nil);
//      Assert(DownstreamValues <> nil);
      if (UpstreamValues <> PriorUpstreamValues)
        or (DownstreamValues <> PriorDownstreamValues) then
      begin
        WriteValue := ISFROPT in [0, 4, 5];
        if (ISFROPT in [4, 5]) and (ParamIcalcItem.ICalc in [1, 2]) then
        begin
          WriteValue := TimeIndex = 0;
        end;
        if WriteValue then
        begin
          UpstreamElev := UpstreamValues.SrfSegmentArray[0].StreambedElevation;
          DownstreamElev := DownstreamValues.SrfSegmentArray[0].StreambedElevation;
          if UpstreamElev <= DownstreamElev then
          begin
            frmErrorsAndWarnings.AddError(Model,
              StrOneOrMoreSFRStre,
              'Object: ' + Segment.FScreenObject.Name
              + '; Time: ' + FloatToStr(StressPeriod.StartTime)
              + '; Upstream elevation: ' + FloatToStr(UpstreamElev)
              + '; Downstream elevation: ' + FloatToStr(DownstreamElev));
          end;
        end;
      end;
      PriorUpstreamValues := UpstreamValues;
      PriorDownstreamValues := DownstreamValues;
    end;
  end;
end;

procedure TModflowSFR_Writer.WriteDataSet1;
var
  NSTRM: integer;
  Index: Integer;
  Segment: TSegment;
  NSS: integer;
  SfrPackage: TSfrPackageSelection;
  LocalModel: TCustomModel;
  NPARSEG: integer;
  sfrCONST: double;
  DLEAK: double;
  ISTCB1: integer;
  ISTCB2: integer;
  NSTRAIL: integer;
  ISUZN: integer;
  NSFRSETS: integer;
  IRTFLG: integer;
  NUMTIM: integer;
  FLWTOL: double;
  WEIGHT: double;
  ParameterNames: TStringList;
  NameIndex: Integer;
  Params: TSfrParamIcalcCollection;
  Item: TSfrParamIcalcItem;
  InstanceItem: TSfrParamInstance;
  ParamIndex: Integer;
  FlowFileName: string;
begin
  NSTRM := 0;
  for Index := 0 to FSegments.Count - 1 do
  begin
    Segment := FSegments[Index];
    NSTRM := NSTRM + Segment.FReaches.Count;
  end;

  NSS := FSegments.Count;

  LocalModel := Model as TCustomModel;
  NSFRPAR := LocalModel.ModflowTransientParameters.CountParam(ptSFR);

  SfrPackage := Package as TSfrPackageSelection;
  NPARSEG := 0;
  for Index := 0 to FSegments.Count - 1 do
  begin
    Segment := FSegments[Index];
    Assert(Segment.FScreenObject.ModflowSfrBoundary <> nil);
    Params := Segment.FScreenObject.ModflowSfrBoundary.ParamIcalc;
    ParameterNames := TStringList.Create;
    try
      for NameIndex := 0 to Params.Count - 1 do
      begin
        Item := Params.Items[NameIndex];
        if (Item.Param <> '') and
          (ParameterNames.IndexOf(Item.Param) < 0) then
        begin
          ParameterNames.Add(Item.Param);
        end;
      end;
      for ParamIndex := 0 to SfrPackage.ParameterInstances.Count - 1 do
      begin
        InstanceItem := SfrPackage.ParameterInstances.Items[ParamIndex];
        if ParameterNames.IndexOf(InstanceItem.ParameterName) >= 0 then
        begin
          Inc(NPARSEG);
        end;
      end;
    finally
      ParameterNames.Free;
    end;
  end;



  sfrCONST := SfrPackage.StreamConstant;

  DLEAK := SfrPackage.Dleak;

  GetFlowUnitNumber(ISTCB1);

  // ISTCB2 < 0 is not supported by ModelMuse.
//  if SfrPackage.PrintStreams then
//  begin
//    ISTCB2 := PhastModel.UnitNumbers.UnitNumber(StrLIST);
//  end
//  else
//  begin
//    ISTCB2 := 0;
//  end;
  ISTCB2 := 0;
  case SfrPackage.PrintFlows of
    pfNoPrint: ISTCB2 := 0;
    pfListing: ISTCB2 := Model.UnitNumbers.UnitNumber(StrLIST);
    pfText:
      begin
        ISTCB2 := Model.UnitNumbers.UnitNumber(StrISTCB2);
        FlowFileName := ChangeFileExt(FNameOfFile, '.sft_out');
        WriteToNameFile('DATA', ISTCB2, FlowFileName, foOutput);
      end;
    else Assert(False);
  end;

  if SfrPackage.KinematicRouting then
  begin
    IRTFLG := 1;
  end
  else
  begin
    IRTFLG := 0;
  end;

  if (ISFROPT > 0) then
  begin
    NSTRM := -NSTRM;
  end;

  NSTRAIL := SfrPackage.Nstrail;

  ISUZN := SfrPackage.Isuzn;

  NSFRSETS := SfrPackage.Nsfrsets;

  NUMTIM := SfrPackage.TimeStepsForKinematicRouting;
  WEIGHT := SfrPackage.KinematicRoutingWeight;
  FLWTOL := SfrPackage.KinematicRoutingTolerance;

  WriteInteger(NSTRM);
  WriteInteger(NSS);
  WriteInteger(NSFRPAR);
  WriteInteger(NPARSEG);
  WriteFloat(sfrCONST);
  WriteFloat(DLEAK);
  WriteInteger(ISTCB1);
  WriteInteger(ISTCB2);
  if NSTRM < 0 then
  begin
    WriteInteger(ISFROPT);
  end;
  if ISFROPT > 1 then
  begin
    WriteInteger(NSTRAIL);
    WriteInteger(ISUZN);
    WriteInteger(NSFRSETS);
  end;
//  if NSTRM < 0 then
  begin
    WriteInteger(IRTFLG);
    if IRTFLG > 0 then
    begin
      WriteInteger(NUMTIM);
      WriteFloat(WEIGHT);
      WriteFloat(FLWTOL);
    end;
  end;

  WriteString(' # Data Set 1: NSTRM NSS NSFRPAR NPARSEG CONST DLEAK ISTCB1  ISTCB2');
  if NSTRM < 0 then
  begin
    WriteString(' ISFROPT');
  end;
  if ISFROPT > 0 then
  begin
    WriteString(' NSTRAIL ISUZN NSFRSETS');
  end;
//  if NSTRM < 0 then
  begin
    WriteString(' IRTFLG');
    if IRTFLG > 0 then
    begin
      WriteString(' NUMTIM WEIGHT FLWTOL');
    end;
  end;
  NewLine;
end;

procedure TModflowSFR_Writer.WriteDataSet2;
const
  WarningRoot = 'SFR reach length (RCHLEN) <= 0';
var
  Index: integer;
  Segment: TSegment;
  Reach: TSfr_Cell;
  ReachIndex: integer;
  LocalLayer: integer;
  ObjectName: string;
begin
  for Index := 0 to FSegments.Count - 1 do
  begin
    Segment := FSegments[Index];
    ObjectName := Segment.FScreenObject.Name;
    Assert(Segment.NewSegmentNumber = Index + 1);
    for ReachIndex := 0 to Segment.FReaches.Count - 1 do
    begin
      Reach := Segment.FReaches[ReachIndex] as TSfr_Cell;
      CheckCell(Reach, 'SFR');
      LocalLayer := Model.
        DataSetLayerToModflowLayer(Reach.Layer);
      WriteInteger(LocalLayer);
      WriteInteger(Reach.Row+1);
      WriteInteger(Reach.Column+1);
      WriteInteger(Segment.NewSegmentNumber);
      WriteInteger(ReachIndex+1);
      WriteFloat(Reach.ReachLength);
      if ISFROPT in [1,2,3] then
      begin
        WriteFloat(Reach.StreambedElevation);
        WriteFloat(Reach.StreamSlope);
        WriteFloat(Reach.StreamBedThickness);
        WriteFloat(Reach.HydraulicConductivity);
      end;
      if ISFROPT in [2,3] then
      begin
        WriteFloat(Reach.SaturatedWaterContent);
        WriteFloat(Reach.InitialWaterContent);
        WriteFloat(Reach.BrooksCoreyExponent);
      end;
      if ISFROPT = 3 then
      begin
        WriteFloat(Reach.VerticalK);
      end;
      WriteString(' # Data Set 2: KRCH IRCH JRCH ISEG IREACH RCHLEN');
      if ISFROPT in [1,2,3] then
      begin
        WriteString(' STRTOP SLOPE STRTHICK STRHC1');
      end;
      if ISFROPT in [2,3] then
      begin
        WriteString(' THTS THTI EPS');
      end;
      if ISFROPT = 3 then
      begin
        WriteString(' UHC');
      end;
      if ReachIndex = 0 then
      begin
        WriteString(' Defined by object: ' + ObjectName);
      end;
      
      NewLine;

      if Reach.ReachLength <= 0 then
      begin
        frmErrorsAndWarnings.AddWarning(Model, WarningRoot,
          'Object = ' + ObjectName + '; '
          + 'Layer = ' + IntToStr(Reach.Layer+1) + '; '
          + 'Row = ' + IntToStr(Reach.Row+1) + '; '
          + 'Column = ' + IntToStr(Reach.Column+1));
      end;
    end;
  end;
end;

procedure TModflowSFR_Writer.WriteDataSets3and4;
var
  ParamIndex: integer;
  SfrPackage: TSfrPackageSelection;
  LocalModel: TCustomModel;
  ParamItem: TModflowTransientListParameter;
  Instances: TList;
  InstanceItem: TSfrParamInstance;
  ScreenObject: TScreenObject;
  Segments: TList;
  ScreenObjectParamIndex: Integer;
  ParamScreenObjectItem: TSfrParamIcalcItem;
  Index: Integer;
  InstanceIndex: Integer;
  SfrBoundary: TSfrBoundary;
  Segment: TSegment;
begin
  SfrPackage := Package as TSfrPackageSelection;
  LocalModel := Model as TCustomModel;
  for ParamIndex := 0 to LocalModel.ModflowTransientParameters.Count - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    ParamItem := LocalModel.ModflowTransientParameters.Items[ParamIndex];
    if ParamItem.ParameterType = ptSFR then
    begin
      Instances := TList.Create;
      Segments := TList.Create;
      try
        for Index := 0 to SfrPackage.ParameterInstances.Count - 1 do
        begin
          Application.ProcessMessages;
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
          InstanceItem := SfrPackage.ParameterInstances.Items[Index];
          if InstanceItem.ParameterName = ParamItem.ParameterName then
          begin
            Instances.Add(InstanceItem);
          end;
        end;
        for Index := 0 to FSegments.Count - 1 do
        begin
          Application.ProcessMessages;
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
          Segment := FSegments[Index];
          ScreenObject := Segment.FScreenObject;
          Assert(ScreenObject.ModflowSfrBoundary <> nil);
          for ScreenObjectParamIndex := 0 to ScreenObject.ModflowSfrBoundary.ParamIcalc.Count - 1 do
          begin
            Application.ProcessMessages;
            if not frmProgressMM.ShouldContinue then
            begin
              Exit;
            end;
            ParamScreenObjectItem := ScreenObject.ModflowSfrBoundary.ParamIcalc.Items[ScreenObjectParamIndex];
            if ParamScreenObjectItem.Param = ParamItem.ParameterName then
            begin
              Segments.Add(Segment);
              break;
            end;
          end;
        end;

        // Data set 3
        frmProgressMM.AddMessage('    Writing parameter '
          + ParamItem.ParameterName);
        WriteString(ParamItem.ParameterName + ' SFR');
        WriteFloat(ParamItem.Value);
        WriteInteger(Segments.Count);
        if Instances.Count > 1 then
        begin
          WriteString(' INSTANCES');
          WriteInteger(Instances.Count);
        end;
        WriteString(' #  Data Set 3: PARNAM PARTYP Parval NLST');
        if Instances.Count > 1 then
        begin
          WriteString(' INSTANCES NUMINST');
        end;
        NewLine;

        Model.WritePValAndTemplate(ParamItem.ParameterName,
          ParamItem.Value);

        // Data set 4a
        for InstanceIndex := 0 to Instances.Count - 1 do
        begin
          Application.ProcessMessages;
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
          InstanceItem := Instances[InstanceIndex];
          if Instances.Count > 1 then
          begin
            WriteString(InstanceItem.ParameterInstance);
            WriteString(' # Data Set 4a: INSTNAM');
            NewLine;
          end;

          for Index := 0 to Segments.Count - 1 do
          begin
            Application.ProcessMessages;
            if not frmProgressMM.ShouldContinue then
            begin
              Exit;
            end;
            Segment := Segments[Index];
            ScreenObject := Segment.FScreenObject;
            Assert(ScreenObject.ModflowSfrBoundary <> nil);
            SfrBoundary := ScreenObject.ModflowSfrBoundary;
            for ScreenObjectParamIndex := 0 to SfrBoundary.ParamIcalc.Count - 1 do
            begin
              Application.ProcessMessages;
              if not frmProgressMM.ShouldContinue then
              begin
                Exit;
              end;
              ParamScreenObjectItem := SfrBoundary.
                ParamIcalc.Items[ScreenObjectParamIndex];
              if (ParamScreenObjectItem.Param = ParamItem.ParameterName)
                and (ParamScreenObjectItem.ParamInstance
                = InstanceItem.ParameterInstance) then
              begin
                // data set 4b
                WriteDataSet4b6a(InstanceItem.StartTime, Segment,
                  ParamScreenObjectItem, SfrBoundary, True);
                Application.ProcessMessages;
                if not frmProgressMM.ShouldContinue then
                begin
                  Exit;
                end;

                // data set 4c
                WriteDataSet4c6b(True, SfrBoundary,
                  ParamScreenObjectItem, InstanceItem.StartTime, 0);
                Application.ProcessMessages;
                if not frmProgressMM.ShouldContinue then
                begin
                  Exit;
                end;

                // data set 4d
                WriteDataSet4d6c(True, SfrBoundary,
                  ParamScreenObjectItem, InstanceItem.StartTime, 0);
                Application.ProcessMessages;
                if not frmProgressMM.ShouldContinue then
                begin
                  Exit;
                end;

                // data set 4e
                WriteDataSet4e6d(True, SfrBoundary, ParamScreenObjectItem, 1);
                Application.ProcessMessages;
                if not frmProgressMM.ShouldContinue then
                begin
                  Exit;
                end;

                // data set 4f
                WriteDataSet4f6e(True, SfrBoundary, ParamScreenObjectItem,
                  InstanceItem.StartTime);
                Application.ProcessMessages;
                if not frmProgressMM.ShouldContinue then
                begin
                  Exit;
                end;

                // data set 4g
              end;
            end;
          end;
        end;

      finally
        Instances.Free;
        Segments.Free;
      end;
    end;
  end;
end;

procedure TModflowSFR_Writer.WriteDataSets5to7;
var
  TimeIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  Segment: TSegment;
  SegementIndex: Integer;
  Item: TSfrParamIcalcItem;
  UsedSegments: TList;
  ITMP: Integer;
  IRDFLG: Integer;
  IPTFLG: Integer;
  ParametersUsed: TStringList;
  NP: Integer;
  Boundary: TSfrBoundary;
  ParamIndex: Integer;
  PIndex: Integer;
  Instance: TSfrParamInstance;
  Parameters: TStringList;
  Location: integer;
  InstanceList: TList;
  InstanceIndex: Integer;
  ParamName: string;
begin
  UsedSegments := TList.Create;
  ParametersUsed := TStringList.Create;
  Parameters := TStringList.Create;
  try
    for PIndex := 0 to Model.ModflowPackages.
      SfrPackage.ParameterInstances.Count - 1 do
    begin
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      Instance := Model.ModflowPackages.
        SfrPackage.ParameterInstances.Items[PIndex];
      Location := Parameters.IndexOf(Instance.ParameterName);
      if Location < 0 then
      begin
        InstanceList := TList.Create;
        Parameters.AddObject(Instance.ParameterName, InstanceList)
      end
      else
      begin
        InstanceList := Parameters.Objects[Location] as TList;
      end;
      InstanceList.Add(Instance);
    end;


    for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
    begin
      frmProgressMM.AddMessage('    Writing stress period '
        + IntToStr(TimeIndex + 1));
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      // data set 5;
      UsedSegments.Clear;
      ParametersUsed.Clear;
      StressPeriod := Model.ModflowFullStressPeriods[TimeIndex];
      for SegementIndex := 0 to FSegments.Count - 1 do
      begin
        Segment := FSegments[SegementIndex];
        Assert(Segment.FScreenObject.ModflowSfrBoundary <> nil);
        Item := Segment.FScreenObject.ModflowSfrBoundary.ParamIcalc.
          GetItemByStartTime(StressPeriod.StartTime);
        if (Item = nil) or (Item.Param = '') then
        begin
          UsedSegments.Add(Segment);
        end
        else
        begin
          if ParametersUsed.IndexOf(Item.Param) < 0 then
          begin
            ParametersUsed.Add(Item.Param);
          end;
        end;
      end;

      ITMP := UsedSegments.Count;

      if Model.ModflowOutputControl.PrintInputCellLists then
      begin
        IRDFLG := 0;
      end
      else
      begin
        IRDFLG := 1;
      end;

      IPTFLG := 0;

      NP := ParametersUsed.Count;

      WriteInteger(ITMP);
      WriteInteger(IRDFLG);
      WriteInteger(IPTFLG);
      if NSFRPAR > 0 then
      begin
        WriteInteger(NP);
      end
      else
      begin
        Assert(NP = 0);
      end;
      WriteString(' # Data Set 5, Stress period ' + IntToStr(TimeIndex+1) + ': ITMP IRDFLG IPTFLG');
      if NSFRPAR > 0 then
      begin
        WriteString(' NP');
      end;
      NewLine;

      // Data Set 6
      for SegementIndex := 0 to UsedSegments.Count - 1 do
      begin
        // Data set 6a
        Segment := UsedSegments[SegementIndex];
        Assert(Segment.FScreenObject.ModflowSfrBoundary <> nil);
        Item := Segment.FScreenObject.ModflowSfrBoundary.ParamIcalc.
          GetItemByStartTime(StressPeriod.StartTime);
        if Item <> nil then
        begin
          
          Boundary := Segment.FScreenObject.ModflowSfrBoundary;
          WriteDataSet4b6a(StressPeriod.StartTime, Segment, Item,
            Boundary, False);

          // Data set 6b
          WriteDataSet4c6b(false, Boundary, Item, StressPeriod.StartTime,
            TimeIndex);

          // data set 6c
          WriteDataSet4d6c(false, Boundary, Item,
            StressPeriod.StartTime, TimeIndex);

          // data set 6d
          WriteDataSet4e6d(false, Boundary, Item, TimeIndex+1);

          // data set 6e
          WriteDataSet4f6e(false, Boundary, Item, StressPeriod.StartTime);

          // data set 6f
        end;
      end;

      // data set 7
      for ParamIndex := 0 to ParametersUsed.Count - 1 do
      begin
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        ParamName := ParametersUsed[ParamIndex];
        Location := Parameters.IndexOf(ParamName);
        Assert(Location >= 0);
        InstanceList := Parameters.Objects[Location] as TList;
        if InstanceList.Count > 1 then
        begin
          for InstanceIndex := 0 to InstanceList.Count - 1 do
          begin
            Application.ProcessMessages;
            if not frmProgressMM.ShouldContinue then
            begin
              Exit;
            end;
            Instance := InstanceList[InstanceIndex];
            if (Instance.StartTime >= StressPeriod.StartTime)
              and (Instance.StartTime < StressPeriod.EndTime) then
            begin
              WriteString(ParamName + ' ' + Instance.ParameterInstance);
              break;
            end;
            Assert(InstanceIndex < InstanceList.Count - 1);
          end;
        end
        else
        begin
          WriteString(ParamName);
        end;
        NewLine;
      end;
    end;
  finally
    UsedSegments.Free;
    ParametersUsed.Free;
    for PIndex := 0 to Parameters.Count - 1 do
    begin
      Parameters.Objects[PIndex].Free;
    end;
    Parameters.Free;
  end;
end;

procedure TModflowSFR_Writer.WriteFile(const AFileName: string;
  var StartUnitNumber: integer; Lines: TStrings);
begin
//  OutputDebugString('SAMPLING ON') ;
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrSFR) then
  begin
    Exit;
  end;
  FNameOfFile := FileName(AFileName);
  WriteToNameFile(StrSFR, Model.UnitNumbers.UnitNumber(StrSFR), FNameOfFile, foInput);
  Evaluate;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  OpenFile(FileName(AFileName));
  try
    frmProgressMM.AddMessage('Writing SFR Package input.');
    frmProgressMM.AddMessage('  Writing Data Set 0.');
    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Set 1.');
    WriteDataSet1;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Set 2.');
    WriteDataSet2;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Sets 3 and 4.');
    WriteDataSets3and4;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Sets 5 to 7.');
    WriteDataSets5to7;
  finally
    CloseFile;
  end;
  WriteGages(StartUnitNumber, Lines);
end;

procedure TModflowSFR_Writer.WriteGages(var StartUnitNumber: integer;
  Lines: TStrings);
var
  SegmentIndex: Integer;
  Segment: TSegment;
  Boundary: TSfrBoundary;
  OutTypes: TByteSet;
  OutIndex: Byte;
  GAGESEG: integer;
  GAGERCH: integer;
  UNIT_Number: integer;
  OUTTYPE: integer;
  ReachIndex: integer;
  procedure WriteGage;
  var
    Line: string;
    OutputName: string;
  begin
    UNIT_Number := StartUnitNumber;
    Line := IntToStr(GAGESEG) + ' '
      + IntToStr(GAGERCH) + ' '
      + IntToStr(UNIT_Number) + ' '
      + IntToStr(OUTTYPE);
    Lines.Add(Line);
    Inc(StartUnitNumber);
    OutputName := ChangeFileExt(FNameOfFile, '.sfrg');
    OutputName := OutputName + IntToStr(Lines.Count);
    WriteToNameFile(StrDATA, UNIT_Number, OutputName, foOutput);
  end;
begin
  for SegmentIndex := 0 to FSegments.Count - 1 do
  begin
    Segment := FSegments[SegmentIndex];
    Boundary := Segment.FScreenObject.ModflowSfrBoundary;
    Assert((Boundary <> nil) and Boundary.Used);
    OutTypes := Boundary.OutTypes;
    if (OutTypes <> []) and (Boundary.GageLocation <> glNone) then
    begin
      GAGESEG := Segment.NewSegmentNumber;
      for OutIndex := 0 to 7 do
      begin
        if OutIndex in OutTypes then
        begin
          OUTTYPE := OutIndex;
          case Boundary.GageLocation of
            glFirst:
              begin
                GAGERCH := 1;
                WriteGage;
              end;
            glLast:
              begin
                GAGERCH := Segment.FReaches.Count;
                WriteGage;
              end;
            glAll:
              begin
                for ReachIndex := 1 to Segment.FReaches.Count  do
                begin
                  GAGERCH := ReachIndex;
                  WriteGage;
                end;
              end;
            else
              Assert(False);
          end;
        end;
        
      end;
    end;
  end;
end;

procedure TModflowSFR_Writer.WriteDataSet4f6e(Parameter: Boolean;
  SfrBoundary: TSfrBoundary; ParamScreenObjectItem: TSfrParamIcalcItem;
  StartTime: double);
var
  TableRow: TSfrFlowTableItemRecord;
  FlowTableIndex: Integer;
  FlowTable: TSfrFlowTableRecord;
begin
  if ParamScreenObjectItem.ICalc = 4 then
  begin
    FlowTable := SfrBoundary.TableCollection.
      GetRecordForTime(StartTime);
    for FlowTableIndex := 0 to Length(FlowTable.SfrFlowTableArray) - 1 do
    begin
      TableRow := FlowTable.SfrFlowTableArray[FlowTableIndex];
      WriteFloat(TableRow.Flow);
    end;
    if Parameter then
    begin
      WriteString(' # Data set 4f: FLOWTAB(1) FLOWTAB(2) ... FLOWTAB(NSTRPTS)');
    end
    else
    begin
      WriteString(' # Data set 6e: FLOWTAB(1) FLOWTAB(2) ... FLOWTAB(NSTRPTS)');
    end;
    NewLine;
    for FlowTableIndex := 0 to Length(FlowTable.SfrFlowTableArray) - 1 do
    begin
      TableRow := FlowTable.SfrFlowTableArray[FlowTableIndex];
      WriteFloat(TableRow.Depth);
    end;
    if Parameter then
    begin
      WriteString(' # Data set 4f: DPTHTAB(1) DPTHTAB(2) ... DPTHTAB(NSTRPTS)');
    end
    else
    begin
      WriteString(' # Data set 6e: DPTHTAB(1) DPTHTAB(2) ... DPTHTAB(NSTRPTS)');
    end;
    NewLine;
    for FlowTableIndex := 0 to Length(FlowTable.SfrFlowTableArray) - 1 do
    begin
      TableRow := FlowTable.SfrFlowTableArray[FlowTableIndex];
      WriteFloat(TableRow.Width);
    end;
    if Parameter then
    begin
      WriteString(' # Data set 4f: WDTHTAB(1) WDTHTAB(2) ... WDTHTAB(NSTRPTS)');
    end
    else
    begin
      WriteString(' # Data set 6e: WDTHTAB(1) WDTHTAB(2) ... WDTHTAB(NSTRPTS)');
    end;
    NewLine;
  end;
end;

procedure TModflowSFR_Writer.WriteDataSet4e6d(Parameter: Boolean;
  SfrBoundary: TSfrBoundary; ParamScreenObjectItem: TSfrParamIcalcItem;
  StressPeriod: integer);
var
  CrossSectionIndex: Integer;
  CrossSection: TSfrChannelRecord;
begin
  if (ParamScreenObjectItem.ICalc = 2) and
    ((ISFROPT <= 1)  or (StressPeriod = 1) or Parameter) then
  begin
    CrossSection := SfrBoundary.ChannelValues.GetChannelTimeValuesFromTime(
      Model, ParamScreenObjectItem.StartTime);
    for CrossSectionIndex := 0 to 7 do
    begin
      WriteFloat(CrossSection.X[CrossSectionIndex]);
    end;
    if Parameter then
    begin
      WriteString(' # Data set 4e: XCPT1 XCPT2 ... XCPT8');
    end
    else
    begin
      WriteString(' # Data set 6d: XCPT1 XCPT2 ... XCPT8');
    end;
    NewLine;

    for CrossSectionIndex := 0 to 7 do
    begin
      WriteFloat(CrossSection.Z[CrossSectionIndex]);
    end;
    if Parameter then
    begin
      WriteString(' # Data set 4e: ZCPT1 ZCPT2 ... ZCPT8');
    end
    else
    begin
      WriteString(' # Data set 6d: ZCPT1 ZCPT2 ... ZCPT8');
    end;
    NewLine;
  end;
end;

procedure TModflowSFR_Writer.WriteUnsatSegmentValues(upstream: Boolean;
  var CommentLine: string; var ValuesWriten: boolean;
  UnsatUpstreamValues: TSfrUnsatSegmentStorage);
var
  UnsatSegmentValues: TSfrUnsatSegmentRecord;
begin
  ValuesWriten := True;
  Assert(Length(UnsatUpstreamValues.SrfUnsatSegmentArray) = 1);
  UnsatSegmentValues := UnsatUpstreamValues.SrfUnsatSegmentArray[0];
  // THTS1, THTS2
  WriteFloat(UnsatSegmentValues.SaturatedWaterContent);
  if upstream then
  begin
    CommentLine := CommentLine + ' THTS1';
  end
  else
  begin
    CommentLine := CommentLine + ' THTS2';
  end;
  // THTI1, THTI2
  WriteFloat(UnsatSegmentValues.InitialWaterContent);
  if upstream then
  begin
    CommentLine := CommentLine + ' THTI1';
  end
  else
  begin
    CommentLine := CommentLine + ' THTI2';
  end;
  // EPS1, EPS2
  WriteFloat(UnsatSegmentValues.BrooksCoreyExponent);
  if upstream then
  begin
    CommentLine := CommentLine + ' EPS1';
  end
  else
  begin
    CommentLine := CommentLine + ' EPS2';
  end;
  // UHC1, UHC2
  if ISFROPT = 5 then
  begin
    WriteFloat(UnsatSegmentValues.VerticalSaturatedK);
    if upstream then
    begin
      CommentLine := CommentLine + ' UHC1';
    end
    else
    begin
      CommentLine := CommentLine + ' UHC2';
    end;
  end;
end;

procedure TModflowSFR_Writer.WriteSegmentValues(StressPeriodIndex: Integer;
  Parameter: Boolean; UpOrDownStreamValues: TSfrSegmentStorage; upstream: Boolean;
  var CommentLine: string; var ValuesWriten: boolean;
  ParamScreenObjectItem: TSfrParamIcalcItem);
var
  SegmentValues: TSfrSegmentRecord;
  WriteValue: Boolean;
begin
  ValuesWriten := False;
  Assert(Length(UpOrDownStreamValues.SrfSegmentArray) >= 1);
  SegmentValues := UpOrDownStreamValues.SrfSegmentArray[0];
  // Hc1fact, Hc2fact, HCOND1, HCOND2
  if ISFROPT in [0, 4, 5] then
  begin
    ValuesWriten := True;
    WriteFloat(SegmentValues.HydraulicConductivity);
    if Parameter then
    begin
      if upstream then
      begin
        CommentLine := CommentLine + ' Hc1fact';
      end
      else
      begin
        CommentLine := CommentLine + ' Hc2fact';
      end;
    end
    else
    begin
      if upstream then
      begin
        CommentLine := CommentLine + ' HCOND1';
      end
      else
      begin
        CommentLine := CommentLine + ' HCOND2';
      end;
    end;
  end;
  // THICKM1, THICKM2
  WriteValue := ISFROPT in [0, 4, 5];
  if (ISFROPT in [4, 5]) and (ParamScreenObjectItem.ICalc in [1, 2]) then
  begin
    WriteValue := StressPeriodIndex = 0;
  end;
  if WriteValue then
  begin
    ValuesWriten := True;
    WriteFloat(SegmentValues.StreamBedThickness);
    if upstream then
    begin
      CommentLine := CommentLine + ' THICKM1';
    end
    else
    begin
      CommentLine := CommentLine + ' THICKM2';
    end;
  end;
  // ELEVUP, ELEVDN
  if WriteValue then
  begin
    ValuesWriten := True;
    WriteFloat(SegmentValues.StreambedElevation);
    if upstream then
    begin
      CommentLine := CommentLine + ' ELEVUP';
    end
    else
    begin
      CommentLine := CommentLine + ' ELEVDN';
    end;
  end;
  // WIDTH1, WIDTH2
  WriteValue := ParamScreenObjectItem.ICalc <= 1;
  if WriteValue and (ISFROPT > 1) and (ParamScreenObjectItem.ICalc = 1) then
  begin
    WriteValue := StressPeriodIndex = 0;
  end;
  if WriteValue then
  begin
    ValuesWriten := True;
    WriteFloat(SegmentValues.StreamWidth);
    if upstream then
    begin
      CommentLine := CommentLine + ' WIDTH1';
    end
    else
    begin
      CommentLine := CommentLine + ' WIDTH2';
    end;
  end;
  // DEPTH1, DEPTH2
  if ParamScreenObjectItem.ICalc = 0 then
  begin
    ValuesWriten := True;
    WriteFloat(SegmentValues.StreamDepth);
    if upstream then
    begin
      CommentLine := CommentLine + ' DEPTH1';
    end
    else
    begin
      CommentLine := CommentLine + ' DEPTH2';
    end;
  end;
end;

function TModflowSFR_Writer.FindConvertedSegment(OriginalSegmentNumber: integer): integer;
var
  Index: Integer;
  Segment: TSegment;
  AScreenObject: TScreenObject;
begin
  result := 0;
  if OriginalSegmentNumber = 0 then
  begin
    Exit;
  end
  else
  begin
    if OriginalSegmentNumber > 0 then
    begin
      for Index := 0 to FSegments.Count - 1 do
      begin
        Segment := FSegments[Index];
        if Segment.OriginalSegmentNumber = OriginalSegmentNumber then
        begin
          result := Segment.NewSegmentNumber;
          Exit;
        end;
      end;
    end
    else
    begin
      if Model.ModflowPackages.LakPackage.IsSelected then
      begin
        // If TModflowSFR_Writer becomes persistant,
        // there will have to be a better test for determining
        // when the lake list needs to be filled.
        if FLakes = nil then
        begin
          FLakes := TList.Create;
          for Index := 0 to Model.ScreenObjectCount - 1 do
          begin
            AScreenObject := Model.ScreenObjects[Index];
            if (AScreenObject.ModflowLakBoundary <> nil)
              and AScreenObject.ModflowLakBoundary.Used then
            begin
              FLakes.Add(AScreenObject);
            end;
          end;
        end;
        OriginalSegmentNumber := -OriginalSegmentNumber;
        for Index := 0 to FLakes.Count - 1 do
        begin
          AScreenObject := FLakes[Index];
          Assert(AScreenObject.ModflowLakBoundary <> nil);
          if AScreenObject.ModflowLakBoundary.LakeID = OriginalSegmentNumber then
          begin
            result := -AScreenObject.ModflowLakBoundary.TrueLakeID;
            Exit;
          end;
        end;
      end;
    end;
  end;
end;

function TModflowSFR_Writer.GetSegment(Index: integer): TSegment;
begin
  result := FSegments[Index];
end;

function TModflowSFR_Writer.GetSegmentCount: integer;
begin
  result := FSegments.Count;
end;

procedure TModflowSFR_Writer.WriteDataSet4b6a(StartTime: double;
  Segment: TSegment; ParamScreenObjectItem: TSfrParamIcalcItem;
  SfrBoundary: TSfrBoundary; DataSet4B: boolean);
var
  ICALC: Integer;
  FlowTable: TSfrFlowTableRecord;
  SegmentFlow: TSfrSegmentFlowRecord;
  ChannelValues: TSfrChannelRecord;
  EqValues: TSfrEquationRecord;
  IUPSEG: Integer;
begin
  // data set 4b and 6a
  //  NSEG
  WriteInteger(Segment.NewSegmentNumber);
  // ICALC
  ICALC := ParamScreenObjectItem.ICalc;
  WriteInteger(ICALC);
  // OUTSEG
  WriteInteger(FindConvertedSegment(ParamScreenObjectItem.OutflowSegment));
  // IUPSEG
  IUPSEG := FindConvertedSegment(ParamScreenObjectItem.DiversionSegment);
  WriteInteger(IUPSEG);
  if IUPSEG > 0 then
  begin
    // IPRIOR
    WriteInteger(ParamScreenObjectItem.IPRIOR);
  end;
  if ICALC = 4 then
  begin
    // NSTRPTS]
    FlowTable := SfrBoundary.TableCollection.GetRecordForTime(
      StartTime);
    WriteInteger(Length(FlowTable.SfrFlowTableArray));
  end;
  // FLOW
  SegmentFlow := SfrBoundary.SegmentFlows.GetFlowValuesFromTime(
    StartTime);
  WriteFloat(SegmentFlow.Flow);
  // RUNOFF
  WriteFloat(SegmentFlow.Runnoff);
  // ETSW
  WriteFloat(SegmentFlow.Evapotranspiration);
  // PPTSW
  WriteFloat(SegmentFlow.Precipitation);
  // [ROUGHCH] [ROUGHBK] [CDPTH] [FDPTH]
  if ICALC in [1, 2] then
  begin
    // ROUGHCH
    ChannelValues := SfrBoundary.ChannelValues.GetChannelTimeValuesFromTime(
      Model, StartTime);
    WriteFloat(ChannelValues.ChannelRoughness);
    if ICALC = 2 then
    begin
      // ROUGHBK
      WriteFloat(ChannelValues.BankRoughness);
    end;
  end;
  if ICALC = 3 then
  begin
    EqValues := SfrBoundary.EquationValues.
      GetEquationTimeValuesFromTime(StartTime);
    // CDPTH
    WriteFloat(EqValues.DepthCoefficient);
    // FDPTH
    WriteFloat(EqValues.DepthExponent);
    // AWDTH
    WriteFloat(EqValues.WidthCoefficient);
    // BWDTH
    WriteFloat(EqValues.WidthExponent);
  end;
  if DataSet4B then
  begin
    WriteString(' # Data Set 4b: ');
  end
  else
  begin
    WriteString(' # Data Set 6a: ');
  end;
  WriteString('NSEG ICALC OUTSEG IUPSEG');
  if IUPSEG <> 0 then
  begin
    WriteString(' IPRIOR');
  end;
  if ICALC = 4 then
  begin
    WriteString(' NSTRPTS');
  end;
  WriteString(' FLOW RUNOFF ETSW PPTSW');
  if ICALC in [1, 2] then
  begin
    // ROUGHCH
    WriteString(' ROUGHCH');
    if ICALC = 2 then
    begin
      WriteString(' ROUGHBK');
    end;
  end;
  if ICALC = 3 then
  begin
    WriteString(' CDPTH FDPTH AWDTH BWDTH');
  end;
  NewLine;
end;

procedure TModflowSFR_Writer.WriteDataSet4c6b(Parameter: Boolean;
  SfrBoundary: TSfrBoundary; ParamScreenObjectItem: TSfrParamIcalcItem;
  StartTime: double; StressPeriodIndex: integer);
var
  upstream: Boolean;
  CommentLine: string;
  UpstreamValues: TSfrSegmentStorage;
  UnsatUpstreamValues: TSfrUnsatSegmentStorage;
  ValuesWriten: boolean;
  ErrorObject: TScreenObject;
begin
  upstream := True;

  CommentLine :=' #';
  if Parameter then
  begin
    CommentLine := CommentLine + ' Data set 4c:';
  end
  else
  begin
    CommentLine := CommentLine + ' Data set 6b:';
  end;
  ValuesWriten := False;

  UpstreamValues := SfrBoundary.UpstreamSegmentValues.
    GetBoundaryByStartTime(StartTime)
    as TSfrSegmentStorage;
  if UpstreamValues = nil then
  begin
    ErrorObject := SfrBoundary.ScreenObject as TScreenObject;
    frmErrorsAndWarnings.AddError(Model,
      StrInvalidStartingTim, ErrorObject.Name);
    Exit;
//    raise EInvalidTime.Create(SfrBoundary.ScreenObject);
  end;
  WriteSegmentValues(StressPeriodIndex, Parameter, UpstreamValues,
    upstream, CommentLine, ValuesWriten, ParamScreenObjectItem);

  if (ISFROPT in [4,5]) and (StressPeriodIndex = 0) then
  begin
    if SfrBoundary.UpstreamUnsatSegmentValues.
      BoundaryCount > 0 then
    begin
      UnsatUpstreamValues := SfrBoundary.UpstreamUnsatSegmentValues.
        Boundaries[0] as TSfrUnsatSegmentStorage;
      WriteUnsatSegmentValues(upstream, CommentLine, ValuesWriten,
        UnsatUpstreamValues);
    end
    else
    begin
      frmErrorsAndWarnings.AddError(Model, UnsatError,
        (SfrBoundary.ScreenObject as TScreenObject).Name);
    end;
  end;

  if ValuesWriten then
  begin
    WriteString(CommentLine);
    NewLine;
  end;

end;

procedure TModflowSFR_Writer.WriteDataSet4d6c(Parameter: Boolean;
  SfrBoundary: TSfrBoundary; ParamScreenObjectItem: TSfrParamIcalcItem;
  StartTime: double; StressPeriodIndex: integer);
var
  upstream: Boolean;
  CommentLine: string;
  DownstreamValues: TSfrSegmentStorage;
  UnsatDownstreamValues: TSfrUnsatSegmentStorage;
  ValuesWriten: boolean;
  ErrorObject: TScreenObject;
begin
  upstream := False;

  CommentLine :=' #';
  if Parameter then
  begin
    CommentLine := CommentLine + ' Data set 4d:';
  end
  else
  begin
    CommentLine := CommentLine + ' Data set 6c:';
  end;
  ValuesWriten:= False;

  DownstreamValues := SfrBoundary.DownstreamSegmentValues.
    GetBoundaryByStartTime(StartTime)
    as TSfrSegmentStorage;
  if DownstreamValues = nil then
  begin
    ErrorObject := SfrBoundary.ScreenObject as TScreenObject;
    frmErrorsAndWarnings.AddError(Model,
      StrInvalidStartingTim, ErrorObject.Name);
    Exit;
//    raise EInvalidTime.Create(SfrBoundary.ScreenObject);
  end;
  WriteSegmentValues(StressPeriodIndex, Parameter, DownstreamValues,
    upstream, CommentLine, ValuesWriten, ParamScreenObjectItem);

  if (ISFROPT in [4,5]) and (StressPeriodIndex = 0) then
  begin
    if SfrBoundary.DownstreamUnsatSegmentValues.
      BoundaryCount > 0 then
    begin
      UnsatDownstreamValues := SfrBoundary.DownstreamUnsatSegmentValues.
        Boundaries[0] as TSfrUnsatSegmentStorage;
      WriteUnsatSegmentValues(upstream, CommentLine, ValuesWriten,
        UnsatDownstreamValues);
    end
    else
    begin
      frmErrorsAndWarnings.AddError(Model, UnsatError,
        (SfrBoundary.ScreenObject as TScreenObject).Name);
    end;
  end;

  if ValuesWriten then
  begin
    WriteString(CommentLine);
    NewLine;
  end;

end;
{ TSegment }

destructor TSegment.Destroy;
begin
  FReaches.Free;
end;

function TSegment.GetReach(Index: integer): TValueCell;
begin
  result := FReaches[Index];
end;

function TSegment.GetReachCount: integer;
begin
  result := FReaches.Count;
end;

function TSegment.OriginalDiversionSegmentNumbers: TIntegerDynArray;
var
  ParamIcalc: TSfrParamIcalcCollection;
  Index: Integer;
  IntList: TIntegerList;
  Item: TSfrParamIcalcItem;
begin
  Assert(FScreenObject.ModflowSfrBoundary <> nil);
  ParamIcalc := FScreenObject.ModflowSfrBoundary.ParamIcalc;
  IntList := TIntegerList.Create;
  try
    IntList.Capacity := ParamIcalc.Count;
    IntList.Sorted := True;
    for Index := 0 to ParamIcalc.Count - 1 do
    begin
      Item := ParamIcalc.Items[Index];
      IntList.AddUnique(Item.DiversionSegment);
    end;
    setLength(result, IntList.Count);
    for Index := 0 to IntList.Count - 1 do
    begin
      result[Index] := IntList[Index];
    end;
  finally
    IntList.Free;
  end;
end;

function TSegment.OriginalDownStreamSegmentNumbers: TIntegerDynArray;
var
  ParamIcalc: TSfrParamIcalcCollection;
  Index: Integer;
  IntList: TIntegerList;
  Item: TSfrParamIcalcItem;
begin
  Assert(FScreenObject.ModflowSfrBoundary <> nil);
  ParamIcalc := FScreenObject.ModflowSfrBoundary.ParamIcalc;
  IntList := TIntegerList.Create;
  try
    IntList.Capacity := ParamIcalc.Count;
    IntList.Sorted := True;
    for Index := 0 to ParamIcalc.Count - 1 do
    begin
      Item := ParamIcalc.Items[Index];
      IntList.AddUnique(Item.OutflowSegment);
    end;
    setLength(result, IntList.Count);
    for Index := 0 to IntList.Count - 1 do
    begin
      result[Index] := IntList[Index];
    end;
  finally
    IntList.Free;
  end;
end;

function TSegment.OriginalSegmentNumber: integer;
begin
  Assert(FScreenObject.ModflowSfrBoundary <> nil);
  result := FScreenObject.ModflowSfrBoundary.SegementNumber;
end;

end.
