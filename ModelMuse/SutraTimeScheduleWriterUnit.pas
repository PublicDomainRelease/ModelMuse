unit SutraTimeScheduleWriterUnit;

interface

uses
  Windows, CustomModflowWriterUnit, PhastModelUnit, SutraTimeScheduleUnit, RealListUnit,
  Generics.Collections, SysUtils, GoPhastTypes, Classes, ScreenObjectUnit;

type
  TTimeValues = class(TObject)
  public
    Schedule: TSutraTimeSchedule;
    Used: Boolean;
    Times: TRealList;
    constructor Create;
    destructor Destroy; override;
  end;

  TTimeValuesList = TList<TTimeValues>;

  TTimeValuesDictionary = TObjectDictionary<string, TTimeValues>;

  TScreenObjectSchedule = class(TObject)
    Name: string;
    Times: TRealCollection;
  end;

  TScreenObjectScheduleList = TObjectList<TScreenObjectSchedule>;

  TSutraTimeScheduleWriter = class(TCustomFileWriter)
  private
    CustomScheduleIndex: Integer;
    FTimeOptions: TSutraTimeOptions;
    FSchedules: TSutraTimeSchedules;
    ItemDictionary: TTimeValuesDictionary;
    CustomScheduleNames: TStringList;
    FluidSourceTimes: TRealList;
    USourceTimes: TRealList;
    SpecifiedPressureTimes: TRealList;
    SpecifiedUTimes: TRealList;
    AllTimes: TRealList;
    CustomSchedules: TScreenObjectScheduleList;
    FirstTimeValues: TTimeValues;
    ExtraTimesDefined: Boolean;
    ScheduleList: TTimeValuesList;
    function CustomScheduleName(AScreenObject: TScreenObject): string;
    procedure Evaluate;
    procedure EvaluateDefinedSchedules;
    procedure EvaluateObjectSchedules;
    procedure MakeListOfUsedSchedules;
    procedure WriteLine1;
    procedure WriteSchedules;
    procedure WriteASchedule(ASchedule: TTimeValues); overload;
    procedure WriteASchedule(SCHNAM: AnsiString; Times: TRealList); overload;
    procedure WriteASchedule(SCHNAM: AnsiString; Times: TRealCollection); overload;
  function ReformatScheduleName(SCHNAM: AnsiString): AnsiString;
  public
    Constructor Create(AModel: TCustomModel); reintroduce;
    procedure WriteFile(FileName: string);
  end;

function SameValues(List1, List2: TRealList): Boolean; overload;
function SameValues(List1: TRealList; List2: TRealCollection): Boolean;
  overload;

implementation

uses
  frmErrorsAndWarningsUnit, SutraBoundariesUnit, SutraBoundaryWriterUnit;

resourcestring
  StrTheFollowingTimeS = 'The following time schedule names are defined more' +
    ' than once. ';

function SameValues(List1, List2: TRealList): Boolean;
var
  index: Integer;
begin
  Assert(List1 <> nil);
  Assert(List2 <> nil);
  result := List1.Count = List2.Count;
  if result then
  begin
    for index := 0 to List1.Count - 1 do
    begin
      result := List1[index] = List2[index];
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

function SameValues(List1: TRealList; List2: TRealCollection): Boolean;
var
  index: Integer;
begin
  Assert(List1 <> nil);
  Assert(List2 <> nil);
  result := List1.Count = List2.Count;
  if result then
  begin
    for index := 0 to List1.Count - 1 do
    begin
      result := List1[index] = List2[index].Value;
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

{ TSutraTimeScheduleWriter }

constructor TSutraTimeScheduleWriter.Create(AModel: TCustomModel);
begin
  inherited Create(AModel, etExport);
  FTimeOptions := (AModel as TPhastModel).SutraTimeOptions;
  FSchedules := FTimeOptions.Schedules;
end;

function TSutraTimeScheduleWriter.CustomScheduleName(AScreenObject: TScreenObject): string;
begin
  // maximum length of a schedule name is 10 characters.
  result := Copy(string(AnsiString(AScreenObject.Name)), 1, 10);
  while ItemDictionary.ContainsKey(result) or
    (CustomScheduleNames.IndexOf(result) >= 0) do
  begin
    Inc(CustomScheduleIndex);
    result := 'SCHED_' + IntToStr(CustomScheduleIndex);
  end;
  CustomScheduleNames.Add(result);
end;

procedure TSutraTimeScheduleWriter.WriteASchedule(SCHNAM: AnsiString;
  Times: TRealList);
var
  SCHTYP: AnsiString;
  CREFT: AnsiString;
  SCALT: double;
  NTLIST: Integer;
  TimeIndex: Integer;
begin
  WriteString(SCHNAM);

  SCHTYP := ' ''TIME LIST''';
  CREFT  := ' ''ABSOLUTE''';
  SCALT := 1.0;
  NTLIST := Times.Count;


  WriteString(SCHTYP);
  WriteString(CREFT);
  WriteFloat(SCALT);
  WriteInteger(NTLIST);

  for TimeIndex := 0 to Times.Count - 1 do
  begin
    WriteFloat(Times[TimeIndex]);
    if ((TimeIndex + 1) mod 10) = 0 then
    begin
      NewLine;
    end;
  end;
  if (Times.Count mod 10) <> 0 then
  begin
    NewLine;
  end;

end;

procedure TSutraTimeScheduleWriter.WriteFile(FileName: string);
begin
  FluidSourceTimes := TRealList.Create;
  USourceTimes := TRealList.Create;
  SpecifiedPressureTimes := TRealList.Create;
  SpecifiedUTimes := TRealList.Create;

  FluidSourceTimes.Sorted := True;
  USourceTimes.Sorted := True;
  SpecifiedPressureTimes.Sorted := True;
  SpecifiedUTimes.Sorted := True;

  CustomScheduleNames := TStringList.Create;
  AllTimes := TRealList.Create;
  CustomSchedules := TScreenObjectScheduleList.Create;
  ItemDictionary := TTimeValuesDictionary.Create([doOwnsValues]);
    ScheduleList := TTimeValuesList.Create;
  try
    Evaluate;

    FileName := ChangeFileExt(FileName, '.6');
    OpenFile(FileName);
    try
      WriteLine1;
      WriteSchedules;
      Model.AddModelInputFile(FileName);
    finally
      CloseFile;
    end;


  finally
    ScheduleList.Free;
    AllTimes.Free;
    ItemDictionary.Free;
    CustomSchedules.Free;
    CustomScheduleNames.Free;

    FluidSourceTimes.Free;
    USourceTimes.Free;
    SpecifiedPressureTimes.Free;
    SpecifiedUTimes.Free;

  end;
end;

function TSutraTimeScheduleWriter.ReformatScheduleName(SCHNAM: AnsiString):
  AnsiString;
var
  NameLength: Integer;
begin
  NameLength := Length(SCHNAM);
  SCHNAM := '''' + SCHNAM + '''';
  if NameLength < 10 then
  begin
    SCHNAM := SCHNAM + StringOfChar(' ', 10 - NameLength);
  end;
  result := SCHNAM;
end;

procedure TSutraTimeScheduleWriter.WriteLine1;
var
  NSCH: integer;
  TimeOptions: TSutraTimeOptions;
  NPCYC: Integer;
  NUCYC: Integer;
begin
  NSCH := ScheduleList.Count + CustomSchedules.Count;
  if FluidSourceTimes.Count > 0 then
  begin
    Inc(NSCH);
  end;
  if USourceTimes.Count > 0 then
  begin
    Inc(NSCH);
  end;
  if SpecifiedPressureTimes.Count > 0 then
  begin
    Inc(NSCH);
  end;
  if SpecifiedUTimes.Count > 0 then
  begin
    Inc(NSCH);
  end;

  TimeOptions := (Model as TPhastModel).SutraTimeOptions;

  NPCYC := TimeOptions.HydraulicSolutionCycleSteps;
  NUCYC := TimeOptions.TransportSolutionCycleSteps;

  WriteInteger(NSCH);
  WriteInteger(NPCYC);
  WriteInteger(NUCYC);
  NewLine;
end;

procedure TSutraTimeScheduleWriter.WriteASchedule(ASchedule: TTimeValues);
var
  SCHNAM: AnsiString;
  SCHTYP: AnsiString;
  CREFT: AnsiString;
//  NameLength: Integer;
  SCALT: double;
  NTLIST: Integer;
  TimeIndex: Integer;
  NTMAX: Integer;
  TIMEI: Double;
  TIMEL: Double;
  NTCYC: Integer;
  TIMEC: Double;
  NSLIST: Integer;
  NSMAX: Integer;
  ISTEPI: Integer;
  ISTEPL: Integer;
  ISTEPC: Integer;
  TCMULT: Double;
  TCMIN: Double;
  TCMAX: Double;
begin
  SCHNAM := ReformatScheduleName(ASchedule.Schedule.Name);

  if ExtraTimesDefined then
  begin
    WriteASchedule(SCHNAM, ASchedule.Times);
  end
  else
  begin
    case ASchedule.Schedule.ScheduleType of
      stTimeList:
        begin
          SCHTYP := ' ''TIME LIST''';
          case ASchedule.Schedule.SutraTimeChoice of
            stcAbsolute: CREFT  := ' ''ABSOLUTE''';
            stcElapsed: CREFT  := ' ''ELAPSED''';
            else
              Assert(False);
          end;
          SCALT := ASchedule.Schedule.ScaleFactor;
          NTLIST := ASchedule.Schedule.Times.Count;

          WriteString(SCHNAM);
          WriteString(SCHTYP);
          WriteString(CREFT);
          WriteFloat(SCALT);
          WriteInteger(NTLIST);

          for TimeIndex := 0 to ASchedule.Schedule.Times.Count - 1 do
          begin
            WriteFloat(ASchedule.Schedule.Times[TimeIndex].Value);
            if ((TimeIndex + 1) mod 10) = 0 then
            begin
              NewLine;
            end;
          end;
          if (ASchedule.Schedule.Times.Count mod 10) <> 0 then
          begin
            NewLine;
          end;
        end;
      stTimeCycle:
        begin
          SCHTYP := ' ''TIME CYCLE''';
          TIMEI := -MAXINT;
          CREFT := '';
          case ASchedule.Schedule.SutraTimeChoice of
            stcAbsolute:
              begin
                CREFT := ' ''ABSOLUTE''';
                TIMEI := ASchedule.Schedule.InitialTime;
              end;

            stcElapsed:
              begin
                CREFT := ' ''ELAPSED''';
                TIMEI := 0
              end;
            else
              Assert(False);
          end;
          SCALT := ASchedule.Schedule.ScaleFactor;
          NTMAX := ASchedule.Schedule.MaxTimeCycle;
          TIMEL := ASchedule.Schedule.LimitingTime;
          TIMEC := ASchedule.Schedule.InitialTimeIncrement;
          NTCYC := ASchedule.Schedule.IncrementUpdateCount;
          TCMULT := ASchedule.Schedule.TimeMultiplier;
          TCMIN := ASchedule.Schedule.MinIncrement;
          TCMAX := ASchedule.Schedule.MaxIncrement;

          WriteString(SCHNAM);
          WriteString(SCHTYP);
          WriteString(CREFT);
          WriteFloat(SCALT);
          WriteInteger(NTMAX);
          WriteFloat(TIMEI);
          WriteFloat(TIMEL);
          WriteFloat(TIMEC);
          WriteInteger(NTCYC);
          WriteFloat(TCMULT);
          WriteFloat(TCMIN);
          WriteFloat(TCMAX);
          NewLine;
        end;
      stStepList:
        begin
          SCHTYP := ' ''STEP LIST''';

          NSLIST := ASchedule.Schedule.Steps.Count;

          WriteString(SCHNAM);
          WriteString(SCHTYP);
          WriteInteger(NSLIST);
          for TimeIndex := 0 to ASchedule.Schedule.Steps.Count - 1 do
          begin
            WriteInteger(ASchedule.Schedule.Steps[TimeIndex].Value);
            if ((TimeIndex + 1) mod 10) = 0 then
            begin
              NewLine;
            end;
          end;
          if (ASchedule.Schedule.Steps.Count mod 10) <> 0 then
          begin
            NewLine;
          end;
        end;
      stStepCycle:
        begin
          SCHTYP := ' ''STEP CYCLE''';
          NSMAX := ASchedule.Schedule.MaxSteps;
          ISTEPI := ASchedule.Schedule.InitialTimeStep;
          ISTEPL := ASchedule.Schedule.LimitingTimeStep;
          ISTEPC := ASchedule.Schedule.TimeStepIncrement;

          WriteString(SCHNAM);
          WriteString(SCHTYP);
          WriteInteger(NSMAX);
          WriteInteger(ISTEPI);
          WriteInteger(ISTEPL);
          WriteInteger(ISTEPC);
          NewLine;
        end;
      else
        Assert(False);
    end;
  end;

end;

procedure TSutraTimeScheduleWriter.WriteSchedules;
var
  ScheduleIndex: Integer;
  ASchedule: TTimeValues;
  ScheduleCount: integer;
  ACustomSchedule: TScreenObjectSchedule;
begin
  ScheduleCount := 0;
  for ScheduleIndex := 0 to ScheduleList.Count - 1 do
  begin
    ASchedule := ScheduleList[ScheduleIndex];
    Inc(ScheduleCount);
    WriteCommentLine('Schedule ' + IntToStr(ScheduleCount));

    if ExtraTimesDefined and (ScheduleIndex = 0) then
    begin
      WriteASchedule(ReformatScheduleName(ASchedule.Schedule.Name), AllTimes);
    end
    else
    begin
      WriteASchedule(ASchedule);
    end;

  end;

  if FluidSourceTimes.Count > 0 then
  begin
    Inc(ScheduleCount);
    WriteCommentLine('Schedule ' + IntToStr(ScheduleCount));
    WriteASchedule(ReformatScheduleName(KFluidFlux), FluidSourceTimes);
  end;

  if USourceTimes.Count > 0 then
  begin
    Inc(ScheduleCount);
    WriteCommentLine('Schedule ' + IntToStr(ScheduleCount));
    WriteASchedule(ReformatScheduleName(KUFlux), USourceTimes);
  end;

  if SpecifiedPressureTimes.Count > 0 then
  begin
    Inc(ScheduleCount);
    WriteCommentLine('Schedule ' + IntToStr(ScheduleCount));
    WriteASchedule(ReformatScheduleName(KSpecifiedP), SpecifiedPressureTimes);
  end;

  if SpecifiedUTimes.Count > 0 then
  begin
    Inc(ScheduleCount);
    WriteCommentLine('Schedule ' + IntToStr(ScheduleCount));
    WriteASchedule(ReformatScheduleName(KSpecifiedU), SpecifiedUTimes);
  end;

  for ScheduleIndex := 0 to CustomSchedules.Count - 1 do
  begin
    ACustomSchedule := CustomSchedules[ScheduleIndex];
    Inc(ScheduleCount);
    WriteCommentLine('Schedule ' + IntToStr(ScheduleCount));
    WriteASchedule(ReformatScheduleName(ACustomSchedule.Name), ACustomSchedule.Times);
  end;

  WriteString('-');
  NewLine;
end;

procedure TSutraTimeScheduleWriter.MakeListOfUsedSchedules;
var
  ScheduleArray: TArray<TPair<string, TTimeValues>>;
  index: Integer;
  AValue: TTimeValues;
begin
  ScheduleArray := ItemDictionary.ToArray;
  ScheduleList.Add(FirstTimeValues);
  for index := 0 to Length(ScheduleArray) - 1 do
  begin
    AValue := ScheduleArray[index].Value;
    if AValue.Used and (AValue <> FirstTimeValues) then
    begin
      ScheduleList.Add(AValue);
    end;
  end;
end;

procedure TSutraTimeScheduleWriter.EvaluateObjectSchedules;
var
  TimeIndex: Integer;
  ScreenObjectIndex: Integer;
  UseScheduleTimes: Boolean;
  TimeValues: TTimeValues;
  Boundaries: TSutraBoundaries;
  AScreenObject: TScreenObject;
  AName: string;
  CustomSchedule: TScreenObjectSchedule;
begin
  CustomScheduleIndex := 0;
  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    AScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if AScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundaries := AScreenObject.SutraBoundaries;
    if Boundaries.Observations.Used then
    begin
      AName := UpperCase(Boundaries.Observations.ScheduleName);
      if (AName <> '') and ItemDictionary.ContainsKey(AName) then
      begin
        TimeValues := ItemDictionary.Items[AName];
        UseScheduleTimes := SameValues(TimeValues.Times, Boundaries.Observations.Times);
        TimeValues.Used := TimeValues.Used or UseScheduleTimes;
        // set default for the ExportScheduleName
        Boundaries.Observations.ExportScheduleName := Boundaries.Observations.ScheduleName;
      end
      else
      begin
        TimeValues := nil;
        UseScheduleTimes := False;
      end;
      if not UseScheduleTimes then
      begin
        Boundaries.Observations.ExportScheduleName := CustomScheduleName(AScreenObject);
        CustomSchedule := TScreenObjectSchedule.Create;
        CustomSchedules.Add(CustomSchedule);
        CustomSchedule.Name := Boundaries.Observations.ExportScheduleName;
        CustomSchedule.Times := Boundaries.Observations.Times;
      end;
    end;
    // The boundary conditions each use a separate schedule.
    // See SutraBoundaryWriterUnit.pas.
    if Boundaries.FluidSource.Used then
    begin
      for TimeIndex := 0 to Boundaries.FluidSource.Values.Count - 1 do
      begin
        FluidSourceTimes.AddUnique(Boundaries.FluidSource.Values[TimeIndex].StartTime);
      end;
    end;
    if Boundaries.MassEnergySource.Used then
    begin
      for TimeIndex := 0 to Boundaries.MassEnergySource.Values.Count - 1 do
      begin
        USourceTimes.AddUnique(Boundaries.MassEnergySource.Values[TimeIndex].StartTime);
      end;
    end;
    if Boundaries.SpecifiedPressure.Used then
    begin
      for TimeIndex := 0 to Boundaries.SpecifiedPressure.Values.Count - 1 do
      begin
        SpecifiedPressureTimes.AddUnique(Boundaries.SpecifiedPressure.Values[TimeIndex].StartTime);
      end;
    end;
    if Boundaries.SpecifiedConcTemp.Used then
    begin
      for TimeIndex := 0 to Boundaries.SpecifiedConcTemp.Values.Count - 1 do
      begin
        SpecifiedUTimes.AddUnique(Boundaries.SpecifiedConcTemp.Values[TimeIndex].StartTime);
      end;
    end;
  end;
end;

procedure TSutraTimeScheduleWriter.EvaluateDefinedSchedules;
var
  ScheduleIndex: Integer;
  TimeValues: TTimeValues;
  TimeIndex: Integer;
  Times: TOneDRealArray;
  AName: string;
  ASchedule: TSutraTimeSchedule;
begin
  FirstTimeValues := nil;
  // Compute all the times for each time schedule and store them
  // in a TimeValues.Times for each schedule.
  for ScheduleIndex := 0 to FSchedules.Count - 1 do
  begin
    ASchedule := FSchedules[ScheduleIndex].Schedule;
    Times := ASchedule.TimeValues(FTimeOptions.InitialTime, FSchedules);
    TimeValues := TTimeValues.Create;
    if ScheduleIndex = 0 then
    begin
      FirstTimeValues := TimeValues;
    end;
    AName := UpperCase(ASchedule.Name);
    if ItemDictionary.ContainsKey(AName) then
    begin
      Beep;
      frmErrorsAndWarnings.AddError(Model, StrTheFollowingTimeS, ASchedule.Name);
      Continue;
    end
    else
    begin
      ItemDictionary.Add(AName, TimeValues);
    end;
    TimeValues.Schedule := ASchedule;
    for TimeIndex := 0 to Length(Times) - 1 do
    begin
      TimeValues.Times.Add(Times[TimeIndex]);
    end;
  end;
end;

procedure TSutraTimeScheduleWriter.Evaluate;
begin
  CustomScheduleNames.CaseSensitive := False;
  CustomScheduleNames.Sorted := True;
  FTimeOptions.CalculateAllTimes;
  AllTimes.Assign(FTimeOptions.AllTimes);

  EvaluateDefinedSchedules;

  // Determine if the screen objects defined any other times not
  // listed in the main schedule.
  ExtraTimesDefined := not SameValues(AllTimes, FirstTimeValues.Times);
  FirstTimeValues.Used := ExtraTimesDefined;

  EvaluateObjectSchedules;
  MakeListOfUsedSchedules;
end;

procedure TSutraTimeScheduleWriter.WriteASchedule(SCHNAM: AnsiString;
  Times: TRealCollection);
var
  TimeList: TRealList;
  TimeIndex: Integer;
begin
  TimeList := TRealList.Create;
  try
    TimeList.Capacity := Times.Count;
    for TimeIndex := 0 to Times.Count - 1 do
    begin
      TimeList.Add(Times[TimeIndex].Value)
    end;
    WriteASchedule(SCHNAM, TimeList);
  finally
    TimeList.Free;
  end;
end;

{ TTimeValues }

constructor TTimeValues.Create;
begin
  Times := TRealList.Create;
  Times.Sorted := True;
  Used := False;
end;

destructor TTimeValues.Destroy;
begin
  Times.Free;
  inherited;
end;

end.
