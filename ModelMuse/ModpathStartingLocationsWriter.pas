unit ModpathStartingLocationsWriter;

interface

uses Classes, SysUtils, Contnrs , PhastModelUnit, ScreenObjectUnit,
  CustomModflowWriterUnit, ModflowPackageSelectionUnit;

type
  TParticleLines = class(TObject)
  private
    FSimulatedLocations: TStringList;
    FNonSimulatedLocations: TStringList;
    FReleaseTimes: TStringList;
    FTrackingDirection: TTrackingDirection;
  public
    Constructor Create(ScreenObject: TScreenObject;
      TrackingDirection: TTrackingDirection; StartTime, EndTime: Real);
    Destructor Destroy; override;
    procedure UpdateLocationLines(Lines: TStringList;
      Layer, Row, Column: integer; SimulatedLayer: boolean);
  end;

  TModpathStartingLocationsWriter = class(TCustomModflowWriter)
  private
    FParticleLines: TList;
    FCellList: TCellAssignmentList;
    FParticleGrid: array of array of array of TParticleLines;
    FStartingLocations: TStringList;
    FStartTime: Real;
    FEndTime: Real;
    procedure AssignParticleLocationsToElements;
    procedure UpdateParticleLines;
    procedure WriteLines;
  public
    class function Extension: string; override;
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    Destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  ModpathParticleUnit, ModflowTimeUnit, frmErrorsAndWarningsUnit,
  ModelMuseUtilities, frmGoPhastUnit;

resourcestring
  StrAStartingTimeFor = 'A starting time for the MODPATH particles defined '
    + 'with the following objects are not valid. Adjust the beginning and '
    + 'ending time for MODPATH or adjust the relese time.';

{ TModpathStartingLocationsWriter }

constructor TModpathStartingLocationsWriter.Create(Model: TCustomModel; EvaluationType: TEvaluationType);
begin
  inherited;
  FParticleLines := TObjectList.Create;
  FCellList:= TCellAssignmentList.Create;
  FStartingLocations := TStringList.Create;
end;

destructor TModpathStartingLocationsWriter.Destroy;
begin
  FStartingLocations.Free;
  FCellList.Free;
  FParticleLines.Free;
  inherited;
end;

class function TModpathStartingLocationsWriter.Extension: string;
begin
  result := '.strt';
end;

procedure TModpathStartingLocationsWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
  StressPeriods: TModflowStressPeriods;
begin
  StressPeriods := Model.ModflowStressPeriods;
  if StressPeriods.CompletelyTransient then
  begin
    FStartTime := Model.ModflowPackages.ModPath.BeginningTime;
  end
  else
  begin
    FStartTime := StressPeriods[0].StartTime;
  end;
  if StressPeriods.TransientModel then
  begin
    FEndTime := Model.ModflowPackages.ModPath.EndingTime;
  end
  else
  begin
    FEndTime := StressPeriods[StressPeriods.Count-1].EndTime;
  end;
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrAStartingTimeFor);

  NameOfFile := FileName(AFileName);
  OpenFile(NameOfFile);
  try
    AssignParticleLocationsToElements;
    UpdateParticleLines;
    WriteLines;
  finally
    CloseFile;
  end;
end;

procedure TModpathStartingLocationsWriter.WriteLines;
var
  LineIndex: Integer;
begin
  for LineIndex := 0 to FStartingLocations.Count - 1 do
  begin
    WriteString(FStartingLocations[LineIndex]);
    NewLine;
  end;
end;

procedure TModpathStartingLocationsWriter.UpdateParticleLines;
var
  LayerIndex: Integer;
  SimulatedLayer: Boolean;
  RowIndex: Integer;
  ColumnIndex: Integer;
  ParticleLines: TParticleLines;
begin
  for LayerIndex := 0 to Model.Grid.LayerCount - 1 do
  begin
    SimulatedLayer := Model.IsLayerSimulated(LayerIndex);
    for RowIndex := 0 to Model.Grid.RowCount - 1 do
    begin
      for ColumnIndex := 0 to Model.Grid.ColumnCount - 1 do
      begin
        ParticleLines := FParticleGrid[LayerIndex, RowIndex, ColumnIndex];
        if ParticleLines <> nil then
        begin
          ParticleLines.UpdateLocationLines(FStartingLocations,
            LayerIndex + 1, RowIndex + 1, ColumnIndex + 1, SimulatedLayer);
        end;
      end;
    end;
  end;
end;

procedure TModpathStartingLocationsWriter.AssignParticleLocationsToElements;
var
  ScreenObject: TScreenObject;
  Index: Integer;
  Cell: TCellAssignment;
  ObjectIndex: Integer;
  ParticleLines: TParticleLines;
  LocalModel: TCustomModel;
begin
  LocalModel := Model;
  SetLength(FParticleGrid, LocalModel.Grid.LayerCount,
    LocalModel.Grid.RowCount, LocalModel.Grid.ColumnCount);
  for Index := 0 to LocalModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := LocalModel.ScreenObjects[Index];
    if (not ScreenObject.Deleted) and ScreenObject.ModpathParticles.Used then
    begin
      ParticleLines := TParticleLines.Create(ScreenObject,
        LocalModel.ModflowPackages.ModPath.TrackingDirection,
        FStartTime, FEndTime);
      FParticleLines.Add(ParticleLines);
      FCellList.Clear;
      ScreenObject.GetModpathCellList(FCellList, LocalModel);
      for ObjectIndex := 0 to FCellList.Count - 1 do
      begin
        Cell := FCellList[ObjectIndex];
        FParticleGrid[Cell.Layer, Cell.Row, Cell.Column] := ParticleLines;
      end;
    end;
  end;
end;

{ TParticleLines }

constructor TParticleLines.Create(ScreenObject: TScreenObject;
TrackingDirection: TTrackingDirection; StartTime, EndTime: Real);
var
  TimeIndex: Integer;
  ParticleReleaseTimes: TModpathTimes;
  TimeItem: TModpathTimeItem;
  Particles: TParticles;
  Index: Integer;
  ParticleItem: TParticleLocation;
  XYString: string;
  ReleaseTimeErrorDetected: boolean;
begin

  Assert(ScreenObject <> nil);
  Assert(not ScreenObject.Deleted);
  Assert(ScreenObject.ModpathParticles.Used);
  FTrackingDirection := TrackingDirection;
  FSimulatedLocations:= TStringList.Create;
  FNonSimulatedLocations:= TStringList.Create;
  FReleaseTimes:= TStringList.Create;
  ParticleReleaseTimes := ScreenObject.ModpathParticles.ReleaseTimes;
  FReleaseTimes.Capacity := ParticleReleaseTimes.Count;
  ReleaseTimeErrorDetected := False;
  for TimeIndex := 0 to ParticleReleaseTimes.Count - 1 do
  begin
    TimeItem := ParticleReleaseTimes.Items[TimeIndex] as TModpathTimeItem;
    if (not ReleaseTimeErrorDetected) and (FTrackingDirection = tdForward) and
      ((TimeItem.Time < 0) or (TimeItem.Time > EndTime-StartTime)) then
    begin
      frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel, StrAStartingTimeFor,
        ScreenObject.Name);
      ReleaseTimeErrorDetected := True;
    end;
    FReleaseTimes.Add(FortranFloatToStr(TimeItem.Time));
  end;
  Particles := ScreenObject.ModpathParticles.Particles;
  for Index := 0 to Particles.Count - 1 do
  begin
    ParticleItem := Particles.Items[Index] as TParticleLocation;
    XYString := FortranFloatToStr(ParticleItem.X) + ' '
      + FortranFloatToStr(ParticleItem.Y) + ' ';
    FSimulatedLocations.Add(XYString + FortranFloatToStr(ParticleItem.Z)
      + ' 0 0 0 ');
    FNonSimulatedLocations.Add(XYString + FortranFloatToStr(1-ParticleItem.Z)
      + ' 0 0 0 ');
  end;
end;

destructor TParticleLines.Destroy;
begin
  FSimulatedLocations.Free;
  FNonSimulatedLocations.Free;
  FReleaseTimes.Free;
  inherited;
end;

procedure TParticleLines.UpdateLocationLines(Lines: TStringList; Layer, Row,
  Column: integer; SimulatedLayer: boolean);
var
  CellLine: string;
  TimeIndex: Integer;
  TimeString: string;
  ParticleIndex: Integer;
  TimeCount: integer;
begin
  CellLine := IntToStr(Column) + ' '
    + IntToStr(Row) + ' ' + IntToStr(Layer) + ' ';
  TimeCount := FReleaseTimes.Count;
  if FTrackingDirection = tdBackward then
  begin
    TimeCount := 1;
  end;
  for TimeIndex := 0 to TimeCount - 1 do
  begin
    case FTrackingDirection of
      tdForward:
        begin
          TimeString := FReleaseTimes[TimeIndex];
        end;
      tdBackward:
        begin
          TimeString := '0';
        end;
      else Assert(False);
    end;
    if SimulatedLayer then
    begin
      for ParticleIndex := 0 to FSimulatedLocations.Count - 1 do
      begin
        Lines.Add(CellLine + FSimulatedLocations[ParticleIndex]
          + TimeString + ' # J I K X Y Z JCODE ICODE KCODE TRELEAS');
      end;
    end
    else
    begin
      for ParticleIndex := 0 to FNonSimulatedLocations.Count - 1 do
      begin
        Lines.Add(CellLine + FNonSimulatedLocations[ParticleIndex]
          + TimeString + ' # J I K X Y Z JCODE ICODE KCODE TRELEAS');
      end;
    end;
  end;
end;

end.
