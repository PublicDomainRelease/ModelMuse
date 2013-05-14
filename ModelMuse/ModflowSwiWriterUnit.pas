unit ModflowSwiWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit, Classes,
  PhastModelUnit;

type
  TSwiWriter = class(TCustomPackageWriter)
  private
    FNameOfFile: string;
    FSwiPackage: TSwiPackage;
    FObservations: TStringList;
    procedure EvaluateObservations;
    procedure WriteDataSet1;
    procedure WriteDataSet2a;
    procedure WriteDataSet2b;
    procedure WriteDataSet3a;
    procedure WriteDataSet3b;
    procedure WriteDataSet4;
    procedure WriteDataSet5;
    procedure WriteDataSet6;
    procedure WriteDataSet7;
    procedure WriteDataSet8;
  protected
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  ModflowUnitNumbers, SysUtils, frmProgressUnit, DataSetUnit,
  AbstractGridUnit, frmErrorsAndWarningsUnit, GoPhastTypes;

resourcestring
  StrTheSWIObservations = 'The SWI observations at the following cells have ' +
  'observation names that are longer than maximum allowed number of characte' +
  'rs. The name will be truncated. (Layer, Row, Column)';
  Str0d1d2d = '(%0:d, %1:d, %2:d)';
  Str0s1d2d3d = '%0:s, %1:d %2:d %3:d # Data set 8 OBSNAM LAYER ROW COLUMN';
  StrZETASurface0dLa = 'ZETA Surface %0:d Layer %1:d';
  StrSSZLayerD = 'SSZ Layer %d';
  StrISOURCELayerD = 'ISOURCE Layer %d';

{ TSwiWriter }

constructor TSwiWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FObservations:= TStringList.Create;
end;

destructor TSwiWriter.Destroy;
begin
  FObservations.Free;
  inherited;
end;

procedure TSwiWriter.EvaluateObservations;
const
  MaxObsNameLength = 12;
var
  DataArray: TDataArray;
  LayerIndex: Integer;
  Grid: TCustomModelGrid;
  Layer: integer;
  RowIndex: Integer;
  ColumnIndex: Integer;
  ObsName: string;
begin
  DataArray := Model.DataArrayManager.GetDataSetByName(KSWI_Observation_Name);
  Assert(DataArray <> nil);
  DataArray.Initialize;
  Grid := Model.Grid;
  for LayerIndex := 0 to Grid.LayerCount - 1 do
  begin
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      Layer := Model.DataSetLayerToModflowLayer(LayerIndex);
      for RowIndex := 0 to Grid.RowCount - 1 do
      begin
        for ColumnIndex := 0 to Grid.ColumnCount - 1 do
        begin
          ObsName := Trim(DataArray.StringData[LayerIndex, RowIndex, ColumnIndex]);
          if ObsName <> '' then
          begin
            if Length(ObsName) > MaxObsNameLength then
            begin
              frmErrorsAndWarnings.AddWarning(Model, StrTheSWIObservations,
                Format(Str0d1d2d, [LayerIndex+1, RowIndex+1,
                ColumnIndex+1]));
              ObsName := Copy(ObsName, 1, MaxObsNameLength);
            end;
            ObsName := StringReplace(ObsName, ' ', '_', [rfReplaceAll, rfIgnoreCase]);

            FObservations.Add(Format(Str0s1d2d3d,
              [ObsName, Layer, RowIndex+1, ColumnIndex+1]))
          end;
        end;
      end;
    end;
  end;
end;

class function TSwiWriter.Extension: string;
begin
  result := '.swi';
end;

function TSwiWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.SwiPackage;
end;

procedure TSwiWriter.WriteDataSet1;
var
  NSRF, ISTRAT, NOBS, ISWIZT, ISWIBD, ISWIOBS: integer;
  OPTIONS: string;
  ObsFileName: string;
  ZetaFileName: string;
begin
  WriteString('# Data Set 1: NSRF, ISTRAT, NOBS, ISWIZT, ISWIBD, ISWIOBS');
  if FSwiPackage.Adaptive then
  begin
    WriteString(' [Options]');
  end;
  NewLine;
  NSRF := FSwiPackage.NumberOfSurfaces;
  ISTRAT := Ord(FSwiPackage.DensityChoice);
  ISWIOBS := 0;
  case FSwiPackage.ObsChoice of
    socNone: ISWIOBS := 0;
    socAscii: ISWIOBS := Model.UnitNumbers.UnitNumber(StrSWI_Obs);
    socBinary: ISWIOBS := -Model.UnitNumbers.UnitNumber(StrSWI_Obs);
    else Assert(False);
  end;
  if FSwiPackage.SaveZeta then
  begin
    ISWIZT := Model.UnitNumbers.UnitNumber(StrSWI_Zeta);
  end
  else
  begin
    ISWIZT := 0
  end;
  GetFlowUnitNumber(ISWIBD);
  if ISWIOBS <> 0 then
  begin
    EvaluateObservations;
    NOBS := FObservations.Count;
    if NOBS = 0 then
    begin
      ISWIOBS := 0;
    end;
  end
  else
  begin
    NOBS := 0
  end;

  if ISWIOBS <> 0 then
  begin
    ObsFileName := ChangeFileExt(FNameOfFile, '.swi_obs');
    if ISWIOBS > 0 then
    begin
      WriteToNameFile(StrData, ISWIOBS, ObsFileName, foOutput);
    end
    else
    begin
      WriteToNameFile(StrDataBinary, -ISWIOBS, ObsFileName, foOutput);
    end;
  end;
  if ISWIZT <> 0 then
  begin
    ZetaFileName := ChangeFileExt(FNameOfFile, strZeta);
    WriteToNameFile(StrDataBinary, ISWIZT, ZetaFileName, foOutput);
  end;
  if FSwiPackage.Adaptive then
  begin
    OPTIONS := ' ADAPTIVE';
  end
  else
  begin
    OPTIONS := '';
  end;
  WriteInteger(NSRF);
  WriteInteger(ISTRAT);
  WriteInteger(NOBS);
  WriteInteger(ISWIZT);
  WriteInteger(ISWIBD);
  WriteInteger(ISWIOBS);
  WriteString(OPTIONS);
  NewLine;
end;

procedure TSwiWriter.WriteDataSet2a;
var
  NSOLVER, IPRSOL, MUTSOL: integer;
begin
  NSOLVER := Ord(FSwiPackage.Solver) + 1;
  IPRSOL := FSwiPackage.SolverPrintoutInterval;
  MUTSOL := Ord(FSwiPackage.SolverPrintChoice);
  WriteInteger(NSOLVER);
  WriteInteger(IPRSOL);
  WriteInteger(MUTSOL);
  WriteString(' # Data Set 2a: NSOLVER, IPRSOL, MUTSOL');
  NewLine;
end;

procedure TSwiWriter.WriteDataSet2b;
var
  MXITER, ITER1, NPCOND: Integer;
  ZCLOSE, RCLOSE, RELAX: double;
  NBPOL: Integer;
  DAMP, DAMPT: double;
begin
  if FSwiPackage.Solver = ssPCG then
  begin
    MXITER := FSwiPackage.MXITER;
    ITER1 := FSwiPackage.ITER1;
    NPCOND := Ord(FSwiPackage.NPCOND)+1;
    ZCLOSE := FSwiPackage.ZCLOSE.Value;
    RCLOSE := FSwiPackage.RCLOSE.Value;
    RELAX := FSwiPackage.RELAX.Value;
    NBPOL := Ord(FSwiPackage.NBPOL)+1;
    DAMP := -FSwiPackage.DAMP.Value;
    DAMPT := FSwiPackage.DAMPT.Value;

    WriteInteger(MXITER);
    WriteInteger(ITER1);
    WriteInteger(NPCOND);
    WriteFloat(ZCLOSE);
    WriteFloat(RCLOSE);
    WriteFloat(RELAX);
    WriteInteger(NBPOL);
    WriteFloat(DAMP);
    WriteFloat(DAMPT);

    WriteString(' # Data Set 2b: MXITER ITER1 NPCOND ZCLOSE RCLOSE RELAX NBPOL DAMP DAMPT');
    NewLine;
  end;
end;

procedure TSwiWriter.WriteDataSet3a;
var
  TOESLOPE, TIPSLOPE, ALPHA, BETA: double;
begin
  TOESLOPE := FSwiPackage.ToeSlope.Value;
  TIPSLOPE := FSwiPackage.TipSlope.Value;
  ALPHA := FSwiPackage.Alpha.Value;
  BETA := FSwiPackage.Beta.Value;

  WriteFloat(TOESLOPE);
  WriteFloat(TIPSLOPE);
  WriteFloat(ALPHA);
  WriteFloat(BETA);

  WriteString(' # Data Set 3a: TOESLOPE TIPSLOPE [ALPHA] [BETA]');
  NewLine;
end;

procedure TSwiWriter.WriteDataSet3b;
var
  NADPTMX, NADPTMN: Integer;
  ADPTFCT: double;
begin
  if FSwiPackage.Adaptive then
  begin
    NADPTMX := FSwiPackage.MaxAdaptiveTimeSteps;
    NADPTMN := FSwiPackage.MinAdaptiveTimeSteps;
    ADPTFCT := FSwiPackage.AdaptiveFactor.Value;
    WriteInteger(NADPTMX);
    WriteInteger(NADPTMN);
    WriteFloat(ADPTFCT);
    WriteString(' # Data Set 3b: NADPTMX NADPTMN ADPTFCT');
    NewLine;
  end;
end;

procedure TSwiWriter.WriteDataSet4;
var
  Index: Integer;
begin
  WriteU2DRELHeader(' # Data Set 4: NU');
  case FSwiPackage.DensityChoice of
    dcLinear: Assert(FSwiPackage.ZoneDimensionlessDensities.Count =
      FSwiPackage.NumberOfSurfaces + 2);
    dcZoned: Assert(FSwiPackage.ZoneDimensionlessDensities.Count =
      FSwiPackage.NumberOfSurfaces + 1);
    else Assert(False);
  end;
  for Index := 0 to FSwiPackage.ZoneDimensionlessDensities.Count - 1 do
  begin
    WriteFloat(FSwiPackage.ZoneDimensionlessDensities[Index].Value);
    if (Index+1) mod 10 = 0 then
    begin
      NewLine;
    end;
  end;
  if FSwiPackage.ZoneDimensionlessDensities.Count mod 10 <> 0 then
  begin
    NewLine;
  end;
end;

procedure TSwiWriter.WriteDataSet5;
var
  SurfaceIndex: Integer;
  DataArrayName: string;
  ADataArray: TDataArray;
  LayerIndex: Integer;
  Layer: Integer;
begin
  for SurfaceIndex := 1 to FSwiPackage.NumberOfSurfaces do
  begin
    DataArrayName := KActive_Surface_Elevation + IntToStr(SurfaceIndex);
    ADataArray := Model.DataArrayManager.GetDataSetByName(DataArrayName);
    Assert(ADataArray <> nil);
    ADataArray.Initialize;
    for LayerIndex := 0 to Model.Grid.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex) then
      begin
        Layer := Model.DataSetLayerToModflowLayer(LayerIndex);
        WriteArray(ADataArray, LayerIndex,
          Format(StrZETASurface0dLa, [SurfaceIndex, Layer]),
          LayerIndex = Model.Grid.LayerCount - 1);
      end;
    end;
  end;
end;

procedure TSwiWriter.WriteDataSet6;
var
  ADataArray: TDataArray;
  LayerIndex: Integer;
  Layer: Integer;
begin
  ADataArray := Model.DataArrayManager.GetDataSetByName(KEffectivePorosity);
  Assert(ADataArray <> nil);
  ADataArray.Initialize;
  for LayerIndex := 0 to Model.Grid.LayerCount - 1 do
  begin
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      Layer := Model.DataSetLayerToModflowLayer(LayerIndex);
      WriteArray(ADataArray, LayerIndex,
        Format(StrSSZLayerD, [Layer]),
        LayerIndex = Model.Grid.LayerCount - 1);
    end;
  end;
end;

procedure TSwiWriter.WriteDataSet7;
var
  ADataArray: TDataArray;
  LayerIndex: Integer;
  Layer: Integer;
begin
  ADataArray := Model.DataArrayManager.GetDataSetByName(KSourceFluidDensityZone);
  Assert(ADataArray <> nil);
  ADataArray.Initialize;
  for LayerIndex := 0 to Model.Grid.LayerCount - 1 do
  begin
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      Layer := Model.DataSetLayerToModflowLayer(LayerIndex);
      WriteArray(ADataArray, LayerIndex,
        Format(StrISOURCELayerD, [Layer]),
        LayerIndex = Model.Grid.LayerCount - 1);
    end;
  end;
end;

procedure TSwiWriter.WriteDataSet8;
var
  LineIndex: Integer;
begin
  for LineIndex := 0 to FObservations.Count - 1 do
  begin
    WriteString(FObservations[LineIndex]);
    NewLine;
  end;
end;

procedure TSwiWriter.WriteFile(const AFileName: string);
begin
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTheSWIObservations);
  FSwiPackage := Model.ModflowPackages.SwiPackage;
  if not (Model.ModelSelection in [msModflow, msModflowNWT]) then
  begin
    Exit;
  end;
  if not FSwiPackage.IsSelected then
  begin
    Exit;
  end;
  if Model.PackageGeneratedExternally(StrSWI) then
  begin
    Exit;
  end;

  FNameOfFile := FileName(AFileName);
  WriteToNameFile(StrSWI, Model.UnitNumbers.UnitNumber(StrSWI),
    FNameOfFile, foInput);
  OpenFile(FNameOfFile);
  try
      frmProgressMM.AddMessage('Writing SWI Package input.');

      WriteDataSet0;
      WriteDataSet1;
      WriteDataSet2a;
      WriteDataSet2b;
      WriteDataSet3a;
      WriteDataSet3b;
      WriteDataSet4;
      WriteDataSet5;
      WriteDataSet6;
      WriteDataSet7;
      WriteDataSet8;

  finally
    CloseFile;
  end;

end;

end.
