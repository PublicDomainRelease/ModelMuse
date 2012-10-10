unit ModpathResponseFileWriterUnit;

interface

uses SysUtils, PhastModelUnit, ModflowPackageSelectionUnit, CustomModflowWriterUnit,
  DataSetUnit;

type
  // MODPATH version 5
  TModpathResponseFileWriter = class(TCustomModflowWriter)
  private
    FOptions: TModpathSelection;
    FNewBudgetFile: Boolean;
    function GetCBF_Option(const AFileName: string): TCompositeBudgetFileOption;
    function CompositeBudgetFileSize: Int64;
    function RespondToLargeBudgetFile(
      CBF_Option: TCompositeBudgetFileOption): string;
  protected
    class function Extension: string; override;
  public
    FLargeBudgetFileResponse: string;
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    procedure WriteFile(const AFileName: string; NewBudgetFile: boolean);
  end;

  // MODPATH version 6
  TModpathSimFileWriter = class(TCustomModflowWriter)
  private
    FOptions: TModpathSelection;
    FNameFile: string;
    FTimePointOption: Integer;
    procedure WriteDataSet0;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSet4;
    procedure WriteDataSet5;
    procedure WriteDataSet6;
    procedure WriteDataSet7;
    procedure WriteDataSet8;
    procedure WriteDataSet10;
    procedure WriteDataSet22;
    procedure WriteDataSet23;
    procedure WriteDataSet24;
    procedure WriteDataSet25;
    procedure WriteDataSets26and27;
    procedure WriteDataSet28;
    procedure WriteDataSet29;
    procedure WriteDataSet30;
    procedure WriteDataSet31;
    procedure WriteDataSet(const DataSetName: string; DataArray: TDataArray);
    procedure WriteDataSets32and33;
  protected
    function PackageID_Comment(APackage: TModflowPackageSelection): string; override;
  public
    class function Extension: string; override;
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  ModflowGridUnit, LayerStructureUnit, ModpathStartingLocationsWriter,
  ModpathParticleUnit, frmProgressUnit, GoPhastTypes, Forms, frmGoPhastUnit,
  frmErrorsAndWarningsUnit;

resourcestring
  StrWritingDataSets32and33 = '  Writing Data Sets 32 and 33.';
  StrThereIsAnIllegal = 'There is an illegal value (less than or equal to ze' +
  'ro) in %s';
  StrInSAllValuesSh = 'In %s, all values should be greater than or equal to ' +
  '1';
  StrLayerRowColumn = 'Layer, Row, Column = (%0:d, %1:d, %2:d)';

{ TModpathResponseFileWriter }

constructor TModpathResponseFileWriter.Create(Model: TCustomModel; EvaluationType: TEvaluationType);
begin
 inherited Create(Model, EvaluationType);
 FOptions := Model.ModflowPackages.ModPath;
end;

class function TModpathResponseFileWriter.Extension: string;
begin
  result := '.mprsp';
end;

function TModpathResponseFileWriter.GetCBF_Option(
  const AFileName: string): TCompositeBudgetFileOption;
var
  CompositeBudgetFileName: string;
  BudgetFileName: string;
  CompositeDate: TDateTime;
  BudgetDate: TDateTime;
begin
  if FNewBudgetFile then
  begin
    result := cbfGenerateNew;
    Exit;
  end;
  CompositeBudgetFileName := ChangeFileExt(AFileName, '.cbf');
  BudgetFileName := ChangeFileExt(AFileName, StrCbcExt);
  if FileExists(CompositeBudgetFileName) and FileExists(BudgetFileName) then
  begin
    if FileAge(CompositeBudgetFileName, CompositeDate)
      and FileAge(BudgetFileName, BudgetDate) then
    begin
      if (CompositeDate > BudgetDate) then
      begin
        result := cbfUseOldFile;
      end
      else
      begin
        result := cbfGenerateNew;
      end;
    end
    else
    begin
      result := cbfGenerateNew;
    end;
  end
  else
  begin
    result := cbfGenerateNew;
  end;
end;

function TModpathResponseFileWriter.CompositeBudgetFileSize: Int64;
var
  NSTEPS: Int64;
  Grid: TModflowGrid;
  NROW: Int64;
  NLAY: Int64;
  NHLAY: Int64;
  GroupIndex: integer;
  Group: TLayerGroup;
  NRPTS: Int64;
  NREC: Int64;
  NCOL: Int64;

begin
  // based on the subroutine  CBFSIZ in the MODPATH source code.
  NSTEPS := Model.ModflowFullStressPeriods.NumberOfSteps;
  Grid := Model.ModflowGrid;
  NROW := Grid.RowCount;
  NCOL := Grid.ColumnCount;
  NLAY := Model.ModflowLayerCount;
  NHLAY := 0;
  for GroupIndex := 1 to Model.LayerStructure.Count - 1 do
  begin
    Group := Model.LayerStructure.LayerGroups[GroupIndex];
    if Group.Simulated then
    begin
      if Group.AquiferType > 0 then
      begin
        NHLAY := NHLAY + Group.LayerCount;
      end;
    end;
  end;

  NRPTS := (6*NROW*NLAY) + (NROW*NHLAY) + NROW + NLAY;
  NREC := (1 + (1+NRPTS)*NSTEPS);
  result := 4*(NCOL+1)*NREC;
end;

procedure TModpathResponseFileWriter.WriteFile(const AFileName: string;
  NewBudgetFile: boolean);
var
  NameOfFile: string;
  CBF_Option: TCompositeBudgetFileOption;
  Index: Integer;
  ComputeLocations: Boolean;
  ReferenceTime: Real;
  procedure WriteResponse;
  begin
    WriteString('@RESPONSE:');
    NewLine;
  end;
begin
  FLargeBudgetFileResponse := '';
  FNewBudgetFile := NewBudgetFile;
  NameOfFile := FileName(AFileName);
  OpenFile(NameOfFile);
  try
    // interactive input
    WriteString('@[MODPATH 5.0]');
    NewLine;

    for Index := 0 to FOptions.Comments.Count - 1 do
    begin
      WriteString('@ ' + FOptions.Comments[Index]);
      NewLine;
    end;

    // MODPATH name file
    // 'ENTER THE NAME FILE:';
    WriteString('* ENTER THE NAME FILE:');
    NewLine;
    WriteResponse;
    WriteString(ExtractFileName(AFileName));
    NewLine;

    CBF_Option := GetCBF_Option(AFileName);

    if not Model.ModflowStressPeriods.TransientModel then
    begin
      // 'DO YOU WANT TO STOP COMPUTING PATHS AFTER A SPECIFIED LENGTH OF TIME ?';
      WriteString('* DO YOU WANT TO STOP COMPUTING PATHS AFTER A SPECIFIED LENGTH OF TIME ?');
      NewLine;
      WriteResponse;
      if FOptions.StopAfterMaxTime then
      begin
        WriteString('Y');
      end
      else
      begin
        WriteString('N');
      end;
      NewLine;

      if FOptions.StopAfterMaxTime then
      begin
        // 'ENTER: MAXIMUM TRACKING TIME & TIME UNITS CONVERSION FACTOR';
        WriteString('* ENTER: MAXIMUM TRACKING TIME & TIME UNITS CONVERSION FACTOR');
        NewLine;
        WriteResponse;
        WriteFloat(FOptions.MaxTime);
        WriteFloat(1);
        NewLine;
      end;
    end
    else
    begin
      WriteString('* DEFINE A REFERENCE TIME FOR RELEASING PARTICLES ...');
      NewLine;
      WriteString('*   SELECT AN OPTION:');
      NewLine;
      WriteString('*      1 = SPECIFY BY ENTERING A STRESS PERIOD AND TIME STEP');
      NewLine;
      WriteString('*      2 = SPECIFY BY ENTERING A VALUE OF SIMULATION TIME');
      NewLine;
      //  'DEFINE A REFERENCE TIME FOR RELEASING PARTICLES ...';
      //  '  SELECT AN OPTION:';
      //  '     1 = SPECIFY BY ENTERING A STRESS PERIOD AND TIME STEP';
      //  '     2 = SPECIFY BY ENTERING A VALUE OF SIMULATION TIME';
      WriteResponse;
      WriteInteger(2);
      NewLine;

      //  '  ENTER: REFERENCE TIME  &  TIME UNITS CONVERSION FACTOR';
      // Offset the reference time by the beginning of the first stress period.
      WriteString('*   ENTER: REFERENCE TIME  &  TIME UNITS CONVERSION FACTOR');
      NewLine;
      ReferenceTime := 0;
      case FOptions.TrackingDirection of
        tdForward:
          begin
            ReferenceTime := FOptions.ReferenceTime;
//              - PhastModel.ModflowStressPeriods[0].StartTime;

//            if ReferenceTime = 0 then
//            begin
//              ReferenceTime := ReferenceTime
//                + Abs(PhastModel.ModflowStressPeriods[0].StartTime)*0.0000000001
//            end;
          end;
        tdBackward:
          begin
            ReferenceTime := FOptions.BackwardsTrackingReleaseTime
//              - PhastModel.ModflowStressPeriods[0].StartTime;
          end;
        else
          Assert(False);
      end;
      WriteResponse;
      WriteFloat(ReferenceTime);
      WriteInteger(1);
      NewLine;
      //  '  ENTER: STRESS PERIOD & TIME STEP ';
      //  '  ENTER: RELATIVE TIME WITHIN TIME STEP';
      //  '         (VALUE FROM 0 TO 1)';

      //  'STOP COMPUTING PATHS AT A SPECIFIED VALUE OF TRACKING TIME ?';
      WriteString('* STOP COMPUTING PATHS AT A SPECIFIED VALUE OF TRACKING TIME ?');
      NewLine;
      WriteResponse;
      if FOptions.StopAfterMaxTime then
      begin
        WriteString('Y');
      end
      else
      begin
        WriteString('N');
      end;
      NewLine;

      if FOptions.StopAfterMaxTime then
      begin
        //  'ENTER: MAXIMUM TRACKING TIME & TIME UNITS CONVERSION FACTOR'
        WriteString('*   ENTER: MAXIMUM TRACKING TIME & TIME UNITS CONVERSION FACTOR');
        NewLine;
        WriteResponse;
        WriteFloat(FOptions.MaxTime);
        WriteFloat(1);
        NewLine;
      end;

      //  'SPECIFY AN OPTION FOR READING HEAD AND FLOW RATE DATA:';
      //  '  1 = READ STANDARD MODFLOW UNFORMATTED FILES & GENERATE A';
      //  '      COMPOSITE BUDGET FILE';
      //  '  2 = READ FROM AN EXISTING COMPOSITE BUDGET FILE';

      WriteString('* SPECIFY AN OPTION FOR READING HEAD AND FLOW RATE DATA:');
      NewLine;
      WriteString('*   1 = READ STANDARD MODFLOW UNFORMATTED FILES & GENERATE A');
      NewLine;
      WriteString('*       COMPOSITE BUDGET FILE');
      NewLine;
      WriteString('*   2 = READ FROM AN EXISTING COMPOSITE BUDGET FILE');
      NewLine;
      // Create a new CBF file if the CBF file doesn't exist or if
      // is older the the budget file.
      WriteResponse;
      case CBF_Option of
        cbfGenerateNew: WriteInteger(1);
        cbfUseOldFile: WriteInteger(2);
        else Assert(False);
      end;
      NewLine;
      //  'ENTER A NAME FOR THE COMPOSITE BUDGET FILE (CBF):';
      // The CBF file will be in the name file.
    end;
    WriteString('* SELECT THE OUTPUT MODE:');
    NewLine;
    WriteString('*     1 = ENDPOINTS');
    NewLine;
    WriteString('*     2 = PATHLINE');
    NewLine;
    WriteString('*     3 = TIME SERIES');
    NewLine;
    //  'SELECT THE OUTPUT MODE:';
    //  '    1 = ENDPOINTS';
    //  '    2 = PATHLINE';
    //  '    3 = TIME SERIES';
    WriteResponse;
    case FOptions.OutputMode of
      mopEndpoints: WriteInteger(1);
      mopPathline: WriteInteger(2);
      mopTimeSeries: WriteInteger(3);
      else Assert(False);
    end;
    NewLine;

    if (FOptions.OutputMode in [mopPathline, mopTimeSeries]) then
    begin
      ComputeLocations := False;
      case FOptions.TimeSeriesMethod of
        tsmUniform: ComputeLocations := FOptions.TimeSeriesMaxCount > 0;
        tsmIndividual: ComputeLocations := FOptions.OutputTimes.Count > 0;
        else Assert(False);
      end;
      if FOptions.OutputMode = mopPathline then
      begin
        //  'DO YOU WANT TO COMPUTE LOCATIONS AT SPECIFIC POINTS IN TIME?';
        WriteString('* DO YOU WANT TO COMPUTE LOCATIONS AT SPECIFIC POINTS IN TIME?');
        NewLine;
        WriteResponse;
        if ComputeLocations then
        begin
          WriteString('Y');
        end
        else
        begin
          WriteString('N');
        end;
        NewLine;
      end;

      if ComputeLocations then
      begin
        //  'HOW SHOULD POINTS IN TIME BE SPECIFIED ?';
        //  '    1 = WITH A CONSTANT TIME INTERVAL';
        //  '    2 = VALUES OF TIME POINTS ARE READ FROM A FILE';
        WriteString('* HOW SHOULD POINTS IN TIME BE SPECIFIED ?');
        NewLine;
        WriteString('*     1 = WITH A CONSTANT TIME INTERVAL');
        NewLine;
        WriteString('*     2 = VALUES OF TIME POINTS ARE READ FROM A FILE');
        NewLine;
        WriteResponse;
        case FOptions.TimeSeriesMethod of
          tsmUniform: WriteInteger(1);
          tsmIndividual: WriteInteger(2);
          else Assert(False);
        end;
        NewLine;
      end;
      if ComputeLocations
        and (FOptions.TimeSeriesMethod = tsmUniform) then
      begin
        //  'ENTER: TIME INTERVAL & TIME UNITS CONVERSION FACTOR';
        WriteString('* ENTER: TIME INTERVAL & TIME UNITS CONVERSION FACTOR');
        NewLine;
        WriteResponse;
        WriteFloat(FOptions.TimeSeriesInterval);
        WriteFloat(1);
        NewLine;

        //  'ENTER THE MAXIMUM NUMBER OF TIME POINTS ALLOWED';
        WriteString('* ENTER THE MAXIMUM NUMBER OF TIME POINTS ALLOWED');
        NewLine;
        WriteResponse;
        WriteInteger(FOptions.TimeSeriesMaxCount);
        NewLine;
      end;
    end;



    //  'HOW ARE STARTING LOCATIONS TO BE ENTERED?';
    //  '    1 = FROM AN EXISTING DATA FILE';
    //  '    2 = ARRAYS OF PARTICLES WILL BE GENERATED INTERNALLY';
    WriteString('* HOW ARE STARTING LOCATIONS TO BE ENTERED?');
    NewLine;
    WriteString('*     1 = FROM AN EXISTING DATA FILE');
    NewLine;
    WriteString('*     2 = ARRAYS OF PARTICLES WILL BE GENERATED INTERNALLY');
    NewLine;
    WriteResponse;
    WriteInteger(1);
    NewLine;
    //  'ENTER NAME OF DATA FILE CONTAINING STARTING LOCATIONS:';
    //  'DO YOU WANT TO STORE INTERNALLY-GENERATED STARTING LOCATIONS ON DISK ?';
    //  'ENTER A FILE NAME:';

    //  'IN WHICH DIRECTION SHOULD PARTICLES BE TRACKED?';
    //  '    1 = FORWARD IN THE DIRECTION OF FLOW';
    //  '    2 = BACKWARDS TOWARD RECHARGE LOCATIONS';
    WriteString('* IN WHICH DIRECTION SHOULD PARTICLES BE TRACKED?');
    NewLine;
    WriteString('*     1 = FORWARD IN THE DIRECTION OF FLOW');
    NewLine;
    WriteString('*     2 = BACKWARDS TOWARD RECHARGE LOCATIONS');
    NewLine;
    WriteResponse;
    case FOptions.TrackingDirection of
      tdForward: WriteInteger(1);
      tdBackward: WriteInteger(2);
      else Assert(False);
    end;
    NewLine;

    //  'HOW SHOULD PARTICLES BE TREATED WHEN THEY ENTER CELLS WITH INTERNAL SINKS ?';
    //  '    1 = PASS THROUGH WEAK SINK CELLS';
    //  '    2 = STOP AT WEAK SINK CELLS';
    //  '    3 = STOP AT WEAK SINK CELLS THAT EXCEED A SPECIFIED STRENGTH';
    WriteString('* HOW SHOULD PARTICLES BE TREATED WHEN THEY ENTER CELLS WITH INTERNAL SINKS ?');
    NewLine;
    WriteString('*     1 = PASS THROUGH WEAK SINK CELLS');
    NewLine;
    WriteString('*     2 = STOP AT WEAK SINK CELLS');
    NewLine;
    WriteString('*     3 = STOP AT WEAK SINK CELLS THAT EXCEED A SPECIFIED STRENGTH');
    NewLine;
    WriteResponse;
    case FOptions.WeakSink of
      wsPassThrough: WriteInteger(1);
      wsStop: WriteInteger(2);
      wsThreshold: WriteInteger(3);
      else Assert(False);
    end;
    NewLine;

    if FOptions.WeakSink = wsThreshold then
    begin
      //  'ENTER A NUMBER BETWEEN 0 AND 1:';
      //  '    (0.0 => NONE OF THE INFLOW TO THE CELL IS DISCHARGED TO INTERNAL SINKS)';
      //  '    (1.0 => ALL INFLOW TO THE CELL IS DISCHARGED TO INTERNAL SINKS)';
      WriteString('* ENTER A NUMBER BETWEEN 0 AND 1:');
      NewLine;
      WriteString('*     (0.0 => NONE OF THE INFLOW TO THE CELL IS DISCHARGED TO INTERNAL SINKS)');
      NewLine;
      WriteString('*     (1.0 => ALL INFLOW TO THE CELL IS DISCHARGED TO INTERNAL SINKS)');
      NewLine;
      WriteResponse;
      WriteFloat(FOptions.WeakSinkThreshold);
      NewLine;
    end;

    WriteString('* DO YOU WANT TO STOP PARTICLES WHENEVER THEY ENTER ONE SPECIFIC ZONE ?');
    NewLine;
    //  'DO YOU WANT TO STOP PARTICLES WHENEVER THEY ENTER ONE SPECIFIC ZONE ?';
    WriteResponse;
    if FOptions.StopInZone then
    begin
      WriteString('Y');
    end
    else
    begin
      WriteString('N');
    end;
    NewLine;

    if FOptions.StopInZone then
    begin
      //  'ENTER THE ZONE NUMBER (MUST BE > 1):';
      WriteString('* ENTER THE ZONE NUMBER (MUST BE > 1):');
      NewLine;
      WriteResponse;
      WriteInteger(FOptions.StopZoneNumber);
      NewLine;

      if FOptions.OutputMode = mopEndpoints then
      begin
        //  'SPECIFY WHICH ENDPOINTS TO RECORD:';
        //  '   1 = ENDPOINT DATA RECORDED FOR ALL PARTICLES';
        //  '   2 = ENDPOINT DATA RECORDED ONLY FOR PARTICLES';
        //  '        TERMINATING IN ZONE ';
        WriteResponse;
        case FOptions.EndpointWrite of
          ewAll: WriteInteger(1);
          ewInStoppingZone: WriteInteger(2);
          else Assert(False);
        end;
        NewLine;
      end;
    end;

    FLargeBudgetFileResponse := RespondToLargeBudgetFile(CBF_Option);

    if FOptions.StopInZone then
    begin
      //  'DO YOU WANT TO CHANGE ANY OF THE ZONE CODES IN THE IBOUND ARRAY ?';
      WriteString('* DO YOU WANT TO CHANGE ANY OF THE ZONE CODES IN THE IBOUND ARRAY ?');
      NewLine;
      WriteResponse;
      WriteString('N');
      NewLine;
    end;

    //  'DO YOU WANT TO COMPUTE VOLUMETRIC BUDGETS FOR ALL CELLS ?';
    WriteString('* DO YOU WANT TO COMPUTE VOLUMETRIC BUDGETS FOR ALL CELLS ?');
    NewLine;
    WriteResponse;
    if FOptions.ComputeBudgetInAllCells then
    begin
      WriteString('Y');
    end
    else
    begin
      WriteString('N');
    end;
    NewLine;

    if FOptions.ComputeBudgetInAllCells then
    begin
      //  'SPECIFY AN ERROR TOLERANCE (IN PERCENT):';
      WriteString('* SPECIFY AN ERROR TOLERANCE (IN PERCENT):');
      NewLine;
      WriteResponse;
      WriteFloat(FOptions.ErrorTolerance);
      NewLine;
    end;

    //  ' DO YOU WANT TO CHECK DATA CELL BY CELL ?';
    WriteString('*  DO YOU WANT TO CHECK DATA CELL BY CELL ?');
    NewLine;
    WriteResponse;
    WriteString('N');
    NewLine;

    //  'SUMMARIZE FINAL STATUS OF PARTICLES IN SUMMARY.PTH FILE ?';
    WriteString('* SUMMARIZE FINAL STATUS OF PARTICLES IN SUMMARY.PTH FILE ?');
    NewLine;
    WriteResponse;
    if FOptions.Summarize then
    begin
      WriteString('Y');
    end
    else
    begin
      WriteString('N');
    end;
    NewLine;

  finally
    CloseFile
  end;

end;

function TModpathResponseFileWriter.RespondToLargeBudgetFile(
  CBF_Option: TCompositeBudgetFileOption): string;
const
  MAXSIZ = 150000000;
var
  BigFile: Boolean;
  CBFileSize: Int64;
begin
  result := '';
  if CBF_Option = cbfGenerateNew then
  begin
    CBFileSize := CompositeBudgetFileSize;
    if FOptions.MaximumSize = 0 then
    begin
      BigFile := CBFileSize > MAXSIZ;
    end
    else
    begin
      BigFile := CBFileSize > FOptions.MaximumSize;
    end;
    if BigFile then
    begin
      //        WriteString('* THIS RUN WILL GENERATE A COMPOSITE BUDGET FILE THAT CONTAINS:');
      //        NewLine;
      //        KCBFileSize := CBFileSize / 1024;
      //        MCBFileSize := KCBFileSize/ 1024;
      //        if KCBFileSize < 500 then
      //        begin
      //          WriteString('* ' + IntToStr(CBFileSize) + ' BYTES ('
      //            + FloatToStr(KCBFileSize) + ' KB)');
      //          NewLine;
      //        end
      //        else
      //        begin
      //          WriteString('* ' + IntToStr(CBFileSize) + ' BYTES ('
      //            + FloatToStr(MCBFileSize) + ' MB)');
      //          NewLine;
      //        end;
      //        WriteString('*  YOU CAN CONTINUE OR STOP NOW.');
      //        NewLine;
      //        WriteString('*  SELECT AN OPTION:');
      //        NewLine;
      //        WriteString('*       1 = CONTINUE');
      //        NewLine;
      //        WriteString('*       2 = STOP NOW, DO NOT GENERATE THE FILE');
      //        NewLine;
      //        WriteResponse;
      if FOptions.MakeBigBudgetFile then
      begin
        result := '1';
      end
      else
      begin
        result := '2';
      end;
    end;
  end;
end;

{ TModpathSimFileWriter }

constructor TModpathSimFileWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
begin
 inherited Create(Model, EvaluationType);
 FOptions := Model.ModflowPackages.ModPath;
end;

class function TModpathSimFileWriter.Extension: string;
begin
  result := '.mpsim';
end;

function TModpathSimFileWriter.PackageID_Comment(
  APackage: TModflowPackageSelection): string;
begin
  result := File_Comment(APackage.PackageIdentifier + ' Simulation file');
end;

procedure TModpathSimFileWriter.WriteDataSet(const DataSetName: string;
  DataArray: TDataArray);
var
  LayerIndex: integer;
begin
  Assert(DataArray <> nil);
  for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
  begin
    if Model.IsLayerSimulated(LayerIndex) then
    begin
      WriteArray(DataArray, LayerIndex, DataSetName + ' '
        + Model.ModflowLayerBottomDescription(LayerIndex));
    end;
  end;
end;

procedure TModpathSimFileWriter.WriteDataSet0;
begin
  WriteCommentLine(PackageID_Comment(FOptions));
  WriteCommentLines(FOptions.Comments);
end;

procedure TModpathSimFileWriter.WriteDataSet1;
begin
  frmProgressMM.AddMessage(StrWritingDataSet1);
  frmGoPhast.PhastModel.AddModelInputFile(FNameFile);
  WriteString(ExtractFileName(FNameFile));
//  WriteString(' # Data Set 1. ModpathNameFile');
  NewLine;
end;

procedure TModpathSimFileWriter.WriteDataSet10;
var
  StopTime: Real;
begin
  if FOptions.StopOption = soTrackingTime then
  begin
    frmProgressMM.AddMessage(StrWritingDataSet10);
    StopTime := FOptions.StopTime;
    WriteFloat(StopTime);
    WriteString(' # Data Set 10: StopTime');
    NewLine;
  end;
end;

procedure TModpathSimFileWriter.WriteDataSet2;
var
  AFileName: string;
begin
  frmProgressMM.AddMessage(StrWritingDataSet2);
  AFileName := ChangeFileExt(FNameFile, '.mplst');
  frmGoPhast.PhastModel.AddFileToArchive(AFileName);
  AFileName := ExtractFileName(AFileName);
  WriteString(AFileName);
//  WriteString(' # Data Set 2. ModpathListingFile');
  NewLine;
end;

procedure TModpathSimFileWriter.WriteDataSet22;
var
  AFileName: string;
begin
  frmProgressMM.AddMessage(StrWritingDataSet22);
  AFileName := ChangeFileExt(FNameFile,
    TModpathStartingLocationsWriter.Extension);
  frmGoPhast.PhastModel.AddModelInputFile(AFileName);
  AFileName := ExtractFileName(AFileName);
  WriteString(AFileName);
//  WriteString(' # Data Set 22. StartingLocationsFile');
  NewLine;
end;

procedure TModpathSimFileWriter.WriteDataSet23;
var
  TimePointCount: integer;
begin
  if FTimePointOption in [2,3] then
  begin
    frmProgressMM.AddMessage(StrWritingDataSet23);
    TimePointCount := -1;
    case FTimePointOption of
      2:
        begin
          TimePointCount := FOptions.TimeSeriesMaxCount;
        end;
      3:
        begin
          TimePointCount := FOptions.OutputTimes.Count;
        end;
      else Assert(False);
    end;
    WriteInteger(TimePointCount);
    WriteString(' # Data Set 23: TimePointCount');
    NewLine;
  end;
end;

procedure TModpathSimFileWriter.WriteDataSet24;
var
  ReleaseTimeIncrement: Double;
begin
  if FTimePointOption = 2 then
  begin
    frmProgressMM.AddMessage(StrWritingDataSet24);
    ReleaseTimeIncrement := FOptions.TimeSeriesInterval;
    WriteFloat(ReleaseTimeIncrement);
    WriteString(' # Data Set 24: ReleaseTimeIncrement');
    NewLine;
  end;
end;

procedure TModpathSimFileWriter.WriteDataSet25;
var
  Index: Integer;
  Item: TModpathTimeItem;
begin
  if FTimePointOption = 3 then
  begin
    frmProgressMM.AddMessage(StrWritingDataSet25);
    for Index := 0 to FOptions.OutputTimes.Count - 1 do
    begin
      Item := FOptions.
        OutputTimes.Items[Index] as TModpathTimeItem;
      WriteFloat(Item.Time);
      if Index = FOptions.OutputTimes.Count - 1 then
      begin
        WriteString(' # Data Set 25: TimePoints');
      end;
      if (((Index + 1) mod 10) = 0) or (Index = FOptions.OutputTimes.Count - 1) then
      begin
        NewLine;
      end;
    end;
  end;
end;

procedure TModpathSimFileWriter.WriteDataSet28;
var
  AFileName: string;
begin
  if FOptions.BudgetChecking = bcTrace then
  begin
    frmProgressMM.AddMessage(StrWritingDataSet28);
    AFileName := ChangeFileExt(FNameFile, '.trace');
    frmGoPhast.PhastModel.AddFileToArchive(AFileName);
    AFileName := ExtractFileName(AFileName);
    WriteString(AFileName);
//    WriteString(' # Data Set 29. TraceFile');
    NewLine;
  end;
end;

procedure TModpathSimFileWriter.WriteDataSet29;
var
  TraceID: integer;
begin
  if FOptions.BudgetChecking = bcTrace then
  begin
    frmProgressMM.AddMessage(StrWritingDataSet29);
    TraceID := FOptions.TraceID;
    WriteInteger(TraceID);
    WriteString(' # Data Set 29: TraceID');
    NewLine;
  end;
end;

procedure TModpathSimFileWriter.WriteDataSet3;
const
  ReferenceTimeOption = 1;
  ParticleGenerationOption = 2;
var
  SimulationType: integer;
  TrackingDirection: Integer;
  WeakSinkOption: integer;
  WeakSouceOption: integer;
  StopOption: integer;
  BudgetOutputOption: Integer;
  ZoneArrayOption: Integer;
  RetardationOption: Integer;
  AdvectiveObservationsOption: Integer;
begin
  frmProgressMM.AddMessage(StrWritingDataSet3);
  SimulationType := Ord(FOptions.OutputMode) + 1;
  TrackingDirection := Ord(FOptions.TrackingDirection) + 1;
  WeakSinkOption := Ord(FOptions.WeakSink) + 1;
  if WeakSinkOption > 2 then
  begin
    WeakSinkOption := 2;
  end;
  WeakSouceOption := Ord(FOptions.WeakSource) + 1;
  if WeakSouceOption > 2 then
  begin
    WeakSouceOption := 2;
  end;
  StopOption := Ord(FOptions.StopOption) + 1;
  FTimePointOption := 0;
  if (FOptions.OutputMode in [mopPathline, mopTimeSeries]) then
  begin
    case FOptions.TimeSeriesMethod of
      tsmUniform:
        begin
          if FOptions.TimeSeriesMaxCount > 0 then
          begin
            FTimePointOption := 2;
          end
          else
          begin
            FTimePointOption := 1;
          end;
        end;
      tsmIndividual:
        begin
          if FOptions.OutputTimes.Count > 0 then
          begin
            FTimePointOption := 3;
          end
          else
          begin
            FTimePointOption := 1;
          end;
        end;
      else Assert(False);
    end;
  end
  else
  begin
    FTimePointOption := 1;
  end;
  BudgetOutputOption := Ord(FOptions.BudgetChecking) + 1;
  ZoneArrayOption := Ord(FOptions.StopInZone) + 1;
  RetardationOption := Ord(FOptions.RetardationOption) + 1;
  if FOptions.OutputMode = mopTimeSeries then
  begin
    AdvectiveObservationsOption := Ord(FOptions.AdvectiveObservations) + 1;
  end
  else
  begin
    AdvectiveObservationsOption := 1;
  end;
  WriteInteger(SimulationType);
  WriteInteger(TrackingDirection);
  WriteInteger(WeakSinkOption);
  WriteInteger(WeakSouceOption);
  WriteInteger(ReferenceTimeOption);
  WriteInteger(StopOption);
  WriteInteger(ParticleGenerationOption);
  WriteInteger(FTimePointOption);
  WriteInteger(BudgetOutputOption);
  WriteInteger(ZoneArrayOption);
  WriteInteger(RetardationOption);
  WriteInteger(AdvectiveObservationsOption);
  WriteString(' # Data Set 3: SimulationType, TrackingDirection, '
    + 'WeakSinkOption, WeakSouceOption, ReferenceTimeOption, StopOption, '
    + 'ParticleGenerationOption, TimePointOption, BudgetOutputOption, '
    + 'ZoneArrayOption, RetardationOption, AdvectiveObservationsOption');
  NewLine;
end;

procedure TModpathSimFileWriter.WriteDataSet30;
var
  StopZone: integer;
begin
  if FOptions.StopInZone then
  begin
    frmProgressMM.AddMessage(StrWritingDataSet30);
    StopZone := FOptions.StopZoneNumber;
    WriteInteger(StopZone);
    WriteString(' # Data Set 30: StopZone');
    NewLine;

    // removed from code
//    if StopZone >= 1 then
//    begin
//      // undocumented. See lines 346-366 of MP6MPBAS1.FOR
//      StopZone := FOptions.StopZoneNumber;
//      WriteInteger(StopZone);
//      WriteString(' # Data Set 30a: StopZone');
//      NewLine;
//    end;
  end;
end;

procedure TModpathSimFileWriter.WriteDataSet31;
var
  ZoneDataArray: TDataArray;
begin
  if FOptions.StopInZone then
  begin
    frmProgressMM.AddMessage(StrWritingDataSet31);
    ZoneDataArray := Model.DataArrayManager.GetDataSetByName(StrModpathZone);
    WriteDataSet('Data Set 31: Zone', ZoneDataArray);
  end;
end;

procedure TModpathSimFileWriter.WriteDataSet4;
var
  AFileName: string;
begin
  frmProgressMM.AddMessage(StrWritingDataSet4);
  AFileName := ChangeFileExt(FNameFile, '.end');
  frmGoPhast.PhastModel.AddFileToArchive(AFileName);
  AFileName := ExtractFileName(AFileName);
  WriteString(AFileName);
//  WriteString(' # Data Set 4. EndpointFile');
  NewLine;
end;

procedure TModpathSimFileWriter.WriteDataSet5;
var
  AFileName: string;
begin
  if FOptions.OutputMode = mopPathline then
  begin
    frmProgressMM.AddMessage(StrWritingDataSet5);
    AFileName := ChangeFileExt(FNameFile, '.path');
    frmGoPhast.PhastModel.AddFileToArchive(AFileName);
    AFileName := ExtractFileName(AFileName);
    WriteString(AFileName);
//    WriteString(' # Data Set 5. PathlineFile');
    NewLine;
  end;
end;

procedure TModpathSimFileWriter.WriteDataSet6;
var
  AFileName: string;
begin
  if FOptions.OutputMode = mopTimeSeries then
  begin
    frmProgressMM.AddMessage(StrWritingDataSet6);
    AFileName := ChangeFileExt(FNameFile, '.ts');
    frmGoPhast.PhastModel.AddFileToArchive(AFileName);
    AFileName := ExtractFileName(AFileName);
    WriteString(AFileName);
//    WriteString(' # Data Set 6. TimeSeriesFile');
    NewLine;
  end;
end;

procedure TModpathSimFileWriter.WriteDataSet7;
var
  AFileName: string;
begin
  if (FOptions.AdvectiveObservations = aoAll)
    and (FOptions.OutputMode = mopTimeSeries) then
  begin
    frmProgressMM.AddMessage(StrWritingDataSet7);
    AFileName := ChangeFileExt(FNameFile, '.advobs');
    frmGoPhast.PhastModel.AddFileToArchive(AFileName);
    AFileName := ExtractFileName(AFileName);
    WriteString(AFileName);
//    WriteString(' # Data Set 7. AdvectionObservationsFile');
    NewLine;
  end;
end;

procedure TModpathSimFileWriter.WriteDataSet8;
var
  ReferenceTime: Real;
begin
  // ReferenceTimeOption is always set to 1
  // so this data set is always exported and
  // data set 9 is never exported.
  frmProgressMM.AddMessage(StrWritingDataSet8);
  ReferenceTime := FOptions.ReferenceTime
    - Model.ModflowStressPeriods[0].StartTime;
  WriteFloat(ReferenceTime);
  WriteString(' # Data Set 8: ReferenceTime');
  NewLine;
end;

procedure TModpathSimFileWriter.WriteDataSets26and27;
const
  Grid = 1;
var
  DataArray: TDataArray;
  CellBudgetCount: integer;
  LayerIndex: integer;
  RowIndex: integer;
  ColIndex: integer;
begin
  if FOptions.BudgetChecking = bcList then
  begin
    frmProgressMM.AddMessage(StrWritingDataSet26);
    DataArray := Model.DataArrayManager.GetDataSetByName(KModpathBudget);
    DataArray.Initialize;
    CellBudgetCount := 0;
    for LayerIndex := 0 to DataArray.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex) then
      begin
        for RowIndex := 0 to DataArray.RowCount - 1 do
        begin
          for ColIndex := 0 to DataArray.ColumnCount - 1 do
          begin
            if DataArray.BooleanData[LayerIndex,RowIndex,ColIndex] then
            begin
              Inc(CellBudgetCount);
            end;
          end;
        end;
      end;
    end;
    WriteInteger(CellBudgetCount);
    WriteString(' # Data Set 26: CellBudgetCount');
    NewLine;

    frmProgressMM.AddMessage(StrWritingDataSet27);
    for LayerIndex := 0 to DataArray.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex) then
      begin
        for RowIndex := 0 to DataArray.RowCount - 1 do
        begin
          for ColIndex := 0 to DataArray.ColumnCount - 1 do
          begin
            if DataArray.BooleanData[LayerIndex,RowIndex,ColIndex] then
            begin
              WriteInteger(Grid);
              WriteInteger(LayerIndex+1);
              WriteInteger(RowIndex+1);
              WriteInteger(ColIndex+1);
              WriteString(' # Data Set 27: Grid, Layer, Row, Column');
              NewLine;
            end;
          end;
        end;
      end;
    end;

  end;
end;

procedure TModpathSimFileWriter.WriteDataSets32and33;
var
  RetardationDataArray: TDataArray;
  LayerIndex: Integer;
  DataSetName: string;
  ActiveDataArray: TDataArray;
  RowIndex: Integer;
  ColIndex: Integer;
  AValue: Double;
  ErrorRoot: string;
  WarningRoot: string;
begin
  if FOptions.RetardationOption = roUsed then
  begin
    frmProgressMM.AddMessage(StrWritingDataSets32and33);
    RetardationDataArray := Model.DataArrayManager.GetDataSetByName(KModpathRetardation);
    Assert(RetardationDataArray <> nil);
    for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
    begin
      if Model.IsLayerSimulated(LayerIndex) then
      begin
        DataSetName := 'Data Set 32: RetardationFactor ';
      end
      else
      begin
        DataSetName := 'Data Set 32: RetardationFactorCB ';
      end;
      WriteArray(RetardationDataArray, LayerIndex, DataSetName
        + Model.ModflowLayerBottomDescription(LayerIndex));
    end;

    ErrorRoot := Format(StrThereIsAnIllegal,
      [RetardationDataArray.DisplayName]);
    WarningRoot := Format(StrInSAllValuesSh,
      [RetardationDataArray.DisplayName]);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, ErrorRoot);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, WarningRoot);
    ActiveDataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
    for LayerIndex := 0 to Model.ModflowGrid.LayerCount - 1 do
    begin
      for RowIndex := 0 to Model.ModflowGrid.RowCount - 1 do
      begin
        for ColIndex := 0 to Model.ModflowGrid.ColumnCount - 1 do
        begin
          if ActiveDataArray.BooleanData[LayerIndex,RowIndex,ColIndex] then
          begin
            AValue := RetardationDataArray.RealData[LayerIndex,RowIndex,ColIndex];
            if AValue <= 0 then
            begin
              frmErrorsAndWarnings.AddError(Model, ErrorRoot,
                Format(StrLayerRowColumn,
                [LayerIndex+1, RowIndex+1, ColIndex+1]));
            end
            else if AValue < 1 then
            begin
              frmErrorsAndWarnings.AddWarning(Model, WarningRoot,
                Format(StrLayerRowColumn,
                [LayerIndex+1, RowIndex+1, ColIndex+1]));
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TModpathSimFileWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  FNameFile := AFileName;
  NameOfFile := FileName(AFileName);
  OpenFile(NameOfFile);
  try
    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet1;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet2;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet3;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet4;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet5;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet6;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet7;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet8;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet10;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet22;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet23;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet24;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet25;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSets26and27;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet28;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet29;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet30;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet31;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSets32and33;

  finally
    CloseFile
  end;
  Model.AddModelInputFile(NameOfFile);
end;

end.
