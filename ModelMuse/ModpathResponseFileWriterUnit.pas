unit ModpathResponseFileWriterUnit;

interface

uses SysUtils, PhastModelUnit, ModflowPackageSelectionUnit, CustomModflowWriterUnit;

type
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
    Constructor Create(Model: TCustomModel); override;
    procedure WriteFile(const AFileName: string; NewBudgetFile: boolean);
  end;

implementation

uses
  ModflowGridUnit, LayerStructureUnit;

{ TModpathResponseFileWriter }

constructor TModpathResponseFileWriter.Create(Model: TCustomModel);
begin
 inherited Create(Model);
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

end.
