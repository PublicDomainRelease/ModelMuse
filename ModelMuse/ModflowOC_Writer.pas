unit ModflowOC_Writer;

interface

uses SysUtils, PhastModelUnit, CustomModflowWriterUnit, ModflowOutputControlUnit;

type
  TOutputControlWriter = class(TCustomModflowWriter)
  private
    FOutputControl: TModflowOutputControl;
    FNameOfFile: string;
    procedure WriteDataSet0;
    procedure WriteDataSet1;
    procedure WriteDataSets2and3;
  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses ModflowUnitNumbers, ModflowTimeUnit, frmErrorsAndWarningsUnit, 
  frmProgressUnit, Forms;

{ TOutputControlWriter }

constructor TOutputControlWriter.Create(AModel: TCustomModel; EvaluationType: TEvaluationType);
begin
  inherited Create(AModel, EvaluationType);
  FOutputControl := Model.ModflowOutputControl;
end;

class function TOutputControlWriter.Extension: string;
begin
  result := '.oc';
end;

procedure TOutputControlWriter.WriteDataSet0;
var
  Index: integer;
begin
  WriteCommentLine('Output Control file created on '
    + DateToStr(Now) + ' by ' + Model.ProgramName
    + ' version ' + ModelVersion + '.');
  for Index := 0 to FOutputControl.Comments.Count - 1 do
  begin
    WriteCommentLine(FOutputControl.Comments[Index]);
  end;
end;

procedure TOutputControlWriter.WriteDataSet1;
var
  NameOfFile: string;
begin
  if FOutputControl.HeadOC.PrintInListing then
  begin
    WriteString('HEAD PRINT FORMAT ');
    WriteInteger(FOutputControl.HeadOC.PrintCode);
    NewLine;
  end;

  if FOutputControl.HeadOC.SaveInExternalFile then
  begin
    case FOutputControl.HeadOC.OutputFileType of
      oftText:
        begin
          WriteString('HEAD SAVE FORMAT ');
          WriteString(FOutputControl.HeadOC.ExternalFormat.FullFormat);
          WriteString(' LABEL');
          NewLine;
          NameOfFile := ChangeFileExt(FNameOfFile, StrFhd);
          WriteToNameFile(StrDATA, Model.UnitNumbers.UnitNumber(StrHEAD), NameOfFile, foOutput);
        end;
      oftBinary:
        begin
          NameOfFile := ChangeFileExt(FNameOfFile, StrBhd);
          WriteToNameFile(StrDATABINARY, Model.UnitNumbers.UnitNumber(StrHEAD), NameOfFile, foOutput);
        end;
      else Assert(False);
    end;
    WriteString('HEAD SAVE UNIT ');
    WriteInteger(Model.UnitNumbers.UnitNumber(StrHEAD));
    NewLine;
  end;

  if FOutputControl.DrawdownOC.PrintInListing then
  begin
    WriteString('DRAWDOWN PRINT FORMAT ');
    WriteInteger(FOutputControl.DrawdownOC.PrintCode);
    NewLine;
  end;

  if FOutputControl.DrawdownOC.SaveInExternalFile then
  begin
    case FOutputControl.DrawdownOC.OutputFileType of
      oftText:
        begin
          WriteString('DRAWDOWN SAVE FORMAT ');
          WriteString(FOutputControl.DrawdownOC.ExternalFormat.FullFormat);
          WriteString(' LABEL');
          NewLine;
          NameOfFile := ChangeFileExt(FNameOfFile, StrFdn);
          WriteToNameFile(StrDATA, Model.UnitNumbers.UnitNumber(StrDRAWDOWN), NameOfFile, foOutput);
        end;
      oftBinary:
        begin
          NameOfFile := ChangeFileExt(FNameOfFile, StrBdn);
          WriteToNameFile(StrDATABINARY, Model.UnitNumbers.UnitNumber(StrDRAWDOWN), NameOfFile, foOutput);
        end;
      else Assert(False);
    end;
    WriteString('DRAWDOWN SAVE UNIT ');
    WriteInteger(Model.UnitNumbers.UnitNumber(StrDRAWDOWN));
    NewLine;
  end;

  if FOutputControl.Compact then
  begin
    WriteString('COMPACT BUDGET AUXILIARY');
    NewLine;
  end;
end;

procedure TOutputControlWriter.WriteDataSets2and3;
const
  StressPeriodWarning = 'One or more transient stress periods are reference '
    + 'periods for calculating drawdown';
var
  StressPeriods: TModflowStressPeriods;
  HeadFrequency: integer;
  HeadFrequencyChoice: TFrequencyChoice;
  DrawdownFrequency: integer;
  DrawdownFrequencyChoice: TFrequencyChoice;
  StressPeriodIndex: integer;
  TimeStepIndex: integer;
  StressPeriod: TModflowStressPeriod;
  ShouldExportHead: boolean;
  ShouldExportDrawdown: boolean;
  SetDDREFERENCE: boolean;
  IPEROC: Integer;
  ITSOC: Integer;
  ShouldExportOverallBudget: boolean;
  ShouldExportCellBudget: boolean;
  BudgetFrequencyChoice: TFrequencyChoice;
  BudgetFrequency: integer;
  WarningMessage: string;
  function ShouldExport(FrequencyChoice: TFrequencyChoice;
    Frequency: integer): boolean;
  begin
    result := False;
    case FrequencyChoice of
      fcTimeSteps:
        begin
          result := (TimeStepIndex < Frequency)
            or ((TimeStepIndex + 1) mod Frequency = 0)
            or (TimeStepIndex = StressPeriod.NumberOfSteps - 1);
        end;
      fcStressPeriods:
        begin
          result := (TimeStepIndex = StressPeriod.NumberOfSteps - 1)
            and (((StressPeriodIndex + 1) mod Frequency = 0)
            or (StressPeriodIndex = StressPeriods.Count - 1));
        end;
      else Assert(False);
    end;
  end;
begin
  if not FOutputControl.HeadOC.PrintInListing
    and not FOutputControl.HeadOC.SaveInExternalFile
    and not FOutputControl.DrawdownOC.PrintInListing
    and not FOutputControl.DrawdownOC.SaveInExternalFile then
  begin
    Exit;
  end;

  StressPeriods := Model.ModflowFullStressPeriods;

  HeadFrequency := FOutputControl.HeadOC.Frequency;
  HeadFrequencyChoice := FOutputControl.HeadOC.FrequencyChoice;
  DrawdownFrequency := FOutputControl.DrawdownOC.Frequency;
  DrawdownFrequencyChoice := FOutputControl.DrawdownOC.FrequencyChoice;
  BudgetFrequencyChoice := FOutputControl.BudgetFrequencyChoice;
  BudgetFrequency := FOutputControl.BudgetFrequency;
  Assert(HeadFrequency >= 1);
  Assert(DrawdownFrequency >= 1);
  Assert(BudgetFrequency >= 1);

  for StressPeriodIndex := 0 to StressPeriods.Count - 1 do
  begin
    StressPeriod := StressPeriods.Items[StressPeriodIndex];
    for TimeStepIndex := 0 to StressPeriod.NumberOfSteps - 1 do
    begin
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      ShouldExportHead := False;
      if FOutputControl.HeadOC.PrintInListing
        or FOutputControl.HeadOC.SaveInExternalFile then
      begin
        ShouldExportHead := ShouldExport(HeadFrequencyChoice, HeadFrequency);
      end;

      ShouldExportDrawdown := False;
      if FOutputControl.DrawdownOC.PrintInListing
        or FOutputControl.DrawdownOC.SaveInExternalFile then
      begin
        ShouldExportDrawdown := ShouldExport(DrawdownFrequencyChoice,
          DrawdownFrequency);
      end;

      ShouldExportCellBudget := False;
      if FOutputControl.SaveCellFlows <> csfNone then
      begin
        ShouldExportCellBudget := ShouldExport(BudgetFrequencyChoice,
          BudgetFrequency);
      end;

      ShouldExportOverallBudget := ShouldExport(BudgetFrequencyChoice,
        BudgetFrequency);

      SetDDREFERENCE := StressPeriod.DrawDownReference
        and (TimeStepIndex = StressPeriod.NumberOfSteps - 1);

      if SetDDREFERENCE
        and (StressPeriod.StressPeriodType = sptTransient) then
      begin
        WarningMessage := 'Stress period: '+ IntToStr(StressPeriodIndex+1)
          + '; Starting Time: ' + FloatToStr(StressPeriod.StartTime);
        frmErrorsAndWarnings.AddWarning(Model, StressPeriodWarning, WarningMessage);
      end;

      if ShouldExportHead or ShouldExportDrawdown or SetDDREFERENCE
        or ShouldExportCellBudget or ShouldExportOverallBudget then
      begin
        // Data set 2
        frmProgressMM.AddMessage('    Writing Data Set 2 for stress period '
          + IntToStr(StressPeriodIndex+1)
          + '; time step ' + IntToStr(TimeStepIndex+1));
        IPEROC := StressPeriodIndex + 1;
        ITSOC := TimeStepIndex + 1;

        WriteString('PERIOD');
        WriteInteger(IPEROC);
        WriteString(' STEP');
        WriteInteger(ITSOC);
        if SetDDREFERENCE then
        begin
          WriteString(' DDREFERENCE');
        end;
        NewLine;

        // Data set 3
        frmProgressMM.AddMessage('    Writing Data Set 3 for stress period '
          + IntToStr(StressPeriodIndex+1)
          + '; time step ' + IntToStr(TimeStepIndex+1));
        if ShouldExportHead and FOutputControl.HeadOC.PrintInListing then
        begin
          WriteString('     PRINT HEAD');
          NewLine;
        end;
        if ShouldExportHead and FOutputControl.HeadOC.SaveInExternalFile then
        begin
          WriteString('     SAVE HEAD');
          NewLine;
        end;
        if ShouldExportDrawdown and FOutputControl.DrawdownOC.PrintInListing then
        begin
          WriteString('     PRINT DRAWDOWN');
          NewLine;
        end;
        if ShouldExportDrawdown and FOutputControl.DrawdownOC.SaveInExternalFile then
        begin
          WriteString('     SAVE DRAWDOWN');
          NewLine;
        end;
        if ShouldExportCellBudget then
        begin
          WriteString('     SAVE BUDGET');
          NewLine;
        end;
        if ShouldExportOverallBudget then
        begin
          WriteString('     PRINT BUDGET');
          NewLine;
        end;
      end;
    end;
  end;
end;

procedure TOutputControlWriter.WriteFile(const AFileName: string);
begin
  if Model.PackageGeneratedExternally(StrOC) then
  begin
    Exit;
  end;
  FNameOfFile := FileName(AFileName);
  WriteToNameFile(StrOC, Model.UnitNumbers.UnitNumber(StrOC), FNameOfFile, foInput);
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage('Writing Output Control input.');
    frmProgressMM.AddMessage('  Writing Data Set 0.');
    WriteDataSet0;
    frmProgressMM.AddMessage('  Writing Data Set 1.');
    WriteDataSet1;
    WriteDataSets2and3;
  finally
    CloseFile;
  end;
end;

end.

