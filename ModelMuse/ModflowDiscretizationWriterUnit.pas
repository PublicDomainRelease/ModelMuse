unit ModflowDiscretizationWriterUnit;

interface

uses SysUtils, CustomModflowWriterUnit, PhastModelUnit;

type
  TModflowDiscretizationWriter = class(TCustomModflowWriter)
  private
    procedure WriteDataSet0;
    procedure WriteDataSet1;
  public
    class function Extension: string; override;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses ModflowUnitNumbers, frmProgressUnit, Forms;

{ TModflowDiscretizationWriter }

class function TModflowDiscretizationWriter.Extension: string;
begin
  result := '.dis';
end;

procedure TModflowDiscretizationWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  if PhastModel.PackageGeneratedExternally(StrDIS) then
  begin
    Exit;
  end;
  NameOfFile := FileName(AFileName);
  WriteToNameFile(StrDIS, PhastModel.UnitNumbers.UnitNumber(StrDIS),
    NameOfFile, foInput);
  OpenFile(NameOfFile);
  try
    frmProgress.AddMessage('Writing Discretization Package input.');
    frmProgress.AddMessage('  Writing Data Set 0.');
    WriteDataSet0;
    frmProgress.AddMessage('  Writing Data Set 1.');
    WriteDataSet1;

    // data set 2
    frmProgress.AddMessage('  Writing Data Set 2.');
    PhastModel.LayerStructure.WriteLAYCB(self);
    Application.ProcessMessages;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    // data set 3
    frmProgress.AddMessage('  Writing Data Set 3.');
    PhastModel.ModflowGrid.WriteDELR(self);
    Application.ProcessMessages;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    // data set 4
    frmProgress.AddMessage('  Writing Data Set 4.');
    PhastModel.ModflowGrid.WriteDELC(self);
    Application.ProcessMessages;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('  Checking column widths.');
    PhastModel.ModflowGrid.CheckColumnWidths;
    Application.ProcessMessages;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('  Checking row height.');
    PhastModel.ModflowGrid.CheckRowHeights;
    Application.ProcessMessages;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('  Checking row to column size ratios.');
    PhastModel.ModflowGrid.CheckRowToColumnRatios;
    Application.ProcessMessages;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    // data set 5
    frmProgress.AddMessage('  Writing Data Set 5.');
    PhastModel.ModflowGrid.WriteTOP(self);
    Application.ProcessMessages;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;
    PhastModel.CacheDataArrays;

    // data set 6
    frmProgress.AddMessage('  Writing Data Set 6.');
    PhastModel.ModflowGrid.WriteBOTM(self, PhastModel);
    Application.ProcessMessages;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;
    PhastModel.CacheDataArrays;

    frmProgress.AddMessage('  Checking elevations.');
    PhastModel.ModflowGrid.CheckElevations;
    Application.ProcessMessages;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    // data set 7
    frmProgress.AddMessage('  Writing Data Set 7.');
    PhastModel.ModflowFullStressPeriods.WriteStressPeriods(self);
    Application.ProcessMessages;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

  finally
    CloseFile;
  end;
end;

procedure TModflowDiscretizationWriter.WriteDataSet0;
begin
  WriteCommentLine('Discretization File created on ' + DateToStr(Now) + ' by '
    + PhastModel.ProgramName
    + ' version ' + ModelVersion + '.');
  WriteCommentLines(PhastModel.ModflowOptions.Description);
end;

procedure TModflowDiscretizationWriter.WriteDataSet1;
begin
  WriteInteger(PhastModel.LayerStructure.ModflowLayerCount);
  WriteInteger(PhastModel.ModflowGrid.RowCount);
  WriteInteger(PhastModel.ModflowGrid.ColumnCount);
  WriteInteger(PhastModel.ModflowFullStressPeriods.Count);
  WriteInteger(PhastModel.ModflowOptions.TimeUnit);
  WriteInteger(PhastModel.ModflowOptions.LengthUnit);
  WriteString(' # NLAY, NROW, NCOL, NPER, ITMUNI, LENUNI');
  NewLine;
end;

end.
