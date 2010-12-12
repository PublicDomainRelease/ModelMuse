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

uses ModflowUnitNumbers, frmProgressUnit, Forms, FastGEO;

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
    frmProgressMM.AddMessage('Writing Discretization Package input.');
    frmProgressMM.AddMessage('  Writing Data Set 0.');
    WriteDataSet0;
    frmProgressMM.AddMessage('  Writing Data Set 1.');
    WriteDataSet1;

    // data set 2
    frmProgressMM.AddMessage('  Writing Data Set 2.');
    PhastModel.LayerStructure.WriteLAYCB(self);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    // data set 3
    frmProgressMM.AddMessage('  Writing Data Set 3.');
    PhastModel.ModflowGrid.WriteDELR(self);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    // data set 4
    frmProgressMM.AddMessage('  Writing Data Set 4.');
    PhastModel.ModflowGrid.WriteDELC(self);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Checking column widths.');
    PhastModel.ModflowGrid.CheckColumnWidths;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Checking row height.');
    PhastModel.ModflowGrid.CheckRowHeights;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Checking row to column size ratios.');
    PhastModel.ModflowGrid.CheckRowToColumnRatios;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    // data set 5
    frmProgressMM.AddMessage('  Writing Data Set 5.');
    PhastModel.ModflowGrid.WriteTOP(self);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    PhastModel.DataArrayManager.CacheDataArrays;

    // data set 6
    frmProgressMM.AddMessage('  Writing Data Set 6.');
    PhastModel.ModflowGrid.WriteBOTM(self, PhastModel);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    PhastModel.DataArrayManager.CacheDataArrays;

    frmProgressMM.AddMessage('  Checking elevations.');
    PhastModel.ModflowGrid.CheckElevations;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    // data set 7
    frmProgressMM.AddMessage('  Writing Data Set 7.');
    PhastModel.ModflowFullStressPeriods.WriteStressPeriods(self);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

  finally
    CloseFile;
  end;
end;

procedure TModflowDiscretizationWriter.WriteDataSet0;
var
  GridAngle: Real;
  procedure WriteCorner(const CornerDesc: string; APoint: TPoint2D);
  begin
    WriteCommentLine(CornerDesc + ' (' + FloatToStr(APoint.x)
      + ', ' + FloatToStr(APoint.y) + ')');
  end;
begin
  WriteCommentLine('Discretization File created on ' + DateToStr(Now) + ' by '
    + PhastModel.ProgramName
    + ' version ' + ModelVersion + '.');
  WriteCommentLines(PhastModel.ModflowOptions.Description);

  WriteCorner('Upper left corner:', PhastModel.Grid.TwoDElementCorner(0,0));
  WriteCorner('Lower left corner:', PhastModel.Grid.TwoDElementCorner(
    0,PhastModel.Grid.RowCount));
  WriteCorner('Upper right corner:', PhastModel.Grid.TwoDElementCorner(
    PhastModel.Grid.ColumnCount,0));
  WriteCorner('Lower right corner:', PhastModel.Grid.TwoDElementCorner(
    PhastModel.Grid.ColumnCount,PhastModel.Grid.RowCount));
  GridAngle := PhastModel.Grid.GridAngle * 180 / Pi;
  WriteCommentLine('Grid angle (in degrees counterclockwise): ' + FloatToStr(GridAngle));
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
