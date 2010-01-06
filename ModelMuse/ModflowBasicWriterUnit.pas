unit ModflowBasicWriterUnit;

interface

uses SysUtils, PhastModelUnit, DataSetUnit, CustomModflowWriterUnit;

type
  TModflowBasicWriter = class(TCustomModflowWriter)
  private
    XSECTION: boolean;
    procedure CheckStartingHeads;
    procedure WriteDataSet0;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSet4;
    procedure WriteDataSet(const DataSetName: string; DataArray: TDataArray);
  protected
    class function Extension: string; override;
  public
    Constructor Create(Model: TPhastModel); override;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses frmErrorsAndWarningsUnit, ModflowUnitNumbers, frmProgressUnit;

{ TModflowBasicWriter }

procedure TModflowBasicWriter.CheckStartingHeads;
var
  DataArray: TDataArray;
  LayerIndex, RowIndex, ColIndex: Integer;
  Head, Bottom: double;
  ErrorString: string;
  ActiveArray: TDataArray;
  Active: boolean;
begin
  ErrorString := 'Initial Head is below the bottom of the layer.';
  DataArray := PhastModel.GetDataSetByName(rsModflow_Initial_Head);
  DataArray.Initialize;
  ActiveArray := PhastModel.GetDataSetByName(rsActive);
  ActiveArray.Initialize;

  for LayerIndex := 0 to PhastModel.ModflowGrid.LayerCount - 1 do
  begin
    if PhastModel.LayerStructure.IsLayerSimulated(LayerIndex) then
    begin
      for RowIndex := 0 to DataArray.RowCount - 1 do
      begin
        for ColIndex := 0 to DataArray.ColumnCount - 1 do
        begin
          Active := ActiveArray.BooleanData[LayerIndex, RowIndex, ColIndex];
          if Active then
          begin
            Head := DataArray.RealData[LayerIndex, RowIndex, ColIndex];
            Bottom := PhastModel.ModflowGrid.CellElevation[ColIndex, RowIndex, LayerIndex+1];
            if Bottom > Head then
            begin
              frmErrorsAndWarnings.AddWarning(ErrorString,
                'Layer: ' + IntToStr(LayerIndex+1)
                + '; Row: ' + IntToStr(RowIndex+1)
                + '; Column: ' + IntToStr(ColIndex+1));
            end;
          end;
        end;
      end;
    end;
  end;
  PhastModel.AddDataSetToCache(DataArray);
  PhastModel.AddDataSetToCache(ActiveArray);

end;

constructor TModflowBasicWriter.Create(Model: TPhastModel);
begin
  inherited;
  XSECTION := PhastModel.ModflowGrid.RowCount = 1;
end;

class function TModflowBasicWriter.Extension: string;
begin
  result := '.bas';
end;

procedure TModflowBasicWriter.WriteDataSet0;
begin
  WriteCommentLines(PhastModel.ModflowOptions.Description);
  WriteCommentLine('Basic Package file created on ' + DateToStr(Now) + ' by '
    + PhastModel.ProgramName
    + ' version ' + ModelVersion + '.');
end;

procedure TModflowBasicWriter.WriteDataSet1;
const
  FREE = True;
var
  CHTOCH: boolean;
  PRINTTIME: boolean;
//  SHOWPROGRESS: boolean;
begin
  CHTOCH := PhastModel.ModflowOptions.ComputeFluxesBetweenConstantHeadCells;
  PRINTTIME := PhastModel.ModflowOptions.PrintTime;
//  SHOWPROGRESS := PhastModel.ModflowOptions.ShowProgress;
  if FREE then
  begin
    WriteString('FREE ');
  end;
  if XSECTION then
  begin
    WriteString('XSECTION ');
  end;
  if CHTOCH then
  begin
    WriteString('CHTOCH ');
  end;
  if PRINTTIME then
  begin
    WriteString('PRINTTIME ');
  end;
//  if SHOWPROGRESS then
//  begin
//    WriteString('SHOWPROGRESS ');
//  end;
  WriteString(' # OPTIONS');
  NewLine;
end;

procedure TModflowBasicWriter.WriteDataSet2;
var
  DataArray: TDataArray;
  DataSetName: string;
begin
  DataArray := PhastModel.GetDataSetByName(rsActive);
  DataSetName := 'IBOUND';
  WriteDataSet(DataSetName, DataArray);
  PhastModel.AddDataSetToCache(DataArray);
  PhastModel.CacheDataArrays;
end;

procedure TModflowBasicWriter.WriteDataSet3;
begin
  WriteFloat(PhastModel.ModflowOptions.HNoFlow);
  WriteString(' # HNOFLO');
  NewLine;
end;

procedure TModflowBasicWriter.WriteDataSet4;
var
  DataArray: TDataArray;
  DataSetName: string;
begin
  DataArray := PhastModel.GetDataSetByName(rsModflow_Initial_Head);
  DataSetName := 'STRT';
  WriteDataSet(DataSetName, DataArray);
  PhastModel.AddDataSetToCache(DataArray);
  PhastModel.CacheDataArrays;
end;

procedure TModflowBasicWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  NameOfFile := FileName(AFileName);
  if PhastModel.PackageGeneratedExternally(StrBAS) then
  begin
    Exit;
  end;
  WriteToNameFile(StrBAS, PhastModel.UnitNumbers.UnitNumber(StrBAS),
    NameOfFile, foInput);
  OpenFile(NameOfFile);
  try
    frmProgress.AddMessage('Writing Basic Package input.');
    frmProgress.AddMessage('  Writing Data Set 0.');
    WriteDataSet0;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('  Writing Data Set 1.');
    WriteDataSet1;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('  Writing Data Set 2.');
    WriteDataSet2;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('  Writing Data Set 3.');
    WriteDataSet3;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('  Writing Data Set 4.');
    WriteDataSet4;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('  Checking starting heads.');
    CheckStartingHeads;
  finally
    CloseFile;
  end;
end;

procedure TModflowBasicWriter.WriteDataSet(const DataSetName: string;
  DataArray: TDataArray);
var
  LayerIndex: Integer;
begin
  Assert(DataArray <> nil);
  if XSECTION then
  begin
    WriteCrossSectionArray(DataArray, DataSetName);
  end
  else
  begin
    for LayerIndex := 0 to PhastModel.ModflowGrid.LayerCount - 1 do
    begin
      if PhastModel.LayerStructure.IsLayerSimulated(LayerIndex) then
      begin
        WriteArray(DataArray, LayerIndex, DataSetName + ' '
          + PhastModel.LayerStructure.ModflowLayerBottomDescription(LayerIndex));
      end;
    end;
  end;
end;

end.

