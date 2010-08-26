unit ModflowBasicWriterUnit;

interface

uses SysUtils, PhastModelUnit, DataSetUnit, CustomModflowWriterUnit;

type
  TModflowBasicWriter = class(TCustomModflowWriter)
  private
    FNameOfFile: string;
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

resourcestring
  StrFileForTheInitial = 'File for the initial heads does not exist.';
  StrWrongExtension = 'File for the initial heads has wrong extension.';

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
  RelativeFileName: string;
  UnitNumber: Integer;
  LayerIndex: Integer;
begin
  if PhastModel.ModflowOptions.InitialHeadFileName = '' then
  begin
    DataArray := PhastModel.GetDataSetByName(rsModflow_Initial_Head);
    DataSetName := 'STRT';
    WriteDataSet(DataSetName, DataArray);
    PhastModel.AddDataSetToCache(DataArray);
    PhastModel.CacheDataArrays;
  end
  else
  begin
    if not FileExists(PhastModel.ModflowOptions.InitialHeadFileName) then
    begin
      frmErrorsAndWarnings.AddError(StrFileForTheInitial, 'The file '
        + PhastModel.ModflowOptions.InitialHeadFileName
        + ' does not exist.');
      Exit;
    end;
    RelativeFileName := ExtractRelativePath(
      IncludeTrailingPathDelimiter(ExtractFileDir(FNameOfFile)),
      PhastModel.ModflowOptions.InitialHeadFileName);
    UnitNumber := PhastModel.UnitNumbers.UnitNumber(BAS_InitialHeads);
    if SameText(ExtractFileExt(PhastModel.ModflowOptions.InitialHeadFileName), '.bhd') then
    begin
      WriteToNameFile(StrDATABINARY, UnitNumber, RelativeFileName,
        foInput, True);
    end
    else
    begin
      frmErrorsAndWarnings.AddError(StrWrongExtension, 'The file '
        + PhastModel.ModflowOptions.InitialHeadFileName
        + ' must have an extension equal to ".bhd".');
      Exit;
    end;

    if XSECTION then
    begin
      WriteString('EXTERNAL');
      WriteInteger(UnitNumber);
      WriteInteger(1);
      WriteString(' (BINARY)');
      WriteInteger(IPRN_Real);
      NewLine;
    end
    else
    begin
      for LayerIndex := 0 to PhastModel.ModflowGrid.LayerCount - 1 do
      begin
        if PhastModel.LayerStructure.IsLayerSimulated(LayerIndex) then
        begin
          WriteString('EXTERNAL');
          WriteInteger(UnitNumber);
          WriteInteger(1);
          WriteString(' (BINARY)');
          WriteInteger(IPRN_Real);
          NewLine;
        end;
      end;
    end;
  end;

end;

procedure TModflowBasicWriter.WriteFile(const AFileName: string);
begin
  frmErrorsAndWarnings.RemoveErrorGroup(StrFileForTheInitial);
  frmErrorsAndWarnings.RemoveErrorGroup(StrWrongExtension);

  FNameOfFile := FileName(AFileName);
  if PhastModel.PackageGeneratedExternally(StrBAS) then
  begin
    Exit;
  end;
  WriteToNameFile(StrBAS, PhastModel.UnitNumbers.UnitNumber(StrBAS),
    FNameOfFile, foInput);
  OpenFile(FNameOfFile);
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

