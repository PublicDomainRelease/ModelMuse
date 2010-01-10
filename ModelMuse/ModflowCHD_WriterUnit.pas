unit ModflowCHD_WriterUnit;

interface

uses SysUtils, Classes, Contnrs, RbwParser, CustomModflowWriterUnit,
  PhastModelUnit, ModflowConstantHeadBoundaryUnit, ScreenObjectUnit,
  ModflowBoundaryUnit, ModflowPackageSelectionUnit, ModflowCellUnit,
  OrderedCollectionUnit, FluxObservationUnit, GoPhastTypes;

type
  TModflowCHD_Writer = class(TFluxObsWriter)
  private
    NPCHD: integer;
    MXL: integer;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSets3And4;
    procedure WriteDataSets5To7;
  protected
    function ObservationPackage: TModflowPackageSelection; override;
    function CellType: TValueCellType; override;
    class function Extension: string; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    function Package: TModflowPackageSelection; override;
    function ParameterType: TParameterType; override;
    procedure WriteParameterCells(CellList: TValueCellList; NLST: Integer;
      const VariableIdentifiers, DataSetIdentifier: string); override;
    procedure WriteCell(Cell: TValueCell;
      const DataSetIdentifier, VariableIdentifiers: string); override;
    class function ObservationExtension: string; override;
    class function ObservationOutputExtension: string; override;
    procedure WriteObservationCells(Variables, DataSets: TList;
      var Expression: TExpression; DataSet5: TStringList; AllCells: TList;
      ScreenObject: TScreenObject; ObsFactor: TObservationFactor); override;
  public
    procedure WriteFile(const AFileName: string);
    procedure WriteFluxObservationFile(const AFileName: string;
      Purpose: TObservationPurpose);
  end;

implementation

uses ModflowTimeUnit, frmErrorsAndWarningsUnit,
  ModflowTransientListParameterUnit, ModflowUnitNumbers, frmProgressUnit, 
  ModflowGridUnit;

{ TModflowCHD_Writer }

function TModflowCHD_Writer.CellType: TValueCellType;
begin
  result := TCHD_Cell;
end;

class function TModflowCHD_Writer.Extension: string;
begin
  result := '.chd';
end;

procedure TModflowCHD_Writer.WriteDataSet1;
begin
  CountParametersAndParameterCells(NPCHD, MXL);
  if NPCHD > 0 then
  begin
    WriteString('PARAMETER');
    WriteInteger(NPCHD);
    WriteInteger(MXL);
    WriteString(' # PARAMETER NPCHD MXL');
    NewLine;
  end;
end;

procedure TModflowCHD_Writer.WriteDataSet2;
var
  MXACTC: integer;
  Option: String;
begin
  CountCells(MXACTC);
  GetOption(Option);

  WriteInteger(MXACTC);
  WriteString(Option);
  WriteString(' # MXACTC');
  if Option <> '' then
  begin
    WriteString(' Option');
  end;
  NewLine
end;

procedure TModflowCHD_Writer.WriteCell(Cell: TValueCell;
  const DataSetIdentifier, VariableIdentifiers: string);
var
  CHD_Cell: TCHD_Cell;
  LocalLayer: integer;
begin
  CHD_Cell := Cell as TCHD_Cell;
  LocalLayer := PhastModel.LayerStructure.
    DataSetLayerToModflowLayer(CHD_Cell.Layer);
  WriteInteger(LocalLayer);
  WriteInteger(CHD_Cell.Row+1);
  WriteInteger(CHD_Cell.Column+1);
  WriteFloat(CHD_Cell.StartingHead);
  WriteFloat(CHD_Cell.EndingHead);
  WriteIface(CHD_Cell.IFace);
  WriteString(' # ' + DataSetIdentifier + ' Layer Row Column '
    + VariableIdentifiers);
  NewLine;
end;

procedure TModflowCHD_Writer.WriteDataSets3And4;
const
  ErrorRoot = 'One or more %s parameters have been eliminated '
    + 'because there are no cells associated with them.';
  DS3 = ' # Data Set 3: PARNAM PARTYP Parval NLST';
  DS3Instances = ' INSTANCES NUMINST';
  DS4A = ' # Data Set 4a: INSTNAM';
  DataSetIdentifier = 'Data Set 4b:';
  VariableIdentifiers = 'Shdfact Ehdfact IFACE';
begin
  WriteParameterDefinitions(DS3, DS3Instances, DS4A, DataSetIdentifier,
    VariableIdentifiers, ErrorRoot);
end;

procedure TModflowCHD_Writer.WriteDataSets5To7;
const
  D7PName =      ' # Data Set 7: PARNAM';
  D7PNameIname = ' # Data Set 7: PARNAM Iname';
  DS5 = ' # Data Set 5: ITMP NP';
  DataSetIdentifier = 'Data Set 6:';
  VariableIdentifiers = 'Shead Ehead IFACE';
begin
  WriteStressPeriods(VariableIdentifiers, DataSetIdentifier, DS5,
    D7PNameIname, D7PName);
end;

procedure TModflowCHD_Writer.WriteFile(const AFileName: string);
var
  NameOfFile: string;
  ShouldWriteFile: Boolean;
  ShouldWriteObservationFile: Boolean;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  ShouldWriteFile := not PhastModel.PackageGeneratedExternally(StrCHD);
  ShouldWriteObservationFile := ObservationPackage.IsSelected
    and not PhastModel.PackageGeneratedExternally(StrCHOB);

  if not ShouldWriteFile and not ShouldWriteObservationFile then
  begin
    Exit;
  end;

  NameOfFile := FileName(AFileName);
  if ShouldWriteFile then
  begin
    WriteToNameFile(StrCHD, PhastModel.UnitNumbers.UnitNumber(StrCHD),
      NameOfFile, foInput);
  end;
  if ShouldWriteFile or ShouldWriteObservationFile then
  begin
    Evaluate;
  end;
  if not ShouldWriteFile then
  begin
    Exit;
  end;
  OpenFile(FileName(AFileName));
  try
    frmProgress.AddMessage('Writing CHD Package input.');
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

    frmProgress.AddMessage('  Writing Data Sets 3 and 4.');
    WriteDataSets3And4;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('  Writing Data Sets 5 to 7.');
    WriteDataSets5To7;
  finally
    CloseFile;
  end;
end;

procedure TModflowCHD_Writer.WriteFluxObservationFile(const AFileName: string;
  Purpose: TObservationPurpose);
const
  DataSet1Comment = ' # Data Set 1: NQCH NQCCH NQTCH IUCHOBSV';
  DataSet2Comment = ' # Data Set 2: TOMULTCH';
  DataSet3Comment = ' # Data Set 3: NQOBCH NQCLCH';
  PackageAbbreviation = StrCHOB;
begin
  WriteFluxObsFile(AFileName, StrIUCHOBSV, PackageAbbreviation,
    DataSet1Comment, DataSet2Comment, DataSet3Comment,
    PhastModel.HeadFluxObservations, Purpose);
end;

procedure TModflowCHD_Writer.WriteObservationCells(Variables, DataSets: TList;
  var Expression: TExpression; DataSet5: TStringList; AllCells: TList;
  ScreenObject: TScreenObject; ObsFactor: TObservationFactor);
var
  List, CellList: TValueCellList;
  TimeIndex: Integer;
  CellIndex: Integer;
  ACell: TValueCell;
  ParamIndex: Integer;
  LocalParamValues: TList;
  TempCells: TList;
  CellArray: array of array of array of TValueCell;
  Grid: TModflowGrid;
begin
  TempCells := TList.Create;
  try
    Grid := PhastModel.ModflowGrid;
    SetLength(CellArray, Grid.LayerCount, Grid.RowCount, Grid.ColumnCount);

    for TimeIndex := 0 to Values.Count - 1 do
    begin
      List := Values[TimeIndex];
      for CellIndex := 0 to List.Count - 1 do
      begin
        ACell := List[CellIndex];
        if (ACell.ScreenObject = ScreenObject)
          and (CellArray[ACell.Layer, ACell.Row, ACell.Column] = nil) then
        begin
          TempCells.Add(ACell);
          CellArray[ACell.Layer, ACell.Row, ACell.Column] := ACell;
        end;
      end;
    end;
    for ParamIndex := 0 to ParamValues.Count - 1 do
    begin
      LocalParamValues := ParamValues.Objects[ParamIndex] as TList;
      for TimeIndex := 0 to LocalParamValues.Count - 1 do
      begin
        CellList := LocalParamValues[TimeIndex];
        for CellIndex := 0 to CellList.Count - 1 do
        begin
          ACell := CellList[CellIndex];
          if (ACell.ScreenObject = ScreenObject)
            and (CellArray[ACell.Layer, ACell.Row, ACell.Column] = nil) then
          begin
            TempCells.Add(ACell);
            CellArray[ACell.Layer, ACell.Row, ACell.Column] := ACell;
          end;
        end;
      end;
    end;
    for CellIndex := 0 to TempCells.Count - 1 do
    begin
      ACell := TempCells[CellIndex];
      WriteObservationCell(ACell, DataSet5, Expression, DataSets, Variables,
        ObsFactor);
    end;

  finally
    TempCells.Free;
  end;


end;

procedure TModflowCHD_Writer.WriteParameterCells(CellList: TValueCellList; NLST: Integer;
  const VariableIdentifiers, DataSetIdentifier: string);
var
  Cell: TCHD_Cell;
  CellIndex: Integer;
begin
  Cell := nil;
  for CellIndex := 0 to CellList.Count - 1 do
  begin
    Cell := CellList[CellIndex] as TCHD_Cell;
    WriteCell(Cell, DataSetIdentifier, VariableIdentifiers);
  end;
  // Dummy inactive cells to fill out data set 4b.
  // Each instance of a parameter is required to have the same
  // number of cells.  This introduces dummy boundaries to fill
  // out the list.  because Shdfact Ehdfact are set equal to zero,
  // and because the cell location is at an exsiting boundary,
  // dummy boundaries have no effect. (Zero will be added to the
  // specified head at the location of an existing specified head
  // boundary.)
  if Cell <> nil then
  begin
    for CellIndex := CellList.Count to NLST - 1 do
    begin
      WriteInteger(Cell.Layer + 1);
      WriteInteger(Cell.Row + 1);
      WriteInteger(Cell.Column + 1);
      WriteFloat(0);
      WriteFloat(0);
      WriteInteger(0);
      WriteString(
        ' # Data Set 4b: Layer Row Column Shdfact Ehdfact IFACE (Dummy boundary)');
      NewLine;
    end;
  end;
end;

function TModflowCHD_Writer.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowChdBoundary;
end;


class function TModflowCHD_Writer.ObservationExtension: string;
begin
  result := '.ob_chob';
end;

class function TModflowCHD_Writer.ObservationOutputExtension: string;
begin
  result := '.chob_out';
end;

function TModflowCHD_Writer.ObservationPackage: TModflowPackageSelection;
begin
  result := PhastModel.ModflowPackages.ChobPackage;
end;

function TModflowCHD_Writer.Package: TModflowPackageSelection;
begin
  result := PhastModel.ModflowPackages.ChdBoundary;
end;

function TModflowCHD_Writer.ParameterType: TParameterType;
begin
  result := ptCHD;
end;

end.