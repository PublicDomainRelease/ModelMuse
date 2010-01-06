unit ModflowGHB_WriterUnit;

interface

uses SysUtils, Classes, Contnrs, CustomModflowWriterUnit, ModflowGhbUnit,
  PhastModelUnit, ScreenObjectUnit, ModflowBoundaryUnit, ModflowCellUnit,
  ModflowPackageSelectionUnit, OrderedCollectionUnit, GoPhastTypes,
  ModflowBoundaryDisplayUnit, ModflowTransientListParameterUnit;

type
  TModflowGHB_Writer = class(TFluxObsWriter)
  private
    NPGHB: integer;
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
  public
    procedure WriteFile(const AFileName: string);
    procedure WriteFluxObservationFile(const AFileName: string;
      Purpose: TObservationPurpose);
  end;

implementation

uses ModflowTimeUnit, frmErrorsAndWarningsUnit, ModflowUnitNumbers, 
  frmProgressUnit;

{ TModflowGHB_Writer }

function TModflowGHB_Writer.CellType: TValueCellType;
begin
  result := TGhb_Cell;
end;

class function TModflowGHB_Writer.Extension: string;
begin
  result := '.ghb';
end;

function TModflowGHB_Writer.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowGhbBoundary;
end;

class function TModflowGHB_Writer.ObservationExtension: string;
begin
  result := '.ob_gbob';
end;

class function TModflowGHB_Writer.ObservationOutputExtension: string;
begin
  result := '.gbob_out';
end;

function TModflowGHB_Writer.ObservationPackage: TModflowPackageSelection;
begin
  result := PhastModel.ModflowPackages.GbobPackage;
end;

function TModflowGHB_Writer.Package: TModflowPackageSelection;
begin
  result := PhastModel.ModflowPackages.GhbBoundary;
end;

function TModflowGHB_Writer.ParameterType: TParameterType;
begin
  result := ptGHB;
end;

procedure TModflowGHB_Writer.WriteCell(Cell: TValueCell;
  const DataSetIdentifier, VariableIdentifiers: string);
var
  GHB_Cell: TGhb_Cell;
  LocalLayer: integer;
begin
  GHB_Cell := Cell as TGhb_Cell;
  LocalLayer := PhastModel.LayerStructure.
    DataSetLayerToModflowLayer(GHB_Cell.Layer);
  WriteInteger(LocalLayer);
  WriteInteger(GHB_Cell.Row+1);
  WriteInteger(GHB_Cell.Column+1);
  WriteFloat(GHB_Cell.BoundaryHead);
  WriteFloat(GHB_Cell.Conductance);
  WriteIface(GHB_Cell.IFace);
  WriteString(' # ' + DataSetIdentifier + ' Layer Row Column Bhead '
    + VariableIdentifiers);

  NewLine;
end;

procedure TModflowGHB_Writer.WriteDataSet1;
begin
  CountParametersAndParameterCells(NPGHB, MXL);
  if NPGHB > 0 then
  begin
    WriteString('PARAMETER');
    WriteInteger(NPGHB);
    WriteInteger(MXL);
    WriteString(' # DataSet 1: PARAMETER NPGHB MXL');
    NewLine;
  end;
end;

procedure TModflowGHB_Writer.WriteDataSet2;
var
  MXACTC: integer;
  Option: String;
  IGHBCB: Integer;
begin
  CountCells(MXACTC);
  GetFlowUnitNumber(IGHBCB);
  GetOption(Option);

  WriteInteger(MXACTC);
  WriteInteger(IGHBCB);
  WriteString(Option);
  WriteString(' # DataSet 2: MXACTC IGHBCB');
  if Option <> '' then
  begin
    WriteString(' Option');
  end;
  NewLine
end;

procedure TModflowGHB_Writer.WriteDataSets3And4;
const
  ErrorRoot = 'One or more %s parameters have been eliminated '
    + 'because there are no cells associated with them.';
  DS3 = ' # Data Set 3: PARNAM PARTYP Parval NLST';
  DS3Instances = ' INSTANCES NUMINST';
  DS4A = ' # Data Set 4a: INSTNAM';
  DataSetIdentifier = 'Data Set 4b:';
  VariableIdentifiers = 'Condfact IFACE';
begin
  WriteParameterDefinitions(DS3, DS3Instances, DS4A, DataSetIdentifier,
    VariableIdentifiers, ErrorRoot);
end;

procedure TModflowGHB_Writer.WriteDataSets5To7;
const
  D7PName =      ' # Data Set 7: PARNAM';
  D7PNameIname = ' # Data Set 7: PARNAM Iname';
  DS5 = ' # Data Set 5: ITMP NP';
  DataSetIdentifier = 'Data Set 6:';
  VariableIdentifiers = 'Cond IFACE';
begin
  WriteStressPeriods(VariableIdentifiers, DataSetIdentifier, DS5,
    D7PNameIname, D7PName);
end;

procedure TModflowGHB_Writer.WriteFile(const AFileName: string);
var
  NameOfFile: string;
  ShouldWriteFile: Boolean;
  ShouldWriteObservationFile: Boolean;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  ShouldWriteFile := not PhastModel.PackageGeneratedExternally(StrGHB);
  ShouldWriteObservationFile := ObservationPackage.IsSelected
    and not PhastModel.PackageGeneratedExternally(StrGBOB);

  if not ShouldWriteFile and not ShouldWriteObservationFile then
  begin
    Exit;
  end;
  NameOfFile := FileName(AFileName);
  if ShouldWriteFile then
  begin
    WriteToNameFile(StrGHB, PhastModel.UnitNumbers.UnitNumber(StrGHB),
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
    frmProgress.AddMessage('Writing GHB Package input.');
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

procedure TModflowGHB_Writer.WriteFluxObservationFile(const AFileName: string;
  Purpose: TObservationPurpose);
const
  DataSet1Comment = ' # Data Set 1: NQGB NQCGB NQTGB IUGBOBSV';
  DataSet2Comment = ' # Data Set 2: TOMULTGB';
  DataSet3Comment = ' # Data Set 3: NQOBGB NQCLGB';
  PackageAbbreviation = StrGBOB;
begin
  WriteFluxObsFile(AFileName, StrIUGBOBSV, PackageAbbreviation,
    DataSet1Comment, DataSet2Comment, DataSet3Comment,
    PhastModel.GhbObservations, Purpose);
end;

procedure TModflowGHB_Writer.WriteParameterCells(CellList: TValueCellList;
  NLST: Integer; const VariableIdentifiers, DataSetIdentifier: string);
var
  Cell: TGhb_Cell;
  CellIndex: Integer;
begin
  // Data set 4b
  for CellIndex := 0 to CellList.Count - 1 do
  begin
    Cell := CellList[CellIndex] as TGhb_Cell;
    WriteCell(Cell, DataSetIdentifier, VariableIdentifiers);
  end;
  // Dummy inactive cells to fill out data set 4b.
  // Each instance of a parameter is required to have the same
  // number of cells.  This introduces dummy boundaries to fill
  // out the list.  because Condfact is set equal to zero, the
  // dummy boundaries have no effect.
  for CellIndex := CellList.Count to NLST - 1 do
  begin
    WriteInteger(1);
    WriteInteger(1);
    WriteInteger(1);
    WriteFloat(0);
    WriteFloat(0);
    WriteInteger(0);
    WriteString(
      ' # Data Set 4b: Layer Row Column Bhead Condfact IFACE (Dummy boundary)');
    NewLine;
  end;
end;

end.
