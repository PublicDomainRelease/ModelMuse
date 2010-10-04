unit ModflowRiverWriterUnit;

interface

uses SysUtils, Classes, Contnrs, CustomModflowWriterUnit, ModflowRivUnit,
  PhastModelUnit, ScreenObjectUnit, ModflowBoundaryUnit, ModflowCellUnit,
  ModflowPackageSelectionUnit, OrderedCollectionUnit, GoPhastTypes;

type
  TModflowRIV_Writer = class(TFluxObsWriter)
  private
    NPRIV: integer;
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
    function ObsNameWarningString: string; override;
  public
    procedure WriteFile(const AFileName: string);
    procedure WriteFluxObservationFile(const AFileName: string;
      Purpose: TObservationPurpose);
  end;

implementation

uses ModflowTimeUnit, frmErrorsAndWarningsUnit,
  ModflowTransientListParameterUnit, ModflowUnitNumbers, frmProgressUnit, Forms;

{ TModflowRIV_Writer }

function TModflowRIV_Writer.CellType: TValueCellType;
begin
  result := TRiv_Cell;
end;

class function TModflowRIV_Writer.Extension: string;
begin
  result := '.riv';
end;

function TModflowRIV_Writer.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowRivBoundary;
end;

class function TModflowRIV_Writer.ObservationExtension: string;
begin
  result := '.ob_rvob';
end;

class function TModflowRIV_Writer.ObservationOutputExtension: string;
begin
  result := '.rvob_out';
end;

function TModflowRIV_Writer.ObservationPackage: TModflowPackageSelection;
begin
  result := PhastModel.ModflowPackages.RvobPackage;
end;

function TModflowRIV_Writer.ObsNameWarningString: string;
begin
  result := 'The following River observation names may be valid for MODFLOW but they are not valid for UCODE.';
end;

function TModflowRIV_Writer.Package: TModflowPackageSelection;
begin
  result := PhastModel.ModflowPackages.RivPackage;
end;

procedure TModflowRIV_Writer.WriteCell(Cell: TValueCell;
  const DataSetIdentifier, VariableIdentifiers: string);
var
  Riv_Cell: TRiv_Cell;
  LocalLayer: integer;
begin
  Riv_Cell := Cell as TRiv_Cell;
  LocalLayer := PhastModel.LayerStructure.
    DataSetLayerToModflowLayer(Riv_Cell.Layer);
  WriteInteger(LocalLayer);
  WriteInteger(Riv_Cell.Row+1);
  WriteInteger(Riv_Cell.Column+1);
  WriteFloat(Riv_Cell.RiverStage);
  WriteFloat(Riv_Cell.Conductance);
  WriteFloat(Riv_Cell.RiverBottom);
  WriteIface(Riv_Cell.IFace);
  WriteString(' # ' + DataSetIdentifier + ' Layer Row Column Stage '
    + VariableIdentifiers);
//  WriteString(' ' + Riv_Cell.ConductanceAnnotation);
  NewLine;
end;

procedure TModflowRIV_Writer.WriteDataSet1;
begin
  CountParametersAndParameterCells(NPRIV, MXL);
  if NPRIV > 0 then
  begin
    WriteString('PARAMETER');
    WriteInteger(NPRIV);
    WriteInteger(MXL);
    WriteString(' # DataSet 1: PARAMETER NPRIV MXL');
    NewLine;
  end;
end;

procedure TModflowRIV_Writer.WriteDataSet2;
var
  MXACTC: integer;
  Option: String;
  IRIVCB: Integer;
begin
  CountCells(MXACTC);
  GetFlowUnitNumber(IRIVCB);
  GetOption(Option);

  WriteInteger(MXACTC);
  WriteInteger(IRIVCB);
  WriteString(Option);
  WriteString(' # DataSet 2: MXACTC IRIVCB');
  if Option <> '' then
  begin
    WriteString(' Option');
  end;
  NewLine
end;

function TModflowRIV_Writer.ParameterType: TParameterType;
begin
  result := ptRIV;
end;

procedure TModflowRIV_Writer.WriteDataSets3And4;
const
  ErrorRoot = 'One or more %s parameters have been eliminated '
    + 'because there are no cells associated with them.';
  DS3 = ' # Data Set 3: PARNAM PARTYP Parval NLST';
  DS3Instances = ' INSTANCES NUMINST';
  DS4A = ' # Data Set 4a: INSTNAM';
  DataSetIdentifier = 'Data Set 4b:';
  VariableIdentifiers = 'Condfact Rbot IFACE';
begin
  WriteParameterDefinitions(DS3, DS3Instances, DS4A, DataSetIdentifier,
    VariableIdentifiers, ErrorRoot);
end;

procedure TModflowRIV_Writer.WriteDataSets5To7;
const
  D7PName =      ' # Data Set 7: PARNAM';
  D7PNameIname = ' # Data Set 7: PARNAM Iname';
  DS5 = ' # Data Set 5: ITMP NP';
  DataSetIdentifier = 'Data Set 6:';
  VariableIdentifiers = 'Cond Rbot IFACE';
begin
  WriteStressPeriods(VariableIdentifiers, DataSetIdentifier, DS5,
    D7PNameIname, D7PName);
end;

procedure TModflowRIV_Writer.WriteFile(const AFileName: string);
var
  NameOfFile: string;
  ShouldWriteFile: Boolean;
  ShouldWriteObservationFile: Boolean;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  ShouldWriteFile := not PhastModel.PackageGeneratedExternally(StrRIV);
  ShouldWriteObservationFile := ObservationPackage.IsSelected
    and not PhastModel.PackageGeneratedExternally(StrRVOB);

  if not ShouldWriteFile and not ShouldWriteObservationFile then
  begin
    Exit;
  end;
  NameOfFile := FileName(AFileName);
  if ShouldWriteFile then
  begin
    WriteToNameFile(StrRIV, PhastModel.UnitNumbers.UnitNumber(StrRIV),
      NameOfFile, foInput);
  end;
  if ShouldWriteFile or ShouldWriteObservationFile then
  begin
    Evaluate;
    Application.ProcessMessages;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;
    ClearTimeLists;
  end;
  if not ShouldWriteFile then
  begin
    Exit;
  end;
  OpenFile(NameOfFile);
  try
    frmProgress.AddMessage('Writing RIV Package input.');
    frmProgress.AddMessage('  Writing Data Set 0.');
    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('  Writing Data Set 1.');
    WriteDataSet1;
    Application.ProcessMessages;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('  Writing Data Set 2.');
    WriteDataSet2;
    Application.ProcessMessages;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('  Writing Data Sets 3 and 4.');
    WriteDataSets3And4;
    Application.ProcessMessages;
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

procedure TModflowRIV_Writer.WriteFluxObservationFile(const AFileName: string;
  Purpose: TObservationPurpose);
const
  DataSet1Comment = ' # Data Set 1: NQRV NQCRV NQTRV IURVOBSV';
  DataSet2Comment = ' # Data Set 2: TOMULTRV';
  DataSet3Comment = ' # Data Set 3: NQOBRV NQCLRV';
  PackageAbbreviation = StrRVOB;
begin
  WriteFluxObsFile(AFileName, StrIURVOBSV, PackageAbbreviation,
    DataSet1Comment, DataSet2Comment, DataSet3Comment,
    PhastModel.RiverObservations, Purpose);
end;

procedure TModflowRIV_Writer.WriteParameterCells(CellList: TValueCellList; NLST: Integer;
  const VariableIdentifiers, DataSetIdentifier: string);
var
  Cell: TRiv_Cell;
  CellIndex: Integer;
begin
  // Data set 4b
  for CellIndex := 0 to CellList.Count - 1 do
  begin
    Cell := CellList[CellIndex] as TRiv_Cell;
    WriteCell(Cell, DataSetIdentifier, VariableIdentifiers);
    CheckCell(Cell, 'RIV');
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
    WriteFloat(0);
    WriteInteger(0);
    WriteString(
      ' # Data Set 4b: Layer Row Column Stage Condfact Rbot IFACE (Dummy boundary)');
    NewLine;
  end;
end;

end.
