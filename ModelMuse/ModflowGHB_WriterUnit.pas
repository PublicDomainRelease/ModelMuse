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
      const VariableIdentifiers, DataSetIdentifier: string;
      AssignmentMethod: TUpdateMethod; MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); override;
    procedure WriteCell(Cell: TValueCell;
      const DataSetIdentifier, VariableIdentifiers: string); override;
    class function ObservationExtension: string; override;
    class function ObservationOutputExtension: string; override;
    function ObsNameWarningString: string; override;
    procedure CheckCell(ValueCell: TValueCell; const PackageName: string); override;
    procedure Evaluate; override;
  public
    procedure WriteFile(const AFileName: string);
    procedure WriteFluxObservationFile(const AFileName: string;
      Purpose: TObservationPurpose);
  end;

implementation

uses ModflowTimeUnit, frmErrorsAndWarningsUnit, ModflowUnitNumbers, 
  frmProgressUnit, Forms, DataSetUnit;

resourcestring
  StrTheFollowingGHBOb = 'The following GHB observation names may be valid f' +
  'or MODFLOW but they are not valid for UCODE.';
  StrWritingGHBPackage = 'Writing GHB Package input.';
//  StrWritingDataSet0 = '  Writing Data Set 0.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';
//  StrWritingDataSets3and4 = '  Writing Data Sets 3 and 4.';
//  StrWritingDataSets5to7 = '  Writing Data Sets 5 to 7.';
  StrGHBBoundaryHeadIs = 'GHB Boundary head is below the bottom of the cell ' +
  'at the following locations.';

{ TModflowGHB_Writer }

function TModflowGHB_Writer.CellType: TValueCellType;
begin
  result := TGhb_Cell;
end;

procedure TModflowGHB_Writer.CheckCell(ValueCell: TValueCell;
  const PackageName: string);
var
  Ghb_Cell: TGhb_Cell;
  ActiveDataArray: TDataArray;
  ScreenObject: TScreenObject;
begin
  inherited;
  Ghb_Cell := ValueCell as TGhb_Cell;
  ActiveDataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
  Assert(ActiveDataArray <> nil);
  if ActiveDataArray.BooleanData[Ghb_Cell.Layer, Ghb_Cell.Row, Ghb_Cell.Column]
    and (Ghb_Cell.BoundaryHead < Model.Grid.CellElevation[
    Ghb_Cell.Column, Ghb_Cell.Row, Ghb_Cell.Layer+1]) then
  begin
    ScreenObject := Ghb_Cell.ScreenObject as TScreenObject;
    if Model.ModelSelection = msModflowNWT then
    begin
      frmErrorsAndWarnings.AddError(Model, StrGHBBoundaryHeadIs,
        Format(StrLayerRowColObject, [
        Ghb_Cell.Layer+1, Ghb_Cell.Row+1, Ghb_Cell.Column+1, ScreenObject.Name]));
    end
    else
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrGHBBoundaryHeadIs,
        Format(StrLayerRowColObject, [
        Ghb_Cell.Layer+1, Ghb_Cell.Row+1, Ghb_Cell.Column+1, ScreenObject.Name]));
    end;
  end;
end;

procedure TModflowGHB_Writer.Evaluate;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrGHBBoundaryHeadIs);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrGHBBoundaryHeadIs);
    inherited;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
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
  result := Model.ModflowPackages.GbobPackage;
end;

function TModflowGHB_Writer.ObsNameWarningString: string;
begin
  result := StrTheFollowingGHBOb;
end;

function TModflowGHB_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.GhbBoundary;
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
  LocalLayer := Model.
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
    VariableIdentifiers, ErrorRoot, umAssign, nil, nil);
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
  ShouldWriteFile := not Model.PackageGeneratedExternally(StrGHB);
  ShouldWriteObservationFile := ObservationPackage.IsSelected
    and not Model.PackageGeneratedExternally(StrGBOB);

  if not ShouldWriteFile and not ShouldWriteObservationFile then
  begin
    Exit;
  end;
  NameOfFile := FileName(AFileName);
  if ShouldWriteFile then
  begin
    WriteToNameFile(StrGHB, Model.UnitNumbers.UnitNumber(StrGHB),
      NameOfFile, foInput);
  end;
  if ShouldWriteFile or ShouldWriteObservationFile then
  begin
    Evaluate;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    ClearTimeLists(Model);
  end;
  if not ShouldWriteFile then
  begin
    Exit;
  end;
  OpenFile(FileName(AFileName));
  try
    frmProgressMM.AddMessage(StrWritingGHBPackage);
    frmProgressMM.AddMessage(StrWritingDataSet0);
    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet1);
    WriteDataSet1;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet2);
    WriteDataSet2;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSets3and4);
    WriteDataSets3And4;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSets5to7);
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
    Model.GhbObservations, Purpose);
end;

procedure TModflowGHB_Writer.WriteParameterCells(CellList: TValueCellList;
  NLST: Integer; const VariableIdentifiers, DataSetIdentifier: string;
  AssignmentMethod: TUpdateMethod; MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection);
var
  Cell: TGhb_Cell;
  CellIndex: Integer;
begin
  // Data set 4b
  for CellIndex := 0 to CellList.Count - 1 do
  begin
    Cell := CellList[CellIndex] as TGhb_Cell;
    WriteCell(Cell, DataSetIdentifier, VariableIdentifiers);
    CheckCell(Cell, 'GHB');
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
