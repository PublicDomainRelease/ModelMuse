unit ModflowDRN_WriterUnit;

interface

uses SysUtils, Classes, Contnrs, CustomModflowWriterUnit, ModflowDrnUnit,
  PhastModelUnit, ScreenObjectUnit, ModflowBoundaryUnit, ModflowCellUnit,
  ModflowPackageSelectionUnit, OrderedCollectionUnit, FluxObservationUnit,
  GoPhastTypes;

type
  TModflowDRN_Writer = class(TFluxObsWriter)
  private
    NPDRN: integer;
    MXL: integer;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSets3And4;
    procedure WriteDataSets5To7;
  protected
    function ObservationPackage: TModflowPackageSelection; override;
    function CellType: TValueCellType; override;
    class function Extension: string; override;
    class function ObservationExtension: string; override;
    class function ObservationOutputExtension: string; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    function Package: TModflowPackageSelection; override;
    function ParameterType: TParameterType; override;
    procedure WriteCell(Cell: TValueCell;
      const DataSetIdentifier, VariableIdentifiers: string); override;
    procedure WriteParameterCells(CellList: TValueCellList; NLST: Integer;
      const VariableIdentifiers, DataSetIdentifier: string;
      AssignmentMethod: TUpdateMethod; MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); override;
    function ObsNameWarningString: string; override;
    procedure Evaluate; override;
    procedure CheckCell(ValueCell: TValueCell; const PackageName: string); override;
  public
    procedure WriteFile(const AFileName: string);
    procedure WriteFluxObservationFile(const AFileName: string;
      Purpose: TObservationPurpose);
  end;

implementation

uses ModflowTimeUnit, frmErrorsAndWarningsUnit,
  ModflowTransientListParameterUnit, ModflowUnitNumbers, frmProgressUnit,
  RbwParser, DataSetUnit, Forms;

resourcestring
  StrTheFollowingDrain = 'The following Drain observation names may be valid' +
  ' for MODFLOW but they are not valid for UCODE.';
  StrWritingDRNPackage = 'Writing DRN Package input.';
//  StrWritingDataSet0 = '  Writing Data Set 0.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';
//  StrWritingDataSets3and4 = '  Writing Data Sets 3 and 4.';
//  StrWritingDataSets5to7 = '  Writing Data Sets 5 to 7.';
  StrDrainElevationIsB = 'Drain elevation is below the bottom of the cell at' +
  ' the following locations.';
//  StrLayerRowColumn = 'Layer, Row Column = %0:d, %1:d, %2:d';

{ TModflowDRN_Writer }

function TModflowDRN_Writer.CellType: TValueCellType;
begin
  result := TDrn_Cell;
end;

procedure TModflowDRN_Writer.CheckCell(ValueCell: TValueCell;
  const PackageName: string);
var
  Drn_Cell: TDrn_Cell;
  ActiveDataArray: TDataArray;
  ScreenObject: TScreenObject;
begin
  inherited;
  Drn_Cell := ValueCell as TDrn_Cell;
  ActiveDataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
  Assert(ActiveDataArray <> nil);
  if ActiveDataArray.BooleanData[Drn_Cell.Layer, Drn_Cell.Row, Drn_Cell.Column]
    and (Drn_Cell.Elevation < Model.Grid.CellElevation[
    Drn_Cell.Column, Drn_Cell.Row, Drn_Cell.Layer+1]) then
  begin
    ScreenObject := Drn_Cell.ScreenObject as TScreenObject;
    if Model.ModelSelection = msModflowNWT then
    begin
      frmErrorsAndWarnings.AddError(Model, StrDrainElevationIsB,
        Format(StrLayerRowColObject, [
        Drn_Cell.Layer+1, Drn_Cell.Row+1, Drn_Cell.Column+1, ScreenObject.Name]));
    end
    else
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrDrainElevationIsB,
        Format(StrLayerRowColObject, [
        Drn_Cell.Layer+1, Drn_Cell.Row+1, Drn_Cell.Column+1, ScreenObject.Name]));
    end;
  end;
end;

procedure TModflowDRN_Writer.Evaluate;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrDrainElevationIsB);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrDrainElevationIsB);
    inherited;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

class function TModflowDRN_Writer.Extension: string;
begin
  result := '.drn';
end;

function TModflowDRN_Writer.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowDrnBoundary;
end;

class function TModflowDRN_Writer.ObservationExtension: string;
begin
  result := '.ob_drob';
end;

class function TModflowDRN_Writer.ObservationOutputExtension: string;
begin
  result := '.drob_out';
end;

function TModflowDRN_Writer.ObservationPackage: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.DrobPackage;
end;

function TModflowDRN_Writer.ObsNameWarningString: string;
begin
  result := StrTheFollowingDrain;
end;

function TModflowDRN_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.DrnPackage;
end;

procedure TModflowDRN_Writer.WriteCell(Cell: TValueCell;
  const DataSetIdentifier, VariableIdentifiers: string);
var
  Drn_Cell: TDrn_Cell;
  LocalLayer: integer;
begin
  Drn_Cell := Cell as TDrn_Cell;
  LocalLayer := Model.
    DataSetLayerToModflowLayer(Drn_Cell.Layer);
  WriteInteger(LocalLayer);
  WriteInteger(Drn_Cell.Row+1);
  WriteInteger(Drn_Cell.Column+1);
  WriteFloat(Drn_Cell.Elevation);
  WriteFloat(Drn_Cell.Conductance);
  WriteIface(Drn_Cell.IFace);
  WriteString(' # ' + DataSetIdentifier + ' Layer Row Column Elevation '
    + VariableIdentifiers);

  NewLine;

end;

procedure TModflowDRN_Writer.WriteDataSet1;
begin
  CountParametersAndParameterCells(NPDRN, MXL);
  if NPDRN > 0 then
  begin
    WriteString('PARAMETER');
    WriteInteger(NPDRN);
    WriteInteger(MXL);
    WriteString(' # DataSet 1: PARAMETER NPDRN MXL');
    NewLine;
  end;
end;

procedure TModflowDRN_Writer.WriteDataSet2;
var
  MXACTD: integer;
  Option: String;
  IDRNCB: Integer;
begin
  CountCells(MXACTD);
  GetFlowUnitNumber(IDRNCB);
  GetOption(Option);

  WriteInteger(MXACTD);
  WriteInteger(IDRNCB);
  WriteString(Option);
  WriteString(' # DataSet 2: MXACTD IDRNCB');
  if Option <> '' then
  begin
    WriteString(' Option');
  end;
  NewLine
end;

function TModflowDRN_Writer.ParameterType: TParameterType;
begin
  result := ptDRN;
end;

procedure TModflowDRN_Writer.WriteDataSets3And4;
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

procedure TModflowDRN_Writer.WriteDataSets5To7;
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

procedure TModflowDRN_Writer.WriteFile(const AFileName: string);
var
  NameOfFile: string;
  ShouldWriteFile: Boolean;
  ShouldWriteObservationFile: Boolean;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  ShouldWriteFile := not Model.PackageGeneratedExternally(StrDRN);
  ShouldWriteObservationFile := ObservationPackage.IsSelected
    and not Model.PackageGeneratedExternally(StrDROB);

  if not ShouldWriteFile and not ShouldWriteObservationFile then
  begin
    Exit;
  end;

  NameOfFile := FileName(AFileName);
  if ShouldWriteFile then
  begin
    WriteToNameFile('DRN', Model.UnitNumbers.UnitNumber(StrDRN),
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
    frmProgressMM.AddMessage(StrWritingDRNPackage);
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

procedure TModflowDRN_Writer.WriteFluxObservationFile(const AFileName: string;
  Purpose: TObservationPurpose);
const
  DataSet1Comment = ' # Data Set 1: NQDR NQCDR NQTDR IUDROBSV';
  DataSet2Comment = ' # Data Set 2: TOMULTDR';
  DataSet3Comment = ' # Data Set 3: NQOBDR NQCLDR';
  PackageAbbreviation = StrDROB;
begin
  WriteFluxObsFile(AFileName, StrIUDROBSV, PackageAbbreviation,
    DataSet1Comment, DataSet2Comment, DataSet3Comment,
    Model.DrainObservations, Purpose);
end;

procedure TModflowDRN_Writer.WriteParameterCells(CellList: TValueCellList;
  NLST: Integer; const VariableIdentifiers, DataSetIdentifier: string;
  AssignmentMethod: TUpdateMethod; MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection);
var
  Cell: TDrn_Cell;
  CellIndex: Integer;
begin
  // Data set 4b
  for CellIndex := 0 to CellList.Count - 1 do
  begin
    Cell := CellList[CellIndex] as TDrn_Cell;
    WriteCell(Cell, DataSetIdentifier, VariableIdentifiers);
    CheckCell(Cell, 'DRN');
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
      ' # Data Set 4b: Layer Row Column Stage Condfact IFACE (Dummy boundary)');
    NewLine;
  end;
end;

end.
