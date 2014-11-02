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

uses ModflowTimeUnit, frmErrorsAndWarningsUnit,
  ModflowTransientListParameterUnit, ModflowUnitNumbers, frmProgressUnit, Forms,
  DataSetUnit;

resourcestring
  StrInTheFollowingRiv = 'In the following river cells, the stage is below t' +
  'he river bottom.';
  StrLayerDRowDC = 'Layer: %0:d, Row %1:d, Column %2:d';
  StrTheFollowingRiver = 'The following River observation names may be valid' +
  ' for MODFLOW but they are not valid for UCODE.';
  StrWritingRIVPackage = 'Writing RIV Package input.';
//  StrWritingDataSet0 = '  Writing Data Set 0.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';
//  StrWritingDataSets3and4 = '  Writing Data Sets 3 and 4.';
//  StrWritingDataSets5to7 = '  Writing Data Sets 5 to 7.';
  StrRiverStageIsBelow = 'River stage is below the bottom of the cell at the' +
  ' following locations.';

{ TModflowRIV_Writer }

function TModflowRIV_Writer.CellType: TValueCellType;
begin
  result := TRiv_Cell;
end;

procedure TModflowRIV_Writer.CheckCell(ValueCell: TValueCell;
  const PackageName: string);
var
  Riv_Cell: TRiv_Cell;
  ActiveDataArray: TDataArray;
  ScreenObject: TScreenObject;
begin
  inherited;
  Riv_Cell := ValueCell as TRiv_Cell;
  ActiveDataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
  Assert(ActiveDataArray <> nil);
  if ActiveDataArray.BooleanData[Riv_Cell.Layer, Riv_Cell.Row, Riv_Cell.Column]
    and (Riv_Cell.RiverStage < Model.Grid.CellElevation[
    Riv_Cell.Column, Riv_Cell.Row, Riv_Cell.Layer+1]) then
  begin
    ScreenObject := Riv_Cell.ScreenObject as TScreenObject;
    if Model.ModelSelection = msModflowNWT then
    begin
      frmErrorsAndWarnings.AddError(Model, StrRiverStageIsBelow,
        Format(StrLayerRowColObject, [
        Riv_Cell.Layer+1, Riv_Cell.Row+1, Riv_Cell.Column+1, ScreenObject.Name]));
    end
    else
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrRiverStageIsBelow,
        Format(StrLayerRowColObject, [
        Riv_Cell.Layer+1, Riv_Cell.Row+1, Riv_Cell.Column+1, ScreenObject.Name]));
    end;
  end;
end;

procedure TModflowRIV_Writer.Evaluate;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrRiverStageIsBelow);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrRiverStageIsBelow);
    inherited;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
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
  result := Model.ModflowPackages.RvobPackage;
end;

function TModflowRIV_Writer.ObsNameWarningString: string;
begin
  result := StrTheFollowingRiver;
end;

function TModflowRIV_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.RivPackage;
end;

procedure TModflowRIV_Writer.WriteCell(Cell: TValueCell;
  const DataSetIdentifier, VariableIdentifiers: string);
var
  Riv_Cell: TRiv_Cell;
  LocalLayer: integer;
begin
  Riv_Cell := Cell as TRiv_Cell;
  LocalLayer := Model.
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
  if Riv_Cell.RiverStage <= Riv_Cell.RiverBottom then
  begin
    if Model.ModelSelection = msModflowNWT then
    begin
      frmErrorsAndWarnings.AddError(Model, StrInTheFollowingRiv,
        Format(StrLayerDRowDC, [Riv_Cell.Layer+1, Riv_Cell.Row+1, Riv_Cell.Column+1]));
    end
    else
    begin
      frmErrorsAndWarnings.AddWarning(Model, StrInTheFollowingRiv,
        Format(StrLayerDRowDC, [Riv_Cell.Layer+1, Riv_Cell.Row+1, Riv_Cell.Column+1]));
    end;
  end;
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
    VariableIdentifiers, ErrorRoot, umAssign, nil, nil);
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
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInTheFollowingRiv);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrInTheFollowingRiv);
  if not Package.IsSelected then
  begin
    Exit
  end;
  ShouldWriteFile := not Model.PackageGeneratedExternally(StrRIV);
  ShouldWriteObservationFile := ObservationPackage.IsSelected
    and not Model.PackageGeneratedExternally(StrRVOB);

  if not ShouldWriteFile and not ShouldWriteObservationFile then
  begin
    Exit;
  end;
  NameOfFile := FileName(AFileName);
  if ShouldWriteFile then
  begin
    WriteToNameFile(StrRIV, Model.UnitNumbers.UnitNumber(StrRIV),
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
  OpenFile(NameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingRIVPackage);
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
    Model.RiverObservations, Purpose);
end;

procedure TModflowRIV_Writer.WriteParameterCells(CellList: TValueCellList;
  NLST: Integer; const VariableIdentifiers, DataSetIdentifier: string;
  AssignmentMethod: TUpdateMethod; MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection);
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
