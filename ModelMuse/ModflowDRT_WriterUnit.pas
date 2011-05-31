unit ModflowDRT_WriterUnit;

interface

uses SysUtils, Classes, Contnrs, CustomModflowWriterUnit, ModflowDrtUnit,
  PhastModelUnit, ScreenObjectUnit, ModflowBoundaryUnit, ModflowCellUnit,
  ModflowPackageSelectionUnit, OrderedCollectionUnit, GoPhastTypes;

type
  TModflowDRT_Writer = class(TCustomListWriter)
  private
    NPDRT: integer;
    MXL: integer;
    procedure WriteDataSet1;
    procedure WriteDataSets2And3;
    procedure WriteDataSets4To6;
  protected
    function CellType: TValueCellType; override;
    class function Extension: string; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    procedure GetOption(var Option: string); override;
    function Package: TModflowPackageSelection; override;
    function ParameterType: TParameterType; override;
    procedure WriteParameterCells(CellList: TValueCellList; NLST: Integer;
      const VariableIdentifiers, DataSetIdentifier: string;
      AssignmentMethod: TUpdateMethod); override;
    procedure WriteCell(Cell: TValueCell;
      const DataSetIdentifier, VariableIdentifiers: string); override;
  public
    procedure WriteFile(const AFileName: string);
  end;


implementation

uses ModflowTimeUnit, frmErrorsAndWarningsUnit,
  ModflowTransientListParameterUnit, ModflowUnitNumbers, frmProgressUnit, Forms;

{ TModflowDRT_Writer }

function TModflowDRT_Writer.CellType: TValueCellType;
begin
  result := TDrt_Cell;
end;

class function TModflowDRT_Writer.Extension: string;
begin
  result := '.drt';
end;

function TModflowDRT_Writer.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowDrtBoundary;
end;

procedure TModflowDRT_Writer.GetOption(var Option: string);
begin
  inherited;
  Option := Option + ' RETURNFLOW';
end;

function TModflowDRT_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.DrtPackage;
end;

function TModflowDRT_Writer.ParameterType: TParameterType;
begin
  result := ptDRT;
end;

procedure TModflowDRT_Writer.WriteCell(Cell: TValueCell; const DataSetIdentifier,
  VariableIdentifiers: string);
var
  Drt_Cell: TDrt_Cell;
  LocalLayer: integer;
begin
  Drt_Cell := Cell as TDrt_Cell;
  LocalLayer := Model.
    DataSetLayerToModflowLayer(Drt_Cell.Layer);
  WriteInteger(LocalLayer);
  WriteInteger(Drt_Cell.Row+1);
  WriteInteger(Drt_Cell.Column+1);
  WriteFloat(Drt_Cell.Elevation);
  WriteFloat(Drt_Cell.Conductance);
  WriteInteger(Drt_Cell.ReturnCell.Layer);
  if Drt_Cell.ReturnCell.Layer > 0 then
  begin
    WriteInteger(Drt_Cell.ReturnCell.Row);
    WriteInteger(Drt_Cell.ReturnCell.Column);
    WriteFloat(Drt_Cell.ReturnFraction);
  end
  else
  begin
    WriteInteger(0);
    WriteInteger(0);
    WriteFloat(0.0);
  end;
  WriteIface(Drt_Cell.IFace);
  WriteString(' # ' + DataSetIdentifier + ' Layer Row Column Elevation '
    + VariableIdentifiers + ' LayR');
  WriteString(' RowR ColR Rfprop IFACE');
  NewLine;
end;

procedure TModflowDRT_Writer.WriteDataSet1;
var
  MXADRT: integer;
  Option: String;
  IDRTCB: Integer;
begin
  CountParametersAndParameterCells(NPDRT, MXL);

  CountCells(MXADRT);
  GetFlowUnitNumber(IDRTCB);
  GetOption(Option);

  WriteInteger(MXADRT);
  WriteInteger(IDRTCB);
  WriteInteger(NPDRT);
  WriteInteger(MXL);
  WriteString(Option);
  WriteString(' # DataSet 1: MXADRT IDRTCB NPDRT MXL');
  if Option <> '' then
  begin
    WriteString(' Option');
  end;
  NewLine
end;

procedure TModflowDRT_Writer.WriteDataSets2And3;
const
  ErrorRoot = 'One or more %s parameters have been eliminated '
    + 'because there are no cells associated with them.';
  DS2 = ' # Data Set 2: PARNAM PARTYP Parval NLST';
  DS2Instances = ' INSTANCES NUMINST';
  DS3A = ' # Data Set 3a: INSTNAM';
  DataSetIdentifier = 'Data Set 3b:';
  VariableIdentifiers = 'Condfact IFACE';
begin
  WriteParameterDefinitions(DS2, DS2Instances, DS3A, DataSetIdentifier,
    VariableIdentifiers, ErrorRoot, umAssign);
end;

procedure TModflowDRT_Writer.WriteDataSets4To6;
const
  D6PName =      ' # Data Set 6: PARNAM';
  D6PNameIname = ' # Data Set 6: PARNAM Iname';
  DS4 = ' # Data Set 4: ITMP NP';
  DataSetIdentifier = 'Data Set 4:';
  VariableIdentifiers = 'Cond ';
begin
  WriteStressPeriods(VariableIdentifiers, DataSetIdentifier, DS4,
    D6PNameIname, D6PName);
end;

procedure TModflowDRT_Writer.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  if not Model.ModflowPackages.DrtPackage.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrDRT) then
  begin
    Exit;
  end;
  NameOfFile := FileName(AFileName);
  WriteToNameFile(StrDRT, Model.UnitNumbers.UnitNumber(StrDRT), NameOfFile, foInput);
  Evaluate;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  ClearTimeLists(Model);
  OpenFile(FileName(AFileName));
  try
    frmProgressMM.AddMessage('Writing DRN Package input.');
    frmProgressMM.AddMessage('  Writing Data Set 0.');
    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Set 1.');
    WriteDataSet1;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Sets 2 and 3.');
    WriteDataSets2And3;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Sets 4 to 6.');
    WriteDataSets4To6;
  finally
    CloseFile;
  end;
end;

procedure TModflowDRT_Writer.WriteParameterCells(CellList: TValueCellList;
  NLST: Integer; const VariableIdentifiers, DataSetIdentifier: string;
  AssignmentMethod: TUpdateMethod);
var
  Cell: TDrt_Cell;
  CellIndex: Integer;
begin
  // Data set 3b
  for CellIndex := 0 to CellList.Count - 1 do
  begin
    Cell := CellList[CellIndex] as TDrt_Cell;
    WriteCell(Cell, DataSetIdentifier, VariableIdentifiers);
    CheckCell(Cell, 'DRT');
  end;
  // Dummy inactive cells to fill out data set 3b.
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
    WriteInteger(0);
    WriteInteger(0);
    WriteInteger(0);
    WriteFloat(0);
    WriteString(
      ' # Data Set 3b: Layer Row Column Stage Condfact LayR  RowR ColR Rfprop IFACE (Dummy boundary)');
    NewLine;
  end;
end;

end.
