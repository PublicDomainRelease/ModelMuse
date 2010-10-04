unit ModflowWellWriterUnit;

interface

uses SysUtils, Classes, CustomModflowWriterUnit, ModflowWellUnit,
  ScreenObjectUnit, ModflowBoundaryUnit, ModflowPackageSelectionUnit,
  ModflowCellUnit, OrderedCollectionUnit, ModflowBoundaryDisplayUnit,
  ModflowTransientListParameterUnit;

type
  TModflowWEL_Writer = class(TCustomListWriter)
  private
    NPWEL: integer;
    MXL: integer;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSets3And4;
    procedure WriteDataSets5To7;
//    procedure ListEvaluate;
  protected
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
  public
    procedure WriteFile(const AFileName: string);
  end;


implementation

uses ModflowUnitNumbers, frmErrorsAndWarningsUnit, frmProgressUnit, Forms;

{ TModflowWEL_Writer }

function TModflowWEL_Writer.CellType: TValueCellType;
begin
  result := TWell_Cell;
end;

class function TModflowWEL_Writer.Extension: string;
begin
  result := '.wel';
end;

function TModflowWEL_Writer.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowWellBoundary;
end;

{procedure TModflowWEL_Writer.ListEvaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ParamIndex: Integer;
  List: TList;
  // ultimately convert Boundary to a TModflowBoundary
  Boundary: TMfWellBoundary;
  NoAssignmentErrorRoot: string;
begin
  NoAssignmentErrorRoot := 'No boundary conditions assigned to the '
    + Package.PackageIdentifier
    + ' because the object does not '
    + 'set the values of either enclosed or intersected cells.';
  frmProgress.AddMessage('Evaluating '
    + Package.PackageIdentifier + ' data.');
  for ScreenObjectIndex := 0 to PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := PhastModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := GetBoundary(ScreenObject) as TMfWellBoundary;
    if Boundary <> nil then
    begin
      if not ScreenObject.SetValuesOfEnclosedCells
        and not ScreenObject.SetValuesOfIntersectedCells then
      begin
        frmErrorsAndWarnings.AddError(NoAssignmentErrorRoot, ScreenObject.Name);
      end;
      frmProgress.AddMessage('  Evaluating '
        + ScreenObject.Name);
      Boundary.GetCellListValues(Values, ParamValues);
    end;
  end;
  for ParamIndex := 0 to FParamValues.Count - 1 do
  begin
    List := FParamValues.Objects[ParamIndex] as TList;
    While List.Count > Values.Count do
    begin
      Values.Add(TValueCellList.Create(CellType))
    end;
  end;
  for ParamIndex := 0 to FParamValues.Count - 1 do
  begin
    List := FParamValues.Objects[ParamIndex] as TList;
    While List.Count < Values.Count do
    begin
      List.Add(TValueCellList.Create(CellType))
    end;
  end;
end;   }

function TModflowWEL_Writer.Package: TModflowPackageSelection;
begin
  result := PhastModel.ModflowPackages.WelPackage;
end;

function TModflowWEL_Writer.ParameterType: TParameterType;
begin
  result := ptQ;
end;

procedure TModflowWEL_Writer.WriteCell(Cell: TValueCell; const DataSetIdentifier,
  VariableIdentifiers: string);
var
  Well_Cell: TWell_Cell;
  LocalLayer: integer;
begin
  Well_Cell := Cell as TWell_Cell;
  LocalLayer := PhastModel.LayerStructure.
    DataSetLayerToModflowLayer(Well_Cell.Layer);
  WriteInteger(LocalLayer);
  WriteInteger(Well_Cell.Row+1);
  WriteInteger(Well_Cell.Column+1);
  WriteFloat(Well_Cell.PumpingRate);
  WriteIface(Well_Cell.IFace);
  WriteString(' # ' + DataSetIdentifier + ' Layer Row Column Bhead '
    + VariableIdentifiers + ' ');
  // The annotation identifies the object used to define the well.
  // This can be helpful in identifying when used with PEST. 
  WriteString(Well_Cell.PumpingRateAnnotation);
  NewLine;
end;

procedure TModflowWEL_Writer.WriteDataSet1;
begin
  CountParametersAndParameterCells(NPWEL, MXL);
  if NPWEL > 0 then
  begin
    WriteString('PARAMETER');
    WriteInteger(NPWEL);
    WriteInteger(MXL);
    WriteString(' # DataSet 1: PARAMETER NPWEL MXL');
    NewLine;
  end;
end;

procedure TModflowWEL_Writer.WriteDataSet2;
var
  MXACTC: integer;
  Option: String;
  IWELCB: Integer;
begin
  CountCells(MXACTC);
  GetFlowUnitNumber(IWELCB);
  GetOption(Option);

  WriteInteger(MXACTC);
  WriteInteger(IWELCB);
  WriteString(Option);
  WriteString(' # DataSet 2: MXACTC IGHBCB');
  if Option <> '' then
  begin
    WriteString(' Option');
  end;
  NewLine
end;

procedure TModflowWEL_Writer.WriteDataSets3And4;
const
  ErrorRoot = 'One or more %s parameters have been eliminated '
    + 'because there are no cells associated with them.';
  DS3 = ' # Data Set 3: PARNAM PARTYP Parval NLST';
  DS3Instances = ' INSTANCES NUMINST';
  DS4A = ' # Data Set 4a: INSTNAM';
  DataSetIdentifier = 'Data Set 4b:';
  VariableIdentifiers = 'Qfact IFACE';
begin
  WriteParameterDefinitions(DS3, DS3Instances, DS4A, DataSetIdentifier,
    VariableIdentifiers, ErrorRoot);
end;

procedure TModflowWEL_Writer.WriteDataSets5To7;
const
  D7PName =      ' # Data Set 7: PARNAM';
  D7PNameIname = ' # Data Set 7: PARNAM Iname';
  DS5 = ' # Data Set 5: ITMP NP';
  DataSetIdentifier = 'Data Set 6:';
  VariableIdentifiers = 'Q IFACE';
begin
  WriteStressPeriods(VariableIdentifiers, DataSetIdentifier, DS5,
    D7PNameIname, D7PName);
end;

procedure TModflowWEL_Writer.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if PhastModel.PackageGeneratedExternally(StrWEL) then
  begin
    Exit;
  end;
  NameOfFile := FileName(AFileName);
  WriteToNameFile(StrWEL, PhastModel.UnitNumbers.UnitNumber(StrWEL), NameOfFile, foInput);
  Evaluate;
  Application.ProcessMessages;
  if not frmProgress.ShouldContinue then
  begin
    Exit;
  end;
  ClearTimeLists;
  OpenFile(FileName(AFileName));
  try
    frmProgress.AddMessage('Writing WEL Package input.');
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

procedure TModflowWEL_Writer.WriteParameterCells(CellList: TValueCellList; NLST: Integer;
  const VariableIdentifiers, DataSetIdentifier: string);
var
  Cell: TWell_Cell;
  CellIndex: Integer;
begin
  // Data set 4b
  for CellIndex := 0 to CellList.Count - 1 do
  begin
    Cell := CellList[CellIndex] as TWell_Cell;
    WriteCell(Cell, DataSetIdentifier, VariableIdentifiers);
    CheckCell(Cell, 'WEL');
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
    WriteInteger(0);
    WriteString(
      ' # Data Set 4b: Layer Row Column Bhead Qfact IFACE (Dummy boundary)');
    NewLine;
  end;
end;

end.
