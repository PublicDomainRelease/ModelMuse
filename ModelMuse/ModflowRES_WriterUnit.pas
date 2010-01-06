unit ModflowRES_WriterUnit;

interface

uses SysUtils, Classes, CustomModflowWriterUnit, ModflowCellUnit,
  ModflowPackageSelectionUnit, ScreenObjectUnit, ModflowBoundaryUnit,
  OrderedCollectionUnit;

type
  TModflowRES_Writer = class(TCustomTransientArrayWriter)
  private
    NRES: integer;
    NRESOP: integer;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSet4;
    procedure WriteDataSet5;
    procedure WriteDataSet6;
    procedure WriteDataSet7;
    procedure WriteCells(CellList: TList; const DataSetIdentifier,
      VariableIdentifiers: string);
  protected
    function CellType: TValueCellType; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    function Prefix: string; override;
    procedure Evaluate; override;
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
    function ParameterType: TParameterType; override;
    procedure WriteStressPeriods(const VariableIdentifiers, DataSetIdentifier,
      DS5, D7PNameIname, D7PName: string); override;
  public
    procedure WriteFile(const AFileName: string);

  end;

implementation

uses RbwParser, ModflowUnitNumbers, DataSetUnit, PhastModelUnit, ModflowResUnit,
  ModflowTimeUnit, frmProgressUnit, frmFormulaErrorsUnit;

{ TModflowRES_Writer }

function TModflowRES_Writer.CellType: TValueCellType;
begin
  result := TRes_Cell;
end;

procedure TModflowRES_Writer.Evaluate;
var
  Index: Integer;
  AScreenObject: TScreenObject;
begin
  frmProgress.AddMessage('Evaluating RES Package data.');
  NRES := 0;
  for Index := 0 to PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := PhastModel.ScreenObjects[Index];
    if AScreenObject.Deleted then
    begin
      Continue;
    end;
    if (AScreenObject.ModflowResBoundary <> nil)
      and AScreenObject.ModflowResBoundary.Used then
    begin
      Inc(NRES);
      AScreenObject.ModflowResBoundary.ResId := NRES;
    end;
  end;
  inherited;
end;

class function TModflowRES_Writer.Extension: string;
begin
  result := '.res';
end;

function TModflowRES_Writer.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowResBoundary;
end;

function TModflowRES_Writer.Package: TModflowPackageSelection;
begin
  result := PhastModel.ModflowPackages.ResPackage;
end;

function TModflowRES_Writer.ParameterType: TParameterType;
begin
  result := ptUndefined;
end;

function TModflowRES_Writer.Prefix: string;
begin
  result := '';
end;

procedure TModflowRES_Writer.WriteCells(CellList: TList;
  const DataSetIdentifier, VariableIdentifiers: string);
var
  DefaultValue: double;
  DataType: TRbwDataType;
  DataTypeIndex: integer;
  Comment: string;
begin
  DefaultValue := 0;
  DataType := rdtInteger;
  DataTypeIndex := 0;
  Comment := DataSetIdentifier + ' ' + VariableIdentifiers;
  WriteTransient2DArray(Comment, DataTypeIndex, DataType,
    DefaultValue, CellList);
end;

procedure TModflowRES_Writer.WriteDataSet1;
var
  IRESCB, IRESPT, NPTS: integer;
begin
  GetFlowUnitNumber(IRESCB);
  case PhastModel.ModflowPackages.ResPackage.LayerOption of
    loTop: NRESOP := 1;
    loSpecified: NRESOP := 2;
    loTopActive: NRESOP := 3;
  end;
  if PhastModel.ModflowPackages.ResPackage.PrintStage then
  begin
    IRESPT := 1;
  end
  else
  begin
    IRESPT := 0;
  end;
  NPTS := PhastModel.ModflowPackages.ResPackage.TableStages;

  WriteString(FixedFormattedInteger(NRES, 10));
  WriteString(FixedFormattedInteger(IRESCB, 10));
  WriteString(FixedFormattedInteger(NRESOP, 10));
  WriteString(FixedFormattedInteger(IRESPT, 10));
  WriteString(FixedFormattedInteger(NPTS, 10));
  WriteString(' # Data Set 1: NRES IRESCB NRESOP IRESPT NPTS');
  NewLine;
end;

procedure TModflowRES_Writer.WriteDataSet2;
var
  List: TList;
  DataSetIdentifier: string;
  VariableIdentifiers: string;
begin
  List := Values[0];
  DataSetIdentifier := ' # Data Set 2:';
  VariableIdentifiers := 'IRES';
  WriteCells(List, DataSetIdentifier, VariableIdentifiers)
end;

procedure TModflowRES_Writer.WriteDataSet3;
var
  DataArray: TDataArray;
//  ArrayIndex: integer;
begin
  if NRESOP = 2 then
  begin
    DataArray := PhastModel.GetDataSetByName(rsResLayer);
    Assert(DataArray <> nil);
//    DataArray := PhastModel.DataSets[ArrayIndex];
    WriteArray(DataArray, 0, ' # Data Set 3: IRESL');
  end;
end;

procedure TModflowRES_Writer.WriteDataSet4;
var
  DataArray: TDataArray;
//  ArrayIndex: integer;
begin
  DataArray := PhastModel.GetDataSetByName(rsResBottom);
  Assert(DataArray <> nil);
//  DataArray := PhastModel.DataSets[ArrayIndex];
  WriteArray(DataArray, 0, ' # Data Set 4: BRES');
end;

procedure TModflowRES_Writer.WriteDataSet5;
var
  DataArray: TDataArray;
//  ArrayIndex: integer;
begin
  DataArray := PhastModel.GetDataSetByName(rsResKv);
  Assert(DataArray <> nil);
//  DataArray := PhastModel.DataSets[ArrayIndex];
  WriteArray(DataArray, 0, ' # Data Set 5: HCres');
end;

procedure TModflowRES_Writer.WriteDataSet6;
var
  DataArray: TDataArray;
//  ArrayIndex: integer;
begin
  DataArray := PhastModel.GetDataSetByName(rsResBedThickness);
  Assert(DataArray <> nil);
//  DataArray := PhastModel.DataSets[ArrayIndex];
  WriteArray(DataArray, 0, ' # Data Set 6: Rbthck');
end;

procedure TModflowRES_Writer.WriteDataSet7;
var
  Index: integer;
  Item: TModflowStressPeriod;
  Reservoirs: TList;
  AScreenObject: TScreenObject;
  Reservoir: TResBoundary;
  ReservoirIndex: integer;
  TimeIndex: integer;
  ResItem: TResItem;
  StartHead: double;
  EndHead: double;
  ScreenObjectIndex: Integer;
  ExportedStartHead: double;
  ExportedEndHead: double;
  Compiler: TRbwParser;
  TempFormula: string;
  Expression: TExpression;
  ScreenObject: TScreenObject;
begin
  Compiler := PhastModel.rpThreeDFormulaCompiler;

  Reservoirs := TList.Create;
  try
    for ScreenObjectIndex := 0 to PhastModel.ScreenObjectCount - 1 do
    begin
      if not frmProgress.ShouldContinue then
      begin
        Exit;
      end;
      AScreenObject := PhastModel.ScreenObjects[ScreenObjectIndex];
      if (AScreenObject.ModflowResBoundary <> nil)
        and AScreenObject.ModflowResBoundary.Used then
      begin
        Reservoirs.Add(AScreenObject.ModflowResBoundary);
      end;
    end;
    for Index := 0 to PhastModel.ModflowFullStressPeriods.Count - 1 do
    begin
      if not frmProgress.ShouldContinue then
      begin
        Exit;
      end;
      Item := PhastModel.ModflowFullStressPeriods.Items[Index];
      for ReservoirIndex := 0 to Reservoirs.Count - 1 do
      begin
        if not frmProgress.ShouldContinue then
        begin
          Exit;
        end;
        Reservoir := Reservoirs[ReservoirIndex];
        ResItem := Reservoir.Values[0] as TResItem;

        Expression := nil;
        TempFormula := ResItem.EndHead;
        try
          Compiler.Compile(TempFormula);
          Expression := Compiler.CurrentExpression;
          Expression.Evaluate;
        except on E: ERbwParserError do
          begin
            ScreenObject := Reservoir.ScreenObject as TScreenObject;
            frmFormulaErrors.AddError(ScreenObject.Name,
              '(Ending head for the '#13#10
              + Package.PackageIdentifier + ')',
              TempFormula, E.Message);

            ResItem.EndHead := '0.';
            TempFormula := ResItem.EndHead;
            Compiler.Compile(TempFormula);
            Expression := Compiler.CurrentExpression;
            Expression.Evaluate;
          end;
        end;
        ExportedStartHead := Expression.DoubleResult;
        ExportedEndHead := ExportedStartHead;

        for TimeIndex := 0 to Reservoir.Values.Count - 1 do
        begin
          if not frmProgress.ShouldContinue then
          begin
            Exit;
          end;
          frmProgress.AddMessage('    Writing Stress Period ' + IntToStr(TimeIndex+1));
          ResItem := Reservoir.Values[TimeIndex] as TResItem;

          TempFormula := ResItem.StartHead;
          try
            Compiler.Compile(TempFormula);
            Expression := Compiler.CurrentExpression;
            Expression.Evaluate;
          except on E: ERbwParserError do
            begin
              ScreenObject := Reservoir.ScreenObject as TScreenObject;
              frmFormulaErrors.AddError(ScreenObject.Name,
                '(Starting head for the '#13#10
                + Package.PackageIdentifier + ')',
              TempFormula, E.Message);

              ResItem.StartHead := '0.';
              TempFormula := ResItem.StartHead;
              Compiler.Compile(TempFormula);
              Expression := Compiler.CurrentExpression;
              Expression.Evaluate;
            end;
          end;
          StartHead := Expression.DoubleResult;

          TempFormula := ResItem.EndHead;
          try
            Compiler.Compile(TempFormula);
            Expression := Compiler.CurrentExpression;
            Expression.Evaluate;
          except on E: ERbwParserError do
            begin
              ScreenObject := Reservoir.ScreenObject as TScreenObject;
              frmFormulaErrors.AddError(ScreenObject.Name,
                '(Ending head for the '#13#10
                + Package.PackageIdentifier + ')',
              TempFormula, E.Message);

              ResItem.EndHead := '0.';
              TempFormula := ResItem.EndHead;
              Compiler.Compile(TempFormula);
              Expression := Compiler.CurrentExpression;
              Expression.Evaluate;
            end;
          end;
          EndHead := Expression.DoubleResult;

          if ResItem.StartTime = Item.StartTime then
          begin
            ExportedStartHead := StartHead;
          end
          else if ResItem.EndTime <= Item.StartTime then
          begin
            ExportedStartHead := EndHead;
          end
          else if (ResItem.StartTime < Item.StartTime)
            and (ResItem.EndTime > Item.StartTime) then
          begin
            Assert(ResItem.EndTime <> ResItem.StartTime);
            ExportedStartHead := (Item.StartTime - ResItem.StartTime)/
              (ResItem.EndTime - ResItem.StartTime)*
              (EndHead - StartHead) + StartHead;
          end;

          if ResItem.EndTime <= Item.EndTime then
          begin
            ExportedEndHead := EndHead;
          end
          else if (ResItem.StartTime < Item.EndTime)
            and (ResItem.EndTime > Item.EndTime) then
          begin
            Assert(ResItem.EndTime <> ResItem.StartTime);
            ExportedEndHead := (Item.EndTime - ResItem.StartTime)/
              (ResItem.EndTime - ResItem.StartTime)*
              (EndHead - StartHead) + StartHead;
          end;

          if ResItem.EndTime >= Item.EndTime then
          begin
            break;
          end;
        end;
        WriteString(FixedFormattedReal(ExportedStartHead, 10));
        WriteString(FixedFormattedReal(ExportedEndHead, 10));
        WriteString(' # Data Set 7, Stress period');
        WriteInteger(Index + 1);
        WriteString(': Ststage Endstage');
        NewLine;
      end;
    end;
  finally
    Reservoirs.Free;
  end;
end;

procedure TModflowRES_Writer.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if PhastModel.PackageGeneratedExternally(StrRES) then
  begin
    Exit;
  end;
  NameOfFile := FileName(AFileName);
  WriteToNameFile(StrRES, PhastModel.UnitNumbers.UnitNumber(StrRES), NameOfFile, foInput);
  Evaluate;
  OpenFile(FileName(AFileName));
  try
//    WriteDataSet0;
    frmProgress.AddMessage('Writing RES Package input.');
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

    frmProgress.AddMessage('  Writing Data Set 5.');
    WriteDataSet5;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('  Writing Data Set 6.');
    WriteDataSet6;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('  Writing Data Set 7.');
    WriteDataSet7;
  finally
    CloseFile;
  end;

end;

procedure TModflowRES_Writer.WriteStressPeriods(const VariableIdentifiers,
  DataSetIdentifier, DS5, D7PNameIname, D7PName: string);
begin

end;

end.
