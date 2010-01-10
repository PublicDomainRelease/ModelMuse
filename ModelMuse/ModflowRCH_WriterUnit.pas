unit ModflowRCH_WriterUnit;

interface

uses SysUtils, Classes, Contnrs, CustomModflowWriterUnit, ScreenObjectUnit,
  ModflowBoundaryUnit, ModflowPackageSelectionUnit, OrderedCollectionUnit,
  ModflowCellUnit, PhastModelUnit, ModflowBoundaryDisplayUnit;

Type
  TModflowRCH_Writer = class(TCustomTransientArrayWriter)
  private
    NPRCH: integer;
    NRCHOP: integer;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSets3And4;
    procedure WriteDataSets5To8;
    procedure WriteCells(CellList: TValueCellList; const DataSetIdentifier,
      VariableIdentifiers: string);
  protected
    function CellType: TValueCellType; override;
    function Prefix: string; override;
    class function Extension: string; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    function Package: TModflowPackageSelection; override;
    function ParameterType: TParameterType; override;
    procedure WriteStressPeriods(const VariableIdentifiers, DataSetIdentifier,
      DS5, D7PNameIname, D7PName: string); override;
    procedure Evaluate; override;
  public
    procedure WriteFile(const AFileName: string);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
  end;

implementation

uses RbwParser, ModflowUnitNumbers, ModflowTransientListParameterUnit,
  frmErrorsAndWarningsUnit, DataSetUnit, ModflowRchUnit, GoPhastTypes, 
  frmProgressUnit;

const
  ErrorRoot = 'One or more %s parameters have been eliminated '
    + 'because there are no cells associated with them.';

{ TModflowRCH_Writer }

function TModflowRCH_Writer.CellType: TValueCellType;
begin
  result := TRch_Cell;
end;

procedure TModflowRCH_Writer.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TRchBoundary;
begin
  inherited;
  for ScreenObjectIndex := 0 to PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := PhastModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowRchBoundary;
    if Boundary <> nil then
    begin
      Boundary.GetRechargeLayerCells(FLayers);
    end;
  end;
end;

class function TModflowRCH_Writer.Extension: string;
begin
  result := '.rch';
end;

function TModflowRCH_Writer.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowRchBoundary;
end;

function TModflowRCH_Writer.Package: TModflowPackageSelection;
begin
  result := PhastModel.ModflowPackages.RchPackage;
end;

function TModflowRCH_Writer.ParameterType: TParameterType;
begin
  result := ptRCH;
end;

function TModflowRCH_Writer.Prefix: string;
begin
  result := 'R'
end;

procedure TModflowRCH_Writer.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  List: TValueCellList;
//  ParameterValues: TValueCellList;
  ParameterValues: TList;
  ParametersUsed: TStringList;
  TimeIndex: Integer;
  Comment: string;
  ParamDefArrays: TList;
  RechRateTimes: TModflowBoundaryDisplayTimeList;
  RechLayerTimes: TModflowBoundaryDisplayTimeList;
  RechRateArray: TModflowBoundaryDisplayDataArray;
  RechLayerArray: TModflowBoundaryDisplayDataArray;
  DefArrayList: TList;
  Index: Integer;
  ATimeList: TModflowBoundaryDisplayTimeList;
const
  D7PNameIname = '';
  D7PName = '';
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;
//  ParameterValues := TValueCellList.Create(CellType);
  ParameterValues := TList.Create;
  try
    Evaluate;
    ParamDefArrays := TObjectList.Create;
    try
      EvaluateParameterDefinitions(ParamDefArrays, ErrorRoot);
      NPRCH := ParameterCount;
      NRCHOP := Ord(PhastModel.ModflowPackages.RchPackage.LayerOption) + 1;
      RechRateTimes := TimeLists[0];
      RechLayerTimes := TimeLists[1];

      Comment := '# Data Set 8: IRCH';
      if Values.Count = 0 then
      begin
        SetTimeListsUpToDate(TimeLists);
        Exit;
      end;
      for TimeIndex := 0 to Values.Count - 1 do
      begin
        RechRateArray := RechRateTimes[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        if RechLayerTimes = nil then
        begin
          RechLayerArray := nil;
        end
        else
        begin
          RechLayerArray := RechLayerTimes[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
        end;

        ParametersUsed := TStringList.Create;
        try
          RetrieveParametersForStressPeriod(D7PNameIname, D7PName, TimeIndex,
            ParametersUsed, ParameterValues);
          List := Values[TimeIndex];
          List.CheckRestore;

          if NPRCH = 0 then
          begin
            // data set 6
            AssignTransient2DArray(RechRateArray, 0, List, 0, rdtDouble, umAssign);
          end
          else
          begin
            // data set 7
            DefArrayList := ParamDefArrays[TimeIndex];
            UpdateTransient2DArray(RechRateArray, DefArrayList);
          end;
          RechRateArray.CacheData;

          // Data set 8
          if RechLayerArray <> nil then
          begin
            if (PhastModel.ModflowPackages.RchPackage.
              LayerOption = loSpecified)
              and not PhastModel.ModflowPackages.RchPackage.
              TimeVaryingLayers then
            begin
              RetrieveParametersForStressPeriod(D7PNameIname, D7PName, 0,
                ParametersUsed, ParameterValues);
              List := Values[0];
            end;
            UpdateLayerDisplay(List, ParameterValues, TimeIndex,
              RechLayerArray);
            RechLayerArray.CacheData;
          end;
          List.Cache;
        finally
          ParametersUsed.Free;
        end;
      end;
      for Index := 0 to TimeLists.Count - 1 do
      begin
        ATimeList := TimeLists[Index];
        if ATimeList <> nil then
        begin
          ATimeList.SetUpToDate(True);
        end;
      end;
    finally
      ParamDefArrays.Free;
    end;
  finally
    ParameterValues.Free;
  end;
end;

procedure TModflowRCH_Writer.WriteDataSet1;
begin
  NPRCH := ParameterCount;
  if NPRCH > 0 then
  begin
    WriteString('PARAMETER');
    WriteInteger(NPRCH);
    WriteString(' # PARAMETER NPRCH');
    NewLine;
  end;
end;

procedure TModflowRCH_Writer.WriteDataSet2;
var
  IRCHCB: integer;
begin
  NRCHOP := Ord(PhastModel.ModflowPackages.RchPackage.LayerOption) + 1;
  GetFlowUnitNumber(IRCHCB);

  WriteInteger(NRCHOP);
  WriteInteger(IRCHCB);
  WriteString(' # DataSet 2: NRCHOP IRCHCB');
  NewLine
end;

procedure TModflowRCH_Writer.WriteDataSets3And4;
const
  DS3 = ' # Data Set 3: PARNAM PARTYP Parval NCLU';
  DS3Instances = ' INSTANCES NUMINST';
  DS4A = ' # Data Set 4a: INSTNAM';
  DataSetIdentifier = 'Data Set 4b:';
  VariableIdentifiers = 'Condfact';
begin
  WriteParameterDefinitions(DS3, DS3Instances, DS4A, DataSetIdentifier,
    VariableIdentifiers, ErrorRoot);
end;

procedure TModflowRCH_Writer.WriteDataSets5To8;
const
  D7PName =      ' # Data Set 7: PARNAM';
  D7PNameIname = ' # Data Set 7: PARNAM Iname';
  DS5 = ' # Data Set 5: INRECH INIRCH';
  DataSetIdentifier = 'Data Set 6:';
  VariableIdentifiers = 'RECH';
begin
  WriteStressPeriods(VariableIdentifiers, DataSetIdentifier, DS5,
    D7PNameIname, D7PName);
end;

procedure TModflowRCH_Writer.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if PhastModel.PackageGeneratedExternally(StrRCH) then
  begin
    Exit;
  end;
//  frmProgress.AddMessage('Evaluating RCH Package data.');
  NameOfFile := FileName(AFileName);
  WriteToNameFile(StrRCH, PhastModel.UnitNumbers.UnitNumber(StrRCH),
    NameOfFile, foInput);
  Evaluate;
  OpenFile(FileName(AFileName));
  try
    frmProgress.AddMessage('Writing RCH Package input.');
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

    frmProgress.AddMessage('  Writing Data Sets 5 to 8.');
    WriteDataSets5To8;
  finally
    CloseFile;
//    Clear;
  end;
end;

procedure TModflowRCH_Writer.WriteCells(CellList: TValueCellList;
  const DataSetIdentifier, VariableIdentifiers: string);
var
  DefaultValue: double;
  DataType: TRbwDataType;
  DataTypeIndex: integer;
  Comment: string;
begin
  DefaultValue := 0;
  DataType := rdtDouble;
  DataTypeIndex := 0;
  Comment := DataSetIdentifier + ' ' + VariableIdentifiers;
  WriteTransient2DArray(Comment, DataTypeIndex, DataType,
    DefaultValue, CellList);
end;

procedure TModflowRCH_Writer.WriteStressPeriods(const VariableIdentifiers,
  DataSetIdentifier, DS5, D7PNameIname, D7PName: string);
var
  NP: Integer;
  List: TValueCellList;
  ParameterValues: TValueCellList;
  ParamIndex: Integer;
  ParametersUsed: TStringList;
  TimeIndex: Integer;
  INRECH, INIRCH: Integer;
  Comment: string;
begin
  ParameterValues := TValueCellList.Create(CellType);
  try
    ParameterValues.OwnsObjects := False;
    Comment := '# Data Set 8: IRCH';
    if Values.Count = 0 then
    begin
      frmErrorsAndWarnings.AddError('No recharge defined',
        'The recharge package is active but '
        + 'no recharge has been defined for any stress period.');
    end;
    for TimeIndex := 0 to Values.Count - 1 do
    begin
      if not frmProgress.ShouldContinue then
      begin
        Exit;
      end;
      frmProgress.AddMessage('    Writing Stress Period ' + IntToStr(TimeIndex+1));
      ParametersUsed := TStringList.Create;
      try
        RetrieveParametersForStressPeriod(D7PNameIname, D7PName, TimeIndex,
          ParametersUsed, ParameterValues);
        NP := ParametersUsed.Count;
        List := Values[TimeIndex];
        // data set 5;
        if NPRCH > 0 then
        begin
          INRECH := NP;
        end
        else
        begin
          INRECH := 1;
        end;
        INIRCH := 1;

        WriteInteger(INRECH);
        WriteInteger(INIRCH);
        WriteString(DS5 + ' Stress period ' + IntToStr(TimeIndex+1));
        NewLine;
        if not frmProgress.ShouldContinue then
        begin
          Exit;
        end;

        if NPRCH = 0 then
        begin
          // data set 6
          WriteCells(List, DataSetIdentifier, VariableIdentifiers);
        end
        else
        begin
          // data set 7
          for ParamIndex := 0 to ParametersUsed.Count - 1 do
          begin
            WriteString(ParametersUsed[ParamIndex]);
            NewLine;
          end;
        end;
        if not frmProgress.ShouldContinue then
        begin
          List.Cache;
          Exit;
        end;

        // Data set 8
        WriteLayerSelection(List, ParameterValues, TimeIndex, Comment);
        if not frmProgress.ShouldContinue then
        begin
          List.Cache;
          Exit;
        end;
        List.Cache;
      finally
        ParametersUsed.Free;
      end;
    end;
  finally
    ParameterValues.Free;
  end;
end;

end.