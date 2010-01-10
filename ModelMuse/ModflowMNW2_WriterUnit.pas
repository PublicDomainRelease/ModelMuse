unit ModflowMNW2_WriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit, Classes, Contnrs,
  PhastModelUnit, ScreenObjectUnit, ModflowCellUnit, ModflowMnw2Unit, SysUtils,
  ModflowBoundaryDisplayUnit, RbwParser;

type
  TMultinodeWell = class(TObject)
  private
    FCells: TValueCellList;
    FScreenObject: TScreenObject;
    function GetCell(Index: integer): TValueCell;
    function GetCellCount: integer;
    function WellBoundary: TMnw2Boundary;
    function VerticalWell: boolean;
    function ConstantWellLossParameters: boolean;
    procedure GetVerticalWellElevations(out Ztop, Zbotm: double);
  public
    property Cells[Index: integer]: TValueCell read GetCell;
    property CellCount: integer read GetCellCount;
    Destructor Destroy; override;
    class function IsScreenObjectVertical(
      AScreenObject: TScreenObject): Boolean;
  end;

  TModflowMNW2_Writer = class(TCustomPackageWriter)
  private
    FNameOfFile: string;
    FValues: TList;
    FWells: TList;
    FWellNames: TStringList;
    NNODES: Integer;
    FMnwiWells: TList;
    FMnwPackage: TMultinodeWellSelection;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet2A(WellBoundary: TMnw2Boundary; Well: TMultinodeWell);
    procedure WriteDataSet2B(WellBoundary: TMnw2Boundary; Well: TMultinodeWell);
    procedure WriteDataSet2C(ConstantWellLossParameters: Boolean;
      WellBoundary: TMnw2Boundary; Well: TMultinodeWell);
    procedure WriteDataSet2D(ConstantWellLossParameters: boolean;
      WellBoundary: TMnw2Boundary; Well: TMultinodeWell);
    procedure WriteDataSet2E(Well: TMultinodeWell; WellBoundary: TMnw2Boundary);
    procedure WriteDataSet2F(WellBoundary: TMnw2Boundary);
    procedure WriteDataSet2G(WellBoundary: TMnw2Boundary);
    procedure WriteDataSet2H(WellBoundary: TMnw2Boundary);
    procedure WriteDataSets3and4;
    procedure WriteDataSet3(StartTime: Double; StressPeriod: integer);
    procedure WriteDataSet4A(WellBoundary: TMnw2Boundary;
      TimeItem: TMnw2TimeItem);
    function GetQCut(Item: TMnw2TimeItem): Integer;
    procedure WriteDataSet4B(WellBoundary: TMnw2Boundary;
      TimeItem: TMnw2TimeItem);
    procedure WriteDataSet4(StartTime: Double);
    procedure EvaluateMnwi;
    procedure WriteMnwiDataSet1(AFileName: string);
    procedure WriteMnwiDataSet2;
    procedure WriteMnwiDataSet3(var StartUnitNumber: Integer;
      AFileName: string);
    procedure EvaluateVerticalScreenFormula(var Expression: TExpression;
      const ADataName: string; var Formula: string; Compiler: TRbwParser; WellBoundary: TMnw2Boundary);
    procedure CheckWells;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
    procedure Evaluate; 
  public
    Constructor Create(Model: TPhastModel); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure WriteMnwiFile(const AFileName: string;
      var StartUnitNumber: integer);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
  end;

implementation

uses
  ModflowUnitNumbers, frmProgressUnit, frmErrorsAndWarningsUnit, GoPhastTypes,
  ModflowTimeUnit, ModflowBoundaryUnit, frmFormulaErrorsUnit, Math;

{ TModflowMNW2_Writer }

constructor TModflowMNW2_Writer.Create(Model: TPhastModel);
begin
  inherited;
  FValues := TObjectList.Create;
  FWells := TObjectList.Create;
  FWellNames:= TStringList.Create;
  FWellNames.CaseSensitive := False;
  FWellNames.Sorted := True;
  FMnwiWells := TList.Create;
end;

destructor TModflowMNW2_Writer.Destroy;
begin
  FMnwiWells.Free;
  FWellNames.Free;
  FWells.Free;
  FValues.Free;
  inherited;
end;

procedure TModflowMNW2_Writer.EvaluateMnwi;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TMnw2Boundary;
begin
  frmProgress.AddMessage('Evaluating MNWI Package data.');
  for ScreenObjectIndex := 0 to PhastModel.ScreenObjectCount - 1 do
  begin
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;
    ScreenObject := PhastModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowMnw2Boundary;
    if (Boundary = nil) or not Boundary.Used then
    begin
      Continue;
    end;
    frmProgress.AddMessage('    Evaluating ' + ScreenObject.Name);
    if Boundary.SaveMnwiInfo
      or Boundary.SaveExternalFlows
      or Boundary.SaveInternalFlows then
    begin
      FMnwiWells.Add(Boundary);
    end;
  end;
end;

procedure TModflowMNW2_Writer.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TMnw2Boundary;
  Dummy: TStringList;
  Well: TMultinodeWell;
  Item: TCustomModflowBoundaryItem;
  StressPeriod: TModflowStressPeriod;
begin
  frmProgress.AddMessage('Evaluating MNW2 Package data.');
  Dummy := TStringList.Create;
  try
    for ScreenObjectIndex := 0 to PhastModel.ScreenObjectCount - 1 do
    begin
      if not frmProgress.ShouldContinue then
      begin
        Exit;
      end;
      ScreenObject := PhastModel.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      Boundary := ScreenObject.ModflowMnw2Boundary;
      if (Boundary = nil) or not Boundary.Used then
      begin
        Continue;
      end;

      Item := Boundary.Values[0];
      StressPeriod := PhastModel.ModflowFullStressPeriods[0];
      Item.StartTime := StressPeriod.StartTime;
      StressPeriod := PhastModel.ModflowFullStressPeriods[
        PhastModel.ModflowFullStressPeriods.Count-1];
      Item.EndTime := StressPeriod.EndTime;

      frmProgress.AddMessage('    Evaluating ' + ScreenObject.Name);
      Boundary.GetCellValues(FValues, Dummy);
      if (FValues.Count >= 1) then
      begin
        Assert(FValues.Count = 1);
        Well := TMultinodeWell.Create;
        Well.FCells := FValues.Extract(FValues[0]);
        if Well.FCells.Count > 0 then
        begin
          FWells.Add(Well);
          Well.FScreenObject := ScreenObject;
        end
        else
        begin
          Well.Free;
        end;
      end;
    end;
    CheckWells;
  finally
    Dummy.Free;
  end;
end;

class function TModflowMNW2_Writer.Extension: string;
begin
  result := '.mnw2';
end;

function TModflowMNW2_Writer.Package: TModflowPackageSelection;
begin
  result := PhastModel.ModflowPackages.Mnw2Package;
end;

procedure TModflowMNW2_Writer.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  WellRadiusTimes: TModflowBoundaryDisplayTimeList;
  List: TValueCellList;
  Well : TMultinodeWell;
  WellIndex: integer;
  DataSets: TList;
  UsedIndicies: TByteSet;
  TimeIndex: Integer;
  Boundary: TMnw2Boundary;
  DataTypeIndex: Integer;
  TimeListIndex: Integer;
  TimeList: TModflowBoundaryDisplayTimeList;
  DataArray: TModflowBoundaryDisplayDataArray;
  DataSetIndex: Integer;
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;

  DataSets := TList.Create;
  try
    Evaluate;

    if FWells.Count = 0 then
    begin
      SetTimeListsUpToDate(TimeLists);
      Exit;
    end;

    WellRadiusTimes := TimeLists[0];
    for TimeIndex := 0 to WellRadiusTimes.Count - 1 do
    begin
      DataSets.Clear;

      for TimeListIndex := 0 to TimeLists.Count - 1 do
      begin
        TimeList := TimeLists[TimeListIndex];
        DataArray := TimeList[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        DataSets.Add(DataArray);
      end;

      for WellIndex := 0 to FWells.Count - 1 do
      begin
        Well := FWells[WellIndex];
        Boundary := Well.WellBoundary;
        UsedIndicies := [];
        for DataTypeIndex := WellRadiusPosition to PartialPenetrationPosition do
        begin
          if Boundary.DataTypeUsed(DataTypeIndex) then
          begin
            Include(UsedIndicies, DataTypeIndex);
          end;
        end;

        if UsedIndicies <> [] then
        begin
          List := Well.FCells;
          List.CheckRestore;

          UpdateCellDisplay(List, DataSets, [], nil, UsedIndicies);
          List.Cache;
        end;
      end;
      for DataSetIndex := 0 to DataSets.Count - 1 do
      begin
        DataArray := DataSets[DataSetIndex];
        DataArray.UpToDate := True;
        DataArray.CacheData;
      end;
    end;
    SetTimeListsUpToDate(TimeLists);
  finally
    DataSets.Free;
  end;
end;

procedure TModflowMNW2_Writer.CheckWells;
var
  TimeItem: TMnw2TimeItem;
  WellBoundary: TMnw2Boundary;
  TimeIndex: Integer;
  WellIndex: Integer;
//  ScreenObject: TScreenObject;
  Well: TMultinodeWell;
  Limit: integer;
const
  SignError = 'The deactivation pumping rate and reactivation pumping rate '
    + 'should have the same sign as the pumping rate.';
  RelativeSizeError = 'The reactivation pumping rate should be larger than '
    + 'the deactivation pumping rate.';
  SizeError = 'The reactivation pumping rate and '
    + 'deactivation pumping rate should be smaller than the pumping rate.';
  InvalidFractionError =
    'The deactivation pumping rate and reactivation pumping rate '
    + 'should both be between 0 and 1.';
begin
  for WellIndex := 0 to FWells.Count - 1 do
  begin
    Well := FWells[WellIndex];
    WellBoundary := Well.WellBoundary;
    if WellBoundary.ConstrainPumping then
    begin
      if WellBoundary.ConstantConstraints then
      begin
        Limit := 1;
      end
      else
      begin
        Limit := WellBoundary.TimeValues.Count;
      end;
//      ScreenObject := Well.FScreenObject;
      for TimeIndex := 0 to Limit - 1 do
      begin
        TimeItem := WellBoundary.TimeValues.Items[TimeIndex] as TMnw2TimeItem;
        case TimeItem.LimitMethod of
          mlmNoMinimum:
            begin
            end;
          mlmRate:
            begin
              if TimeItem.PumpingRateValue <> 0 then
              begin
                if (Sign(TimeItem.PumpingRateValue)
                  <> Sign(TimeItem.InactivationPumpingRateValue))
                  or (Sign(TimeItem.PumpingRateValue)
                  <> Sign(TimeItem.ReactivationPumpingRateValue))
                  or (Sign(TimeItem.InactivationPumpingRateValue)
                  <> Sign(TimeItem.ReactivationPumpingRateValue)) then
                begin
                  frmErrorsAndWarnings.AddError(SignError,
                    'Object = ' + Well.FScreenObject.Name
                    + '; Starting time = ' + FloatToStr(TimeItem.StartTime));
                end;
                if Abs(TimeItem.InactivationPumpingRateValue) >=
                  Abs(TimeItem.ReactivationPumpingRateValue) then
                begin
                  frmErrorsAndWarnings.AddError(RelativeSizeError,
                    'Object = ' + Well.FScreenObject.Name
                    + '; Starting time = ' + FloatToStr(TimeItem.StartTime));
                end;
                if (Abs(TimeItem.InactivationPumpingRateValue)
                  >= Abs(TimeItem.PumpingRateValue))
                  or (Abs(TimeItem.ReactivationPumpingRateValue)
                  >= Abs(TimeItem.PumpingRateValue)) then
                begin
                  frmErrorsAndWarnings.AddError(SizeError,
                    'Object = ' + Well.FScreenObject.Name
                    + '; Starting time = ' + FloatToStr(TimeItem.StartTime));
                end;
              end;
            end;
          mlmFraction:
            begin
              if TimeItem.PumpingRateValue <> 0 then
              begin
                if (TimeItem.InactivationPumpingRateValue < 0)
                  or (TimeItem.InactivationPumpingRateValue > 1)
                  or (TimeItem.ReactivationPumpingRateValue < 0)
                  or (TimeItem.ReactivationPumpingRateValue > 1) then
                begin
                  frmErrorsAndWarnings.AddError(InvalidFractionError,
                    'Object = ' + Well.FScreenObject.Name
                    + '; Starting time = ' + FloatToStr(TimeItem.StartTime));
                end;
                if Abs(TimeItem.InactivationPumpingRateValue) >=
                  Abs(TimeItem.ReactivationPumpingRateValue) then
                begin
                  frmErrorsAndWarnings.AddError(RelativeSizeError,
                    'Object = ' + Well.FScreenObject.Name
                    + '; Starting time = ' + FloatToStr(TimeItem.StartTime));
                end;
              end;
            end;
        else
          // do nothing
          Assert(False);
        end;
      end;
    end;
  end;
end;

procedure TModflowMNW2_Writer.EvaluateVerticalScreenFormula(
  var Expression: TExpression; const ADataName: string; var Formula: string;
  Compiler: TRbwParser; WellBoundary: TMnw2Boundary);
  var
    LocalScreenObject: TScreenObject;
begin
  try
    Compiler.Compile(Formula);
  except
    on E: ERbwParserError do
    begin
      LocalScreenObject := WellBoundary.ScreenObject as TScreenObject;
      frmFormulaErrors.AddError(LocalScreenObject.Name, '', Formula, E.Message + '; Error in formula for ' + ADataName + '.');
      Formula := '0';
      Compiler.Compile(Formula);
    end;
  end;
  Expression := Compiler.CurrentExpression;
  Expression.Evaluate;
end;

procedure TModflowMNW2_Writer.WriteDataSet1;
const
  OPTION = ' AUXILIARY IFACE';
var
  MNWMAX: integer;
  IWL2CB: integer;
  MNWPRNT: integer;
begin
  MNWMAX := FWells.Count;
  GetFlowUnitNumber(IWL2CB);
  MNWPRNT := Ord(PhastModel.ModflowPackages.Mnw2Package.PrintOption);
  WriteInteger(MNWMAX);
  WriteInteger(IWL2CB);
  WriteInteger(MNWPRNT);
  WriteString(OPTION);
  WriteString(' # DataSet 1: MNWMAX, IWL2CB, MNWPRNT, OPTION');
  NewLine;
end;

procedure TModflowMNW2_Writer.WriteDataSet2;
var
  WellIndex: Integer;
  Well: TMultinodeWell;
  WellBoundary: TMnw2Boundary;
  ConstantWellLossParameters: Boolean;
begin
  for WellIndex := 0 to FWells.Count - 1 do
  begin
    Well := FWells[WellIndex];
    WellBoundary := Well.WellBoundary;
    WriteDataSet2A(WellBoundary, Well);
    WriteDataSet2B(WellBoundary, Well);
    ConstantWellLossParameters := Well.ConstantWellLossParameters;
    WriteDataSet2C(ConstantWellLossParameters, WellBoundary, Well);
    WriteDataSet2D(ConstantWellLossParameters, WellBoundary, Well);
    WriteDataSet2E(Well, WellBoundary);
    WriteDataSet2F(WellBoundary);
    WriteDataSet2G(WellBoundary);
    WriteDataSet2H(WellBoundary);
  end;
end;

procedure TModflowMNW2_Writer.WriteFile(const AFileName: string);
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if PhastModel.PackageGeneratedExternally(StrMNW2) then
  begin
    Exit;
  end;
  FNameOfFile := FileName(AFileName);
  WriteToNameFile(StrMNW2, PhastModel.UnitNumbers.UnitNumber(StrMNW2),
    FNameOfFile, foInput);
  Evaluate;
  OpenFile(FNameOfFile);
  try
    frmProgress.AddMessage('Writing MNW2 Package input.');
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
    WriteDataSets3and4;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;
  finally
    CloseFile;
  end;
end;

procedure TModflowMNW2_Writer.WriteMnwiFile(const AFileName: string;
  var StartUnitNumber: integer);
var
  NameOfMnwiFile: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if PhastModel.PackageGeneratedExternally(StrMNWI) then
  begin
    Exit;
  end;
  EvaluateMnwi;
  FMnwPackage := PhastModel.ModflowPackages.Mnw2Package;
  if (FMnwiWells.Count > 0) or FMnwPackage.CreateWellFile
    or FMnwPackage.SummarizeByWell or FMnwPackage.SummarizeByNode then
  begin
    NameOfMnwiFile := ChangeFileExt(AFileName, '.mnwi');;
    WriteToNameFile(StrMNWI, PhastModel.UnitNumbers.UnitNumber(StrMNWI),
      NameOfMnwiFile, foInput);

    OpenFile(NameOfMnwiFile);
    try
      frmProgress.AddMessage('Writing MNWI Package input.');
      WriteMnwiDataSet1(AFileName);
      WriteMnwiDataSet2;
      WriteMnwiDataSet3(StartUnitNumber, AFileName);

    finally
      CloseFile;
    end;

  end;
end;

procedure TModflowMNW2_Writer.WriteMnwiDataSet3(var StartUnitNumber: Integer;
  AFileName: string);

var
  QBHflag: Integer;
  QNDflag: Integer;
  UNIT_Number: Integer;
  WELLID: string;
  Boundary: TMnw2Boundary;
  WellIndex: Integer;

  OutputFileName: string;
begin
  AFileName := ExtractFileName(AFileName);
  AFileName := ChangeFileExt(AFileName, '');
  frmProgress.AddMessage('  Writing Data Set 3.');
  for WellIndex := 0 to FMnwiWells.Count - 1 do
  begin
    Boundary := FMnwiWells[WellIndex];
    WELLID := Boundary.WellID;
    OutputFileName := AFileName + '_' + WELLID;
    OutputFileName := StringReplace(OutputFileName, ' ', '_', [rfReplaceAll, rfIgnoreCase]);
    OutputFileName := ChangeFileExt(OutputFileName, '.mnwi_out');

    if Pos(' ', WELLID) > 0 then
    begin
      WELLID := '"' + WELLID + '"';
    end;
    WELLID := WELLID + ' ';
    UNIT_Number := StartUnitNumber;
    Inc(StartUnitNumber);
    if Boundary.SaveExternalFlows then
    begin
      QNDflag := 1;
    end
    else
    begin
      QNDflag := 0;
    end;
    if Boundary.SaveInternalFlows then
    begin
      QBHflag := 1;
    end
    else
    begin
      QBHflag := 0;
    end;
    WriteString(WELLID);
    WriteInteger(UNIT_Number);
    WriteInteger(QNDflag);
    WriteInteger(QBHflag);
    WriteString(' # WELLID, UNIT, QNDflag, QBHflag');
    NewLine;

    WriteToNameFile(StrDATA, UNIT_Number,
      OutputFileName, foOutput);
  end;
end;

procedure TModflowMNW2_Writer.WriteMnwiDataSet2;
var
  MNWOBS: Integer;
begin
  frmProgress.AddMessage('  Writing Data Set 2.');

  MNWOBS := FMnwiWells.Count;
  WriteInteger(MNWOBS);
  WriteString(' # Data Set 2: MNWOBS');
  NewLine;
end;

procedure TModflowMNW2_Writer.WriteMnwiDataSet1(AFileName: string);
var
  WEL1flag: Integer;
  QSUMflag: Integer;
  BYNDflag: Integer;
  OutputFileName: string;
begin
  frmProgress.AddMessage('  Writing Data Set 1.');
  AFileName := ExtractFileName(AFileName);
  AFileName := ChangeFileExt(AFileName, '');
  if FMnwPackage.CreateWellFile then
  begin
    WEL1flag := PhastModel.UnitNumbers.UnitNumber(StrMNWI_Wells);
    OutputFileName := ChangeFileExt(AFileName, '.wel_out');
    WriteToNameFile(StrDATA, WEL1flag,
      OutputFileName, foOutput);
  end
  else
  begin
    WEL1flag := 0;
  end;
  if FMnwPackage.SummarizeByWell then
  begin
    QSUMflag := PhastModel.UnitNumbers.UnitNumber(StrMNWI_SummarizeByWell);
    OutputFileName := ChangeFileExt(AFileName, '.QSUM_out');
    WriteToNameFile(StrDATA, QSUMflag,
      OutputFileName, foOutput);
  end
  else
  begin
    QSUMflag := 0;
  end;
  if FMnwPackage.SummarizeByNode then
  begin
    BYNDflag := PhastModel.UnitNumbers.UnitNumber(StrMNWI_SummarizeByNode);
    OutputFileName := ChangeFileExt(AFileName, '.BYND_out');
    WriteToNameFile(StrDATA, BYNDflag,
      OutputFileName, foOutput);
  end
  else
  begin
    BYNDflag := 0;
  end;
  WriteInteger(WEL1flag);
  WriteInteger(QSUMflag);
  WriteInteger(BYNDflag);
  WriteString(' # Data Set 1: WEL1flag, QSUMflag, BYNDflag');
  NewLine;
end;

procedure TModflowMNW2_Writer.WriteDataSet4(StartTime: Double);
var
  WellBoundary: TMnw2Boundary;
  TimeIndex: Integer;
  TimeItem: TMnw2TimeItem;
  WellIndex: Integer;
  Well: TMultinodeWell;
begin
  for WellIndex := 0 to FWells.Count - 1 do
  begin
    Well := FWells[WellIndex];
    WellBoundary := Well.WellBoundary;
    TimeIndex := WellBoundary.TimeValues.IndexOfContainedStartTime(StartTime);
    if TimeIndex >= 0 then
    begin
      TimeItem := WellBoundary.TimeValues.Items[TimeIndex] as TMnw2TimeItem;
      WriteDataSet4A(WellBoundary, TimeItem);
      WriteDataSet4B(WellBoundary, TimeItem);
    end;
  end;
end;

procedure TModflowMNW2_Writer.WriteDataSet4B(WellBoundary: TMnw2Boundary;
  TimeItem: TMnw2TimeItem);
var
  Qfrcmx: Double;
  Qfrcmn: Double;
  Comment: string;
  QCut: Integer;
  Hlim: Double;
begin
  if WellBoundary.ConstrainPumping and
    not WellBoundary.ConstantConstraints then
  begin
    Hlim := TimeItem.LimitingWaterLevelValue;
    QCut := GetQCut(TimeItem);
    Comment := ' # Data Set 4B: Hlim, QCut';
    WriteFloat(Hlim);
    WriteInteger(QCut);
    if QCut <> 0 then
    begin
      Qfrcmn := TimeItem.InactivationPumpingRateValue;
      Qfrcmx := TimeItem.ReactivationPumpingRateValue;
      Comment := Comment + ', Qfrcmn, Qfrcmx';
      WriteFloat(Qfrcmn);
      WriteFloat(Qfrcmx);
    end;
    WriteString(Comment);
    NewLine;
  end;
end;

function TModflowMNW2_Writer.GetQCut(Item: TMnw2TimeItem): Integer;
begin
  result := 0;
  case Item.LimitMethod of
    mlmNoMinimum:
      result := 0;
    mlmRate:
      result := 1;
    mlmFraction:
      result := -1;
    else
      Assert(False);
  end;
end;

procedure TModflowMNW2_Writer.WriteDataSet4A(WellBoundary: TMnw2Boundary;
  TimeItem: TMnw2TimeItem);
var
  WELLID: string;
  QDes: Double;
  Comment: string;
  CapMult: Double;
  IFACE: TIface;
begin
  WELLID := WellBoundary.WellID;
  if Pos(' ', WELLID) > 0 then
  begin
    WELLID := '"' + WELLID + '"';
  end;
  WELLID := WELLID + ' ';
  QDes := TimeItem.PumpingRateValue;
  WriteString(WELLID);
  WriteFloat(QDes);
  Comment := ' # Data Set 4A: WELLID, QDes';
  if WellBoundary.AdjustPumping then
  begin
    CapMult := TimeItem.HeadCapacityMultiplierValue;
    WriteFloat(CapMult);
    Comment := Comment + ', CapMult';
  end;
  IFACE := (WellBoundary.ScreenObject as TScreenObject).IFace;
  WriteIFACE(IFACE);
  Comment := Comment + ', IFACE';
  WriteString(Comment);
  NewLine;
end;

procedure TModflowMNW2_Writer.WriteDataSet3(StartTime: Double;
  StressPeriod: integer);
var
  TimeIndex: Integer;
  WellBoundary: TMnw2Boundary;
  Well: TMultinodeWell;
  WellIndex: Integer;
  ITMP: Integer;
begin
  ITMP := 0;
  for WellIndex := 0 to FWells.Count - 1 do
  begin
    Well := FWells[WellIndex];
    WellBoundary := Well.WellBoundary;
    TimeIndex := WellBoundary.TimeValues.IndexOfContainedStartTime(StartTime);
    if TimeIndex >= 0 then
    begin
      Inc(ITMP);
    end;
  end;
  WriteInteger(ITMP);
  WriteString(' # Data Set 3, Stress Period '
    + IntToStr(StressPeriod+1) + ': ITMP');
  NewLine;
end;

procedure TModflowMNW2_Writer.WriteDataSet2H(WellBoundary: TMnw2Boundary);
var
  Qn: Double;
  LIFTn: Double;
  Item: TLiftItem;
  LiftIndex: Integer;
begin
  if WellBoundary.AdjustPumping then
  begin
    WellBoundary.LiftValues.Sort;
    for LiftIndex := 0 to WellBoundary.LiftValues.Count - 1 do
    begin
      Item := WellBoundary.LiftValues.Items[LiftIndex] as TLiftItem;
      LIFTn := Item.Lift;
      Qn := Item.Q;
      WriteFloat(LIFTn);
      WriteFloat(Qn);
      WriteString(' Data Set 2H: LIFTn, Qn');
      NewLine;
    end;
  end;
end;

procedure TModflowMNW2_Writer.WriteDataSets3and4;
var
  StressPeriodIndex: Integer;
  StressPeriod: TModflowStressPeriod;
begin
  for StressPeriodIndex := 0 to
    PhastModel.ModflowFullStressPeriods.Count - 1 do
  begin
    StressPeriod := PhastModel.ModflowFullStressPeriods[StressPeriodIndex];
    WriteDataSet3(StressPeriod.StartTime, StressPeriodIndex);
    WriteDataSet4(StressPeriod.StartTime);
  end;
end;

procedure TModflowMNW2_Writer.WriteDataSet2G(WellBoundary: TMnw2Boundary);
var
  Hlift: Double;
  HWtol: Double;
  LIFTqmax: Double;
  LIFTq0: Double;
begin
  if WellBoundary.AdjustPumping then
  begin
    Hlift := WellBoundary.ReferenceHead;
    LIFTq0 := WellBoundary.MaximumLift;
    LIFTqmax := WellBoundary.LiftAtMaxRate;
    HWtol := WellBoundary.WellTolerance;
    WriteFloat(Hlift);
    WriteFloat(LIFTq0);
    WriteFloat(LIFTqmax);
    WriteFloat(HWtol);
    WriteString(' Data Set 2G: Hlift, LIFTq0, LIFTqmax, HWtol');
    NewLine;
  end;
end;

procedure TModflowMNW2_Writer.WriteDataSet2F(WellBoundary: TMnw2Boundary);
var
  Qfrcmn: Double;
  Qfrcmx: Double;
  Comment: string;
  QCut: Integer;
  Hlim: Double;
  Item: TMnw2TimeItem;
begin
  if WellBoundary.ConstrainPumping and WellBoundary.ConstantConstraints then
  begin
    Item := WellBoundary.TimeValues[0] as TMnw2TimeItem;
    Hlim := Item.LimitingWaterLevelValue;
    QCut := GetQCut(Item);
    Comment := ' # Data Set 2F: Hlim, QCut';
    WriteFloat(Hlim);
    WriteInteger(QCut);
    if QCut <> 0 then
    begin
      Qfrcmn := Item.InactivationPumpingRateValue;
      Qfrcmx := Item.ReactivationPumpingRateValue;
      Comment := Comment + ', Qfrcmn, Qfrcmx';
      WriteFloat(Qfrcmn);
      WriteFloat(Qfrcmx);
    end;
    WriteString(Comment);
    NewLine;
  end;
end;

procedure TModflowMNW2_Writer.WriteDataSet2E(Well: TMultinodeWell;
  WellBoundary: TMnw2Boundary);
var
  PumpLocation: TCellLocation;
  Zpump: Double;
begin
  if WellBoundary.SpecifyPump then
  begin
    if Well.VerticalWell then
    begin
      Zpump := WellBoundary.PumpElevation;
      WriteFloat(Zpump);
      WriteString(' # Data Set 2E: Zpump');
      NewLine;
    end
    else
    begin
      PumpLocation := WellBoundary.TargetCellLocation;
      WriteInteger(PumpLocation.Layer);
      WriteInteger(PumpLocation.Row);
      WriteInteger(PumpLocation.Column);
      WriteString(' # Data Set 2E: PUMPLAY, PUMPROW, PUMPCOL');
      NewLine;
    end;
  end;
end;

procedure TModflowMNW2_Writer.WriteDataSet2D(
  ConstantWellLossParameters: boolean; WellBoundary: TMnw2Boundary;
  Well: TMultinodeWell);
var
  Comment: string;
  COL: Integer;
  ROW: Integer;
  Zbotm: Double;
  Ztop: Double;
  Cell: TMnw2_Cell;
  PP: Double;
  LAY: Integer;
  CellIndex: Integer;
  ScreenIndex: Integer;
  VerticalScreen: TVerticalScreen;
  Compiler: TRbwParser;
  Formula: string;
  Expression: TExpression;
  Rw: double;
  Rskin: double;
  Kskin: double;
  B: double;
  C: double;
  P: Double;
  CWC: double;
  procedure WriteOptionalData;
  var
    Rw: Double;
    Rskin: Double;
    Kskin: Double;
    B: Double;
    C: Double;
    P: Double;
    CWC: Double;
  begin
    if not ConstantWellLossParameters then
    begin
      case WellBoundary.LossType of
        mltNone: Assert(False);
        mltThiem:
          begin
            Rw := Cell.WellRadius;
            WriteFloat(Rw);
            Comment := Comment +', Rw';
          end;
        mltSkin:
          begin
            Rw := Cell.WellRadius;
            Rskin := Cell.SkinRadius;
            Kskin := Cell.SkinK;
            WriteFloat(Rw);
            WriteFloat(Rskin);
            WriteFloat(Kskin);
            Comment := Comment +', Rw, Rskin, Kskin';
          end;
        mltEquation:
          begin
            Rw := Cell.WellRadius;
            B := Cell.B;
            C := Cell.C;
            P := Cell.P;
            WriteFloat(Rw);
            WriteFloat(B);
            WriteFloat(C);
            WriteFloat(P);
            Comment := Comment +', Rw, B, C, P';
          end;
        mtlSpecify:
          begin
            CWC := Cell.CellToWellConductance;
            WriteFloat(CWC);
            Comment := Comment +', CWC';
          end;
        else Assert(False);
      end;
    end;
  end;
begin
  if Well.VerticalWell and (WellBoundary.LossType <> mltNone) then
  begin
    Cell := Well.Cells[0] as TMnw2_Cell;
    ROW := Cell.Row+1;
    COL := Cell.Column+1;
    if WellBoundary.VerticalScreens.Count > 0 then
    begin
      for ScreenIndex := 0 to WellBoundary.VerticalScreens.Count - 1 do
      begin
        VerticalScreen := WellBoundary.
          VerticalScreens.Items[ScreenIndex] as TVerticalScreen;
        Ztop := VerticalScreen.ZTop;
        Zbotm := VerticalScreen.ZBottom;
        WriteFloat(Ztop);
        WriteFloat(Zbotm);
        WriteInteger(ROW);
        WriteInteger(COL);
        Comment := ' # Data Set 2D; Ztop, Zbotm, ROW, COL';

        Compiler := PhastModel.rpThreeDFormulaCompiler;

        case WellBoundary.LossType of
          mltNone: Assert(False);
          mltThiem:
            begin
              Formula := VerticalScreen.WellRadius;
              EvaluateVerticalScreenFormula(Expression, 'Rw', Formula,
                Compiler, WellBoundary);
              if Formula <> VerticalScreen.WellRadius then
              begin
                VerticalScreen.WellRadius := Formula;
              end;
              Rw := Expression.DoubleResult;
              WriteFloat(Rw);
              Comment := Comment +', Rw';
            end;
          mltSkin:
            begin
              Formula := VerticalScreen.WellRadius;
              EvaluateVerticalScreenFormula(Expression, 'Rw', Formula,
                Compiler, WellBoundary);
              if Formula <> VerticalScreen.WellRadius then
              begin
                VerticalScreen.WellRadius := Formula;
              end;
              Rw := Expression.DoubleResult;
              WriteFloat(Rw);

              Formula := VerticalScreen.SkinRadius;
              EvaluateVerticalScreenFormula(Expression, 'Rskin', Formula,
                Compiler, WellBoundary);
              if Formula <> VerticalScreen.SkinRadius then
              begin
                VerticalScreen.SkinRadius := Formula;
              end;
              Rskin := Expression.DoubleResult;
              WriteFloat(Rskin);

              Formula := VerticalScreen.SkinK;
              EvaluateVerticalScreenFormula(Expression, 'Kskin', Formula,
                Compiler, WellBoundary);
              if Formula <> VerticalScreen.SkinK then
              begin
                VerticalScreen.SkinK := Formula;
              end;
              Kskin := Expression.DoubleResult;
              WriteFloat(Kskin);

              Comment := Comment +', Rw, Rskin, Kskin';
            end;
          mltEquation:
            begin
              Formula := VerticalScreen.WellRadius;
              EvaluateVerticalScreenFormula(Expression, 'Rw', Formula,
                Compiler, WellBoundary);
              if Formula <> VerticalScreen.WellRadius then
              begin
                VerticalScreen.WellRadius := Formula;
              end;
              Rw := Expression.DoubleResult;
              WriteFloat(Rw);

              Formula := VerticalScreen.B;
              EvaluateVerticalScreenFormula(Expression, 'B', Formula,
                Compiler, WellBoundary);
              if Formula <> VerticalScreen.B then
              begin
                VerticalScreen.B := Formula;
              end;
              B := Expression.DoubleResult;
              WriteFloat(B);


              Formula := VerticalScreen.C;
              EvaluateVerticalScreenFormula(Expression, 'C', Formula,
                Compiler, WellBoundary);
              if Formula <> VerticalScreen.C then
              begin
                VerticalScreen.C := Formula;
              end;
              C := Expression.DoubleResult;
              WriteFloat(C);

              Formula := VerticalScreen.P;
              EvaluateVerticalScreenFormula(Expression, 'P', Formula,
                Compiler, WellBoundary);
              if Formula <> VerticalScreen.P then
              begin
                VerticalScreen.P := Formula;
              end;
              P := Expression.DoubleResult;
              WriteFloat(P);

              Comment := Comment +', Rw, B, C, P';
            end;
          mtlSpecify:
            begin
              Formula := VerticalScreen.CellToWellConductance;
              EvaluateVerticalScreenFormula(Expression, 'CWC', Formula,
                Compiler, WellBoundary);
              if Formula <> VerticalScreen.CellToWellConductance then
              begin
                VerticalScreen.CellToWellConductance := Formula;
              end;
              CWC := Expression.DoubleResult;
              WriteFloat(CWC);

              Comment := Comment +', CWC';
            end;
          else Assert(False);
        end;
        WriteString(Comment);
        NewLine;
      end;
    end
    else
    begin
      Well.GetVerticalWellElevations(Ztop, Zbotm);
      WriteFloat(Ztop);
      WriteFloat(Zbotm);
      WriteInteger(ROW);
      WriteInteger(COL);
      Comment := ' # Data Set 2D; Ztop, Zbotm, ROW, COL';
      WriteOptionalData;
      WriteString(Comment);
      NewLine;
    end;
  end
  else
  begin
    for CellIndex := 0 to Well.CellCount - 1 do
    begin
      Cell := Well.Cells[CellIndex] as TMnw2_Cell;
      LAY := Cell.Layer+1;
      ROW := Cell.Row+1;
      COL := Cell.Column+1;
      WriteInteger(LAY);
      WriteInteger(ROW);
      WriteInteger(COL);
      Comment := ' # Data Set 2D; LAY, ROW, COL';
      WriteOptionalData;
      if WellBoundary.PartialPenetrationCorrection then
      begin
        PP := Cell.PartialPenetration;
        WriteFloat(PP);
        Comment := Comment + ', PP';
      end;
      WriteString(Comment);
      NewLine;
    end;
  end;
end;

procedure TModflowMNW2_Writer.WriteDataSet2C(
  ConstantWellLossParameters: Boolean; WellBoundary: TMnw2Boundary;
  Well: TMultinodeWell);
var
  CWC: Double;
  P: Double;
  C: Double;
  B: Double;
  Kskin: Double;
  Rskin: Double;
  Rw: Double;
  Cell: TMnw2_Cell;
begin
  case WellBoundary.LossType of
    mltNone:
      begin
      end;
    mltThiem:
      begin
        if (WellBoundary.VerticalScreens.Count = 0)
          and ConstantWellLossParameters then
        begin
          Cell := Well.Cells[0] as TMnw2_Cell;
          Rw := Cell.WellRadius;
        end
        else
        begin
          Rw := -1;
        end;
        WriteFloat(Rw);
        WriteString(' # Data Set 2C; Rw');
        NewLine;
      end;
    mltSkin:
      begin
        if (WellBoundary.VerticalScreens.Count = 0)
          and ConstantWellLossParameters then
        begin
          Cell := Well.Cells[0] as TMnw2_Cell;
          Rw := Cell.WellRadius;
          Rskin := Cell.SkinRadius;
          Kskin := Cell.SkinK;
        end
        else
        begin
          Rw := -1;
          Rskin := -1;
          Kskin := -1;
        end;
        WriteFloat(Rw);
        WriteFloat(Rskin);
        WriteFloat(Kskin);
        WriteString(' # Data Set 2C; Rw, Rskin, Kskin');
        NewLine;
      end;
    mltEquation:
      begin
        if (WellBoundary.VerticalScreens.Count = 0)
          and ConstantWellLossParameters then
        begin
          Cell := Well.Cells[0] as TMnw2_Cell;
          Rw := Cell.WellRadius;
          B := Cell.B;
          C := Cell.C;
          P := Cell.P;
        end
        else
        begin
          Rw := -1;
          B := -1;
          C := -1;
          P := -1;
        end;
        WriteFloat(Rw);
        WriteFloat(B);
        WriteFloat(C);
        WriteFloat(P);
        WriteString(' # Data Set 2C; Rw, B, C, P');
        NewLine;
      end;
    mtlSpecify:
      begin
        if (WellBoundary.VerticalScreens.Count = 0)
          and ConstantWellLossParameters then
        begin
          Cell := Well.Cells[0] as TMnw2_Cell;
          CWC := Cell.CellToWellConductance;
        end
        else
        begin
          CWC := -1;
        end;
        WriteFloat(CWC);
        WriteString(' # Data Set 2C; CWC');
        NewLine;
      end;
  else
    Assert(False);
  end;
end;

procedure TModflowMNW2_Writer.WriteDataSet2B(WellBoundary: TMnw2Boundary;
  Well: TMultinodeWell);
var
  Qlimit: Integer;
  PUMPLOC: Integer;
  LOSSTYPE: string;
  PUMPCAP: Integer;
  PPFLAG: Integer;
const
  LossTypes: array[Low(TMnwLossType)..High(TMnwLossType)] of string =
    ('NONE ', 'THIEM ', 'SKIN ', 'GENERAL ', 'SPECIFYcwc ');
  LossTypeError = 'In the MNW2 package, a LOSSTYPE of "NONE" is only '
    + 'valid if the well has only one cell. Objects in which this is violated'
    + ' are listed below.';
begin
  if WellBoundary.LossType = mltNone then
  begin
    if NNODES <> 1 then
    begin
      frmErrorsAndWarnings.AddError(LossTypeError, Well.FScreenObject.Name);
    end;
  end;
  LOSSTYPE := LossTypes[WellBoundary.LossType];
  if WellBoundary.SpecifyPump then
  begin
    if Well.VerticalWell then
    begin
      PUMPLOC := -1;
    end
    else
    begin
      PUMPLOC := 1;
    end;
  end
  else
  begin
    PUMPLOC := 0;
  end;
  if WellBoundary.ConstrainPumping then
  begin
    if WellBoundary.ConstantConstraints then
    begin
      Qlimit := 1;
    end
    else
    begin
      Qlimit := -1;
    end;
  end
  else
  begin
    Qlimit := 0;
  end;
  if WellBoundary.PartialPenetrationCorrection then
  begin
    PPFLAG := 1;
  end
  else
  begin
    PPFLAG := 0;
  end;
  if WellBoundary.AdjustPumping then
  begin
    PUMPCAP := WellBoundary.LiftValues.Count;
  end
  else
  begin
    PUMPCAP := 0;
  end;
  WriteString(LOSSTYPE);
  WriteInteger(PUMPLOC);
  WriteInteger(Qlimit);
  WriteInteger(PPFLAG);
  WriteInteger(PUMPCAP);
  WriteString(' # Data Set 2B: LOSSTYPE, PUMPLOC, Qlimit, PPFLAG, PUMPCAP');
  NewLine;
end;

procedure TModflowMNW2_Writer.WriteDataSet2A(WellBoundary: TMnw2Boundary;
  Well: TMultinodeWell);
var
  WELLID: string;
const
  ErrorRoot = 'The following objects define WELLIDs '
    + 'in the MNW2 that are not unique.';
  ErrorRoot2 = 'Vertical screens are not allowed '
    + 'in wells in which the LOSSTYPE is "NONE".';
begin
  WELLID := WellBoundary.WellID;
  if FWellNames.IndexOf(WELLID) >= 0 then
  begin
    frmErrorsAndWarnings.AddError(ErrorRoot,
      'Object = ' + Well.FScreenObject.Name
      + '; ' + 'WELLID = ' + WELLID);
  end;
  FWellNames.Add(WELLID);
  if Pos(' ', WELLID) > 0 then
  begin
    WELLID := '"' + WELLID + '"';
  end;
  WELLID := WELLID + ' ';
  NNODES := Well.FCells.Count;
  if Well.VerticalWell then
  begin
    if WellBoundary.LossType = mltNone then
    begin
      if WellBoundary.VerticalScreens.Count > 0 then
      begin
        frmErrorsAndWarnings.AddError(ErrorRoot2,
          'Object = ' + Well.FScreenObject.Name
          + '; ' + 'WELLID = ' + WELLID);
      end;
    end;
    if WellBoundary.VerticalScreens.Count > 0 then
    begin
      NNODES := -WellBoundary.VerticalScreens.Count;
    end
    else if WellBoundary.LossType <> mltNone then
    begin
      NNODES := -1;
    end;
  end;
  WriteString(WELLID);
  WriteInteger(NNODES);
  WriteString(' # Data Set 2A: WELLID, NNODES');
  NewLine;
end;

{ TMultinodeWell }

function TMultinodeWell.ConstantWellLossParameters: boolean;
var
  WellRadius: double;
  Cell: TMnw2_Cell;
  CellIndex: Integer;
  SkinRadius: Double;
  SkinK: Double;
  B: Double;
  C: Double;
  CellToWellConductance: Double;
  P: Double;
begin
  Assert(CellCount> 0);
  Result := True;
  case WellBoundary.LossType of
    mltNone: result := True;
    mltThiem:
      begin
        Cell := Cells[0] as TMnw2_Cell;
        WellRadius := Cell.WellRadius;
        for CellIndex := 1 to CellCount - 1 do
        begin
          Cell := Cells[CellIndex] as TMnw2_Cell;
          result := WellRadius = Cell.WellRadius;
          if not Result then
          begin
            Exit;
          end;
        end;
      end;
    mltSkin:
      begin
        Cell := Cells[0] as TMnw2_Cell;
        WellRadius := Cell.WellRadius;
        SkinRadius := Cell.SkinRadius;
        SkinK := Cell.SkinK;
        for CellIndex := 1 to CellCount - 1 do
        begin
          Cell := Cells[CellIndex] as TMnw2_Cell;
          result := (WellRadius = Cell.WellRadius)
            and (SkinRadius = Cell.SkinRadius)
            and (SkinK = Cell.SkinK);
          if not Result then
          begin
            Exit;
          end;
        end;
      end;
    mltEquation:
      begin
        Cell := Cells[0] as TMnw2_Cell;
        WellRadius := Cell.WellRadius;
        B := Cell.B;
        C := Cell.C;
        P := Cell.P;
        for CellIndex := 1 to CellCount - 1 do
        begin
          Cell := Cells[CellIndex] as TMnw2_Cell;
          result := (WellRadius = Cell.WellRadius)
            and (B = Cell.B)
            and (C = Cell.C)
            and (P = Cell.P);
          if not Result then
          begin
            Exit;
          end;
        end;
      end;
    mtlSpecify:
      begin
        Cell := Cells[0] as TMnw2_Cell;
        CellToWellConductance := Cell.CellToWellConductance;
        for CellIndex := 1 to CellCount - 1 do
        begin
          Cell := Cells[CellIndex] as TMnw2_Cell;
          result := CellToWellConductance = Cell.CellToWellConductance;
          if not Result then
          begin
            Exit;
          end;
        end;
      end;
    else Assert(False);
  end;
end;

destructor TMultinodeWell.Destroy;
begin
  FCells.Free;
  inherited;
end;

class function TMultinodeWell.IsScreenObjectVertical(
  AScreenObject: TScreenObject): Boolean; 
begin
  result := (AScreenObject.Count = 1)
    and (AScreenObject.ViewDirection = vdTop)
    and (AScreenObject.ElevationCount = ecTwo);
end;

function TMultinodeWell.GetCell(Index: integer): TValueCell;
begin
  result := FCells[Index];
end;

function TMultinodeWell.GetCellCount: integer;
begin
  result := FCells.Count;
end;

procedure TMultinodeWell.GetVerticalWellElevations(out Ztop, Zbotm: double);
begin
  Zbotm := FScreenObject.BottomElevation;
  Ztop := FScreenObject.TopElevation;
end;

function TMultinodeWell.VerticalWell: boolean;
begin
  Result := IsScreenObjectVertical(FScreenObject);
end;

function TMultinodeWell.WellBoundary: TMnw2Boundary;
begin
  result := FScreenObject.ModflowMnw2Boundary;
  assert(result <> nil);
end;

end.