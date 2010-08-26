unit ModflowUzfWriterUnit;

interface

uses SysUtils, Classes, Contnrs, RbwParser, PhastModelUnit,
  CustomModflowWriterUnit, ModflowPackageSelectionUnit, ScreenObjectUnit,
  ModflowBoundaryUnit, OrderedCollectionUnit, ModflowBoundaryDisplayUnit;

type
  TModflowUzfWriter = class(TCustomTransientWriter)
  private
    NUZGAG: integer;
    IUZFOPT: integer;
    IRUNFLG: integer;
    FEtDemand: TList;
    FEExtinctionDepths: TList;
    FExtinctionWaterContent : TList;
    procedure CountGages;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSet4;
    procedure WriteDataSet5;
    procedure WriteDataSet6;
    procedure WriteDataSet7;
    procedure WriteDataSet8(var GageStart: integer);
    procedure WriteStressPeriods; reintroduce;
    procedure WriteInfiltrationRates(CellList: TList);
    procedure WritePotentialEtRates(CellList: TList);
    procedure WriteExtinctionDepth(CellList: TList);
    procedure WriteExtinctionWaterContent(CellList: TList);
    procedure WriteGagesToNameFile(const AFileName: string; GageStart: integer);
  protected
    procedure Evaluate; override;
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
  public
    Constructor Create(Model: TPhastModel); override;
    // @name destroys the current instance of @classname.
    Destructor Destroy; override;
    procedure WriteFile(const AFileName: string; var GageStart: integer);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
  end;

implementation

uses ModflowUnitNumbers, DataSetUnit, ModflowUzfUnit, frmErrorsAndWarningsUnit, 
  frmProgressUnit, ModflowCellUnit;

{ TModflowUzfWriter }

constructor TModflowUzfWriter.Create(Model: TPhastModel);
begin
  inherited;
  FEtDemand := TObjectList.Create;
  FEExtinctionDepths := TObjectList.Create;
  FExtinctionWaterContent := TObjectList.Create;
end;

destructor TModflowUzfWriter.Destroy;
begin
  FEtDemand.Free;
  FEExtinctionDepths.Free;
  FExtinctionWaterContent.Free;
  inherited;
end;

procedure TModflowUzfWriter.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TUzfBoundary;
  NoAssignmentErrorRoot: string;
begin
  NoAssignmentErrorRoot := 'No boundary conditions assigned to the '
    + Package.PackageIdentifier
    + ' because the object does not '
    + 'set the values of either enclosed or intersected cells.';
  frmProgress.AddMessage('Evaluating UZF Package data.');
  CountGages;

  for ScreenObjectIndex := 0 to PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := PhastModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowUzfBoundary;
    if Boundary <> nil then
    begin
      frmProgress.AddMessage('    Evaluating ' + ScreenObject.Name + '.');
      if not ScreenObject.SetValuesOfEnclosedCells
        and not ScreenObject.SetValuesOfIntersectedCells then
      begin
        frmErrorsAndWarnings.AddError(NoAssignmentErrorRoot, ScreenObject.Name);
      end;
      Boundary.GetCellValues(Values, nil);
      if PhastModel.ModflowPackages.UzfPackage.SimulateET then
      begin
        Boundary.GetEvapotranspirationDemandCells(FEtDemand);
        Boundary.GetExtinctionDepthCells(FEExtinctionDepths);
        Boundary.GetWaterContentCells(FExtinctionWaterContent);
      end;
    end;
  end;
end;

class function TModflowUzfWriter.Extension: string;
begin
  result := '.uzf';
end;

function TModflowUzfWriter.Package: TModflowPackageSelection;
begin
  result := PhastModel.ModflowPackages.UzfPackage;
end;

procedure TModflowUzfWriter.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  TimeIndex: Integer;
  Infiltration: TModflowBoundaryDisplayTimeList;
  EtDemand: TModflowBoundaryDisplayTimeList;
  ExtinctionDepth: TModflowBoundaryDisplayTimeList;
  WaterContent: TModflowBoundaryDisplayTimeList;
  InfiltrationArray: TModflowBoundaryDisplayDataArray;
  EtDemandArray: TModflowBoundaryDisplayDataArray;
  ExtinctionDepthArray: TModflowBoundaryDisplayDataArray;
  WaterContentArray: TModflowBoundaryDisplayDataArray;
  CellList: TValueCellList;
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;
  Evaluate;
  if not frmProgress.ShouldContinue then
  begin
    Exit;
  end;
  if Values.Count = 0 then
  begin
    SetTimeListsUpToDate(TimeLists);
    Exit;
  end;
  Infiltration := TimeLists[0];
  if PhastModel.ModflowPackages.UzfPackage.SimulateET then
  begin
    EtDemand := TimeLists[1];
    ExtinctionDepth := TimeLists[2];
    WaterContent := TimeLists[3];
  end
  else
  begin
    EtDemand := nil;
    ExtinctionDepth := nil;
    WaterContent := nil;
  end;
  for TimeIndex := 0 to Values.Count - 1 do
  begin
    InfiltrationArray := Infiltration[TimeIndex]
      as TModflowBoundaryDisplayDataArray;
    CellList := Values[TimeIndex];
    CellList.CheckRestore;
    AssignTransient2DArray(InfiltrationArray, 0, CellList, 0,
      rdtDouble, umAssign);
    CellList.Cache;

    if PhastModel.ModflowPackages.UzfPackage.SimulateET then
    begin
      EtDemandArray := EtDemand[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      CellList := FEtDemand[TimeIndex];
      CellList.CheckRestore;
      AssignTransient2DArray(EtDemandArray, 0, CellList, 0,
        rdtDouble, umAssign);
      CellList.Cache;

      ExtinctionDepthArray := ExtinctionDepth[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      CellList := FEExtinctionDepths[TimeIndex];
      CellList.CheckRestore;
      AssignTransient2DArray(ExtinctionDepthArray, 0, CellList, 0,
        rdtDouble, umAssign);
      CellList.Cache;

      WaterContentArray := WaterContent[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      CellList := FExtinctionWaterContent[TimeIndex];
      CellList.CheckRestore;
      AssignTransient2DArray(WaterContentArray, 0, CellList, 0,
        rdtDouble, umAssign);
      CellList.Cache;
    end;
  end;
  SetTimeListsUpToDate(TimeLists);

end;

procedure TModflowUzfWriter.WriteGagesToNameFile(const AFileName: string;
  GageStart: integer);
var
  FileRoot: string;
  Index: Integer;
begin
  FileRoot := ChangeFileExt(AFileName, '');
  for Index := 0 to NUZGAG - 1 do
  begin
    WriteToNameFile(StrDATA, GageStart + Index, FileRoot
      + IntToStr(Index + 1) + '.uzfg', foOutput);
  end;
end;

procedure TModflowUzfWriter.WritePotentialEtRates(CellList: TList);
var
  DefaultValue: double;
  DataType: TRbwDataType;
  DataTypeIndex: integer;
  Comment: string;
  Dummy: TDataArray;
begin
  DefaultValue := 0;
  DataType := rdtDouble;
  DataTypeIndex := 0;
  Comment := '# Data Set 12: PET';
  WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
    CellList, Dummy);
end;

procedure TModflowUzfWriter.WriteExtinctionDepth(CellList: TList);
var
  DefaultValue: double;
  DataType: TRbwDataType;
  DataTypeIndex: integer;
  Comment: string;
  Dummy: TDataArray;
begin
  DefaultValue := 0;
  DataType := rdtDouble;
  DataTypeIndex := 0;
  Comment := '# Data Set 14: EXTDP';
  WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
    CellList, Dummy);
end;

procedure TModflowUzfWriter.WriteExtinctionWaterContent(CellList: TList);
var
  DefaultValue: double;
  DataType: TRbwDataType;
  DataTypeIndex: integer;
  Comment: string;
  Dummy: TDataArray;
begin
  DefaultValue := 0;
  DataType := rdtDouble;
  DataTypeIndex := 0;
  Comment := '# Data Set 16: EXTWC';
  WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
    CellList, Dummy);
end;

procedure TModflowUzfWriter.WriteDataSet1;
var
  NUZTOP: integer;
  IETFLG: integer;
  IUZFCB1: integer;
  IUZFCB2: integer;
  NTRAIL2: integer;
  NSETS2: integer;
  SURFDEP: double;
begin
  NUZTOP := 0;
  case PhastModel.ModflowPackages.UzfPackage.LayerOption of
    loTop:
      begin
        NUZTOP := 1;
      end;
    loSpecified:
      begin
        NUZTOP := 2;
      end;
    loTopActive:
      begin
        NUZTOP := 3;
      end;
    else
      Assert(False);
  end;
  IUZFOPT := PhastModel.ModflowPackages.UzfPackage.VerticalKSource;
  if not PhastModel.ModflowPackages.LpfPackage.IsSelected then
  begin
    IUZFOPT := 1;
  end;
  if PhastModel.ModflowPackages.UzfPackage.RouteDischargeToStreams
    and (PhastModel.ModflowPackages.SfrPackage.IsSelected
    or PhastModel.ModflowPackages.LakPackage.IsSelected) then
  begin
    IRUNFLG := 1;
  end
  else
  begin
    IRUNFLG := 0;
  end;
  if PhastModel.ModflowPackages.UzfPackage.SimulateET then
  begin
    IETFLG := 1;
  end
  else
  begin
    IETFLG := 0;
  end;
  if PhastModel.ModflowOutputControl.Compact then
  begin
    IUZFCB1 := 0;
    IUZFCB2 := 0;
    GetFlowUnitNumber(IUZFCB2);
  end
  else
  begin
    IUZFCB2 := 0;
    IUZFCB1 := 0;
    GetFlowUnitNumber(IUZFCB1);
  end;
  NTRAIL2 := PhastModel.ModflowPackages.UzfPackage.NumberOfTrailingWaves;
  NSETS2 := PhastModel.ModflowPackages.UzfPackage.NumberOfWaveSets;
  SURFDEP := PhastModel.ModflowPackages.UzfPackage.DepthOfUndulations;


  WriteInteger(NUZTOP);
  WriteInteger(IUZFOPT);
  WriteInteger(IRUNFLG);
  WriteInteger(IETFLG);
  WriteInteger(IUZFCB1);
  WriteInteger(IUZFCB2);
  if IUZFOPT > 0 then
  begin
    WriteInteger(NTRAIL2);
    WriteInteger(NSETS2);
  end;
  WriteInteger(NUZGAG);
  WriteFloat(SURFDEP);
  WriteString(' # Data Set 1: NUZTOP IUZFOPT IRUNFLG IETFLG IUZFCB1 IUZFCB2');
  if IUZFOPT > 0 then
  begin
    WriteString(' NTRAIL2 NSETS2');
  end;
  WriteString(' NUZGAG SURFDEP');
  NewLine;
end;

procedure TModflowUzfWriter.WriteDataSet2;
var
  IUZFBND: TDataArray;
begin
  IUZFBND := PhastModel.GetDataSetByName(StrUzfLayer);
  WriteArray(IUZFBND, 0, 'Data Set 2: IUZFBND');
end;

procedure TModflowUzfWriter.WriteDataSet3;
var
  IRUNBND: TDataArray;
begin
  if IRUNFLG > 0 then
  begin
    IRUNBND := PhastModel.GetDataSetByName(StrUzfDischargeRouting);
    WriteArray(IRUNBND, 0, 'Data Set 3: IRUNBND');
  end;
end;

procedure TModflowUzfWriter.WriteDataSet4;
var
  VKS: TDataArray;
begin
  if IUZFOPT = 1 then
  begin
    VKS := PhastModel.GetDataSetByName(StrUzfVerticalK);
    WriteArray(VKS, 0, 'Data Set 4: VKS');
  end;
end;

procedure TModflowUzfWriter.WriteDataSet5;
var
  EPS: TDataArray;
begin
  EPS := PhastModel.GetDataSetByName(StrUzfBrooksCoreyEpsilon);
  WriteArray(EPS, 0, 'Data Set 5: EPS');
end;

procedure TModflowUzfWriter.WriteDataSet6;
var
  THTS: TDataArray;
begin
  THTS := PhastModel.GetDataSetByName(StrUzfSaturatedWaterContent);
  WriteArray(THTS, 0, 'Data Set 6: THTS');
end;

procedure TModflowUzfWriter.WriteDataSet7;
var
  THTI: TDataArray;
begin
  if PhastModel.ModflowStressPeriods.CompletelyTransient then
  begin
    THTI := PhastModel.GetDataSetByName(StrUzfInitialUnsaturatedWaterContent);
    WriteArray(THTI, 0, 'Data Set 7: THTI');
  end;
end;

procedure TModflowUzfWriter.WriteDataSet8(var GageStart: integer);
var
  GageArray: TDataArray;
  ColIndex: integer;
  RowIndex: integer;
begin
  if NUZGAG > 0 then
  begin
    if PhastModel.ModflowPackages.UzfPackage.PrintSummary <> 0 then
    begin
      WriteInteger(-GageStart);
      Inc(GageStart);
      WriteString(' # Data Set 8: IFTUNIT');
      NewLine;
    end;
  end;

  GageArray := PhastModel.GetDataSetByName(StrUzfGage_1_and_2);
  GageArray.Initialize;
  for RowIndex := 0 to PhastModel.ModflowGrid.RowCount -1 do
  begin
    for ColIndex := 0 to PhastModel.ModflowGrid.ColumnCount -1 do
    begin
      if GageArray.IntegerData[0,RowIndex,ColIndex] <> 0 then
      begin
        WriteInteger(RowIndex+1);
        WriteInteger(ColIndex+1);
        WriteInteger(GageStart);
        Inc(GageStart);
        WriteInteger(GageArray.IntegerData[0,RowIndex,ColIndex]);
        WriteString(' # Data Set 8: IUZROW IUZCOL IFTUNIT IUZOPT');
        NewLine;
      end;
    end;
  end;

  GageArray := PhastModel.GetDataSetByName(StrUzfGage3);
  GageArray.Initialize;
  for RowIndex := 0 to PhastModel.ModflowGrid.RowCount -1 do
  begin
    for ColIndex := 0 to PhastModel.ModflowGrid.ColumnCount -1 do
    begin
      if GageArray.IntegerData[0,RowIndex,ColIndex] <> 0 then
      begin
        WriteInteger(RowIndex+1);
        WriteInteger(ColIndex+1);
        WriteInteger(GageStart);
        Inc(GageStart);
        WriteInteger(GageArray.IntegerData[0,RowIndex,ColIndex]);
        WriteString(' # Data Set 8: IUZROW IUZCOL IFTUNIT IUZOPT');
        NewLine;
      end;
    end;
  end;
end;

procedure TModflowUzfWriter.WriteFile(const AFileName: string;
  var GageStart: integer);
var
  NameOfFile: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if PhastModel.PackageGeneratedExternally(StrUZF) then
  begin
    Exit;
  end;
  frmProgress.AddMessage('Writing UZF Package input.');
//  frmProgress.AddMessage('  Evaluating data.');
  Evaluate;
  if not frmProgress.ShouldContinue then
  begin
    Exit;
  end;

  NameOfFile := FileName(AFileName);
  WriteToNameFile(StrUZF, PhastModel.UnitNumbers.UnitNumber(StrUZF), NameOfFile, foInput);
  WriteGagesToNameFile(AFileName, GageStart);
  OpenFile(NameOfFile);
  try
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
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('  Writing Data Set 8.');
    WriteDataSet8(GageStart);
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('  Writing Data Sets 9 to 16.');
    WriteStressPeriods;
  finally
    CloseFile;
  end;
end;

procedure TModflowUzfWriter.WriteInfiltrationRates(CellList: TList);
var
  DefaultValue: double;
  DataType: TRbwDataType;
  DataTypeIndex: integer;
  Comment: string;
  Dummy: TDataArray;
begin
  DefaultValue := 0;
  DataType := rdtDouble;
  DataTypeIndex := 0;
  Comment := '# Data Set 10: FINF';
  WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
    CellList, Dummy);
end;


procedure TModflowUzfWriter.WriteStressPeriods;
var
  TimeIndex: Integer;
  CellList: TList;
begin
  if Values.Count = 0 then
  begin
    frmErrorsAndWarnings.AddError('Unspecified UZF data',
      'No transient data (infiltration and/or evapotranspiration) '
      + 'has been defined.');
  end;
  for TimeIndex := 0 to Values.Count - 1 do
  begin
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;
    frmProgress.AddMessage('    Writing Stress Period ' + IntToStr(TimeIndex+1));

    // data set 9
    WriteInteger(0);
    WriteString(' # Data Set 9, Stress Period ');
    WriteInteger(TimeIndex + 1);
    WriteString(': NUZF1');
    NewLine;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    // Data Set 10
    CellList := Values[TimeIndex];
    WriteInfiltrationRates(CellList);
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    if (PhastModel as TPhastModel).ModflowPackages.UzfPackage.SimulateET then
    begin
      // data set 11
      WriteInteger(0);
      WriteString(' # Data Set 11, Stress Period ');
      WriteInteger(TimeIndex + 1);
      WriteString(': NUZF2');
      NewLine;
      if not frmProgress.ShouldContinue then
      begin
        Exit;
      end;

      // Data Set 12
      CellList := FEtDemand[TimeIndex];
      WritePotentialEtRates(CellList);
      if not frmProgress.ShouldContinue then
      begin
        Exit;
      end;


      // data set 13
      WriteInteger(0);
      WriteString(' # Data Set 13, Stress Period ');
      WriteInteger(TimeIndex + 1);
      WriteString(': NUZF3');
      NewLine;
      if not frmProgress.ShouldContinue then
      begin
        Exit;
      end;


      // Data Set 14
      CellList := FEExtinctionDepths[TimeIndex];
      WriteExtinctionDepth(CellList);
      if not frmProgress.ShouldContinue then
      begin
        Exit;
      end;


      // data set 15
      WriteInteger(0);
      WriteString(' # Data Set 5, Stress Period ');
      WriteInteger(TimeIndex + 1);
      WriteString(': NUZF4');
      NewLine;
      if not frmProgress.ShouldContinue then
      begin
        Exit;
      end;


      // Data Set 16
      CellList := FExtinctionWaterContent[TimeIndex];
      WriteExtinctionWaterContent(CellList);
      if not frmProgress.ShouldContinue then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TModflowUzfWriter.CountGages;
var
  RowIndex: Integer;
  GageArray: TDataArray;
  ColIndex: Integer;
begin
  NUZGAG := 0;

  GageArray := PhastModel.GetDataSetByName(StrUzfGage_1_and_2);
  GageArray.Initialize;
  for ColIndex := 0 to PhastModel.ModflowGrid.ColumnCount - 1 do
  begin
    for RowIndex := 0 to PhastModel.ModflowGrid.RowCount - 1 do
    begin
      if GageArray.IntegerData[0, RowIndex, ColIndex] <> 0 then
      begin
        Inc(NUZGAG);
      end;
    end;
  end;

  GageArray := PhastModel.GetDataSetByName(StrUzfGage3);
  GageArray.Initialize;
  for ColIndex := 0 to PhastModel.ModflowGrid.ColumnCount - 1 do
  begin
    for RowIndex := 0 to PhastModel.ModflowGrid.RowCount - 1 do
    begin
      if GageArray.IntegerData[0, RowIndex, ColIndex] <> 0 then
      begin
        Inc(NUZGAG);
      end;
    end;
  end;

  if PhastModel.ModflowPackages.UzfPackage.PrintSummary <> 0 then
  begin
    Inc(NUZGAG);
  end;
end;

end.
