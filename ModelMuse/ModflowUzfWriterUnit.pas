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
    Constructor Create(Model: TCustomModel); override;
    // @name destroys the current instance of @classname.
    Destructor Destroy; override;
    procedure WriteFile(const AFileName: string; var GageStart: integer);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
  end;

implementation

uses ModflowUnitNumbers, DataSetUnit, ModflowUzfUnit, frmErrorsAndWarningsUnit,
  frmProgressUnit, ModflowCellUnit, Forms, GoPhastTypes;

resourcestring
  StrUnspecifiedUZFData = 'Unspecified UZF data';
  StrTheInfiltrationRat = 'The infiltration rate in the UZF package was not ' +
  'defined in the following stress periods.';
  StrTheETDemandRateI = 'The ET demand rate in the UZF package was not defin' +
  'ed in the following stress periods.';
  StrTheETExtinctionDe = 'The ET extinction depth in the UZF package was not' +
  ' defined in the following stress periods.';
  StrTheETExtinctionWa = 'The ET extinction water content in the UZF package' +
  ' was not defined in the following stress periods.';

{ TModflowUzfWriter }

constructor TModflowUzfWriter.Create(Model: TCustomModel);
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
  frmProgressMM.AddMessage('Evaluating UZF Package data.');
  CountGages;

  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    if not ScreenObject.UsedModels.UsesModel(Model) then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowUzfBoundary;
    if Boundary <> nil then
    begin
      frmProgressMM.AddMessage('    Evaluating ' + ScreenObject.Name + '.');
      if not ScreenObject.SetValuesOfEnclosedCells
        and not ScreenObject.SetValuesOfIntersectedCells then
      begin
        frmErrorsAndWarnings.AddError(Model,
          NoAssignmentErrorRoot, ScreenObject.Name);
      end;
      Boundary.GetCellValues(Values, nil, Model);
      if Model.ModflowPackages.UzfPackage.SimulateET then
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
  result := Model.ModflowPackages.UzfPackage;
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
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  if Values.Count = 0 then
  begin
    SetTimeListsUpToDate(TimeLists);
    Exit;
  end;

  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrUnspecifiedUZFData);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheInfiltrationRat);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheETDemandRateI);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheETExtinctionDe);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheETExtinctionWa);

  Infiltration := TimeLists[0];
  if Model.ModflowPackages.UzfPackage.SimulateET then
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
      rdtDouble, Model.ModflowPackages.UzfPackage.AssignmentMethod);
    Model.AdjustDataArray(InfiltrationArray);
    CellList.Cache;

    if Model.ModflowPackages.UzfPackage.SimulateET then
    begin
      EtDemandArray := EtDemand[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      if TimeIndex < FEtDemand.Count then
      begin
        CellList := FEtDemand[TimeIndex];
        CellList.CheckRestore;
        AssignTransient2DArray(EtDemandArray, 0, CellList, 0,
          rdtDouble, umAssign);
        Model.AdjustDataArray(EtDemandArray);
        CellList.Cache;
      end
      else
      begin
        frmErrorsAndWarnings.AddError(Model,
          StrTheETDemandRateI, IntToStr(TimeIndex+1));
      end;

      ExtinctionDepthArray := ExtinctionDepth[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      if TimeIndex < FEExtinctionDepths.Count then
      begin
        CellList := FEExtinctionDepths[TimeIndex];
        CellList.CheckRestore;
        AssignTransient2DArray(ExtinctionDepthArray, 0, CellList, 0,
          rdtDouble, umAssign);
        CellList.Cache;
      end
      else
      begin
        frmErrorsAndWarnings.AddError(Model,
          StrTheETExtinctionDe, IntToStr(TimeIndex+1));
      end;


      WaterContentArray := WaterContent[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      if TimeIndex <  FExtinctionWaterContent.Count then
      begin
        CellList := FExtinctionWaterContent[TimeIndex];
        CellList.CheckRestore;
        AssignTransient2DArray(WaterContentArray, 0, CellList, 0,
          rdtDouble, umAssign);
        CellList.Cache;
      end
      else
      begin
        frmErrorsAndWarnings.AddError(Model,
          StrTheETExtinctionWa, IntToStr(TimeIndex+1));
      end;
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
    CellList, umAssign, True, Dummy);
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
    CellList, umAssign, False, Dummy);
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
    CellList, umAssign, False, Dummy);
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
  case Model.ModflowPackages.UzfPackage.LayerOption of
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
  IUZFOPT := Model.ModflowPackages.UzfPackage.VerticalKSource;
  if not Model.ModflowPackages.LpfPackage.IsSelected then
  begin
    IUZFOPT := 1;
  end;
  if Model.ModflowPackages.UzfPackage.RouteDischargeToStreams
    and (Model.ModflowPackages.SfrPackage.IsSelected
    or Model.ModflowPackages.LakPackage.IsSelected) then
  begin
    IRUNFLG := 1;
  end
  else
  begin
    IRUNFLG := 0;
  end;
  if Model.ModflowPackages.UzfPackage.SimulateET then
  begin
    IETFLG := 1;
  end
  else
  begin
    IETFLG := 0;
  end;
  if Model.ModflowOutputControl.Compact then
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
  NTRAIL2 := Model.ModflowPackages.UzfPackage.NumberOfTrailingWaves;
  NSETS2 := Model.ModflowPackages.UzfPackage.NumberOfWaveSets;
  SURFDEP := Model.ModflowPackages.UzfPackage.DepthOfUndulations;


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
  IUZFBND := Model.DataArrayManager.GetDataSetByName(StrUzfLayer);
  WriteArray(IUZFBND, 0, 'Data Set 2: IUZFBND');
end;

procedure TModflowUzfWriter.WriteDataSet3;
var
  IRUNBND: TDataArray;
begin
  if IRUNFLG > 0 then
  begin
    IRUNBND := Model.DataArrayManager.GetDataSetByName(StrUzfDischargeRouting);
    WriteArray(IRUNBND, 0, 'Data Set 3: IRUNBND');
  end;
end;

procedure TModflowUzfWriter.WriteDataSet4;
var
  VKS: TDataArray;
begin
  if IUZFOPT = 1 then
  begin
    VKS := Model.DataArrayManager.GetDataSetByName(StrUzfVerticalK);
    WriteArray(VKS, 0, 'Data Set 4: VKS');
  end;
end;

procedure TModflowUzfWriter.WriteDataSet5;
var
  EPS: TDataArray;
begin
  EPS := Model.DataArrayManager.GetDataSetByName(StrUzfBrooksCoreyEpsilon);
  WriteArray(EPS, 0, 'Data Set 5: EPS');
end;

procedure TModflowUzfWriter.WriteDataSet6;
var
  THTS: TDataArray;
begin
  THTS := Model.DataArrayManager.GetDataSetByName(StrUzfSaturatedWaterContent);
  WriteArray(THTS, 0, 'Data Set 6: THTS');
end;

procedure TModflowUzfWriter.WriteDataSet7;
var
  THTI: TDataArray;
begin
  if Model.ModflowStressPeriods.CompletelyTransient then
  begin
    THTI := Model.DataArrayManager.GetDataSetByName(StrUzfInitialUnsaturatedWaterContent);
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
    if Model.ModflowPackages.UzfPackage.PrintSummary <> 0 then
    begin
      WriteInteger(-GageStart);
      Inc(GageStart);
      WriteString(' # Data Set 8: IFTUNIT');
      NewLine;
    end;
  end;

  GageArray := Model.DataArrayManager.GetDataSetByName(StrUzfGage_1_and_2);
  GageArray.Initialize;
  for RowIndex := 0 to Model.ModflowGrid.RowCount -1 do
  begin
    for ColIndex := 0 to Model.ModflowGrid.ColumnCount -1 do
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

  GageArray := Model.DataArrayManager.GetDataSetByName(StrUzfGage3);
  GageArray.Initialize;
  for RowIndex := 0 to Model.ModflowGrid.RowCount -1 do
  begin
    for ColIndex := 0 to Model.ModflowGrid.ColumnCount -1 do
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
  if Model.PackageGeneratedExternally(StrUZF) then
  begin
    Exit;
  end;
  frmProgressMM.AddMessage('Writing UZF Package input.');
//  frmProgress.AddMessage('  Evaluating data.');
  Evaluate;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrUnspecifiedUZFData);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheInfiltrationRat);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheETDemandRateI);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheETExtinctionDe);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheETExtinctionWa);

  NameOfFile := FileName(AFileName);
  WriteToNameFile(StrUZF, Model.UnitNumbers.UnitNumber(StrUZF), NameOfFile, foInput);
  WriteGagesToNameFile(AFileName, GageStart);
  OpenFile(NameOfFile);
  try
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

    frmProgressMM.AddMessage('  Writing Data Set 2.');
    WriteDataSet2;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Set 3.');
    WriteDataSet3;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Set 4.');
    WriteDataSet4;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Set 5.');
    WriteDataSet5;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Set 6.');
    WriteDataSet6;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Set 7.');
    WriteDataSet7;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Set 8.');
    WriteDataSet8(GageStart);
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Sets 9 to 16.');
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
    CellList, Model.ModflowPackages.UzfPackage.AssignmentMethod, True, Dummy);
end;


procedure TModflowUzfWriter.WriteStressPeriods;
var
  TimeIndex: Integer;
  CellList: TList;
begin
  if Values.Count = 0 then
  begin
    frmErrorsAndWarnings.AddError(Model, StrUnspecifiedUZFData,
      'No transient data (infiltration and/or evapotranspiration) '
      + 'has been defined.');
  end;
  for TimeIndex := 0 to Values.Count - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    frmProgressMM.AddMessage('    Writing Stress Period ' + IntToStr(TimeIndex+1));

    // data set 9
    WriteInteger(0);
    WriteString(' # Data Set 9, Stress Period ');
    WriteInteger(TimeIndex + 1);
    WriteString(': NUZF1');
    NewLine;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    // Data Set 10
    if TimeIndex < Values.Count then
    begin
      CellList := Values[TimeIndex];
      WriteInfiltrationRates(CellList);
    end
    else
    begin
      frmErrorsAndWarnings.AddError(Model,
        StrTheInfiltrationRat, IntToStr(TimeIndex+1));
    end;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    if (Model as TCustomModel).ModflowPackages.UzfPackage.SimulateET then
    begin
      // data set 11
      WriteInteger(0);
      WriteString(' # Data Set 11, Stress Period ');
      WriteInteger(TimeIndex + 1);
      WriteString(': NUZF2');
      NewLine;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // Data Set 12
      if TimeIndex < FEtDemand.Count then
      begin
        CellList := FEtDemand[TimeIndex];
        WritePotentialEtRates(CellList);
      end
      else
      begin
        frmErrorsAndWarnings.AddError(Model,
          StrTheETDemandRateI, IntToStr(TimeIndex+1));
      end;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // data set 13
      WriteInteger(0);
      WriteString(' # Data Set 13, Stress Period ');
      WriteInteger(TimeIndex + 1);
      WriteString(': NUZF3');
      NewLine;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // Data Set 14
      if TimeIndex < FEExtinctionDepths.Count then
      begin
        CellList := FEExtinctionDepths[TimeIndex];
        WriteExtinctionDepth(CellList);
      end
      else
      begin
        frmErrorsAndWarnings.AddError(Model,
          StrTheETExtinctionDe, IntToStr(TimeIndex+1));
      end;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // data set 15
      WriteInteger(0);
      WriteString(' # Data Set 5, Stress Period ');
      WriteInteger(TimeIndex + 1);
      WriteString(': NUZF4');
      NewLine;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      // Data Set 16
      if TimeIndex < FExtinctionWaterContent.Count then
      begin
        CellList := FExtinctionWaterContent[TimeIndex];
        WriteExtinctionWaterContent(CellList);
      end
      else
      begin
        frmErrorsAndWarnings.AddError(Model,
          StrTheETExtinctionWa, IntToStr(TimeIndex+1));
      end;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
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

  GageArray := Model.DataArrayManager.GetDataSetByName(StrUzfGage_1_and_2);
  GageArray.Initialize;
  for ColIndex := 0 to Model.ModflowGrid.ColumnCount - 1 do
  begin
    for RowIndex := 0 to Model.ModflowGrid.RowCount - 1 do
    begin
      if GageArray.IntegerData[0, RowIndex, ColIndex] <> 0 then
      begin
        Inc(NUZGAG);
      end;
    end;
  end;

  GageArray := Model.DataArrayManager.GetDataSetByName(StrUzfGage3);
  GageArray.Initialize;
  for ColIndex := 0 to Model.ModflowGrid.ColumnCount - 1 do
  begin
    for RowIndex := 0 to Model.ModflowGrid.RowCount - 1 do
    begin
      if GageArray.IntegerData[0, RowIndex, ColIndex] <> 0 then
      begin
        Inc(NUZGAG);
      end;
    end;
  end;

  if Model.ModflowPackages.UzfPackage.PrintSummary <> 0 then
  begin
    Inc(NUZGAG);
  end;
end;

end.
