unit ModflowLAK_Writer;

interface

uses SysUtils, Classes, PhastModelUnit, CustomModflowWriterUnit,
  ModflowPackageSelectionUnit, ModflowTimeUnit;

type
  TModflowLAK_Writer = class(TCustomPackageWriter)
  private
    // @name contains all the @link(TScreenObject)s that define lakes.
    FLakeList: TList;
    FNameOfFile: string;
    procedure Evaluate;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSets4To9;
    procedure WriteLakeDefinitions;
    procedure WriteDataSet5;
    procedure WriteDataSet6;
    procedure WriteDataSets7And8;
    procedure WriteDataSet9(StressPeriod: TModflowStressPeriod);
    procedure WriteGages(var StartUnitNumber: integer; Lines: TStrings);
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    Constructor Create(Model: TPhastModel); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string;
      var StartUnitNumber: integer; Lines: TStrings);
  end;

implementation

uses ModflowUnitNumbers, ScreenObjectUnit, frmErrorsAndWarningsUnit,
  ModflowLakUnit, DataSetUnit, frmProgressUnit;

{ TModflowLAK_Writer }

constructor TModflowLAK_Writer.Create(Model: TPhastModel);
begin
  inherited;
  FLakeList := TList.Create;
end;

destructor TModflowLAK_Writer.Destroy;
begin
  FLakeList.Free;
  inherited;
end;

function SortLakes(Item1, Item2: Pointer): Integer;
var
  Lake1, Lake2: TScreenObject;
begin
  Lake1 := Item1;
  Lake2 := Item2;
  Assert(Lake1.ModflowLakBoundary <> nil);
  Assert(Lake2.ModflowLakBoundary <> nil);
  result := Lake1.ModflowLakBoundary.LakeID - Lake2.ModflowLakBoundary.LakeID;
end;

procedure TModflowLAK_Writer.Evaluate;
const
  DupNameErrorMessage = 'The following Lakes have the same Lake ID.';
  InvalidCenterLake = 'The follow lakes have invalid center lake numbers.';
var
  ScreenObjectIndex: Integer;
  ScreenObject, OtherObject: TScreenObject;
  TempList: TList;
  SubLakeIndex: Integer;
begin
  frmProgress.AddMessage('Evaluating LAK Package data.');
  TempList := TList.Create;
  try
    for ScreenObjectIndex := 0 to PhastModel.ScreenObjectCount - 1 do
    begin
      ScreenObject := PhastModel.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      if (ScreenObject.ModflowLakBoundary <> nil)
        and ScreenObject.ModflowLakBoundary.Used
        and (ScreenObject.ModflowLakBoundary.LakeID > 0) then
      begin
        While (FLakeList.Count <= ScreenObject.ModflowLakBoundary.LakeID) do
        begin
          FLakeList.Add(nil);
        end;
        if FLakeList[ScreenObject.ModflowLakBoundary.LakeID] <> nil then
        begin
          OtherObject := FLakeList[ScreenObject.ModflowLakBoundary.LakeID];
          frmErrorsAndWarnings.AddError(DupNameErrorMessage, OtherObject.Name
            + ' and ' + ScreenObject.Name);
        end;
        FLakeList[ScreenObject.ModflowLakBoundary.LakeID]
          := ScreenObject;
        TempList.Add(ScreenObject);
        ScreenObject.ModflowLakBoundary.ClearSubLakes;
      end;
    end;
    for ScreenObjectIndex := 0 to TempList.Count - 1 do
    begin
      ScreenObject := TempList[ScreenObjectIndex];
      Assert(ScreenObject.ModflowLakBoundary <> nil);
      ScreenObject.ModflowLakBoundary.TrueLakeID := -1;
      if ScreenObject.ModflowLakBoundary.CenterLake < FLakeList.Count then
      begin
        if ScreenObject.ModflowLakBoundary.CenterLake <> 0 then
        begin
          OtherObject := FLakeList[ScreenObject.ModflowLakBoundary.CenterLake];
          if OtherObject = nil then
          begin
            frmErrorsAndWarnings.AddError(InvalidCenterLake, ScreenObject.Name)
          end
          else
          begin
            OtherObject.ModflowLakBoundary.AddSubLake(
              ScreenObject);
          end;
        end;
      end
      else
      begin
        frmErrorsAndWarnings.AddError(InvalidCenterLake, ScreenObject.Name)
      end;
    end;
    FLakeList.Pack;
    FLakeList.Sort(SortLakes);
    for ScreenObjectIndex := 0 to FLakeList.Count - 1 do
    begin
      ScreenObject := FLakeList[ScreenObjectIndex];
      Assert(ScreenObject.ModflowLakBoundary <> nil);
      ScreenObject.ModflowLakBoundary.TrueLakeID := ScreenObjectIndex + 1;
    end;
    for ScreenObjectIndex := 0 to FLakeList.Count - 1 do
    begin
      ScreenObject := FLakeList[ScreenObjectIndex];
      Assert(ScreenObject.ModflowLakBoundary <> nil);
      for SubLakeIndex :=
        ScreenObject.ModflowLakBoundary.SubLakeCount - 1 downto 0 do
      begin
        OtherObject := ScreenObject.ModflowLakBoundary.
          SubLakes[SubLakeIndex] as TScreenObject;
        if OtherObject.ModflowLakBoundary.TrueLakeID < 0 then
        begin
          ScreenObject.ModflowLakBoundary.DeleteSubLake(SubLakeIndex);
        end;
      end;
    end;
  finally
    TempList.Free;
  end;
end;

class function TModflowLAK_Writer.Extension: string;
begin
  result := '.lak';
end;

function TModflowLAK_Writer.Package: TModflowPackageSelection;
begin
  result := PhastModel.ModflowPackages.LakPackage;
end;

procedure TModflowLAK_Writer.WriteDataSet1;
var
  NLAKES: integer;
  ILKCB: integer;
begin
  NLAKES := FLakeList.Count;
  GetFlowUnitNumber(ILKCB);

  WriteInteger(NLAKES);
  WriteInteger(ILKCB);
  WriteString(' # DataSet 1: NLAKES ILKCB');
  NewLine;
end;

procedure TModflowLAK_Writer.WriteDataSet2;
var
  THETA: double;
  NSSITR: integer;
  SSCNCR: double;
  LakePkg: TLakePackageSelection;
  SURFDEPTH: double;
begin
  LakePkg := PhastModel.ModflowPackages.LakPackage;
  THETA := LakePkg.Theta;
//  if PhastModel.ModflowFullStressPeriods.CompletelyTransient then
//  begin
    // Make Theta negative to force NSSITR and SSCNCR to be read.
    // Make Theta negative even for steady state models to force SURFDEPTH to be read
    THETA := -THETA;
//  end;
  NSSITR := LakePkg.NumberOfIterations;
  SSCNCR := LakePkg.ConvergenceCriterion;
  SURFDEPTH := LakePkg.SurfDepth.Value;

  WriteFloat(THETA);
  WriteInteger(NSSITR);
  WriteFloat(SSCNCR);
  WriteFloat(SURFDEPTH);
  WriteString(' # DataSet 2: THETA NSSITR SSCNCR SURFDEPTH');
  NewLine;
end;

procedure TModflowLAK_Writer.WriteDataSet3;
var
  STAGES, SSMN, SSMX: double;
  FirstPeriodIsSteadyState: boolean;
  LakeIndex: Integer;
  ScreenObject: TScreenObject;
  Lake: TLakBoundary;
  LakeTime: TLakItem;
begin
  FirstPeriodIsSteadyState :=
    PhastModel.ModflowFullStressPeriods[0].StressPeriodType = sptSteadyState;
  for LakeIndex := 0 to FLakeList.Count - 1 do
  begin
    ScreenObject := FLakeList[LakeIndex];
    Assert(ScreenObject.ModflowLakBoundary <> nil);
    Lake := ScreenObject.ModflowLakBoundary;
    STAGES := Lake.InitialStage;
    WriteFloat(STAGES);
    if FirstPeriodIsSteadyState then
    begin
      LakeTime := Lake.Values[0] as TLakItem;
      SSMN := LakeTime.SSMN;
      SSMX := LakeTime.SSMX;

      WriteFloat(SSMN);
      WriteFloat(SSMX);
      WriteString(' # DataSet 3: STAGES SSMN SSMX');
    end
    else
    begin
      WriteString(' # DataSet 3: STAGES');
    end;
    NewLine;
  end;
end;

procedure TModflowLAK_Writer.WriteDataSet5;
var
  LakeID: TDataArray;
  LayerIndex: integer;
  ModflowLayer: integer;
begin
  LakeID := PhastModel.GetDataSetByName(rsLakeID);
  ModflowLayer := 0;
  for LayerIndex := 0 to PhastModel.ModflowGrid.LayerCount - 1 do
  begin
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;
    if PhastModel.LayerStructure.IsLayerSimulated(LayerIndex) then
    begin
      Inc(ModflowLayer);
      WriteArray(LakeID, LayerIndex, 'Data Set 5, LKARR: Layer '
        + IntToStr(ModflowLayer));
    end;
  end;
end;

procedure TModflowLAK_Writer.WriteDataSet6;
var
  LakeLeakance: TDataArray;
  LayerIndex: integer;
  ModflowLayer: integer;
begin
  LakeLeakance := PhastModel.GetDataSetByName(rsLakeLeakance);
  ModflowLayer := 0;
  for LayerIndex := 0 to PhastModel.ModflowGrid.LayerCount - 1 do
  begin
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;
    if PhastModel.LayerStructure.IsLayerSimulated(LayerIndex) then
    begin
      Inc(ModflowLayer);
      WriteArray(LakeLeakance, LayerIndex, 'Data Set 6, BDLKNC: Layer ' + IntToStr(ModflowLayer));
    end;
  end;
end;

procedure TModflowLAK_Writer.WriteDataSet9(StressPeriod: TModflowStressPeriod);
var
  LakeIndex: Integer;
  ScreenObject: TScreenObject;
  Lake: TLakBoundary;
  TimeIndex: Integer;
  LakeItem: TLakItem;
  PRCPLK, EVAPLK, RNF, WTHDRW, SSMN, SSMX: double;
begin
  for LakeIndex := 0 to FLakeList.Count - 1 do
  begin
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;
    ScreenObject := FLakeList[LakeIndex];
    Assert(ScreenObject.ModflowLakBoundary <> nil);
    Lake := ScreenObject.ModflowLakBoundary;
    for TimeIndex := 0 to Lake.Values.Count -1 do
    begin
      if not frmProgress.ShouldContinue then
      begin
        Exit;
      end;
      LakeItem := Lake.Values[TimeIndex] as TLakItem;
      if (LakeItem.StartTime <= StressPeriod.StartTime)
        and (LakeItem.EndTime > StressPeriod.StartTime) then
      begin
        PRCPLK := LakeItem.PRCPLK;
        EVAPLK := LakeItem.EVAPLK;
        RNF := LakeItem.RNF;
        WTHDRW := LakeItem.WTHDRW;

        WriteFloat(PRCPLK);
        WriteFloat(EVAPLK);
        WriteFloat(RNF);
        WriteFloat(WTHDRW);

        if StressPeriod.StressPeriodType = sptSteadyState then
        begin
          SSMN := LakeItem.SSMN;
          SSMX := LakeItem.SSMX;
          WriteFloat(SSMN);
          WriteFloat(SSMX);
        end;
        WriteString(' # DataSet 9: PRCPLK EVAPLK RNF WTHDRW');
        if StressPeriod.StressPeriodType = sptSteadyState then
        begin
          WriteString(' SSMN SSMX');
        end;
        NewLine;
        break;
      end;
    end;
  end;
end;

procedure TModflowLAK_Writer.WriteDataSets4To9;
var
  TimeIndex: integer;
  StressPeriod: TModflowStressPeriod;
  ITMP, ITMP1, LWRT: integer;
begin
  ITMP1 := 1;
  if PhastModel.ModflowPackages.LakPackage.PrintLakes then
  begin
    LWRT := 0;
  end
  else
  begin
    LWRT := 1;
  end;
  for TimeIndex := 0 to PhastModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;
    // data set 4;
    if TimeIndex = 0 then
    begin
      ITMP := 1;
    end
    else
    begin
      ITMP := -1;
    end;
    WriteInteger(ITMP);
    WriteInteger(ITMP1);
    WriteInteger(LWRT);
    WriteString(' # DataSet 4: ITMP ITMP1 LWRT');
    NewLine;

    if TimeIndex = 0 then
    begin
      WriteLakeDefinitions;
    end;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    StressPeriod := PhastModel.ModflowFullStressPeriods[TimeIndex];
    WriteDataSet9(StressPeriod);
  end;
end;

procedure TModflowLAK_Writer.WriteDataSets7And8;
var
  LakeIndex: integer;
  ScreenObject, SubScreenObject: TScreenObject;
  Lake, SubLake: TLakBoundary;
  SubLakeIndex: Integer;
  IC: integer;
  NSLMS: integer;
begin
  // data set 7
  NSLMS := 0;
  for LakeIndex := 0 to FLakeList.Count - 1 do
  begin
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;
    ScreenObject := FLakeList[LakeIndex];
    Lake := ScreenObject.ModflowLakBoundary;
    Assert(ScreenObject.ModflowLakBoundary <> nil);
    if Lake.SubLakeCount > 0 then
    begin
      Inc(NSLMS);
    end;
  end;
  WriteInteger(NSLMS);
  WriteString(' # DataSet 7: NSLMS');
  NewLine;

  if NSLMS > 0 then
  begin
    for LakeIndex := 0 to FLakeList.Count - 1 do
    begin
      if not frmProgress.ShouldContinue then
      begin
        Exit;
      end;
      ScreenObject := FLakeList[LakeIndex];
      Assert(ScreenObject.ModflowLakBoundary <> nil);
      Lake := ScreenObject.ModflowLakBoundary;
      if Lake.SubLakeCount > 0 then
      begin
        // Data Set 8a
        IC := Lake.SubLakeCount+1;
        WriteInteger(IC);
        WriteInteger(Lake.TrueLakeID);
        for SubLakeIndex := 0 to Lake.SubLakeCount - 1 do
        begin
          SubScreenObject := Lake.SubLakes[SubLakeIndex] as TScreenObject;
          Assert(SubScreenObject.ModflowLakBoundary <> nil);
          SubLake := SubScreenObject.ModflowLakBoundary;
          WriteInteger(SubLake.TrueLakeID);
        end;
        WriteString(' # DataSet 8a: IC ISUB(1) ISUB(2) ............ ISUB(IC)');
        NewLine;

        // Data Set 8b
        for SubLakeIndex := 0 to Lake.SubLakeCount - 1 do
        begin
          SubScreenObject := Lake.SubLakes[SubLakeIndex] as TScreenObject;
          Assert(SubScreenObject.ModflowLakBoundary <> nil);
          SubLake := SubScreenObject.ModflowLakBoundary;
          WriteFloat(SubLake.Sill);
        end;
        WriteString(' # DataSet 8b: SILLVT(2) ............. SILLVT(IC)');
        NewLine;
      end;
    end;
  end;
end;

procedure TModflowLAK_Writer.WriteFile(const AFileName: string;
  var StartUnitNumber: integer; Lines: TStrings);
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if PhastModel.PackageGeneratedExternally(StrLAK) then
  begin
    Exit;
  end;
  FNameOfFile := FileName(AFileName);
  WriteToNameFile(StrLAK, PhastModel.UnitNumbers.UnitNumber(StrLAK), FNameOfFile, foInput);
  Evaluate;
  OpenFile(FileName(AFileName));
  try
    frmProgress.AddMessage('Writing LAK Package input.');
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

    frmProgress.AddMessage('  Writing Data Sets 4 to 9.');
    WriteDataSets4To9;
  finally
    CloseFile;
  end;
  WriteGages(StartUnitNumber, Lines);
end;

procedure TModflowLAK_Writer.WriteGages(var StartUnitNumber: integer;
  Lines: TStrings);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  LAKE: integer;
  UNIT_Number: integer;
  OUTTYPE: integer;
  Line: string;
  OutputName: string;
  procedure WriteGage;
  begin
    LAKE := -ScreenObject.ModflowLakBoundary.TrueLakeID;
    UNIT_Number := StartUnitNumber;
    Line := IntToStr(LAKE) + ' '
      + IntToStr(UNIT_Number) + ' '
      + IntToStr(OUTTYPE);
    Lines.Add(Line);
    Inc(StartUnitNumber);
    OutputName := ChangeFileExt(FNameOfFile, '.lakg');
    OutputName := OutputName + IntToStr(Lines.Count);
    WriteToNameFile(StrDATA, UNIT_Number, OutputName, foOutput);
  end;
begin
  for ScreenObjectIndex := 0 to FLakeList.Count - 1 do
  begin
    ScreenObject := FLakeList[ScreenObjectIndex];
    Assert((ScreenObject.ModflowLakBoundary <> nil)
      and ScreenObject.ModflowLakBoundary.Used);
    OUTTYPE := ScreenObject.ModflowLakBoundary.OutType;
    if OUTTYPE >= 0 then
    begin
      WriteGage;
    end;
    if ScreenObject.ModflowLakBoundary.Gage4 then
    begin
      OUTTYPE := 4;
      WriteGage;
    end;
  end;
end;

procedure TModflowLAK_Writer.WriteLakeDefinitions;
begin
  WriteDataSet5;
  WriteDataSet6;
  WriteDataSets7And8;
end;

end.