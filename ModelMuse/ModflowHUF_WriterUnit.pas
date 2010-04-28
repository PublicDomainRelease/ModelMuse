unit ModflowHUF_WriterUnit;

interface

uses SysUtils, Classes, CustomModflowWriterUnit, PhastModelUnit, 
  ModflowPackageSelectionUnit, HufDefinition;

type
  TModflowHUF_Writer = class(TCustomFlowPackageWriter)
  private
    FHufPackage: THufPackageSelection;
    FNameOfFile: string;
    FTransient: Boolean;
    FConvertibleLayerPresent: Boolean;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSet4;
    procedure WriteDataSet5;
    procedure WriteDataSets6to8;
    procedure WriteDataSet6(HGU: THydrogeologicUnit);
    procedure WriteDataSet7(HGU: THydrogeologicUnit);
    procedure WriteDataSet8(HGU: THydrogeologicUnit);
    procedure WriteDataSet9;
    procedure WriteDataSets10and11;
    procedure WriteDataSet12;
    procedure CheckElevations;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string);
    Constructor Create(Model: TPhastModel); override;
  end;

implementation

uses Math, Contnrs , ModflowUnitNumbers, frmProgressUnit, OrderedCollectionUnit,
  GoPhastTypes, DataSetUnit, frmErrorsAndWarningsUnit, ModflowParameterUnit;

const
  HufSteadyAndTransientParameters = [ptHUF_HK, ptHUF_HANI, ptHUF_VK, ptHUF_VANI, ptHUF_SS, ptHUF_SY];
  HufSteadyParameters = [ptHUF_HK, ptHUF_HANI, ptHUF_VK, ptHUF_VANI];

type
  THguSort = class(TObject)
    HGU: THydrogeologicUnit;
    Top: double;
    Bottom: double;
  end;

function HguSorter(Item1, Item2: pointer): integer;
var
  HguSort1: THguSort;
  HguSort2: THguSort;
begin
  HguSort1 := Item1;
  HguSort2 := Item2;
  result := Sign(HguSort2.Top - HguSort1.Top);
  if result = 0 then
  begin
    result := Sign(HguSort2.Bottom - HguSort1.Bottom);
  end;
end;

{ TModflowHUF_Writer }

procedure TModflowHUF_Writer.CheckElevations;
var
  ActiveDataArray: TDataArray;
  HguList: TList;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  CellUsed: Boolean;
  HguIndex: Integer;
  HGU: THydrogeologicUnit;
  ThickArray: TDataArray;
  Thickness: Double;
  SortItem, SortItem1, SortItem2: THguSort;
  TopArray: TDataArray;
  Delta: Double;
const
  Epsilon = 1e-4;
  GapWarning = 'Gap between hydrogeologic units';
  OverlapWarning = 'Overlap between hydrogeologic units';
  WarningFormat = 'Column: %d; Row: %d; Higher unit: %s; Lower unit: %s';
begin
  ActiveDataArray := PhastModel.GetDataSetByName(rsActive);
  ActiveDataArray.Initialize;

  HguList := TObjectList.Create;
  try
    for ColIndex := 0 to PhastModel.ModflowGrid.ColumnCount - 1 do
    begin
      for RowIndex := 0 to PhastModel.ModflowGrid.RowCount - 1 do
      begin
        CellUsed := False;
        for LayerIndex := 0 to PhastModel.ModflowGrid.LayerCount - 1 do
        begin
          if PhastModel.LayerStructure.IsLayerSimulated(LayerIndex) then
          begin
            if ActiveDataArray.BooleanData[0, RowIndex, ColIndex] then
            begin
              CellUsed := True;
              Break;
            end;
          end;
        end;
        if CellUsed then
        begin
          for HguIndex := 0 to PhastModel.HydrogeologicUnits.Count - 1 do
          begin
            HGU := PhastModel.HydrogeologicUnits[HguIndex];
            ThickArray := PhastModel.GetDataSetByName(HGU.ThickessDataArrayName);
            ThickArray.Initialize;
            PhastModel.AddDataSetToCache(ThickArray);
            Thickness := ThickArray.RealData[0, RowIndex, ColIndex];
            if Thickness > 0 then
            begin
              SortItem := THguSort.Create;
              HguList.Add(SortItem);
              SortItem.HGU := HGU;
              TopArray := PhastModel.GetDataSetByName(HGU.TopDataArrayName);
              TopArray.Initialize;
              PhastModel.AddDataSetToCache(TopArray);
              SortItem.Top := TopArray.RealData[0, RowIndex, ColIndex];
              SortItem.Bottom := SortItem.Top - Thickness;
            end;
          end;
          HguList.Sort(HguSorter);
          for HguIndex := 1 to HguList.Count - 1 do
          begin
            SortItem1 := HguList[HguIndex-1];
            SortItem2 := HguList[HguIndex];
            Delta := SortItem1.Bottom - SortItem2.Top;
            if Delta > Epsilon then
            begin
              frmErrorsAndWarnings.AddWarning(GapWarning,
                Format(WarningFormat, [ColIndex+1, RowIndex+1,
                  SortItem1.HGU.HufName, SortItem2.HGU.HufName]));
            end
            else if Delta < -Epsilon then
            begin
              frmErrorsAndWarnings.AddWarning(OverlapWarning,
                Format(WarningFormat, [ColIndex+1, RowIndex+1,
                  SortItem1.HGU.HufName, SortItem2.HGU.HufName]));
            end;
          end;
          HguList.Clear;
        end;
      end;
    end;
  finally
    HguList.Free;
  end;

  PhastModel.AddDataSetToCache(ActiveDataArray);
  PhastModel.CacheDataArrays;
end;

constructor TModflowHUF_Writer.Create(Model: TPhastModel);
begin
  inherited;
  FHufPackage := Package as THufPackageSelection;
end;

class function TModflowHUF_Writer.Extension: string;
begin
  result := '.huf';
end;

function TModflowHUF_Writer.Package: TModflowPackageSelection;
begin
  result := PhastModel.ModflowPackages.HufPackage;
end;

procedure TModflowHUF_Writer.WriteDataSet1;
var
  IHUFCB: integer;
  HDRY: double;
  NHUF: integer;
  NPHUF: integer;
  IOHUFHEADS: integer;
  IOHUFFLOWS: integer;
  NameOfFile: string;
begin
  IHUFCB := 0;
  GetFlowUnitNumber(IHUFCB);
  HDRY := PhastModel.ModflowOptions.HDry;
  NHUF := PhastModel.HydrogeologicUnits.Count;
  FTransient := PhastModel.ModflowFullStressPeriods.TransientModel;
  if FTransient then
  begin
    NPHUF := PhastModel.HufParameters.CountParameters(HufSteadyAndTransientParameters) +
      PhastModel.ModflowSteadyParameters.CountParameters([ptHUF_SYTP]);
  end
  else
  begin
    NPHUF := PhastModel.HufParameters.CountParameters(HufSteadyParameters);
  end;
  if FHufPackage.SaveHeads then
  begin
    IOHUFHEADS := PhastModel.UnitNumbers.UnitNumber(StrIOHUFHEADS);
    if PhastModel.ModflowOutputControl.HeadOC.FormatDefined then
    begin
      NameOfFile := ChangeFileExt(FNameOfFile, '.huf_fhd');
      WriteToNameFile(StrDATA, IOHUFHEADS,
        NameOfFile, foOutput);
    end
    else
    begin
      NameOfFile := ChangeFileExt(FNameOfFile, '.huf_bhd');
      WriteToNameFile(StrDATABINARY, IOHUFHEADS,
        NameOfFile, foOutput);
    end;
  end
  else
  begin
    IOHUFHEADS := 0;
  end;
  if FHufPackage.SaveFlows then
  begin
    IOHUFFLOWS := PhastModel.UnitNumbers.UnitNumber(StrIOHUFFLOWS);
    NameOfFile := ChangeFileExt(FNameOfFile, '.huf_flow');
    WriteToNameFile(StrDATABINARY, IOHUFFLOWS,
      NameOfFile, foOutput);
  end
  else
  begin
    IOHUFFLOWS := 0;
  end;

  WriteInteger(IHUFCB);
  WriteFloat(HDRY);
  WriteInteger(NHUF);
  WriteInteger(NPHUF);
  WriteInteger(IOHUFHEADS);
  WriteInteger(IOHUFFLOWS);
  WriteString(' # Data set 1 IHUFCB HDRY NHUF NPHUF IOHUFHEADS IOHUFFLOWS');
  NewLine;
end;

procedure TModflowHUF_Writer.WriteDataSet12;
var
  UnitIndex: Integer;
  HGU: THydrogeologicUnit;
  HGUNAM: string;
  PRINTCODE: Integer;
  PrintIndex: TPrintParam;
  PrintItem: TPrintItem;
  PRINTFLAGS: string;
begin
  for UnitIndex := 0 to PhastModel.HydrogeologicUnits.Count - 1 do
  begin
    HGU := PhastModel.HydrogeologicUnits[UnitIndex];
    HGUNAM := HGU.HufName;
    PRINTCODE := HGU.PrintFormat;
    PRINTFLAGS := '';
    for PrintIndex := Low(TPrintParam) to High(TPrintParam) do
    begin
      PrintItem := HGU.PrintItems[PrintIndex];
      if PrintItem.ShouldPrint then
      begin
        PRINTFLAGS := PRINTFLAGS + ' ' + PrintItem.PrintString;
      end;
    end;
    if PRINTFLAGS <> '' then
    begin
      WriteString('PRINT ');
      WriteString(HGUNAM);
      WriteInteger(PRINTCODE);
      WriteString(PRINTFLAGS);
      NewLine;
    end;
  end;
end;

procedure TModflowHUF_Writer.WriteDataSet2;
var
  LTHUF: TOneDIntegerArray;
  LayerIndex: Integer;
begin
  LTHUF := PhastModel.LayerStructure.Laytyp;
  for LayerIndex := 0 to Length(LTHUF) - 1 do
  begin
    WriteInteger(LTHUF[LayerIndex]);
  end;
  WriteString(' # Data set 2: LTHUF');
  NewLine;
end;

procedure TModflowHUF_Writer.WriteDataSet3;
var
  index: Integer;
  LTHUF: TOneDIntegerArray;
begin
  LTHUF := PhastModel.LayerStructure.Laytyp;
  FConvertibleLayerPresent := False;
  for index := 0 to PhastModel.LayerStructure.ModflowLayerCount - 1 do
  begin
    if PhastModel.ModflowWettingOptions.WettingActive
      and (LTHUF[index] <> 0) then
    begin
      WriteInteger(1);
      FConvertibleLayerPresent := True;
    end
    else
    begin
      WriteInteger(0);
    end;
  end;
  WriteString(' # Data set 3: LAYWT');
  NewLine;
end;

procedure TModflowHUF_Writer.WriteDataSet4;
var
  WETFCT: double;
  IWETIT: integer;
  IHDWET: integer;
begin
  if PhastModel.ModflowWettingOptions.WettingActive
    and FConvertibleLayerPresent then
  begin
    WETFCT := PhastModel.ModflowWettingOptions.WettingFactor;
    IWETIT := PhastModel.ModflowWettingOptions.WettingIterations;
    IHDWET := PhastModel.ModflowWettingOptions.WettingEquation;
    WriteFloat(WETFCT);
    WriteInteger(IWETIT);
    WriteInteger(IHDWET);
    WriteString(' # Data set 4: WETFCT, IWETIT, IHDWET');
    NewLine;
  end;
end;

procedure TModflowHUF_Writer.WriteDataSet5;
var
  MFLayerIndex: Integer;
  DataArrayLayerIndex: Integer;
  DataArray: TDataArray;
  LayerDescription: string;
  LTHUF: TOneDIntegerArray;
begin
  LTHUF := PhastModel.LayerStructure.Laytyp;
  if PhastModel.ModflowWettingOptions.WettingActive then
  begin
    DataArray := PhastModel.GetDataSetByName(rsWetDry);
    Assert(DataArray <> nil);

    try
      for MFLayerIndex := 1 to PhastModel.LayerStructure.ModflowLayerCount do
      begin
        if LTHUF[MFLayerIndex-1] <> 0 then
        begin
          DataArrayLayerIndex := PhastModel.LayerStructure.
            ModflowLayerToDataSetLayer(MFLayerIndex);

          LayerDescription := PhastModel.LayerStructure.
            ModflowLayerBottomDescription(DataArrayLayerIndex);

          WriteArray(DataArray, DataArrayLayerIndex, 'Data set 5: WETDRY ' + LayerDescription);
        end;
        if not frmProgress.ShouldContinue then
        begin
          Exit;
        end;
      end;
    finally
      PhastModel.CacheDataArrays;
    end;
  end;
end;

procedure TModflowHUF_Writer.WriteDataSet6(HGU: THydrogeologicUnit);
begin
  WriteString(HGU.HufName);
  WriteString(' # Data set 6: HGUNAM');
  NewLine;
end;

procedure TModflowHUF_Writer.WriteDataSet7(HGU: THydrogeologicUnit);
var
  DataArray: TDataArray;
begin
  DataArray := PhastModel.GetDataSetByName(HGU.TopDataArrayName);
  Assert(DataArray <> nil);
  WriteArray(DataArray, 0, ' Data set 7: TOP ' + HGU.HufName);
  PhastModel.CacheDataArrays;
end;

procedure TModflowHUF_Writer.WriteDataSet8(HGU: THydrogeologicUnit);
var
  DataArray: TDataArray;
begin
  DataArray := PhastModel.GetDataSetByName(HGU.ThickessDataArrayName);
  Assert(DataArray <> nil);
  WriteArray(DataArray, 0, ' Data set 8:  THCK ' + HGU.HufName);
  PhastModel.CacheDataArrays;
end;

procedure TModflowHUF_Writer.WriteDataSet9;
const
  ErrorRoot = 'No VK parameter defined for the following hydrogeologic units';
var
  UnitIndex: Integer;
  HGU: THydrogeologicUnit;
  HGUHANI: double;
  HGUVANI: double;
  HGUNAM: string;
begin
  for UnitIndex := 0 to PhastModel.HydrogeologicUnits.Count - 1 do
  begin
    HGU := PhastModel.HydrogeologicUnits[UnitIndex];
    HGUNAM := HGU.HufName;
    
    if HGU.UsesHaniParam then
    begin
      HGUHANI := 0;
    end
    else
    begin
      HGUHANI := HGU.HorizontalAnisotropy;
    end;

    HGUVANI := 0;
    case HGU.VK_Method of
      vkVK: HGUVANI := 0;
      vkVANI: HGUVANI := HGU.VerticalAnisotropy;
      else Assert(False);
    end;
    if HGUVANI = 0 then
    begin
      if (not HGU.UsesVkParam)
        and (PhastModel.LayerStructure.ModflowLayerCount > 1) then
      begin
        frmErrorsAndWarnings.AddError(ErrorRoot, HGU.HufName);
      end;
    end;
    WriteString(HGUNAM);
    WriteFloat(HGUHANI);
    WriteFloat(HGUVANI);
    WriteString(' # Data set 9: HGUNAM HGUHANI HGUVANI');
    NewLine;
  end;
end;

procedure TModflowHUF_Writer.WriteDataSets10and11;
const
  NoClusters = 'The following parameters in the HUF2 package are not used with '
    + 'any hydrogeologic units.';
var
  ParamIndex: Integer;
  Parameter: THufParameter;
  HufUnitIndex: Integer;
  HGU: THydrogeologicUnit;
  UsedParam: THufUsedParameter;
  UsedParameters: TList;
  PARNAM: string;
  PARTYP: string;
  PARVAL: Double;
  NCLU: Integer;
  Param: TModflowSteadyParameter;
  ClusterIndex: Integer;
  UsedHufUnits: TList;
  HGUNAM: string;
  Mltarr: string;
  Zonarr: string;
  IZ: string;
begin
  UsedParameters := TList.Create;
  UsedHufUnits := TList.Create;
  try
    for ParamIndex := 0 to PhastModel.HufParameters.Count - 1 do
    begin
      Parameter := PhastModel.HufParameters[ParamIndex];
      if FTransient then
      begin
        if not (Parameter.ParameterType in HufSteadyAndTransientParameters) then
        begin
          Continue;
        end;
      end
      else
      begin
        if not (Parameter.ParameterType in HufSteadyParameters) then
        begin
          Continue;
        end;
      end;
      UsedParameters.Clear;
      UsedHufUnits.Clear;
      for HufUnitIndex := 0 to PhastModel.HydrogeologicUnits.Count - 1 do
      begin
        HGU := PhastModel.HydrogeologicUnits[HufUnitIndex];
        UsedParam := HGU.UsesParameter(Parameter);
        if UsedParam <> nil then
        begin
          UsedParameters.Add(UsedParam);
          UsedHufUnits.Add(HGU);
        end;
      end;

      if UsedParameters.Count = 0 then
      begin
        frmErrorsAndWarnings.AddWarning(NoClusters, Parameter.ParameterName);
      end;

      PARNAM := ExpandString(Parameter.ParameterName, 10);
      case Parameter.ParameterType of
        ptHUF_HK: PARTYP := 'HK';
        ptHUF_HANI: PARTYP := 'HANI';
        ptHUF_VK: PARTYP := 'VK';
        ptHUF_VANI: PARTYP := 'VANI';
        ptHUF_SS: PARTYP := 'SS';
        ptHUF_SY: PARTYP := 'SY';
        else Assert(False);
      end;
      PARTYP := ExpandString(' ' + PARTYP, 10);
      PARVAL := Parameter.Value;
      NCLU := UsedParameters.Count;
      
      WriteString(PARNAM);
      WriteString(PARTYP);
      WriteFloat(PARVAL);
      WriteInteger(NCLU);
      WriteString(' # Data set 10: PARNAM PARTYP Parval NCLU');
      NewLine;
      PhastModel.WritePValAndTemplate(PARNAM,PARVAL);

      for ClusterIndex := 0 to UsedParameters.Count - 1 do
      begin
        UsedParam := UsedParameters[ClusterIndex];
        HGU := UsedHufUnits[ClusterIndex];
        HGUNAM := ExpandString(HGU.HufName, 10);

        if UsedParam.UseMultiplier then
        begin
          UsedParam.GenerateMultiplierArrayName;
          Mltarr := UsedParam.MultiplierArrayName;
          UsedMultiplierArrayNames.Add(Mltarr);
        end
        else
        begin
          Mltarr := ' NONE      ';
        end;
        Mltarr := ExpandString(Mltarr, 10);

        if UsedParam.UseZone then
        begin
          UsedParam.GenerateZoneArrayName;
          Zonarr := UsedParam.ZoneArrayName;
          UsedZoneArrayNames.Add(Zonarr);
          IZ := ' 1';
        end
        else
        begin
          Zonarr := ' ALL       ';
          IZ := '';
        end;
        Zonarr := ExpandString(Zonarr, 10);

        WriteString(HGUNAM + ' ');
        WriteString(Mltarr + ' ');
        WriteString(Zonarr + ' ');
        WriteString(IZ);
        WriteString(' # Data Set 11: HGUNAM Mltarr Zonarr IZ');
        NewLine;
      end;
    end;
  finally
    UsedParameters.Free;
    UsedHufUnits.Free;
  end;

  if FTransient then
  begin
    for ParamIndex := 0 to PhastModel.ModflowSteadyParameters.Count - 1 do
    begin
      Param := PhastModel.ModflowSteadyParameters.Items[ParamIndex];
      if Param.ParameterType = ptHUF_SYTP then
      begin
        PARNAM := Param.ParameterName;
        PARTYP := ' SYTP';
        PARVAL := Param.Value;
        NCLU := 1;
    
        WriteString(PARNAM);
        WriteString(PARTYP);
        WriteFloat(PARVAL);
        WriteInteger(NCLU);
        WriteString(' # Data set 10: PARNAM PARTYP Parval NCLU');
        NewLine;

        HGUNAM := 'SYTP';

        if Param.UseMultiplier then
        begin
          Mltarr := Param.MultiplierArrayName(1);
          UsedMultiplierArrayNames.Add(Mltarr);
        end
        else
        begin
          Mltarr := ' NONE ';
        end;

        if Param.UseZone then
        begin
          Zonarr := Param.ZoneArrayName(1);
          IZ := ' 1';
          UsedZoneArrayNames.Add(Zonarr);
        end
        else
        begin
          Zonarr := ' ALL ';
          IZ := '';
        end;

        WriteString(HGUNAM + ' ');
        WriteString(Mltarr + ' ');
        WriteString(Zonarr + ' ');
        WriteString(IZ);
        WriteString(' # Data Set 11: HGUNAM Mltarr Zonarr IZ');
        NewLine;
      end;
    end;
  end;
end;

procedure TModflowHUF_Writer.WriteDataSets6to8;
var
  UnitIndex: Integer;
  HGU: THydrogeologicUnit;
begin
  for UnitIndex := 0 to PhastModel.HydrogeologicUnits.Count - 1 do
  begin
    HGU := PhastModel.HydrogeologicUnits[UnitIndex];
    frmProgress.AddMessage('  Writing Data Set 6 for ' + HGU.HufName);
    WriteDataSet6(HGU);
    frmProgress.AddMessage('  Writing Data Set 7 for ' + HGU.HufName);
    WriteDataSet7(HGU);
    frmProgress.AddMessage('  Writing Data Set 8 for ' + HGU.HufName);
    WriteDataSet8(HGU);
  end;
end;

procedure TModflowHUF_Writer.WriteFile(const AFileName: string);
begin
  if not FHufPackage.IsSelected then
  begin
    Exit
  end;
  if FlowPackageFileGeneratedExternally then
  begin
    Exit;
  end;
  FNameOfFile := FileName(AFileName);
  WriteToNameFile(StrHUF2, PhastModel.UnitNumbers.UnitNumber(StrHUF2),
    FNameOfFile, foInput);
  OpenFile(FNameOfFile);
  try
    frmProgress.AddMessage('Writing HUF2 Package input.');
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

    WriteDataSets6to8;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('  Writing Data Set 9.');
    WriteDataSet9;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('  Writing Data Sets 10 and 11.');
    WriteDataSets10and11;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('  Writing Data Set 12.');
    WriteDataSet12;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('  Checking elevations.');
    CheckElevations;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;
  finally
    CloseFile;
  end;
end;

end.
