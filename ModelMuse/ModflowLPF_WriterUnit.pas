unit ModflowLPF_WriterUnit;

interface

uses Types, SysUtils, GoPhastTypes, CustomModflowWriterUnit, PhastModelUnit,
  ModflowParameterUnit, OrderedCollectionUnit, ModflowPackageSelectionUnit;

type
  TModflowLPF_Writer = class(TCustomFlowPackageWriter)
  private
    NPLPF: integer;
    FParameterUsed: array[ptLPF_HK..ptLPF_VKCB] of boolean;
    FConvertibleLayerPresent: Boolean;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSet4;
    procedure WriteDataSet5;
    procedure WriteDataSet6;
    procedure WriteDataSet7;
    procedure WriteDataSest8and9;
    procedure WriteDataSets10to16;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string);
    Constructor Create(Model: TCustomModel); override;
  end;

const
  AllLpfParameters : TParameterTypes = [ptLPF_HK,
    ptLPF_HANI, ptLPF_VK, ptLPF_VANI, ptLPF_SS, ptLPF_SY, ptLPF_VKCB];
  SteadyLpfParameters : TParameterTypes = [ptLPF_HK,
    ptLPF_HANI, ptLPF_VK, ptLPF_VANI, ptLPF_VKCB];

implementation

uses ModflowUnitNumbers, ModflowOutputControlUnit, DataSetUnit,
  LayerStructureUnit, frmErrorsAndWarningsUnit, frmProgressUnit, Forms;


{ TModflowLPF_Writer }

procedure TModflowLPF_Writer.WriteDataSet1;
var
  ILPFCB: integer;
  HDRY: double;
  Options: string;
  LocalPackage: TLpfSelection;
begin
  ILPFCB := 0;
  GetFlowUnitNumber(ILPFCB);
  HDRY := PhastModel.ModflowOptions.HDry;
  if PhastModel.ModflowFullStressPeriods.TransientModel then
  begin
    NPLPF := PhastModel.ModflowSteadyParameters.CountParameters(AllLpfParameters);
  end
  else
  begin
    NPLPF := PhastModel.ModflowSteadyParameters.CountParameters(SteadyLpfParameters);
  end;
  LocalPackage := Package as TLpfSelection;
  Options := '';
  if LocalPackage.UseStorageCoefficient then
  begin
    Options := ' STORAGECOEFFICIENT';
  end;
  if LocalPackage.UseConstantCV then
  begin
    Options := ' CONSTANTCV';
  end;
  if LocalPackage.UseSaturatedThickness then
  begin
    Options := Options + ' THICKSTRT';
  end;
  if not LocalPackage.UseCvCorrection then
  begin
    Options := Options + ' NOCVCORRECTION';
  end;
  if not LocalPackage.UseVerticalFlowCorrection then
  begin
    Options := Options + ' NOVFC';
  end;

  WriteInteger(ILPFCB);
  WriteFloat(HDRY);
  WriteInteger(NPLPF);
  if Options <> '' then
  begin
    WriteString(Options);
  end;
  WriteString(' # ILPFCB, HDRY, NPLPF');
  if Options <> '' then
  begin
    WriteString(' Options');
  end;
  NewLine;
end;

procedure TModflowLPF_Writer.WriteDataSet2;
var
  LAYTYP: TOneDIntegerArray;
  LayerIndex: Integer;
begin
  LAYTYP := PhastModel.LayerStructure.Laytyp;
  for LayerIndex := 0 to Length(LAYTYP) - 1 do
  begin
    WriteInteger(LAYTYP[LayerIndex]);
  end;
  WriteString(' # LAYTYP');
  NewLine;
end;

procedure TModflowLPF_Writer.WriteDataSet3;
var
  LAYAVG: TOneDIntegerArray;
  LayerIndex: Integer;
begin
  LAYAVG := PhastModel.LayerStructure.Layavg;
  for LayerIndex := 0 to Length(LAYAVG) - 1 do
  begin
    WriteInteger(LAYAVG[LayerIndex]);
  end;
  WriteString(' # LAYAVG');
  NewLine;
end;

procedure TModflowLPF_Writer.WriteDataSet4;
var
  CHANI: TOneDIntegerArray;
  LayerIndex: Integer;
begin
  CHANI := PhastModel.LayerStructure.Chani;
  for LayerIndex := 0 to Length(CHANI) - 1 do
  begin
    WriteInteger(CHANI[LayerIndex]);
  end;
  WriteString(' # CHANI');
  NewLine;
end;

procedure TModflowLPF_Writer.WriteDataSet5;
var
  LAYVKA: TOneDIntegerArray;
  LayerIndex: Integer;
begin
  LAYVKA := PhastModel.LayerStructure.Layvka;
  for LayerIndex := 0 to Length(LAYVKA) - 1 do
  begin
    WriteInteger(LAYVKA[LayerIndex]);
  end;
  WriteString(' # LAYVKA');
  NewLine;
end;

procedure TModflowLPF_Writer.WriteDataSet6;
var
  index: Integer;
  LAYTYP: TOneDIntegerArray;
begin
  LAYTYP := PhastModel.LayerStructure.Laytyp;
  FConvertibleLayerPresent := False;
  for index := 0 to PhastModel.LayerStructure.ModflowLayerCount - 1 do
  begin
    if PhastModel.ModflowWettingOptions.WettingActive
      and (LAYTYP[index] <> 0) then
    begin
      WriteInteger(1);
      FConvertibleLayerPresent := True;
    end
    else
    begin
      WriteInteger(0);
    end;
  end;
  WriteString(' # LAYWET');
  NewLine;
end;

procedure TModflowLPF_Writer.WriteDataSet7;
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
    WriteString(' # WETFCT, IWETIT, IHDWET');
    NewLine;
  end;
end;

procedure TModflowLPF_Writer.WriteDataSets10to16;
var
  GroupIndex: integer;
  Group, NextGroup: TLayerGroup;
  MFLayerIndex: integer;
  LayerIndex: integer;
  ArrayIndex: integer;
  DataArray: TDataArray;
  TransientModel: boolean;
begin
  MFLayerIndex := 0;
  TransientModel := PhastModel.ModflowFullStressPeriods.TransientModel;
  for GroupIndex := 1 to PhastModel.LayerStructure.Count -1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    Group := PhastModel.LayerStructure.LayerGroups[GroupIndex];
    if Group.Simulated then
    begin
      for LayerIndex := 0 to Group.ModflowLayerCount -1 do
      begin
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;

        Inc(MFLayerIndex);
        ArrayIndex := PhastModel.LayerStructure.
          ModflowLayerToDataSetLayer(MFLayerIndex);

        // Data Set 10;
        if FParameterUsed[ptLPF_HK] then
        begin
          WriteInteger(IPRN_Real);
          WriteString(' # HK ' + Group.AquiferName
            + ' Layer ' + IntToStr(MFLayerIndex));
          NewLine;
        end
        else
        begin
          frmProgressMM.AddMessage('  Writing Data Set 10 for layer '
            + IntToStr(MFLayerIndex));
          DataArray := PhastModel.DataArrayManager.GetDataSetByName(rsKx);
          Assert(DataArray <> nil);
          WriteArray(DataArray, ArrayIndex, 'HK ' + Group.AquiferName
            + ' Layer ' + IntToStr(MFLayerIndex));
          CheckArray(DataArray, ArrayIndex, 'Negative '
            + rsKx + ' value',
            cvmGreaterEqual, 0, etError);
        end;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;

        // Data set 11
        if FParameterUsed[ptLPF_HANI] then
        begin
          WriteInteger(IPRN_Real);
          WriteString(' # HANI ' + Group.AquiferName
            + ' Layer ' + IntToStr(MFLayerIndex));
          NewLine;
        end
        else
        begin
          frmProgressMM.AddMessage('  Writing Data Set 11 for layer '
            + IntToStr(MFLayerIndex));
          DataArray := PhastModel.DataArrayManager.GetDataSetByName(rsHorizontalAnisotropy);
          Assert(DataArray <> nil);
          WriteArray(DataArray, ArrayIndex, 'HANI ' + Group.AquiferName
            + ' Layer ' + IntToStr(MFLayerIndex));
          CheckArray(DataArray, ArrayIndex, 'Negative '
            + rsHorizontalAnisotropy + ' value',
            cvmGreaterEqual, 0, etError);
        end;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;

        // Data set 12
        if Group.VerticalHydraulicConductivityMethod = 0 then
        begin
          if FParameterUsed[ptLPF_VK] then
          begin
            WriteInteger(IPRN_Real);
            WriteString(' # VKA ' + Group.AquiferName
              + ' Layer ' + IntToStr(MFLayerIndex));
            NewLine;
          end
          else
          begin
            frmProgressMM.AddMessage('  Writing Data Set 12 for layer '
              + IntToStr(MFLayerIndex));
            DataArray := PhastModel.DataArrayManager.GetDataSetByName(rsKz);
            Assert(DataArray <> nil);
            WriteArray(DataArray, ArrayIndex, 'VKA ' + Group.AquiferName
              + ' Layer ' + IntToStr(MFLayerIndex));
            if PhastModel.Grid.LayerCount > 1 then
            begin
              CheckArray(DataArray, ArrayIndex, 'Negative '
                + rsKz + ' value',
                cvmGreaterEqual, 0, etError);
            end;
          end;
        end
        else
        begin
          if FParameterUsed[ptLPF_VANI] then
          begin
            WriteInteger(IPRN_Real);
            WriteString(' # VKA ' + Group.AquiferName
              + ' Layer ' + IntToStr(MFLayerIndex));
            NewLine;
          end
          else
          begin
            frmProgressMM.AddMessage('  Writing Data Set 12 for layer '
              + IntToStr(MFLayerIndex));
            DataArray := PhastModel.DataArrayManager.GetDataSetByName(rsVerticalAnisotropy);
            Assert(DataArray <> nil);
            WriteArray(DataArray, ArrayIndex, 'VKA ' + Group.AquiferName
              + ' Layer ' + IntToStr(MFLayerIndex));
            if PhastModel.Grid.LayerCount > 1 then
            begin
              CheckArray(DataArray, ArrayIndex, 'Negative or zero '
                + rsVerticalAnisotropy + ' value',
                cvmGreater, 0, etError);
            end;
          end;
        end;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;

        // Data set 13
        if TransientModel then
        begin
          { TODO : Check on SPECIFICSTORAGE option in MODFLOW-2005. }
          if FParameterUsed[ptLPF_SS] then
          begin
            WriteInteger(IPRN_Real);
            WriteString(' # SS ' + Group.AquiferName
              + ' Layer ' + IntToStr(MFLayerIndex));
            NewLine;
          end
          else
          begin
            frmProgressMM.AddMessage('  Writing Data Set 13 for layer '
              + IntToStr(MFLayerIndex));
            DataArray := PhastModel.DataArrayManager.GetDataSetByName(rsSpecific_Storage);
            Assert(DataArray <> nil);
            WriteArray(DataArray, ArrayIndex, 'SS ' + Group.AquiferName
              + ' Layer ' + IntToStr(MFLayerIndex));
            CheckArray(DataArray, ArrayIndex, 'Negative '
              + rsSpecific_Storage + ' value',
              cvmGreaterEqual, 0, etError);
          end;
        end;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;

        // Data set 14
        if TransientModel and (Group.AquiferType <> 0) then
        begin
          if FParameterUsed[ptLPF_SY] then
          begin
            WriteInteger(IPRN_Real);
            WriteString(' # SY ' + Group.AquiferName
              + ' Layer ' + IntToStr(MFLayerIndex));
            NewLine;
          end
          else
          begin
            frmProgressMM.AddMessage('  Writing Data Set 14 for layer '
              + IntToStr(MFLayerIndex));
            DataArray := PhastModel.DataArrayManager.GetDataSetByName(rsSpecificYield);
            Assert(DataArray <> nil);
            WriteArray(DataArray, ArrayIndex, 'SY ' + Group.AquiferName
              + ' Layer ' + IntToStr(MFLayerIndex));
            CheckArray(DataArray, ArrayIndex, 'Negative '
              + rsSpecificYield + ' value',
              cvmGreaterEqual, 0, etError);
          end;
        end;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;

        // Data set 15
        if (LayerIndex = Group.ModflowLayerCount -1) and
          (GroupIndex < PhastModel.LayerStructure.Count-1) then
        begin
          NextGroup := PhastModel.LayerStructure.LayerGroups[GroupIndex+1];
          if not NextGroup.Simulated then
          begin
            if FParameterUsed[ptLPF_VKCB] then
            begin
              WriteInteger(IPRN_Real);
              WriteString(' # VKCB ' + NextGroup.AquiferName
                + ' Layer ' + IntToStr(MFLayerIndex));
              NewLine;
            end
            else
            begin
              frmProgressMM.AddMessage('  Writing Data Set 15 for layer '
                + IntToStr(MFLayerIndex));
              DataArray := PhastModel.DataArrayManager.GetDataSetByName(rsModflow_CBKz);
              Assert(DataArray <> nil);
              WriteArray(DataArray, ArrayIndex+1, 'VKCB ' + NextGroup.AquiferName
                + ' Layer ' + IntToStr(MFLayerIndex));
              CheckArray(DataArray, ArrayIndex, 'Negative '
                + rsKz + ' value',
                cvmGreaterEqual, 0, etError);
            end;
          end;
        end;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;

        // Data set 16
        if PhastModel.ModflowWettingOptions.WettingActive
          and (Group.AquiferType <> 0) then
        begin
          { TODO : Consider supporting LAYWET }
          frmProgressMM.AddMessage('  Writing Data Set 16 for layer '
            + IntToStr(MFLayerIndex));
          DataArray := PhastModel.DataArrayManager.GetDataSetByName(rsWetDry);
          Assert(DataArray <> nil);
          WriteArray(DataArray, ArrayIndex, 'WETDRY ' + Group.AquiferName
            + ' Layer ' + IntToStr(MFLayerIndex));
        end;
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TModflowLPF_Writer.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  if not PhastModel.ModflowPackages.LpfPackage.IsSelected then
  begin
    Exit
  end;
  if FlowPackageFileGeneratedExternally then
  begin
    Exit;
  end;
  NameOfFile := FileName(AFileName);
  WriteToNameFile(StrLPF, PhastModel.UnitNumbers.UnitNumber(StrLPF),
    NameOfFile, foInput);
  OpenFile(NameOfFile);
  try
    frmProgressMM.AddMessage('Writing LPF Package input.');
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

    WriteDataSest8and9;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSets10to16;
  finally
    CloseFile;
  end;
end;

constructor TModflowLPF_Writer.Create(Model: TCustomModel);
var
  Index: TParameterType;
begin
  inherited;
  for Index := Low(TParameterType) to High(TParameterType) do
  begin
    if Index in AllLpfParameters then FParameterUsed[Index] := False;
  end;
end;

function TModflowLPF_Writer.Package: TModflowPackageSelection;
begin
  result := PhastModel.ModflowPackages.LpfPackage;
end;

class function TModflowLPF_Writer.Extension: string;
begin
  result := '.lpf';
end;

procedure TModflowLPF_Writer.WriteDataSest8and9;
var
  ParamIndex: Integer;
  Param: TModflowSteadyParameter;
  PARNAM: string;
  PARTYP: string;
  PARVAL: double;
  NCLU: integer;
  ValidParamTypes: TParameterTypes;
  LayerCount: integer;
  Clusters: TOneDIntegerArray;
  UniformLayers: TBooleanDynArray;
  ClusterIndex: Integer;
  LAYER: integer;
  MLTARR: string;
  ZONARR: string;
  Error: string;
const
  IZ = 1;
begin
  LayerCount := PhastModel.LayerStructure.ModflowLayerCount;
  if NPLPF > 0 then
  begin
    if PhastModel.ModflowFullStressPeriods.TransientModel then
    begin
      ValidParamTypes := AllLpfParameters;
    end
    else
    begin
      ValidParamTypes := SteadyLpfParameters;
    end;
    for ParamIndex := 0 to PhastModel.ModflowSteadyParameters.Count - 1 do
    begin
      Param := PhastModel.ModflowSteadyParameters.Items[ParamIndex];
      if Param.ParameterType in ValidParamTypes then
      begin
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;

        PARNAM := Param.ParameterName;
        case Param.ParameterType of
          ptLPF_HK: PARTYP := 'HK';
          ptLPF_HANI: PARTYP := 'HANI';
          ptLPF_VK: PARTYP := 'VK';
          ptLPF_VANI: PARTYP := 'VANI';
          ptLPF_SS: PARTYP := 'SS';
          ptLPF_SY: PARTYP := 'SY';
          ptLPF_VKCB: PARTYP := 'VKCB';
          else Assert(False);
        end;
        FParameterUsed[Param.ParameterType] := True;
        PARTYP := ' ' + PARTYP;
        PARVAL := Param.Value;

        if (Param.ParameterType = ptLPF_VKCB) and
          (PhastModel.LayerStructure.ModflowConfiningBedCount = 0) then
        begin
          frmErrorsAndWarnings.AddError('VKCB parameter improperly defined.',
            Param.ParameterName + ' is a VKCB parameter but all the layers '
            + 'in the model are simulated');
        end;

        if not Param.UseZone then
        begin
          if Param.ParameterType = ptLPF_VKCB then
          begin
            NCLU := PhastModel.LayerStructure.ModflowConfiningBedCount;
          end
          else
          begin
            NCLU := PhastModel.LayerStructure.ModflowLayerCount;
          end;
          SetLength(Clusters, 0);
        end
        else
        begin
          IdentifyZoneClusters(NCLU, Clusters, UniformLayers, LayerCount, Param);
          if NCLU = 0 then
          begin
            Error := 'Parameter ' + Param.ParameterName
              + ' is not applied to any cells.  Check that '
              + Param.ZoneName + ' is set to "True" in at least one ';
            if Param.ParameterType = ptLPF_VKCB then
            begin
              Error := Error + 'non-simulated unit.';
            end
            else
            begin
              Error := Error + 'simulated unit.';
            end;
            frmErrorsAndWarnings.AddError('Parameter zones not defined.',
              Error);
          end;
        end;

        // Data set 8
        frmProgressMM.AddMessage('  Writing Data Set 8 for parameter: ' + PARNAM);
        WriteString(PARNAM);
        WriteString(PARTYP);
        WriteFloat(PARVAL);
        WriteInteger(NCLU);
        WriteString(' # PARNAM, PARTYP, PARVAL, NCLU');
        NewLine;

        PhastModel.WritePValAndTemplate(PARNAM,PARVAL);

        // Data set 9
        frmProgressMM.AddMessage('  Writing Data Set 9 for parameter: ' + PARNAM);
        for ClusterIndex := 0 to NCLU - 1 do
        begin
          if Param.UseZone then
          begin
            LAYER := Clusters[ClusterIndex];
            if UniformLayers[ClusterIndex] then
            begin
              ZONARR := 'ALL';
            end
            else
            begin
              ZONARR := Param.ZoneArrayName(LAYER);
              UsedZoneArrayNames.Add(ZONARR);
            end;
          end
          else
          begin
            LAYER := ClusterIndex + 1;
            ZONARR := 'ALL'
          end;
          ZONARR := ' ' + ZONARR;
          if Param.UseMultiplier then
          begin
            MLTARR := Param.MultiplierArrayName(LAYER);
            UsedMultiplierArrayNames.Add(MLTARR);
          end
          else
          begin
            MLTARR := 'NONE';
          end;
          MLTARR := ' ' + MLTARR;
          WriteInteger(Layer);
          WriteString(MLTARR + ' ');
          WriteString(ZONARR + ' ');
          if Param.UseZone then
          begin
            WriteInteger(IZ);
          end;
          WriteString(' # Layer, MLTARR, ZONARR');
          if Param.UseZone then
          begin
            WriteString(' IZ');
          end;
          NewLine;
        end;
        PhastModel.DataArrayManager.CacheDataArrays;
      end;
    end;
  end;
end;

end.
