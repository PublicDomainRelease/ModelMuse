unit ModflowMultiplierZoneWriterUnit;

interface

uses LayerStructureUnit, Classes, PhastModelUnit, SysUtils, HufDefinition,
  CustomModflowWriterUnit, ModflowParameterUnit, DataSetUnit;

type
  TCustomMultZoneWriter = class(TCustomModflowWriter)
  protected
    FFileUnit: integer;
    FFileType: string;
    procedure WriteDataSet0; virtual; abstract;
    procedure WriteDataSet1; virtual; abstract;
    function UseSteadyParameter(Param: TModflowSteadyParameter): boolean;
      virtual; 
    function GetDataArray(Param: TModflowSteadyParameter): TDataArray;
      virtual; abstract;
    function GetArrayName(Param: TModflowSteadyParameter;
      LayerIndex: integer): string; virtual; abstract;
    function ArrayUsed(const ArrayName: string): boolean; virtual; abstract;
    function ArrayType: string; virtual; abstract;
    function DataSet2Comment: string; virtual; abstract;
    procedure WriteDataSets2And3;
    function NumberOfArrays: integer; virtual; abstract;
    property FileType : string read FFileType;
    property FileUnit : integer read FFileUnit;
    function TransientArrayList: TList; virtual; abstract;
    function UsesHufParam(UsedParam: THufUsedParameter;
      var ArrayName: string; var DataArray: TDataArray): boolean; virtual;
  public
    procedure WriteFile(const AFileName: string);
  end;

  TModflowZoneWriter = class(TCustomMultZoneWriter)
  protected
    procedure WriteDataSet0; override;
    procedure WriteDataSet1; override;
    function UseSteadyParameter(Param: TModflowSteadyParameter): boolean;
      override;
    function GetDataArray(Param: TModflowSteadyParameter): TDataArray;
      override;
    function GetArrayName(Param: TModflowSteadyParameter;
      LayerIndex: integer): string; override;
    function ArrayUsed(const ArrayName: string): boolean; override;
    function ArrayType: string; override;
    function DataSet2Comment: string; override;
    class function Extension: string; override;
    function NumberOfArrays: integer; override;
    function TransientArrayList: TList; override;
    function UsesHufParam(UsedParam: THufUsedParameter;
      var ArrayName: string; var DataArray: TDataArray): boolean; override;
  public
    Constructor Create(Model: TCustomModel); override;
  end;

  TModflowMultiplierWriter = class(TCustomMultZoneWriter)
  protected
    procedure WriteDataSet0; override;
    procedure WriteDataSet1; override;
    function UseSteadyParameter(Param: TModflowSteadyParameter): boolean;
      override;
    function GetDataArray(Param: TModflowSteadyParameter): TDataArray;
      override;
    function GetArrayName(Param: TModflowSteadyParameter;
      LayerIndex: integer): string; override;
    function ArrayUsed(const ArrayName: string): boolean; override;
    function ArrayType: string; override;
    function DataSet2Comment: string; override;
    class function Extension: string; override;
    function NumberOfArrays: integer; override;
    function TransientArrayList: TList; override;
    function UsesHufParam(UsedParam: THufUsedParameter;
      var ArrayName: string; var DataArray: TDataArray): boolean; override;
  public
    Constructor Create(Model: TCustomModel); override;
  end;

implementation

uses frmErrorsAndWarningsUnit, OrderedCollectionUnit, ModflowUnitNumbers, 
  frmProgressUnit, Forms;

{ TCustomMultZoneWriter }

function TCustomMultZoneWriter.UsesHufParam(UsedParam: THufUsedParameter;
  var ArrayName: string; var DataArray: TDataArray): boolean;
begin
  result := False;
  case UsedParam.Parameter.ParameterType of
    ptUndefined, ptLPF_HK, ptLPF_HANI, ptLPF_VK, ptLPF_VANI, ptLPF_SS,
    ptLPF_SY, ptLPF_VKCB, ptRCH, ptEVT, ptETS, ptCHD, ptGHB, ptQ, ptRIV,
    ptDRN, ptDRT, ptSFR, ptHFB:
      begin
        Assert(False);
      end;
    ptHUF_HK, ptHUF_HANI, ptHUF_VK, ptHUF_VANI, ptHUF_KDEP, ptHUF_LVDA:
      begin
        result := PhastModel.ModflowPackages.HufPackage.IsSelected;
      end;
    ptHUF_SS, ptHUF_SY, ptHUF_SYTP:
      begin
        result := PhastModel.ModflowPackages.HufPackage.IsSelected
          and PhastModel.ModflowFullStressPeriods.TransientModel;
      end;
    else Assert(False);
  end;
end;

function TCustomMultZoneWriter.UseSteadyParameter(
  Param: TModflowSteadyParameter): boolean;
begin
  result := False;
  case Param.ParameterType of
    ptLPF_HK, ptLPF_HANI, ptLPF_VK, ptLPF_VANI, ptLPF_VKCB:
      begin
        result := PhastModel.ModflowPackages.LpfPackage.IsSelected;
      end;
    ptLPF_SS, ptLPF_SY:
      begin
        result := PhastModel.ModflowPackages.LpfPackage.IsSelected
          and PhastModel.ModflowFullStressPeriods.TransientModel;
      end;
    ptHUF_LVDA:
      begin
        result := PhastModel.ModflowPackages.HufPackage.IsSelected;
      end;
    ptHUF_SYTP:
      begin
        result := PhastModel.ModflowPackages.HufPackage.IsSelected
          and PhastModel.ModflowFullStressPeriods.TransientModel;
      end;
    ptUndefined, ptRCH, ptEVT, ptETS, ptCHD, ptGHB, ptQ,
    ptRIV, ptDRN, ptDRT, ptSFR, ptHFB,
    ptHUF_HK, ptHUF_HANI, ptHUF_VK, ptHUF_VANI, ptHUF_SS, ptHUF_SY,
    ptHUF_KDEP:
      begin
        Assert(False);
      end;
  end;
end;

procedure TCustomMultZoneWriter.WriteDataSets2And3;
var
  LayerCount: Integer;
  ArrayIndex: Integer;
  ArrayName: string;
  LayerIndex: Integer;
  DataArray: TDataArray;
  Param: TModflowSteadyParameter;
  ParamIndex: Integer;
  TransientList: TList;
  Index: Integer;
  Limit: Integer;
  HGU: THydrogeologicUnit;
  UnitIndex: Integer;
  UsedParam: THufUsedParameter;
  Description: string;
begin
  LayerCount := PhastModel.LayerStructure.ModflowLayerCount;
  for ParamIndex := 0 to PhastModel.ModflowSteadyParameters.Count - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    Param := PhastModel.ModflowSteadyParameters.Items[ParamIndex];
    if UseSteadyParameter(Param) then
    begin
      DataArray := GetDataArray(Param);
      for LayerIndex := 1 to LayerCount do
      begin
        ArrayName := GetArrayName(Param, LayerIndex);
        if ArrayUsed(ArrayName) then
        begin
          Assert(ArrayName <> '');
          While Length(ArrayName) < 10 do
          begin
            ArrayName := ArrayName + ' ';
          end;
          // Data set 2;
          WriteString(ArrayName);
          WriteString(DataSet2Comment);
          NewLine;

          // Data set 3
          ArrayIndex := PhastModel.LayerStructure.
            ModflowLayerToDataSetLayer(LayerIndex);
          if Param.ParameterType = ptLPF_VKCB then
          begin
            Inc(ArrayIndex);
          end;
          WriteArray(DataArray, ArrayIndex, ArrayType + ' array for '
            + Param.ParameterName + ' in '
            + PhastModel.LayerStructure.ModflowLayerBottomDescription(ArrayIndex));
        end;
      end;
      PhastModel.DataArrayManager.CacheDataArrays;
      DataArray.CacheData;
    end;
  end;

  if PhastModel.ModflowPackages.HufPackage.IsSelected then
  begin
    for UnitIndex := 0 to PhastModel.HydrogeologicUnits.Count - 1 do
    begin
      HGU := PhastModel.HydrogeologicUnits[UnitIndex];
      for ParamIndex := 0 to HGU.HufUsedParameters.Count - 1 do
      begin
        UsedParam := HGU.HufUsedParameters[ParamIndex];
        if UsesHufParam(UsedParam, ArrayName, DataArray) then
        begin
          // Data set 2;
          Assert(ArrayName <> '');
          WriteString(ArrayName);
          WriteString('          ');
          WriteString(DataSet2Comment);
          NewLine;

          // Data set 3
          Description := UsedParam.Description;
          WriteArray(DataArray, 0, Description);
          PhastModel.DataArrayManager.CacheDataArrays;
          DataArray.CacheData;
        end;
      end;
    end;
  end;

  TransientList := TransientArrayList;
  for Index := 0 to TransientList.Count - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    DataArray := TransientList[Index];
    if DataArray.LayerCount = 1 then
    begin
      Limit := 1;
    end
    else
    begin
      Limit := LayerCount;
    end;

    for LayerIndex := 1 to Limit do
    begin
      ArrayName := DataArray.Name;
      Assert(ArrayName <> '');
      if DataArray.LayerCount > 1 then
      begin
        ArrayName := ArrayName + '_' + IntToStr(LayerIndex);
      end;
      While Length(ArrayName) < 10 do
      begin
        ArrayName := ArrayName + ' ';
      end;

          // Data set 2;
      WriteString(ArrayName);
      WriteString(DataSet2Comment);
      NewLine;

      // Data set 3
      ArrayIndex := PhastModel.LayerStructure.
        ModflowLayerToDataSetLayer(LayerIndex);

      WriteArray(DataArray, ArrayIndex, ArrayType + ' array');
    end;
    PhastModel.DataArrayManager.CacheDataArrays;
    DataArray.CacheData;
  end;
end;

procedure TCustomMultZoneWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  if NumberOfArrays = 0 then Exit;
  if PhastModel.PackageGeneratedExternally(FileType) then
  begin
    Exit;
  end;

  NameOfFile := FileName(AFileName);
  WriteToNameFile(FileType, FileUnit, NameOfFile, foInput);
  OpenFile(FileName(AFileName));
  try
    frmProgressMM.AddMessage('Writing ' + FileType + ' Package input.');
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
  finally
    CloseFile;
  end;
end;

{ TModflowZoneWriter }

function TModflowZoneWriter.ArrayType: string;
begin
  result := 'Zone';
end;

function TModflowZoneWriter.ArrayUsed(const ArrayName: string): boolean;
begin
  result := UsedZoneArrayNames.IndexOf(ArrayName) >= 0
end;

constructor TModflowZoneWriter.Create(Model: TCustomModel);
begin
  inherited;
  FFileUnit := PhastModel.UnitNumbers.UnitNumber(StrZONE);
  FFileType := 'ZONE';
end;

function TModflowZoneWriter.DataSet2Comment: string;
begin
  result := ' # ZONNAM';
end;

class function TModflowZoneWriter.Extension: string;
begin
  result := '.zon';
end;

function TModflowZoneWriter.GetArrayName(Param: TModflowSteadyParameter;
  LayerIndex: integer): string;
begin
  result := Param.ZoneArrayName(LayerIndex);
end;

function TModflowZoneWriter.GetDataArray(
  Param: TModflowSteadyParameter): TDataArray;
begin
  result := PhastModel.DataArrayManager.GetDataSetByName(Param.ZoneName);
end;

function TModflowZoneWriter.NumberOfArrays: integer;
var
  List: TList;
  Index: Integer;
  DataArray: TDataArray;
begin
  result := UsedZoneArrayNames.Count;
  List := TransientArrayList;
  for Index := 0 to List.Count - 1 do
  begin
    DataArray := List[Index];
    if DataArray.LayerCount = 1 then
    begin
      Inc(result);
    end
    else
    begin
      Assert(False);
    end;
  end;
end;

function TModflowZoneWriter.TransientArrayList: TList;
begin
  result := PhastModel.TransientZoneArrays;
end;

function TModflowZoneWriter.UsesHufParam(UsedParam: THufUsedParameter;
  var ArrayName: string; var DataArray: TDataArray): boolean;
begin
  result := inherited UsesHufParam(UsedParam, ArrayName, DataArray);
  if result then
  begin
    result := UsedParam.UseZone;
    if result then
    begin
      ArrayName := UsedParam.ZoneArrayName;
      if ArrayName = '' then
      begin
        UsedParam.GenerateZoneArrayName;
        ArrayName := UsedParam.ZoneArrayName;
        UsedZoneArrayNames.Add(ArrayName);
      end;
      DataArray := PhastModel.DataArrayManager.GetDataSetByName(UsedParam.ZoneDataSetName);
    end;
  end;
end;

function TModflowZoneWriter.UseSteadyParameter(
  Param: TModflowSteadyParameter): boolean;
begin
  result := inherited UseSteadyParameter(Param);
  if result then
  begin
    result := Param.UseZone;
  end;
end;

procedure TModflowZoneWriter.WriteDataSet0;
begin
  WriteCommentLine('Zone (ZONE) File created on '
    + DateToStr(Now) + ' by ' + PhastModel.ProgramName
    + ' version ' + ModelVersion + '.');
end;

procedure TModflowZoneWriter.WriteDataSet1;
var
  UnitIndex: Integer;
  HGU: THydrogeologicUnit;
  ParamIndex: Integer;
  UsedParam: THufUsedParameter;
  ArrayName: string;
  DataArray: TDataArray;
begin
  //  If required, update UsedZoneArrayNames
  if PhastModel.ModflowPackages.HufPackage.IsSelected then
  begin
    for UnitIndex := 0 to PhastModel.HydrogeologicUnits.Count - 1 do
    begin
      HGU := PhastModel.HydrogeologicUnits[UnitIndex];
      for ParamIndex := 0 to HGU.HufUsedParameters.Count - 1 do
      begin
        UsedParam := HGU.HufUsedParameters[ParamIndex];
        UsesHufParam(UsedParam, ArrayName, DataArray)
      end;
    end;
  end;

  WriteInteger(NumberOfArrays);
  WriteString(' # NZN');
  NewLine;
end;

{ TModflowMultiplierWriter }

function TModflowMultiplierWriter.ArrayType: string;
begin
  result := 'Multiplier';
end;

function TModflowMultiplierWriter.ArrayUsed(const ArrayName: string): boolean;
begin
  result := UsedMultiplierArrayNames.IndexOf(ArrayName) >= 0
end;

constructor TModflowMultiplierWriter.Create(Model: TCustomModel);
begin
  inherited;
  FFileUnit := PhastModel.UnitNumbers.UnitNumber(StrMULT);
  FFileType := 'MULT';
end;

function TModflowMultiplierWriter.DataSet2Comment: string;
begin
  result := ' # MLTNAM';
end;

class function TModflowMultiplierWriter.Extension: string;
begin
  result := '.mlt';
end;

function TModflowMultiplierWriter.GetArrayName(Param: TModflowSteadyParameter;
  LayerIndex: integer): string;
begin
  result := Param.MultiplierArrayName(LayerIndex);
end;

function TModflowMultiplierWriter.GetDataArray(
  Param: TModflowSteadyParameter): TDataArray;
begin
  result := PhastModel.DataArrayManager.GetDataSetByName(Param.MultiplierName);
end;

function TModflowMultiplierWriter.NumberOfArrays: integer;
var
  List: TList;
  Index: Integer;
  DataArray: TDataArray;
begin
  result := UsedMultiplierArrayNames.Count;
  List := TransientArrayList;
  for Index := 0 to List.Count - 1 do
  begin
    DataArray := List[Index];
    if DataArray.LayerCount = 1 then
    begin
      Inc(result);
    end
    else
    begin
      Assert(False);
    end;
  end;
end;

function TModflowMultiplierWriter.TransientArrayList: TList;
begin
  result := PhastModel.TransientMultiplierArrays;
end;

function TModflowMultiplierWriter.UsesHufParam(UsedParam: THufUsedParameter;
  var ArrayName: string; var DataArray: TDataArray): boolean;
begin
  result := inherited UsesHufParam(UsedParam, ArrayName, DataArray);
  if result then
  begin
    result := UsedParam.UseMultiplier;
    if result then
    begin
      ArrayName := UsedParam.MultiplierArrayName;
      if ArrayName = '' then
      begin
        UsedParam.GenerateMultiplierArrayName;
        ArrayName := UsedParam.MultiplierArrayName;
        UsedMultiplierArrayNames.Add(ArrayName);
      end;
      DataArray := PhastModel.DataArrayManager.GetDataSetByName(UsedParam.MultiplierDataSetName);
    end;
  end;
end;

function TModflowMultiplierWriter.UseSteadyParameter(
  Param: TModflowSteadyParameter): boolean;
begin
  result := inherited UseSteadyParameter(Param);
  if result then
  begin
    result := Param.UseMultiplier;
  end;
end;

procedure TModflowMultiplierWriter.WriteDataSet0;
begin
  WriteCommentLine('Multiplier (MULT) File created on '
    + DateToStr(Now) + ' by ' + PhastModel.ProgramName
    + ' version ' + ModelVersion + '.');
end;

procedure TModflowMultiplierWriter.WriteDataSet1;
var
  UnitIndex: Integer;
  HGU: THydrogeologicUnit;
  ParamIndex: Integer;
  UsedParam: THufUsedParameter;
  ArrayName: string;
  DataArray: TDataArray;
begin
  //  If required, update UsedMultiplierArrayNames
  if PhastModel.ModflowPackages.HufPackage.IsSelected then
  begin
    for UnitIndex := 0 to PhastModel.HydrogeologicUnits.Count - 1 do
    begin
      HGU := PhastModel.HydrogeologicUnits[UnitIndex];
      for ParamIndex := 0 to HGU.HufUsedParameters.Count - 1 do
      begin
        UsedParam := HGU.HufUsedParameters[ParamIndex];
        UsesHufParam(UsedParam, ArrayName, DataArray)
      end;
    end;
  end;
  
  WriteInteger(NumberOfArrays);
  WriteString(' # NML');
  NewLine;
end;

end.
