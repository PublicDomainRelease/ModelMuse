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
      virtual; abstract;
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
      var ArrayName: string; var DataArray: TDataArray): boolean; virtual; abstract;
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
    Constructor Create(Model: TPhastModel); override;
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
    Constructor Create(Model: TPhastModel); override;
  end;

implementation

uses frmErrorsAndWarningsUnit, OrderedCollectionUnit, ModflowUnitNumbers, 
  frmProgressUnit;

{ TCustomMultZoneWriter }

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
    if not frmProgress.ShouldContinue then
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
      PhastModel.CacheDataArrays;
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
          WriteString(ArrayName);
          WriteString('          ');
          WriteString(DataSet2Comment);
          NewLine;

          // Data set 3
          Description := UsedParam.Description;
          WriteArray(DataArray, 0, Description);
          PhastModel.CacheDataArrays;
          DataArray.CacheData;
        end;
      end;
    end;
  end;

  TransientList := TransientArrayList;
  for Index := 0 to TransientList.Count - 1 do
  begin
    if not frmProgress.ShouldContinue then
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
    PhastModel.CacheDataArrays;
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
    frmProgress.AddMessage('Writing ' + FileType + ' Package input.');
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

    frmProgress.AddMessage('  Writing Data Sets 2 and 3.');
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

constructor TModflowZoneWriter.Create(Model: TPhastModel);
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
  result := PhastModel.GetDataSetByName(Param.ZoneName);
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
  result := UsedParam.UseZone;
  if result then
  begin
    ArrayName := UsedParam.ZoneArrayName;
    DataArray := PhastModel.GetDataSetByName(UsedParam.ZoneDataSetName);
  end;
end;

function TModflowZoneWriter.UseSteadyParameter(
  Param: TModflowSteadyParameter): boolean;
begin
  result := Param.UseZone;
end;

procedure TModflowZoneWriter.WriteDataSet0;
begin
  WriteCommentLine('Zone (ZONE) File created on '
    + DateToStr(Now) + ' by ' + PhastModel.ProgramName
    + ' version ' + ModelVersion + '.');
end;

procedure TModflowZoneWriter.WriteDataSet1;
begin
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

constructor TModflowMultiplierWriter.Create(Model: TPhastModel);
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
  result := PhastModel.GetDataSetByName(Param.MultiplierName);
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
  result := UsedParam.UseMultiplier;
  if result then
  begin
    ArrayName := UsedParam.MultiplierArrayName;
    DataArray := PhastModel.GetDataSetByName(UsedParam.MultiplierDataSetName);
  end;
end;

function TModflowMultiplierWriter.UseSteadyParameter(
  Param: TModflowSteadyParameter): boolean;
begin
  result := Param.UseMultiplier;
end;

procedure TModflowMultiplierWriter.WriteDataSet0;
begin
  WriteCommentLine('Multiplier (MULT) File created on '
    + DateToStr(Now) + ' by ' + PhastModel.ProgramName
    + ' version ' + ModelVersion + '.');
end;

procedure TModflowMultiplierWriter.WriteDataSet1;
begin
  WriteInteger(NumberOfArrays);
  WriteString(' # NML');
  NewLine;
end;

end.
