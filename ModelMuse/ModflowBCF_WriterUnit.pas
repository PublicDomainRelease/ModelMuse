unit ModflowBCF_WriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit, PhastModelUnit,
  LayerStructureUnit, SysUtils;

type
  TModflowBCF_Writer = class(TCustomFlowPackageWriter)
  private
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSets4to11;
    procedure WriteDataSet4(Layer: Integer; TransientModel: Boolean;
      AquiferType: integer);
    procedure WriteDataSet5or6(AquiferType: Integer; Layer: Integer);
    procedure WriteDataSet7(Layer: Integer; GroupIndex: Integer;
      Group: TLayerGroup; LayerIndex: Integer);
    procedure WriteDataSet8(AquiferType: Integer; Layer: Integer);
    procedure WriteDataSet9(AquiferType: Integer; Layer: Integer);
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  ModflowUnitNumbers, frmProgressUnit, GoPhastTypes, 
  DataSetUnit;

{ TModflowBCF_Writer }

class function TModflowBCF_Writer.Extension: string;
begin
  result := '.bcf';
end;

procedure TModflowBCF_Writer.WriteDataSet5or6(AquiferType: Integer;
  Layer: Integer);
var
  DataArray: TDataArray;
begin
  case AquiferType of
    0, 2:
      begin
        // Data set 5
        DataArray := PhastModel.GetDataSetByName(StrTransmissivity);
        Assert(DataArray <> nil);
        WriteArray(DataArray, Layer, 'Tran');
      end;
    1, 3:
      begin
        // Data set 6
        DataArray := PhastModel.GetDataSetByName(rsKx);
        Assert(DataArray <> nil);
        WriteArray(DataArray, Layer, 'HY');
      end;
  end;
end;

function TModflowBCF_Writer.Package: TModflowPackageSelection;
begin
  result := PhastModel.ModflowPackages.BcfPackage;
end;

procedure TModflowBCF_Writer.WriteDataSet1;
var
  IBCFCB: Integer;
  HDRY: Real;
  IWDFLG: integer;
  WETFCT: Real;
  IWETIT: Integer;
  IHDWET: Integer;
begin
  IBCFCB := 0;
  GetFlowUnitNumber(IBCFCB);
  HDRY := PhastModel.ModflowOptions.HDry;
  IWDFLG := Ord(PhastModel.ModflowWettingOptions.WettingActive);
  WETFCT := PhastModel.ModflowWettingOptions.WettingFactor;
  IWETIT := PhastModel.ModflowWettingOptions.WettingIterations;
  IHDWET := PhastModel.ModflowWettingOptions.WettingEquation;
  WriteInteger(IBCFCB);
  WriteFloat(HDRY);
  WriteInteger(IWDFLG);
  WriteFloat(WETFCT);
  WriteInteger(IWETIT);
  WriteInteger(IHDWET);
  WriteString(' # IBCFCB HDRY IWDFLG WETFCT IWETIT IHDWET');
  NewLine;
end;

procedure TModflowBCF_Writer.WriteDataSet2;
var
  Ltype: TOneDIntegerArray;
  LayerIndex: Integer;
begin
  Ltype := PhastModel.LayerStructure.Laytyp;
  for LayerIndex := 0 to Length(Ltype) - 1 do
  begin
    WriteInteger(Ltype[LayerIndex]);
  end;
  WriteString(' # Ltype');
  NewLine;
end;

procedure TModflowBCF_Writer.WriteDataSet3;
var
  LayerIndex: Integer;
  Trpy: TOneDRealArray;
begin
  WriteU2DRELHeader('TRPY');
  Trpy := PhastModel.LayerStructure.Trpy;
  for LayerIndex := 0 to Length(Trpy) - 1 do
  begin
    WriteFloat(Trpy[LayerIndex]);
  end;
  WriteString(' # TRPY');
  NewLine;
end;

procedure TModflowBCF_Writer.WriteDataSets4to11;
var
  GroupIndex: Integer;
  Group: TLayerGroup;
  LayerIndex: Integer;
  Layer: integer;
  TransientModel: Boolean;
  AquiferType: Integer;
begin
  Layer := 0;
  TransientModel := PhastModel.ModflowStressPeriods.TransientModel;
  for GroupIndex := 1 to PhastModel.LayerStructure.Count - 1 do
  begin
    Group := PhastModel.LayerStructure[GroupIndex];
    if Group.Simulated then
    begin
      for LayerIndex := 0 to Group.ModflowLayerCount - 1 do
      begin
        frmProgress.AddMessage('  Writing data for layer '
          + IntToStr(Layer+1) + '.');
        AquiferType := Group.AquiferType;
        if (AquiferType = 1) and (Layer > 0) then
        begin
          AquiferType := 3;
        end;
        WriteDataSet4(Layer, TransientModel, AquiferType);
        if not frmProgress.ShouldContinue then
        begin
          Exit;
        end;

        WriteDataSet5or6(AquiferType, Layer);
        if not frmProgress.ShouldContinue then
        begin
          Exit;
        end;

        WriteDataSet7(Layer, GroupIndex, Group, LayerIndex);
        if not frmProgress.ShouldContinue then
        begin
          Exit;
        end;

        WriteDataSet8(AquiferType, Layer);
        if not frmProgress.ShouldContinue then
        begin
          Exit;
        end;

        WriteDataSet9(AquiferType, Layer);
        if not frmProgress.ShouldContinue then
        begin
          Exit;
        end;

        Inc(Layer);
      end;
    end
    else
    begin
      Inc(Layer);
    end;
  end;
end;

procedure TModflowBCF_Writer.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if FlowPackageFileGeneratedExternally then
  begin
    Exit;
  end;
  NameOfFile := FileName(AFileName);
  WriteToNameFile(StrBCF, PhastModel.UnitNumbers.UnitNumber(StrBCF),
    NameOfFile, foInput);
  OpenFile(NameOfFile);
  try
    frmProgress.AddMessage('Writing BCF Package input.');
    WriteDataSet1;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet2;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet3;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSets4to11;
  finally
    CloseFile;
  end;

end;

procedure TModflowBCF_Writer.WriteDataSet9(AquiferType: Integer;
  Layer: Integer);
var
  DataArray: TDataArray;
begin
  if PhastModel.ModflowWettingOptions.WettingActive
    and (AquiferType in [1, 3]) then
  begin
    DataArray := PhastModel.GetDataSetByName(rsWetDry);
    Assert(DataArray <> nil);
    WriteArray(DataArray, Layer, 'WETDRY');
  end;
end;

procedure TModflowBCF_Writer.WriteDataSet8(AquiferType: Integer;
  Layer: Integer);
var
  DataArray: TDataArray;
begin
  if AquiferType in [2, 3] then
  begin
    DataArray := PhastModel.GetDataSetByName(rsSpecificYield);
    Assert(DataArray <> nil);
    WriteArray(DataArray, Layer, 'Sf2');
  end;
end;

procedure TModflowBCF_Writer.WriteDataSet7(Layer: Integer; GroupIndex: Integer;
  Group: TLayerGroup; LayerIndex: Integer);
var
  DataArray: TDataArray;
begin
  if (GroupIndex <> PhastModel.LayerStructure.Count - 1)
    or (LayerIndex <> Group.ModflowLayerCount - 1) then
  begin
    DataArray := PhastModel.GetDataSetByName(StrVerticalConductance);
    Assert(DataArray <> nil);
    WriteArray(DataArray, Layer, 'Vcont');
  end;
end;

procedure TModflowBCF_Writer.WriteDataSet4(Layer: Integer;
  TransientModel: Boolean; AquiferType: integer);
var
  DataArray: TDataArray;
begin
  if TransientModel then
  begin
    DataArray := nil;
    case AquiferType of
      0, 2, 3:
        begin
          // confined storage coeficient
          DataArray := PhastModel.GetDataSetByName(StrConfinedStorageCoe);
        end;
      1:
        begin
          // specific yield
          DataArray := PhastModel.GetDataSetByName(rsSpecificYield);
        end;
    else
      Assert(False);
    end;
    Assert(DataArray <> nil);
    WriteArray(DataArray, Layer, 'Sf1');
  end;
end;

end.
