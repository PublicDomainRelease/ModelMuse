unit ModpathMainFileWriterUnit;

interface

uses SysUtils, CustomModflowWriterUnit, DataSetUnit;

type
  TModpathMainFileWriter = class(TCustomModflowWriter)
  private
    XSECTION: Boolean;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSet(const DataSetName: string; DataArray: TDataArray);
    procedure WriteDataSet4;
    procedure WriteDataSet5;
    procedure WriteDataSet6a;
    procedure WriteDataSet6b;
  public
    class function Extension: string; override;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  frmErrorsAndWarningsUnit, LayerStructureUnit, PhastModelUnit;

{ TModpathMainFileWriter }

class function TModpathMainFileWriter.Extension: string;
begin
  result := '.mpm';
end;

procedure TModpathMainFileWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  NameOfFile := FileName(AFileName);
  OpenFile(NameOfFile);
  try
    WriteDataSet1;
    WriteDataSet2;
    WriteDataSet3;
    WriteDataSet4;
    WriteDataSet5;
    WriteDataSet6a;
    WriteDataSet6b;
  finally
    CloseFile;
  end;
end;

procedure TModpathMainFileWriter.WriteDataSet6a;
var
  TBEGIN: Double;
  FirstTime: Double;
  ReferenceTime: Real;
begin
  ReferenceTime := PhastModel.ModflowPackages.ModPath.ReferenceTime;
  FirstTime := PhastModel.ModflowFullStressPeriods.Items[0].StartTime;
  TBEGIN := FirstTime - ReferenceTime;
  WriteFloat(TBEGIN);
  WriteString(' # Data Set 6A: TBEGIN');
  NewLine;
end;

procedure TModpathMainFileWriter.WriteDataSet6b;
var
  BeginPeriod, BeginStep, EndPeriod, EndStep: integer;
  ATime: Real;
begin
    if PhastModel.ModflowStressPeriods.CompletelyTransient then
    begin
      ATime := PhastModel.ModflowPackages.ModPath.BeginningTime;
      PhastModel.ModflowFullStressPeriods.TimeToPeriodAndStep(
        ATime, BeginPeriod, BeginStep);
      Inc(BeginPeriod);
      Inc(BeginStep);
    end
    else
    begin
      BeginPeriod := 1;
      BeginStep := 1;
    end;
    ATime := PhastModel.ModflowPackages.ModPath.EndingTime;
    PhastModel.ModflowFullStressPeriods.TimeToPeriodAndStep(
      ATime, EndPeriod, EndStep);

    Inc(EndPeriod);
    Inc(EndStep);
    WriteInteger(BeginPeriod);
    WriteInteger(BeginStep);
    WriteInteger(EndPeriod);
    WriteInteger(EndStep);
    WriteString(' # Data set 6B: BeginPeriod, BeginStep, EndPeriod, EndStep');
    NewLine;
end;

procedure TModpathMainFileWriter.WriteDataSet5;
var
  LayerIndex: Integer;
  LayerGroup: TLayerGroup;
  Index: Integer;
  Layer: Integer;
  LayerCount: Integer;
  PorosityArray: TDataArray;
begin
  PorosityArray := PhastModel.DataArrayManager.GetDataSetByName(rsPorosity);
  LayerCount := 0;
  Layer := -1;
  for Index := 1 to PhastModel.LayerStructure.Count - 1 do
  begin
    LayerGroup := PhastModel.LayerStructure[Index];
    if LayerGroup.Simulated then
    begin
      for LayerIndex := 0 to LayerGroup.ModflowLayerCount - 1 do
      begin
        Inc(LayerCount);
        Inc(Layer);
        WriteArray(PorosityArray, Layer, 'Data Set 5: POR' + ' Layer '
          + IntToStr(LayerCount) + ': '
          + PhastModel.LayerStructure.ModflowLayerBottomDescription(LayerCount));
      end;
    end
    else
    begin
      Inc(Layer);
      WriteArray(PorosityArray, Layer, 'Data Set 5: PorCB' + ' Layer '
        + IntToStr(LayerCount) + ': '
        + PhastModel.LayerStructure.ModflowLayerBottomDescription(LayerCount));
    end;
  end;
end;

procedure TModpathMainFileWriter.WriteDataSet4;
var
  ZoneDataArray: TDataArray;
begin
  ZoneDataArray := PhastModel.DataArrayManager.GetDataSetByName(StrModpathZone);
  WriteDataSet('Data Set 4: IBOUND', ZoneDataArray);
end;

// copied from TModflowBasicWriter
procedure TModpathMainFileWriter.WriteDataSet(const DataSetName: string;
  DataArray: TDataArray);
var
  LayerIndex: Integer;
begin
  Assert(DataArray <> nil);
  if XSECTION then
  begin
    WriteCrossSectionArray(DataArray, DataSetName);
  end
  else
  begin
    for LayerIndex := 0 to PhastModel.ModflowGrid.LayerCount - 1 do
    begin
      if PhastModel.LayerStructure.IsLayerSimulated(LayerIndex) then
      begin
        WriteArray(DataArray, LayerIndex, DataSetName + ' '
          + PhastModel.LayerStructure.ModflowLayerBottomDescription(LayerIndex));
      end;
    end;
  end;
end;


procedure TModpathMainFileWriter.WriteDataSet3;
var
  LayerIndex: Integer;
  LAYCON: Integer;
  LayerGroup: TLayerGroup;
  Index: Integer;
  LayerCount: Integer;
begin
  LayerCount := 0;
  for Index := 1 to PhastModel.LayerStructure.Count - 1 do
  begin
    LayerGroup := PhastModel.LayerStructure[Index];
    if LayerGroup.Simulated then
    begin
      LAYCON := LayerGroup.AquiferType;
      for LayerIndex := 0 to LayerGroup.ModflowLayerCount - 1 do
      begin
        WriteInteger(LAYCON);
        Inc(LayerCount);
        if (LayerCount mod 10) = 0 then
        begin
          NewLine;
        end;
      end;
    end;
  end;
  if (LayerCount mod 10) <> 0 then
  begin
    NewLine;
  end;
end;

procedure TModpathMainFileWriter.WriteDataSet2;
var
  Option: string;
begin
  XSECTION := PhastModel.ModflowGrid.RowCount = 1;
  Option := '';
  if XSECTION then
  begin
    Option := Option + ' XSECTION';
  end;
  if PhastModel.ModflowPackages.ModPath.Compact then
  begin
    Option := Option + ' COMPACT';
  end;
  if PhastModel.ModflowPackages.ModPath.Binary then
  begin
    Option := Option + ' BINARY';
  end;
  case PhastModel.ModflowOptions.LengthUnit of
    0:
      // undefined
      begin
        frmErrorsAndWarnings.AddWarning('Undefined length units', 'The length units of the model are undefined. MODPATH-PLOT ' + 'will treat the units as feet');
      end;
    1:
      // feet
      begin
      end;
    2:
      // meters
      begin
        Option := Option + ' METERS';
      end;
    3:
      // centimeters
      begin
        frmErrorsAndWarnings.AddWarning('Unsupported length units for MODPATH-PLOT', 'The length units of the model are centimeters. MODPATH-PLOT ' + 'will treat the units as feet');
      end;
  else
    Assert(False);
  end;
  Option := Trim(Option + ' # Data Set 2: OPTION');
  WriteString(Option);
  NewLine;
end;

procedure TModpathMainFileWriter.WriteDataSet1;
var
  MAXSIZ: Integer;
  HNOFLO: Real;
  HDRY: Real;
  NPART: Integer;
  IRCHTP: Integer;
  IEVTTP: Integer;
begin
  MAXSIZ := PhastModel.ModflowPackages.ModPath.MaximumSize;
  HNOFLO := PhastModel.ModflowOptions.HNoFlow;
  HDRY := PhastModel.ModflowOptions.HDry;
  NPART := 0;
  IRCHTP := Ord(PhastModel.ModflowPackages.ModPath.RCH_Source);
  IEVTTP := Ord(PhastModel.ModflowPackages.ModPath.EVT_Sink);
  WriteInteger(MAXSIZ);
  WriteFloat(HNOFLO);
  WriteFloat(HDRY);
  WriteInteger(NPART);
  WriteInteger(IRCHTP);
  WriteInteger(IEVTTP);
  WriteString(' # Data Set 1: MAXSIZ HNOFLO  HDRY  NPART  IRCHTP  IEVTTP');
  NewLine;
end;

end.
