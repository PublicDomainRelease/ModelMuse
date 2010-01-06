unit ModflowLVDA_WriterUnit;

interface

uses Types, SysUtils, Classes, CustomModflowWriterUnit, ModflowPackageSelectionUnit;

type
  TModflowLVDA_Writer = class(TCustomPackageWriter)
  private
    FNameOfFile: string;
    procedure WriteDataSet0;
    procedure WriteDataSet1;
    procedure WriteDataSets2and3;
    function PackageID_Comment: string;
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  PhastModelUnit, OrderedCollectionUnit, ModflowParameterUnit, frmProgressUnit, 
  GoPhastTypes, frmErrorsAndWarningsUnit, ModflowUnitNumbers;

{ TModflowLVDA_Writer }

class function TModflowLVDA_Writer.Extension: string;
begin
  result := '.lvda';
end;

function TModflowLVDA_Writer.Package: TModflowPackageSelection;
begin
  result := PhastModel.ModflowPackages.HufPackage;
end;

function TModflowLVDA_Writer.PackageID_Comment: string;
begin
  result := 'LVDA file created on '
    + DateToStr(Now) + ' by ' + PhastModel.ProgramName + ' version '
    + ModelVersion + '.';
end;

procedure TModflowLVDA_Writer.WriteDataSet0;
begin
  WriteCommentLine(PackageID_Comment);
end;

procedure TModflowLVDA_Writer.WriteDataSet1;
var
  NPLVDA: Integer;
begin
  NPLVDA := PhastModel.ModflowSteadyParameters.CountParameters([ptHUF_LVDA]);
  WriteInteger(NPLVDA);
  WriteString(' # Data Set 1: NPLVDA');
  NewLine;
end;

procedure TModflowLVDA_Writer.WriteDataSets2and3;
var
  ParamIndex: Integer;
  Param: TModflowSteadyParameter;
  PARNAM: string;
  PARTYP: string;
  PARVAL: Double;
  LayerCount: Integer;
  NCLU: Integer;
  Clusters: TOneDIntegerArray;
  ClusterIndex: Integer;
  LAYER: Integer;
  UniformLayers: TBooleanDynArray;
  ZONARR: string;
  MLTARR: string;
  Error: string;
const
  IZ = 1;
begin
  LayerCount := PhastModel.LayerStructure.ModflowLayerCount;
  for ParamIndex := 0 to PhastModel.ModflowSteadyParameters.Count - 1 do
  begin
    Param := PhastModel.ModflowSteadyParameters.Items[ParamIndex];
    if Param.ParameterType = ptHUF_LVDA then
    begin
      if not frmProgress.ShouldContinue then
      begin
        Exit;
      end;

      PARNAM := Param.ParameterName;
      PARTYP := ' LVDA';
      PARVAL := Param.Value;

      if not Param.UseZone then
      begin
        NCLU := PhastModel.LayerStructure.ModflowLayerCount;
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
      frmProgress.AddMessage('  Writing Data Set 8 for parameter: ' + PARNAM);
      WriteString(PARNAM);
      WriteString(PARTYP);
      WriteFloat(PARVAL);
      WriteInteger(NCLU);
      WriteString(' # PARNAM, PARTYP, PARVAL, NCLU');
      NewLine;

      PhastModel.WritePValAndTemplate(PARNAM,PARVAL);

      // Data set 9
      frmProgress.AddMessage('  Writing Data Set 9 for parameter: ' + PARNAM);
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
        WriteString(MLTARR);
        WriteString(ZONARR);
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
      PhastModel.CacheDataArrays;
    end;
  end;
end;

procedure TModflowLVDA_Writer.WriteFile(const AFileName: string);
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if PhastModel.PackageGeneratedExternally(StrLVDA) then
  begin
    Exit;
  end;
  if PhastModel.ModflowSteadyParameters.CountParameters([ptHUF_LVDA]) = 0 then
  begin
    Exit;
  end;

  FNameOfFile := FileName(AFileName);
  WriteToNameFile(StrLVDA, PhastModel.UnitNumbers.UnitNumber(StrLVDA),
    FNameOfFile, foInput);
  OpenFile(FNameOfFile);
  try
    frmProgress.AddMessage('Writing LVDA Package input.');
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
    WriteDataSets2and3;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

  finally
    CloseFile;
  end;
end;

end.
