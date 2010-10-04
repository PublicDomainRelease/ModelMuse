unit ZoneBudgetWriterUnit;

interface

uses
  CustomModflowWriterUnit, PhastModelUnit, ModflowPackageSelectionUnit,
  SysUtils, IntListUnit, Classes, DataSetUnit;

type
  TZoneBudgetZoneFileWriter = class(TCustomModflowWriter)
  private
    FZoneBudget: TZoneBudgetSelect;
    FUsedZones: TIntegerList;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure InitializeUsedZones;
    procedure CheckValidZoneNumbers;
    procedure ReportInvalidZonesInCompositeZones(MissingZones: TIntegerList; NAMCOMP: string);
  protected
    class function Extension: string; override;
  public
    Constructor Create(Model: TPhastModel); override;
    destructor Destroy; override;
    Procedure WriteU2DINTHeader(const Comment: string); override;
    procedure WriteFile(const AFileName: string);
  end;

  TZoneBudgetResponseFileWriter = class(TCustomModflowWriter)
  private
    FZoneBudget: TZoneBudgetSelect;
    FNameOfFile: string;
    procedure WriteResponse1;
    procedure WriteResponse2;
    procedure WriteResponse3;
    procedure WriteResponse4;
    procedure WriteResponse5;
  public
    class function Extension: string; override;
    Constructor Create(Model: TPhastModel); override;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  ModflowUnitNumbers, frmProgressUnit, frmErrorsAndWarningsUnit, Forms;

resourcestring
  StrZONEBUDGETZonesMus = 'ZONEBUDGET Zones must be  between 0 and 999 ' +
    'inclusive. The following zones are outside that range';
  StrSomeCompositeZones = 'Some composite zones contain numerical Zone values'
    + ' that are not included in the zone arrays.';
  StrTheNamesOfSomeZO = 'The names of some ZONEBUDGET composite zones appear' +
    ' more than once.';
  StrInTheFollowingZON = 'In the following ZONEBUDGET composite zones, a "0"' +
    ' appears before the end of the list of zones.  ZONEBUDGET will ignore all' +
    ' zones after zone 0';
  StrTheBudgetFileRequ = 'The budget file required by ZONEBUDGET is absent. '
    + 'Try running MODFLOW again.';

{ TZoneBudgetWriter }

constructor TZoneBudgetZoneFileWriter.Create(Model: TPhastModel);
begin
  inherited;
  FZoneBudget := Model.ModflowPackages.ZoneBudget;
  FUsedZones:= TIntegerList.Create;
end;

destructor TZoneBudgetZoneFileWriter.Destroy;
begin
  FUsedZones.Free;
  inherited;
end;

class function TZoneBudgetZoneFileWriter.Extension: string;
begin
  result := '.zb_zones';
end;

procedure TZoneBudgetZoneFileWriter.WriteDataSet1;
var
  NLAY: Integer;
  NROW: Integer;
  NCOL: Integer;
begin
  NLAY := PhastModel.LayerStructure.ModflowLayerCount;
  NROW := PhastModel.Grid.RowCount;
  NCOL := PhastModel.Grid.ColumnCount;
  WriteInteger(NLAY);
  WriteInteger(NROW);
  WriteInteger(NCOL);
  WriteString(' # Data set 1: NROW NROW NCOL');
  NewLine;
end;

procedure TZoneBudgetZoneFileWriter.WriteDataSet2;
var
  LayerIndex: Integer;
  DataArray: TDataArray;
begin
  DataArray := PhastModel.GetDataSetByName(StrZones);
  for LayerIndex := 0 to PhastModel.LayerStructure.LayerCount - 1 do
  begin
    if PhastModel.LayerStructure.IsLayerSimulated(LayerIndex) then
    begin
      WriteArray(DataArray, LayerIndex,
        'IZONE for layer ' + IntToStr(LayerIndex+1));
    end;
  end;
end;

procedure TZoneBudgetZoneFileWriter.WriteDataSet3;
var
  CompositeZoneIndex: Integer;
  CompositeZone: TCompositeZone;
  NAMCOMP: string;
  ZoneIndex: Integer;
  ICOMP: Integer;
  MissingZones: TIntegerList;
  DuplicateNames: TStringList;
  AllNames: TStringList;
  DupIndex: Integer;
begin
  InitializeUsedZones;
  CheckValidZoneNumbers;

  frmErrorsAndWarnings.RemoveErrorGroup(StrSomeCompositeZones);
  frmErrorsAndWarnings.RemoveErrorGroup(StrTheNamesOfSomeZO);
  frmErrorsAndWarnings.RemoveWarningGroup(StrInTheFollowingZON);

  AllNames:= TStringList.Create;
  DuplicateNames:= TStringList.Create;
  MissingZones := TIntegerList.Create;
  try
    DuplicateNames.Sorted := True;
    DuplicateNames.Duplicates := dupIgnore;
    for CompositeZoneIndex := 0 to FZoneBudget.CompositeZones.Count - 1 do
    begin
      CompositeZone := FZoneBudget.CompositeZones[
        CompositeZoneIndex].CompositeZone;
      NAMCOMP := CompositeZone.ZoneName;
      if AllNames.IndexOf(NAMCOMP) >= 0 then
      begin
        DuplicateNames.Add(NAMCOMP);
      end;
      AllNames.Add(NAMCOMP);

      WriteString(NAMCOMP + ' ');
      MissingZones.Clear;
      for ZoneIndex := 0 to CompositeZone.Count - 1 do
      begin
        ICOMP := CompositeZone[ZoneIndex].ZoneNumber;
        WriteInteger(ICOMP);
        if ICOMP > 0 then
        begin
          if FUsedZones.IndexOf(ICOMP) < 0 then
          begin
            MissingZones.Add(ICOMP);
          end;
        end
        else if (ICOMP = 0) and (ZoneIndex < CompositeZone.Count - 1) then
        begin
          frmErrorsAndWarnings.AddWarning(StrInTheFollowingZON, NAMCOMP);
        end;
             
      end;
      WriteInteger(0);
      WriteString(' # NAMCOMP, ICOMP');
      NewLine;
      ReportInvalidZonesInCompositeZones(MissingZones, NAMCOMP);
    end;
    for DupIndex := 0 to DuplicateNames.Count - 1 do
    begin
      frmErrorsAndWarnings.AddError(StrTheNamesOfSomeZO, DuplicateNames[DupIndex]);
    end;
  finally
    MissingZones.Free;
    DuplicateNames.Free;
    AllNames.Free;
  end;
end;

procedure TZoneBudgetZoneFileWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  if not FZoneBudget.IsSelected then
  begin
    Exit
  end;
  NameOfFile := FileName(AFileName);
  PhastModel.AddFileToArchive(NameOfFile);

  OpenFile(NameOfFile);
  try
    frmProgress.AddMessage('Writing ZONEBUDGET Zone File input.');
    frmProgress.AddMessage('  Writing Data Set 1.');
    WriteDataSet1;
    Application.ProcessMessages;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('Writing Zone File input.');
    frmProgress.AddMessage('  Writing Data Set 2.');
    WriteDataSet2;
    Application.ProcessMessages;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('Writing Zone File input.');
    frmProgress.AddMessage('  Writing Data Set 3.');
    WriteDataSet3;
    Application.ProcessMessages;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;
  finally
    CloseFile;
  end;
end;

procedure TZoneBudgetZoneFileWriter.ReportInvalidZonesInCompositeZones(MissingZones: TIntegerList; NAMCOMP: string);
var
  ErrorString: string;
  MissIndex: Integer;
begin
  if MissingZones.Count > 0 then
  begin
    ErrorString := '';
    for MissIndex := 0 to MissingZones.Count - 1 do
    begin
      ErrorString := ErrorString + IntToStr(MissingZones[MissIndex]) + ' ';
    end;
    frmErrorsAndWarnings.AddError(StrSomeCompositeZones, NAMCOMP + '; ' + Trim(ErrorString));
  end;
end;

procedure TZoneBudgetZoneFileWriter.CheckValidZoneNumbers;
var
  AValue: Integer;
  Index: Integer;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(StrZONEBUDGETZonesMus);
  for Index := 0 to FUsedZones.Count - 1 do
  begin
    AValue := FUsedZones[Index];
    if (AValue < 0) or (AValue > 999) then
    begin
      frmErrorsAndWarnings.AddError(StrZONEBUDGETZonesMus, IntToStr(AValue));
    end;
  end;
end;

procedure TZoneBudgetZoneFileWriter.InitializeUsedZones;
var
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  DataArray: TDataArray;
begin
  DataArray := PhastModel.GetDataSetByName(StrZones);
  DataArray.Initialize;
  FUsedZones.Sorted := True;
  for LayerIndex := 0 to PhastModel.LayerStructure.LayerCount - 1 do
  begin
    if PhastModel.LayerStructure.IsLayerSimulated(LayerIndex) then
    begin
      for RowIndex := 0 to DataArray.RowCount - 1 do
      begin
        for ColIndex := 0 to DataArray.ColumnCount - 1 do
        begin
          FUsedZones.AddUnique(DataArray.IntegerData[LayerIndex, RowIndex, ColIndex]);
        end;
      end;
    end;
  end;
  PhastModel.AddDataSetToCache(DataArray);
  PhastModel.CacheDataArrays;
end;

procedure TZoneBudgetZoneFileWriter.WriteU2DINTHeader(const Comment: string);
begin
  WriteString( 'INTERNAL () ');
  WriteInteger(IPRN_Integer);
  WriteString( ' # ' + Comment);
  NewLine;
end;

{ TZoneBudgetResponseFileWriter }

constructor TZoneBudgetResponseFileWriter.Create(Model: TPhastModel);
begin
  inherited;
  FZoneBudget := Model.ModflowPackages.ZoneBudget;
end;

class function TZoneBudgetResponseFileWriter.Extension: string;
begin
  result := '.zb_response'
end;

procedure TZoneBudgetResponseFileWriter.WriteFile(const AFileName: string);
begin
  if not FZoneBudget.IsSelected then
  begin
    Exit
  end;
  FNameOfFile := FileName(AFileName);
  PhastModel.AddFileToArchive(FNameOfFile);

  frmProgress.AddMessage('Writing ZONEBUDGET Response file.');
  OpenFile(FNameOfFile);
  try
    WriteResponse1;
    WriteResponse2;
    WriteResponse3;
    WriteResponse4;
    WriteResponse5;
  finally
    CloseFile;
  end;
end;

procedure TZoneBudgetResponseFileWriter.WriteResponse1;
var
  AFileName: string;
begin
  // Write the output file names and options.
  AFileName := ExtractFileName(FNameOfFile);
  if FZoneBudget.ExportZBLST
    and not FZoneBudget.ExportCSV
    and not FZoneBudget.ExportCSV2 then
  begin
    AFileName := ChangeFileExt(AFileName, '.zblst');
    WriteString(AFileName);
  end
  else
  begin
    AFileName := ChangeFileExt(AFileName, '');
    WriteString(AFileName);
    if FZoneBudget.ExportZBLST then
    begin
      WriteString(' ZBLST');
    end;
    if FZoneBudget.ExportCSV then
    begin
      WriteString(' CSV');
    end;
    if FZoneBudget.ExportCSV2 then
    begin
      WriteString(' CSV2');
    end;
  end;
  NewLine;
end;

procedure TZoneBudgetResponseFileWriter.WriteResponse2;
var
  AFileName: string;
begin
  // write the name of the budget file 
  frmErrorsAndWarnings.RemoveErrorGroup(StrTheBudgetFileRequ);
  AFileName := ChangeFileExt(FNameOfFile, '.cbc');
  if not FileExists(AFileName) then
  begin
    frmErrorsAndWarnings.AddError(StrTheBudgetFileRequ, AFileName);
  end;
  AFileName := ExtractFileName(AFileName);
  WriteString(AFileName);
  NewLine;
end;

procedure TZoneBudgetResponseFileWriter.WriteResponse3;
var
  ATitle: string;
begin
  // Use the first line of the comments as the title if there is one.
  if FZoneBudget.Comments.Count > 0 then
  begin
    ATitle := FZoneBudget.Comments[0];
  end
  else
  begin
    ATitle := PackageID_Comment(FZoneBudget);
  end;
  WriteString(ATitle);
  NewLine;
end;

procedure TZoneBudgetResponseFileWriter.WriteResponse4;
var
  AFileName: string;
begin
  // write the name of the zone file
  AFileName := ExtractFileName(FNameOfFile);
  AFileName := ChangeFileExt(AFileName,
    TZoneBudgetZoneFileWriter.Extension);
  WriteString(AFileName);
  NewLine;
end;

procedure TZoneBudgetResponseFileWriter.WriteResponse5;
begin
  // Compute budgets for all times.
  WriteString('A');
  NewLine;
end;




end.
