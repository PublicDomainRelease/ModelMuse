unit ModflowEVT_WriterUnit;

interface

uses SysUtils, Classes, Contnrs, RbwParser, CustomModflowWriterUnit,
  ScreenObjectUnit, ModflowBoundaryUnit, ModflowPackageSelectionUnit,
  OrderedCollectionUnit, ModflowCellUnit, PhastModelUnit,
  ModflowBoundaryDisplayUnit;

Type
  TModflowEVT_Writer = class(TCustomTransientArrayWriter)
  private
    NPEVT: integer;
    NEVTOP: integer;
    FDepthSurface: TList;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSets3And4;
    procedure WriteDataSets5To10;
    procedure WriteCells(CellList: TValueCellList; const DataSetIdentifier,
      VariableIdentifiers: string);
    procedure WriteEvapotranspirationSurface(CellList: TValueCellList);
    procedure WriteExtinctionDepth(CellList: TValueCellList);
  protected
    function CellType: TValueCellType; override;
    function Prefix: string; override;
    class function Extension: string; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    function Package: TModflowPackageSelection; override;
    function ParameterType: TParameterType; override;
    procedure WriteStressPeriods(const VariableIdentifiers, DataSetIdentifier,
      DS5, D7PNameIname, D7PName: string); override;
    procedure Evaluate; override;
  public
    Constructor Create(Model: TPhastModel); override;
    // @name destroys the current instance of @classname.
    Destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
  end;

implementation

uses ModflowUnitNumbers, ModflowTransientListParameterUnit,
  frmErrorsAndWarningsUnit, DataSetUnit, ModflowEvtUnit, GoPhastTypes, 
  frmProgressUnit;

{ TModflowEVT_Writer }

function TModflowEVT_Writer.CellType: TValueCellType;
begin
  result := TEvt_Cell
end;

constructor TModflowEVT_Writer.Create(Model: TPhastModel);
begin
  inherited;
  FDepthSurface := TObjectList.Create;
end;

destructor TModflowEVT_Writer.Destroy;
begin
  FDepthSurface.Free;
  inherited;
end;

procedure TModflowEVT_Writer.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TEvtBoundary;
begin
  inherited;
  for ScreenObjectIndex := 0 to PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := PhastModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowEvtBoundary;
    if Boundary <> nil then
    begin
      Boundary.GetEvapotranspirationLayerCells(FLayers);
      Boundary.GetEvapotranspirationSurfaceDepthCells(FDepthSurface);
    end;
  end;
end;

class function TModflowEVT_Writer.Extension: string;
begin
  result := '.evt';
end;

function TModflowEVT_Writer.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowEvtBoundary;
end;

function TModflowEVT_Writer.Package: TModflowPackageSelection;
begin
  result := PhastModel.ModflowPackages.EvtPackage;
end;

function TModflowEVT_Writer.ParameterType: TParameterType;
begin
  result := ptEVT;
end;

function TModflowEVT_Writer.Prefix: string;
begin
  result := 'ET'
end;

const
  ErrorRoot = 'One or more %s parameters have been eliminated '
    + 'because there are no cells associated with them.';

procedure TModflowEVT_Writer.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  List: TValueCellList;
  ParameterValues: TList;
  ParametersUsed: TStringList;
  TimeIndex: Integer;
  DepthSurfaceCellList: TValueCellList;
  EvapRateTimes: TModflowBoundaryDisplayTimeList;
  EvapotranspirationSurfaceTimes : TModflowBoundaryDisplayTimeList;
  EvapotranspirationDepthTimes : TModflowBoundaryDisplayTimeList;
  EvapotranspirationLayerTimes : TModflowBoundaryDisplayTimeList;
  NPEVT: integer;
  ParamDefArrays: TList;
  EvapRateArray: TModflowBoundaryDisplayDataArray;
  EvapSurfArray: TModflowBoundaryDisplayDataArray;
  EvapDepthArray: TModflowBoundaryDisplayDataArray;
  EvapLayerArray: TModflowBoundaryDisplayDataArray;
  DefArrayList: TList;
  Index: Integer;
  ATimeList: TModflowBoundaryDisplayTimeList;
const
  D7PNameIname = '';
  D7PName = '';
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;
  ParameterValues := TList.Create;
  try
    Evaluate;
    ParamDefArrays := TObjectList.Create;
    try
      EvaluateParameterDefinitions(ParamDefArrays, ErrorRoot);
      NPEVT := ParameterCount;
      NEVTOP := Ord(PhastModel.ModflowPackages.EvtPackage.LayerOption) + 1;
      EvapRateTimes := TimeLists[0];
      EvapotranspirationSurfaceTimes := TimeLists[1];
      EvapotranspirationDepthTimes := TimeLists[2];
      EvapotranspirationLayerTimes := TimeLists[3];

      if Values.Count = 0 then
      begin
        SetTimeListsUpToDate(TimeLists);
        Exit;
      end;
      for TimeIndex := 0 to Values.Count - 1 do
      begin
        EvapRateArray := EvapRateTimes[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        EvapSurfArray := EvapotranspirationSurfaceTimes[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        EvapDepthArray := EvapotranspirationDepthTimes[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        if EvapotranspirationLayerTimes = nil then
        begin
          EvapLayerArray := nil;
        end
        else
        begin
          EvapLayerArray := EvapotranspirationLayerTimes[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
        end;

        ParametersUsed := TStringList.Create;
        try
          RetrieveParametersForStressPeriod(D7PNameIname, D7PName, TimeIndex,
            ParametersUsed, ParameterValues);
          List := Values[TimeIndex];
          List.CheckRestore;

          // data set 6
          DepthSurfaceCellList := FDepthSurface[TimeIndex];
          DepthSurfaceCellList.CheckRestore;
          AssignTransient2DArray(EvapSurfArray, 0, DepthSurfaceCellList, 0,
            rdtDouble, umAssign);

          if NPEVT = 0 then
          begin
            // data set 7
            AssignTransient2DArray(EvapRateArray, 0, List, 0, rdtDouble, umAssign);
          end
          else
          begin
            // data set 8
            DefArrayList := ParamDefArrays[TimeIndex];
            UpdateTransient2DArray(EvapRateArray, DefArrayList);
          end;
          EvapRateArray.CacheData;

          // data set 9
          AssignTransient2DArray(EvapDepthArray, 1, DepthSurfaceCellList, 0,
            rdtDouble, umAssign);

          // data set 10
          if EvapLayerArray <> nil then
          begin
            if (PhastModel.ModflowPackages.EvtPackage.
              LayerOption = loSpecified)
              and not PhastModel.ModflowPackages.EvtPackage.
              TimeVaryingLayers then
            begin
              RetrieveParametersForStressPeriod(D7PNameIname, D7PName, 0,
                ParametersUsed, ParameterValues);
              List := Values[0];
            end;
            UpdateLayerDisplay(List, ParameterValues, TimeIndex,
              EvapLayerArray);
            EvapLayerArray.CacheData;
          end;
          List.Cache;
        finally
          ParametersUsed.Free;
        end;
      end;
      for Index := 0 to TimeLists.Count - 1 do
      begin
        ATimeList := TimeLists[Index];
        if ATimeList <> nil then
        begin
          ATimeList.SetUpToDate(True);
        end;
      end;
    finally
      ParamDefArrays.Free;
    end;
  finally
    ParameterValues.Free;
  end;
end;

procedure TModflowEVT_Writer.WriteDataSets3And4;
const
  DS3 = ' # Data Set 3: PARNAM PARTYP Parval NCLU';
  DS3Instances = ' INSTANCES NUMINST';
  DS4A = ' # Data Set 4a: INSTNAM';
  DataSetIdentifier = 'Data Set 4b:';
  VariableIdentifiers = 'Condfact';
begin
  WriteParameterDefinitions(DS3, DS3Instances, DS4A, DataSetIdentifier,
    VariableIdentifiers, ErrorRoot);
end;

procedure TModflowEVT_Writer.WriteDataSet1;
begin
  NPEVT := ParameterCount;
  if NPEVT > 0 then
  begin
    WriteString('PARAMETER');
    WriteInteger(NPEVT);
    WriteString(' # PARAMETER NPEVT');
    NewLine;
  end;
end;

procedure TModflowEVT_Writer.WriteDataSet2;
var
  IEVTCB: integer;
begin
  NEVTOP := Ord(PhastModel.ModflowPackages.EvtPackage.LayerOption) + 1;
  GetFlowUnitNumber(IEVTCB);

  WriteInteger(NEVTOP);
  WriteInteger(IEVTCB);
  WriteString(' # DataSet 2: NEVTOP IEVTCB');
  NewLine;
end;

procedure TModflowEVT_Writer.WriteDataSets5To10;
const
  D7PName =      ' # Data Set 8: PARNAM';
  D7PNameIname = ' # Data Set 8: PARNAM Iname';
  DS5 = ' # Data Set 5: INSURF INEVTR INEXDP INIEVT';
  DataSetIdentifier = 'Data Set 7:';
  VariableIdentifiers = 'EVTR';
begin
  WriteStressPeriods(VariableIdentifiers, DataSetIdentifier, DS5,
    D7PNameIname, D7PName);
end;

procedure TModflowEVT_Writer.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if PhastModel.PackageGeneratedExternally(StrEVT) then
  begin
    Exit;
  end;
//  frmProgress.AddMessage('Evaluating EVT Package data.');
  NameOfFile := FileName(AFileName);
  WriteToNameFile(StrEVT, PhastModel.UnitNumbers.UnitNumber(StrEVT),
    NameOfFile, foInput);
  Evaluate;
  OpenFile(FileName(AFileName));
  try
    frmProgress.AddMessage('Writing EVT Package input.');
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

    frmProgress.AddMessage('  Writing Data Sets 3 and 4.');
    WriteDataSets3And4;
    if not frmProgress.ShouldContinue then
    begin
      Exit;
    end;

    frmProgress.AddMessage('  Writing Data Sets 5 to 10.');
    WriteDataSets5To10;
  finally
    CloseFile;
//    Clear;
  end;
end;

procedure TModflowEVT_Writer.WriteCells(CellList: TValueCellList;
  const DataSetIdentifier, VariableIdentifiers: string);
var
  DefaultValue: double;
  DataType: TRbwDataType;
  DataTypeIndex: integer;
  Comment: string;
begin
  DefaultValue := 0;
  DataType := rdtDouble;
  DataTypeIndex := 0;
  Comment := DataSetIdentifier + ' ' + VariableIdentifiers;
  WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue, CellList);
end;

procedure TModflowEVT_Writer.WriteEvapotranspirationSurface(CellList: TValueCellList);
var
  DefaultValue: double;
  DataType: TRbwDataType;
  DataTypeIndex: integer;
  Comment: string;
begin
  DefaultValue := 0;
  DataType := rdtDouble;
  DataTypeIndex := 0;
  Comment := '# Data Set 6: SURF';
  WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue, CellList);
end;

procedure TModflowEVT_Writer.WriteExtinctionDepth(CellList: TValueCellList);
var
  DefaultValue: double;
  DataType: TRbwDataType;
  DataTypeIndex: integer;
  Comment: string;
begin
  DefaultValue := 0;
  DataType := rdtDouble;
  DataTypeIndex := 1;
  Comment := '# Data Set 9: EXDP';
  WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue, CellList);
end;

procedure TModflowEVT_Writer.WriteStressPeriods(const VariableIdentifiers,
  DataSetIdentifier, DS5, D7PNameIname, D7PName: string);
var
  NP: Integer;
  List: TValueCellList;
  ParameterValues: TValueCellList;
  ParamIndex: Integer;
  ParametersUsed: TStringList;
  TimeIndex: Integer;
  INEVTR, INIEVT: Integer;
  INSURF: Integer;
  INEXDP: Integer;
  DepthSurfaceCellList: TValueCellList;
  Comment: string;
begin
  ParameterValues := TValueCellList.Create(CellType);
  try
    ParameterValues.OwnsObjects := False;
    Comment := '# Data Set 10: IEVT';
    if Values.Count = 0 then
    begin
      frmErrorsAndWarnings.AddError('No evapotranspiration defined',
        'The Evapotranspiration package is active but '
        + 'no evapotranspiration has been defined for any stress period.');
    end;
    for TimeIndex := 0 to Values.Count - 1 do
    begin
      if not frmProgress.ShouldContinue then
      begin
        Exit;
      end;
      frmProgress.AddMessage('    Writing Stress Period ' + IntToStr(TimeIndex+1));
      ParametersUsed := TStringList.Create;
      try
        RetrieveParametersForStressPeriod(D7PNameIname, D7PName, TimeIndex,
          ParametersUsed, ParameterValues);
        NP := ParametersUsed.Count;
        List := Values[TimeIndex];
        // data set 5;
        INSURF := 1;
        if NPEVT > 0 then
        begin
          INEVTR := NP;
        end
        else
        begin
          INEVTR := 1;
        end;
        INEXDP := 1;
        if NEVTOP = 2 then
        begin
          INIEVT := 1;
        end
        else
        begin
          INIEVT := -1;
        end;

        WriteInteger(INSURF);
        WriteInteger(INEVTR);
        WriteInteger(INEXDP);
        WriteInteger(INIEVT);
        WriteString(DS5 + ' Stress period ' + IntToStr(TimeIndex+1));
        NewLine;
        if not frmProgress.ShouldContinue then
        begin
          List.Cache;
          Exit;
        end;

        // data set 6
        DepthSurfaceCellList := FDepthSurface[TimeIndex];
        WriteEvapotranspirationSurface(DepthSurfaceCellList);
        if not frmProgress.ShouldContinue then
        begin
          List.Cache;
          Exit;
        end;

        if NPEVT = 0 then
        begin
          // data set 7
          WriteCells(List, DataSetIdentifier, VariableIdentifiers);
        end
        else
        begin
          // data set 8
          for ParamIndex := 0 to ParametersUsed.Count - 1 do
          begin
            WriteString(ParametersUsed[ParamIndex]);
            NewLine;
          end;
        end;
        if not frmProgress.ShouldContinue then
        begin
          List.Cache;
          Exit;
        end;

        // data set 9
        WriteExtinctionDepth(DepthSurfaceCellList);
        if not frmProgress.ShouldContinue then
        begin
          List.Cache;
          Exit;
        end;

        // data set 10
        WriteLayerSelection(List, ParameterValues, TimeIndex, Comment);
        if not frmProgress.ShouldContinue then
        begin
          List.Cache;
          Exit;
        end;
        List.Cache;
      finally
        ParametersUsed.Free;
      end;
    end;
  finally
    ParameterValues.Free;
  end;
end;

end.
