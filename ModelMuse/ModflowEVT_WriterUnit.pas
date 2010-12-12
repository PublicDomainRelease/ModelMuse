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
    Constructor Create(Model: TCustomModel); override;
    // @name destroys the current instance of @classname.
    Destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
  end;

implementation

uses ModflowUnitNumbers, ModflowTransientListParameterUnit,
  frmErrorsAndWarningsUnit, DataSetUnit, ModflowEvtUnit, GoPhastTypes, 
  frmProgressUnit, Forms;

{ TModflowEVT_Writer }

function TModflowEVT_Writer.CellType: TValueCellType;
begin
  result := TEvt_Cell
end;

constructor TModflowEVT_Writer.Create(Model: TCustomModel);
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
      Boundary.EvtSurfDepthCollection.ClearBoundaries;
      Boundary.EvapotranspirationLayers.ClearBoundaries;
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
  EtSurfaceError = 'The ET Surface is undefined in the EVT package.';
  EtSurfaceErrorMessage = 'No objects define the ET Surface in the EVT package.';
  EtDepthError = 'The ET Depth is undefined in the EVT package.';
  EtDepthErrorMessage = 'No objects define the ET Depth in the EVT package.';

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
  frmErrorsAndWarnings.RemoveErrorGroup(EtSurfaceError);
  frmErrorsAndWarnings.RemoveErrorGroup(EtDepthError);
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;
  ParameterValues := TList.Create;
  try
    Evaluate;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    ClearTimeLists;
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
            ParametersUsed, ParameterValues, True);
          List := Values[TimeIndex];
          List.CheckRestore;

          // data set 6
          if FDepthSurface.Count > 0 then
          begin
            DepthSurfaceCellList := FDepthSurface[TimeIndex];
            DepthSurfaceCellList.CheckRestore;
            AssignTransient2DArray(EvapSurfArray, 0, DepthSurfaceCellList, 0,
              rdtDouble, umAssign);
          end
          else
          begin
            DepthSurfaceCellList := nil;
            EvapSurfArray.UpToDate := True;
            frmErrorsAndWarnings.AddError(EtSurfaceError, EtSurfaceErrorMessage);
          end;
          EvapSurfArray.CacheData;

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
          if DepthSurfaceCellList <> nil then
          begin
            AssignTransient2DArray(EvapDepthArray, 1, DepthSurfaceCellList, 0,
              rdtDouble, umAssign);
          end
          else
          begin
            EvapDepthArray.UpToDate := True;
            frmErrorsAndWarnings.AddError(EtDepthError, EtDepthErrorMessage);
          end;
          EvapDepthArray.CacheData;

          // data set 10
          if EvapLayerArray <> nil then
          begin
            if (PhastModel.ModflowPackages.EvtPackage.
              LayerOption = loSpecified)
              and not PhastModel.ModflowPackages.EvtPackage.
              TimeVaryingLayers and (ParameterCount > 0)  then
            begin
              List.Cache;
              RetrieveParametersForStressPeriod(D7PNameIname, D7PName, 0,
                ParametersUsed, ParameterValues, True);
              List := Values[0];
              List.CheckRestore;
            end;
            UpdateLayerDisplay(List, ParameterValues, TimeIndex,
              EvapLayerArray);
            EvapLayerArray.CacheData;
          end;
          List.Cache;
          if DepthSurfaceCellList <> nil then
          begin
            DepthSurfaceCellList.Cache;
          end;
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
  D7PName =      ' # Data Set 8: PARNAM IEVTPF';
  D7PNameIname = ' # Data Set 8: PARNAM Iname IEVTPF';
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
  frmErrorsAndWarnings.RemoveErrorGroup(EtSurfaceError);
  frmErrorsAndWarnings.RemoveErrorGroup(EtDepthError);
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
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  ClearTimeLists;
  OpenFile(FileName(AFileName));
  try
    frmProgressMM.AddMessage('Writing EVT Package input.');
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

    frmProgressMM.AddMessage('  Writing Data Sets 3 and 4.');
    WriteDataSets3And4;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Sets 5 to 10.');
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
  Dummy: TDataArray;
begin
  DefaultValue := 0;
  DataType := rdtDouble;
  DataTypeIndex := 0;
  Comment := DataSetIdentifier + ' ' + VariableIdentifiers;
  WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
    CellList, Dummy);
end;

procedure TModflowEVT_Writer.WriteEvapotranspirationSurface(CellList: TValueCellList);
var
  DefaultValue: double;
  DataType: TRbwDataType;
  DataTypeIndex: integer;
  Comment: string;
  Dummy: TDataArray;
begin
  DefaultValue := 0;
  DataType := rdtDouble;
  DataTypeIndex := 0;
  Comment := 'Data Set 6: SURF';
  WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
    CellList, Dummy);
end;

procedure TModflowEVT_Writer.WriteExtinctionDepth(CellList: TValueCellList);
var
  DefaultValue: double;
  DataType: TRbwDataType;
  DataTypeIndex: integer;
  Comment: string;
  Dummy: TDataArray;
begin
  DefaultValue := 0;
  DataType := rdtDouble;
  DataTypeIndex := 1;
  Comment := 'Data Set 9: EXDP';
  WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
    CellList, Dummy);
end;

procedure TModflowEVT_Writer.WriteStressPeriods(const VariableIdentifiers,
  DataSetIdentifier, DS5, D7PNameIname, D7PName: string);
var
  NP: Integer;
  EtRateList, PriorEtRateList: TValueCellList;
  ParameterValues: TValueCellList;
  ParamIndex: Integer;
  ParametersUsed: TStringList;
  TimeIndex: Integer;
  INEVTR, INIEVT: Integer;
  INSURF: Integer;
  INEXDP: Integer;
  DepthSurfaceCellList, PriorListDepthSurfaceCellList: TValueCellList;
  Comment: string;
begin
  inherited;
  ParameterValues := TValueCellList.Create(CellType);
  try
    ParameterValues.OwnsObjects := False;
    Comment := 'Data Set 10: IEVT';
    if Values.Count = 0 then
    begin
      frmErrorsAndWarnings.AddError('No evapotranspiration defined',
        'The Evapotranspiration package is active but '
        + 'no evapotranspiration has been defined for any stress period.');
    end;
    for TimeIndex := 0 to Values.Count - 1 do
    begin
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      frmProgressMM.AddMessage('    Writing Stress Period ' + IntToStr(TimeIndex+1));
      ParametersUsed := TStringList.Create;
      try
        RetrieveParametersForStressPeriod(D7PNameIname, D7PName, TimeIndex,
          ParametersUsed, ParameterValues, True);
        NP := ParametersUsed.Count;
        EtRateList := Values[TimeIndex];
        // data set 5;
        if (TimeIndex > 0) and (FDepthSurface.Count > 0) then
        begin
          PriorListDepthSurfaceCellList := FDepthSurface[TimeIndex-1];
          DepthSurfaceCellList := FDepthSurface[TimeIndex];
          if PriorListDepthSurfaceCellList.AreRealValuesIdentical(DepthSurfaceCellList, 0) then
          begin
            INSURF := -1;
//            DepthSurfaceCellList.Cache;
          end
          else
          begin
            INSURF := 1;
          end;
          PriorListDepthSurfaceCellList.Cache;
        end
        else
        begin
          INSURF := 1;
        end;

        if NPEVT > 0 then
        begin
          INEVTR := NP;
        end
        else
        begin
          if (TimeIndex > 0) then
          begin
            PriorEtRateList := Values[TimeIndex-1];
            if PriorEtRateList.AreRealValuesIdentical(EtRateList, 0) then
            begin
              INEVTR := -1;
//              EtRateList.Cache;
            end
            else
            begin
              INEVTR := 1;
            end;
            PriorEtRateList.Cache;
          end
          else
          begin
            INEVTR := 1;
          end;
        end;

        if (TimeIndex > 0) and (FDepthSurface.Count > 0) then
        begin
          PriorListDepthSurfaceCellList := FDepthSurface[TimeIndex-1];
          DepthSurfaceCellList := FDepthSurface[TimeIndex];
          if PriorListDepthSurfaceCellList.AreRealValuesIdentical(DepthSurfaceCellList, 1) then
          begin
            INEXDP := -1;
//            DepthSurfaceCellList.Cache;
          end
          else
          begin
            INEXDP := 1;
          end;
          PriorListDepthSurfaceCellList.Cache;
        end
        else
        begin
          INEXDP := 1;
        end;

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
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          EtRateList.Cache;
          Exit;
        end;

        // data set 6
        if INSURF > 0 then
        begin
          if FDepthSurface.Count > 0 then
          begin
            DepthSurfaceCellList := FDepthSurface[TimeIndex];
            WriteEvapotranspirationSurface(DepthSurfaceCellList);
            Application.ProcessMessages;
            if not frmProgressMM.ShouldContinue then
            begin
              EtRateList.Cache;
              Exit;
            end;
            if (INEXDP < 0) and (TimeIndex = Values.Count - 1) then
            begin
              DepthSurfaceCellList.Cache;
            end;
          end
          else
          begin
            DepthSurfaceCellList := nil;
            frmErrorsAndWarnings.AddError(EtSurfaceError, EtSurfaceErrorMessage);
          end;

        end;
        if NPEVT = 0 then
        begin
          // data set 7
          if INEVTR > 0 then
          begin
            WriteCells(EtRateList, DataSetIdentifier, VariableIdentifiers);
          end;
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
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          EtRateList.Cache;
          Exit;
        end;

        // data set 9
        if INEXDP > 0 then
        begin
          if FDepthSurface.Count > 0 then
          begin
            DepthSurfaceCellList := FDepthSurface[TimeIndex];
            WriteExtinctionDepth(DepthSurfaceCellList);
            Application.ProcessMessages;
            if not frmProgressMM.ShouldContinue then
            begin
              EtRateList.Cache;
              Exit;
            end;
            if  (TimeIndex = Values.Count - 1) then
            begin
              DepthSurfaceCellList.Cache;
            end;
          end
          else
          begin
            frmErrorsAndWarnings.AddError(EtDepthError, EtDepthErrorMessage);
          end;
        end;

        // data set 10
        WriteLayerSelection(EtRateList, ParameterValues, TimeIndex, Comment);
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          EtRateList.Cache;
          Exit;
        end;
        EtRateList.Cache;
      finally
        ParametersUsed.Free;
      end;
    end;
  finally
    ParameterValues.Free;
  end;
end;

end.

