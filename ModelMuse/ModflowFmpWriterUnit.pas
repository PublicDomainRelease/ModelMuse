unit ModflowFmpWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowPackageSelectionUnit, ModflowCellUnit,
  ModflowBoundaryUnit, OrderedCollectionUnit, ScreenObjectUnit, GoPhastTypes,
  ModflowFmpWellUnit, PhastModelUnit, ModflowFmpFarmUnit,
  Generics.Collections, ModflowFmpCropUnit;

  {$IFDEF FMP}
type
  TModflowFmpWriter = class(TCustomListWriter)
  private
    FFarmProcess: TFarmProcess;
    FNameOfFile: string;
    NPFWL: Integer;
    MXL: Integer;
    FFarmWellID: Integer;
    FFarms: TFarmList;
    IRTFL: Integer;
    IEFFL: Integer;
    NCROPS: Integer;
    procedure WriteDataSet1;
    procedure WriteDataSet2a;
    procedure WriteDataSet2cParamDimen;
    procedure WriteDataSet2cWhenToRead;
    procedure WriteDataSet2cCropConsumptiveUse;
    procedure WriteDataSets3And4;
    procedure WriteDataSet5;
    procedure WriteDataSet6;
    procedure WriteDataSet7;
    procedure WriteDataSet8;
    procedure WriteDataSet9;
    procedure WriteDataSet11;
    procedure WriteDataSets21to36;
    function NumberOfFarms: Integer;
    function GetEfficiencyFlag: integer;
    function GetRootingDepthFlag: integer;
    procedure FillFarmList;
    procedure WriteValueFromGlobalFormula(Formula: string;
      ErrorObject: TObject; const DataSetErrorString: string);
  protected
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
//    procedure Evaluate; override;
    function CellType: TValueCellType; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    procedure WriteStressPeriods(const VariableIdentifiers, DataSetIdentifier,
      DS5, D7PNameIname, D7PName: string); override;
    function ParameterType: TParameterType; override;
    procedure WriteParameterCells(CellList: TValueCellList; NLST: Integer;
      const VariableIdentifiers, DataSetIdentifier: string;
      AssignmentMethod: TUpdateMethod;
      MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); override;
//    procedure WriteParameterDefinitions(const DS3, DS3Instances, DS4A,
//      DataSetIdentifier, VariableIdentifiers, ErrorRoot: string;
//      AssignmentMethod: TUpdateMethod;
//      MultiplierArrayNames: TTransientMultCollection;
//      ZoneArrayNames: TTransientZoneCollection); override;
    procedure WriteCell(Cell: TValueCell;
      const DataSetIdentifier, VariableIdentifiers: string); override;
  public
    // @name creates and instance of @classname.
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
  end;
  {$ENDIF}

implementation

{$IFDEF FMP}
uses
  ModflowUnitNumbers, Forms, frmProgressUnit, DataSetUnit, RbwParser,
  frmFormulaErrorsUnit, ModflowFmpSoilUnit, SysUtils;

resourcestring
  StrWritingDataSet2a = '  Writing Data Set 2a.';
  StrWritingDataSet2c = '  Writing Data Set 2c.';
  StrWritingDataSets3and4 = '  Writing Data Sets 3 and 4.';
  StrWritingDataSets = '  Writing Data Sets 21 to 36.';
  StrSoilS = 'Soil: %s';
  StrCapillaryFringe = 'Capillary Fringe';
  StrRootingDepth = 'Rooting Depth';
  StrCropS = 'Crop: %s';

{ TModflowFmpWriter }

function TModflowFmpWriter.CellType: TValueCellType;
begin
  result := TFmpWell_Cell;
end;

constructor TModflowFmpWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FFarmProcess := Model.ModflowPackages.FarmProcess;
  FFarms := TFarmList.Create;
end;

destructor TModflowFmpWriter.Destroy;
begin
  FFarms.Free;
  inherited;
end;

class function TModflowFmpWriter.Extension: string;
begin
  Result := '.fmp';
end;

function TModflowFmpWriter.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowFmpWellBoundary;
end;

function TModflowFmpWriter.GetEfficiencyFlag: integer;
var
  FarmIndex: Integer;
  AFarm: TFarm;
  ACropEfficiency: TFarmEfficienciesItem;
begin
  result := 1;
  for FarmIndex := 0 to FFarms.Count - 1 do
  begin
    AFarm := FFarms[FarmIndex];
    if AFarm.FarmEfficiencyCollection.Count > 0 then
    begin
      // @ACropEfficiency represents the crop efficiencies for one crop
      // for one farm over
      // multiple time periods. FMP Data sets 7 or 24.
      // The times will be the same for all the different crops at one farm.
      ACropEfficiency := AFarm.FarmEfficiencyCollection[0];
      if ACropEfficiency.CropEfficiency.Count > 1 then
      begin
        result := 2;
        Exit;
      end;
    end;
  end;
end;

function TModflowFmpWriter.GetRootingDepthFlag: integer;
var
  Crops: TCropCollection;
  CropIndex: Integer;
  RootDepths: TFmpRootDepthCollection;
begin
  result := 0;
  case FFarmProcess.RootingDepth of
    rdSpecified:
      begin
        result := 1;
        Crops := (Model as TPhastModel).FmpCrops;
        for CropIndex := 0 to Crops.Count - 1 do
        begin
          RootDepths := Crops[CropIndex].FmpRootDepthCollection;
          if RootDepths.Count > 1 then
          begin
            result := 2;
            Exit;
          end;
        end;
      end;
    rdCalculated:
      begin
        result := 3;
      end;
    else
      Assert(False);
  end;
end;

function TModflowFmpWriter.NumberOfFarms: Integer;
begin
  result := FFarms.Count;
end;

function TModflowFmpWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.FarmProcess;
end;

function TModflowFmpWriter.ParameterType: TParameterType;
begin
  result := ptQMAX;
end;

procedure TModflowFmpWriter.WriteCell(Cell: TValueCell; const DataSetIdentifier,
  VariableIdentifiers: string);
var
  Well_Cell: TFmpWell_Cell;
  LocalLayer: integer;
  AuxValue: integer;
  AuxName: string;
begin
  Well_Cell := Cell as TFmpWell_Cell;
  LocalLayer := Model.
    DataSetLayerToModflowLayer(Well_Cell.Layer);
  WriteInteger(LocalLayer);
  WriteInteger(Well_Cell.Row+1);
  WriteInteger(Well_Cell.Column+1);
  WriteInteger(FFarmWellID);
  WriteInteger(Well_Cell.FarmID);
  WriteFloat(Well_Cell.MaxPumpingRate);
  Inc(FFarmWellID);

//  Skip QMAXRESET because MNW1 is not supported.

  AuxName := '';
  case FFarmProcess.CropIrrigationRequirement of
    cirOnlyWhenNeeded:
      begin
        if Well_Cell.PumpOnlyIfCropRequiresWater then
        begin
          AuxValue := 1;
        end
        else
        begin
          AuxValue := 0;
        end;
        WriteInteger(AuxValue);
        AuxName := 'AUX-NOCIRNOQ';
      end;
    cirContinuously: ;// do nothing
    else Assert(False);
  end;

  WriteIface(Well_Cell.IFace);
  WriteString(' # ' + DataSetIdentifier + ' Layer Row Column Farm-Well-ID Farm ID '
    + VariableIdentifiers + AuxName + ' IFACE');
//  // The annotation identifies the object used to define the well.
//  // This can be helpful in identifying when used with PEST.
//  WriteString(Well_Cell.PumpingRateAnnotation);
  NewLine;
end;

procedure TModflowFmpWriter.WriteDataSet1;
begin
  CountParametersAndParameterCells(NPFWL, MXL);
  if NPFWL > 0 then
  begin
    WriteString('PARAMETER');
    WriteInteger(NPFWL);
    WriteInteger(MXL);
    WriteString(' # DataSet 1: PARAMETER NPFWL MXL');
    NewLine;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet11;
var
  Crops: TCropCollection;
  CropIndex: Integer;
  RootDepths: TFmpRootDepthCollection;
  CropID: Integer;
  RootItem: TRootingDepthItem;
begin
  if (IRTFL = 1) then
  begin
    Crops := (Model as TPhastModel).FmpCrops;
    for CropIndex := 0 to Crops.Count - 1 do
    begin
      RootDepths := Crops[CropIndex].FmpRootDepthCollection;
      Assert(RootDepths.Count = 1);
      CropID := CropIndex + 1;
      RootItem := RootDepths[0];
      WriteInteger(CropID);
      WriteValueFromGlobalFormula(RootItem.RootingDepth,
        Crops[CropIndex], StrRootingDepth);
      WriteString(' # Data set 11: Crop-ID ROOT');
      NewLine;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet2a;
begin
  WriteString('FLAG_BLOCKS # DataSet 2a');
  NewLine;
end;

procedure TModflowFmpWriter.WriteDataSet2cCropConsumptiveUse;
var
  ICCFL: Integer;
begin
  ICCFL := 0;
  case FFarmProcess.CropConsumptiveLinkage of
    cclNotLinked:
      begin
        case FFarmProcess.CropConsumptiveConcept of
          cccConcept1:
            begin
              ICCFL := 1;
            end;
          cccConcept2:
            begin
              ICCFL := 2;
            end;
        end;
      end;
    cclLinked:
      begin
        case FFarmProcess.CropConsumptiveConcept of
          cccConcept1:
            begin
              ICCFL := 3;
            end;
          cccConcept2:
            begin
              ICCFL := 4;
            end;
        end;
      end;
  end;
  WriteInteger(ICCFL);
  WriteString(' # Data Set 2c: ICCFL');
  NewLine;
end;

procedure TModflowFmpWriter.WriteDataSet2cParamDimen;
var
  MXACTW: Integer;
  NFARMS: Integer;
  NSOILS: Integer;
  LocalModel: TPhastModel;
begin
  CountCells(MXACTW);
  NFARMS := NumberOfFarms;
  LocalModel := Model as TPhastModel;
  NCROPS := LocalModel.FmpCrops.Count;
  NSOILS := LocalModel.FmpSoils.Count;

  WriteInteger(MXACTW);
  WriteInteger(NFARMS);
  WriteInteger(NCROPS);
  WriteInteger(NSOILS);
  WriteString(' # Data Set 2c: MXACTW NFARMS NCROPS NSOILS');
  NewLine
end;

procedure TModflowFmpWriter.WriteDataSet2cWhenToRead;
var
  ICUFL, IPFL, IFTEFL, IIESWFL: integer;
begin
  IRTFL := GetRootingDepthFlag;
  IEFFL := GetEfficiencyFlag;
  { TODO -cFMP : Set the rest of the FMP variables }

  WriteInteger(IRTFL);
  WriteInteger(ICUFL);
  WriteInteger(IPFL);
  WriteInteger(IFTEFL);
  WriteInteger(IIESWFL);
  WriteInteger(IEFFL);
  WriteString(' # Data Set 2c: IRTFL ICUFL IPFL IFTEFL IIESWFL IEFFL');
  NewLine;
end;

procedure TModflowFmpWriter.WriteDataSets21to36;
const
  D7PName =      ' # Data Set 23: PARNAM';
  D7PNameIname = ' # Data Set 23: PARNAM Iname';
  DS5 = ' # Data Set 21: ITMP NP';
  DataSetIdentifier = 'Data Set 22:';
  VariableIdentifiers = 'QMAX';
begin
  WriteStressPeriods(VariableIdentifiers, DataSetIdentifier, DS5,
    D7PNameIname, D7PName);
end;

procedure TModflowFmpWriter.WriteDataSets3And4;
const
  ErrorRoot = 'One or more %s parameters have been eliminated '
    + 'because there are no cells associated with them.';
  DS3 = ' # Data Set 3: PARNAM PARTYP Parval NLST';
  DS3Instances = ' INSTANCES NUMINST';
  DS4A = ' # Data Set 4a: INSTNAM';
  DataSetIdentifier = 'Data Set 4b:';
  VariableIdentifiers = 'QMAXfact';
begin
  WriteParameterDefinitions(DS3, DS3Instances, DS4A, DataSetIdentifier,
    VariableIdentifiers, ErrorRoot, umAssign, nil, nil);
end;

procedure TModflowFmpWriter.WriteFile(const AFileName: string);
begin
  if (not Package.IsSelected) or not (Model.ModelSelection = msModflowFmp) then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrFMP) then
  begin
    Exit;
  end;
  FNameOfFile := FileName(AFileName);
  WriteToNameFile(StrFMP, Model.UnitNumbers.UnitNumber(StrFMP),
    FNameOfFile, foInput);

  Evaluate;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  FillFarmList;

  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  ClearTimeLists(Model);
  FFarmWellID := 1;
  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage('Writing FMP2 Package input.');
    frmProgressMM.AddMessage(StrWritingDataSet0);
    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet1);
    WriteDataSet1;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet2a);
    WriteDataSet2a;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet2c);
    WriteDataSet2cParamDimen;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet2cWhenToRead;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    { TODO -cFMP : Write the water policy flags data set 2C }

    WriteDataSet2cCropConsumptiveUse;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    { TODO -cFMP : Write the rest of data set 2C }

    frmProgressMM.AddMessage(StrWritingDataSets3and4);
    WriteDataSets3And4;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet5);
    WriteDataSet5;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet6);
    WriteDataSet6;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet7);
    WriteDataSet7;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet8);
    WriteDataSet8;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet9);
    WriteDataSet9;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet11);
    WriteDataSet11;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    { TODO -cFMP : Write data sets 10-20 }

    frmProgressMM.AddMessage(StrWritingDataSets);
    WriteDataSets21to36;

  finally
    CloseFile;
  end;


end;

procedure TModflowFmpWriter.FillFarmList;
var
  AFarm: TFarm;
  AScreenObject: TScreenObject;
  ScreenObjectIndex: Integer;
begin
  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    AScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if AScreenObject.Deleted then
    begin
      Continue;
    end;
    AFarm := AScreenObject.ModflowFmpFarm;
    if (AFarm <> nil) and AFarm.Used then
    begin
      FFarms.Add(AFarm);
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet8;
var
  DataArray: TDataArray;
begin
  DataArray := Model.DataArrayManager.GetDataSetByName(KSoilID);
  Assert(DataArray <> nil);
  WriteArray(DataArray, 0, 'Data set 8: SID');
end;

procedure TModflowFmpWriter.WriteValueFromGlobalFormula(Formula: string;
  ErrorObject: TObject; const DataSetErrorString: string);
var
  Compiler: TRbwParser;
  ErrorFormula: string;
  ObjectString: string;
  Expression: TExpression;
  Value: double;
  ASoil: TSoilItem;
  AScreenObject: TScreenObject;
  function GetObjectString: string;
  var
    ACrop: TCropItem;
  begin
    if ErrorObject = nil then
    begin
      result := '';
    end
    else if ErrorObject is TSoilItem then
    begin
      ASoil := TSoilItem(ErrorObject);
      result := Format(StrSoilS, [ASoil.SoilName]);
    end
    else if ErrorObject is TCropItem then
    begin
      ACrop := TCropItem(ErrorObject);
      result := Format(StrCropS, [ACrop.CropName]);
    end
    else if ErrorObject is TScreenObject then
    begin
      AScreenObject := TScreenObject(ErrorObject);
      result := AScreenObject.Name;
    end
    else
    begin
      Assert(False);
    end;
  end;
begin
  Compiler := Model.rpThreeDFormulaCompiler;

  ErrorFormula := Formula;

  try
    Compiler.Compile(Formula)
  except on E: ERbwParserError do
    begin
      ObjectString := GetObjectString;
      frmFormulaErrors.AddFormulaError(ObjectString,
        DataSetErrorString, ErrorFormula, E.Message);
      Formula := '0';
      Compiler.Compile(Formula);
      // send error message
    end;
  end;
  Expression := Compiler.CurrentExpression;
  if Expression = nil then
  begin
    Formula := '0';
    Compiler.Compile(Formula);
    Expression := Compiler.CurrentExpression;
  end;
  if not (Expression.ResultType in [rdtDouble, rdtInteger]) then
  begin
    ObjectString := GetObjectString;
    frmFormulaErrors.AddFormulaError(ObjectString,
      DataSetErrorString, ErrorFormula, StrInvalidResultType);
    Formula := '0';
    Compiler.Compile(Formula);
    // send error message
    Expression := Compiler.CurrentExpression;
  end;

  Expression.Evaluate;
  Value := Expression.DoubleResult;
  WriteFloat(Value);
end;

procedure TModflowFmpWriter.WriteDataSet9;
var
  SoilIndex: Integer;
  LocalModel: TPhastModel;
  ASoil: TSoilItem;
  SoilID: Integer;
  Formula: string;
begin
  LocalModel := Model as TPhastModel;
  for SoilIndex := 0 to LocalModel.FmpSoils.Count - 1 do
  begin
    ASoil := LocalModel.FmpSoils[SoilIndex];
    SoilID := SoilIndex + 1;
    WriteInteger(SoilID);

    Formula := ASoil.CapillaryFringe;
    WriteValueFromGlobalFormula(Formula, ASoil, StrCapillaryFringe);

    if FFarmProcess.CropConsumptiveConcept = cccConcept1 then
    begin
      case ASoil.SoilType of
        stSandyLoam:
          WriteString('SANDYLOAM');
        stSilt:
          WriteString('SILT');
        stSiltyClay:
          WriteString('SILTYCLAY');
        stOther:
          begin
            WriteValueFromGlobalFormula(ASoil.ACoeff, ASoil, 'A-Coeff');
            WriteValueFromGlobalFormula(ASoil.BCoeff, ASoil, 'B-Coeff');
            WriteValueFromGlobalFormula(ASoil.CCoeff, ASoil, 'C-Coeff');
            WriteValueFromGlobalFormula(ASoil.DCoeff, ASoil, 'D-Coeff');
            WriteValueFromGlobalFormula(ASoil.ECoeff, ASoil, 'E-Coeff');
          end;
        else
          Assert(False);
      end;
    end;
    WriteString(' # Data Set 9: Soil-ID CapFringe');
    if FFarmProcess.CropConsumptiveConcept = cccConcept1 then
    begin
      if ASoil.SoilType = stOther then
      begin
        WriteString(' A-Coeff B-Coeff C-Coeff D-Coeff E-Coeff');
      end
      else
      begin
        WriteString(' Soil-Type');
      end;
    end;
    NewLine;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet6;
var
  DataArray: TDataArray;
begin
  DataArray := Model.DataArrayManager.GetDataSetByName(KFarmID);
  Assert(DataArray <> nil);
  WriteArray(DataArray, 0, 'Data set 6: FID');
end;

procedure TModflowFmpWriter.WriteDataSet7;
var
  FarmIndex: Integer;
  Efficiencies: TFarmEfficiencyCollection;
  CropIndex: Integer;
  ACropEffCollection: TCropEfficiencyCollection;
  AFarm: TFarm;
  EffItem: TCropEfficiencyItem;
  Formula: string;
  Crops: TCropCollection;
begin
  Crops := (Model as TPhastModel).FmpCrops;
  if IEFFL = 1 then
  begin
    for FarmIndex := 0 to FFarms.Count - 1 do
    begin
      AFarm := FFarms[FarmIndex];
      WriteInteger(AFarm.FarmId);
      Efficiencies := AFarm.FarmEfficiencyCollection;
      Assert(Efficiencies.Count = NCROPS);
      for CropIndex := 0 to Efficiencies.Count - 1 do
      begin
        ACropEffCollection := Efficiencies[CropIndex].CropEfficiency;
        Assert(ACropEffCollection.Count = 1);
        EffItem := ACropEffCollection[0];
        Formula := EffItem.Efficiency;

        WriteValueFromGlobalFormula(Formula, AFarm.ScreenObject,
          Crops[CropIndex].CropName);
      end;
      WriteString(' # Data Set 7 Farm-ID OFE(FID,CID)');
      NewLine;
    end;
  end;
end;

procedure TModflowFmpWriter.WriteDataSet5;
var
  DataArray: TDataArray;
begin
  DataArray := Model.DataArrayManager.GetDataSetByName(StrUzfLandSurface);
  Assert(DataArray <> nil);
  WriteArray(DataArray, 0, 'Data set 5: GSURF');
end;

procedure TModflowFmpWriter.WriteParameterCells(CellList: TValueCellList;
  NLST: Integer; const VariableIdentifiers, DataSetIdentifier: string;
  AssignmentMethod: TUpdateMethod;
  MultiplierArrayNames: TTransientMultCollection;
  ZoneArrayNames: TTransientZoneCollection);
var
  Cell: TFmpWell_Cell;
  CellIndex: Integer;
begin
  // Data set 4b
  for CellIndex := 0 to CellList.Count - 1 do
  begin
    Cell := CellList[CellIndex] as TFmpWell_Cell;
//    Cell.FarmWellId := CellIndex+1;
    WriteCell(Cell, DataSetIdentifier, VariableIdentifiers);
    CheckCell(Cell, 'FMP2');
  end;
  // Dummy inactive cells to fill out data set 4b.
  // Each instance of a parameter is required to have the same
  // number of cells.  This introduces dummy boundaries to fill
  // out the list.  because Condfact is set equal to zero, the
  // dummy boundaries have no effect.
  for CellIndex := CellList.Count to NLST - 1 do
  begin
    WriteInteger(1);
    WriteInteger(1);
    WriteInteger(1);
    WriteInteger(0);
    WriteInteger(0);
    WriteFloat(0);
    WriteInteger(0);
    WriteInteger(0);
    WriteInteger(0);
    WriteString(
      ' # Data Set 4b: Layer Row Column Farm-Well-ID Farm-ID QMAXfact] [xyz] (Dummy boundary)');
    NewLine;
  end;
end;

procedure TModflowFmpWriter.WriteStressPeriods(const VariableIdentifiers,
  DataSetIdentifier, DS5, D7PNameIname, D7PName: string);
begin
  inherited;
  { TODO -cFMP : Write data sets 24-36 }
end;
  {$ENDIF}

end.
