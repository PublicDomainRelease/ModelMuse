unit frmSelectResultToImportUnit;

 { TODO : Make it easy for the user to select all the results data sets of a particular type or all the results data sets for a particular time step. }

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, CheckLst, JvExCheckLst,
  JvCheckListBox, Buttons, JvDialogs, IntListUnit, ReadModflowArrayUnit,
  DataSetUnit, ScreenObjectUnit, StrUtils, UndoItems, Contnrs, RealListUnit,
  ModflowGridUnit, ExtCtrls, EdgeDisplayUnit, GoPhastTypes, Grids, RbwDataGrid4,
  PhastModelUnit;

type
  TModflowResultFormat = (mrBinary, mrAscii, mrFlux, mrHufAscii, mrHufBinary,
    mrHufFlux, mfSubBinary, mfMt3dConc);

  TModelColumns = (mcModelName, mcUse, mcFileName);

  TDataArrayForm = (dafLayer, dafSystem, dafSubsidence, dafWaterTable);

  TDisplayChoice = (dcColor, dcContour, dcNone);

  TUndoImportModelResults = class(TCustomUndo)
  private
    FNewTopDataSet: TDataArray;
    FNewFrontDataSet: TDataArray;
    FNewSideDataSet: TDataArray;
    FNew3DDataSet: TDataArray;
    FNewTopContourDataSet: TDataArray;
    FNewFrontContourDataSet: TDataArray;
    FNewSideContourDataSet: TDataArray;
    FNew3DContourDataSet: TDataArray;
    FNewEdgeDisplay: TCustomModflowGridEdgeDisplay;

    FNewThreeDTimeList: TCustomTimeList;
    FNewTopTimeList: TCustomTimeList;
    FNewFrontTimeList: TCustomTimeList;
    FNewSideTimeList: TCustomTimeList;

    FOldTopDataSet: TDataArray;
    FOldFrontDataSet: TDataArray;
    FOldSideDataSet: TDataArray;
    FOld3DDataSet: TDataArray;
    FOldTopContourDataSet: TDataArray;
    FOldFrontContourDataSet: TDataArray;
    FOldSideContourDataSet: TDataArray;
    FOld3DContourDataSet: TDataArray;
    FOldEdgeDisplay: TCustomModflowGridEdgeDisplay;

    FOldThreeDTimeList: TCustomTimeList;
    FOldTopTimeList: TCustomTimeList;
    FOldFrontTimeList: TCustomTimeList;
    FOldSideTimeList: TCustomTimeList;

    FContainedUndos: TList;
    FNewDataSets: TList;
    FOldComments: TStringList;
    FNewComments: TStringList;
    FModel: TCustomModel;
    procedure SetComments(Comments: TStringList);
  protected
    function Description: string; override;
  public
    Constructor Create(NewDataSets: TList;
      DataSetNames, OldComments: TStringList;
      DisplayDataSet: TDataArray; DisplayChoice: TDisplayChoice;
      AModel: TCustomModel);
    Destructor Destroy; override;
    // @name does the command for the first time.
    procedure DoCommand; override;
    // @name undoes the command.
    procedure Undo; override;
  end;

type
  TFormulaAssigner = class(TObject)
  strict private
    FDataArray: TDataArray;
    FFormulas: TStringList;
    FModels: TList;
  public
    Constructor Create(ADataArray: TDataArray);
    Destructor Destroy; override;
    procedure AddFormula(AFormula: string; AModel: TBaseModel);
    procedure AssignFinalFormula;
    property DataArray: TDataArray read FDataArray;
  end;

  TFormulaAssignerList = class(TObject)
  strict private
    FList: TList;
    function GetFormulaAssigner(ADataArray: TDataArray): TFormulaAssigner;
  public
    Constructor Create;
    Destructor Destroy; override;
    property FormulaAssigners[ADataArray: TDataArray]: TFormulaAssigner
      read GetFormulaAssigner; default;
    procedure AssignFinalFormulas;
  end;

  TfrmSelectResultToImport = class(TfrmCustomGoPhast)
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    odSelectFiles: TJvOpenDialog;
    comboColorGrid: TComboBox;
    Label1: TLabel;
    btnSelectAll: TButton;
    btnSelectNone: TButton;
    rgDisplayChoice: TRadioGroup;
    Panel1: TPanel;
    rdgModels: TRbwDataGrid4;
    clData: TJvCheckListBox;
    splitData: TSplitter;
    procedure clDataClickCheck(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure odSelectFilesTypeChange(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnSelectNoneClick(Sender: TObject);
    procedure rdgModelsBeforeDrawCell(Sender: TObject; ACol, ARow: Integer);
    procedure rdgModelsButtonClick(Sender: TObject; ACol, ARow: Integer);
  private
    FPeriods: TIntegerList;
    FSteps: TIntegerList;
    FTransportSteps: TIntegerList;
    FDescriptions: TStringList;
    FFileStream: TFileStream;
    FFileVariable: TFileVariable;
    FResultFormat: TModflowResultFormat;
    FAskedUser: Boolean;
    FCreateNewDataSet: Boolean;
    FMaxPeriod: Integer;
    FMaxTrans: integer;
    FMaxStep: Integer;
    FMaxLayer: Integer;
    FGrid: TModflowGrid;
    FNewDataSetNames: TStringList;
    FNewDefaultDataSetNames: TStringList;
    FFormulaAssigners: TFormulaAssignerList;
    FModifiedParentDataSets: TList;
    function DefaultFileName(AModel: TCustomModel): string;
    function OpenResultFile(AFileName: string;out Precision: TModflowPrecision;
      out HufFormat: boolean): boolean;
    procedure ReadArray(var AnArray: TModflowDoubleArray;
      var EndReached: Boolean; var NTRANS, KPER, KSTP, ILAY: Integer;
      var TOTIM: TModflowDouble;
      var Description: string; Precision: TModflowPrecision;
      ShouldReadArray: boolean);
    procedure CreateOrRetrieveLayerDataSet(const Description: string;
      NTRANS, KSTP, KPER, ILAY: integer; TOTIM: TModflowDouble;
      out LayerData: TDataArray; out OldComment: string; NewDataSets: TList;
      ScreenObjectsToDelete: TScreenObjectList; FileNames: string;
      AModel: TCustomModel; DataArrayForm: TDataArrayForm = dafLayer);
    procedure CreateScreenObject(LayerIndex: integer; AModel: TCustomModel;
      out ScreenObject: TScreenObject);
    procedure AssignValues(LayerIndex: integer; ScreenObject: TScreenObject;
      LayerData: TDataArray; AnArray: TModflowDoubleArray;
      ValuesToIgnore: TOneDRealArray; AModel: TCustomModel;
      out MinMaxAssigned: boolean);
    procedure CreateOrRetrieve3DDataSet(Description: string;
      NTRANS, KPER, KSTP: integer; TOTIM: TModflowDouble;
      LayerNumbers: TIntegerList; LayerDataSets: TList;
      out New3DArray: TDataArray; out OldComment: string; FluxData: boolean;
      NewDataSets: TList; FileNames: string; AModel: TCustomModel);
    procedure CloseFiles;
    procedure Read3DArray(var NLAY: Integer; var EndReached: Boolean;
      var KPER, KSTP: Integer; var TOTIM: TModflowDouble;
      var Description: string; var A3DArray: T3DTModflowArray;
      Precision: TModflowPrecision; HufFormat: boolean;
      ShouldReadArray: boolean);
    procedure Assign3DValues(ScreenObject: TScreenObject; LayerData: TDataArray;
      AnArray: T3DTModflowArray; LayerIndex: integer; CheckAllLayers: boolean;
      ValuesToIgnore: TOneDRealArray; AModel: TCustomModel);
    procedure SetData;
    function AskUserIfNewDataSet: boolean;
    procedure AssignLimits(MinValues, MaxValues: TRealList;
      New3DArray: TDataArray; ValuesToIgnore: TOneDRealArray);
    procedure AssignObjectName(var ScreenObject: TScreenObject; LayerData: TDataArray);
    procedure UpdateCombo;
    procedure GetShouldIgnore(ValuesToIgnore: TOneDRealArray;
      Temp: TModflowFloat; var ShouldIgnore: Boolean);
    function SubsidenceDescription(DESC: String; ILAY: integer): string;
    // In the label for data sets, TOTIM will be measured from the end of
    // the first stress period if there are more than one stress period and
    // the first stress period is a steady-state stress period.
    procedure AdjustTotalTime(var TOTIM: TModflowDouble);
    procedure AssignWaterTableArray(
      var WaterTableArray: TModflowDoubleArray;
      ILAY: Integer;
      AnArray: TModflowDoubleArray;
      ValuesToIgnore: TOneDRealArray;
      const Description: string);
    procedure AssignWaterTable(NewDataSets: TList; OldComments: TStringList;
      DataSetNames: TStringList; ScreenObjectsToDelete: TScreenObjectList;
      NewCreateScreenObjects: TList; NTRANS, KPER: Integer;
      WaterTableArray: TModflowDoubleArray; KSTP: Integer;
      const Description: string; ValuesToIgnore: TOneDRealArray;
      TOTIM: TModflowDouble; FileNames: string; AModel: TCustomModel);
    procedure SetDefaultDisplayOption;
    { Private declarations }
    function ReadDataHeadings(AModel: TCustomModel; RowIndex: integer; AFileName: string): boolean;
    procedure AddModelRow(AModel: TCustomModel; ARow: integer;
      AFileName: string);
    procedure UpdateOldComments(OldComments: TStringList;
      ADataArray: TDataArray; OldComment: string);
//    procedure SetData;
  public
    function SelectFiles: boolean;
    { Public declarations }
  end;

var
  frmSelectResultToImport: TfrmSelectResultToImport;
  // @name records how many times the user has chosen to color the grid,
  // contour the data, or do nothing.  The most frequent choice is then
  // selected at the default when the @link(TfrmSelectResultToImport) is
  // created.
  DisplayChoices : array[TDisplayChoice] of integer = (0, 0, 0);
const
  MaxDisplayChoiceCount = 10;

implementation

uses Math, frmGoPhastUnit, RbwParser,
  GIS_Functions, ValueArrayStorageUnit, ModelMuseUtilities, 
  frmUpdateDataSetsUnit, UndoItemsScreenObjects,
  InterpolationUnit, HufDefinition, ModflowTimeUnit,
  frmGridValueUnit, shlobj, activex, AnsiStrings, frmDisplayDataUnit,
  Mt3dmsChemSpeciesUnit, frmExportImageUnit;

resourcestring
  StrHead = 'Head';
  StrTheFileCouldNotB = 'The file could not be read.' + sLineBreak + '"%s"';
  StrImportModelResults = 'import model results';
  StrTheNumberOfRowsOrColumns = 'The number of rows or columns in the data s' +
  'et doesn''t match the number of rows or columns in the grid.';
  StrFileIsEmpty = 'File is empty.';
  StrTheNumberOfLayersOrCol = 'The number of layers or columns in the data s' +
  'et doesn''t match the number of layers or columns in the grid.';
  StrTheNumberOfRows = 'The number of rows, columns, or layers in the data s' +
  'et doesn''t match the number of rows, columns, or layers in the grid.';
  StrTheNumberOfHydrogeologic = 'The number of rows, columns, or hydrogeolog' +
  'ic units in the data set doesn''t match the number of rows, columns, or h' +
  'ydrogeologic units in the grid.';
  StrReadFrom0sOn = 'read from: "%0:s" on %1:s'
    + sLineBreak + 'Stress Period: %2:d'
    + sLineBreak + 'Time Step: %3:d'
    + sLineBreak + 'Elapsed Time: %4:g';
  StrLayer = 'Layer: ';
  StrSystem = 'System: ';
  StrChildLayer = 'Child Layer: ';
  StrChildSystem = 'Child System: ';
  StrAtLeastOneOfThe = 'At least one of the result files does not exist.';
  StrTheFileYouAreTry = 'The file you are trying to read appears to have mor' +
  'e simulated layers than does your model. Aborting data import.';
  StrMinimumValueG = 'Minimum value: %g';
  StrMaximumValueG = 'Maximum value: %g';
  StrModel = 'Model';
  StrImportData = 'Import Data';
  StrFileName = 'FileName';
  StrFormattedHeadFiles = 'Formatted head files';
  StrFormattedDrawdownF = 'Formatted drawdown files';
  StrBinaryHeadFiles = 'Binary head files';
  StrBinaryDrawdownFile = 'Binary drawdown files';
  StrBinaryFlowFiles = 'Binary flow files';
  StrFormattedHUFHeadF = 'Formatted HUF head files';
  StrBinaryHUFHeadFile = 'Binary HUF head files';
  StrHUFFlowFiles = 'HUF flow files';
  StrMT3DMSConcentration = 'MT3DMS Concentration file';
  StrCombinedSUBOutput = 'Combined SUB output file';
  StrCombinedSWTOutput = 'Combined SWT output file';
  StrSUBSubsidence = 'SUB Subsidence';
  StrSUBCompactionByMo = 'SUB Compaction by model layer';
  StrSUBCompactionByIn = 'SUB Compaction by interbed system';
  StrSUBVerticalDisplac = 'SUB Vertical displacement';
  StrSUBCriticalHeadFo = 'SUB Critical head for no-delay interbeds';
  StrSUBCriticalHeadFoDelay = 'SUB Critical head for delay interbeds';
  StrSWTSubsidence = 'SWT Subsidence';
  StrSWTCompactionByMo = 'SWT Compaction by model layer';
  StrSWTCompactionByIn = 'SWT Compaction by interbed system';
  StrSWTVerticalDisplac = 'SWT Vertical displacement';
  StrSWTPreconsolidation = 'SWT Preconsolidation stress';
  StrSWTChangeInPrecon = 'SWT Change in preconsolidation stress';
  StrSWTGeostaticStress = 'SWT Geostatic stress';
  StrSWTChangeInGeosta = 'SWT Change in geostatic stress';
  StrSWTEffectiveStress = 'SWT Effective stress';
  StrSWTChangeInEffect = 'SWT Change in effective stress';
  StrSWTVoidRatio = 'SWT Void ratio';
  StrSWTThicknessOfCom = 'SWT Thickness of compressible sediments';
  StrSWTLayercenterEle = 'SWT Layer-center elevation';
  StrCommonSupportedFil = 'Common supported file types|*';
  StrSubsidenceFiles = '|Subsidence files|*';
  Str0s1sPeriod2 = '%0:s%1:s: Period: %2:d; Step: %3:d';
  Str0sTransportStep = '%0:s; Transport Step: %1:d';
  Str0sTotalTime1 = '%0:s; Total Time: %1:g';
  StrSDoesNotExist = '%s does not exist.';
  StrSIsEmpty = '%s is empty.';
  StrErrorReadingS = 'Error reading %s.';
  StrWaterTable = 'Water Table';
  StrObject = '_Object';
  StrTheFileTypeMustB = 'The file type must be one of the file types recogni' +
  'zed by ModelMuse. The recognized file types are displayed in the "files o' +
  'f type" combo box in the "Open File" dialog box.';

const
  StrModelResults = 'Model Results';
  StrLayerData = StrModelResults + '|Layer Data';
  StrThreeDData = StrModelResults + '|3D Data';
  KSystem = StrModelResults + '|System';
  KWaterTable = StrModelResults + '|Water Table';

{$R *.dfm}

Function PaddedIntToStr(Value, MaxValue: integer): string;
var
  Index: Integer;
begin
  result := IntToStr(Value);
  for Index := Trunc(Log10(Value)) to Trunc(Log10(MaxValue)) - 1 do
  begin
    result := '0' + result;
  end;
end;

function PaddedFloatToStr(Value: double): string;
begin
  result := FloatToStr(Value);
  while Length(result) < 15 do
  begin
    result := '_' + result;
  end;
end;

{ TfrmSelectResultToImport }

procedure TfrmSelectResultToImport.CreateOrRetrieveLayerDataSet(
  const Description: string; NTRANS, KSTP, KPER, ILAY: integer;
  TOTIM: TModflowDouble;
  out LayerData: TDataArray; out OldComment: string; NewDataSets: TList;
  ScreenObjectsToDelete: TScreenObjectList; FileNames: string;
  AModel: TCustomModel; DataArrayForm: TDataArrayForm = dafLayer);
var
  NewName: string;
//  Grid: TModflowGrid;
  CreateNewDataSet: boolean;
  Index: Integer;
  ScreenObject: TScreenObject;
  NewDataSetPosition: Integer;
  DefaultName: string;
  ParentLayerData: TDataArray;
begin
  NewName := TitleCase(Description);
  NewName := NewName + '_P' + PaddedIntToStr(KPER, FMaxPeriod) +
    '_S' + PaddedIntToStr(KSTP, FMaxStep);
  if FResultFormat = mfMt3dConc then
  begin
    NewName := NewName + '_TS' + PaddedIntToStr(NTRANS, FMaxTrans);
  end;
  NewName := ValidName(NewName);
  case DataArrayForm of
    dafLayer:
      begin
        NewName := NewName + '_L' + PaddedIntToStr(ILAY, FMaxLayer);
      end;
    dafSystem:
      begin
        NewName := NewName + '_Sys' + PaddedIntToStr(ILAY, FMaxLayer);
      end;
    dafSubsidence, dafWaterTable:
      begin
        // do nothing
      end;
    else
      Assert(False);
  end;
  DefaultName := NewName;
  NewDataSetPosition := FNewDefaultDataSetNames.IndexOf(NewName);
  if NewDataSetPosition >= 0 then
  begin
    NewName := FNewDataSetNames[NewDataSetPosition];
    CreateNewDataSet := False;
  end
  else
  begin
    CreateNewDataSet := True;
    if AModel.DataArrayManager.GetDataSetByName(NewName) <> nil then
    begin
      CreateNewDataSet := AskUserIfNewDataSet;
      if CreateNewDataSet then
      begin
        NewName := GenerateNewName(NewName, nil, '_');
      end;
    end;
  end;
  if CreateNewDataSet then
  begin
    FNewDefaultDataSetNames.Add(DefaultName);
    FNewDataSetNames.Add(NewName);
    LayerData := TDataArray.Create(frmGoPhast.PhastModel);
    NewDataSets.Add(LayerData);
    LayerData.UpDateWithName(NewName);
//    LayerData.OnNameChange := frmGoPhast.PhastModel.DataArrayNameChange;
    frmGoPhast.PhastModel.AddDataSet(LayerData);
    LayerData.DataType := rdtDouble;
    LayerData.Orientation := dsoTop;
    LayerData.Formula :=
      FortranFloatToStr(AModel.ModflowOptions.HNoFlow);
    LayerData.TwoInterpolatorClass := TLinearSfrpackInterpolator.ClassName;

//    Grid := frmGoPhast.PhastModel.ModflowGrid;
    frmGoPhast.PhastModel.UpdateDataArrayDimensions(LayerData);
//    LayerData.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
//      Grid.ColumnCount);
    LayerData.EvaluatedAt := eaBlocks;
    case DataArrayForm of
      dafLayer: LayerData.Classification := StrLayerData;
      dafSystem: LayerData.Classification := KSystem;
      dafSubsidence: LayerData.Classification := StrLayerData;
      dafWaterTable: LayerData.Classification := KWaterTable;
    end;

    LayerData.OnDataSetUsed := frmGoPhast.PhastModel.ModelResultsRequired;
    frmGoPhast.PhastModel.CreateVariables(LayerData);
    LayerData := AModel.DataArrayManager.GetDataSetByName(NewName);
//    Grid := AModel.ModflowGrid;
    AModel.UpdateDataArrayDimensions(LayerData);
//    LayerData.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
//      Grid.ColumnCount);
  end
  else
  begin
    LayerData := AModel.DataArrayManager.GetDataSetByName(NewName);
//    Grid := AModel.ModflowGrid;
    AModel.UpdateDataArrayDimensions(LayerData);
//    LayerData.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
//      Grid.ColumnCount);
    for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      ScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
      if (ScreenObject.IndexOfDataSet(LayerData) >= 0)
        and ScreenObject.UsedModels.UsesModel(AModel) then
      begin
        ScreenObject.Deleted := True;
        ScreenObjectsToDelete.Add(ScreenObject);
      end;
    end;
  end;
  AdjustTotalTime(TOTIM);
  if LayerData.Model = frmGoPhast.PhastModel then
  begin
    ParentLayerData := LayerData;
  end
  else
  begin
    ParentLayerData := frmGoPhast.PhastModel.DataArrayManager.
      GetDataSetByName(LayerData.Name);
  end;
  OldComment := ParentLayerData.Comment;
  if LayerData = ParentLayerData then
  begin
    ParentLayerData.Comment := Format(StrReadFrom0sOn,
      [FileNames, DateTimeToStr(Now), KPER, KSTP, TOTIM]);
    if FResultFormat = mfMt3dConc then
    begin
      ParentLayerData.Comment := ParentLayerData.Comment
        + sLineBreak + StrTransportStep + IntToStr(NTRANS)
    end;
    case DataArrayForm of
      dafLayer:
        begin
          ParentLayerData.Comment := ParentLayerData.Comment
            + sLineBreak + StrLayer + IntToStr(ILAY);
        end;
      dafSystem:
        begin
          ParentLayerData.Comment := ParentLayerData.Comment
            + sLineBreak + StrSystem + IntToStr(ILAY);
        end;
      dafSubsidence, dafWaterTable:
        begin
          // do nothing
        end;
      else
        Assert(False);
    end;
  end
  else
  begin
    if ParentLayerData.Comment = '' then
    begin
      ParentLayerData.Comment := Format(StrReadFrom0sOn,
        [FileNames, DateTimeToStr(Now), KPER, KSTP, TOTIM]);
      if FResultFormat = mfMt3dConc then
      begin
        ParentLayerData.Comment := ParentLayerData.Comment
          + sLineBreak + StrTransportStep + IntToStr(NTRANS)
      end;
    end;
    case DataArrayForm of
      dafLayer:
        begin
          ParentLayerData.Comment := ParentLayerData.Comment
            + sLineBreak + StrChildLayer + IntToStr(ILAY);
        end;
      dafSystem:
        begin
          ParentLayerData.Comment := ParentLayerData.Comment
            + sLineBreak + StrChildSystem + IntToStr(ILAY);
        end;
      dafSubsidence, dafWaterTable:
        begin
          // do nothing
        end;
      else
        Assert(False);
    end;
  end;
  if FModifiedParentDataSets.IndexOf(ParentLayerData) < 0 then
  begin
    FModifiedParentDataSets.Add(ParentLayerData);
  end;
end;

procedure TfrmSelectResultToImport.CreateScreenObject(LayerIndex: integer;
  AModel: TCustomModel; out ScreenObject: TScreenObject);
var
  RowIndex: Integer;
  ColIndex: Integer;
  UndoCreateScreenObject: TCustomUndo;
  Grid: TModflowGrid;
  ActiveDataSet: TDataArray;
  LI: Integer;
begin
  ActiveDataSet := AModel.DataArrayManager.GetDataSetByName(rsActive);
  ActiveDataSet.Initialize;

  Grid := AModel.ModflowGrid;
  ScreenObject := TScreenObject.CreateWithViewDirection(
    frmGoPhast.PhastModel, vdTop,
    UndoCreateScreenObject, False);
  frmGoPhast.PhastModel.AddScreenObject(ScreenObject);
  ScreenObject.ElevationCount := ecZero;
  ScreenObject.SetValuesOfIntersectedCells := True;

  if frmGoPhast.PhastModel.LgrUsed then
  begin
    ScreenObject.UsedModels.UsedWithAllModels := False;
    ScreenObject.UsedModels.AddModel(AModel);
  end;

  // Don't SetValuesByInterpolation because it causes
  // inactive cells to be assigned inappropriate values.
  // This causes such cells to be colored when the grid is colored
  // with the data set.
//  ScreenObject.SetValuesByInterpolation := True;
  ScreenObject.EvaluatedAt := eaBlocks;
  ScreenObject.Visible := False;
  ScreenObject.Capacity := Grid.RowCount * Grid.ColumnCount;
  for RowIndex := 0 to Grid.RowCount - 1 do
  begin
    for ColIndex := 0 to Grid.ColumnCount - 1 do
    begin
      if LayerIndex >= 0 then
      begin
        if ActiveDataSet.BooleanData[LayerIndex, RowIndex, ColIndex] then
        begin
          ScreenObject.AddPoint(
            Grid.TwoDElementCenter(ColIndex, RowIndex), True);
        end;
      end
      else
      begin
        for LI := 0 to ActiveDataSet.LayerCount - 1 do
        begin
          if ActiveDataSet.BooleanData[LI, RowIndex, ColIndex] then
          begin
            ScreenObject.AddPoint(
              Grid.TwoDElementCenter(ColIndex, RowIndex), True);
            break;
          end;
        end;
      end;
    end;
  end;
end;

function TfrmSelectResultToImport.DefaultFileName(AModel: TCustomModel): string;
begin
  Assert(frmGoPhast.PhastModel.ModelSelection in [msModflow, msModflowLGR, msModflowNWT]);
  result := AModel.DefaultModflowOutputFileName;
  if not FileExists(result) then
  begin
    result := '';
  end;
end;

procedure TfrmSelectResultToImport.AddModelRow(AModel: TCustomModel; ARow: integer; AFileName: string);
begin
  rdgModels.Objects[Ord(mcModelName), ARow] := AModel;
  rdgModels.Cells[Ord(mcModelName), ARow] := AModel.DisplayName;
  rdgModels.Cells[Ord(mcFileName), ARow] := AFileName;
  rdgModels.Checked[Ord(mcUse), ARow] := FileExists(AFileName);
end;

function TfrmSelectResultToImport.SelectFiles: boolean;
var
  ChildIndex: Integer;
  ChildModel: TChildModel;
  AFileName: string;
  Extension: string;
begin
  FMaxTrans := -1;
  try
    odSelectFiles.FileName := DefaultFileName(frmGoPhast.PhastModel);
    SetCurrentDir(ExtractFileDir(odSelectFiles.FileName));
    result := odSelectFiles.Execute;
    if result then
    begin
      Screen.Cursor := crHourGlass;
      rdgModels.BeginUpdate;
      try
          if frmGoPhast.PhastModel.LgrUsed then
          begin
            rdgModels.RowCount := 2 + frmGoPhast.PhastModel.ChildModels.Count;
          end
          else
          begin
            rdgModels.RowCount := 2;
            rdgModels.Visible := False;
            splitData.Visible := False;
          end;
          AFileName := odSelectFiles.FileName;
          AddModelRow(frmGoPhast.PhastModel, 1,  AFileName);
          result := ReadDataHeadings(frmGoPhast.PhastModel, 1, AFileName);
          if result and frmGoPhast.PhastModel.LgrUsed then
          begin
            Extension := ExtractFileExt(AFileName);
            for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
            begin
              ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
              AFileName := DefaultFileName(ChildModel);
              AFileName := ChangeFileExt(AFileName, Extension);
              AddModelRow(ChildModel, ChildIndex + 2,  AFileName);
              if FileExists(AFileName) then
              begin
                result := ReadDataHeadings(ChildModel, ChildIndex + 2,  AFileName);
                if not result then
                begin
                  Exit;
                end;
              end
              else
              begin

              end;
            end;
          end;
      finally
        rdgModels.EndUpdate;
        Screen.Cursor := crDefault;
      end;
    end;
  except on E: EInOutError do
    begin
      result := False;
      Beep;
      MessageDlg(Format(StrTheFileCouldNotB,
        [E.message]), mtError, [mbOK], 0);
    end;
  end;
end;

procedure TfrmSelectResultToImport.UpdateOldComments(OldComments: TStringList;
  ADataArray: TDataArray; OldComment: string);
var
  ParentDataArray: TDataArray;
begin
  if ADataArray.Model = frmGoPhast.PhastModel then
  begin
    ParentDataArray := ADataArray;
  end
  else
  begin
    ParentDataArray := frmGoPhast.PhastModel.
      DataArrayManager.GetDataSetByName(ADataArray.Name);
  end;
  if OldComments.IndexOfObject(ParentDataArray) < 0 then
  begin
    OldComments.AddObject(OldComment, ParentDataArray);
  end;
end;

procedure TfrmSelectResultToImport.AssignValues(LayerIndex: integer;
  ScreenObject: TScreenObject; LayerData: TDataArray;
  AnArray: TModflowDoubleArray; ValuesToIgnore: TOneDRealArray;
  AModel: TCustomModel; out MinMaxAssigned: boolean);
var
  RowIndex: Integer;
  ColIndex: Integer;
  DataSetIndex: Integer;
  ImportedValues: TValueArrayItem;
  PointIndex: Integer;
  Grid: TModflowGrid;
  ActiveDataSet: TDataArray;
  Temp: TModflowFloat;
  MinValue, MaxValue: TModflowFloat;
  ShouldCheck: Boolean;
  LI: Integer;
  ShouldIgnore: Boolean;
  IgnoreIndex: Integer;
  SkipReal: TSkipReal;
  ParentDataSet: TDataArray;
  ParentLayerData: TDataArray;
begin
  ActiveDataSet := AModel.DataArrayManager.GetDataSetByName(rsActive);
  ActiveDataSet.Initialize;

  Grid := AModel.ModflowGrid;
  if AModel is TPhastModel then
  begin
    DataSetIndex := ScreenObject.AddDataSet(LayerData);
  end
  else
  begin
    ParentDataSet := frmGoPhast.PhastModel.DataArrayManager.
      GetDataSetByName(LayerData.Name);
    DataSetIndex := ScreenObject.AddDataSet(ParentDataSet);
  end;
  ScreenObject.DataSetFormulas[DataSetIndex] := rsObjectImportedValuesR
    + '("' + LayerData.Name + '")';

  AssignObjectName(ScreenObject, LayerData);

  ScreenObject.ImportedValues.Add;
  ImportedValues := ScreenObject.ImportedValues.Items[0];
  ImportedValues.Values.DataType := rdtDouble;
  ImportedValues.Values.Count := Grid.RowCount * Grid.ColumnCount;
  ImportedValues.Name := LayerData.Name;
  PointIndex := 0;
  MinValue := 0;
  MaxValue := 0;
  MinMaxAssigned := False;
  for RowIndex := 0 to Grid.RowCount - 1 do
  begin
    for ColIndex := 0 to Grid.ColumnCount - 1 do
    begin
      ShouldCheck := False;
      if LayerIndex >= 0 then
      begin
        ShouldCheck := ActiveDataSet.BooleanData[LayerIndex, RowIndex, ColIndex];
      end
      else
      begin
        for LI := 0 to ActiveDataSet.LayerCount - 1 do
        begin
          if ActiveDataSet.BooleanData[LI, RowIndex, ColIndex] then
          begin
            ShouldCheck := True;
            break;
          end;
        end;
      end;
      if ShouldCheck then
      begin
        Temp:= AnArray[RowIndex, ColIndex];
        ImportedValues.Values.RealValues[PointIndex] := Temp;
        Inc(PointIndex);
        GetShouldIgnore(ValuesToIgnore, Temp, ShouldIgnore);

        if not ShouldIgnore then
        begin
          if not MinMaxAssigned then
          begin
            MinValue := Temp;
            MaxValue := Temp;
            MinMaxAssigned := True;
          end
          else
          begin
            if MinValue > Temp then
            begin
              MinValue := Temp;
            end
            else if MaxValue < Temp then
            begin
              MaxValue := Temp;
            end;
          end;
        end;
      end;
    end;
  end;
  ImportedValues.Values.Count := PointIndex;
  ImportedValues.CacheData;
  ParentLayerData := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(LayerData.Name);
  if ParentLayerData = LayerData then
  begin
    ParentLayerData.Limits.LowerLimit.UseLimit := True;
    ParentLayerData.Limits.LowerLimit.RealLimitValue := MinValue;
    ParentLayerData.Limits.UpperLimit.UseLimit := True;
    ParentLayerData.Limits.UpperLimit.RealLimitValue := MaxValue;
    ParentLayerData.Limits.Update;
    ParentLayerData.Limits.RealValuesToSkip.Clear;
    for IgnoreIndex := 0 to Length(ValuesToIgnore) - 1 do
    begin
      SkipReal := ParentLayerData.Limits.RealValuesToSkip.Add as TSkipReal;
      SkipReal.RealValue := ValuesToIgnore[IgnoreIndex];
    end;
    ParentLayerData.ContourLimits := ParentLayerData.Limits;
  end
  else
  begin
    LayerData.Limits.LowerLimit.RealLimitValue := MinValue;
    LayerData.Limits.UpperLimit.RealLimitValue := MaxValue;
    ParentLayerData.Limits.LowerLimit.RealLimitValue :=
      Min(MinValue, ParentLayerData.Limits.LowerLimit.RealLimitValue);
    ParentLayerData.Limits.UpperLimit.RealLimitValue :=
      Max(MaxValue, ParentLayerData.Limits.UpperLimit.RealLimitValue);
    for IgnoreIndex := 0 to Length(ValuesToIgnore) - 1 do
    begin
      if ParentLayerData.Limits.RealValuesToSkip.IndexOf(ValuesToIgnore[IgnoreIndex]) < 0 then
      begin
        SkipReal := ParentLayerData.Limits.RealValuesToSkip.Add as TSkipReal;
        SkipReal.RealValue := ValuesToIgnore[IgnoreIndex];
      end;
    end;
    ParentLayerData.ContourLimits := ParentLayerData.Limits;
  end;
end;

procedure TfrmSelectResultToImport.Assign3DValues(ScreenObject: TScreenObject;
  LayerData: TDataArray; AnArray: T3DTModflowArray; LayerIndex: integer;
  CheckAllLayers: boolean; ValuesToIgnore: TOneDRealArray; AModel: TCustomModel);
var
  RowIndex: Integer;
  ColIndex: Integer;
  DataSetIndex: Integer;
  ImportedValues: TValueArrayItem;
  PointIndex: Integer;
  Grid: TModflowGrid;
  MinMaxAssigned: boolean;
  Temp: TModflowFloat;
  MinValue, MaxValue: TModflowFloat;
  ActiveDataSet: TDataArray;
  ShouldCheck: Boolean;
  LI: Integer;
  ShouldIgnore: Boolean;
  ParentLayerData: TDataArray;
begin
  AssignObjectName(ScreenObject, LayerData);

  ActiveDataSet := AModel.DataArrayManager.GetDataSetByName(rsActive);
  ActiveDataSet.Initialize;

  Grid := AModel.ModflowGrid;
  DataSetIndex := ScreenObject.AddDataSet(LayerData);
  ScreenObject.DataSetFormulas[DataSetIndex] := rsObjectImportedValuesR
    + '("' + LayerData.Name + '")';
  ScreenObject.ImportedValues.Add;
  ImportedValues := ScreenObject.ImportedValues.Items[0];
  ImportedValues.Values.DataType := rdtDouble;
  ImportedValues.Values.Count := Grid.RowCount * Grid.ColumnCount;
  ImportedValues.Name := LayerData.Name;
  PointIndex := 0;
  MinValue := 0;
  MaxValue := 0;
  MinMaxAssigned := False;
  for RowIndex := 0 to Grid.RowCount - 1 do
  begin
    for ColIndex := 0 to Grid.ColumnCount - 1 do
    begin
      ShouldCheck := False;
      if not CheckAllLayers then
      begin
        ShouldCheck := ActiveDataSet.BooleanData[LayerIndex, RowIndex, ColIndex];
      end
      else
      begin
        for LI := 0 to ActiveDataSet.LayerCount - 1 do
        begin
          if ActiveDataSet.BooleanData[LI, RowIndex, ColIndex] then
          begin
            ShouldCheck := True;
            break;
          end;
        end;
      end;
      if ShouldCheck then
      begin
        Temp := AnArray[LayerIndex, RowIndex, ColIndex];
        ImportedValues.Values.RealValues[PointIndex] := Temp;
        Inc(PointIndex);
        GetShouldIgnore(ValuesToIgnore, Temp, ShouldIgnore);

        if not ShouldIgnore then
        begin
          if not MinMaxAssigned then
          begin
            MinValue := Temp;
            MaxValue := Temp;
            MinMaxAssigned := True;
          end
          else
          begin
            if MinValue > Temp then
            begin
              MinValue := Temp;
            end
            else if MaxValue < Temp then
            begin
              MaxValue := Temp;
            end;
          end;
        end;
      end;
    end;
  end;
  ImportedValues.Values.Count := PointIndex;
  ImportedValues.CacheData;
  ParentLayerData := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(LayerData.Name);
  if ParentLayerData = LayerData then
  begin
    ParentLayerData.Limits.LowerLimit.UseLimit := True;
    ParentLayerData.Limits.LowerLimit.RealLimitValue := MinValue;
    ParentLayerData.Limits.UpperLimit.UseLimit := True;
    ParentLayerData.Limits.UpperLimit.RealLimitValue := MaxValue;
  end
  else
  begin
    ParentLayerData.Limits.LowerLimit.RealLimitValue :=
      Min(MinValue, ParentLayerData.Limits.LowerLimit.RealLimitValue);
    ParentLayerData.Limits.UpperLimit.RealLimitValue := MaxValue;
      Max(MaxValue, ParentLayerData.Limits.UpperLimit.RealLimitValue);
  end;
  ParentLayerData.Limits.Update;
end;

procedure TfrmSelectResultToImport.CreateOrRetrieve3DDataSet(Description: string;
  NTRANS, KPER, KSTP: integer; TOTIM: TModflowDouble; LayerNumbers: TIntegerList;
  LayerDataSets: TList; out New3DArray: TDataArray; out OldComment: string; FluxData: boolean;
  NewDataSets: TList; FileNames: string; AModel: TCustomModel);
var
  NewName: string;
  NewFormula: string;
  LayerIndex: Integer;
  LayerPosition: Integer;
  DataArray: TDataArray;
  Grid: TModflowGrid;
  CreateNewDataSet: Boolean;
  DefaultName: string;
  NamePosition: Integer;
  Assigner: TFormulaAssigner;
  Parent3DArray: TDataArray;
begin
  NewName := TitleCase(Description);
  NewName := NewName + '_P' + PaddedIntToStr(KPER, FMaxPeriod) +
    '_S' + PaddedIntToStr(KSTP, FMaxStep);
  if FResultFormat = mfMt3dConc then
  begin
    NewName := NewName + '_TS' + PaddedIntToStr(NTRANS, FMaxTrans);
  end;
  NewName := ValidName(NewName);

  DefaultName := NewName;
  NamePosition := FNewDefaultDataSetNames.IndexOf(DefaultName);
  if NamePosition >= 0 then
  begin
    NewName := FNewDataSetNames[NamePosition];
    CreateNewDataSet := False;
  end
  else
  begin
    CreateNewDataSet := True;
    if AModel.DataArrayManager.GetDataSetByName(NewName) <> nil then
    begin
      CreateNewDataSet := AskUserIfNewDataSet;
      if CreateNewDataSet then
      begin
        NewName := GenerateNewName(NewName, nil, '_');
      end;
    end;
  end;
//  Grid := AModel.ModflowGrid;
  if CreateNewDataSet then
  begin
    FNewDefaultDataSetNames.Add(DefaultName);
    FNewDataSetNames.Add(NewName);
    New3DArray := TDataArray.Create(frmGoPhast.PhastModel);
    NewDataSets.Add(New3DArray);
    New3DArray.UpDateWithName(NewName);
//    New3DArray.OnNameChange := frmGoPhast.PhastModel.DataArrayNameChange;
    frmGoPhast.PhastModel.AddDataSet(New3DArray);
    New3DArray.DataType := rdtDouble;
    New3DArray.Orientation := dso3D;
//    Grid := frmGoPhast.PhastModel.ModflowGrid;
    frmGoPhast.PhastModel.UpdateDataArrayDimensions(New3DArray);
//    New3DArray.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
//      Grid.ColumnCount);
    New3DArray.EvaluatedAt := eaBlocks;
    New3DArray.Classification := StrThreeDData;
    New3DArray.OnDataSetUsed := AModel.ModelResultsRequired;
    frmGoPhast.PhastModel.CreateVariables(New3DArray);

    New3DArray := AModel.DataArrayManager.GetDataSetByName(NewName);
    Grid := AModel.ModflowGrid;
    AModel.UpdateDataArrayDimensions(New3DArray);
//    New3DArray.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
//      Grid.ColumnCount);
  end
  else
  begin
    New3DArray := AModel.DataArrayManager.GetDataSetByName(NewName);
    New3DArray.Orientation := dso3D;
    Grid := AModel.ModflowGrid;
    AModel.UpdateDataArrayDimensions(New3DArray);
//    New3DArray.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
//      Grid.ColumnCount);
  end;
  AdjustTotalTime(TOTIM);
  if New3DArray.Model = frmGoPhast.PhastModel then
  begin
    Parent3DArray := New3DArray;
  end
  else
  begin
    Parent3DArray := frmGoPhast.PhastModel.DataArrayManager.
      GetDataSetByName(New3DArray.Name);
  end;
  OldComment := Parent3DArray.Comment;
  if (Parent3DArray = New3DArray) or (Parent3DArray.Comment = '') then
  begin
    Parent3DArray.Comment := Format(StrReadFrom0sOn,
      [FileNames, DateTimeToStr(Now), KPER, KSTP, TOTIM]);
    if FResultFormat = mfMt3dConc then
    begin
      Parent3DArray.Comment := Parent3DArray.Comment
        + sLineBreak + StrTransportStep + IntToStr(NTRANS)
    end;
  end;
  if Grid.LayerCount = 1 then
  begin
    LayerPosition := LayerNumbers.IndexOf(1);
    Assert(LayerPosition = 0);
    DataArray := LayerDataSets[LayerPosition];
    NewFormula := DataArray.Name;
  end
  else
  begin
    NewFormula := 'CaseR(Layer, ';
    for LayerIndex := 1 to Grid.LayerCount do
    begin
      LayerPosition := LayerNumbers.IndexOf(LayerIndex);
      if LayerPosition >= 0 then
      begin
        DataArray := LayerDataSets[LayerPosition];
        NewFormula := NewFormula + DataArray.Name;
        if LayerIndex < Grid.LayerCount then
        begin
          NewFormula := NewFormula + ', ';
        end;
      end
      else
      begin
        if FluxData then
        begin
          NewFormula := NewFormula + '0, ';
        end
        else
        begin
          LayerPosition := LayerNumbers.IndexOf(LayerIndex-1);
          Assert(LayerPosition >= 0);
          DataArray := LayerDataSets[LayerPosition];
          NewFormula := NewFormula + '((' + DataArray.Name + ' + ';

          LayerPosition := LayerNumbers.IndexOf(LayerIndex+1);
          Assert(LayerPosition >= 0);
          DataArray := LayerDataSets[LayerPosition];
          NewFormula := NewFormula + DataArray.Name + ') / 2.), ';
        end;
      end;
    end;
    NewFormula := NewFormula + ')';
    NewFormula := 'If((Layer <= ' + IntToStr(Grid.LayerCount)
      + '), ' + NewFormula + ', 0)'
  end;
  Assigner := FFormulaAssigners[Parent3DArray];
  Assigner.AddFormula(NewFormula, AModel);
//  New3DArray.Formula := NewFormula;
  LayerDataSets.Clear;
  LayerNumbers.Clear;
  if FModifiedParentDataSets.IndexOf(Parent3DArray) < 0 then
  begin
    FModifiedParentDataSets.Add(Parent3DArray);
  end;
end;

procedure TfrmSelectResultToImport.btnOKClick(Sender: TObject);
var
  RowIndex: Integer;
  DisplayChoice: TDisplayChoice;
begin
  inherited;
  for RowIndex := 1 to rdgModels.RowCount - 1 do
  begin
    if rdgModels.Checked[Ord(mcUse), RowIndex]
      and not FileExists(rdgModels.Cells[Ord(mcFileName), RowIndex]) then
    begin
      Beep;
      MessageDlg(StrAtLeastOneOfThe, mtError, [mbOK], 0);
      ModalResult := mrNone;
      rdgModels.Row := RowIndex;
      rdgModels.Col := Ord(mcUse);
      Exit;
    end;
  end;

  SetData;
  
  DisplayChoice := TDisplayChoice(rgDisplayChoice.ItemIndex);
  Inc(DisplayChoices[DisplayChoice]);
end;

procedure TfrmSelectResultToImport.btnSelectAllClick(Sender: TObject);
begin
  inherited;
  clData.CheckAll;
  UpdateCombo;
end;

procedure TfrmSelectResultToImport.btnSelectNoneClick(Sender: TObject);
begin
  inherited;
  clData.UnCheckAll;
  UpdateCombo;
end;

procedure TfrmSelectResultToImport.AssignLimits( MinValues, MaxValues: TRealList;
  New3DArray: TDataArray; ValuesToIgnore: TOneDRealArray);
var
  IgnoreIndex: Integer;
  SkipReal: TSkipReal;
  ParentArray: TDataArray;
begin
  MinValues.Sort;
  MaxValues.Sort;
  ParentArray := frmGoPhast.PhastModel.DataArrayManager.
    GetDataSetByName(New3DArray.Name);
  if ParentArray = New3DArray then
  begin
    if MinValues.Count > 0 then
    begin
      ParentArray.Limits.LowerLimit.UseLimit := True;
      ParentArray.Limits.LowerLimit.RealLimitValue := MinValues[0];
      ParentArray.Limits.UpperLimit.UseLimit := True;
      ParentArray.Limits.UpperLimit.RealLimitValue :=
        MaxValues[MaxValues.Count -1];
    end;
    ParentArray.Limits.RealValuesToSkip.Clear;
    for IgnoreIndex := 0 to Length(ValuesToIgnore) - 1 do
    begin
      SkipReal := ParentArray.Limits.RealValuesToSkip.Add as TSkipReal;
      SkipReal.RealValue := ValuesToIgnore[IgnoreIndex];
    end;
    ParentArray.ContourLimits := ParentArray.Limits;
  end
  else
  begin
    if MinValues.Count > 0 then
    begin
      ParentArray.Limits.LowerLimit.UseLimit := True;
      ParentArray.Limits.LowerLimit.RealLimitValue :=
        Min(MinValues[0], ParentArray.Limits.LowerLimit.RealLimitValue);
      ParentArray.Limits.UpperLimit.UseLimit := True;
      ParentArray.Limits.UpperLimit.RealLimitValue :=
        Max(MaxValues[MaxValues.Count -1], ParentArray.Limits.UpperLimit.RealLimitValue);
    end;
    for IgnoreIndex := 0 to Length(ValuesToIgnore) - 1 do
    begin
      if ParentArray.Limits.RealValuesToSkip.IndexOf(ValuesToIgnore[IgnoreIndex]) < 0 then
      begin
        SkipReal := ParentArray.Limits.RealValuesToSkip.Add as TSkipReal;
        SkipReal.RealValue := ValuesToIgnore[IgnoreIndex];
      end;
    end;
    ParentArray.ContourLimits := ParentArray.Limits;
  end;
  ParentArray.Limits.Update;
  MinValues.Clear;
  MaxValues.Clear;
end;

procedure TfrmSelectResultToImport.SetData;
var
  Index: Integer;
  KSTP: Integer;
  KPER: Integer;
  ILAY: Integer;
  AnArray: TModflowDoubleArray;
  WaterTableArray: TModflowDoubleArray;
  LayerArray: TModflowDoubleArray;
  EndReached: Boolean;
  LayerData: TDataArray;
  Description: string;
  ScreenObject: TScreenObject;
  LayerNumbers: TIntegerList;
  LayerDataSets: TList;
  Count: integer;
  New3DArray: TDataArray;
  A3DArray: T3DTModflowArray;
  LayerIndex: Integer;
  NLAY: Integer;
  NewDataSets: TList;
  UndoImportResults : TUndoImportModelResults;
  UndoChangeDataSets: TUndoChangeDataSets;
  DeletedDataSets: TList;
  NewDataSetProperties : TObjectList;
  DataArray: TDataArray;
  DataStorage: TPhastDataSetStorage;
  ScreenObjectsToDelete: TScreenObjectList;
  UndoDeleteScreenObjects: TUndoDeleteScreenObjects;
  NewCreateScreenObjects: TList;
  MinValues: TRealList;
  MaxValues: TRealList;
  OldComments: TStringList;
  OldComment: string;
  DataSetNames: TStringList;
  ColIndex: Integer;
  Precision: TModflowPrecision;
  HufFormat: boolean;
  HGU: THydrogeologicUnit;
  LayerDescription: string;
  ValuesToIgnore: TOneDRealArray;
  MinMaxAssigned: Boolean;
  TOTIM: TModflowDouble;
//  WaterTableData: TDataArray;
  DisplayChoice: TDisplayChoice;
  AModel: TCustomModel;
  AFileName: string;
  LastItem: Integer;
  RowIndex: Integer;
  ParentArray: TDataArray;
  FileNames: string;
  AParentArray: TDataArray;
  UndoCreateObject: TUndoCreateScreenObject;
  Mt3dComponentName: string;
  LastCharIndex: Integer;
  CharIndex: Integer;
  NTRANS: Integer;
begin
  inherited;
  FModifiedParentDataSets.Clear;
  MinValues := TRealList.Create;
  MaxValues := TRealList.Create;
  NewDataSets := TList.Create;
  OldComments := TStringList.Create;
  DataSetNames := TStringList.Create;
  ScreenObjectsToDelete := TScreenObjectList.Create;
  NewCreateScreenObjects := TList.Create;
  FileNames := '';
  Mt3dComponentName := '';
  for RowIndex := 1 to rdgModels.RowCount - 1 do
  begin
    if rdgModels.Checked[Ord(mcUse), RowIndex] then
    begin
      AFileName := rdgModels.Cells[Ord(mcFileName), RowIndex];
      Assert(FileExists(AFileName));
      if FileNames <> '' then
      begin
        FileNames := FileNames + sLineBreak;
      end;
      FileNames := FileNames + AFileName;
      if (FResultFormat = mfMt3dConc) and (Mt3dComponentName = '') then
      begin
        AFileName := ChangeFileExt(ExtractFileName(AFileName), '');
        LastCharIndex := Length(AFileName);
        if (LastCharIndex > 1) and (Copy(AFileName, LastCharIndex-1, 2) = '_S') then
        begin
          LastCharIndex := LastCharIndex -2;
        end;
        for CharIndex := LastCharIndex downto 1 do
        begin
          if AFileName[CharIndex] = '_' then
          begin
            Mt3dComponentName := ' ' + Copy(AFileName, CharIndex+1, MAXINT);
            break;
          end;
        end;
      end;
    end;
  end;
  try
    FMaxLayer := 0;
    for RowIndex := 1 to rdgModels.RowCount - 1 do
    begin
      if rdgModels.Checked[Ord(mcUse), RowIndex] then
      begin
        AModel := rdgModels.Objects[Ord(mcModelName), RowIndex] as TCustomModel;
        FMaxLayer := Max(FMaxLayer, AModel.ModflowLayerCount);
      end;
    end;
    for RowIndex := 1 to rdgModels.RowCount - 1 do
    begin
      if rdgModels.Checked[Ord(mcUse), RowIndex] then
      begin
        AFileName := rdgModels.Cells[Ord(mcFileName), RowIndex];
        Assert(FileExists(AFileName));

        LayerNumbers:= TIntegerList.Create;
        LayerDataSets := TList.Create;
        try

          AModel := rdgModels.Objects[Ord(mcModelName), RowIndex] as TCustomModel;
          FGrid := AModel.ModflowGrid;
          FFileStream := nil;
          FFileVariable := nil;
          FMaxPeriod := Max(frmGoPhast.PhastModel.ModflowStressPeriods.Count,
            frmGoPhast.PhastModel.ModflowFullStressPeriods.Count);
          FMaxStep := frmGoPhast.PhastModel.ModflowStressPeriods.MaxStepsInAnyStressPeriod;
//          FMaxLayer := AModel.ModflowLayerCount;
          OpenResultFile(AFileName, Precision, HufFormat);
          EndReached := False;
          KPER := -1;
          KSTP := -1;
          Description := '';
          Count := 0;
          NLAY := 1;
          if FResultFormat = mrFlux then
          begin
            SetLength(ValuesToIgnore, 0);
          end
          else if FResultFormat = mfMt3dConc then
          begin
            SetLength(ValuesToIgnore, 1);
            ValuesToIgnore[0] := AModel.ModflowPackages.Mt3dBasic.InactiveConcentration;
          end
          else
          begin
            SetLength(ValuesToIgnore, 2);
            ValuesToIgnore[0] := AModel.ModflowOptions.HDry;
            ValuesToIgnore[1] := AModel.ModflowOptions.HNoFlow;
          end;
          LastItem := 0;
          for Index := clData.Items.Count - 1 downto 0 do
          begin
            if clData.Checked[Index] then
            begin
              LastItem := Index;
              break;
            end;
          end;
          for Index := 0 to LastItem do
          begin
            case FResultFormat of
              mrBinary, mrAscii, mfMt3dConc:
                begin
                  if Index = 0 then
                  begin
                    ReadArray(AnArray, EndReached,
                      NTRANS, KPER, KSTP, ILAY, TOTIM, Description, Precision,
                      clData.Checked[Index]);
                  end;
                  While (KPER = FPeriods[Index])
                    and (KSTP = FSteps[Index])
                    and (NTRANS = FTransportSteps[Index])
                    and (Description = FDescriptions[Index])
                    and not EndReached do
                  begin
                    if ILAY < 0 then
                    begin
                      // cross section
                      for LayerIndex := 1 to AModel.ModflowLayerCount do
                      begin
                        ILAY := LayerIndex;
                        ILAY := AModel.ModflowLayerToDataSetLayer(ILAY)+1;
                        if clData.Checked[Index] then
                        begin
                          SetLength(LayerArray, 1, Length(AnArray[0]));
                          for ColIndex := 0 to Length(AnArray[0]) - 1 do
                          begin
                            LayerArray[0,ColIndex] := AnArray[LayerIndex-1,ColIndex];
                          end;
                          AssignWaterTableArray(WaterTableArray,
                            ILAY, LayerArray, ValuesToIgnore, Description);
                          CreateOrRetrieveLayerDataSet(Description, NTRANS, KSTP, KPER, ILAY,
                            TOTIM, LayerData, OldComment, NewDataSets,
                            ScreenObjectsToDelete, FileNames, AModel);
                          CreateScreenObject(ILAY-1, AModel, ScreenObject);
                          AssignValues(ILAY-1, ScreenObject, LayerData, LayerArray,
                            ValuesToIgnore, AModel, MinMaxAssigned);
                          LayerNumbers.Add(ILAY);
                          LayerDataSets.Add(LayerData);
                          Assert(FModifiedParentDataSets.Count> 0);
                          UpdateOldComments(OldComments, LayerData, OldComment);
                          DataSetNames.AddObject(LayerData.Name, LayerData);
                          if MinMaxAssigned then
                          begin
                            MinValues.Add(LayerData.Limits.LowerLimit.RealLimitValue);
                            MaxValues.Add(LayerData.Limits.UpperLimit.RealLimitValue);
                          end;

                          if ILAY = FGrid.LayerCount then
                          begin
                            Inc(Count);
                            CreateOrRetrieve3DDataSet(Description, NTRANS, KPER, KSTP, TOTIM,
                              LayerNumbers, LayerDataSets, New3DArray, OldComment,
                              False, NewDataSets, FileNames, AModel);
                            UpdateOldComments(OldComments, New3DArray, OldComment);
                            DataSetNames.AddObject(New3DArray.Name, New3DArray);
                            ParentArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(New3DArray.Name);
                            comboColorGrid.Items.Objects[Count] := ParentArray;

                            AssignLimits(MinValues, MaxValues, New3DArray,
                              ValuesToIgnore);
                          end;
                          UndoCreateObject := TUndoCreateScreenObject.Create(ScreenObject);
                          UndoCreateObject.UpdateObservations;
                          NewCreateScreenObjects.Add(UndoCreateObject);
                        end
                      end;
                      if clData.Checked[Index] then
                      begin
                        AssignWaterTable(NewDataSets, OldComments, DataSetNames,
                          ScreenObjectsToDelete, NewCreateScreenObjects, NTRANS, KPER,
                          WaterTableArray, KSTP, Description, ValuesToIgnore,
                          TOTIM, FileNames, AModel);
                      end;
                    end
                    else
                    begin
                      Assert(ILAY > 0);
                      if ILAY > FMaxLayer then
                      begin
                        Beep;
                        MessageDlg(StrTheFileYouAreTry, mtError, [mbOK], 0);
                        Exit;
                      end;
                      // not a cross section
                      ILAY := AModel.ModflowLayerToDataSetLayer(ILAY)+1;
                      if clData.Checked[Index] then
                      begin
                        AssignWaterTableArray(WaterTableArray, ILAY, AnArray,
                          ValuesToIgnore, Description);
                        CreateOrRetrieveLayerDataSet(
                          Description+Mt3dComponentName, NTRANS, KSTP, KPER, ILAY,
                          TOTIM, LayerData, OldComment, NewDataSets,
                          ScreenObjectsToDelete, FileNames, AModel);
                        CreateScreenObject(ILAY-1, AModel, ScreenObject);
                        AssignValues(ILAY-1, ScreenObject, LayerData, AnArray,
                          ValuesToIgnore, AModel, MinMaxAssigned);
                        LayerNumbers.Add(ILAY);
                        LayerDataSets.Add(LayerData);
                        UpdateOldComments(OldComments, LayerData, OldComment);
                        DataSetNames.AddObject(LayerData.Name, LayerData);
                        if MinMaxAssigned then
                        begin
                          MinValues.Add(LayerData.Limits.LowerLimit.RealLimitValue);
                          MaxValues.Add(LayerData.Limits.UpperLimit.RealLimitValue);
                        end;

                        if ILAY = FGrid.LayerCount then
                        begin
                          Inc(Count);
                          CreateOrRetrieve3DDataSet(
                            Description+Mt3dComponentName, NTRANS, KPER, KSTP, TOTIM,
                            LayerNumbers, LayerDataSets, New3DArray, OldComment,
                            False, NewDataSets, FileNames, AModel);
                          UpdateOldComments(OldComments, New3DArray, OldComment);
                          DataSetNames.AddObject(New3DArray.Name, New3DArray);
                          ParentArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(New3DArray.Name);
                          comboColorGrid.Items.Objects[Count] := ParentArray;

                          AssignLimits(MinValues, MaxValues, New3DArray,
                            ValuesToIgnore);

                        end;
                        UndoCreateObject := TUndoCreateScreenObject.Create(ScreenObject);
                        UndoCreateObject.UpdateObservations;
                        NewCreateScreenObjects.Add(UndoCreateObject);
//                        NewCreateScreenObjects.Add(
//                          TUndoCreateScreenObject.Create(ScreenObject));
                        if ILAY = FGrid.LayerCount then
                        begin
                          AssignWaterTable(NewDataSets, OldComments, DataSetNames,
                            ScreenObjectsToDelete, NewCreateScreenObjects, NTRANS, KPER,
                            WaterTableArray, KSTP, Description, ValuesToIgnore,
                            TOTIM, FileNames, AModel);
                        end;
                      end;
                    end;

                    // read next array
                    if ILAY = FGrid.LayerCount then
                    begin
                      ReadArray(AnArray, EndReached,
                        NTRANS, KPER, KSTP, ILAY, TOTIM, Description, Precision,
                        (LastItem <> Index) and clData.Checked[Index+1])
                    end
                    else
                    begin
                      ReadArray(AnArray, EndReached,
                        NTRANS, KPER, KSTP, ILAY, TOTIM, Description, Precision,
                        clData.Checked[Index])
                    end;
                  end;
                end;
              mrFlux:
                begin
                  Read3DArray(NLAY, EndReached, KPER, KSTP, TOTIM, Description,
                    A3DArray, Precision, HufFormat, clData.Checked[Index]);
                  if clData.Checked[Index] then
                  begin
                    for LayerIndex := 0 to Abs(NLAY) - 1 do
                    begin
                      ILAY := AModel.ModflowLayerToDataSetLayer(LayerIndex+1)+1;
                      CreateOrRetrieveLayerDataSet(Description, NTRANS, KSTP, KPER, ILAY,
                        TOTIM, LayerData, OldComment, NewDataSets,
                        ScreenObjectsToDelete, FileNames, AModel);
                      CreateScreenObject(ILAY-1, AModel, ScreenObject);
                      Assign3DValues(ScreenObject, LayerData, A3DArray, LayerIndex,
                        False, ValuesToIgnore, AModel);
                      LayerNumbers.Add(ILAY);
                      LayerDataSets.Add(LayerData);
                      UpdateOldComments(OldComments, LayerData, OldComment);
                      DataSetNames.AddObject(LayerData.Name, LayerData);
                      MinValues.Add(LayerData.Limits.LowerLimit.RealLimitValue);
                      MaxValues.Add(LayerData.Limits.UpperLimit.RealLimitValue);
                      UndoCreateObject := TUndoCreateScreenObject.Create(ScreenObject);
                      UndoCreateObject.UpdateObservations;
                      NewCreateScreenObjects.Add(UndoCreateObject);
//                      NewCreateScreenObjects.Add(
//                        TUndoCreateScreenObject.Create(ScreenObject))
                    end;
                    Inc(Count);
                    CreateOrRetrieve3DDataSet(Description, NTRANS, KPER, KSTP, TOTIM,
                      LayerNumbers, LayerDataSets, New3DArray, OldComment, True,
                      NewDataSets, FileNames, AModel);
                    UpdateOldComments(OldComments, New3DArray, OldComment);
                    DataSetNames.AddObject(New3DArray.Name, New3DArray);
                    ParentArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(New3DArray.Name);
                    comboColorGrid.Items.Objects[Count] := ParentArray;

                    AssignLimits(MinValues, MaxValues, New3DArray, ValuesToIgnore);
                  end;

                end;
              mrHufAscii, mrHufBinary:
                begin
                  ReadArray(AnArray, EndReached,
                    NTRANS, KPER, KSTP, ILAY, TOTIM, Description, Precision,
                    clData.Checked[Index]);
                  Assert((KPER = FPeriods[Index])
                    and (KSTP = FSteps[Index])
                    and (Description = FDescriptions[Index]));
                  Assert( ILAY > 0);
                  // not a cross section
                  if clData.Checked[Index] then
                  begin
                    HGU := AModel.HydrogeologicUnits[ILAY-1];
                    Description := Description + ' ' + HGU.HufName;
                    CreateOrRetrieveLayerDataSet(Description, NTRANS, KSTP, KPER, ILAY, TOTIM,
                      LayerData, OldComment, NewDataSets,
                      ScreenObjectsToDelete, FileNames, AModel);
                    CreateScreenObject(-1, AModel, ScreenObject);
                    AssignValues(-1, ScreenObject, LayerData, AnArray,
                      ValuesToIgnore, AModel, MinMaxAssigned);
                    UpdateOldComments(OldComments, LayerData, OldComment);
                    DataSetNames.AddObject(LayerData.Name, LayerData);
                    Inc(Count);
                    ParentArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(LayerData.Name);
                    comboColorGrid.Items.Objects[Count] := ParentArray;
                    UndoCreateObject := TUndoCreateScreenObject.Create(ScreenObject);
                    UndoCreateObject.UpdateObservations;
                    NewCreateScreenObjects.Add(UndoCreateObject);
//                    NewCreateScreenObjects.Add(
//                      TUndoCreateScreenObject.Create(ScreenObject))
                  end;
                end;
              mrHufFlux:
                begin
                  if (Index = 0) or ((Index mod NLAY) = 0) then
                  begin
                    Read3DArray(NLAY, EndReached, KPER, KSTP, TOTIM, Description,
                      A3DArray, Precision, HufFormat, clData.Checked[Index]);
                  end;

                  if clData.Checked[Index] then
                  begin
                    LayerIndex := Index mod NLAY;
                    ILAY := LayerIndex+1;
                    HGU := AModel.HydrogeologicUnits[ILAY-1];
                    LayerDescription := Description + ' ' + HGU.HufName;
                    CreateOrRetrieveLayerDataSet(LayerDescription, NTRANS, KSTP, KPER, ILAY,
                      TOTIM, LayerData, OldComment, NewDataSets,
                      ScreenObjectsToDelete, FileNames, AModel);
                    CreateScreenObject(-1, AModel, ScreenObject);
                    Assign3DValues(ScreenObject, LayerData, A3DArray, LayerIndex,
                      True, ValuesToIgnore, AModel);
                    UpdateOldComments(OldComments, LayerData, OldComment);
                    DataSetNames.AddObject(LayerData.Name, LayerData);
                    Inc(Count);
                    ParentArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(LayerData.Name);
                    comboColorGrid.Items.Objects[Count] := ParentArray;
                    UndoCreateObject := TUndoCreateScreenObject.Create(ScreenObject);
                    UndoCreateObject.UpdateObservations;
                    NewCreateScreenObjects.Add(UndoCreateObject);
//                    NewCreateScreenObjects.Add(
//                      TUndoCreateScreenObject.Create(ScreenObject))
                  end;
                end;
              mfSubBinary:
                begin
                  ReadArray(AnArray, EndReached,
                    NTRANS, KPER, KSTP, ILAY, TOTIM, Description, Precision,
                    clData.Checked[Index]);
                  Description := SubsidenceDescription(Description, ILAY);
                  if clData.Checked[Index] then
                  begin
                    Inc(Count);
                    CreateOrRetrieveLayerDataSet(Description, NTRANS, KSTP, KPER, ILAY, TOTIM,
                      LayerData, OldComment, NewDataSets, ScreenObjectsToDelete,
                      FileNames, AModel, dafSubsidence);
                    ParentArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(LayerData.Name);
                    comboColorGrid.Items.Objects[Count] := ParentArray;
                    CreateScreenObject(ILAY-1, AModel, ScreenObject);
                    AssignValues(ILAY-1, ScreenObject, LayerData, AnArray,
                      ValuesToIgnore, AModel, MinMaxAssigned);
                    LayerNumbers.Add(ILAY);
                    LayerDataSets.Add(LayerData);
                    UpdateOldComments(OldComments, LayerData, OldComment);
                    DataSetNames.AddObject(LayerData.Name, LayerData);
                    if MinMaxAssigned then
                    begin
                      MinValues.Add(LayerData.Limits.LowerLimit.RealLimitValue);
                      MaxValues.Add(LayerData.Limits.UpperLimit.RealLimitValue);
                    end;
                  end;

                end;
              else Assert(False);
            end;
          end;
        finally
          CloseFiles;
          LayerNumbers.Free;
          LayerDataSets.Free;
        end;
      end;
    end;
    New3DArray := comboColorGrid.Items.Objects[comboColorGrid.ItemIndex]
      as TDataArray;
    DisplayChoice := TDisplayChoice(rgDisplayChoice.ItemIndex);
    if New3DArray <> nil then
    begin
      case DisplayChoice of
        dcColor:
          begin
            frmGoPhast.acColoredGrid.Enabled := True;
            frmGoPhast.acColoredGrid.Checked := True;
            frmGoPhast.tb3DColors.Down := True;
          end;
        dcContour, dcNone:
          begin
            // do nothing
          end;
        else Assert(False);
      end;
    end;
    for Index := 0 to FModifiedParentDataSets.Count - 1 do
    begin
      AParentArray := FModifiedParentDataSets[Index];
      AParentArray.Comment := AParentArray.Comment
        + sLineBreak + Format(StrMinimumValueG,
        [AParentArray.Limits.LowerLimit.RealLimitValue])
        + sLineBreak + Format(StrMaximumValueG,
        [AParentArray.Limits.UpperLimit.RealLimitValue]);
    end;
    FFormulaAssigners.AssignFinalFormulas;
    UndoImportResults := TUndoImportModelResults.Create(NewDataSets,
      DataSetNames, OldComments, New3DArray, DisplayChoice, frmGoPhast.PhastModel);
    DeletedDataSets := TList.Create;
    NewDataSetProperties := TObjectList.Create;
    try
      for Index := 0 to NewDataSets.Count - 1 do
      begin
        DataArray := NewDataSets[Index];
        DataStorage := TPhastDataSetStorage.Create;
        DataStorage.Assign(DataArray);
        NewDataSetProperties.Add(DataStorage);
      end;
      UndoChangeDataSets := TUndoChangeDataSets.Create(
        DeletedDataSets, NewDataSets, NewDataSetProperties);
      UndoImportResults.FContainedUndos.Add(UndoChangeDataSets);
    finally
      DeletedDataSets.Free;
      NewDataSetProperties.Free;
    end;

    if ScreenObjectsToDelete.Count > 0 then
    begin
      UndoDeleteScreenObjects:=
        TUndoDeleteScreenObjects.Create(ScreenObjectsToDelete);
      UndoImportResults.FContainedUndos.Add(UndoDeleteScreenObjects);
    end;

    for Index := 0 to NewCreateScreenObjects.Count - 1 do
    begin
      UndoImportResults.FContainedUndos.Add(NewCreateScreenObjects[Index]);
    end;

    frmGoPhast.UndoStack.Submit(UndoImportResults);
  finally
    NewDataSets.Free;
    ScreenObjectsToDelete.Free;
    NewCreateScreenObjects.Free;
    MinValues.Free;
    MaxValues.Free;
    OldComments.Free;
    DataSetNames.Free;
  end;

end;

function TfrmSelectResultToImport.SubsidenceDescription(DESC: String;
  ILAY: integer): string;
begin
  result := string(Trim(DESC));
  if SameText(result, 'SUBSIDENCE') then
  begin
    Exit;
  end
  else if SameText(result, 'NDSYS COMPACTION')
    or SameText(result, 'ND CRITICAL HEAD')
    or SameText(result, 'DSYS COMPACTION')
    or SameText(result, 'D CRITICAL HEAD')
    or SameText(result, 'SYSTM COMPACTION') then
  begin
    result := result + ' System ' + IntToStr(ILAY);
  end
  else
  begin
    result := result + ' Layer ' + IntToStr(ILAY);
  end;
end;

procedure TfrmSelectResultToImport.clDataClickCheck(Sender: TObject);
begin
  inherited;
  UpdateCombo;
end;

procedure TfrmSelectResultToImport.FormCreate(Sender: TObject);
var
  FilterDescriptions: TStringList;
  FileExtensions: TStringList;
  index: Integer;
  SubsidenceDescriptions: TStringList;
  SubsidenceExtensions: TStringList;
begin
  inherited;
  FModifiedParentDataSets:= TList.Create;
  FFormulaAssigners := TFormulaAssignerList.Create;

  rdgModels.Cells[Ord(mcModelName), 0] := StrModel;
  rdgModels.Cells[Ord(mcUse), 0] := StrImportData;
  rdgModels.Cells[Ord(mcFileName), 0] := StrFileName;

  FNewDataSetNames:= TStringList.Create;
  FNewDefaultDataSetNames:= TStringList.Create;

  FPeriods := TIntegerList.Create;
  FSteps := TIntegerList.Create;
  FTransportSteps := TIntegerList.Create;

  FDescriptions := TStringList.Create;
  FAskedUser := False;
  SetDefaultDisplayOption;

  FilterDescriptions := TStringList.Create;
  FileExtensions := TStringList.Create;
  SubsidenceDescriptions := TStringList.Create;
  SubsidenceExtensions := TStringList.Create;
  try
    FilterDescriptions.Add(StrFormattedHeadFiles);
    FileExtensions.Add(StrFhd);

    FilterDescriptions.Add(StrFormattedDrawdownF);
    FileExtensions.Add(StrFdn);

    FilterDescriptions.Add(StrBinaryHeadFiles);
    FileExtensions.Add(StrBhd);

    FilterDescriptions.Add(StrBinaryDrawdownFile);
    FileExtensions.Add(StrBdn);

    FilterDescriptions.Add(StrBinaryFlowFiles);
    FileExtensions.Add(StrCbcExt);

    FilterDescriptions.Add(StrFormattedHUFHeadF);
    FileExtensions.Add(StrHuffhd);

    FilterDescriptions.Add(StrBinaryHUFHeadFile);
    FileExtensions.Add(StrHufbhd);

    FilterDescriptions.Add(StrHUFFlowFiles);
    FileExtensions.Add(StrHufflow);

    FilterDescriptions.Add(StrMT3DMSConcentration);
    FileExtensions.Add(StrMt3dConcFile);

    SubsidenceDescriptions.Add(StrCombinedSUBOutput);
    SubsidenceExtensions.Add(StrSubOut);

    SubsidenceDescriptions.Add(StrCombinedSWTOutput);
    SubsidenceExtensions.Add(StrSwtOut);

    SubsidenceDescriptions.Add(StrSUBSubsidence);
    SubsidenceExtensions.Add(StrSubSubOut);

    SubsidenceDescriptions.Add(StrSUBCompactionByMo);
    SubsidenceExtensions.Add(StrSubComMlOut);

    SubsidenceDescriptions.Add(StrSUBCompactionByIn);
    SubsidenceExtensions.Add(StrSubComIsOut);

    SubsidenceDescriptions.Add(StrSUBVerticalDisplac);
    SubsidenceExtensions.Add(StrSubVdOut);

    SubsidenceDescriptions.Add(StrSUBCriticalHeadFo);
    SubsidenceExtensions.Add(StrSubNdCritHeadOut);

    SubsidenceDescriptions.Add(StrSUBCriticalHeadFoDelay);
    SubsidenceExtensions.Add(StrSubDCritHeadOut);

    SubsidenceDescriptions.Add(StrSWTSubsidence);
    SubsidenceExtensions.Add(StrSwtSubOut);

    SubsidenceDescriptions.Add(StrSWTCompactionByMo);
    SubsidenceExtensions.Add(StrSwtComMLOut);

    SubsidenceDescriptions.Add(StrSWTCompactionByIn);
    SubsidenceExtensions.Add(StrSwtComIsOut);

    SubsidenceDescriptions.Add(StrSWTVerticalDisplac);
    SubsidenceExtensions.Add(StrSwtVDOut);

    SubsidenceDescriptions.Add(StrSWTPreconsolidation);
    SubsidenceExtensions.Add(StrSwtDeltaPreConStrOu);

    SubsidenceDescriptions.Add(StrSWTChangeInPrecon);
    SubsidenceExtensions.Add(StrSwtDeltaPreConStrOu);

    SubsidenceDescriptions.Add(StrSWTGeostaticStress);
    SubsidenceExtensions.Add(StrSwtGeoStatOut);

    SubsidenceDescriptions.Add(StrSWTChangeInGeosta);
    SubsidenceExtensions.Add(StrSwtDeltaGeoStatOut);

    SubsidenceDescriptions.Add(StrSWTEffectiveStress);
    SubsidenceExtensions.Add(StrSwtEffStressOut);

    SubsidenceDescriptions.Add(StrSWTChangeInEffect);
    SubsidenceExtensions.Add(StrSwtDeltaEffStressOu);

    SubsidenceDescriptions.Add(StrSWTVoidRatio);
    SubsidenceExtensions.Add(StrSwtVoidRatioOut);

    SubsidenceDescriptions.Add(StrSWTThicknessOfCom);
    SubsidenceExtensions.Add(StrSwtThickCompSedOut);

    SubsidenceDescriptions.Add(StrSWTLayercenterEle);
    SubsidenceExtensions.Add(StrSwtLayerCentElevOut);

    odSelectFiles.Filter := StrCommonSupportedFil + Trim(FileExtensions[0]);
    for index := 1 to FileExtensions.Count - 1 do
    begin
      odSelectFiles.Filter := odSelectFiles.Filter
        + ';*' + Trim(FileExtensions[index]);
    end;

    odSelectFiles.Filter := odSelectFiles.Filter + StrSubsidenceFiles + SubsidenceExtensions[0];
    for index := 1 to SubsidenceExtensions.Count - 1 do
    begin
      odSelectFiles.Filter := odSelectFiles.Filter
        + ';*' + Trim(SubsidenceExtensions[index]);
    end;

    Assert(FileExtensions.Count = FilterDescriptions.Count);
    for index := 0 to FileExtensions.Count - 1 do
    begin
      odSelectFiles.Filter := odSelectFiles.Filter
        + '|' + FilterDescriptions[index]
        + '(*' + Trim(FileExtensions[index]) + ')|*' + Trim(FileExtensions[index]);
    end;

    Assert(SubsidenceExtensions.Count = SubsidenceDescriptions.Count);
    for index := 0 to SubsidenceExtensions.Count - 1 do
    begin
      odSelectFiles.Filter := odSelectFiles.Filter
        + '|' + SubsidenceDescriptions[index]
        + '(*' + Trim(SubsidenceExtensions[index]) + ')|*' + Trim(SubsidenceExtensions[index]);
    end;
  finally
    FilterDescriptions.Free;
    FileExtensions.Free;
    SubsidenceDescriptions.Free;
    SubsidenceExtensions.Free;
  end;

end;

procedure TfrmSelectResultToImport.FormDestroy(Sender: TObject);
begin
  inherited;
  FPeriods.Free;
  FSteps.Free;
  FTransportSteps.Free;
  FDescriptions.Free;
  FNewDataSetNames.Free;
  FNewDefaultDataSetNames.Free;
  FFormulaAssigners.Free;
  FModifiedParentDataSets.Free;
end;

procedure TfrmSelectResultToImport.odSelectFilesTypeChange(Sender: TObject);
var
  Dialog: TOpenDialog;
  NewFileName: string;
  Extension: string;
  Index: Integer;
  Position: integer;
  AChemSpecies: TChemSpeciesItem;
begin
  inherited;
  Dialog := Sender as TOpenDialog;
  if (Dialog.FilterIndex > 2) and (Dialog.FileName <> '') then
  begin
    Position := 1;
    for Index := 0 to Dialog.FilterIndex*2 - 2 do
    begin
      Position := PosEx('|', Dialog.Filter, Position+1);
    end;
    Extension := Copy(Dialog.Filter, Position+2,MAXINT);
    Position := Pos('|', Extension);
    if Position >= 1 then
    begin
      Extension := Copy(Extension, 1, Position-1);
    end;
    Position := Pos(';', Extension);
    if Position >= 1 then
    begin
      Extension := Copy(Extension, 1, Position-1);
    end;

    if SameText(Extension, StrMt3dConcFile) then
    begin
      NewFileName :=
        ChangeFileExt(Dialog.FileName, '');
      if frmGoPhast.PhastModel.MobileComponents.Count > 0 then
      begin
        AChemSpecies := frmGoPhast.PhastModel.MobileComponents[0];
      end
      else if frmGoPhast.PhastModel.ImmobileComponents.Count > 0 then
      begin
        AChemSpecies := frmGoPhast.PhastModel.ImmobileComponents[0];
      end
      else
      begin
        AChemSpecies := nil;
      end;
      if AChemSpecies <> nil then
      begin
        NewFileName := NewFileName + '_' + AChemSpecies.Name;
      end;
      NewFileName :=
        ChangeFileExt(NewFileName, Extension);
    end
    else
    begin
      NewFileName :=
        ChangeFileExt(Dialog.FileName, Extension);
    end;

    Dialog.FileName := NewFileName;
    UpdateDialogBoxFileName(Dialog, NewFileName);
  end;
end;

function TfrmSelectResultToImport.ReadDataHeadings(AModel: TCustomModel;
  RowIndex: integer; AFileName: string): boolean;
var
  KSTP: Integer;
  AnArray: TModflowDoubleArray;
  A3DArray: T3DTModflowArray;
  KPER: Integer;
  NTRANS: integer;
  PERTIM: TModflowDouble;
  TOTIM: TModflowDouble;
  DESC: TModflowDesc;
  NCOL: Integer;
  NROW: Integer;
  ILAY: Integer;
  Item: string;
  DESC2: TModflowDesc2;
  NLAY: Integer;
  Precision: TModflowPrecision;
  HufFormat: boolean;
  LayerIndex: Integer;
  Description: String;
  AFileStream: TFileStream;
  function WriteLabel(Description: string): string;
  var
    HGU: THydrogeologicUnit;
    HufName: string;
  begin
    Description := Trim(Description);
    Assert(Length(Description) > 0);
    Description := TitleCase(Description);
    if FResultFormat in [mrHufAscii, mrHufBinary, mrHufFlux] then
    begin
      HGU := AModel.HydrogeologicUnits[ILAY-1];
      HufName := ' ' + HGU.HufName;
    end
    else
    begin
      HufName := '';
    end;

    result := Format(Str0s1sPeriod2,
      [Description, HufName, KPER, KSTP]);
    if FResultFormat = mfMt3dConc then
    begin
      result := Format(Str0sTransportStep, [result, NTRANS]);
    end;
    if TOTIM >= 0 then
    begin
      result := Format(Str0sTotalTime1, [result, TOTIM]);
    end;
  end;
  procedure RecordItem(Description: String);
  begin
    Item := WriteLabel(Description);
    if clData.Items.IndexOf(Item) < 0 then
    begin
      FPeriods.Add(KPER);
      FSteps.Add(KSTP);
      FTransportSteps.Add(NTRANS);
      FDescriptions.Add(TitleCase(Trim(Description)));
      clData.Items.Add(Item);
    end;
  end;
begin
  result := True;
  NTRANS := 0;
  try
    try
      if not FileExists(AFileName) then
      begin
        result := False;
        Beep;
        MessageDlg(Format(StrSDoesNotExist, [AFileName]), mtError, [mbOK], 0);
        Exit;
      end;

      AFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
      try
        if AFileStream.Size = 0 then
        begin
          result := False;
          Beep;
          MessageDlg(Format(StrSIsEmpty, [AFileName]), mtError, [mbOK], 0);
          Exit;
        end;
      finally
        AFileStream.Free;
      end;

      FFileStream := nil;
      FFileVariable := nil;
      try
        if not OpenResultFile(AFileName, Precision, HufFormat) then
        begin
          result := False;
          Exit;
        end;
      except on EPrecisionReadError do
        begin
          result := False;
          Beep;
          MessageDlg(Format(StrErrorReadingS, [AFileName]), mtError, [mbOK], 0);
          Exit;
        end;
      end;

      case FResultFormat of
        mfMt3dConc:
          begin
            while FFileStream.Position < FFileStream.Size do
            begin
              case Precision of
                mpSingle:
                  ReadSinglePrecisionMt3dmsBinaryRealArray(FFileStream, NTRANS,
                    KSTP, KPER, TOTIM, DESC, NCOL, NROW, ILAY, AnArray, False);
                mpDouble:
                  ReadDoublePrecisionMt3dmsBinaryRealArray(FFileStream, NTRANS,
                    KSTP, KPER, TOTIM, DESC, NCOL, NROW, ILAY, AnArray, False);
                else Assert(False);
              end;
              RecordItem(string(DESC));
              if (AModel.ModflowGrid.RowCount <> NROW)
                or (AModel.ModflowGrid.ColumnCount <> NCOL) then
              begin
                Beep;
                MessageDlg(StrTheNumberOfRowsOrColumns,
                  mtError, [mbOK], 0);
                result := False;
                break;
              end;
            end;
          end;
        mrBinary:
          begin
            while FFileStream.Position < FFileStream.Size do
            begin
              case Precision of
                mpSingle:
                  ReadSinglePrecisionModflowBinaryRealArray(FFileStream, KSTP,
                    KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray, False);
                mpDouble:
                  ReadDoublePrecisionModflowBinaryRealArray(FFileStream, KSTP,
                    KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray, False);
                else Assert(False);
              end;
              RecordItem(string(DESC));
              if (AModel.ModflowGrid.RowCount = 1) then
              begin
                if (AModel.ModflowLayerCount <> NROW)
                  or (AModel.ModflowGrid.ColumnCount <> NCOL) then
                begin
                  Beep;
                  MessageDlg(StrTheNumberOfLayersOrCol,
                    mtError, [mbOK], 0);
                  result := False;
                  break;
                end;
              end
              else
              begin
                if (AModel.ModflowGrid.RowCount <> NROW)
                  or (AModel.ModflowGrid.ColumnCount <> NCOL) then
                begin
                  Beep;
                  MessageDlg(StrTheNumberOfRowsOrColumns,
                    mtError, [mbOK], 0);
                  result := False;
                  break;
                end;
              end;
            end;
          end;
        mrAscii:
          begin
            while not EOF(FFileVariable.AFile) do
            begin
              ReadModflowAsciiRealArray(FFileVariable, KSTP, KPER,
                PERTIM, TOTIM, DESC2, NCOL, NROW, ILAY, AnArray, False);
              RecordItem(string(DESC2));
              if AModel.ModflowGrid.RowCount = 1 then
              begin
                if (AModel.ModflowLayerCount <> NROW)
                  or (AModel.ModflowGrid.ColumnCount <> NCOL) then
                begin
                  Beep;
                  MessageDlg(StrTheNumberOfLayersOrCol,
                    mtError, [mbOK], 0);
                  result := False;
                  break;
                end;
              end
              else
              begin
                if (AModel.ModflowGrid.RowCount <> NROW)
                  or (AModel.ModflowGrid.ColumnCount <> NCOL) then
                begin
                  Beep;
                  MessageDlg(StrTheNumberOfRowsOrColumns,
                    mtError, [mbOK], 0);
                  result := False;
                  break;
                end;
              end;
            end;
          end;
        mrFlux:
          begin
            while FFileStream.Position < FFileStream.Size do
            begin
              case Precision of
                mpSingle:
                  ReadModflowSinglePrecFluxArray(FFileStream, KSTP, KPER,
                    PERTIM, TOTIM, DESC, NCOL, NROW, NLAY, A3DArray, HufFormat,
                    False);
                mpDouble:
                  ReadModflowDoublePrecFluxArray(FFileStream, KSTP, KPER,
                    PERTIM, TOTIM, DESC, NCOL, NROW, NLAY, A3DArray, HufFormat,
                    False);
                else Assert(False);
              end;
              RecordItem(string(DESC));
              if (AModel.ModflowGrid.RowCount <> NROW)
                or (AModel.ModflowGrid.ColumnCount <> NCOL)
                or (AModel.ModflowLayerCount <> Abs(NLAY)) then
              begin
                Beep;
                MessageDlg(StrTheNumberOfRows,
                  mtError, [mbOK], 0);
                result := False;
                break;
              end;
            end;
          end;
        mrHufAscii:
          begin
            while not EOF(FFileVariable.AFile) do
            begin
              ReadModflowAsciiRealArray(FFileVariable, KSTP, KPER,
                PERTIM, TOTIM, DESC2, NCOL, NROW, ILAY, AnArray, False);
              RecordItem(string(DESC2));
              if (AModel.ModflowGrid.RowCount <> NROW)
                or (AModel.ModflowGrid.ColumnCount <> NCOL) then
              begin
                Beep;
                MessageDlg(StrTheNumberOfRowsOrColumns,
                  mtError, [mbOK], 0);
                result := False;
                break;
              end;
            end
          end;
        mrHufBinary:
          begin
            while FFileStream.Position < FFileStream.Size do
            begin
              case Precision of
                mpSingle:
                  ReadSinglePrecisionModflowBinaryRealArray(FFileStream, KSTP, KPER,
                    PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray, False);
                mpDouble:
                  ReadDoublePrecisionModflowBinaryRealArray(FFileStream, KSTP, KPER,
                    PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray, False);
                else Assert(False);
              end;
              RecordItem(string(DESC));
              if (AModel.ModflowGrid.RowCount <> NROW)
                or (AModel.ModflowGrid.ColumnCount <> NCOL) then
              begin
                Beep;
                MessageDlg(StrTheNumberOfRowsOrColumns,
                  mtError, [mbOK], 0);
                result := False;
                break;
              end;
            end;
          end;
        mrHufFlux:
          begin
            while FFileStream.Position < FFileStream.Size do
            begin
              case Precision of
                mpSingle:
                  ReadModflowSinglePrecFluxArray(FFileStream, KSTP, KPER,
                    PERTIM, TOTIM, DESC, NCOL, NROW, NLAY, A3DArray, HufFormat,
                    False);
                mpDouble:
                  ReadModflowDoublePrecFluxArray(FFileStream, KSTP, KPER,
                    PERTIM, TOTIM, DESC, NCOL, NROW, NLAY, A3DArray, HufFormat,
                    False);
                else Assert(False);
              end;
              for LayerIndex := 0 to Abs(NLAY) - 1 do
              begin
                ILAY := LayerIndex+1;
                RecordItem(string(DESC));
              end;
              if (AModel.ModflowGrid.RowCount <> NROW)
                or (AModel.ModflowGrid.ColumnCount <> NCOL)
                or (AModel.HydrogeologicUnits.Count <> Abs(NLAY)) then
              begin
                Beep;
                MessageDlg(StrTheNumberOfHydrogeologic,
                  mtError, [mbOK], 0);
                result := False;
                break;
              end;
            end;
          end;
        mfSubBinary:
          begin
            while FFileStream.Position < FFileStream.Size do
            begin
              case Precision of
                mpSingle:
                  ReadSinglePrecisionModflowBinaryRealArray(FFileStream, KSTP,
                    KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray,
                    False);
                mpDouble:
                  ReadDoublePrecisionModflowBinaryRealArray(FFileStream, KSTP,
                    KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray,
                    False);
                else Assert(False);
              end;
              Description := SubsidenceDescription(string(DESC), ILAY);
              RecordItem(string(Description));
              if (AModel.ModflowGrid.RowCount <> NROW)
                or (AModel.ModflowGrid.ColumnCount <> NCOL) then
              begin
                Beep;
                MessageDlg(StrTheNumberOfRowsOrColumns,
                  mtError, [mbOK], 0);
                result := False;
                break;
              end;
            end;
          end
        else Assert(False);
      end;
    except on E: EFOpenError do
      begin
        result := False;
        Beep;
        MessageDlg(E.message, mtError, [mbOK], 0);
        Exit;
      end;
    end;
  finally
    CloseFiles;
  end;
  if clData.Items.Count = 0 then
  begin
    Beep;
    MessageDlg(StrFileIsEmpty, mtError, [mbOK], 0);
    result := False;
  end;
  if clData.Items.Count >= 1 then
  begin
    clData.Checked[clData.Items.Count-1] := True;
    clDataClickCheck(clData);
  end;
end;

procedure TfrmSelectResultToImport.SetDefaultDisplayOption;
var
  Choice: TDisplayChoice;
  DefaultChoice: TDisplayChoice;
begin
  DefaultChoice := dcColor;
  for Choice := Low(TDisplayChoice) to High(TDisplayChoice) do
  begin
    if DisplayChoices[Choice] > DisplayChoices[DefaultChoice] then
    begin
      DefaultChoice := Choice;
    end;
  end;
  rgDisplayChoice.ItemIndex := Ord(DefaultChoice);
end;

procedure TfrmSelectResultToImport.AssignWaterTable(NewDataSets: TList;
  OldComments: TStringList; DataSetNames: TStringList;
  ScreenObjectsToDelete: TScreenObjectList; NewCreateScreenObjects: TList;
  NTRANS, KPER: Integer; WaterTableArray: TModflowDoubleArray; KSTP: Integer;
  const Description: string; ValuesToIgnore: TOneDRealArray;
  TOTIM: TModflowDouble; FileNames: string; AModel: TCustomModel);
var
  WaterTableData: TDataArray;
  OldComment: string;
  ScreenObject: TScreenObject;
  MinMaxAssigned: Boolean;
  UndoCreateObject: TUndoCreateScreenObject;
begin
  if Description = StrHead then
  begin
    CreateOrRetrieveLayerDataSet(StrWaterTable, NTRANS, KSTP, KPER, -1, TOTIM,
      WaterTableData, OldComment, NewDataSets, ScreenObjectsToDelete, FileNames,
      AModel, dafWaterTable);
    CreateScreenObject(-1, AModel, ScreenObject);
    AssignValues(-1, ScreenObject, WaterTableData, WaterTableArray,
      ValuesToIgnore, AModel, MinMaxAssigned);
    UpdateOldComments(OldComments, WaterTableData, OldComment);
    DataSetNames.AddObject(WaterTableData.Name, WaterTableData);
    UndoCreateObject := TUndoCreateScreenObject.Create(ScreenObject);
    UndoCreateObject.UpdateObservations;
    NewCreateScreenObjects.Add(UndoCreateObject);
//    NewCreateScreenObjects.Add(TUndoCreateScreenObject.Create(ScreenObject));
  end;
end;

procedure TfrmSelectResultToImport.AssignWaterTableArray(
  var WaterTableArray: TModflowDoubleArray;
  ILAY: Integer;
  AnArray: TModflowDoubleArray;
  ValuesToIgnore: TOneDRealArray; const Description: string);
var
  AValue: single;
  ValueIndex: Integer;
  ValueOK: Boolean;
  RowIndex: Integer;
  ColIndex: Integer;
  WaterTableValue: single;
begin
  if (Description = StrHead) then
  begin
    if ILAY = 1 then
    begin
      WaterTableArray := AnArray;
      SetLength(WaterTableArray, Length(WaterTableArray),
        Length(WaterTableArray[0]));
    end
    else
    begin
      for RowIndex := 0 to Length(WaterTableArray) - 1 do
      begin
        for ColIndex := 0 to Length(WaterTableArray[0]) - 1 do
        begin
          ValueOK := True;
          for ValueIndex := 0 to Length(ValuesToIgnore) - 1 do
          begin
            AValue := ValuesToIgnore[ValueIndex];
            WaterTableValue := WaterTableArray[RowIndex, ColIndex];
            if WaterTableValue = AValue then
            begin
              ValueOK := False;
              break;
            end;
          end;
          if not ValueOK then
          begin
            WaterTableArray[RowIndex, ColIndex] := AnArray[RowIndex, ColIndex];
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmSelectResultToImport.AdjustTotalTime(var TOTIM: TModflowDouble);
var
  FirstStressPeriod: TModflowStressPeriod;
  ShouldAdjust: Boolean;
  SP_Index: Integer;
  StressPeriod: TModflowStressPeriod;
begin
  if frmGoPhast.PhastModel.ModflowStressPeriods.Count > 1 then
  begin
    FirstStressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods[0];
    if FirstStressPeriod.StressPeriodType = sptSteadyState then
    begin
      ShouldAdjust := True;
      for SP_Index := 1 to frmGoPhast.PhastModel.ModflowStressPeriods.Count - 1 do
      begin
        StressPeriod := frmGoPhast.PhastModel.ModflowStressPeriods[SP_Index];
        if StressPeriod.StressPeriodType = sptSteadyState then
        begin
          ShouldAdjust := False;
          break;
        end;
      end;
      if ShouldAdjust then
      begin
        TOTIM := TOTIM - FirstStressPeriod.PeriodLength;
      end;
    end;
  end;
end;

procedure TfrmSelectResultToImport.GetShouldIgnore(ValuesToIgnore: TOneDRealArray; Temp: TModflowFloat; var ShouldIgnore: Boolean);
var
  Delta: Double;
  IgnoreIndex: Integer;
  IgnoreValue: Double;
const
  Epsilon = 1E-07;
begin
  ShouldIgnore := False;
  for IgnoreIndex := 0 to Length(ValuesToIgnore) - 1 do
  begin
    IgnoreValue := ValuesToIgnore[IgnoreIndex];
    Delta := Abs(IgnoreValue) * Epsilon;
    if (IgnoreValue - Delta <= Temp) and (IgnoreValue + Delta >= Temp) then
    begin
      ShouldIgnore := True;
      Break;
    end;
  end;
end;

procedure TfrmSelectResultToImport.UpdateCombo;
var
  Index: Integer;
  CurrentItem: string;
  ColorItems: TStringList;
  CheckCount: Integer;
begin
  CheckCount := 0;
  ColorItems := TStringList.Create;
  try
    ColorItems.Add('none');
    CurrentItem := comboColorGrid.Text;
    btnOK.Enabled := False;
    for Index := 0 to clData.Items.Count - 1 do
    begin
      if clData.Checked[Index] then
      begin
        Inc(CheckCount);
        btnOK.Enabled := True;
        ColorItems.Add(clData.Items[Index]);
      end;
    end;
    comboColorGrid.Items := ColorItems;
    comboColorGrid.ItemIndex := ColorItems.IndexOf(CurrentItem);
    if comboColorGrid.ItemIndex < 0 then
    begin
      comboColorGrid.ItemIndex := 0;
    end;
    if (CheckCount = 1) then
    begin
      comboColorGrid.ItemIndex := 1;
    end;
  finally
    ColorItems.Free;
  end;
end;

procedure TfrmSelectResultToImport.AssignObjectName(var ScreenObject: TScreenObject; LayerData: TDataArray);
var
  Root: string;
  ExistingObjectCount: Integer;
begin
  Root := LayerData.Name + StrObject;
  ExistingObjectCount := frmGoPhast.PhastModel.
    NumberOfLargestScreenObjectsStartingWith(Root);
  Inc(ExistingObjectCount);
  ScreenObject.Name := Root + IntToStr(ExistingObjectCount);
end;

function TfrmSelectResultToImport.AskUserIfNewDataSet: boolean;
begin
  if not FAskedUser then
  begin
    FAskedUser := True;
    frmUpdateDataSets := TfrmUpdateDataSets.Create(nil);
    try
      frmUpdateDataSets.ShowModal;
      FCreateNewDataSet := frmUpdateDataSets.ModalResult <> mrOK;
    finally
      frmUpdateDataSets.Free;
    end;
  end;
  result := FCreateNewDataSet;
end;

procedure TfrmSelectResultToImport.Read3DArray(var NLAY: Integer;
   var EndReached: Boolean; var KPER, KSTP: Integer; var TOTIM: TModflowDouble;
   var Description: string; var A3DArray: T3DTModflowArray;
   Precision: TModflowPrecision; HufFormat: boolean; ShouldReadArray: boolean);
var
  PERTIM: TModflowDouble;
  DESC: TModflowDesc;
  NCOL: Integer;
  NROW: Integer;
begin
  if FFileStream.Position < FFileStream.Size then
  begin
    case Precision of
      mpSingle:
        ReadModflowSinglePrecFluxArray(FFileStream, KSTP, KPER, PERTIM, TOTIM, DESC,
          NCOL, NROW, NLAY, A3DArray, HufFormat, ShouldReadArray);
      mpDouble:
        ReadModflowDoublePrecFluxArray(FFileStream, KSTP, KPER, PERTIM, TOTIM, DESC,
          NCOL, NROW, NLAY, A3DArray, HufFormat, ShouldReadArray);
      else Assert(False);
    end;
    Description := string(Trim(DESC));
  end
  else
  begin
    EndReached := True;
  end;
end;

procedure TfrmSelectResultToImport.CloseFiles;
begin
  FFileStream.Free;
  if FFileVariable <> nil then
  begin
    CloseFile(FFileVariable.AFile);
  end;
  FFileVariable.Free;
end;

procedure TfrmSelectResultToImport.ReadArray(var AnArray: TModflowDoubleArray;
  var EndReached: Boolean; var NTRANS, KPER, KSTP, ILAY: Integer;
  var TOTIM: TModflowDouble;
  var Description: string; Precision: TModflowPrecision; ShouldReadArray: boolean);
var
  NROW: Integer;
  DESC2: TModflowDesc2;
  DESC: TModflowDesc;
  PERTIM: TModflowDouble;
  NCOL: Integer;

begin
  NTRANS := 0;
  case FResultFormat of
    mfMt3dConc:
      begin
        if FFileStream.Position < FFileStream.Size then
        begin
          case Precision of
            mpSingle:
              ReadSinglePrecisionMt3dmsBinaryRealArray(FFileStream, NTRANS, KSTP,
                KPER, TOTIM, DESC, NCOL, NROW, ILAY, AnArray,
                ShouldReadArray);
            mpDouble:
              ReadDoublePrecisionMt3dmsBinaryRealArray(FFileStream, NTRANS, KSTP,
                KPER, TOTIM, DESC, NCOL, NROW, ILAY, AnArray,
                ShouldReadArray);
            else Assert(False);
          end;
          Description := string(Trim(DESC));
          if NTRANS > FMaxTrans then
          begin
            FMaxTrans := NTRANS;
          end;
        end
        else
        begin
          EndReached := True;
        end;
      end;
    mrBinary, mrHufBinary, mfSubBinary:
      begin
        if FFileStream.Position < FFileStream.Size then
        begin
          case Precision of
            mpSingle:
              ReadSinglePrecisionModflowBinaryRealArray(FFileStream, KSTP,
                KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray,
                ShouldReadArray);
            mpDouble:
              ReadDoublePrecisionModflowBinaryRealArray(FFileStream, KSTP,
                KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray,
                ShouldReadArray);
            else Assert(False);
          end;
          Description := string(Trim(DESC));
        end
        else
        begin
          EndReached := True;
        end;
      end;
    mrAscii, mrHufAscii:
      begin
        if not EOF(FFileVariable.AFile) then
        begin
          ReadModflowAsciiRealArray(FFileVariable, KSTP, KPER, PERTIM, TOTIM,
            DESC2, NCOL, NROW, ILAY, AnArray, ShouldReadArray);
          Description := string(Trim(DESC2));
        end
        else
        begin
          EndReached := True;
        end;
      end;
    else Assert(False);
  end;
  Assert(Length(Description) > 0);
  Description := TitleCase(Description);
end;

function TfrmSelectResultToImport.OpenResultFile(AFileName: string;
  out Precision: TModflowPrecision; out HufFormat: boolean): boolean;
var
  Extension: string;
begin
  result := True;
  Precision := mpSingle;
  Extension := ExtractFileExt(AFileName);
  if (SameText(Extension, StrBdn))
    or (SameText(Extension, StrBhd)) then
  begin
    FResultFormat := mrBinary;
  end
  else if (SameText(Extension, StrFdn))
    or (SameText(Extension, StrFhd)) then
  begin
    FResultFormat := mrAscii;
  end
  else if (SameText(Extension, StrCbcExt)) then
  begin
    FResultFormat := mrFlux;
  end
  else if (SameText(Extension, StrHuffhd)) then
  begin
    FResultFormat := mrHufAscii;
  end
  else if (SameText(Extension, StrHufbhd)) then
  begin
    FResultFormat := mrHufBinary;
  end
  else if (SameText(Extension, StrHufflow)) then
  begin
    FResultFormat := mrHufFlux;
  end
  else if (SameText(Extension, StrSubOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSubSubOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSubComMlOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSubComIsOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSubVdOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSubNdCritHeadOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSubDCritHeadOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtSubOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtComMLOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtComIsOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtVDOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtPreConStrOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtDeltaPreConStrOu)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtGeoStatOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtDeltaGeoStatOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtEffStressOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtDeltaEffStressOu)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtVoidRatioOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtThickCompSedOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrSwtLayerCentElevOut)) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, StrMt3dConcFile)) then
  begin
    FResultFormat := mfMt3dConc;
  end
  else
  begin
    result := False;
    Beep;
    MessageDlg(StrTheFileTypeMustB, mtError, [mbOK], 0);
    Exit;
  end;

  HufFormat:= false;
  case FResultFormat of
    mrBinary, mrHufBinary, mfSubBinary:
      begin
        FFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
        Precision := CheckArrayPrecision(FFileStream);
      end;
    mfMt3dConc:
      begin
        FFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
        Precision := CheckMt3dmsArrayPrecision(FFileStream);
      end;
    mrFlux, mrHufFlux:
      begin
        FFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
        Precision := CheckBudgetPrecision(FFileStream, HufFormat);
      end;
    mrAscii, mrHufAscii:
      begin
        FFileVariable := TFileVariable.Create;
        AssignFile(FFileVariable.AFile, AFileName);
        Reset(FFileVariable.AFile);
        Precision := mpDouble;
      end;
    else Assert(False);
  end;
end;

procedure TfrmSelectResultToImport.rdgModelsBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
begin
  inherited;
  if (ARow >= rdgModels.FixedRows) and (ACol = Ord(mcFileName)) then
  begin
    if (rdgModels.Cells[ACol, ARow] <> '')
      and rdgModels.Checked[Ord(mcUse), ARow]
      and not FileExists(rdgModels.Cells[ACol, ARow]) then
    begin
      rdgModels.Canvas.Brush.Color := clRed;
    end;
  end;
end;

procedure TfrmSelectResultToImport.rdgModelsButtonClick(Sender: TObject; ACol,
  ARow: Integer);
var
  AModel: TCustomModel;
  AFileName: string;
  Extension: string;
begin
  inherited;
  Assert(ACol = Ord(mcFileName));
  AModel := rdgModels.Objects[Ord(mcModelName), ARow] as TCustomModel;
  Assert(AModel <> nil);
  if FileExists(rdgModels.Cells[ACol, ARow]) then
  begin
    odSelectFiles.FileName := rdgModels.Cells[ACol, ARow];
  end
  else
  begin
    AFileName := DefaultFileName(AModel);
    if (ARow > 2) and FileExists(rdgModels.Cells[ACol, 2]) then
    begin
      Extension := ExtractFileExt(rdgModels.Cells[ACol, 2]);
      AFileName := ChangeFileExt(AFileName, Extension);
    end;
    odSelectFiles.FileName := AFileName;
  end;
  if odSelectFiles.Execute then
  begin
    ReadDataHeadings(AModel, ARow, odSelectFiles.FileName);
    AddModelRow(AModel, ARow,  odSelectFiles.FileName);
  end;
end;

{ TUndoImportModelResults }

constructor TUndoImportModelResults.Create(NewDataSets: TList;
  DataSetNames, OldComments: TStringList;
  DisplayDataSet: TDataArray; DisplayChoice: TDisplayChoice;
  AModel: TCustomModel);
var
  DSIndex: Integer;
  DataArray: TDataArray;
  Position: Integer;
begin
  inherited Create;

  FModel := AModel;
  FNewEdgeDisplay := FModel.EdgeDisplay;
  FOldEdgeDisplay := FModel.EdgeDisplay;
  FNew3DDataSet := FModel.ThreeDDataSet;
  FOld3DDataSet := FModel.ThreeDDataSet;
  FNewThreeDTimeList := FModel.ThreeDTimeList;
  FOldThreeDTimeList := FModel.ThreeDTimeList;
  FNewTopDataSet := FModel.TopDataSet;
  FOldTopDataSet := FModel.TopDataSet;
  FNewTopTimeList := FModel.TopTimeList;
  FOldTopTimeList := FModel.TopTimeList;
  FNewFrontDataSet := FModel.FrontDataSet;
  FOldFrontDataSet := FModel.FrontDataSet;
  FNewFrontTimeList := FModel.FrontTimeList;
  FOldFrontTimeList := FModel.FrontTimeList;
  FNewSideDataSet := FModel.SideDataSet;
  FOldSideDataSet := FModel.SideDataSet;
  FNewSideTimeList := FModel.SideTimeList;
  FOldSideTimeList := FModel.SideTimeList;
  FNewTopContourDataSet := FModel.Grid.TopContourDataSet;
  FOldTopContourDataSet := FModel.Grid.TopContourDataSet;
  FNewFrontContourDataSet := FModel.Grid.FrontContourDataSet;
  FOldFrontContourDataSet := FModel.Grid.FrontContourDataSet;
  FNewSideContourDataSet := FModel.Grid.SideContourDataSet;
  FOldSideContourDataSet := FModel.Grid.SideContourDataSet;
  FNew3DContourDataSet := FModel.Grid.ThreeDContourDataSet;
  FOld3DContourDataSet := FModel.Grid.ThreeDContourDataSet;

  if DisplayDataSet <> nil then
  begin
    case DisplayChoice of
      dcColor:
        begin
          FNewEdgeDisplay := nil;

          FNew3DDataSet := DisplayDataSet;
          FNewThreeDTimeList := nil;

          FNewTopDataSet := DisplayDataSet;
          FNewTopTimeList := nil;

          if DisplayDataSet.Orientation = dso3D then
          begin
            FNewFrontDataSet := DisplayDataSet;
            FNewFrontTimeList := nil;

            FNewSideDataSet := DisplayDataSet;
            FNewSideTimeList := nil;
          end
          else
          begin
            FNewFrontDataSet := nil;
            FNewFrontTimeList := nil;

            FNewSideDataSet := nil;
            FNewSideTimeList := nil;
          end;
        end;
      dcContour:
        begin
          FNew3DContourDataSet := DisplayDataSet;
          FNewTopContourDataSet := DisplayDataSet;

          if DisplayDataSet.Orientation = dso3D then
          begin
            FNewFrontContourDataSet := DisplayDataSet;
            FNewSideContourDataSet := DisplayDataSet;
          end
          else
          begin
            FNewFrontContourDataSet := nil;
            FNewSideContourDataSet := nil;
          end;
        end;
      dcNone:
        begin
          // do nothing
        end;
      else Assert(False);
    end;
//    if frmDisplayData = nil then
//    begin
//      Application.CreateForm(TfrmDisplayData, frmDisplayData);
//    end;
//    UpdateFrmDisplayData(True);
  end;

  FContainedUndos := TObjectList.Create;
  FNewDataSets := TList.Create;
  FNewDataSets.Assign(NewDataSets);

  FOldComments:= TStringList.Create;
  FNewComments:= TStringList.Create;

  for DSIndex := 0 to NewDataSets.Count - 1 do
  begin
    DataArray := NewDataSets[DSIndex];
    Position := DataSetNames.IndexOf(DataArray.Name);
    if Position >= 0 then
    begin
      DataSetNames.Delete(Position);
      Position := OldComments.IndexOfObject(DataArray);
      if Position >= 0 then
      begin
        OldComments.Delete(Position);
      end;
    end;
  end;
  FOldComments.Assign(OldComments);
  FNewComments.Capacity := DataSetNames.Count;
  for DSIndex := 0 to DataSetNames.Count - 1 do
  begin
    DataArray := DataSetNames.Objects[DSIndex] as TDataArray;
    FNewComments.AddObject(DataArray.Comment, DataArray);
  end;

end;

function TUndoImportModelResults.Description: string;
begin
  result := StrImportModelResults;
end;

destructor TUndoImportModelResults.Destroy;
begin
  FNewDataSets.Free;
  FContainedUndos.Free;
  FOldComments.Free;
  FNewComments.Free;
  inherited;
end;

procedure TUndoImportModelResults.DoCommand;
var
  Index: Integer;
  AnUndo: TCustomUndo;
  DataSet: TDataArray;
  DataArrayManager: TDataArrayManager;
begin
  inherited;
  frmGoPhast.PhastModel.BeginDataSetUpdate;
  DisallowChildGridUpdates;
  try
    DataArrayManager := FModel.DataArrayManager;
    DataArrayManager.HandleAddedDataArrays(FNewDataSets);

    for Index := 0 to FContainedUndos.Count - 1 do
    begin
      AnUndo := FContainedUndos[Index];
      AnUndo.DoCommand;
    end;
    for Index := 0 to DataArrayManager.DataSetCount - 1 do
    begin
      DataSet := DataArrayManager.DataSets[Index];
      if FNewDataSets.IndexOf(DataSet) < 0 then
      begin
        FModel.CreateVariables(DataSet);
      end;
    end;

    SetComments(FNewComments);

    FModel.EdgeDisplay := FNewEdgeDisplay;
    FModel.ThreeDDataSet := FNew3DDataSet;
    FModel.ThreeDTimeList := FNewThreeDTimeList;
    FModel.TopDataSet := FNewTopDataSet;
    FModel.TopTimeList := FNewTopTimeList;
    FModel.FrontDataSet := FNewFrontDataSet;
    FModel.FrontTimeList := FNewFrontTimeList;
    FModel.SideDataSet := FNewSideDataSet;
    FModel.SideTimeList := FNewSideTimeList;
    FModel.Grid.TopContourDataSet := FNewTopContourDataSet;
    FModel.Grid.FrontContourDataSet := FNewFrontContourDataSet;
    FModel.Grid.SideContourDataSet := FNewSideContourDataSet;
    FModel.Grid.ThreeDContourDataSet := FNew3DContourDataSet;
    FModel.DiscretizationChanged;
  finally
    AllowChildGridUpdates;
    frmGoPhast.PhastModel.EndDataSetUpdate;
  end;

  UpdateFrmDisplayData;
//  UpdateFrmContourData;
  UpdateFrmGridValue;

end;

procedure TUndoImportModelResults.Undo;
var
  Index: Integer;
  AnUndo: TCustomUndo;
  DataArrayManager: TDataArrayManager;
begin
  inherited;
  DisallowChildGridUpdates;
  try
    for Index := FContainedUndos.Count - 1 downto 0 do
    begin
      AnUndo := FContainedUndos[Index];
      AnUndo.Undo;
    end;
    SetComments(FOldComments);

    FModel.EdgeDisplay := FOldEdgeDisplay;
    FModel.ThreeDDataSet := FOld3DDataSet;
    FModel.ThreeDTimeList := FOldThreeDTimeList;
    FModel.TopDataSet := FOldTopDataSet;
    FModel.TopTimeList := FOldTopTimeList;
    FModel.FrontDataSet := FOldFrontDataSet;
    FModel.FrontTimeList := FOldFrontTimeList;
    FModel.SideDataSet := FOldSideDataSet;
    FModel.SideTimeList := FOldSideTimeList;
    FModel.Grid.TopContourDataSet := FOldTopContourDataSet;
    FModel.Grid.FrontContourDataSet := FOldFrontContourDataSet;
    FModel.Grid.SideContourDataSet := FOldSideContourDataSet;
    FModel.Grid.ThreeDContourDataSet := FOld3DContourDataSet;
    FModel.DiscretizationChanged;


    DataArrayManager := FModel.DataArrayManager;
    DataArrayManager.HandleDeletedDataArrays(FNewDataSets);
    finally
    AllowChildGridUpdates;

  end;
  UpdateFrmDisplayData;
//  UpdateFrmContourData;
  UpdateFrmGridValue;
end;

procedure TUndoImportModelResults.SetComments(Comments: TStringList);
var
  Index: Integer;
  DataArray: TDataArray;
begin
  for Index := 0 to Comments.Count - 1 do
  begin
    DataArray := Comments.Objects[Index] as TDataArray;
    DataArray.Comment := Comments[Index];
  end;
end;

{ TFormulaAssigner }

procedure TFormulaAssigner.AddFormula(AFormula: string; AModel: TBaseModel);
begin
  FFormulas.Add(AFormula);
  FModels.Add(AModel);
end;

procedure TFormulaAssigner.AssignFinalFormula;
var
  FinalFormula: string;
  ModelPosition: Integer;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  Assert(FFormulas.Count = FModels.Count);
  Assert(FFormulas.Count >= 1);
  if FFormulas.Count = 1 then
  begin
    FDataArray.Formula := FFormulas[0];
  end
  else
  begin
    Assert(frmGoPhast.PhastModel.LgrUsed);
    FinalFormula := 'If('+ StrGridNumber + ' <= '
      + IntToStr(frmGoPhast.PhastModel.ChildModels.Count+1) + ', Case(' + StrGridNumber;
    ModelPosition := FModels.IndexOf(frmGoPhast.PhastModel);
    if ModelPosition >= 0 then
    begin
      FinalFormula := FinalFormula + ', ' + FFormulas[ModelPosition];
    end
    else
    begin
      FinalFormula := FinalFormula + ', 0'
    end;
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      ModelPosition := FModels.IndexOf(ChildModel);
      if ModelPosition >= 0 then
      begin
        FinalFormula := FinalFormula + ', ' + FFormulas[ModelPosition];
      end
      else
      begin
        FinalFormula := FinalFormula + ', 0'
      end;
    end;
    FinalFormula := FinalFormula + '0), 0)';
    FDataArray.Formula := FinalFormula;
  end;
end;

constructor TFormulaAssigner.Create(ADataArray: TDataArray);
begin
  FDataArray := ADataArray;
  FFormulas := TStringList.Create;
  FModels:= TList.Create;
end;

destructor TFormulaAssigner.Destroy;
begin
  FFormulas.Free;
  FModels.Free;
  inherited;
end;

{ TFormulaAssignerList }

procedure TFormulaAssignerList.AssignFinalFormulas;
var
  Index: Integer;
  AFormulaAssigner: TFormulaAssigner;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    AFormulaAssigner := FList[Index];
    AFormulaAssigner.AssignFinalFormula;
  end;
end;

constructor TFormulaAssignerList.Create;
begin
  FList := TObjectList.Create;
end;

destructor TFormulaAssignerList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFormulaAssignerList.GetFormulaAssigner(
  ADataArray: TDataArray): TFormulaAssigner;
var
  Index: Integer;
  AFormulaAssigner: TFormulaAssigner;
begin
  Assert(ADataArray <> nil);
  Assert(ADataArray.Model = frmGoPhast.PhastModel);
  for Index := 0 to FList.Count - 1 do
  begin
    AFormulaAssigner := FList[Index];
    if AFormulaAssigner.DataArray = ADataArray then
    begin
      result := AFormulaAssigner;
      Exit;
    end;
  end;
  result := TFormulaAssigner.Create(ADataArray);
  FList.Add(result);
end;

end.

