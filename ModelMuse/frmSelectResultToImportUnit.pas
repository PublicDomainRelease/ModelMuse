unit frmSelectResultToImportUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, CheckLst, JvExCheckLst,
  JvCheckListBox, Buttons, JvDialogs, IntListUnit, ReadModflowArrayUnit,
  DataSetUnit, ScreenObjectUnit, StrUtils, UndoItems, Contnrs, RealListUnit,
  ModflowGridUnit, ExtCtrls, EdgeDisplayUnit, GoPhastTypes;

type
  TModflowResultFormat = (mrBinary, mrAscii, mrFlux, mrHufAscii, mrHufBinary,
    mrHufFlux, mfSubBinary);

  TDataArrayForm = (dafLayer, dafSystem, dafNone);

  TfrmSelectResultToImport = class(TfrmCustomGoPhast)
    clData: TJvCheckListBox;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    odSelectFiles: TJvOpenDialog;
    comboColorGrid: TComboBox;
    Label1: TLabel;
    btnSelectAll: TButton;
    btnSelectNone: TButton;
    rgDisplayChoice: TRadioGroup;
    procedure clDataClickCheck(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure odSelectFilesTypeChange(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnSelectNoneClick(Sender: TObject);
  private
    FFileName: string;
    FPeriods: TIntegerList;
    FSteps: TIntegerList;
    FDescriptions: TStringList;
    FFileStream: TFileStream;
    FFileVariable: TFileVariable;
    FResultFormat: TModflowResultFormat;
    FAskedUser: Boolean;
    FCreateNewDataSet: Boolean;
    FMaxPeriod: Integer;
    FMaxStep: Integer;
    FMaxLayer: Integer;
    FGrid: TModflowGrid;
    function DefaultFileName: string;
    procedure OpenResultFile(out Precision: TModflowPrecision; out HufFormat: boolean);
    procedure ReadArray(var AnArray: TModflowDoubleArray;
      var EndReached: Boolean; var KPER, KSTP, ILAY: Integer;
      var Description: string; Precision: TModflowPrecision);
    procedure CreateOrRetrieveLayerDataSet(const Description: string;
      KSTP, KPER, ILAY: integer;
      out LayerData: TDataArray; out OldComment: string; NewDataSets: TList;
      ScreenObjectsToDelete: TScreenObjectList; DataArrayForm: TDataArrayForm = dafLayer);
    procedure CreateScreenObject(LayerIndex: integer;
      out ScreenObject: TScreenObject);
    procedure AssignValues(LayerIndex: integer; ScreenObject: TScreenObject; LayerData: TDataArray;
      AnArray: TModflowDoubleArray; ValuesToIgnore: TOneDRealArray;
      out MinMaxAssigned: boolean);
    procedure CreateOrRetrieve3DDataSet(Description: string;
      KPER, KSTP: integer;
      LayerNumbers: TIntegerList; LayerDataSets: TList;
      out New3DArray: TDataArray; out OldComment: string; FluxData: boolean; NewDataSets: TList);
    procedure CloseFiles;
    procedure Read3DArray(var NLAY: Integer; var EndReached: Boolean;
      var KPER, KSTP: Integer; var Description: string;
      var A3DArray: T3DTModflowArray; Precision: TModflowPrecision; HufFormat: boolean);
    procedure Assign3DValues(ScreenObject: TScreenObject; LayerData: TDataArray;
      AnArray: T3DTModflowArray; LayerIndex: integer; CheckAllLayers: boolean;
      ValuesToIgnore: TOneDRealArray);
    procedure SetData;
    function AskUserIfNewDataSet: boolean;
    procedure AssignLimits(MinValues, MaxValues: TRealList;
      New3DArray: TDataArray; ValuesToIgnore: TOneDRealArray);
    procedure AssignObjectName(var ScreenObject: TScreenObject; LayerData: TDataArray);
    procedure UpdateCombo;
    procedure GetShouldIgnore(ValuesToIgnore: TOneDRealArray; Temp: TModflowFloat; var ShouldIgnore: Boolean);
    function SubsidenceDescription(DESC: string; ILAY: integer): string;
    { Private declarations }

  public
    function SelectFiles: boolean;
    { Public declarations }
  end;

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
    procedure SetComments(Comments: TStringList);
  protected
    function Description: string; override;
  public
    Constructor Create(NewDataSets: TList;
      DataSetNames, OldComments: TStringList;
      DisplayDataSet: TDataArray; DisplayChoice: TDisplayChoice);
    Destructor Destroy; override;
    // @name does the command for the first time.
    procedure DoCommand; override;
    // @name undoes the command.
    procedure Undo; override;
  end;

var
  frmSelectResultToImport: TfrmSelectResultToImport;

implementation

uses Math, frmGoPhastUnit, RbwParser, 
  GIS_Functions, ValueArrayStorageUnit, ModelMuseUtilities, PhastModelUnit,
  frmUpdateDataSetsUnit, UndoItemsScreenObjects, frmGridColorUnit,
  InterpolationUnit, frmContourDataUnit, HufDefinition;

const
  StrModelResults = 'Model Results';
  StrLayerData = StrModelResults + '|Layer Data';
  StrThreeDData = StrModelResults + '|3D Data';

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

{ TfrmSelectResultToImport }

procedure TfrmSelectResultToImport.CreateOrRetrieveLayerDataSet(
  const Description: string; KSTP, KPER, ILAY: integer;
  out LayerData: TDataArray; out OldComment: string; NewDataSets: TList;
  ScreenObjectsToDelete: TScreenObjectList;
  DataArrayForm: TDataArrayForm = dafLayer);
var
  NewName: string;
  Grid: TModflowGrid;
  CreateNewDataSet: boolean;
  Index: Integer;
  ScreenObject: TScreenObject;
begin
  NewName := TitleCase(Description);
  NewName := ValidName(NewName);
  NewName := NewName + '_P' + PaddedIntToStr(KPER, FMaxPeriod) +
    '_S' + PaddedIntToStr(KSTP, FMaxStep);
  case DataArrayForm of
    dafLayer:
      begin
        NewName := NewName + '_L' + PaddedIntToStr(ILAY, FMaxLayer);
      end;
    dafSystem:
      begin
        NewName := NewName + '_Sys' + PaddedIntToStr(ILAY, FMaxLayer);
      end;
    dafNone:
      begin
        // do nothing
      end;
    else
      Assert(False);
  end;
  CreateNewDataSet := True;
  if frmGoPhast.PhastModel.GetDataSetByName(NewName) <> nil then
  begin
    CreateNewDataSet := AskUserIfNewDataSet;
    if CreateNewDataSet then
    begin
      NewName := GenerateNewName(NewName, '_');
    end;
  end;
  if CreateNewDataSet then
  begin
    LayerData := TDataArray.Create(frmGoPhast.PhastModel);
    NewDataSets.Add(LayerData);
    LayerData.UpDateWithName(NewName);
    LayerData.OnNameChange := frmGoPhast.PhastModel.DataArrayNameChange;
    frmGoPhast.PhastModel.AddDataSet(LayerData);
    LayerData.DataType := rdtDouble;
    LayerData.Orientation := dsoTop;
    LayerData.Formula :=
      FloatToStr(frmGoPhast.PhastModel.ModflowOptions.HNoFlow);
    LayerData.TwoInterpolatorClass := TLinearSfrpackInterpolator.ClassName;

    Grid := frmGoPhast.PhastModel.ModflowGrid;
    LayerData.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
      Grid.ColumnCount);
    LayerData.EvaluatedAt := eaBlocks;
    LayerData.Classification := StrLayerData;
    LayerData.OnDataSetUsed := frmGoPhast.PhastModel.ModelResultsRequired;
    frmGoPhast.PhastModel.CreateVariables(LayerData);
    OldComment := '';
  end
  else
  begin
    LayerData := frmGoPhast.PhastModel.GetDataSetByName(NewName);
    OldComment := LayerData.Comment;
    for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      ScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
      if ScreenObject.IndexOfDataSet(LayerData) >= 0 then
      begin
        ScreenObject.Deleted := True;
        ScreenObjectsToDelete.Add(ScreenObject);
      end;
    end;
  end;
  LayerData.Comment := 'read from file: "' + FFileName
    + '" on ' + DateTimeToStr(Now)
    + #13#10 + 'Stress Period: ' + IntToStr(KPER)
    + #13#10 + 'Time Step: ' + IntToStr(KSTP)
    + #13#10 + 'Layer: ' + IntToStr(ILAY);
end;

procedure TfrmSelectResultToImport.CreateScreenObject(LayerIndex: integer;
  out ScreenObject: TScreenObject);
var
  RowIndex: Integer;
  ColIndex: Integer;
  UndoCreateScreenObject: TCustomUndo;
  Grid: TModflowGrid;
  ActiveDataSet: TDataArray;
  LI: Integer;
begin
  ActiveDataSet := frmGoPhast.PhastModel.GetDataSetByName(rsActive);
  ActiveDataSet.Initialize;

  Grid := frmGoPhast.PhastModel.ModflowGrid;
  ScreenObject := TScreenObject.CreateWithViewDirection(
    frmGoPhast.PhastModel, vdTop,
    UndoCreateScreenObject, False);
  frmGoPhast.PhastModel.AddScreenObject(ScreenObject);
  ScreenObject.ElevationCount := ecZero;
  ScreenObject.SetValuesOfIntersectedCells := True;

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

function TfrmSelectResultToImport.DefaultFileName: string;
begin
  Assert(frmGoPhast.PhastModel.ModelSelection = msModflow);
  result := frmGoPhast.PhastModel.DefaultModflowOutputFileName;
  if not FileExists(result) then
  begin
    result := '';
  end;
end;

procedure TfrmSelectResultToImport.AssignValues(LayerIndex: integer;
  ScreenObject: TScreenObject; LayerData: TDataArray;
  AnArray: TModflowDoubleArray; ValuesToIgnore: TOneDRealArray;
  out MinMaxAssigned: boolean);
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
begin
  ActiveDataSet := frmGoPhast.PhastModel.GetDataSetByName(rsActive);
  ActiveDataSet.Initialize;

  Grid := frmGoPhast.PhastModel.ModflowGrid;
  DataSetIndex := ScreenObject.AddDataSet(LayerData);
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
  LayerData.Limits.LowerLimit.UseLimit := True;
  LayerData.Limits.LowerLimit.RealLimitValue := MinValue;
  LayerData.Limits.UpperLimit.UseLimit := True;
  LayerData.Limits.UpperLimit.RealLimitValue := MaxValue;
  LayerData.Limits.Update;
  LayerData.Comment := LayerData.Comment
    + #13#10 + 'Minimum value: ' + FloatToStr(MinValue)
    + #13#10 + 'Maximum value: ' + FloatToStr(MaxValue);
  LayerData.Limits.RealValuesToSkip.Clear;
  for IgnoreIndex := 0 to Length(ValuesToIgnore) - 1 do
  begin
    SkipReal := LayerData.Limits.RealValuesToSkip.Add as TSkipReal;
    SkipReal.RealValue := ValuesToIgnore[IgnoreIndex];
  end;
end;

procedure TfrmSelectResultToImport.Assign3DValues(ScreenObject: TScreenObject;
  LayerData: TDataArray; AnArray: T3DTModflowArray; LayerIndex: integer;
  CheckAllLayers: boolean; ValuesToIgnore: TOneDRealArray);
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
begin
  AssignObjectName(ScreenObject, LayerData);

  ActiveDataSet := frmGoPhast.PhastModel.GetDataSetByName(rsActive);
  ActiveDataSet.Initialize;

  Grid := frmGoPhast.PhastModel.ModflowGrid;
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
  LayerData.Limits.LowerLimit.UseLimit := True;
  LayerData.Limits.LowerLimit.RealLimitValue := MinValue;
  LayerData.Limits.UpperLimit.UseLimit := True;
  LayerData.Limits.UpperLimit.RealLimitValue := MaxValue;
  LayerData.Limits.Update;
  LayerData.Comment := LayerData.Comment
    + #13#10 + 'Minimum value: ' + FloatToStr(MinValue)
    + #13#10 + 'Maximum value: ' + FloatToStr(MaxValue)
end;

procedure TfrmSelectResultToImport.CreateOrRetrieve3DDataSet(Description: string;
  KPER, KSTP: integer; LayerNumbers: TIntegerList;
  LayerDataSets: TList; out New3DArray: TDataArray; out OldComment: string; FluxData: boolean;
  NewDataSets: TList);
var
  NewName: string;
  NewFormula: string;
  LayerIndex: Integer;
  LayerPosition: Integer;
  DataArray: TDataArray;
  Grid: TModflowGrid;
  CreateNewDataSet: Boolean;
begin
  NewName := TitleCase(Description);
  NewName := ValidName(NewName);
  NewName := NewName + '_P' + PaddedIntToStr(KPER, FMaxPeriod) +
    '_S' + PaddedIntToStr(KSTP, FMaxStep);
  CreateNewDataSet := True;
  if frmGoPhast.PhastModel.GetDataSetByName(NewName) <> nil then
  begin
    CreateNewDataSet := AskUserIfNewDataSet;
    if CreateNewDataSet then
    begin
      NewName := GenerateNewName(NewName, '_');
    end;
  end;
  Grid := frmGoPhast.PhastModel.ModflowGrid;
  if CreateNewDataSet then
  begin
    New3DArray := TDataArray.Create(frmGoPhast.PhastModel);
    NewDataSets.Add(New3DArray);
    New3DArray.UpDateWithName(NewName);
    New3DArray.OnNameChange := frmGoPhast.PhastModel.DataArrayNameChange;
    frmGoPhast.PhastModel.AddDataSet(New3DArray);
    New3DArray.DataType := rdtDouble;
    New3DArray.Orientation := dso3D;
    New3DArray.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
      Grid.ColumnCount);
    New3DArray.EvaluatedAt := eaBlocks;
    New3DArray.Classification := StrThreeDData;
    New3DArray.OnDataSetUsed := frmGoPhast.PhastModel.ModelResultsRequired;
    frmGoPhast.PhastModel.CreateVariables(New3DArray);
    OldComment := '';
  end
  else
  begin
    New3DArray := frmGoPhast.PhastModel.GetDataSetByName(NewName);
    OldComment := New3DArray.Comment;
  end;
  New3DArray.Comment := 'read from file: "' + FFileName 
    + '" on ' + DateTimeToStr(Now)
    + #13#10 + 'Stress Period: ' + IntToStr(KPER)
    + #13#10 + 'Time Step: ' + IntToStr(KSTP);
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
  end;
  New3DArray.Formula := NewFormula;
  LayerDataSets.Clear;
  LayerNumbers.Clear;
end;

procedure TfrmSelectResultToImport.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
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
begin
  MinValues.Sort;
  MaxValues.Sort;
  if MinValues.Count > 0 then
  begin
    New3DArray.Limits.LowerLimit.UseLimit := True;
    New3DArray.Limits.LowerLimit.RealLimitValue := MinValues[0];
    New3DArray.Limits.UpperLimit.UseLimit := True;
    New3DArray.Limits.UpperLimit.RealLimitValue :=
      MaxValues[MaxValues.Count -1];
  end;
  New3DArray.Limits.Update;
  MinValues.Clear;
  MaxValues.Clear;
  New3DArray.Comment := New3DArray.Comment
    + #13#10 + 'Minimum value: '
    + FloatToStr(New3DArray.Limits.LowerLimit.RealLimitValue)
    + #13#10 + 'Maximum value: '
    + FloatToStr(New3DArray.Limits.UpperLimit.RealLimitValue);
  New3DArray.Limits.RealValuesToSkip.Clear;
  for IgnoreIndex := 0 to Length(ValuesToIgnore) - 1 do
  begin
    SkipReal := New3DArray.Limits.RealValuesToSkip.Add as TSkipReal;
    SkipReal.RealValue := ValuesToIgnore[IgnoreIndex];
  end;
end;

procedure TfrmSelectResultToImport.SetData;
var
  Index: Integer;
  KSTP: Integer;
  KPER: Integer;
  ILAY: Integer;
  AnArray: TModflowDoubleArray;
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
  MinValues,
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
begin
  inherited;
  FGrid := frmGoPhast.PhastModel.ModflowGrid;
  FFileStream := nil;
  FFileVariable := nil;
  MinValues := TRealList.Create;
  MaxValues := TRealList.Create;
  LayerNumbers:= TIntegerList.Create;
  LayerDataSets := TList.Create;
  NewDataSets := TList.Create;
  OldComments := TStringList.Create;
  DataSetNames := TStringList.Create;
  ScreenObjectsToDelete := TScreenObjectList.Create;
  NewCreateScreenObjects := TList.Create;
  FMaxPeriod := Max(frmGoPhast.PhastModel.ModflowStressPeriods.Count,
    frmGoPhast.PhastModel.ModflowFullStressPeriods.Count);
  FMaxStep := frmGoPhast.PhastModel.ModflowStressPeriods.MaxStepsInAnyStressPeriod;
  FMaxLayer := frmGoPhast.PhastModel.LayerStructure.ModflowLayerCount;
  try
    OpenResultFile(Precision, HufFormat);
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
    else
    begin
      SetLength(ValuesToIgnore, 2);
      ValuesToIgnore[0] := frmGoPhast.PhastModel.ModflowOptions.HDry;
      ValuesToIgnore[1] := frmGoPhast.PhastModel.ModflowOptions.HNoFlow;
    end;
    for Index := 0 to clData.Items.Count - 1 do
    begin
      case FResultFormat of
        mrBinary, mrAscii:
          begin
            if Index = 0 then
            begin
              ReadArray(AnArray, EndReached,
                KPER, KSTP, ILAY, Description, Precision);
            end;
            While (KPER = FPeriods[Index])
              and (KSTP = FSteps[Index])
              and (Description = FDescriptions[Index])
              and not EndReached do
            begin
              if ILAY < 0 then
              begin
                // cross section
                for LayerIndex := 1 to frmGoPhast.
                  PhastModel.LayerStructure.ModflowLayerCount do
                begin
                  ILAY := LayerIndex;
                  ILAY := frmGoPhast.PhastModel.LayerStructure.
                    ModflowLayerToDataSetLayer(ILAY)+1;
                  SetLength(LayerArray, 1, Length(AnArray[0]));
                  for ColIndex := 0 to Length(AnArray[0]) - 1 do
                  begin
                    LayerArray[0,ColIndex] := AnArray[LayerIndex-1,ColIndex];
                  end;
                  if clData.Checked[Index] then
                  begin
                    CreateOrRetrieveLayerDataSet(Description, KSTP, KPER, ILAY,
                      LayerData, OldComment, NewDataSets, ScreenObjectsToDelete);
                    CreateScreenObject(ILAY-1, ScreenObject);
                    AssignValues(ILAY-1, ScreenObject, LayerData, LayerArray,
                      ValuesToIgnore, MinMaxAssigned);
                    LayerNumbers.Add(ILAY);
                    LayerDataSets.Add(LayerData);
                    OldComments.AddObject(OldComment, LayerData);
                    DataSetNames.AddObject(LayerData.Name, LayerData);
                    if MinMaxAssigned then
                    begin
                      MinValues.Add(LayerData.Limits.LowerLimit.RealLimitValue);
                      MaxValues.Add(LayerData.Limits.UpperLimit.RealLimitValue);
                    end;

                    if ILAY = FGrid.LayerCount then
                    begin
                      Inc(Count);
                      CreateOrRetrieve3DDataSet(Description, KPER, KSTP,
                        LayerNumbers, LayerDataSets, New3DArray, OldComment,
                        False, NewDataSets);
                      OldComments.AddObject(OldComment, New3DArray);
                      DataSetNames.AddObject(New3DArray.Name, New3DArray);
                      comboColorGrid.Items.Objects[Count] := New3DArray;

                      AssignLimits(MinValues, MaxValues, New3DArray,
                        ValuesToIgnore);
                    end;
                    NewCreateScreenObjects.Add(
                      TUndoCreateScreenObject.Create(ScreenObject))
                  end
                end;
              end
              else
              begin
                // not a cross section
                ILAY := frmGoPhast.PhastModel.LayerStructure.
                  ModflowLayerToDataSetLayer(ILAY)+1;
                if clData.Checked[Index] then
                begin
                  CreateOrRetrieveLayerDataSet(Description, KSTP, KPER, ILAY,
                    LayerData, OldComment, NewDataSets, ScreenObjectsToDelete);
                  CreateScreenObject(ILAY-1, ScreenObject);
                  AssignValues(ILAY-1, ScreenObject, LayerData, AnArray,
                    ValuesToIgnore, MinMaxAssigned);
                  LayerNumbers.Add(ILAY);
                  LayerDataSets.Add(LayerData);
                  OldComments.AddObject(OldComment, LayerData);
                  DataSetNames.AddObject(LayerData.Name, LayerData);
                  if MinMaxAssigned then
                  begin
                    MinValues.Add(LayerData.Limits.LowerLimit.RealLimitValue);
                    MaxValues.Add(LayerData.Limits.UpperLimit.RealLimitValue);
                  end;

                  if ILAY = FGrid.LayerCount then
                  begin
                    Inc(Count);
                    CreateOrRetrieve3DDataSet(Description, KPER, KSTP,
                      LayerNumbers, LayerDataSets, New3DArray, OldComment,
                      False, NewDataSets);
                    OldComments.AddObject(OldComment, New3DArray);
                    DataSetNames.AddObject(New3DArray.Name, New3DArray);
                    comboColorGrid.Items.Objects[Count] := New3DArray;

                    AssignLimits(MinValues, MaxValues, New3DArray,
                      ValuesToIgnore);
                  end;
                  NewCreateScreenObjects.Add(
                    TUndoCreateScreenObject.Create(ScreenObject))
                end;
              end;

              // read next array
              ReadArray(AnArray, EndReached,
                KPER, KSTP, ILAY, Description, Precision)
            end;
          end;
        mrFlux:
          begin
            Read3DArray(NLAY, EndReached, KPER, KSTP, Description, A3DArray,
              Precision, HufFormat);
            if clData.Checked[Index] then
            begin
              for LayerIndex := 0 to Abs(NLAY) - 1 do
              begin
                ILAY := frmGoPhast.PhastModel.LayerStructure.
                  ModflowLayerToDataSetLayer(LayerIndex+1)+1;
                CreateOrRetrieveLayerDataSet(Description, KSTP, KPER, ILAY,
                  LayerData, OldComment, NewDataSets, ScreenObjectsToDelete);
                CreateScreenObject(ILAY-1, ScreenObject);
                Assign3DValues(ScreenObject, LayerData, A3DArray, LayerIndex,
                  False, ValuesToIgnore);
                LayerNumbers.Add(ILAY);
                LayerDataSets.Add(LayerData);
                OldComments.AddObject(OldComment, LayerData);
                DataSetNames.AddObject(LayerData.Name, LayerData);
                MinValues.Add(LayerData.Limits.LowerLimit.RealLimitValue);
                MaxValues.Add(LayerData.Limits.UpperLimit.RealLimitValue);
                NewCreateScreenObjects.Add(
                  TUndoCreateScreenObject.Create(ScreenObject))
              end;
              Inc(Count);
              CreateOrRetrieve3DDataSet(Description, KPER, KSTP, LayerNumbers,
                LayerDataSets, New3DArray, OldComment, True, NewDataSets);
              OldComments.AddObject(OldComment, New3DArray);
              DataSetNames.AddObject(New3DArray.Name, New3DArray);
              comboColorGrid.Items.Objects[Count] := New3DArray;

              AssignLimits(MinValues, MaxValues, New3DArray, ValuesToIgnore);
            end;

          end;
        mrHufAscii, mrHufBinary:
          begin
            ReadArray(AnArray, EndReached,
              KPER, KSTP, ILAY, Description, Precision);
            Assert((KPER = FPeriods[Index])
              and (KSTP = FSteps[Index])
              and (Description = FDescriptions[Index]));
            Assert( ILAY > 0);
            // not a cross section
            if clData.Checked[Index] then
            begin
              HGU := frmGoPhast.PhastModel.HydrogeologicUnits[ILAY-1];
              Description := Description + ' ' + HGU.HufName;
              CreateOrRetrieveLayerDataSet(Description, KSTP, KPER, ILAY,
                LayerData, OldComment, NewDataSets, ScreenObjectsToDelete);
              CreateScreenObject(-1, ScreenObject);
              AssignValues(-1, ScreenObject, LayerData, AnArray,
                ValuesToIgnore, MinMaxAssigned);
              OldComments.AddObject(OldComment, LayerData);
              DataSetNames.AddObject(LayerData.Name, LayerData);
              Inc(Count);
              comboColorGrid.Items.Objects[Count] := LayerData;
              NewCreateScreenObjects.Add(
                TUndoCreateScreenObject.Create(ScreenObject))
            end;
          end;
        mrHufFlux:
          begin
            if (Index = 0) or ((Index mod NLAY) = 0) then
            begin
              Read3DArray(NLAY, EndReached, KPER, KSTP, Description, A3DArray,
                Precision, HufFormat);
            end;

            if clData.Checked[Index] then
            begin
              LayerIndex := Index mod NLAY;
              ILAY := LayerIndex+1;
              HGU := frmGoPhast.PhastModel.HydrogeologicUnits[ILAY-1];
              LayerDescription := Description + ' ' + HGU.HufName;
              CreateOrRetrieveLayerDataSet(LayerDescription, KSTP, KPER, ILAY,
                LayerData, OldComment, NewDataSets, ScreenObjectsToDelete);
              CreateScreenObject(-1, ScreenObject);
              Assign3DValues(ScreenObject, LayerData, A3DArray, LayerIndex,
                True, ValuesToIgnore);
              OldComments.AddObject(OldComment, LayerData);
              DataSetNames.AddObject(LayerData.Name, LayerData);
              Inc(Count);
              comboColorGrid.Items.Objects[Count] := LayerData;
              NewCreateScreenObjects.Add(
                TUndoCreateScreenObject.Create(ScreenObject))
            end;
          end;
        mfSubBinary:
          begin
            ReadArray(AnArray, EndReached,
              KPER, KSTP, ILAY, Description, Precision);
            Description := SubsidenceDescription(Description, ILAY);
            if clData.Checked[Index] then
            begin
              Inc(Count);
              CreateOrRetrieveLayerDataSet(Description, KSTP, KPER, ILAY,
                LayerData, OldComment, NewDataSets, ScreenObjectsToDelete,
                dafNone);
              comboColorGrid.Items.Objects[Count] := LayerData;
              CreateScreenObject(ILAY-1, ScreenObject);
              AssignValues(ILAY-1, ScreenObject, LayerData, AnArray,
                ValuesToIgnore, MinMaxAssigned);
              LayerNumbers.Add(ILAY);
              LayerDataSets.Add(LayerData);
              OldComments.AddObject(OldComment, LayerData);
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
    New3DArray := comboColorGrid.Items.Objects[comboColorGrid.ItemIndex]
      as TDataArray;
    if New3DArray <> nil then
    begin
      case rgDisplayChoice.ItemIndex of
        0:
          begin
            frmGoPhast.acColoredGrid.Enabled := True;
            frmGoPhast.acColoredGrid.Checked := True;
            frmGoPhast.tb3DColors.Down := True;
          end;
        1, 2:
          begin
            // do nothing
          end;
        else Assert(False);
      end;
    end;
    UndoImportResults := TUndoImportModelResults.Create(NewDataSets,
      DataSetNames, OldComments, New3DArray, TDisplayChoice(rgDisplayChoice.ItemIndex));
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

    frmGoPhast.UndoStack.Submit(UndoImportResults)
  finally
    CloseFiles;
    LayerNumbers.Free;
    LayerDataSets.Free;
    NewDataSets.Free;
    ScreenObjectsToDelete.Free;
    NewCreateScreenObjects.Free;
    MinValues.Free;
    MaxValues.Free;
    OldComments.Free;
    DataSetNames.Free;
//    Min3DValues.Free;
//    Max3DValues.Free;
  end;

end;

function TfrmSelectResultToImport.SubsidenceDescription(DESC: string;
  ILAY: integer): string;
begin
  result := Trim(DESC);
  if SameText(result, 'SUBSIDENCE') then
  begin
    Exit;
  end
  else if SameText(result, 'NDSYS COMPACTION')
    or SameText(result, 'ND CRITICAL HEAD')
    or SameText(result, 'DSYS COMPACTION')
    or SameText(result, 'D CRITICAL HEAD') then
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
begin
  inherited;
  FPeriods := TIntegerList.Create;
  FSteps := TIntegerList.Create;
  FDescriptions := TStringList.Create;
  FAskedUser := False;
end;

procedure TfrmSelectResultToImport.FormDestroy(Sender: TObject);
begin
  inherited;
  FPeriods.Free;
  FSteps.Free;
  FDescriptions.Free;
end;

procedure TfrmSelectResultToImport.odSelectFilesTypeChange(Sender: TObject);
//const
//  CB_FILENAME_ID = 1148;
var
  Dialog: TOpenDialog;
  NewFileName: string;
  Extension: string;
  Index: Integer;
  Position: integer;
begin
  inherited;
  Dialog := Sender as TOpenDialog;
  if (Dialog.FilterIndex > 1) and (Dialog.FileName <> '') then
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

    NewFileName :=
      ChangeFileExt(Dialog.FileName, Extension);
    Dialog.FileName := NewFileName;
    UpdateDialogBoxFileName(Dialog, NewFileName);
  end;
end;

function TfrmSelectResultToImport.SelectFiles: boolean;
var
  KSTP: Integer;
  AnArray: TModflowDoubleArray;
  A3DArray: T3DTModflowArray;
  KPER: Integer;
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
  Description: string;
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
      HGU := frmGoPhast.PhastModel.HydrogeologicUnits[ILAY-1];
      HufName := ' ' + HGU.HufName;
    end
    else
    begin
      HufName := '';
    end;

    result := Description + HufName + ': Period: ' + IntToStr(KPER)
      + '; Step: ' + IntToStr(KSTP);
    if TOTIM >= 0 then
    begin
      result := result + '; Total Time: ' + FloatToStr(TOTIM);
    end;
  end;
  procedure RecordItem(Description: string);
  begin
    Item := WriteLabel(Description);
    if clData.Items.IndexOf(Item) < 0 then
    begin
      FPeriods.Add(KPER);
      FSteps.Add(KSTP);
      FDescriptions.Add(TitleCase(Trim(Description)));
      clData.Items.Add(Item);
    end;
  end;
begin
  odSelectFiles.FileName := DefaultFileName;
  result := odSelectFiles.Execute;
  if result then
  begin
    try
      FFileName := odSelectFiles.FileName;
      FFileStream := nil;
      FFileVariable := nil;
      OpenResultFile(Precision, HufFormat);

      case FResultFormat of
        mrBinary:
          begin
            while FFileStream.Position < FFileStream.Size do
            begin
              case Precision of
                mpSingle:
                  ReadSinglePrecisionModflowBinaryRealArray(FFileStream, KSTP, KPER,
                    PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray);
                mpDouble:
                  ReadDoublePrecisionModflowBinaryRealArray(FFileStream, KSTP, KPER,
                    PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray);
                else Assert(False);
              end;
              RecordItem(DESC);
              if frmGoPhast.ModflowGrid.RowCount = 1 then
              begin
                if (frmGoPhast.PhastModel.LayerStructure.ModflowLayerCount <> NROW)
                  or (frmGoPhast.ModflowGrid.ColumnCount <> NCOL) then
                begin
                  Beep;
                  MessageDlg('The number of layers or columns in the data set doesn''t'
                    + ' match the number of layers or columns in the grid.',
                    mtError, [mbOK], 0);
                  result := False;
                  break;
                end;
              end
              else
              begin
                if (frmGoPhast.ModflowGrid.RowCount <> NROW)
                  or (frmGoPhast.ModflowGrid.ColumnCount <> NCOL) then
                begin
                  Beep;
                  MessageDlg('The number of rows or columns in the data set doesn''t'
                    + ' match the number of rows or columns in the grid.',
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
                PERTIM, TOTIM, DESC2, NCOL, NROW, ILAY, AnArray);
              RecordItem(DESC2);
              if frmGoPhast.ModflowGrid.RowCount = 1 then
              begin
                if (frmGoPhast.PhastModel.LayerStructure.ModflowLayerCount <> NROW)
                  or (frmGoPhast.ModflowGrid.ColumnCount <> NCOL) then
                begin
                  Beep;
                  MessageDlg('The number of layers or columns in the data set doesn''t'
                    + ' match the number of layers or columns in the grid.',
                    mtError, [mbOK], 0);
                  result := False;
                  break;
                end;
              end
              else
              begin
                if (frmGoPhast.ModflowGrid.RowCount <> NROW)
                  or (frmGoPhast.ModflowGrid.ColumnCount <> NCOL) then
                begin
                  Beep;
                  MessageDlg('The number of rows or columns in the data set doesn''t'
                    + ' match the number of rows or columns in the grid.',
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
                    PERTIM, TOTIM, DESC, NCOL, NROW, NLAY, A3DArray, HufFormat);
                mpDouble:
                  ReadModflowDoublePrecFluxArray(FFileStream, KSTP, KPER,
                    PERTIM, TOTIM, DESC, NCOL, NROW, NLAY, A3DArray, HufFormat);
                else Assert(False);
              end;
              RecordItem(DESC);
              if (frmGoPhast.ModflowGrid.RowCount <> NROW)
                or (frmGoPhast.ModflowGrid.ColumnCount <> NCOL)
                or (frmGoPhast.PhastModel.LayerStructure.ModflowLayerCount <> Abs(NLAY)) then
              begin
                Beep;
                MessageDlg('The number of rows, columns, or layers in the data set doesn''t'
                  + ' match the number of rows, columns, or layers in the grid.',
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
                PERTIM, TOTIM, DESC2, NCOL, NROW, ILAY, AnArray);
              RecordItem(DESC2);
              if (frmGoPhast.ModflowGrid.RowCount <> NROW)
                or (frmGoPhast.ModflowGrid.ColumnCount <> NCOL) then
              begin
                Beep;
                MessageDlg('The number of rows or columns in the data set doesn''t'
                  + ' match the number of rows or columns in the grid.',
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
                    PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray);
                mpDouble:
                  ReadDoublePrecisionModflowBinaryRealArray(FFileStream, KSTP, KPER,
                    PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray);
                else Assert(False);
              end;
              RecordItem(DESC);
              if (frmGoPhast.ModflowGrid.RowCount <> NROW)
                or (frmGoPhast.ModflowGrid.ColumnCount <> NCOL) then
              begin
                Beep;
                MessageDlg('The number of rows or columns in the data set doesn''t'
                  + ' match the number of rows or columns in the grid.',
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
                    PERTIM, TOTIM, DESC, NCOL, NROW, NLAY, A3DArray, HufFormat);
                mpDouble:
                  ReadModflowDoublePrecFluxArray(FFileStream, KSTP, KPER,
                    PERTIM, TOTIM, DESC, NCOL, NROW, NLAY, A3DArray, HufFormat);
                else Assert(False);
              end;
              for LayerIndex := 0 to Abs(NLAY) - 1 do
              begin
                ILAY := LayerIndex+1;
                RecordItem(DESC);
              end;
              if (frmGoPhast.ModflowGrid.RowCount <> NROW)
                or (frmGoPhast.ModflowGrid.ColumnCount <> NCOL)
                or (frmGoPhast.PhastModel.HydrogeologicUnits.Count <> Abs(NLAY)) then
              begin
                Beep;
                MessageDlg('The number of rows, columns, or hydrogeologic units in the data set doesn''t'
                  + ' match the number of rows, columns, or hydrogeologic units in the grid.',
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
                  ReadSinglePrecisionModflowBinaryRealArray(FFileStream, KSTP, KPER,
                    PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray);
                mpDouble:
                  ReadDoublePrecisionModflowBinaryRealArray(FFileStream, KSTP, KPER,
                    PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray);
                else Assert(False);
              end;
              Description := SubsidenceDescription(DESC, ILAY);
              RecordItem(Description);
              if (frmGoPhast.ModflowGrid.RowCount <> NROW)
                or (frmGoPhast.ModflowGrid.ColumnCount <> NCOL) then
              begin
                Beep;
                MessageDlg('The number of rows or columns in the data set doesn''t'
                  + ' match the number of rows or columns in the grid.',
                  mtError, [mbOK], 0);
                result := False;
                break;
              end;
            end;
          end
        else Assert(False);
      end;
    finally
      CloseFiles;
    end;
    if clData.Items.Count = 0 then
    begin
      Beep;
      MessageDlg('File is empty.', mtInformation, [mbOK], 0);
      result := False;
    end;
    if clData.Items.Count >= 1 then
    begin
      clData.Checked[clData.Items.Count-1] := True;
      clDataClickCheck(clData);
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
  Root := LayerData.Name + '_Object';
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
   var EndReached: Boolean; var KPER, KSTP: Integer; var Description: string;
   var A3DArray: T3DTModflowArray; Precision: TModflowPrecision; HufFormat: boolean);
var
  PERTIM: TModflowDouble;
  TOTIM: TModflowDouble;
  DESC: TModflowDesc;
  NCOL: Integer;
  NROW: Integer;
begin
  if FFileStream.Position < FFileStream.Size then
  begin
    case Precision of
      mpSingle:
        ReadModflowSinglePrecFluxArray(FFileStream, KSTP, KPER, PERTIM, TOTIM, DESC,
          NCOL, NROW, NLAY, A3DArray, HufFormat);
      mpDouble:
        ReadModflowDoublePrecFluxArray(FFileStream, KSTP, KPER, PERTIM, TOTIM, DESC,
          NCOL, NROW, NLAY, A3DArray, HufFormat);
      else Assert(False);
    end;
    Description := Trim(DESC);
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
  var EndReached: Boolean; var KPER: Integer; var KSTP: Integer;
  var ILAY: Integer; var Description: string; Precision: TModflowPrecision);
var
  NROW: Integer;
  DESC2: TModflowDesc2;
  DESC: TModflowDesc;
  PERTIM: TModflowDouble;
  NCOL: Integer;
  TOTIM: TModflowDouble;
begin
  case FResultFormat of
    mrBinary, mrHufBinary, mfSubBinary:
      begin
        if FFileStream.Position < FFileStream.Size then
        begin
          case Precision of
            mpSingle:
              ReadSinglePrecisionModflowBinaryRealArray(FFileStream, KSTP,
                KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray);
            mpDouble:
              ReadDoublePrecisionModflowBinaryRealArray(FFileStream, KSTP,
                KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray);
            else Assert(False);
          end;
          Description := Trim(DESC);
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
            DESC2, NCOL, NROW, ILAY, AnArray);
          Description := Trim(DESC2);
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

procedure TfrmSelectResultToImport.OpenResultFile(
  out Precision: TModflowPrecision; out HufFormat: boolean);
var
  Extension: string; 
begin
  Precision := mpSingle;
  Extension := ExtractFileExt(FFileName);
  if (SameText(Extension, '.bdn'))
    or (CompareText(Extension, '.bhd') = 0) then
  begin
    FResultFormat := mrBinary;
  end
  else if (SameText(Extension, '.fdn'))
    or (SameText(Extension, '.fhd')) then
  begin
    FResultFormat := mrAscii;
  end
  else if (SameText(Extension, '.cbc')) then
  begin
    FResultFormat := mrFlux;
  end
  else if (SameText(Extension, '.huf_fhd')) then
  begin
    FResultFormat := mrHufAscii;
  end
  else if (SameText(Extension, '.huf_bhd')) then
  begin
    FResultFormat := mrHufBinary;
  end
  else if (SameText(Extension, '.huf_flow')) then
  begin
    FResultFormat := mrHufFlux;
  end
  else if (SameText(Extension, '.Sub_Out')) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, '.SubSubOut')) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, '.SubComMlOut')) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, '.SubComIsOut')) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, '.SubVdOut')) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, '.SubNdCritHeadOut')) then
  begin
    FResultFormat := mfSubBinary;
  end
  else if (SameText(Extension, '.SubDCritHeadOut')) then
  begin
    FResultFormat := mfSubBinary;
  end
  else
  begin
    Assert(False);
  end;

  HufFormat:= false;
  case FResultFormat of
    mrBinary, mrHufBinary, mfSubBinary:
      begin
        FFileStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
        Precision := CheckArrayPrecision(FFileStream);
      end;
    mrFlux, mrHufFlux:
      begin
        FFileStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
        Precision := CheckBudgetPrecision(FFileStream, HufFormat);
      end;
    mrAscii, mrHufAscii:
      begin
        FFileVariable := TFileVariable.Create;
        AssignFile(FFileVariable.AFile, FFileName);
        Reset(FFileVariable.AFile);
        Precision := mpDouble;
      end;
    else Assert(False);
  end;
end;

{ TUndoImportModelResults }

constructor TUndoImportModelResults.Create(NewDataSets: TList;
  DataSetNames, OldComments: TStringList;
  DisplayDataSet: TDataArray; DisplayChoice: TDisplayChoice);
var
  DSIndex: Integer;
  DataArray: TDataArray;
  Position: Integer;
begin
  inherited Create;

  FNewEdgeDisplay := frmGoPhast.PhastModel.EdgeDisplay;
  FOldEdgeDisplay := frmGoPhast.PhastModel.EdgeDisplay;
  FNew3DDataSet := frmGoPhast.Grid.ThreeDDataSet;
  FOld3DDataSet := frmGoPhast.Grid.ThreeDDataSet;
  FNewThreeDTimeList := frmGoPhast.PhastModel.ThreeDTimeList;
  FOldThreeDTimeList := frmGoPhast.PhastModel.ThreeDTimeList;
  FNewTopDataSet := frmGoPhast.Grid.TopDataSet;
  FOldTopDataSet := frmGoPhast.Grid.TopDataSet;
  FNewTopTimeList := frmGoPhast.PhastModel.TopTimeList;
  FOldTopTimeList := frmGoPhast.PhastModel.TopTimeList;
  FNewFrontDataSet := frmGoPhast.Grid.FrontDataSet;
  FOldFrontDataSet := frmGoPhast.Grid.FrontDataSet;
  FNewFrontTimeList := frmGoPhast.PhastModel.FrontTimeList;
  FOldFrontTimeList := frmGoPhast.PhastModel.FrontTimeList;
  FNewSideDataSet := frmGoPhast.Grid.SideDataSet;
  FOldSideDataSet := frmGoPhast.Grid.SideDataSet;
  FNewSideTimeList := frmGoPhast.PhastModel.SideTimeList;
  FOldSideTimeList := frmGoPhast.PhastModel.SideTimeList;
  FNewTopContourDataSet := frmGoPhast.Grid.TopContourDataSet;
  FOldTopContourDataSet := frmGoPhast.Grid.TopContourDataSet;
  FNewFrontContourDataSet := frmGoPhast.Grid.FrontContourDataSet;
  FOldFrontContourDataSet := frmGoPhast.Grid.FrontContourDataSet;
  FNewSideContourDataSet := frmGoPhast.Grid.SideContourDataSet;
  FOldSideContourDataSet := frmGoPhast.Grid.SideContourDataSet;
  FNew3DContourDataSet := frmGoPhast.Grid.ThreeDContourDataSet;
  FOld3DContourDataSet := frmGoPhast.Grid.ThreeDContourDataSet;

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
      OldComments.Delete(Position);
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
  result := 'import model results';
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
begin
  inherited;
  for Index := 0 to FNewDataSets.Count - 1do
  begin
    DataSet := FNewDataSets[Index];
    frmGoPhast.DeletedDataSets.Extract(DataSet);
    if frmGoPhast.PhastModel.GetDataSetByName(DataSet.Name) = nil then
    begin
      frmGoPhast.PhastModel.AddDataSet(DataSet);
    end;
  end;

  for Index := 0 to FContainedUndos.Count - 1 do
  begin
    AnUndo := FContainedUndos[Index];
    AnUndo.DoCommand;
  end;
  for Index := 0 to frmGoPhast.PhastModel.DataSetCount - 1 do
  begin
    DataSet := frmGoPhast.PhastModel.DataSets[Index];
    if FNewDataSets.IndexOf(DataSet) < 0 then
    begin
      frmGoPhast.PhastModel.CreateVariables(DataSet);
    end;
  end;

  SetComments(FNewComments);

  frmGoPhast.PhastModel.EdgeDisplay := FNewEdgeDisplay;
  frmGoPhast.Grid.ThreeDDataSet := FNew3DDataSet;
  frmGoPhast.PhastModel.ThreeDTimeList := FNewThreeDTimeList;
  frmGoPhast.Grid.TopDataSet := FNewTopDataSet;
  frmGoPhast.PhastModel.TopTimeList := FNewTopTimeList;
  frmGoPhast.Grid.FrontDataSet := FNewFrontDataSet;
  frmGoPhast.PhastModel.FrontTimeList := FNewFrontTimeList;
  frmGoPhast.Grid.SideDataSet := FNewSideDataSet;
  frmGoPhast.PhastModel.SideTimeList := FNewSideTimeList;
  frmGoPhast.Grid.TopContourDataSet := FNewTopContourDataSet;
  frmGoPhast.Grid.FrontContourDataSet := FNewFrontContourDataSet;
  frmGoPhast.Grid.SideContourDataSet := FNewSideContourDataSet;
  frmGoPhast.Grid.ThreeDContourDataSet := FNew3DContourDataSet;
  frmGoPhast.Grid.GridChanged;

  UpdateFrmGridColor;
  UpdateFrmContourData;
end;

procedure TUndoImportModelResults.Undo;
var
  Index: Integer;
  AnUndo: TCustomUndo;
  DataSet: TDataArray;
begin
  inherited;
  for Index := FContainedUndos.Count - 1 downto 0 do
  begin
    AnUndo := FContainedUndos[Index];
    AnUndo.Undo;
  end;
  SetComments(FOldComments);

  frmGoPhast.PhastModel.EdgeDisplay := FOldEdgeDisplay;
  frmGoPhast.Grid.ThreeDDataSet := FOld3DDataSet;
  frmGoPhast.PhastModel.ThreeDTimeList := FOldThreeDTimeList;
  frmGoPhast.Grid.TopDataSet := FOldTopDataSet;
  frmGoPhast.PhastModel.TopTimeList := FOldTopTimeList;
  frmGoPhast.Grid.FrontDataSet := FOldFrontDataSet;
  frmGoPhast.PhastModel.FrontTimeList := FOldFrontTimeList;
  frmGoPhast.Grid.SideDataSet := FOldSideDataSet;
  frmGoPhast.PhastModel.SideTimeList := FOldSideTimeList;
  frmGoPhast.Grid.TopContourDataSet := FOldTopContourDataSet;
  frmGoPhast.Grid.FrontContourDataSet := FOldFrontContourDataSet;
  frmGoPhast.Grid.SideContourDataSet := FOldSideContourDataSet;
  frmGoPhast.Grid.ThreeDContourDataSet := FOld3DContourDataSet;
  frmGoPhast.Grid.GridChanged;

  UpdateFrmGridColor;
  UpdateFrmContourData;
  for Index := FNewDataSets.Count - 1 downto 0 do
  begin
    DataSet := FNewDataSets[Index];
    frmGoPhast.PhastModel.RemoveVariables(DataSet);
    frmGoPhast.PhastModel.ExtractDataSet(DataSet);
  end;
  frmGoPhast.DeletedDataSets.Assign(FNewDataSets, laOr);
//  for Index := 0 to frmGoPhast.PhastModel.DataSetCount - 1 do
//  begin
//    DataSet := frmGoPhast.PhastModel.DataSets[Index];
//    frmGoPhast.PhastModel.CreateVariables(DataSet);
//  end;
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

end.

