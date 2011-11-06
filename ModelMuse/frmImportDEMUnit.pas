unit frmImportDEMUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomImportSimpleFileUnit, StdCtrls, Buttons, ExtCtrls,
  frmImportShapefileUnit, ArgusDataEntry, FastGEO, AbstractGridUnit,
  GoPhastTypes, ScreenObjectUnit, ValueArrayStorageUnit;

type
  {@abstract(@name is the command used to import
    DEM files or reverse the import.)}
  TUndoImportDemFile = class(TUndoImportShapefile)
  protected
    // @name describes what @classname does.
    function Description: string; override;
  end;

  TfrmImportDEM = class(TfrmCustomImportSimpleFile)
    rgFilterMethod: TRadioGroup;
    memoCorners: TMemo;
    Label1: TLabel;
    cbIgnore: TCheckBox;
    rdeIgnore: TRbwDataEntry;
    procedure btnOKClick(Sender: TObject);
    procedure cbIgnoreClick(Sender: TObject);
  protected
    procedure SetCheckBoxCaptions; override;
  private
    FDemIndex: Integer;
    procedure SetData;
    procedure DemProgress(Sender: TObject; FractionDone: double);
    procedure ImportProgress(Sender: TObject; FractionDone: double);
    procedure DisplayCornerCoordinates;
    procedure InvalidDem;
    { Private declarations }
  public
    function GetData: boolean;
    { Public declarations }
  end;

implementation

uses
  frmGoPhastUnit, DemReaderUnit, 
  ModelMuseUtilities, DataSetUnit, frmProgressUnit, UndoItems,
  GIS_Functions, CoordinateConversionUnit;

{$R *.dfm}


{ TfrmCustomImportSimpleFile2 }

procedure TfrmImportDEM.btnOKClick(Sender: TObject);
begin
  inherited;
  Screen.Cursor := crHourGlass;
  try
    SetData;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmImportDEM.cbIgnoreClick(Sender: TObject);
begin
  inherited;
  rdeIgnore.Enabled := cbIgnore.Checked;
end;

procedure TfrmImportDEM.DemProgress(Sender: TObject; FractionDone: double);
begin
  FractionDone := (FDemIndex + FractionDone)/(OpenDialogFile.Files.Count+1);
  frmProgressMM.pbProgress.Position
    := Round(frmProgressMM.pbProgress.Max * FractionDone);
  frmProgressMM.ProgressLabelCaption := 'Reading ' + OpenDialogFile.Files[FDemIndex];
  Application.ProcessMessages;
end;

procedure TfrmImportDEM.DisplayCornerCoordinates;
const
  FormatString = '(%g, %g)';
var
  DemIndex: Integer;
  DemReader: TDemReader;
  CornerIndex: Integer;
  Corner: TCornerPoint;
  Line: string;
  CentralMeridian: double;
  X: Double;
  Y: Double;
//  OldDecSep: Char;
begin
  CentralMeridian := 0.0;
  for DemIndex := 0 to OpenDialogFile.Files.Count - 1 do
  begin
    DemReader := TDemReader.Create;
    try
      DemReader.ReadHeader(OpenDialogFile.Files[DemIndex]);
      if DemIndex = 0 then
      begin
//        OldDecSep := FormatSettings.DecimalSeparator;
//        try
          CentralMeridian := DemReader.CentralMeridianRadians;
//        finally
//          FormatSettings.DecimalSeparator := OldDecSep;
//        end;
      end;
      memoCorners.Lines.Add(OpenDialogFile.Files[DemIndex]);
      for CornerIndex := 0 to 3 do
      begin
        Corner := DemReader.Corners[CornerIndex];
        if DemReader.CoordInSec then
        begin
          ConvertToUTM(Corner.Y/60/60/180*PI, Corner.X/60/60/180*PI, CentralMeridian,
            X, Y);
          Line := Format(FormatString, [Corner.X/3600, Corner.Y/3600])
            + '; ' + Format(FormatString,[X,Y]);
        end
        else
        begin
          Line := Format(FormatString, [Corner.X, Corner.Y]);
        end;
        memoCorners.Lines.Add(Line);
      end;
    finally
      DemReader.Free;
    end;
  end;
end;

procedure TfrmImportDEM.ImportProgress(Sender: TObject; FractionDone: double);
begin
  FractionDone := (OpenDialogFile.Files.Count + FractionDone)/
    (OpenDialogFile.Files.Count+1);
  frmProgressMM.pbProgress.Position
    := Round(frmProgressMM.pbProgress.Max * FractionDone);
  frmProgressMM.ProgressLabelCaption := 'Importing data';
  Application.ProcessMessages;
end;

procedure TfrmImportDEM.InvalidDem;
begin
  Beep;
  MessageDlg('There was an error reading the DEM file.  Please check that the '
    + 'format of the DEM is a format that ModelMuse supports.  Check the '
    + 'ModelMuse help to see what formats ModelMuse supports.  For further '
    + 'assistance contact rbwinst@usgs.gov.', mtError, [mbOK], 0);
end;

function TfrmImportDEM.GetData: boolean;
var
  Grid: TCustomModelGrid;
begin
  Grid := frmGoPhast.PhastModel.Grid;
  result := (Grid <> nil) and (Grid.ColumnCount > 0)
    and (Grid.RowCount > 0);
  if result then
  begin
    result := OpenDialogFile.Execute;
  end
  else
  begin
    MessageDlg('You must create the grid before importing '
      + 'a Digital Elevation Model.', mtInformation, [mbOK], 0);
  end;
  if result then
  begin
  GetDataSets;
    GetInterpolators;
    UpdateEvalAt;
    SetCheckBoxCaptions;
    try
      DisplayCornerCoordinates;
    except
      on EConvertError do
      begin
        result := False;
        InvalidDem
      end;
      on E: EInOutError do
      begin
        result := False;
        Beep;
        MessageDlg(E.Message, mtError, [mbOK], 0);
      end;
    end;
  end;
end;

procedure TfrmImportDEM.SetCheckBoxCaptions;
var
  NodeElemString: string;
  EvalAt: TEvaluatedAt;
  CenterString: string;
begin
  inherited;
  EvalAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
  case frmGoPhast.PhastModel.ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast:
      begin
        case EvalAt of
          eaBlocks:
            begin
              NodeElemString := 'element';
              CenterString := 'element center'
            end;
          eaNodes:
            begin
              NodeElemString := 'cell';
              CenterString := 'node';
            end;
        end;
      end;
    msModflow, msModflowLGR, msModflowNWT:
      begin
        NodeElemString := 'cell';
        CenterString := 'cell center'
      end;
    else Assert(False);
  end;
  rgFilterMethod.Items[Ord(imLowest)] := 'Lowest point in ' + NodeElemString;
  rgFilterMethod.Items[Ord(imHighest)] := 'Highest point in ' + NodeElemString;
  rgFilterMethod.Items[Ord(imAverage)] :=
    'Average of points in ' + NodeElemString;
  rgFilterMethod.Items[Ord(imClosest)] := 'Point closest to ' + CenterString;
end;

procedure TfrmImportDEM.SetData;
var
  Grid: TCustomModelGrid;
  DemReader: TDemReader;
  FirstFile: string;
  CentralMeridian: Double;
  DemIndex: Integer;
  PointIndex: Integer;
  APoint: TElevationPoint;
  Point2D: TPoint2D;
  EvalAt: TEvaluatedAt;
  ImportMethod: TImportMethod;
  NewDataSets: TList;
  Undo: TUndoImportDemFile;
  Root: string;
  DataSetName: string;
  DataSet: TDataArray;
  ScreenObjectList: TList;
  ExistingObjectCount: Integer;
  AScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  Item: TValueArrayItem;
  DS_Position: Integer;
  IgnoreValue: Integer;
  APoint3D: TPoint3D;
begin
  frmProgressMM.Caption := 'Progress';
  frmProgressMM.Show;
  try
    Grid := frmGoPhast.PhastModel.Grid;
    EvalAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
    ImportMethod := TImportMethod(rgFilterMethod.ItemIndex);
    InitializeArrays(ImportMethod);
    FirstFile := OpenDialogFile.Files[0];
    try
      DemReader := TDemReader.Create;
      try
        DemReader.ReadHeader(FirstFile);
        CentralMeridian := DemReader.CentralMeridianRadians;
      finally
        DemReader.Free;
      end;
    except on EConvertError do
      begin
        InvalidDEM;
        Exit;
      end;
    end;
    GetGridMinMax;

    IgnoreValue := StrToInt(rdeIgnore.Text);
    try
      for DemIndex := 0 to OpenDialogFile.Files.Count - 1 do
      begin
        FDemIndex := DemIndex;
        DemReader := TDemReader.Create;
        try
          DemReader.OnProgress := DemProgress;
          DemReader.CentralMeridianRadians := CentralMeridian;
          DemReader.ReadFile(OpenDialogFile.Files[DemIndex], False);
          for PointIndex := 0 to DemReader.PointCount - 1 do
          begin
            APoint := DemReader.Points[PointIndex];
            if cbIgnore.Checked and (IgnoreValue = APoint.Value) then
            begin
              Continue
            end;

            Point2D.x := APoint.X;
            Point2D.y := APoint.Y;
            Point2D := Grid.
              RotateFromRealWorldCoordinatesToGridCoordinates(Point2D);
            APoint3D.x := Point2D.x;
            APoint3D.y := Point2D.y;
            APoint3D.z := APoint.Elevation;
            HandleAPoint(APoint3D, ImportMethod, EvalAt, Grid);
          end;
        finally
          DemReader.Free;
        end;
      end;
    except on EConvertError do
      begin
        InvalidDEM;
        Exit;
      end;
    end;
    ComputeAverage(ImportMethod, ImportProgress);
    frmGoPhast.PhastModel.BeginScreenObjectUpdate;
    frmGoPhast.CanDraw := False;
    try
      NewDataSets := TList.Create;
      try
        MakeNewDataSet(NewDataSets, '_DEM_Elevation',
          strDefaultClassification + '|Sampled from DEM files using '
          + LowerCase(rgFilterMethod.Items[rgFilterMethod.ItemIndex]),
          comboDataSets.ItemIndex = 0);
        DataSetName := comboDataSets.Text;
        DataSet := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(DataSetName);
        Assert(DataSet <> nil);
        ScreenObjectList := TList.Create;
        try
          Root := TScreenObject.ValidName(
            ExtractFileRoot(OpenDialogFile.FileName)+ '_');
          ScreenObjectList.Capacity := 1;
          ExistingObjectCount :=
            frmGoPhast.PhastModel.NumberOfLargestScreenObjectsStartingWith(Root);

          AScreenObject :=
            TScreenObject.CreateWithViewDirection(
            frmGoPhast.PhastModel, vdTop,
            UndoCreateScreenObject, False);
          Inc(ExistingObjectCount);
          AScreenObject.Name := Root + IntToStr(ExistingObjectCount);
          AScreenObject.SetValuesOfEnclosedCells
            := cbEnclosedCells.Checked;
          AScreenObject.SetValuesOfIntersectedCells
            := cbIntersectedCells.Checked;
          AScreenObject.SetValuesByInterpolation
            := cbInterpolation.Checked;
          AScreenObject.ColorLine := False;
          AScreenObject.FillScreenObject := False;
          AScreenObject.ElevationCount := ecZero;
          AScreenObject.Capacity := Length(Values) * Length(Values[0]);
          AScreenObject.EvaluatedAt := EvalAt;
          AScreenObject.Visible := False;

          Item := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          Item.Name := DataSet.Name;
          Item.Values.DataType := DataSet.DataType;
          Item.Values.Count := AScreenObject.Capacity;
          AssignPointsAndValues(Grid, AScreenObject, Item);


          DS_Position := AScreenObject.AddDataSet(DataSet);
          AScreenObject.DataSetFormulas[DS_Position] :=
            rsObjectImportedValuesR + '("' + DataSet.Name + '")';
          if AScreenObject.Count = 0 then
          begin
            Beep;
            MessageDlg('None of the points in the DEM are inside the grid.',
              mtError, [mbOK], 0);
            AScreenObject.Free;
          end
          else
          begin
            ScreenObjectList.Add(AScreenObject);
          end;

          Undo := TUndoImportDemFile.Create;
          try
            Undo.StoreNewScreenObjects(ScreenObjectList);
            Undo.StoreNewDataSets(NewDataSets);
            frmGoPhast.UndoStack.Submit(Undo);
            if ScreenObjectList.Count = 0 then
            begin
              frmGoPhast.UndoStack.Undo(1);
              frmGoPhast.UndoStack.Remove(Undo);
              Undo.Free;
              frmGoPhast.tbRedo.Enabled := False;
            end;
          except
            Undo.Free;
            raise;
          end;
          for DemIndex := 0 to OpenDialogFile.Files.Count-1 do
          begin
            frmGoPhast.PhastModel.AddFileToArchive
              (OpenDialogFile.Files[DemIndex]);
          end;
        finally
          ScreenObjectList.Free;
        end;
      finally
        NewDataSets.Free;
      end;
    finally
      frmGoPhast.CanDraw := True;
      frmGoPhast.PhastModel.EndScreenObjectUpdate;
    end;
  finally
    frmProgressMM.Hide;
  end;
end;

{ TUndoImportDemFile }

function TUndoImportDemFile.Description: string;
begin
  result := 'sample Digital Elevation Model';
end;

end.
