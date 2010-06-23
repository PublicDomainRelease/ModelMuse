unit frmImportDEMUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomImportSimpleFileUnit, StdCtrls, Buttons, ExtCtrls,
  frmImportShapefileUnit, ArgusDataEntry;

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
    { Private declarations }
  public
    function GetData: boolean;
    { Public declarations }
  end;

implementation

uses
  AbstractGridUnit, frmGoPhastUnit, DemReaderUnit, FastGEO, GoPhastTypes,
  ScreenObjectUnit, ModelMuseUtilities, DataSetUnit, frmProgressUnit, UndoItems,
  ValueArrayStorageUnit, GIS_Functions, CoordinateConversionUnit;

{$R *.dfm}

type TImportMethod = (imLowest, imHighest, imAverage, imClosest);

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
  frmProgress.pbProgress.Position
    := Round(frmProgress.pbProgress.Max * FractionDone);
  frmProgress.ProgressLabelCaption := 'Reading ' + OpenDialogFile.Files[FDemIndex];
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
begin
  CentralMeridian := 0.0;
  for DemIndex := 0 to OpenDialogFile.Files.Count - 1 do
  begin
    DemReader := TDemReader.Create;
    try
      DemReader.ReadHeader(OpenDialogFile.Files[DemIndex]);
      if DemIndex = 0 then
      begin
        CentralMeridian := DemReader.CentralMeridianRadians;
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
  FractionDone := (OpenDialogFile.Files.Count + FractionDone)/(OpenDialogFile.Files.Count+1);
  frmProgress.pbProgress.Position
    := Round(frmProgress.pbProgress.Max * FractionDone);
  frmProgress.ProgressLabelCaption := 'Importing data';
  Application.ProcessMessages;
end;

function TfrmImportDEM.GetData: boolean;
var
  Grid: TCustomGrid;
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
    DisplayCornerCoordinates;
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
    msModflow:
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
  Values: array of array of double;
  Counts: array of array of integer;
  CenterPoints: array of array of TPoint2D;
  Distances: array of array of double;
  Grid: TCustomGrid;
  DemReader: TDemReader;
  FirstFile: string;
  CentralMeridian: Double;
  DemIndex: Integer;
  PointIndex: Integer;
  APoint: TElevationPoint;
  Point2D: TPoint2D;
  MinX: Real;
  MaxX: Real;
  MinY: Real;
  MaxY: Real;
  EvalAt: TEvaluatedAt;
  ACol: integer;
  ARow: Integer;
  ColIndex: Integer;
  RowIndex: Integer;
  ImportMethod: TImportMethod;
  ADistance: TFloat;
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
  ValueIndex: Integer;
  IgnoreValue: Integer;
  procedure EnsureMinMax(var MinValue, MaxValue: Real);
  var
    Temp: Real;
  begin
    if MinValue > MaxValue then
    begin
      Temp := MinValue;
      MinValue := MaxValue;
      MaxValue := Temp;
    end;
  end;
begin
  frmProgress.Show;
  try
    Grid := frmGoPhast.PhastModel.Grid;
    EvalAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
    ImportMethod := TImportMethod(rgFilterMethod.ItemIndex);
    case EvalAt of
      eaBlocks:
        begin
          SetLength(Values, Grid.RowCount, Grid.ColumnCount);
          SetLength(Counts, Grid.RowCount, Grid.ColumnCount);
          for RowIndex := 0 to Grid.RowCount - 1 do
          begin
            for ColIndex := 0 to Grid.ColumnCount - 1 do
            begin
              Counts[RowIndex,ColIndex]  := 0;
            end;
          end;

          SetLength(CenterPoints, Grid.RowCount, Grid.ColumnCount);
          for RowIndex := 0 to Grid.RowCount - 1 do
          begin
            for ColIndex := 0 to Grid.ColumnCount - 1 do
            begin
              CenterPoints[RowIndex,ColIndex]  :=
                Grid.UnrotatedTwoDElementCenter(ColIndex, RowIndex);
            end;
          end;

          if ImportMethod = imClosest then
          begin
            SetLength(Distances, Grid.RowCount, Grid.ColumnCount);
          end;
        end;
      eaNodes:
        begin
          SetLength(Values, Grid.RowCount+1, Grid.ColumnCount+1);
          SetLength(Counts, Grid.RowCount+1, Grid.ColumnCount+1);
          for RowIndex := 0 to Grid.RowCount do
          begin
            for ColIndex := 0 to Grid.ColumnCount do
            begin
              Counts[RowIndex,ColIndex]  := 0;
            end;
          end;
          SetLength(CenterPoints, Grid.RowCount+1, Grid.ColumnCount+1);
          for RowIndex := 0 to Grid.RowCount do
          begin
            for ColIndex := 0 to Grid.ColumnCount do
            begin
              CenterPoints[RowIndex,ColIndex]  :=
                Grid.UnrotatedTwoDElementCorner(ColIndex, RowIndex);
            end;
          end;
          if ImportMethod = imClosest then
          begin
            SetLength(Distances, Grid.RowCount+1, Grid.ColumnCount+1);
          end;
        end;
      else
        Assert(False);
    end;
    FirstFile := OpenDialogFile.Files[0];
    DemReader := TDemReader.Create;
    try
      DemReader.ReadHeader(FirstFile);
      CentralMeridian := DemReader.CentralMeridianRadians;
    finally
      DemReader.Free;
    end;

    MinX := Grid.ColumnPosition[0];
    MaxX := Grid.ColumnPosition[Grid.ColumnCount];
    EnsureMinMax(MinX, MaxX);
    MinY := Grid.RowPosition[0];
    MaxY := Grid.RowPosition[Grid.RowCount];
    EnsureMinMax(MinY, MaxY);

    IgnoreValue := StrToInt(rdeIgnore.Text);

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
          if (Point2D.x >= MinX) and (Point2D.x <= MaxX)
            and (Point2D.y >= MinY) and (Point2D.y <= MaxY) then
          begin
            ACol := -1;
            ARow := -1;
            case EvalAt of
              eaBlocks:
                begin
                  ACol := Grid.GetContainingColumn(Point2D.x);
                  ARow := Grid.GetContainingRow(Point2D.y);
                end;
              eaNodes:
                begin
                  ACol := Grid.NearestColumnPosition(Point2D.x);
                  ARow := Grid.NearestRowPosition(Point2D.y);
                end
              else Assert(False);
            end;
            if Counts[ARow, ACol] = 0 then
            begin
              Counts[ARow, ACol] := 1;
              Values[ARow, ACol] := APoint.Elevation;
              if ImportMethod = imClosest then
              begin
                Distances[ARow, ACol] := Distance(Point2D, CenterPoints[ARow, ACol]);
              end;
            end;
            case ImportMethod of
              imLowest:
                begin
                  if Values[ARow, ACol] > APoint.Elevation then
                  begin
                    Values[ARow, ACol] := APoint.Elevation;
                  end;
                end;
              imHighest:
                begin
                  if Values[ARow, ACol] < APoint.Elevation then
                  begin
                    Values[ARow, ACol] := APoint.Elevation;
                  end;
                end;
              imAverage:
                begin
                  Values[ARow, ACol] := Values[ARow, ACol] + APoint.Elevation;
                  Counts[ARow, ACol] := Counts[ARow, ACol] + 1;
                end;
              imClosest:
                begin
                  ADistance := Distance(Point2D, CenterPoints[ARow, ACol]);
                  if Distances[ARow, ACol] > ADistance then
                  begin
                    Values[ARow, ACol] := APoint.Elevation;
                    Distances[ARow, ACol] := ADistance;
                  end;
                end;
            end;
          end;
        end;
      finally
        DemReader.Free;
      end;
    end;
    if ImportMethod = imAverage then
    begin
      for RowIndex := 0 to Length(Values)-1 do
      begin
        ImportProgress(self, RowIndex/Length(Values));
        for ColIndex := 0 to Length(Values[0])-1 do
        begin
          if Counts[RowIndex,ColIndex] > 1  then
          begin
            Values[RowIndex,ColIndex] :=
              Values[RowIndex,ColIndex]/Counts[RowIndex,ColIndex]
          end;
        end;
      end;
    end;
    frmGoPhast.PhastModel.BeginScreenObjectUpdate;
    frmGoPhast.CanDraw := False;
    try
      NewDataSets := TList.Create;
      try
        MakeNewDataSet(NewDataSets, '_DEM_Elevation',
          strDefaultClassification + '|Sampled from DEM files using '
          + LowerCase(rgFilterMethod.Items[rgFilterMethod.ItemIndex]));
        DataSetName := comboDataSets.Text;
        DataSet := frmGoPhast.PhastModel.GetDataSetByName(DataSetName);
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

          ValueIndex := 0;
          for RowIndex := 0 to Length(Values)-1 do
          begin
            for ColIndex := 0 to Length(Values[0])-1 do
            begin
              if Counts[RowIndex,ColIndex] > 0  then
              begin
                Point2D := CenterPoints[RowIndex,ColIndex];
                Point2D := Grid.
                  RotateFromGridCoordinatesToRealWorldCoordinates(Point2D);
                AScreenObject.AddPoint(Point2D, True);
                Item.Values.RealValues[ValueIndex] := Values[RowIndex,ColIndex];
                Inc(ValueIndex);
              end;
            end;
          end;
          Item.Values.Count := ValueIndex;
          DS_Position := AScreenObject.AddDataSet(DataSet);
          AScreenObject.DataSetFormulas[DS_Position] :=
            rsObjectImportedValuesR + '("' + DataSet.Name + '")';
          ScreenObjectList.Add(AScreenObject);

          Undo := TUndoImportDemFile.Create;
          try
            Undo.StoreNewScreenObjects(ScreenObjectList);
            Undo.StoreNewDataSets(NewDataSets);
            frmGoPhast.UndoStack.Submit(Undo);
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
    frmProgress.Hide;
  end;
end;

{ TUndoImportDemFile }

function TUndoImportDemFile.Description: string;
begin
  result := 'sample Digital Elevation Model';
end;

end.
