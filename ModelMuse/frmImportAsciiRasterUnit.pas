unit frmImportAsciiRasterUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomImportSimpleFileUnit, StdCtrls, Buttons, ExtCtrls, FastGEO, 
  frmImportShapefileUnit, AbstractGridUnit, GoPhastTypes, Grids, RbwDataGrid4;

type

  {@abstract(@name is the command used to import
    the ASCII Raster file or reverse the import.)}
  TUndoImportAsciiRasterFile = class(TUndoImportShapefile)
  protected
    // @name describes what @classname does.
    function Description: string; override;
  end;

  TfrmImportAsciiRaster = class(TfrmCustomImportSimpleFile)
    rgFilterMethod: TRadioGroup;
    rdgFilesAndDataSets: TRbwDataGrid4;
    procedure btnOKClick(Sender: TObject);
    procedure rdgFilesAndDataSetsButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure FormCreate(Sender: TObject); override;
  private
    FAsciiRasterFileName: string;
    FValues: TPoint3DArray;
    Grid: TCustomModelGrid;
    ImportMethod: TImportMethod;
    EvalAt: TEvaluatedAt;
    procedure SetData;
    procedure HandleARasterPoint(Sender: TObject; APoint: TPoint3D);
    { Private declarations }
  protected
    procedure SetCheckBoxCaptions; override;
    procedure GetDataSets; override;
  public
    function GetData: boolean;
    { Public declarations }
  end;

var
  frmImportAsciiRaster: TfrmImportAsciiRaster;

implementation

uses
  frmProgressUnit, AsciiRasterReaderUnit, frmGoPhastUnit, DataSetUnit,
  ScreenObjectUnit, UndoItems, ModelMuseUtilities, 
  ValueArrayStorageUnit, GIS_Functions;

resourcestring
  StrYouMustHaveAVali = 'You must have a valid grid before attempting ' +
    'to sample a raster file.';
  StrTheLocationsInS = 'The locations in %s that have data are different fro' +
  'm the locations in %s that have data.';
  StrImportASCIIRaster = 'import ASCII raster file';

{$R *.dfm}

type
  TAsciiRasterGridColumns = (gcNone, gcFileName, gcDataSet);

{ TfrmImportAsciiRaster }

procedure TfrmImportAsciiRaster.btnOKClick(Sender: TObject);
begin
  inherited;
  Screen.Cursor := crHourGlass;
  try
    SetData;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmImportAsciiRaster.FormCreate(Sender: TObject);
begin
  inherited;
  rdgFilesAndDataSets.Cells[Ord(gcFileName), 0] := 'File name';
  rdgFilesAndDataSets.Cells[Ord(gcDataSet), 0] := 'Data set';
end;

function TfrmImportAsciiRaster.GetData: boolean;
var
  AsciiReader: TAsciiRasterReader;
  FileHeader: TRasterHeader;
  NumberOfPoints: double;
  FileIndex: Integer;
begin
  UpdateEvalAt;

  result := OpenDialogFile.Execute;
  if result then
  begin
    if OpenDialogFile.Files.Count > 1 then
    begin
      rdgFilesAndDataSets.RowCount := OpenDialogFile.Files.Count + 1;
    end;
    rdgFilesAndDataSets.BeginUpdate;
    try
      for FileIndex := 0 to OpenDialogFile.Files.Count - 1 do
      begin

        FAsciiRasterFileName := OpenDialogFile.Files[FileIndex];
        if not FileExists(FAsciiRasterFileName) then
        begin
          result := False;
          Beep;
          MessageDlg('The file "' + FAsciiRasterFileName + '" does not exist.',
            mtError, [mbOK], 0);
          Exit;
        end;

        AsciiReader := TAsciiRasterReader.Create;
        try
          AsciiReader.FileName := FAsciiRasterFileName;
          result := AsciiReader.ValidFileHeader;
          if not result then
          begin
            Beep;
            MessageDlg('The file "' + FAsciiRasterFileName
              + '" is not an ASCII raster file.',
              mtError, [mbOK], 0);
            Exit;
          end
          else if FileIndex = 0 then
          begin
            FileHeader := AsciiReader.FileHeader;
            NumberOfPoints := FileHeader.NumberOfColumns * FileHeader.NumberOfRows;
            rgFilterMethod.Items[4] := Format('None (import all '
              + FloatToStrF(NumberOfPoints, ffNumber, 15, 0)
              + ' points)', [NumberOfPoints]);
          end;
        finally
          AsciiReader.Free;
        end;
        if OpenDialogFile.Files.Count > 1 then
        begin
          rdgFilesAndDataSets.Cells[Ord(gcFileName), FileIndex+1]
            := FAsciiRasterFileName;
          rdgFilesAndDataSets.Cells[Ord(gcDataSet), FileIndex+1]
            := rsNewDataSet;
        end;
      end;
    finally
      rdgFilesAndDataSets.EndUpdate;
    end;

    if OpenDialogFile.Files.Count = 1 then
    begin
      Caption := Caption + ' - ' + FAsciiRasterFileName;
    end
    else
    begin
      Caption := Caption + ' - multiple files';
      rdgFilesAndDataSets.Visible := True;
      lblDataSet.Visible := False;
      comboDataSets.Visible := False;
      Height := Height + 150;
//      rgFilterMethod.Controls[4].Enabled := False;
    end;
    GetDataSets;
    comboInterpolators.ItemIndex := 1;
  end;
end;

procedure TfrmImportAsciiRaster.GetDataSets;
begin
  inherited;
  rdgFilesAndDataSets.Columns[Ord(gcDataSet)].PickList := comboDataSets.Items;
end;

procedure TfrmImportAsciiRaster.HandleARasterPoint(Sender: TObject; APoint: TPoint3D);
var
  Point2D: TPoint2D;
begin
  if Grid.GridAngle <> 0 then
  begin
    Point2D.x := APoint.X;
    Point2D.y := APoint.Y;
    Point2D := Grid.
      RotateFromRealWorldCoordinatesToGridCoordinates(Point2D);
    APoint.x := Point2D.x;
    APoint.y := Point2D.y;
  end;
  HandleAPoint(APoint, ImportMethod, EvalAt, Grid);
end;

procedure TfrmImportAsciiRaster.rdgFilesAndDataSetsButtonClick(Sender: TObject;
  ACol, ARow: Integer);
begin
  inherited;
  OpenDialogFile.Options := OpenDialogFile.Options - [ofAllowMultiSelect];
  if OpenDialogFile.Execute then
  begin

  end;
end;

procedure TfrmImportAsciiRaster.SetCheckBoxCaptions;
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

procedure TfrmImportAsciiRaster.SetData;
var
  NewDataSets: TList;
  DataArrayName: string;
  DataArray: TDataArray;
  ScreenObjectList: TList;
  AScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  PointIndex: integer;
  Root: string;
  ExistingObjectCount: integer;
  Item: TValueArrayItem;
  DA_Position: integer;
  Undo: TUndoImportAsciiRasterFile;
  AsciiReader: TAsciiRasterReader;
  DataSetCount: Integer;
  DataSetIndex: Integer;
  APoint: TPoint2D;
  function ConvertPoint(const A3D_Point: TPoint3D): TPoint2D;
  begin
    result.X := A3D_Point.X;
    result.Y := A3D_Point.Y;
  end;
begin
//  OutputDebugString('SAMPLING ON');
  AScreenObject := nil;
  try
    frmGoPhast.PhastModel.BeginScreenObjectUpdate;
    frmGoPhast.CanDraw := False;
    try
      NewDataSets := TList.Create;
      try
        if rdgFilesAndDataSets.Visible then
        begin
          DataSetCount := rdgFilesAndDataSets.RowCount -1;
        end
        else
        begin
          DataSetCount := 1;
        end;

        ScreenObjectList := TList.Create;
        try
          Root := TScreenObject.ValidName(
            ExtractFileRoot(OpenDialogFile.FileName)+ '_');
          ExistingObjectCount :=
            frmGoPhast.PhastModel.NumberOfLargestScreenObjectsStartingWith(Root);

          AScreenObject :=
            TScreenObject.CreateWithViewDirection(
            frmGoPhast.PhastModel, vdTop,
            UndoCreateScreenObject, False);

          AScreenObject.Name := Root + IntToStr(ExistingObjectCount+1);
          AScreenObject.SetValuesOfEnclosedCells
            := False;
          AScreenObject.SetValuesOfIntersectedCells
            := cbIntersectedCells.Checked;
          AScreenObject.SetValuesByInterpolation
            := cbInterpolation.Checked;
          AScreenObject.ElevationCount := ecZero;
          case TEvaluatedAt(rgEvaluatedAt.ItemIndex) of
            eaBlocks:
              begin
                AScreenObject.Capacity := frmGoPhast.PhastModel.Grid.ColumnCount
                  * frmGoPhast.PhastModel.Grid.RowCount;
              end;
            eaNodes:
              begin
                AScreenObject.Capacity :=
                  (frmGoPhast.PhastModel.Grid.ColumnCount+1)
                  * (frmGoPhast.PhastModel.Grid.RowCount + 1);
              end;
            else
              Assert(False);
          end;
          AScreenObject.EvaluatedAt :=
            TEvaluatedAt(rgEvaluatedAt.ItemIndex);
          AScreenObject.Visible := False;

          for DataSetIndex := 0 to DataSetCount - 1 do
          begin

            if rgFilterMethod.ItemIndex = 4 then
            begin
              if DataSetCount = 1 then
              begin
                MakeNewDataSet(NewDataSets, '',
                  strDefaultClassification + '|Imported from an ASCII Raster file',
                  comboDataSets.ItemIndex = 0);
              end
              else
              begin
                FAsciiRasterFileName := rdgFilesAndDataSets.Cells[Ord(gcFileName), DataSetIndex+1];
                MakeNewDataSet(NewDataSets, '',
                  strDefaultClassification + '|Imported from an ASCII Raster file',
                  rdgFilesAndDataSets.ItemIndex[Ord(gcDataSet), DataSetIndex+1] = 0,
                  FAsciiRasterFileName);
              end;
            end
            else
            begin
              if DataSetCount = 1 then
              begin
                MakeNewDataSet(NewDataSets, '',
                  strDefaultClassification + '|Sampled from an ASCII Raster file',
                  comboDataSets.ItemIndex = 0);
              end
              else
              begin
                FAsciiRasterFileName := rdgFilesAndDataSets.Cells[Ord(gcFileName), DataSetIndex+1];
                MakeNewDataSet(NewDataSets, '',
                  strDefaultClassification + '|Sampled from an ASCII Raster file',
                  rdgFilesAndDataSets.ItemIndex[Ord(gcDataSet), DataSetIndex+1] = 0,
                  FAsciiRasterFileName);
              end;
            end;

            if (DataSetCount = 1) or
              (rdgFilesAndDataSets.ItemIndex[Ord(gcDataSet), DataSetIndex+1] = 0)  then
            begin
              DataArrayName := comboDataSets.Text;
            end
            else
            begin
              DataArrayName := rdgFilesAndDataSets.Cells[Ord(gcDataSet), DataSetIndex+1];
            end;

            DataArray := frmGoPhast.PhastModel.DataArrayManager.
              GetDataSetByName(DataArrayName);
            Assert(DataArray <> nil);

            AsciiReader := TAsciiRasterReader.Create;
            try
              AsciiReader.FileName := FAsciiRasterFileName;
              if rgFilterMethod.ItemIndex = 4 then
              begin
                try
                  frmProgressMM.PopupParent := self;
                  frmProgressMM.Caption := 'Progress';
                  frmProgressMM.Show;
                  try
                    AsciiReader.ReadAsciiRaster(FValues, frmProgressMM.pbProgress);
                  finally
                    frmProgressMM.Hide
                  end;
                except on E: EOutOfMemory do
                  begin
                    AScreenObject.Free;
                    Beep;
                    MessageDlg(E.Message, mtError, [mbOK], 0);
                    Exit;
                  end;
                end;
                AScreenObject.Capacity := Length(FValues);
                Item := AScreenObject.
                  ImportedValues.Add as TValueArrayItem;
                Item.Name := DataArray.Name;
                Item.Values.DataType := DataArray.DataType;
                Item.Values.Count := AScreenObject.Capacity;
                for PointIndex := 0 to Length(FValues) - 1 do
                begin
                  Assert(DataSetCount = 1);
                  APoint := ConvertPoint(FValues[PointIndex]);
                  if DataSetIndex = 0 then
                  begin
                    AScreenObject.AddPoint(APoint, True);
                  end
                  else
                  begin
                    if not PointsEqual(AScreenObject.Points[PointIndex], APoint) then
                    begin
                      raise EDifferentPointsError.Create(StrPointsDontMatch);
                    end;
                  end;
                  Item.Values.RealValues[PointIndex] := FValues[PointIndex].z;
                end;
                Item.CacheData;
              end
              else
              begin
                if (frmGoPhast.Grid.ColumnCount <= 0)
                  or (frmGoPhast.Grid.RowCount <= 0) then
                begin
                  AScreenObject.Free;
                  Beep;
                  MessageDlg(StrYouMustHaveAVali , mtError, [mbOK], 0);
                  Exit;
                end;
                try
                  GetGridMinMax;
                except on EInvalidGrid do
                  begin
                    AScreenObject.Free;
                    Beep;
                    MessageDlg(StrYouMustHaveAVali , mtError, [mbOK], 0);
                    Exit;
                  end;
                end;
                AsciiReader.OnReadPoint := HandleARasterPoint;
                EvalAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
                ImportMethod := TImportMethod(rgFilterMethod.ItemIndex);
                Grid := frmGoPhast.Grid;
                InitializeArrays(ImportMethod);

                frmProgressMM.PopupParent := self;
                frmProgressMM.Caption := 'Progress';
                frmProgressMM.Show;
                try
                  AsciiReader.ReadAsciiRaster(frmProgressMM.pbProgress);
                finally
                  frmProgressMM.Hide
                end;

                ComputeAverage(ImportMethod);

                Item := AScreenObject.
                  ImportedValues.Add as TValueArrayItem;
                Item.Name := DataArray.Name;
                Item.Values.DataType := DataArray.DataType;
                case EvalAt of
                  eaBlocks: AScreenObject.Capacity
                    := Grid.ColumnCount * Grid.RowCount;
                  eaNodes: AScreenObject.Capacity
                    := (Grid.ColumnCount+1) * (Grid.RowCount+1);
                  else Assert(False);
                end;
                Item.Values.Count := AScreenObject.Capacity;
                AssignPointsAndValues(Grid, AScreenObject, Item, DataSetIndex <> 0);
              end;
            finally
              AsciiReader.Free;
            end;

            DA_Position := AScreenObject.AddDataSet(DataArray);
            AScreenObject.DataSetFormulas[DA_Position] :=
              rsObjectImportedValuesR + '("' + DataArray.Name + '")';
          end;
          ScreenObjectList.Add(AScreenObject);

          Undo := TUndoImportAsciiRasterFile.Create;
          try
            Undo.StoreNewScreenObjects(ScreenObjectList);
            Undo.StoreNewDataSets(NewDataSets);
            frmGoPhast.UndoStack.Submit(Undo);
          except
            AScreenObject.Free;
            AScreenObject := nil;
            Undo.Free;
            raise;
          end;

          frmGoPhast.PhastModel.AddFileToArchive(OpenDialogFile.FileName);
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
  except on EDifferentPointsError do
    begin
      Beep;
      MessageDlg(Format(StrTheLocationsInS, [FAsciiRasterFileName,
        rdgFilesAndDataSets.Cells[Ord(gcFileName), 1]]), mtError, [mbOK], 0);
      AScreenObject.Free;
    end;
  end;
//  OutputDebugString('SAMPLING OFF');
end;

{ TUndoImportAsciiRasterFile }

function TUndoImportAsciiRasterFile.Description: string;
begin
  result := StrImportASCIIRaster;
end;

end.
