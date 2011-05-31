unit frmImportAsciiRasterUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomImportSimpleFileUnit, StdCtrls, Buttons, ExtCtrls, FastGEO, 
  frmImportShapefileUnit, AbstractGridUnit, GoPhastTypes;

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
    procedure btnOKClick(Sender: TObject);
  private
    FAsciiRasterFileName: string;
    FValues: TPoint3DArray;
    Grid: TCustomGrid;
    ImportMethod: TImportMethod;
    EvalAt: TEvaluatedAt;
    procedure SetData;
    procedure HandleARasterPoint(Sender: TObject; APoint: TPoint3D);
    { Private declarations }
  protected
    procedure SetCheckBoxCaptions; override;
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

{$R *.dfm}

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

function TfrmImportAsciiRaster.GetData: boolean;
var
  AsciiReader: TAsciiRasterReader;
  FileHeader: TRasterHeader;
  NumberOfPoints: double;
begin
  UpdateEvalAt;

  result := OpenDialogFile.Execute;
  if result then
  begin
    FAsciiRasterFileName := OpenDialogFile.FileName;
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
      else
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

    Caption := Caption + ' - ' + FAsciiRasterFileName;
    GetDataSets;
    comboInterpolators.ItemIndex := 1;
  end;
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
    msModflow, msModflowLGR:
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
  function ConvertPoint(const A3D_Point: TPoint3D): TPoint2D;
  begin
    result.X := A3D_Point.X;
    result.Y := A3D_Point.Y;
  end;
begin
//  OutputDebugString('SAMPLING ON');
  frmGoPhast.PhastModel.BeginScreenObjectUpdate;
  frmGoPhast.CanDraw := False;
  try
    NewDataSets := TList.Create;
    try
      if rgFilterMethod.ItemIndex = 4 then
      begin
        MakeNewDataSet(NewDataSets, '',
          strDefaultClassification + '|Imported from an ASCII Raster file');
      end
      else
      begin
        MakeNewDataSet(NewDataSets, '',
          strDefaultClassification + '|Sampled from an ASCII Raster file');
      end;

      DataArrayName := comboDataSets.Text;

      DataArray := frmGoPhast.PhastModel.DataArrayManager.
        GetDataSetByName(DataArrayName);
      Assert(DataArray <> nil);
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
        AScreenObject.Capacity := Length(FValues);
        AScreenObject.EvaluatedAt :=
          TEvaluatedAt(rgEvaluatedAt.ItemIndex);
        AScreenObject.Visible := False;

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
              AScreenObject.AddPoint(ConvertPoint(FValues[PointIndex]), True);
              Item.Values.RealValues[PointIndex] := FValues[PointIndex].z;
            end;
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
            AssignPointsAndValues(Grid, AScreenObject, Item);
          end;
        finally
          AsciiReader.Free;
        end;

        DA_Position := AScreenObject.AddDataSet(DataArray);
        AScreenObject.DataSetFormulas[DA_Position] :=
          rsObjectImportedValuesR + '("' + DataArray.Name + '")';
        ScreenObjectList.Add(AScreenObject);

        Undo := TUndoImportAsciiRasterFile.Create;
        try
          Undo.StoreNewScreenObjects(ScreenObjectList);
          Undo.StoreNewDataSets(NewDataSets);
          frmGoPhast.UndoStack.Submit(Undo);
        except
          AScreenObject.Free;
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
//  OutputDebugString('SAMPLING OFF');
end;

{ TUndoImportAsciiRasterFile }

function TUndoImportAsciiRasterFile.Description: string;
begin
  result := 'import ASCII raster file';
end;

end.
