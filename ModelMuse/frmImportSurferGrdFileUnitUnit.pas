unit frmImportSurferGrdFileUnitUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, frmCustomImportSimpleFileUnit, StdCtrls,
  Buttons, ExtCtrls, Grids, RbwDataGrid4, SurferGridFileReaderUnit,
  frmImportShapefileUnit;

type
  {@abstract(@name is the command used to import
    Surfer Grid files or reverse the import.)}
  TUndoImportGrdFile = class(TUndoImportShapefile)
  protected
    // @name describes what @classname does.
    function Description: string; override;
  end;

  TfrmImportSurferGrdFile = class(TfrmCustomImportSimpleFile)
    rdgLimits: TRbwDataGrid4;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
  private
    FGrd6: TSurfer6Grid;
    FFileType: TSurferFileType;
    FGrd7: TSurfer7Grid;
    procedure SetData;
    { Private declarations }
  public
    function GetData: boolean;
    { Public declarations }
  end;

var
  frmImportSurferGrdFile: TfrmImportSurferGrdFile;

implementation

uses
  frmGoPhastUnit, DataSetUnit, ScreenObjectUnit, ModelMuseUtilities,
  GoPhastTypes, UndoItems, FastGEO, ValueArrayStorageUnit, GIS_Functions;

{$R *.dfm}

function ConvertPoint(const SurferPoint: TSurferPoint): TPoint2D;
begin
  result.X := SurferPoint.X;
  result.Y := SurferPoint.Y;
end;


procedure TfrmImportSurferGrdFile.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmImportSurferGrdFile.FormCreate(Sender: TObject);
begin
  inherited;
  rdgLimits.BeginUpdate;
  try
    rdgLimits.Cells[1,0] := 'X';
    rdgLimits.Cells[2,0] := 'Y';
    rdgLimits.Cells[3,0] := 'Z';
    rdgLimits.Cells[0,1] := 'Minimum';
    rdgLimits.Cells[0,2] := 'Maximum';
  finally
    rdgLimits.EndUpdate;
  end;
end;


function TfrmImportSurferGrdFile.GetData: boolean;
begin
  UpdateEvalAt;
  result := OpenDialogFile.Execute;
  if result then
  begin
    FFileType := SurferFileType(OpenDialogFile.FileName);
    case FFileType of
      sft6:
        begin
          ReadSurfer6GrdFile(OpenDialogFile.FileName, FGrd6);
          rdgLimits.BeginUpdate;
          try
            rdgLimits.Cells[1,1] := FloatToStr(FGrd6.Header.Xlo);
            rdgLimits.Cells[1,2] := FloatToStr(FGrd6.Header.Xhi);
            rdgLimits.Cells[2,1] := FloatToStr(FGrd6.Header.Ylo);
            rdgLimits.Cells[2,2] := FloatToStr(FGrd6.Header.Yhi);
            rdgLimits.Cells[3,1] := FloatToStr(FGrd6.Header.Zlo);
            rdgLimits.Cells[3,2] := FloatToStr(FGrd6.Header.Zhi);
          finally
            rdgLimits.EndUpdate;
          end;
        end;
      sft7:
        begin
          ReadSurfer7GrdFile(OpenDialogFile.FileName, FGrd7);
          rdgLimits.BeginUpdate;
          try
            rdgLimits.Cells[1,1] := FloatToStr(FGrd7.Header.xLL);
            rdgLimits.Cells[1,2] := FloatToStr(FGrd7.Header.xLL
              + FGrd7.Header.xSize * FGrd7.Header.nCol);
            rdgLimits.Cells[2,1] := FloatToStr(FGrd7.Header.yLL);
            rdgLimits.Cells[2,2] := FloatToStr(FGrd7.Header.yLL
              + FGrd7.Header.ySize * FGrd7.Header.nRow);
            rdgLimits.Cells[3,1] := FloatToStr(FGrd7.Header.zMin);
            rdgLimits.Cells[3,2] := FloatToStr(FGrd7.Header.zMax);
          finally
            rdgLimits.EndUpdate;
          end;
        end;
      sftAscii:
        begin
          ReadSurferAsciiFile(OpenDialogFile.FileName, FGrd6);
          rdgLimits.BeginUpdate;
          try
            rdgLimits.Cells[1,1] := FloatToStr(FGrd6.Header.Xlo);
            rdgLimits.Cells[1,2] := FloatToStr(FGrd6.Header.Xhi);
            rdgLimits.Cells[2,1] := FloatToStr(FGrd6.Header.Ylo);
            rdgLimits.Cells[2,2] := FloatToStr(FGrd6.Header.Yhi);
            rdgLimits.Cells[3,1] := FloatToStr(FGrd6.Header.Zlo);
            rdgLimits.Cells[3,2] := FloatToStr(FGrd6.Header.Zhi);
          finally
            rdgLimits.EndUpdate;
          end;
        end;
      else Assert(False);
    end;
    GetDataSets;
    comboDataSets.ItemIndex := 0;
    comboInterpolators.ItemIndex := 1;
  end;
end;

procedure TfrmImportSurferGrdFile.SetData;
const
  CommentStr = 'Minimum X %0g' + sLineBreak
    + 'Maximum X %1g' + sLineBreak
    + 'Minimum Y %2g' + sLineBreak
    + 'Maximum Y %3g' + sLineBreak
    + 'Minimum Z %4g' + sLineBreak
    + 'Maximum Z %5g' + sLineBreak;
var
  DataSetName: string;
  DataSet: TDataArray;
  ScreenObjectList: TList;
  Undo: TUndoImportGrdFile;
  Root: string;
  ExistingObjectCount: integer;
  AScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  PointIndex: Integer;
  Item: TValueArrayItem;
  Formula: string;
  NewDataSets: TList;
  Position: integer;
  Count: Integer;
begin
  frmGoPhast.PhastModel.BeginScreenObjectUpdate;
  frmGoPhast.CanDraw := False;
  try
    NewDataSets := TList.Create;
    try
      MakeNewDataSet(NewDataSets, '_Grd_Z', 'Imported from Surfer Grid files');
      DataSetName := comboDataSets.Text;
      DataSet := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(DataSetName);
      case FFileType of
        sft6, sftAscii:
          begin
            DataSet.Comment := Format(CommentStr,
              [FGrd6.Header.Xlo, FGrd6.Header.Xhi,
              FGrd6.Header.Ylo, FGrd6.Header.Yhi,
              FGrd6.Header.Zlo, FGrd6.Header.Zhi]);
          end;
        sft7:
          begin
            DataSet.Comment := Format(CommentStr,
              [FGrd7.Header.xLL,
              FGrd7.Header.xLL + FGrd7.Header.xSize* FGrd7.Header.nCol,
              FGrd7.Header.yLL,
              FGrd7.Header.yLL + FGrd7.Header.ySize* FGrd7.Header.nRow,
              FGrd7.Header.zMin, FGrd7.Header.zMax]);
          end
        else Assert(False);
      end;
      Assert(DataSet <> nil);
      ScreenObjectList := TList.Create;
      //MultipleParts := false;
      try
        Undo := TUndoImportGrdFile.Create;
        try
          Root := TScreenObject.ValidName(
            ExtractFileRoot(OpenDialogFile.FileName));
          ScreenObjectList.Capacity := 1;
          ExistingObjectCount :=
            frmGoPhast.PhastModel.NumberOfLargestScreenObjectsStartingWith(Root);

          AScreenObject :=
            TScreenObject.CreateWithViewDirection(
            frmGoPhast.PhastModel, vdTop,
            UndoCreateScreenObject, False);
          if ExistingObjectCount > 0 then
          begin
            AScreenObject.Name := Root + '_'+ IntToStr(ExistingObjectCount);
          end
          else
          begin
            AScreenObject.Name := Root;
          end;
          AScreenObject.SetValuesOfIntersectedCells
            := cbIntersectedCells.Checked;
          AScreenObject.SetValuesByInterpolation
            := cbInterpolation.Checked;
          AScreenObject.ElevationCount := ecZero;
          AScreenObject.EvaluatedAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
          AScreenObject.Visible := False;

          Position := -1;
          case FFileType of
            sft6, sftAscii:
              begin
                AScreenObject.Capacity := FGrd6.Header.nx * FGrd6.Header.ny;
                for PointIndex := 0 to Length(FGrd6.Points) - 1 do
                begin
                  AScreenObject.AddPoint(ConvertPoint(FGrd6.Points[PointIndex]), True);
                end;
                ScreenObjectList.Add(AScreenObject);
                Position := AScreenObject.AddDataSet(DataSet);
                Assert(Position >= 0);

                Item := AScreenObject.
                  ImportedValues.Add as TValueArrayItem;
                Item.Name := DataSet.Name;
                Item.Values.DataType := DataSet.DataType;
                Item.Values.Count := FGrd6.Header.nx * FGrd6.Header.ny;

                for PointIndex := 0 to Length(FGrd6.Points) - 1 do
                begin
                  Item.Values.RealValues[PointIndex] := FGrd6.Points[PointIndex].Z;
                end;
              end;
            sft7:
              begin
                Count := 0;
                for PointIndex := 0 to Length(FGrd7.Points) - 1 do
                begin
                  if FGrd7.Points[PointIndex].Z < FGrd7.Header.BlankValue then
                  begin
                    Inc(Count);
                  end;
                end;

                AScreenObject.Capacity := Count;
                for PointIndex := 0 to Length(FGrd7.Points) - 1 do
                begin
                  if FGrd7.Points[PointIndex].Z < FGrd7.Header.BlankValue then
                  begin
                    AScreenObject.AddPoint(ConvertPoint(FGrd7.Points[PointIndex]), True);
                  end;
                end;
                ScreenObjectList.Add(AScreenObject);
                Position := AScreenObject.AddDataSet(DataSet);
                Assert(Position >= 0);

                Item := AScreenObject.
                  ImportedValues.Add as TValueArrayItem;
                Item.Name := DataSet.Name;
                Item.Values.DataType := DataSet.DataType;
                Item.Values.Count := Count;

                for PointIndex := 0 to Length(FGrd7.Points) - 1 do
                begin
                  if FGrd7.Points[PointIndex].Z < FGrd7.Header.BlankValue then
                  begin
                    Item.Values.RealValues[PointIndex] := FGrd7.Points[PointIndex].Z;
                  end;
                end;
              end;
            else Assert(False);
          end;

          Formula := rsObjectImportedValuesR + '("' + DataSet.Name + '")';
          AScreenObject.DataSetFormulas[Position] := Formula;

          Undo.StoreNewScreenObjects(ScreenObjectList);
          Undo.StoreNewDataSets(NewDataSets);
          frmGoPhast.UndoStack.Submit(Undo);
          frmGoPhast.PhastModel.AddFileToArchive(OpenDialogFile.FileName);
        except
          Undo.Free;
          raise;
        end;
      finally
        ScreenObjectList.Free;
      end;
    finally
      NewDataSets.Free
    end;
  finally
    frmGoPhast.CanDraw := True;
    frmGoPhast.PhastModel.EndScreenObjectUpdate;
  end;

end;

{ TUndoImportGrdFile }

function TUndoImportGrdFile.Description: string;
begin
  result := 'import Surfer grid file';
end;

end.
