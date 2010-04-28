unit frmGridValueUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ScreenObjectUnit;

type
  TfrmGridValue = class(TfrmCustomGoPhast)
    lblLayer: TLabel;
    lblRow: TLabel;
    lblColumn: TLabel;
    lblCellValue: TLabel;
    lblExplanation: TLabel;
    memoExplanation: TMemo;
    edCellValue: TEdit;
    btnHelp: TBitBtn;
    btnClose: TBitBtn;
    lblDataSet: TLabel;
    lblLayerHeight: TLabel;
    lblRowWidth: TLabel;
    lblColumnWidth: TLabel;
    lblSelectedObject: TLabel;
    cbShowThirdDValues: TCheckBox;
    lblHigher3rdDimensionCoordinate: TLabel;
    lblLower3rdDimensionCoordinate: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject); override;
    procedure edCellValueKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure memoExplanationKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FSelectedScreenObject: TScreenObject;
    procedure UpdatedSelectedObject;
    procedure UpdateScreenObjectInfo(const Column, Row, Layer: Integer);
    { Private declarations }
  public
    procedure UpdateValue(const Layer, Row, Column: integer;
      const DataSetName, CellValue: string; Explanation: string); 
    { Public declarations }
  end;

var
  frmGridValue: TfrmGridValue;

implementation

uses Clipbrd, CustomModflowWriterUnit, AbstractGridUnit, frmGoPhastUnit, 
  DataSetUnit, GoPhastTypes, GIS_Functions;

resourcestring
  StrSelectedObject = 'Selected object';
  StrHigher3rdDimension = 'Higher 3rd dimension coordinate';
  StrLower3rdDimension = 'Lower 3rd dimension coordinate';
  StrNotAssigned = ': (not assigned)';
  StrHigher = 'Higher ';
  StrLower = 'Lower ';

{$R *.dfm}

{ TfrmGridValue }

procedure TfrmGridValue.edCellValueKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if ((Key = Ord('C')) or (Key = Ord('c'))) and (ssCtrl in Shift) then
  begin
    edCellValue.CopyToClipboard;
  end;
end;

procedure TfrmGridValue.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  frmGridValue := nil;
  Action := caFree;
end;

procedure TfrmGridValue.FormCreate(Sender: TObject);
begin
  inherited;
  AdjustFormPosition(dpLeft);
end;

procedure TfrmGridValue.memoExplanationKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if ((Key = Ord('C')) or (Key = Ord('c'))) and (ssCtrl in Shift) then
  begin
    memoExplanation.CopyToClipboard;
  end;
end;

procedure TfrmGridValue.UpdateValue(const Layer, Row, Column: integer;
  const DataSetName, CellValue: string; Explanation: string);
var
  Grid: TCustomGrid;
  DataArray: TDataArray;
  EvaluatedAt: TEvaluatedAt;
  ColumnWidth: Double;
  RowWidth: Double;
  LayerHeight: Double;
begin
  lblLayer.Caption := 'Layer: ' + IntToStr(Layer+1);
  lblRow.Caption := 'Row: ' + IntToStr(Row+1);
  lblColumn.Caption := 'Column: ' + IntToStr(Column+1);
  lblDataSet.Caption := DataSetName;
  edCellValue.Text := CellValue;
  if (Explanation <> StrNoValueAssigned) and (Pos(StrNoValueAssigned, Explanation) > 0) then
  begin
    Explanation := StringReplace(Explanation, StrNoValueAssigned, '', []);
  end;
  memoExplanation.Text := Explanation;

  Grid := frmGoPhast.Grid;
  if (Grid <> nil) and (Grid.LayerCount >= 1)
    and (Grid.RowCount >= 1) and (Grid.ColumnCount >= 1) then
  begin
    if DataSetName <> '' then
    begin
      DataArray := frmGoPhast.PhastModel.GetDataSetByName(DataSetName)
    end
    else
    begin
      DataArray := nil;
    end;
    if DataArray = nil then
    begin
      EvaluatedAt := eaBlocks;
    end
    else
    begin
      EvaluatedAt := DataArray.EvaluatedAt;
    end;
    GlobalEvaluatedAt := EvaluatedAt;
    ColumnWidth := GetColumnWidth(Column);
    RowWidth := GetRowWidth(Row);
    LayerHeight := GetLayerHeight(Column, Row, Layer);

    lblLayerHeight.Caption := 'Layer height: ' + FloatToStr(LayerHeight);
    lblRowWidth.Caption := 'Row width:'  + FloatToStr(RowWidth);
    lblColumnWidth.Caption := 'Column width: ' + FloatToStr(ColumnWidth);
  end
  else
  begin
    lblLayerHeight.Caption := 'Layer height:';
    lblRowWidth.Caption := 'Row width:';
    lblColumnWidth.Caption := 'Column width:';
  end;
  
  UpdateScreenObjectInfo(Column, Row, Layer);
end;

procedure TfrmGridValue.UpdateScreenObjectInfo
  (const Column, Row, Layer: Integer);
var
  Value: Double;
  DirectionText: string;
begin
  if (frmGoPhast.PhastModel.SelectedScreenObjectCount = 1) then
  begin
    if (FSelectedScreenObject = nil)
      or not FSelectedScreenObject.Selected then
    begin
      UpdatedSelectedObject;
    end;
    lblSelectedObject.Caption := StrSelectedObject
      + ': ' + FSelectedScreenObject.Name;
    case FSelectedScreenObject.ViewDirection of
      vdTop:
        begin
          DirectionText := 'Z-coordinate';
        end;
      vdFront:
        begin
          DirectionText := 'Y-coordinate';
        end;
      vdSide:
        begin
          DirectionText := 'X-coordinate';
        end;
    else
      Assert(False);
    end;
    case FSelectedScreenObject.ElevationCount of
      ecZero:
        begin
          cbShowThirdDValues.Enabled := False;
          lblHigher3rdDimensionCoordinate.Caption := '';
          lblLower3rdDimensionCoordinate.Caption := '';
        end;
      ecOne:
        begin
          cbShowThirdDValues.Enabled := True;
          lblLower3rdDimensionCoordinate.Caption := '';
          if cbShowThirdDValues.Checked then
          begin
            if FSelectedScreenObject.
              IsHigher3DElevationAssigned(Column, Row, Layer) then
            begin
              Value := FSelectedScreenObject.
                Higher3DElevations[Layer, Row, Column];
              lblHigher3rdDimensionCoordinate.Caption :=
                DirectionText + ': ' + FloatToStr(Value);
            end
            else
            begin
              lblHigher3rdDimensionCoordinate.Caption :=
                DirectionText + StrNotAssigned;
            end;
          end
          else
          begin
            lblHigher3rdDimensionCoordinate.Caption := DirectionText;
          end;
        end;
      ecTwo:
        begin
          cbShowThirdDValues.Enabled := True;
          if cbShowThirdDValues.Checked then
          begin
            if FSelectedScreenObject.
              IsHigher3DElevationAssigned(Column, Row, Layer) then
            begin
              Value := FSelectedScreenObject.
                Higher3DElevations[Layer, Row, Column];
              lblHigher3rdDimensionCoordinate.Caption :=
                StrHigher + DirectionText + ': ' + FloatToStr(Value);
            end
            else
            begin
              lblHigher3rdDimensionCoordinate.Caption :=
                StrHigher + DirectionText + StrNotAssigned;
            end;
            if FSelectedScreenObject.
              IsLower3DElevationAssigned(Column, Row, Layer) then
            begin
              Value := FSelectedScreenObject.
                Lower3DElevations[Layer, Row, Column];
              lblLower3rdDimensionCoordinate.Caption :=
                StrLower + DirectionText + ': ' + FloatToStr(Value);
            end
            else
            begin
              lblLower3rdDimensionCoordinate.Caption :=
                StrLower + DirectionText + StrNotAssigned;
            end;
          end
          else
          begin
            lblHigher3rdDimensionCoordinate.Caption :=
              StrHigher + DirectionText;
            lblLower3rdDimensionCoordinate.Caption :=
              StrLower + DirectionText;
          end;
        end;
    else
      Assert(False);
    end;
  end
  else if frmGoPhast.PhastModel.SelectedScreenObjectCount > 1 then
  begin
    lblSelectedObject.Caption := StrSelectedObject + ': (multiple objects)';
    lblHigher3rdDimensionCoordinate.Caption := StrHigher3rdDimension;
    lblLower3rdDimensionCoordinate.Caption := StrLower3rdDimension;
  end
  else
  begin
    lblSelectedObject.Caption := StrSelectedObject + ': (none)';
    lblHigher3rdDimensionCoordinate.Caption := StrHigher3rdDimension;
    lblLower3rdDimensionCoordinate.Caption := StrLower3rdDimension;
  end;
end;

procedure TfrmGridValue.UpdatedSelectedObject;
var
  ScreenObject: TScreenObject;
  Index: Integer;
begin
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if ScreenObject.Selected then
    begin
      FSelectedScreenObject := ScreenObject;
      break;
    end;
  end;
end;

end.
