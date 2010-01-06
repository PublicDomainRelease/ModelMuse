unit frmGridValueUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons;

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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject); override;
    procedure edCellValueKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure memoExplanationKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
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
    RowWidth := GetColumnWidth(Row);
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
end;

end.
