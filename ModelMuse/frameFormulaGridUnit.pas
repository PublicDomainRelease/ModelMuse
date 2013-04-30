unit frameFormulaGridUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameGridUnit, Grids, RbwDataGrid4,
  StdCtrls, Mask, JvExMask, JvSpin, Buttons, ExtCtrls;

type
  TValidCellEvent = procedure (Sender: TObject; ACol, ARow: Integer; var ValidCell: Boolean) of object;

  TframeFormulaGrid = class(TframeGrid)
    pnlTop: TPanel;
    edFormula: TLabeledEdit;
    procedure edFormulaChange(Sender: TObject);
    procedure GridColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure GridHorizontalScroll(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FFirstFormulaColumn: Integer;
    FOnValidCell: TValidCellEvent;
    { Private declarations }
  public
    procedure LayoutMultiRowEditControls; virtual;
    property FirstFormulaColumn: Integer read FFirstFormulaColumn
      write FFirstFormulaColumn;
    property OnValidCell: TValidCellEvent read FOnValidCell write FOnValidCell;
    procedure ClearGrid;
    { Public declarations }
  end;

var
  frameFormulaGrid: TframeFormulaGrid;

implementation

uses
  frmCustomGoPhastUnit, Math;

{$R *.dfm}

procedure TframeFormulaGrid.ClearGrid;
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  Grid.BeginUpdate;
  try
    for RowIndex := Grid.FixedRows to Grid.RowCount - 1 do
    begin
      for ColIndex := Grid.FixedCols to Grid.ColCount - 1 do
      begin
        Grid.Cells[ColIndex,RowIndex] := '';
        Grid.Checked[ColIndex,RowIndex] := False;
      end;
    end;
  finally
    Grid.EndUpdate;
  end;
end;

procedure TframeFormulaGrid.edFormulaChange(Sender: TObject);
var
  ColIndex: Integer;
  RowIndex: Integer;
  TempOptions: TGridOptions;
  ValidCell: Boolean;
begin
  Grid.BeginUpdate;
  try
    for RowIndex := Grid.FixedRows to
      Grid.RowCount - 1 do
    begin
      for ColIndex := FirstFormulaColumn to Grid.ColCount - 1 do
      begin
        if Grid.IsSelectedCell(ColIndex, RowIndex) then
        begin
          ValidCell := True;
          if Assigned (OnValidCell) then
          begin
            OnValidCell(Grid, ColIndex, RowIndex, ValidCell);
          end;
          if ValidCell then
          begin
            Grid.Cells[ColIndex, RowIndex] := edFormula.Text;
            if Assigned(Grid.OnSetEditText) then
            begin
              Grid.OnSetEditText(
                Grid,ColIndex,RowIndex, edFormula.Text);
            end;
          end;
        end;
      end;
    end;
  finally
    Grid.EndUpdate
  end;
  TempOptions := Grid.Options;
  try
    Grid.Options := [goEditing, goAlwaysShowEditor];
    Grid.UpdateEditor;
  finally
    Grid.Options := TempOptions;
  end;
end;

procedure TframeFormulaGrid.FrameResize(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TframeFormulaGrid.GridColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TframeFormulaGrid.GridHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControls;
end;

procedure TframeFormulaGrid.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ShouldEnable: boolean;
  ColIndex, RowIndex: Integer;
begin
  inherited;
  ShouldEnable := False;
  for RowIndex := Grid.FixedRows to Grid.RowCount -1 do
  begin
    for ColIndex := FirstFormulaColumn to Grid.ColCount - 1 do
    begin
      ShouldEnable := Grid.IsSelectedCell(ColIndex,RowIndex);
      if ShouldEnable then
      begin
        if Assigned(OnValidCell) then
        begin
          OnValidCell(self, ColIndex, RowIndex, ShouldEnable);
          if ShouldEnable then
          begin
            Break;
          end;
        end
        else
        begin
          break;
        end;
      end;
    end;
    if ShouldEnable then
    begin
      break;
    end;
  end;
  edFormula.Enabled := ShouldEnable;
end;

procedure TframeFormulaGrid.LayoutMultiRowEditControls;
var
  Column: integer;
  Row: Integer;
  ColIndex: Integer;
  ValidCell: Boolean;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  Column := Max(FirstFormulaColumn,Grid.LeftCol);
  if Assigned(OnValidCell) then
  begin
    Row := 1;
    for ColIndex := Column to Grid.ColCount - 1 do
    begin
      ValidCell := True;
      OnValidCell(self, ColIndex,Row,ValidCell);
      if ValidCell then
      begin
        Column := ColIndex;
        break;
      end;
    end;
  end;
  LayoutControls(Grid, edFormula, nil,
    Column);
end;

end.
