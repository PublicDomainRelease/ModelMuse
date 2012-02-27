unit frameGridUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, Buttons, ExtCtrls, Grids, RbwDataGrid4, StdCtrls, Mask, JvExMask,
  JvSpin;

type
  TframeGrid = class(TFrame)
    Panel: TPanel;
    Grid: TRbwDataGrid4;
    seNumber: TJvSpinEdit;
    lbNumber: TLabel;
    sbAdd: TSpeedButton;
    sbInsert: TSpeedButton;
    sbDelete: TSpeedButton;
    procedure GridEndUpdate(Sender: TObject);
    procedure seNumberChange(Sender: TObject);
    procedure sbAddClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
    procedure sbInsertClick(Sender: TObject);
  private
    procedure ClearSelectedRow;
    { Private declarations }
  protected
    procedure SetEnabled(Value: boolean); override;
  public
    { Public declarations }
  end;

implementation

uses
  Math;

{$R *.dfm}

procedure TframeGrid.GridEndUpdate(Sender: TObject);
begin
  seNumber.AsInteger := Grid.RowCount -1;
end;

procedure TframeGrid.sbAddClick(Sender: TObject);
begin
  seNumber.AsInteger := seNumber.AsInteger +1;
end;

procedure TframeGrid.sbDeleteClick(Sender: TObject);
begin
  if Grid.SelectedRow >= Grid.FixedRows  then
  begin
    if Grid.RowCount > Grid.FixedRows + 1 then
    begin
      Grid.DeleteRow(Grid.SelectedRow);
      GridEndUpdate(nil);
    end
    else
    begin
      ClearSelectedRow;
      seNumber.AsInteger := seNumber.AsInteger -1;
    end;
  end;
end;

procedure TframeGrid.sbInsertClick(Sender: TObject);
begin
  if Grid.SelectedRow >= Grid.FixedRows  then
  begin
    Grid.InsertRow(Grid.SelectedRow);
    ClearSelectedRow;
    GridEndUpdate(nil);
  end;
end;

procedure TframeGrid.SetEnabled(Value: boolean);
begin
  inherited;
  sbAdd.Enabled := Value;
  sbInsert.Enabled := Value;
  Grid.Enabled := Value;
  seNumber.Enabled := Value;
  sbDelete.Enabled := Value and (seNumber.AsInteger >= 1);
end;

procedure TframeGrid.ClearSelectedRow;
var
  ColIndex: Integer;
begin
  for ColIndex := 0 to Grid.ColCount - 1 do
  begin
    Grid.Cells[ColIndex, Grid.SelectedRow] := '';
    Grid.Checked[ColIndex, Grid.SelectedRow] := False;
  end;
end;

procedure TframeGrid.seNumberChange(Sender: TObject);
begin
  Grid.RowCount := Max(2, seNumber.AsInteger+1);
  sbDelete.Enabled := seNumber.AsInteger > 0;
end;

end.
