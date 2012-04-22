{@name is used for specifying a table that relates stream depth and width
to stream flow in the MODFLOW SFR package.}
unit frameFlowTableUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, Grids, RbwDataGrid4, StdCtrls, Mask, JvExMask, JvSpin, Buttons;

type
  // @name identifies the columns in @link(TframeFlowTable.dgSfrTable
  // TframeFlowTable.dgSfrTable).
  TSfrTableColumns = (stcFlow, stcDepth, stcWidth);

  {@name is used for specifying a table that relates stream depth and width
  to stream flow in the MODFLOW SFR package.

  @member(seTableCount @name controls the number of rows in @link(dgSfrTable).
    See @link(seTableCountChange).)

  @member(lblNumberOfPoints @name is a label for seTableCount.)

  @member(dgSfrTable Each row of @name contains a data point specifying
  how stream depth and width depend of flow.)

  @member(btnInsertFlowTableRow @name is used for inserting a new row into @link(dgSfrTable).
  See @link(btnInsertFlowTableRowClick).)

  @member(btnDeleteFlowTableRow @name is used for deleting a row from @link(dgSfrTable).
  See @link(btnDeleteFlowTableRowClick).)

  @member(seTableCountChange @name changes the number of rows
  in @link(dgSfrTable).)

  @member(btnInsertFlowTableRowClick @name inserts a row
  in @link(dgSfrTable).)

  @member(btnDeleteFlowTableRowClick @name deletes a row
  from @link(dgSfrTable).)

  @member(dgSfrTableSetEditText @name adjusts @link(seTableCount) to
  reflect the number of rows in @link(dgSfrTable))
  }
  TframeFlowTable = class(TFrame)
    seTableCount: TJvSpinEdit;
    lblNumberOfPoints: TLabel;
    dgSfrTable: TRbwDataGrid4;
    btnInsertFlowTableRow: TBitBtn;
    btnDeleteFlowTableRow: TBitBtn;
    procedure seTableCountChange(Sender: TObject);
    procedure btnInsertFlowTableRowClick(Sender: TObject);
    procedure btnDeleteFlowTableRowClick(Sender: TObject);
    procedure dgSfrTableSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    FTableCountChanged: boolean;
    { Private declarations }
  public
    property TableCountChanged: boolean read FTableCountChanged
      write FTableCountChanged;
    procedure EnableDelete;
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
  end;

implementation

{$R *.dfm}

resourcestring
  StrFlow = 'Flow';
  StrDepth = 'Depth';
  StrWidth = 'Width';

{ TFrame23 }

procedure TframeFlowTable.btnDeleteFlowTableRowClick(Sender: TObject);
begin
  if dgSfrTable.SelectedRow > 0 then
  begin
    dgSfrTable.DeleteRow(dgSfrTable.SelectedRow);
    seTableCount.AsInteger := seTableCount.AsInteger -1;
    seTableCount.OnChange(nil);
  end;
end;

procedure TframeFlowTable.btnInsertFlowTableRowClick(Sender: TObject);
begin
  if dgSfrTable.SelectedRow > 0 then
  begin
    dgSfrTable.InsertRow(dgSfrTable.SelectedRow);
    seTableCount.AsInteger := seTableCount.AsInteger +1;
    seTableCount.OnChange(nil);
  end;
end;

constructor TframeFlowTable.Create(AOwner: TComponent);
begin
  inherited;
  dgSfrTable.Cells[Ord(stcFlow),0] := StrFlow;
  dgSfrTable.Cells[Ord(stcDepth),0] := StrDepth;
  dgSfrTable.Cells[Ord(stcWidth),0] := StrWidth;
end;

procedure TframeFlowTable.EnableDelete;
begin
  btnDeleteFlowTableRow.Enabled := seTableCount.Enabled
    and (seTableCount.AsInteger > 2);
end;

procedure TframeFlowTable.dgSfrTableSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  if seTableCount.AsInteger < dgSfrTable.RowCount - 1 then
  begin
    seTableCount.AsInteger := dgSfrTable.RowCount - 1;
    seTableCount.OnChange(seTableCount);
  end;
end;

procedure TframeFlowTable.seTableCountChange(Sender: TObject);
begin
  dgSfrTable.RowCount := seTableCount.AsInteger + 1;
  EnableDelete;
  dgSfrTable.OnSetEditText(Sender, 0, dgSfrTable.RowCount -1,
    dgSfrTable.Cells[0,dgSfrTable.RowCount -1]);
  TableCountChanged := True;
end;

end.
