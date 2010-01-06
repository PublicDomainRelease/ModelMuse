{@abstract(The main purpose of @name is to declare @link(TfrmFormulaErrors)
  which displays warning messages to the user when a formula has
  been found to be in error.)}
unit frmFormulaErrorsUnit;

interface

uses
  SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, frmCustomGoPhastUnit, Grids, ExtCtrls, Buttons, RbwDataGrid4;

type
  {@abstract(@name displays warning messages to the user when a formula has
    been found to be in error.)}
  TfrmFormulaErrors = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // @name closes the dialog box.
    btnClose: TBitBtn;
    // @name: TButton;
    // @name copies the error message to the clipboard.
    btnCopy: TButton;
    // @name: TBitBtn;
    // @name displays help for the @classname.
    // See @link(btnCopyClick).
    btnHelp: TBitBtn;
    // @name: TLabel;
    // @name displays static text.
    Label1: TLabel;
    // @name: TPanel;
    // @name is the panel at the bottom of the
    // dialog box that holds the buttons.
    pnlBottom: TPanel;
    // @name: TPanel;
    // @name is the panel at the top of the
    // dialog box that holds the caption.
    pnlTop: TPanel;
    // @name: TTimer;
    // @name is a timer used to prevent the dialog box to be shown for 0.1
    // seconds after an error message has been added.
    Timer: TTimer;
    sgErrors: TRbwDataGrid4;
    btnClear: TButton;
    // @name copies the error messages to the clipboard.
    procedure btnCopyClick(Sender: TObject);
    // @name initializes the dialog box.
    procedure FormCreate(Sender: TObject); override;
    // @name resizes the columns.
    procedure FormResize(Sender: TObject);
    // @name reinitializes the dialog box.
    procedure FormShow(Sender: TObject);
    // @name shows the dialog box.
    procedure TimerTimer(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    // @name is the number of errors that have been added.
    FErrorCount: integer;
    { Private declarations }
  public
    // @name adds an error message to @classname.
    procedure AddError(const ObjectName, DataSetName, Formula, ErrorMessage:
      string);
    { Public declarations }
  end;

// @name returns an instance of @link(TfrmFormulaErrors).
// It will create one if needed.
function frmFormulaErrors: TfrmFormulaErrors;
procedure ClearFormulaErrors;

var
  ClearingDeletedDataSets: boolean = False;

implementation

uses Clipbrd;

{$R *.dfm}

var
  FfrmFormulaErrors: TfrmFormulaErrors;

function frmFormulaErrors: TfrmFormulaErrors;
begin
  if FfrmFormulaErrors = nil then
  begin
    FfrmFormulaErrors := TfrmFormulaErrors.Create(nil);
//    Application.CreateForm(TfrmFormulaErrors, FfrmFormulaErrors);
  end;
  result := FfrmFormulaErrors;
end;

procedure ClearFormulaErrors;
begin
  FreeAndNil(FfrmFormulaErrors);
end;

procedure TfrmFormulaErrors.AddError(const ObjectName, DataSetName, Formula,
  ErrorMessage: string);
begin
  Inc(FErrorCount);
  sgErrors.RowCount := FErrorCount + 1;
  sgErrors.Cells[0, FErrorCount] := ObjectName;
  sgErrors.Cells[1, FErrorCount] := DataSetName;
  sgErrors.Cells[2, FErrorCount] := Formula;
  sgErrors.Cells[3, FErrorCount] := ErrorMessage;
  Beep;
  Timer.Enabled := True;
end;

procedure TfrmFormulaErrors.FormCreate(Sender: TObject);
begin
  inherited;
  sgErrors.Cells[0, 0] := 'Objects';
  sgErrors.Cells[1, 0] := 'Data Sets';
  sgErrors.Cells[2, 0] := 'Formulas';
  sgErrors.Cells[3, 0] := 'Error Messages';
  sgErrors.ColWidths[3] := 130;
  FormResize(nil);
  FErrorCount := 0;
end;

procedure TfrmFormulaErrors.btnClearClick(Sender: TObject);
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  inherited;
  for RowIndex := 1 to sgErrors.RowCount - 1 do
  begin
    for ColIndex := 0 to sgErrors.ColCount - 1 do
    begin
      sgErrors.Cells[ColIndex, RowIndex] := '';
    end;
    sgErrors.RowHeights[RowIndex] := sgErrors.DefaultRowHeight
  end;
  FErrorCount := 0;
  sgErrors.RowCount := 2;
end;

procedure TfrmFormulaErrors.btnCopyClick(Sender: TObject);
var
  Lines: TStringList;
  RowIndex: integer;
  ColIndex: integer;
  ALine: string;
begin
  inherited;
  Lines := TStringList.Create;
  try
    for RowIndex := 1 to FErrorCount do
    begin
      ALine := '';
      for ColIndex := 0 to sgErrors.ColCount - 1 do
      begin
        if ColIndex <> 0 then
        begin
          ALine := ALine + #9;
        end;
        ALine := ALine + sgErrors.Cells[ColIndex, RowIndex];
      end;
      Lines.Add(ALine)
    end;
    Clipboard.AsText := Lines.Text;
  finally
    Lines.Free;
  end;
end;

procedure TfrmFormulaErrors.FormShow(Sender: TObject);
begin
  inherited;
  SetAppearance;
end;

procedure TfrmFormulaErrors.FormResize(Sender: TObject);
var
  Index: integer;
  TotalColWidth: integer;
  Delta: integer;
begin
  inherited;
  TotalColWidth := 0;
  for Index := 0 to sgErrors.ColCount - 1 do
  begin
    TotalColWidth := TotalColWidth + sgErrors.ColWidths[Index];
  end;
  Delta := sgErrors.Width - 30 - TotalColWidth;
  if Delta > 0 then
  begin
    sgErrors.ColWidths[3] := sgErrors.ColWidths[3] + Delta;
  end;
end;

procedure TfrmFormulaErrors.TimerTimer(Sender: TObject);
begin
  inherited;
  Show;
  Timer.Enabled := False;
end;

initialization

finalization
  FfrmFormulaErrors.Free;

end.

