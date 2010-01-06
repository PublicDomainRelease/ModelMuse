unit frmModflowNameFileUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, UndoItems;

type
  TfrmModflowNameFile = class(TfrmCustomGoPhast)
    memoLines: TMemo;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    lblLines: TLabel;
    cbFlowPackage: TCheckBox;
    cbSolvers: TCheckBox;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
  private
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

  TUndoNameFileLines = class(TCustomUndo)
  private
    FNewLines: TStrings;
    FOldLines: TStrings;
  protected
    function Description: string; override;
  public
    procedure DoCommand; override;
    // @name undoes the command.
    procedure Undo; override;
    Constructor Create(NewLines: TStrings);
    Destructor Destroy; override;
  end;

implementation

uses
  frmGoPhastUnit;

{$R *.dfm}

{ TfrmModflowNameFile }

procedure TfrmModflowNameFile.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmModflowNameFile.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmModflowNameFile.GetData;
begin
  memoLines.Lines := frmGoPhast.PhastModel.ModflowNameFileLines;
  cbFlowPackage.Checked := frmGoPhast.PhastModel.AlternateFlowPackage;
  cbSolvers.Checked := frmGoPhast.PhastModel.AlternateSolver;
end;

procedure TfrmModflowNameFile.SetData;
begin
  frmGoPhast.UndoStack.Submit(TUndoNameFileLines.Create(memoLines.Lines));
  frmGoPhast.PhastModel.AlternateFlowPackage := cbFlowPackage.Checked;
  frmGoPhast.PhastModel.AlternateSolver := cbSolvers.Checked;
end;

{ TUndoNameFileLines }

constructor TUndoNameFileLines.Create(NewLines: TStrings);
begin
  FNewLines := TStringList.Create;
  FOldLines := TStringList.Create;
  FNewLines.Assign(NewLines);
  FOldLines.Assign(frmGoPhast.PhastModel.ModflowNameFileLines);
end;

function TUndoNameFileLines.Description: string;
begin
  result := 'change MODFLOW name file';
end;

destructor TUndoNameFileLines.Destroy;
begin
  FNewLines.Free;
  FOldLines.Free;
  inherited;
end;

procedure TUndoNameFileLines.DoCommand;
begin
  frmGoPhast.PhastModel.ModflowNameFileLines := FNewLines;
end;

procedure TUndoNameFileLines.Undo;
begin
  frmGoPhast.PhastModel.ModflowNameFileLines := FOldLines;
end;

end.
