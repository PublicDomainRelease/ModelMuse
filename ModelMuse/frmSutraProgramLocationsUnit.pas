unit frmSutraProgramLocationsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, StdCtrls,
  Mask, JvExMask, JvToolEdit, JvExStdCtrls, JvHtControls, Buttons,
  ExtCtrls;

type
  TfrmSutraProgramLocations = class(TfrmCustomGoPhast)
    htlblSutra22: TJvHTLabel;
    fedSutra22: TJvFilenameEdit;
    lblSutra22: TLabel;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
  private
    procedure SetData;
    procedure GetData;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSutraProgramLocations: TfrmSutraProgramLocations;

implementation

uses
  frmProgramLocationsUnit, PhastModelUnit, frmGoPhastUnit;

{$R *.dfm}

procedure TfrmSutraProgramLocations.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmSutraProgramLocations.GetData;
begin
  fedSutra22.FileName := frmGoPhast.PhastModel.ProgramLocations.Sutra22Location;
end;

procedure TfrmSutraProgramLocations.SetData;
var
  NewLocations: TProgramLocations;
  Undo: TUndoChangeProgramLocations;
begin
  inherited;
  NewLocations := TProgramLocations.Create;
  NewLocations.Assign(frmGoPhast.PhastModel.ProgramLocations);
  NewLocations.Sutra22Location := fedSutra22.FileName;
  Undo := TUndoChangeProgramLocations.Create(NewLocations);
  frmGoPhast.UndoStack.Submit(Undo);
end;

procedure TfrmSutraProgramLocations.btnOKClick(Sender: TObject);
begin
  SetData;
end;

end.
