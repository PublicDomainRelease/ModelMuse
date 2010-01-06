unit frmPhastLocationUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Mask, JvExMask, JvToolEdit,
  JvExStdCtrls, JvHtControls, Buttons, ExtCtrls;

type
  TfrmPhastLocation = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Panel1: TPanel;
    lblPhast: TLabel;
    htlblPhast: TJvHTLabel;
    fedPhast: TJvFilenameEdit;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure fedPhastChange(Sender: TObject);
  private
    Procedure GetData;
    procedure SetData;
    procedure HighlightControls;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPhastLocation: TfrmPhastLocation;

implementation

uses
  PhastModelUnit, frmGoPhastUnit, frmProgramLocationsUnit;

{$R *.dfm}

{ TfrmPhastLocation }

procedure TfrmPhastLocation.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmPhastLocation.fedPhastChange(Sender: TObject);
begin
  inherited;
  HighlightControls;
end;

procedure TfrmPhastLocation.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
  HighlightControls;
end;

procedure TfrmPhastLocation.GetData;
var
  Locations: TProgramLocations;
begin
  frmGoPhast.ReadIniFile;
  Locations := frmGoPhast.PhastModel.ProgramLocations;
  fedPhast.FileName := Locations.PhastLocation;
end;

procedure TfrmPhastLocation.HighlightControls;
var
  FilesExist: Boolean;
  function CheckControl(Edit: TJvFilenameEdit): boolean;
  begin
//    if Edit = fedTextEditor then
//    begin
//      result := FileExists(Edit.FileName) or (Edit.FileName = '')
//        or (LowerCase(Edit.FileName) = 'notepad.exe');
//    end
//    else
    begin
      result := FileExists(Edit.FileName);
    end;
    if result then
    begin
      Edit.Color := clWindow;
    end
    else
    begin
      Edit.Color := clRed;
    end;
  end;
begin
//  CheckControl(fedModelMonitor);
  FilesExist := CheckControl(fedPhast);
//    and (CheckControl(fedModpath)
//    or not frmGoPhast.PhastModel.ModflowPackages.ModPath.IsSelected)
//    and CheckControl(fedTextEditor);
  btnOK.Enabled := FilesExist;
end;

procedure TfrmPhastLocation.SetData;
var
  Locations: TProgramLocations;
  Undo: TUndoChangeProgramLocations;
begin
  Locations := TProgramLocations.Create;
  try
    Locations.PhastLocation := fedPhast.FileName;
    Undo := TUndoChangeProgramLocations.Create(Locations);
    frmGoPhast.UndoStack.Submit(Undo);
  finally
    Locations.Free
  end;
end;

end.
