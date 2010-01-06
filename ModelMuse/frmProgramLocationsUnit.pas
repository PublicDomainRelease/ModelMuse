unit frmProgramLocationsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ExtCtrls, Mask, JvExMask,
  JvToolEdit, JvExControls, JvLinkLabel, JvExStdCtrls, JvHtControls, UndoItems,
  PhastModelUnit;

type
  TUndoChangeProgramLocations = class(TCustomUndo)
  private
    FOldLocations: TProgramLocations;
    FNewLocations: TProgramLocations;
  protected
    function Description: string; override;
  public
    Constructor Create(var NewLocations: TProgramLocations);
    Destructor Destroy; override;
    procedure DoCommand;  override;
    procedure Undo; override;
  end;

  TfrmProgramLocations = class(TfrmCustomGoPhast)
    fedModflow: TJvFilenameEdit;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Panel1: TPanel;
    lblModflow: TLabel;
    htlblModflow: TJvHTLabel;
    fedTextEditor: TJvFilenameEdit;
    lblTextEditor: TLabel;
    htlblModPath: TJvHTLabel;
    lblModpath: TLabel;
    fedModpath: TJvFilenameEdit;
    lblModelMonitor: TLabel;
    fedModelMonitor: TJvFilenameEdit;
    procedure fedModflowChange(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
  private
    Procedure GetData;
    procedure SetData;
    procedure HighlightControls;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses frmGoPhastUnit;

{$R *.dfm}

{ TfrmProgramLocations }

procedure TfrmProgramLocations.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmProgramLocations.fedModflowChange(Sender: TObject);
begin
  inherited;
  HighlightControls;
end;

procedure TfrmProgramLocations.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
  HighlightControls;
end;

procedure TfrmProgramLocations.GetData;
var
  Locations: TProgramLocations;
begin
  frmGoPhast.ReadIniFile;
  Locations := frmGoPhast.PhastModel.ProgramLocations;
  fedModflow.FileName := Locations.ModflowLocation;
  fedTextEditor.FileName := Locations.TextEditorLocation;
  fedModpath.FileName := Locations.ModPathLocation;
  fedModelMonitor.FileName := Locations.ModelMonitorLocation;
end;

procedure TfrmProgramLocations.SetData;
var
  Locations: TProgramLocations;
  Undo: TUndoChangeProgramLocations;
begin
  Locations := TProgramLocations.Create;
  try
    Locations.ModflowLocation := fedModflow.FileName;
    Locations.TextEditorLocation := fedTextEditor.FileName;
    Locations.ModPathLocation := fedModpath.FileName;
    Locations.ModelMonitorLocation := fedModelMonitor.FileName;
    Undo := TUndoChangeProgramLocations.Create(Locations);
    frmGoPhast.UndoStack.Submit(Undo);
  finally
    Locations.Free
  end;
end;

procedure TfrmProgramLocations.HighlightControls;
var
  FilesExist: Boolean;
  function CheckControl(Edit: TJvFilenameEdit): boolean;
  begin
    if Edit = fedTextEditor then
    begin
      result := FileExists(Edit.FileName) or (Edit.FileName = '')
        or (LowerCase(Edit.FileName) = 'notepad.exe');
    end
    else
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
  CheckControl(fedModelMonitor);
  FilesExist := CheckControl(fedModflow)
    and (CheckControl(fedModpath)
    or not frmGoPhast.PhastModel.ModflowPackages.ModPath.IsSelected)
    and CheckControl(fedTextEditor);
  btnOK.Enabled := FilesExist;
end;

{ TUndoChangeProgramLocations }

constructor TUndoChangeProgramLocations.Create(
  var NewLocations: TProgramLocations);
begin
  Assert(NewLocations <> nil);
  FOldLocations := TProgramLocations.Create;
  FOldLocations.Assign(frmGoPhast.PhastModel.ProgramLocations);
  FNewLocations := NewLocations;
  NewLocations := nil;
end;

function TUndoChangeProgramLocations.Description: string;
begin
  result := 'change program locations';
end;

destructor TUndoChangeProgramLocations.Destroy;
begin
  FNewLocations.Free;
  FOldLocations.Free;
  inherited;
end;

procedure TUndoChangeProgramLocations.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.ProgramLocations.Assign(FNewLocations);
  frmGoPhast.WriteIniFile;
end;

procedure TUndoChangeProgramLocations.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.ProgramLocations.Assign(FOldLocations);
  frmGoPhast.WriteIniFile;
end;

end.
