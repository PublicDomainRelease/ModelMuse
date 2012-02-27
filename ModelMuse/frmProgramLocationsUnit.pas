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
    htlblZoneBudger: TJvHTLabel;
    fedZonebudget: TJvFilenameEdit;
    lblZoneBudget: TLabel;
    lblModelMate: TLabel;
    fedModelMate: TJvFilenameEdit;
    Label1: TLabel;
    JvHTLabel1: TJvHTLabel;
    fedModflowLgr: TJvFilenameEdit;
    htlblModelMate: TJvHTLabel;
    lblModflowNWT: TLabel;
    htlblModflowNWT: TJvHTLabel;
    fedModflowNWT: TJvFilenameEdit;
    lblMt3dms: TLabel;
    htlblMt3dms: TJvHTLabel;
    fedMt3dms: TJvFilenameEdit;
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

uses frmGoPhastUnit, GoPhastTypes;

resourcestring
  StrThisFileDoesNotE = 'This file does not exist';
  StrTheNameOfThisPro = 'The name of this program should not be "ModelMuse."';
  StrChangeProgramLocat = 'change program locations';

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
  fedZonebudget.FileName := Locations.ZoneBudgetLocation;
  fedModelMate.FileName := Locations.ModelMateLocation;
  fedModflowLgr.FileName := Locations.ModflowLgrLocation;
  fedModflowNwt.FileName := Locations.ModflowNwtLocation;
  fedMt3dms.FileName := Locations.Mt3dmsLocation;
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
    Locations.ZoneBudgetLocation := fedZonebudget.FileName;
    Locations.ModelMateLocation := fedModelMate.FileName;
    Locations.ModflowLgrLocation := fedModflowLgr.FileName;
    Locations.ModflowNwtLocation := fedModflowNwt.FileName;
    Locations.Mt3dmsLocation := fedMt3dms.FileName;
    Undo := TUndoChangeProgramLocations.Create(Locations);
    frmGoPhast.UndoStack.Submit(Undo);
  finally
    Locations.Free
  end;
end;

procedure TfrmProgramLocations.HighlightControls;
var
  ModflowOK: Boolean;
  ModflowLgrOK: Boolean;
  ModpathOK: Boolean;
  ZoneBudgetOK: Boolean;
  FileEditorOK: Boolean;
  ModflowNwtOK: Boolean;
  function CheckControl(Edit: TJvFilenameEdit): boolean;
  begin
    result := ExtractFileName(Edit.FileName) <> 'ModelMuse.exe';
    if result then
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
      if not result then
      begin
        Edit.Hint := StrThisFileDoesNotE;
        Edit.ShowHint := True;
      end;
    end
    else
    begin
      Edit.Hint := StrTheNameOfThisPro;
      Edit.ShowHint := True;
    end;
    if result then
    begin
      Edit.Color := clWindow;
      Edit.ShowHint := False;
    end
    else
    begin
      Edit.Color := clRed;
    end;
  end;
begin
  CheckControl(fedModelMonitor);
  CheckControl(fedModelMate);
  ModflowOK := CheckControl(fedModflow)
    or (frmGoPhast.PhastModel.ModelSelection  <> msModflow);
  ModflowLgrOK := CheckControl(fedModflowLgr)
    or (frmGoPhast.PhastModel.ModelSelection  <> msModflowLGR);
  ModflowNwtOK := CheckControl(fedModflowNWT)
    or (frmGoPhast.PhastModel.ModelSelection  <> msModflowNWT);
  ModpathOK := CheckControl(fedModpath)
    or not frmGoPhast.PhastModel.ModPathIsSelected;
  ZoneBudgetOK := CheckControl(fedZonebudget)
    or not frmGoPhast.PhastModel.ZoneBudgetIsSelected;
  CheckControl(fedMt3dms);
  FileEditorOK := CheckControl(fedTextEditor);
  btnOK.Enabled := ModflowOK and ModflowLgrOK and ModflowNwtOK and ModpathOK
    and ZoneBudgetOK and FileEditorOK;
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
  result := StrChangeProgramLocat;
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
