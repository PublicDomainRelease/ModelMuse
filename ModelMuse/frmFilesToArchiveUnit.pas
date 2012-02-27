unit frmFilesToArchiveUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ExtCtrls, JvExStdCtrls,
  JvRichEdit, UndoItems, StrUtils, JvExControls, JvLinkLabel;

type
  TfrmFilesToArchive = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    reFilesToSave: TJvRichEdit;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    btnArchive: TButton;
    sdArchive: TSaveDialog;
    btnAddFiles: TButton;
    odAddFiles: TOpenDialog;
    JvLinkLabel1: TJvLinkLabel;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure reFilesToSaveChange(Sender: TObject);
    procedure btnArchiveClick(Sender: TObject);
    procedure btnAddFilesClick(Sender: TObject);
    procedure JvLinkLabel1LinkClick(Sender: TObject; LinkNumber: Integer;
      LinkText, LinkParam: string);
  private
    FFilesToArchive: TStrings;
    procedure GetData;
    procedure SetData;
    procedure ColorText;
    { Private declarations }
  public
    { Public declarations }
  end;

  TUndoFilesToArchive = class(TCustomUndo)
  private
    FFilesToAdd: TStrings;
    FFilesToDelete: TStrings;
    FOriginalFilesToArchive: TStrings;
    FNewFilesToArchive: TStrings;
    FOriginalFileName: string;
    FNewFileName: string;
    FOriginalModelFiles: TStrings;
    FNewModelFiles: TStrings;
    FOriginalAllFiles: TStrings;
  protected
    function Description: string; override;
  public
    constructor Create(UpdatedFiles: TStrings);
    destructor Destroy; override;
    // @name does the command for the first time.
    procedure DoCommand; override;
    // @name undoes the command.
    procedure Undo; override;
    function Changed: boolean;
  end;

implementation

uses frmGoPhastUnit, JvLinkLabelTools;

resourcestring
  StrChangedFilesToArc = 'changed files to archive';

{$R *.dfm}

procedure TfrmFilesToArchive.btnAddFilesClick(Sender: TObject);
var
  Index: integer;
begin
  inherited;
  if odAddFiles.Execute then
  begin
    for Index := 0 to odAddFiles.Files.Count - 1 do
    begin
      if reFilesToSave.Lines.IndexOf(odAddFiles.Files[Index]) < 0 then
      begin
        reFilesToSave.Lines.Add(odAddFiles.Files[Index]);
      end;
    end;
  end;
end;

procedure TfrmFilesToArchive.btnArchiveClick(Sender: TObject);
begin
  inherited;
  sdArchive.FileName := frmGoPhast.PhastModel.ArchiveName;
  if sdArchive.Execute then
  begin
    frmGoPhast.PhastModel.CreateArchive(sdArchive.FileName);
  end;
end;

procedure TfrmFilesToArchive.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmFilesToArchive.FormCreate(Sender: TObject);
var
  TempOnChange : TNotifyEvent;
begin
  inherited;
  reFilesToSave.DoubleBuffered := False;
  FFilesToArchive := TStringList.Create;
  TempOnChange := reFilesToSave.OnChange;
  reFilesToSave.OnChange := nil;
  try
    GetData;
    ColorText;
  finally
    reFilesToSave.OnChange := TempOnChange;
  end;
  btnArchive.Enabled := FFilesToArchive.Count > 0;
end;

procedure TfrmFilesToArchive.FormDestroy(Sender: TObject);
begin
  inherited;
  FFilesToArchive.Free;
end;

procedure TfrmFilesToArchive.GetData;
begin
  FFilesToArchive.Clear;
  if frmGoPhast.PhastModel.ModelFileName <> '' then
  begin
    FFilesToArchive.Add(frmGoPhast.PhastModel.ModelFileName);
  end;
  FFilesToArchive.AddStrings(frmGoPhast.PhastModel.FilesToArchive);
  FFilesToArchive.AddStrings(frmGoPhast.PhastModel.ModelInputFiles);
  reFilesToSave.Lines := FFilesToArchive;
end;

procedure TfrmFilesToArchive.JvLinkLabel1LinkClick(Sender: TObject;
  LinkNumber: Integer; LinkText, LinkParam: string);
begin
  inherited;
  TWebTools.OpenWebPage('http://water.usgs.gov/admin/memo/GW/gw00.02.html');
end;

procedure TfrmFilesToArchive.ColorText;
const
  EndLine = #$D;
var
  SelText: string;
  StartPosition: Integer;
  EndPosition: Integer;
  OriginalStart, OriginalLength: integer;
  FileName: string;
begin
  OriginalStart := reFilesToSave.SelStart;
  OriginalLength := reFilesToSave.SelLength;
  try
    reFilesToSave.SetSelection(0,MAXINT,False);
    SelText := reFilesToSave.SelText;
    reFilesToSave.SelAttributes.BackColor := clWindow;
    StartPosition := 1;
    EndPosition := Pos(EndLine, SelText);
    while EndPosition >= 1 do
    begin
      reFilesToSave.SetSelection(StartPosition-1,EndPosition-1,False);
      FileName := Trim(reFilesToSave.SelText);
      if not FileExists(FileName) then
      begin
        reFilesToSave.SelAttributes.BackColor := clRed;
      end;
      StartPosition := EndPosition+1;
      EndPosition := PosEx(EndLine, SelText, StartPosition);
    end;
    
    reFilesToSave.SetSelection(StartPosition-1,MAXINT,False);
    FileName := Trim(reFilesToSave.SelText);
    if not FileExists(FileName) then
    begin
      reFilesToSave.SelAttributes.BackColor := clRed;
    end;
  finally
    reFilesToSave.SelStart := OriginalStart;
    reFilesToSave.SelLength := OriginalLength;
  end;
end;

procedure TfrmFilesToArchive.reFilesToSaveChange(Sender: TObject);
begin
  inherited;
  ColorText;
end;

procedure TfrmFilesToArchive.SetData;
var
  UpdatedFiles: TStringList;
  Undo: TUndoFilesToArchive;
begin
  UpdatedFiles := TStringList.Create;
  try
    reFilesToSave.WordWrap := False;
    UpdatedFiles.Assign(reFilesToSave.Lines);
    Undo := TUndoFilesToArchive.Create(UpdatedFiles);
    try
      if Undo.Changed then
      begin
        frmGoPhast.UndoStack.Submit(Undo)
      end
      else
      begin
        Undo.Free;
      end;
    except
      Undo.Free;
    end;
  finally
    UpdatedFiles.Free;
  end;
end;

{ TUndoFilesToArchive }

function TUndoFilesToArchive.Changed: boolean;
begin
  result := (FFilesToAdd.Count > 0) or (FFilesToDelete.Count > 0);
end;

constructor TUndoFilesToArchive.Create(UpdatedFiles: TStrings);
var
  Index: Integer;
  ModelDeleteIndex: Integer;
  AdditionalFileIndex: Integer;
begin
  inherited Create;
  FFilesToAdd := TStringList.Create;
  FFilesToDelete := TStringList.Create;
  FOriginalFilesToArchive := TStringList.Create;
  FNewFilesToArchive := TStringList.Create;
  FOriginalModelFiles := TStringList.Create;
  FNewModelFiles := TStringList.Create;
  FOriginalAllFiles := TStringList.Create;

  FOriginalFileName := frmGoPhast.PhastModel.ModelFileName;
  FNewFileName := frmGoPhast.PhastModel.ModelFileName;

  FOriginalFilesToArchive.Assign(frmGoPhast.PhastModel.FilesToArchive);
  FNewFilesToArchive.Assign(frmGoPhast.PhastModel.FilesToArchive);
  FOriginalModelFiles.Assign(frmGoPhast.PhastModel.ModelInputFiles);
  FNewModelFiles.Assign(frmGoPhast.PhastModel.ModelInputFiles);

  if frmGoPhast.PhastModel.ModelFileName <> '' then
  begin
    FOriginalAllFiles.Add(frmGoPhast.PhastModel.ModelFileName);
  end;
  FOriginalAllFiles.AddStrings(frmGoPhast.PhastModel.FilesToArchive);
  FOriginalAllFiles.AddStrings(frmGoPhast.PhastModel.ModelInputFiles);
  for Index := 0 to UpdatedFiles.Count - 1 do
  begin
    if FOriginalAllFiles.IndexOf(UpdatedFiles[Index]) < 0 then
    begin
      FFilesToAdd.Add(UpdatedFiles[Index]);
    end;
  end;
  for Index := 0 to FOriginalAllFiles.Count - 1 do
  begin
    if UpdatedFiles.IndexOf(FOriginalAllFiles[Index]) < 0 then
    begin
      FFilesToDelete.Add(FOriginalAllFiles[Index]);
    end;
  end;
  for Index := 0 to FFilesToDelete.Count - 1 do
  begin
    ModelDeleteIndex := FNewModelFiles.IndexOf(
      FFilesToDelete[Index]);
    AdditionalFileIndex := FNewFilesToArchive.IndexOf(
      FFilesToDelete[Index]);
    if FNewFileName = FFilesToDelete[Index] then
    begin
      FNewFileName := '';
    end
    else if ModelDeleteIndex >= 0 then
    begin
      FNewModelFiles.Delete(ModelDeleteIndex);
    end
    else if AdditionalFileIndex >= 0 then
    begin
      FNewFilesToArchive.Delete(AdditionalFileIndex);
    end;
  end;
  for Index := 0 to FFilesToAdd.Count - 1 do
  begin
    FNewFilesToArchive.Add(FFilesToAdd[Index]);
  end;
end;

function TUndoFilesToArchive.Description: string;
begin
  result := StrChangedFilesToArc;
end;

destructor TUndoFilesToArchive.Destroy;
begin
  FOriginalFilesToArchive.Free;
  FNewFilesToArchive.Free;
  FOriginalModelFiles.Free;
  FNewModelFiles.Free;
  FFilesToAdd.Free;
  FFilesToDelete.Free;
  FOriginalAllFiles.Free;
  inherited;
end;

procedure TUndoFilesToArchive.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.FilesToArchive := FNewFilesToArchive;
  frmGoPhast.PhastModel.ModelInputFiles := FNewModelFiles;
  frmGoPhast.PhastModel.ModelFileName := FNewFileName;
  frmGoPhast.PhastModel.Invalidate;
end;

procedure TUndoFilesToArchive.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.FilesToArchive := FOriginalFilesToArchive;
  frmGoPhast.PhastModel.ModelInputFiles := FOriginalFilesToArchive;
  frmGoPhast.PhastModel.ModelFileName := FOriginalFileName;
  frmGoPhast.PhastModel.Invalidate;
end;

end.
