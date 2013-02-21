unit frmCustomizeMeshUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, Buttons,
  StdCtrls, DisplaySettingsUnit, UndoItems;

type
  {$IFDEF SUTRA}
  TUndoSutraMeshDisplay = class(TCustomUndo)
  private
    FOldSutraSettings: TSutraSettings;
    FNewSutraSettings: TSutraSettings;
    procedure ForceRedraw;
  protected
    function Description: string; override;
  public
    constructor Create(var NewSettings: TSutraSettings);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;
  {$ENDIF}

  TfrmCustomizeMesh = class(TfrmCustomGoPhast)
    cbShowNodeNumbers: TCheckBox;
    cbShowElementNumbers: TCheckBox;
    btnEditNodeFont: TButton;
    btnEditElementFont: TButton;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    dlgFont: TFontDialog;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
    procedure btnEditNodeFontClick(Sender: TObject);
    procedure btnEditElementFontClick(Sender: TObject);
  private
  {$IFDEF SUTRA}
    FSutraSettings: TSutraSettings;
  {$ENDIF}
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCustomizeMesh: TfrmCustomizeMesh;

implementation

uses
  frmGoPhastUnit, SutraMeshUnit;

{$R *.dfm}

procedure TfrmCustomizeMesh.btnEditElementFontClick(Sender: TObject);
begin
  inherited;
  {$IFDEF SUTRA}
  dlgFont.Font := FSutraSettings.ElementFont;
  if dlgFont.Execute then
  begin
    FSutraSettings.ElementFont := dlgFont.Font;
  end;
  {$ENDIF}
end;

procedure TfrmCustomizeMesh.btnEditNodeFontClick(Sender: TObject);
begin
  inherited;
  {$IFDEF SUTRA}
  dlgFont.Font := FSutraSettings.NodeFont;
  if dlgFont.Execute then
  begin
    FSutraSettings.NodeFont := dlgFont.Font;
  end;
  {$ENDIF}
end;

procedure TfrmCustomizeMesh.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmCustomizeMesh.FormCreate(Sender: TObject);
begin
  inherited;
  {$IFDEF SUTRA}
  FSutraSettings := TSutraSettings.Create(nil);
  GetData;
  {$ENDIF}
end;

procedure TfrmCustomizeMesh.FormDestroy(Sender: TObject);
begin
  inherited;
  {$IFDEF SUTRA}
  FSutraSettings.Free;
  {$ENDIF}
end;

procedure TfrmCustomizeMesh.GetData;
  {$IFDEF SUTRA}
var
  Mesh: TSutraMesh3D;
  {$ENDIF}
begin
  {$IFDEF SUTRA}
  Mesh := frmGoPhast.PhastModel.SutraMesh;
  cbShowNodeNumbers.Checked := Mesh.DrawNodeNumbers;
  cbShowElementNumbers.Checked := Mesh.DrawElementNumbers;
  FSutraSettings.Assign(Mesh);
  {$ENDIF}
end;

procedure TfrmCustomizeMesh.SetData;
  {$IFDEF SUTRA}
var
  Undo: TUndoSutraMeshDisplay;
  {$ENDIF}
begin
  {$IFDEF SUTRA}
  FSutraSettings.ShowNodeNumbers := cbShowNodeNumbers.Checked;
  FSutraSettings.ShowElementNumbers := cbShowElementNumbers.Checked;
  Undo := TUndoSutraMeshDisplay.Create(FSutraSettings);
  frmGoPhast.UndoStack.Submit(Undo);
  {$ENDIF}
end;

{ TUndoSutraMeshDisplay }

  {$IFDEF SUTRA}

constructor TUndoSutraMeshDisplay.Create(var NewSettings: TSutraSettings);
begin
  FOldSutraSettings := TSutraSettings.Create(nil);
  FOldSutraSettings.Assign(frmGoPhast.PhastModel.SutraMesh);
  FNewSutraSettings := NewSettings;
  NewSettings := nil;
end;

function TUndoSutraMeshDisplay.Description: string;
begin
  result := 'customize SUTRA mesh';
end;

destructor TUndoSutraMeshDisplay.Destroy;
begin
  FOldSutraSettings.Free;
  FNewSutraSettings.Free;
  inherited;
end;

procedure TUndoSutraMeshDisplay.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.SutraMesh.Assign(FNewSutraSettings);
  ForceRedraw;
end;

procedure TUndoSutraMeshDisplay.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.SutraMesh.Assign(FOldSutraSettings);
  ForceRedraw;
end;

procedure TUndoSutraMeshDisplay.ForceRedraw;
begin
  frmGoPhast.frameTopView.ModelChanged := True;
  frmGoPhast.frameFrontView.ModelChanged := True;
  frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
  frmGoPhast.frameFrontView.ZoomBox.InvalidateImage32;
end;
  {$ENDIF}


end.
