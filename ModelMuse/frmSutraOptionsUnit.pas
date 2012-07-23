unit frmSutraOptionsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ExtCtrls, UndoItems,
  SutraOptionsUnit, SutraMeshUnit;

type
  TfrmSutraOptions = class(TfrmCustomGoPhast)
    rgMeshType: TRadioGroup;
    rgTransport: TRadioGroup;
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
  private
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

  TUndoChangeSutraOptions = class(TCustomUndo)
  private
    FNewSutraOptions: TSutraOptions;
    FOldSutraOptions: TSutraOptions;
    FNewMeshType: TMeshType;
    FOldMeshType: TMeshType;
  protected
    function Description: string; override;
  public
    constructor Create(var NewSutraOptions: TSutraOptions;
      NewMeshType: TMeshType);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

var
  frmSutraOptions: TfrmSutraOptions;

implementation

uses
  frmGoPhastUnit;

{$R *.dfm}

{ TfrmSutraOptions }

procedure TfrmSutraOptions.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData
end;

procedure TfrmSutraOptions.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmSutraOptions.GetData;
var
  SutraOptions: TSutraOptions;
begin
  SutraOptions := frmGoPhast.PhastModel.SutraOptions;
  if frmGoPhast.PhastModel.SutraMesh = nil then
  begin
    rgMeshType.Enabled := False;
  end
  else
  begin
    rgMeshType.ItemIndex := Ord(frmGoPhast.PhastModel.SutraMesh.MeshType);
  end;
  rgTransport.ItemIndex := Ord(SutraOptions.TransportChoice);
end;

procedure TfrmSutraOptions.SetData;
var
  SutraOptions: TSutraOptions;
  UndoItem: TUndoChangeSutraOptions;
  MeshType: TMeshType;
begin
  SutraOptions := TSutraOptions.Create(nil);
  try
    MeshType := TMeshType(rgMeshType.ItemIndex);
    SutraOptions.TransportChoice := TTransportChoice(rgTransport.ItemIndex);
    UndoItem := TUndoChangeSutraOptions.Create(SutraOptions, MeshType);
    frmGoPhast.UndoStack.Submit(UndoItem);
  finally
    SutraOptions.Free;
  end;
end;

{ TUndoChangeSutraOptions }

constructor TUndoChangeSutraOptions.Create(var NewSutraOptions: TSutraOptions;
  NewMeshType: TMeshType);
begin
  FNewSutraOptions := NewSutraOptions;
  NewSutraOptions := nil;
  FOldSutraOptions := TSutraOptions.Create(nil);
  FOldSutraOptions.Assign(frmGoPhast.PhastModel.SutraOptions);
  FNewMeshType := NewMeshType;
  if frmGoPhast.PhastModel.SutraMesh <> nil then
  begin
    FOldMeshType := frmGoPhast.PhastModel.SutraMesh.MeshType;
  end
  else
  begin
    FOldMeshType := mt2D;
  end;
end;

function TUndoChangeSutraOptions.Description: string;
begin
  result := 'change SUTRA options';
end;

destructor TUndoChangeSutraOptions.Destroy;
begin
  FOldSutraOptions.Free;
  FNewSutraOptions.Free;
  inherited;
end;

procedure TUndoChangeSutraOptions.DoCommand;
begin
  inherited;
  if frmGoPhast.PhastModel.SutraMesh <> nil then
  begin
    frmGoPhast.PhastModel.SutraMesh.MeshType := FNewMeshType;
  end;
  frmGoPhast.PhastModel.SutraOptions := FNewSutraOptions;
  frmGoPhast.PhastModel.SutraLayerStructure.Loaded;
end;

procedure TUndoChangeSutraOptions.Undo;
begin
  inherited;
  if frmGoPhast.PhastModel.SutraMesh <> nil then
  begin
    frmGoPhast.PhastModel.SutraMesh.MeshType := FOldMeshType;
  end;
  frmGoPhast.PhastModel.SutraOptions := FOldSutraOptions;
  frmGoPhast.PhastModel.SutraLayerStructure.Loaded;
end;

end.
