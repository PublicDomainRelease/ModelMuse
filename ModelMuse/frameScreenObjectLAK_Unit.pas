unit frameScreenObjectLAK_Unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frameScreenObjectNoParamUnit, Grids, RbwDataGrid4, StdCtrls,
  ArgusDataEntry, Buttons, Mask, JvExMask, JvSpin, ExtCtrls, ComCtrls,
  JvToolEdit;

type
  TBathColumn = (bcStage, bcVolume, bcSurfaceArea);

  TframeScreenObjectLAK = class(TframeScreenObjectNoParam)
    lblInitialStage: TLabel;
    rdeInitialStage: TRbwDataEntry;
    rdeCenterLake: TRbwDataEntry;
    lblCenterLake: TLabel;
    rdeSill: TRbwDataEntry;
    lblSill: TLabel;
    rdeLakeID: TRbwDataEntry;
    lblLakeID: TLabel;
    gbGage: TGroupBox;
    cbGagStandard: TCheckBox;
    cbGagFluxAndCond: TCheckBox;
    cbGagDelta: TCheckBox;
    cbGage4: TCheckBox;
    pcLake: TPageControl;
    tabLakeProperties: TTabSheet;
    tabBathymetry: TTabSheet;
    rdgLakeTable: TRbwDataGrid4;
    pnlBathChoice: TPanel;
    rgBathChoice: TRadioGroup;
    feLakeBathymetry: TJvFilenameEdit;
    procedure rdeCenterLakeChange(Sender: TObject);
    procedure cbGagStandardClick(Sender: TObject);
    procedure cbGagFluxAndCondClick(Sender: TObject);
    procedure cbGagDeltaClick(Sender: TObject);
    procedure cbGage4Click(Sender: TObject);
    procedure rgBathChoiceClick(Sender: TObject);
  protected
    procedure Loaded; override;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

resourcestring
  StrStage = 'Stage';
  StrVolume = 'Volume';
  StrSurfaceArea = 'Surface Area';

procedure TframeScreenObjectLAK.cbGagDeltaClick(Sender: TObject);
begin
  inherited;
  cbGagDelta.AllowGrayed := cbGagDelta.State = cbGrayed;
end;

procedure TframeScreenObjectLAK.cbGage4Click(Sender: TObject);
begin
  inherited;
  cbGage4.AllowGrayed := cbGage4.State = cbGrayed;
end;

procedure TframeScreenObjectLAK.cbGagFluxAndCondClick(Sender: TObject);
begin
  inherited;
  cbGagFluxAndCond.AllowGrayed := cbGagFluxAndCond.State = cbGrayed;
end;

procedure TframeScreenObjectLAK.cbGagStandardClick(Sender: TObject);
begin
  inherited;
  cbGagStandard.AllowGrayed := cbGagStandard.State = cbGrayed;
  cbGagFluxAndCond.Enabled := cbGagStandard.State <> cbUnchecked;
  cbGagDelta.Enabled := cbGagStandard.State <> cbUnchecked;
end;

procedure TframeScreenObjectLAK.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    pnlBottom.Parent := tabLakeProperties;
    pnlGrid.Parent := tabLakeProperties;
    pnlGrid.Align := alClient;
    pcLake.ActivePageIndex := 0;
    rdgLakeTable.Cells[Ord(bcStage), 0] := StrStage;
    rdgLakeTable.Cells[Ord(bcVolume), 0] := StrVolume;
    rdgLakeTable.Cells[Ord(bcSurfaceArea), 0] := StrSurfaceArea;
  end;

end;

procedure TframeScreenObjectLAK.rdeCenterLakeChange(Sender: TObject);
var
  CenterLakeID: integer;
begin
  inherited;
  if csLoading in ComponentState then
  begin
    Exit;
  end;
  if TryStrToInt(rdeCenterLake.Text, CenterLakeID) then
  begin
    rdeSill.Enabled := CenterLakeID >= 1;
  end
  else
  begin
    rdeSill.Enabled := False;
  end;
end;

procedure TframeScreenObjectLAK.rgBathChoiceClick(Sender: TObject);
begin
  inherited;
  feLakeBathymetry.Enabled := rgBathChoice.ItemIndex = 1;
  rdgLakeTable.Enabled := rgBathChoice.ItemIndex = 0;
  if rdgLakeTable.Enabled then
  begin
    rdgLakeTable.Color := clWindow;
  end
  else
  begin
    rdgLakeTable.Color := clBtnFace;
  end;
end;

end.
