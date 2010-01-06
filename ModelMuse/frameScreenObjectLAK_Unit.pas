unit frameScreenObjectLAK_Unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frameScreenObjectNoParamUnit, Grids, RbwDataGrid4, StdCtrls,
  ArgusDataEntry, Buttons, Mask, JvExMask, JvSpin, ExtCtrls;

type
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
    procedure rdeCenterLakeChange(Sender: TObject);
    procedure cbGagStandardClick(Sender: TObject);
    procedure cbGagFluxAndCondClick(Sender: TObject);
    procedure cbGagDeltaClick(Sender: TObject);
    procedure cbGage4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

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

end.
