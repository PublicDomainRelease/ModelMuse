unit framePackageUZFUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageLayerChoiceUnit, RbwController, StdCtrls, ExtCtrls,
  ArgusDataEntry, framePackageTransientLayerChoiceUnit,
  ModflowPackageSelectionUnit, RbwRadioGroup;

type
  TframePackageUZF = class(TframePackageLayerChoice)
    lblVerticalKSource: TLabel;
    comboVerticalKSource: TComboBox;
    cbRouteDischargeToStreamsAndLakes: TCheckBox;
    cbSimulateEvapotranspiration: TCheckBox;
    rdeNumberOfTrailingWaves: TRbwDataEntry;
    lblNumberOfTrailingWaves: TLabel;
    lblNumberOfWaveSets: TLabel;
    rdeNumberOfWaveSets: TRbwDataEntry;
    cbPrintSummary: TCheckBox;
    lblSURFDEP: TLabel;
    rdeSURFDEP: TRbwDataEntry;
    rgAssignmentMethod: TRbwRadioGroup;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageUZF: TframePackageUZF;

implementation

uses
  GoPhastTypes;

{$R *.dfm}

{ TframePackageUZF }

procedure TframePackageUZF.GetData(Package: TModflowPackageSelection);
var
  Uzf: TUzfPackageSelection;
begin
  inherited;
  Uzf := Package as TUzfPackageSelection;
  comboVerticalKSource.ItemIndex := Uzf.VerticalKSource -1;
  cbRouteDischargeToStreamsAndLakes.Checked := Uzf.RouteDischargeToStreams;
  cbSimulateEvapotranspiration.Checked := Uzf.SimulateET;
  rdeNumberOfTrailingWaves.Text := IntToStr(Uzf.NumberOfTrailingWaves);
  rdeNumberOfWaveSets.Text := IntToStr(Uzf.NumberOfWaveSets);
  cbPrintSummary.Checked := Uzf.PrintSummary = 1;
  rdeSURFDEP.Text := FloatToStr(Uzf.DepthOfUndulations);
  rgAssignmentMethod.ItemIndex := Ord(Uzf.AssignmentMethod);
end;

procedure TframePackageUZF.SetData(Package: TModflowPackageSelection);
var
  Uzf: TUzfPackageSelection;
  Value: integer;
  RValue: double;
begin
  inherited;
  Uzf := Package as TUzfPackageSelection;
  Uzf.VerticalKSource := comboVerticalKSource.ItemIndex + 1;
  Uzf.RouteDischargeToStreams := cbRouteDischargeToStreamsAndLakes.Checked;
  Uzf.SimulateET := cbSimulateEvapotranspiration.Checked;
  if TryStrToInt(rdeNumberOfTrailingWaves.Text, Value) then
  begin
    Uzf.NumberOfTrailingWaves := Value;
  end;
  if TryStrToInt(rdeNumberOfWaveSets.Text, Value) then
  begin
    Uzf.NumberOfWaveSets := Value;
  end;
  if cbPrintSummary.Checked then
  begin
    Uzf.PrintSummary := 1;
  end
  else
  begin
    Uzf.PrintSummary := 0;
  end;
  if TryStrToFloat(rdeSURFDEP.Text, RValue) then
  begin
    Uzf.DepthOfUndulations := RValue;
  end;
  Uzf.AssignmentMethod := TUpdateMethod(rgAssignmentMethod.ItemIndex);
end;

end.
