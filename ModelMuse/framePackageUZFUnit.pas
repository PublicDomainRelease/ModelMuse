unit framePackageUZFUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageLayerChoiceUnit, RbwController, StdCtrls, ExtCtrls,
  ArgusDataEntry, framePackageTransientLayerChoiceUnit,
  ModflowPackageSelectionUnit, CheckLst;

type
  TCheckListRows = (clrRouteDischarge, clrET, clrPrint, clrSpecifyResid,
    clrSpecifyInitial, clrSufaceLeakage);

  TframePackageUZF = class(TframePackageLayerChoice)
    lblVerticalKSource: TLabel;
    comboVerticalKSource: TComboBox;
    rdeNumberOfTrailingWaves: TRbwDataEntry;
    lblNumberOfTrailingWaves: TLabel;
    lblNumberOfWaveSets: TLabel;
    rdeNumberOfWaveSets: TRbwDataEntry;
    lblSURFDEP: TLabel;
    rdeSURFDEP: TRbwDataEntry;
    chklstOptions: TCheckListBox;
    rgAssignmentMethod: TRadioGroup;
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

  chklstOptions.Checked[Ord(clrRouteDischarge)] := Uzf.RouteDischargeToStreams;
  chklstOptions.Checked[Ord(clrET)] := Uzf.SimulateET;
  chklstOptions.Checked[Ord(clrPrint)] := Uzf.PrintSummary = 1;
  chklstOptions.Checked[Ord(clrSpecifyResid)] := Uzf.SpecifyResidualWaterContent;
  chklstOptions.Checked[Ord(clrSpecifyInitial)] := Uzf.SpecifyInitialWaterContent;
  chklstOptions.Checked[Ord(clrSufaceLeakage)] := Uzf.CalulateSurfaceLeakage;

//  cbRouteDischargeToStreamsAndLakes.Checked := Uzf.RouteDischargeToStreams;
//  cbSimulateEvapotranspiration.Checked := Uzf.SimulateET;
  rdeNumberOfTrailingWaves.Text := IntToStr(Uzf.NumberOfTrailingWaves);
  rdeNumberOfWaveSets.Text := IntToStr(Uzf.NumberOfWaveSets);
//  cbPrintSummary.Checked := Uzf.PrintSummary = 1;
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

  Uzf.RouteDischargeToStreams := chklstOptions.Checked[Ord(clrRouteDischarge)];
  Uzf.SimulateET := chklstOptions.Checked[Ord(clrET)];
  Uzf.PrintSummary := Ord(chklstOptions.Checked[Ord(clrPrint)]);
  Uzf.SpecifyResidualWaterContent := chklstOptions.Checked[Ord(clrSpecifyResid)];
  Uzf.SpecifyInitialWaterContent := chklstOptions.Checked[Ord(clrSpecifyInitial)];
  Uzf.CalulateSurfaceLeakage := chklstOptions.Checked[Ord(clrSufaceLeakage)];


//  Uzf.RouteDischargeToStreams := cbRouteDischargeToStreamsAndLakes.Checked;
//  Uzf.SimulateET := cbSimulateEvapotranspiration.Checked;
  if TryStrToInt(rdeNumberOfTrailingWaves.Text, Value) then
  begin
    Uzf.NumberOfTrailingWaves := Value;
  end;
  if TryStrToInt(rdeNumberOfWaveSets.Text, Value) then
  begin
    Uzf.NumberOfWaveSets := Value;
  end;
//  if cbPrintSummary.Checked then
//  begin
//    Uzf.PrintSummary := 1;
//  end
//  else
//  begin
//    Uzf.PrintSummary := 0;
//  end;
  if TryStrToFloat(rdeSURFDEP.Text, RValue) then
  begin
    Uzf.DepthOfUndulations := RValue;
  end;
  Uzf.AssignmentMethod := TUpdateMethod(rgAssignmentMethod.ItemIndex);
end;

end.
