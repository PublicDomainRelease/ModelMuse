unit framePackageWellUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, framePackageUnit, RbwController,
  StdCtrls, ArgusDataEntry, ModflowPackageSelectionUnit;

type
  TframePackageWell = class(TframePackage)
    rdePhiRamp: TRbwDataEntry;
    lblPhiRamp: TLabel;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageWell: TframePackageWell;

implementation

{$R *.dfm}

{ TframePackageWell }

procedure TframePackageWell.GetData(Package: TModflowPackageSelection);
begin
  rdePhiRamp.Text := FloatToStr((Package as TWellPackage).PhiRamp);
  inherited;
end;

procedure TframePackageWell.SetData(Package: TModflowPackageSelection);
begin
  (Package as TWellPackage).PhiRamp := StrToFloat(rdePhiRamp.Text);
  inherited;
end;

end.
