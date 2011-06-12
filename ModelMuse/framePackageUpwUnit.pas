unit framePackageUpwUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, 
  ModflowPackageSelectionUnit;

type
  TframePackageUpw = class(TframePackage)
    cbPrintHDRY: TCheckBox;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageUpw: TframePackageUpw;

implementation

{$R *.dfm}

{ TframePackageUpw }

procedure TframePackageUpw.GetData(Package: TModflowPackageSelection);
begin
  inherited;
  cbPrintHDRY.Checked := (Package as TUpwPackageSelection).
    HDryPrintOption = hpoPrintHdry;
end;

procedure TframePackageUpw.SetData(Package: TModflowPackageSelection);
begin
  inherited;
  if cbPrintHDRY.Checked then
  begin
    (Package as TUpwPackageSelection).HDryPrintOption := hpoPrintHdry;
  end
  else
  begin
    (Package as TUpwPackageSelection).HDryPrintOption := hpoDontPrintHdry;
  end;
end;

end.
