unit frmRenumberingMethodUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, StdCtrls,
  Buttons, ExtCtrls;

type
  TfrmRenumberingMethod = class(TfrmCustomGoPhast)
    rgMethod: TRadioGroup;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRenumberingMethod: TfrmRenumberingMethod;

implementation

{$R *.dfm}

end.
