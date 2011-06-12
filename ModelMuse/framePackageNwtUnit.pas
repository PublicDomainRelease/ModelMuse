unit framePackageNwtUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, ArgusDataEntry, Mask,
  JvExMask, JvSpin, JvExStdCtrls, JvCombobox, JvListComb, ComCtrls;

type
  TframePackageNwt = class(TframePackage)
    PageControl1: TPageControl;
    tabBasic: TTabSheet;
    rdeHeadTolerance: TRbwDataEntry;
    rdeFluxTolerance: TRbwDataEntry;
    spinMaxOuterIt: TJvSpinEdit;
    rdeThicknessFactor: TRbwDataEntry;
    comboSolverMethod: TJvImageComboBox;
    lblSolverMethod: TLabel;
    lblThicknessFactor: TLabel;
    lblMaxOuterIt: TLabel;
    lblFluxTolerance: TLabel;
    lblHeadTolerance: TLabel;
    cbPrintFlag: TCheckBox;
    cbCorrectForCellBottom: TCheckBox;
    comboOptions: TJvImageComboBox;
    lblOptions: TLabel;
    tabAdditional: TTabSheet;
    rdeDbdTheta: TRbwDataEntry;
    lblDbdTheta: TLabel;
    Label1: TLabel;
    rdeDbdKappa: TRbwDataEntry;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  framePackageNwt: TframePackageNwt;

implementation

{$R *.dfm}

end.
