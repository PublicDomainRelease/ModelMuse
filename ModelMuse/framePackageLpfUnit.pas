unit framePackageLpfUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, StdCtrls, RbwController, JvExStdCtrls, JvCheckBox,
  ModflowPackageSelectionUnit, Grids, RbwDataGrid4;

type
  TframePackageLpf = class(TframePackage)
    rdgOptions: TRbwDataGrid4;
    procedure FrameResize(Sender: TObject);
    procedure rdgOptionsVerticalScroll(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure Loaded; override;
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageLpf: TframePackageLpf;

implementation

{$R *.dfm}

type
  TLpfOptionRows = (lorStorageCoefficient, lorThikStrt, lorConstantCV, 
    lorNoCvCorrection, lorNoVFC);


{ TframePackageLpf }

procedure TframePackageLpf.FrameResize(Sender: TObject);
begin
  inherited;
  rdgOptions.BeginUpdate;
  rdgOptions.ColWidths[0] := rdgOptions.Width-4;
  rdgOptions.EndUpdate;
end;

procedure TframePackageLpf.GetData(Package: TModflowPackageSelection);
var
  LpfPackage: TLpfSelection;
begin
  inherited;
  LpfPackage := Package as TLpfSelection;

  rdgOptions.Checked[0, Ord(lorStorageCoefficient)] :=
    LpfPackage.UseStorageCoefficient;
  rdgOptions.Checked[0, Ord(lorConstantCV)] := LpfPackage.UseConstantCV;
  rdgOptions.Checked[0, Ord(lorThikStrt)] := LpfPackage.UseSaturatedThickness;
  rdgOptions.Checked[0, Ord(lorNoCvCorrection)] := LpfPackage.UseCvCorrection;
  rdgOptions.Checked[0, Ord(lorNoVFC)] := LpfPackage.UseVerticalFlowCorrection;

end;

procedure TframePackageLpf.Loaded;
begin
  inherited;
  FrameResize(self);
  rdgOptions.Cells[0, Ord(lorStorageCoefficient)] :=
    'Interpret variable Ss and SS parameters as storage coefficient rather than specific storage (STORAGECOEFFICIENT)';
  rdgOptions.Cells[0, Ord(lorConstantCV)] :=
    'Use cell thickness to compute vertical conductance in unconfined cells (CONSTANTCV)';

// ModelMuse determines the designated layers by those in which LAYTYP < 0
  rdgOptions.Cells[0, Ord(lorThikStrt)] :=
    'In designated confined layers; starting heads will be used to compute cell thickness (THICKSTRT)';
  rdgOptions.Cells[0, Ord(lorNoCvCorrection)] :=
    'Use vertical conductance correction (inverse of NOCVCORRECTION)';
  rdgOptions.Cells[0, Ord(lorNoVFC)] :=
    'Use vertical flow correction under dewatered conditions (inverse of NOVFC)';
end;

procedure TframePackageLpf.rdgOptionsVerticalScroll(Sender: TObject);
begin
  inherited;
  // this ensures that the cells are redrawn properly.
  rdgOptions.EditorMode := False;
end;

procedure TframePackageLpf.SetData(Package: TModflowPackageSelection);
var
  LpfPackage: TLpfSelection;
begin
  inherited;
  LpfPackage := Package as TLpfSelection;

  LpfPackage.UseStorageCoefficient := rdgOptions.Checked[0, Ord(lorStorageCoefficient)];
  LpfPackage.UseConstantCV := rdgOptions.Checked[0, Ord(lorConstantCV)];
  LpfPackage.UseSaturatedThickness := rdgOptions.Checked[0, Ord(lorThikStrt)];
  LpfPackage.UseCvCorrection := rdgOptions.Checked[0, Ord(lorNoCvCorrection)];
  LpfPackage.UseVerticalFlowCorrection := rdgOptions.Checked[0, Ord(lorNoVFC)];
end;

end.
