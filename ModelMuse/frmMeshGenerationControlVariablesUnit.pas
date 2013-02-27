unit frmMeshGenerationControlVariablesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, Grids,
  RbwDataGrid4, StdCtrls, Buttons, ExtCtrls, UndoItems,
  SutraMeshUnit, ArgusDataEntry;

type
  TMeshControlType = (mtcNone, mtcGrowthRate, mctSplittingAngle,
    mtcLineLength, mtcSymmetry, mtcConcave, mctStructure, mtcNodePlacementError);
  TMeshControlLocation = (mtlNone, mtlVariable);

  TUndoChangeMeshGenControls = class(TCustomUndo)
  private
    FOldControls: TMeshGenerationControls;
    FNewControls: TMeshGenerationControls;
  protected
    function Description: string; override;
  public
    constructor Create(var Controls: TMeshGenerationControls);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TfrmMeshGenerationControlVariables = class(TfrmCustomGoPhast)
    rdgControlVariables: TRbwDataGrid4;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnResetDefaults: TButton;
    rgMethod: TRadioGroup;
    pnlTop: TPanel;
    rgRenumberingMethod: TRadioGroup;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure btnResetDefaultsClick(Sender: TObject);
    procedure rgMethodClick(Sender: TObject);
  private
    procedure GetData;
    procedure SetData;
    procedure AssignValues(MeshGenControls: TMeshGenerationControls);
    { Private declarations }
  public
    { Public declarations }
  end;

  // var
  // frmMeshGenerationControlVariables: TfrmMeshGenerationControlVariables;

implementation

uses
  frmGoPhastUnit, MeshRenumberingTypes;

resourcestring
  StrSplittingAngle = 'Splitting angle';
  StrStructure = 'Structure';
  StrNodePlacementError = 'Node placement error';
  StrLineLength = 'Line length';
  StrSymmetry = 'Symmetry';
  StrConcave = 'Concave';
  StrControlVariable = 'Control variable';
//  StrSixNodes = 'Six-Nodes';
  StrElementGrowthRate = 'Element growth rate';
  StrValue = 'Value';
  StrTheElementGrowthR = 'The element growth rate must be greater than 1.';

{$R *.dfm}

procedure TfrmMeshGenerationControlVariables.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmMeshGenerationControlVariables.btnResetDefaultsClick
  (Sender: TObject);
var
  MeshGenControls: TMeshGenerationControls;
begin
  inherited;
  MeshGenControls := TMeshGenerationControls.Create(nil);
  try
    AssignValues(MeshGenControls);
  finally
    MeshGenControls.Free;
  end;
end;

procedure TfrmMeshGenerationControlVariables.FormCreate(Sender: TObject);
begin
  inherited;
  rdgControlVariables.Cells[Ord(mtlNone), Ord(mctSplittingAngle)] :=
    StrSplittingAngle;
  rdgControlVariables.Cells[Ord(mtlNone), Ord(mctStructure)] := StrStructure;
  rdgControlVariables.Cells[Ord(mtlNone), Ord(mtcNodePlacementError)] :=
    StrNodePlacementError;
  rdgControlVariables.Cells[Ord(mtlNone), Ord(mtcLineLength)] := StrLineLength;
  rdgControlVariables.Cells[Ord(mtlNone), Ord(mtcSymmetry)] := StrSymmetry;
  rdgControlVariables.Cells[Ord(mtlNone), Ord(mtcConcave)] := StrConcave;
  rdgControlVariables.Cells[Ord(mtlNone), Ord(mtcGrowthRate)] := StrElementGrowthRate;

  rdgControlVariables.Cells[Ord(mtlNone), Ord(mtcNone)] := StrControlVariable;
  rdgControlVariables.Cells[Ord(mtlVariable), Ord(mtcNone)] := StrValue;
//  rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mtcNone)] := StrSixNodes;
  GetData;
end;

procedure TfrmMeshGenerationControlVariables.GetData;
var
  MeshGenControls: TMeshGenerationControls;
begin
  MeshGenControls := frmGoPhast.PhastModel.SutraMesh.Mesh2D.MeshGenControls;
  AssignValues(MeshGenControls);
end;

procedure TfrmMeshGenerationControlVariables.rgMethodClick(Sender: TObject);
begin
  inherited;
  rdgControlVariables.Enabled := rgMethod.ItemIndex = 1;
//  rdeGrowthRate.Enabled := rdgControlVariables.Enabled;
  if rdgControlVariables.Enabled then
  begin
    rdgControlVariables.Color := clWindow;
  end
  else
  begin
    rdgControlVariables.Color := clBtnFace;
  end;
end;

procedure TfrmMeshGenerationControlVariables.SetData;
var
  MeshGenControls: TMeshGenerationControls;
  Undo: TUndoChangeMeshGenControls;
begin
  MeshGenControls := TMeshGenerationControls.Create(nil);
  try
    MeshGenControls.MeshGenerationMethod :=
      TMeshGenerationMethod(rgMethod.ItemIndex);
    MeshGenControls.RenumberingAlgorithm :=
      TRenumberingAlgorithm(rgRenumberingMethod.ItemIndex);
    MeshGenControls.SplittingAngle.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlVariable),
      Ord(mctSplittingAngle)]);
    MeshGenControls.Structure.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlVariable), Ord(mctStructure)]);
    MeshGenControls.NodePlacementError.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlVariable),
      Ord(mtcNodePlacementError)]);
    MeshGenControls.LineLength.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlVariable),
      Ord(mtcLineLength)]);
    MeshGenControls.Symmetry.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlVariable), Ord(mtcSymmetry)]);
    MeshGenControls.Concave.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlVariable), Ord(mtcConcave)]);

//    MeshGenControls.AltSplittingAngle.Value :=
//      StrToFloat(rdgControlVariables.Cells[Ord(mtlSixNode),
//      Ord(mctSplittingAngle)]);
//    MeshGenControls.AltStructure.Value :=
//      StrToFloat(rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mctStructure)]);
//    MeshGenControls.AltNodePlacementError.Value :=
//      StrToFloat(rdgControlVariables.Cells[Ord(mtlSixNode),
//      Ord(mtcNodePlacementError)]);
//    MeshGenControls.AltLineLength.Value :=
//      StrToFloat(rdgControlVariables.Cells[Ord(mtlSixNode),
//      Ord(mtcLineLength)]);
//    MeshGenControls.AltSymmetry.Value :=
//      StrToFloat(rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mtcSymmetry)]);
//    MeshGenControls.AltConcave.Value :=
//      StrToFloat(rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mtcConcave)]);

    MeshGenControls.ElementGrowthRate.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlVariable), Ord(mtcGrowthRate)]);

    if MeshGenControls.ElementGrowthRate.Value <= 1 then
    begin
      Beep;
      MessageDlg(StrTheElementGrowthR, mtError, [mbOK], 0);
      ModalResult := mrNone;
      Exit;
    end;

    Undo := TUndoChangeMeshGenControls.Create(MeshGenControls);
    frmGoPhast.UndoStack.Submit(Undo);
  finally
    MeshGenControls.Free;
  end;
end;

procedure TfrmMeshGenerationControlVariables.AssignValues(MeshGenControls
  : TMeshGenerationControls);
begin
  case MeshGenControls.MeshGenerationMethod of
    mgmFishnet, mgmIrregular:
      begin
        rgMethod.ItemIndex := Ord(MeshGenControls.MeshGenerationMethod);
      end;
    mgmUnknown:
      begin
        if frmGoPhast.PhastModel.FishnetMeshGenerator.Elements.Count > 0 then
        begin
          rgMethod.ItemIndex := Ord(mgmFishnet);
        end
        else
        begin
          rgMethod.ItemIndex := Ord(mgmIrregular);
        end;
      end;
    else Assert(False);
  end;

  rgRenumberingMethod.ItemIndex := Ord(MeshGenControls.RenumberingAlgorithm);

  rdgControlVariables.Cells[Ord(mtlVariable), Ord(mctSplittingAngle)] :=
    FloatToStr(MeshGenControls.SplittingAngle.Value);
  rdgControlVariables.Cells[Ord(mtlVariable), Ord(mctStructure)] :=
    FloatToStr(MeshGenControls.Structure.Value);
  rdgControlVariables.Cells[Ord(mtlVariable), Ord(mtcNodePlacementError)] :=
    FloatToStr(MeshGenControls.NodePlacementError.Value);
  rdgControlVariables.Cells[Ord(mtlVariable), Ord(mtcLineLength)] :=
    FloatToStr(MeshGenControls.LineLength.Value);
  rdgControlVariables.Cells[Ord(mtlVariable), Ord(mtcSymmetry)] :=
    FloatToStr(MeshGenControls.Symmetry.Value);
  rdgControlVariables.Cells[Ord(mtlVariable), Ord(mtcConcave)] :=
    FloatToStr(MeshGenControls.Concave.Value);
//  rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mctSplittingAngle)] :=
//    FloatToStr(MeshGenControls.AltSplittingAngle.Value);
//  rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mctStructure)] :=
//    FloatToStr(MeshGenControls.AltStructure.Value);
//  rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mtcNodePlacementError)] :=
//    FloatToStr(MeshGenControls.AltNodePlacementError.Value);
//  rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mtcLineLength)] :=
//    FloatToStr(MeshGenControls.AltLineLength.Value);
//  rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mtcSymmetry)] :=
//    FloatToStr(MeshGenControls.AltSymmetry.Value);
//  rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mtcConcave)] :=
//    FloatToStr(MeshGenControls.AltConcave.Value);
  rdgControlVariables.Cells[Ord(mtlVariable), Ord(mtcGrowthRate)] :=
    FloatToStr(MeshGenControls.ElementGrowthRate.Value);
end;

{ TUndoChangeMeshGenControls }

constructor TUndoChangeMeshGenControls.Create(var Controls
  : TMeshGenerationControls);
begin
  FOldControls := TMeshGenerationControls.Create(nil);
  FOldControls.Assign(frmGoPhast.PhastModel.SutraMesh.Mesh2D.MeshGenControls);
  FNewControls := Controls;
  Controls := nil;
end;

function TUndoChangeMeshGenControls.Description: string;
begin
  result := 'change mesh generation controls';
end;

destructor TUndoChangeMeshGenControls.Destroy;
begin
  FOldControls.Free;
  FNewControls.Free;
  inherited;
end;

procedure TUndoChangeMeshGenControls.DoCommand;
begin
  frmGoPhast.PhastModel.SutraMesh.Mesh2D.MeshGenControls := FNewControls;
  frmGoPhast.PhastModel.SutraMesh.Mesh2D.MeshGenControls.Apply;
  inherited;
end;

procedure TUndoChangeMeshGenControls.Undo;
begin
  frmGoPhast.PhastModel.SutraMesh.Mesh2D.MeshGenControls := FOldControls;
  frmGoPhast.PhastModel.SutraMesh.Mesh2D.MeshGenControls.Apply;
  inherited;
end;

end.
