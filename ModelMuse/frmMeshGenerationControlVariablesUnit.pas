unit frmMeshGenerationControlVariablesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, Grids,
  RbwDataGrid4, StdCtrls, Buttons, ExtCtrls, UndoItems,
  SutraMeshUnit, ArgusDataEntry;

type
  TMeshControlType = (mtcNone, mctSplittingAngle, mctStructure,
    mtcNodePlacementError, mtcLineLength, mtcSymmetry, mtcConcave);
  TMeshControlLocation = (mtlNone, mtlGeneral, mtlSixNode);

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
    rdeGrowthRate: TRbwDataEntry;
    lblElementGrowthRate: TLabel;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure btnResetDefaultsClick(Sender: TObject);
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
  frmGoPhastUnit;

resourcestring
  StrSplittingAngle = 'Splitting angle';
  StrStructure = 'Structure';
  StrNodePlacementError = 'Node placement error';
  StrLineLength = 'Line length';
  StrSymmetry = 'Symmetry';
  StrConcave = 'Concave';
  StrGeneral = 'General';
  StrSixNodes = 'Six-Nodes';

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

  rdgControlVariables.Cells[Ord(mtlGeneral), Ord(mtcNone)] := StrGeneral;
  rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mtcNone)] := StrSixNodes;
  GetData;
end;

procedure TfrmMeshGenerationControlVariables.GetData;
var
  MeshGenControls: TMeshGenerationControls;
begin
  MeshGenControls := frmGoPhast.PhastModel.SutraMesh.Mesh2D.MeshGenControls;
  AssignValues(MeshGenControls);
end;

procedure TfrmMeshGenerationControlVariables.SetData;
var
  MeshGenControls: TMeshGenerationControls;
  Undo: TUndoChangeMeshGenControls;
begin
  MeshGenControls := TMeshGenerationControls.Create(nil);
  try
    MeshGenControls.SplittingAngle.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlGeneral),
      Ord(mctSplittingAngle)]);
    MeshGenControls.Structure.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlGeneral), Ord(mctStructure)]);
    MeshGenControls.NodePlacementError.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlGeneral),
      Ord(mtcNodePlacementError)]);
    MeshGenControls.LineLength.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlGeneral),
      Ord(mtcLineLength)]);
    MeshGenControls.Symmetry.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlGeneral), Ord(mtcSymmetry)]);
    MeshGenControls.Concave.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlGeneral), Ord(mtcConcave)]);

    MeshGenControls.AltSplittingAngle.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlSixNode),
      Ord(mctSplittingAngle)]);
    MeshGenControls.AltStructure.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mctStructure)]);
    MeshGenControls.AltNodePlacementError.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlSixNode),
      Ord(mtcNodePlacementError)]);
    MeshGenControls.AltLineLength.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlSixNode),
      Ord(mtcLineLength)]);
    MeshGenControls.AltSymmetry.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mtcSymmetry)]);
    MeshGenControls.AltConcave.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mtcConcave)]);

    MeshGenControls.ElementGrowthRate.Value := rdeGrowthRate.RealValue;

    Undo := TUndoChangeMeshGenControls.Create(MeshGenControls);
    frmGoPhast.UndoStack.Submit(Undo);
  finally
    MeshGenControls.Free;
  end;
end;

procedure TfrmMeshGenerationControlVariables.AssignValues(MeshGenControls
  : TMeshGenerationControls);
begin
  rdgControlVariables.Cells[Ord(mtlGeneral), Ord(mctSplittingAngle)] :=
    FloatToStr(MeshGenControls.SplittingAngle.Value);
  rdgControlVariables.Cells[Ord(mtlGeneral), Ord(mctStructure)] :=
    FloatToStr(MeshGenControls.Structure.Value);
  rdgControlVariables.Cells[Ord(mtlGeneral), Ord(mtcNodePlacementError)] :=
    FloatToStr(MeshGenControls.NodePlacementError.Value);
  rdgControlVariables.Cells[Ord(mtlGeneral), Ord(mtcLineLength)] :=
    FloatToStr(MeshGenControls.LineLength.Value);
  rdgControlVariables.Cells[Ord(mtlGeneral), Ord(mtcSymmetry)] :=
    FloatToStr(MeshGenControls.Symmetry.Value);
  rdgControlVariables.Cells[Ord(mtlGeneral), Ord(mtcConcave)] :=
    FloatToStr(MeshGenControls.Concave.Value);
  rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mctSplittingAngle)] :=
    FloatToStr(MeshGenControls.AltSplittingAngle.Value);
  rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mctStructure)] :=
    FloatToStr(MeshGenControls.AltStructure.Value);
  rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mtcNodePlacementError)] :=
    FloatToStr(MeshGenControls.AltNodePlacementError.Value);
  rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mtcLineLength)] :=
    FloatToStr(MeshGenControls.AltLineLength.Value);
  rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mtcSymmetry)] :=
    FloatToStr(MeshGenControls.AltSymmetry.Value);
  rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mtcConcave)] :=
    FloatToStr(MeshGenControls.AltConcave.Value);
  rdeGrowthRate.RealValue := MeshGenControls.ElementGrowthRate.Value;
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
