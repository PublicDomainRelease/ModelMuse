unit ImportQuadMesh;

interface

uses
  Classes, SutraMeshUnit, SysUtils;

procedure ImportSutraMeshFromFile(AFileName: string);

implementation

uses
  IOUtils, frmGoPhastUnit, ModelMuseUtilities, UndoItems;

type
  TUndoImportMesh = class(TUndoChangeMesh)
  private
    FUndoVE: TUndoVerticalExaggeration;
  protected
    function Description: string; override;
  public
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

procedure ImportSutraMeshFromFile(AFileName: string);
var
  FileReader: TStreamReader;
  Splitter: TStringList;
  Mesh2D: TSutraMesh2D;
  ElementCount: Integer;
  NodeCount: Integer;
  Nodes: array of TSutraNode2D;
  NodeNumber: Integer;
  X: Extended;
  Y: Extended;
  ANode: TSutraNode2D;
  ElementNumber: Integer;
  NodeIndex: Integer;
  AnElement: TSutraElement2D;
  NumberedNode: TSutraNodeNumber2D_Item;
  Undo: TUndoImportMesh;
  Mesh3D: TSutraMesh3D;
  NewNodeNumber: integer;
  NewElementNumber: integer;
begin
  FileReader := TFile.OpenText(AFileName);
  try
    Mesh3D := TSutraMesh3D.Create(nil);
    try
      Mesh2D := Mesh3D.Mesh2D;
      Mesh2D.MeshGenControls := frmGoPhast.PhastModel.SutraMesh.Mesh2D.MeshGenControls;
      Splitter := TStringList.Create;
      try
        Splitter.Delimiter := ' ';
        repeat
          Splitter.DelimitedText := FileReader.ReadLine;
        until (Splitter.Count > 0) and (Splitter[0] <> '#');
        Assert(Splitter.Count >= 2);
        ElementCount := StrToInt(Splitter[0]);
        NodeCount := StrToInt(Splitter[1]);
        SetLength(Nodes, NodeCount+1);
        Mesh2D.Nodes.Capacity := NodeCount;
        Mesh2D.Elements.Capacity := ElementCount;
        NewNodeNumber := 0;
        NewElementNumber := 0;
  //      SetLength(Elements, ElementCount+1);

        while not FileReader.EndOfStream do
        begin
          Splitter.DelimitedText := FileReader.ReadLine;
          if (Splitter.Count > 0) and (Splitter[0] <> '#') then
          begin
            if Splitter[0] = 'N' then
            begin
              Assert(Splitter.Count >= 4);
              NodeNumber := StrToInt(Splitter[1]);
              X := FortranStrToFloat(Splitter[2]);
              Y := FortranStrToFloat(Splitter[3]);
              ANode := TSutraNode2D.Create(Mesh2D.Nodes);
              ANode.X := X;
              ANode.Y := Y;
              ANode.Number := NewNodeNumber;
              Inc(NewNodeNumber);
              if NodeNumber >= Length(Nodes) then
              begin
                SetLength(Nodes, NodeNumber*2);
              end;
              Nodes[NodeNumber] := ANode;
            end
            else if Splitter[0] = 'E' then
            begin
              Assert(Splitter.Count >= 6);
              AnElement := TSutraElement2D.Create(Mesh2D.Elements);
              AnElement.ElementNumber := NewElementNumber;
              Inc(NewElementNumber);
              ElementNumber := StrToInt(Splitter[1]);
              for NodeIndex := 2 to 5 do
              begin
                NodeNumber := StrToInt(Splitter[NodeIndex]);
                ANode := Nodes[NodeNumber];
                Assert(ANode <> nil);
                NumberedNode := AnElement.Nodes.Add;
                NumberedNode.Node := ANode;
              end;
            end
            else
            begin
              Assert(False);
            end;
          end;
        end;

      finally
        Splitter.Free;
      end;
      Undo := TUndoImportMesh.Create;
      Undo.UpdateOldMesh(frmGoPhast.PhastModel.SutraMesh);
      Undo.UpdateNewMesh(Mesh3D);
      frmGoPhast.UndoStack.Submit(Undo);
    finally
      Mesh3D.Free;
    end;
  finally
    FileReader.Free
  end;
end;

{ TUndoImportMesh }

function TUndoImportMesh.Description: string;
begin
  result := 'ímport SUTRA mesh';
end;

destructor TUndoImportMesh.Destroy;
begin
  FUndoVE.Free;
  inherited;
end;

procedure TUndoImportMesh.DoCommand;
begin
  inherited;
  if FUndoVE = nil then
  begin
    FUndoVE := TUndoVerticalExaggeration.Create(frmGoPhast.DefaultVE);
  end;
  FUndoVE.DoCommand;
end;

procedure TUndoImportMesh.Undo;
begin
  inherited;
  FUndoVE.Undo;
end;

end.
