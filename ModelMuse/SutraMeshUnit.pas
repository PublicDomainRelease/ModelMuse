unit SutraMeshUnit;

interface

uses
  Windows, FastGEO, Classes, GoPhastTypes, GR32, ZoomBox2, MeshRenumbering,
  AbstractGridUnit, Generics.Collections, gpc, Generics.Defaults;

Type
  TDrawingChoice = (dcAll, dcEdge);

  TCustomSutraMesh = class;

  TCustomSutraItem = class(TPhastCollectionItem)
  protected
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
  end;

  TCustomSutraNode = class(TCustomSutraItem)
  private
    FNumber: Integer;
    procedure SetNumber(const Value: Integer);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Number: Integer read FNumber write SetNumber;
  end;

  TVertexList = TList<Tgpc_vertex>;
  TVertexArray = array of Tgpc_vertex;
  TSutraElement2D_List = class;
  TPoint2DComparer = TComparer<TPoint2D>;

  TSutraNode2D = class(TCustomSutraNode)
  private
    FLocation: TPoint2D;
    FNodeType: TNodeType;
    FElements: TSutraElement2D_List;
    procedure SetLocation(const Value: TPoint2D);
    procedure SetX(const Value: FastGEO.TFloat);
    procedure SetY(const Value: FastGEO.TFloat);
    procedure SetNodeType(const Value: TNodeType);
    procedure GetCellOutline(var CellOutline: TVertexArray);
    procedure DrawTop(const BitMap: TBitmap32;
      const ZoomBox: TQRbwZoomBox2; DrawingChoice: TDrawingChoice);
  public
    property Location: TPoint2D read FLocation write SetLocation;
    procedure Assign(Source: TPersistent); override;
    procedure AssignINode(Source: INode);
    function IsInsideCell(APoint: TPoint2D): Boolean;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function CellIntersection(const Input: TSegment2D;
      out IntersectingSegments: TSegment2DArray): boolean;
    function EdgeNode: boolean;
  published
    property X: FastGEO.TFloat read FLocation.x write SetX;
    property Y: FastGEO.TFloat read FLocation.y write SetY;
    property NodeType: TNodeType read FNodeType write SetNodeType;
  end;

  TSutraNode2D_List = TList<TSutraNode2D>;

  TCustomSutraCollection = class(TPhastCollection)
  public
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
  end;

  TSutraNode2D_Collection = class(TCustomSutraCollection)
  private
    function GetItem(Index: integer): TSutraNode2D;
  public
    constructor Create(Model: TBaseModel);
    property Items[Index: integer]: TSutraNode2D read GetItem; default;
    function Add: TSutraNode2D;
    function TopContainingCell(APoint: TPoint2D): T2DTopCell;
  end;

  TCustomSutraNodeNumberItem = class(TCustomSutraItem)
  private
    function GetNodeNumber: integer;
    procedure SetNodeNumber(const Value: integer);
    procedure SetNode(const Value: TCustomSutraNode);
  protected
    FNode: TCustomSutraNode;
    FStoredNodeNumber: integer;
    // Set FNode to the node whose number is FStoredNodeNumber
    procedure UpdateNode; virtual; abstract;
  public
    procedure Assign(Source: TPersistent); override;
    property Node: TCustomSutraNode read FNode write SetNode;
  published
    Property NodeNumber: integer read GetNodeNumber write SetNodeNumber;
  end;

  TSutraNodeNumber2D_Item = class(TCustomSutraNodeNumberItem)
  private
    function GetNode: TSutraNode2D;
    procedure SetNode(const Value: TSutraNode2D);
  protected
    // Set FNode to the node whose number is FStoredNodeNumber
    procedure UpdateNode; override;
  public
    property Node: TSutraNode2D read GetNode write SetNode;
  end;

  TSutraElement2D = class;

  TSutraNodeNumber2D_Collection = class(TCustomSutraCollection)
  private
    FElement: TSutraElement2D;
    function GetItem(Index: integer): TSutraNodeNumber2D_Item;
  public
    constructor Create(Model: TBaseModel; Element: TSutraElement2D);
    property Items[Index: integer]: TSutraNodeNumber2D_Item read GetItem; default;
    function Add: TSutraNodeNumber2D_Item;
    function IndexOfNode(Node: TSutraNode2D): Integer;
  end;

  TCustomSutraElement = class(TCustomSutraItem)
  private
    FElementNumber: integer;
    procedure SetElementNumber(const Value: integer);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property ElementNumber: integer read FElementNumber write SetElementNumber;
  end;

  TSutraElement2D = class(TCustomSutraElement)
  private
    FNodes: TSutraNodeNumber2D_Collection;
    procedure SetNodes(const Value: TSutraNodeNumber2D_Collection);
    procedure DrawTop(const BitMap: TBitmap32;
      const ZoomBox: TQRbwZoomBox2; DrawingChoice: TDrawingChoice);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignIElement(Source: IElement);
    function IsInside(APoint: TPoint2D): Boolean;
    function Center: TPoint2D;
    function Intersection(const Input: TSegment2D;
      out IntersectingSegment: TSegment2D): boolean;
  published
    property Nodes: TSutraNodeNumber2D_Collection read FNodes write SetNodes;
  end;

  TSutraElement2D_List = class(TList<TSutraElement2D>);

  TSutraElement2D_Collection = class(TCustomSutraCollection)
  private
    function GetItems(Index: integer): TSutraElement2D;
  public
    constructor Create(Model: TBaseModel);
    property Items[Index: integer]: TSutraElement2D read GetItems; default;
    function Add: TSutraElement2D;
    function TopContainingElement(APoint: TPoint2D): T2DTopCell;
  end;

  TCustomSutraMesh = class(TGoPhastPersistent)
  private
    FUpdateCount: Integer;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
  end;

  TSutraMesh2D = class(TCustomSutraMesh)
  private
    FElements: TSutraElement2D_Collection;
    FNodes: TSutraNode2D_Collection;
    FDrawingChoice: TDrawingChoice;
    procedure SetElements(const Value: TSutraElement2D_Collection);
    procedure SetNodes(const Value: TSutraNode2D_Collection);
    procedure DrawTop(const BitMap: TBitmap32;
      const ZoomBox: TQRbwZoomBox2);
    procedure SetDrawingChoice(const Value: TDrawingChoice);
    function MeshLimits: TGridLimit;
    function MeshBox: TPolygon2D;
    function MeshOutline: TPolygon2D;
  public
    Constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    procedure Draw(const BitMap: TBitmap32;
      const ViewDirection: TViewDirection);
    procedure Clear;
    property DrawingChoice: TDrawingChoice read FDrawingChoice
      write SetDrawingChoice;
    function TopContainingCell(APoint: TPoint2D;
      const EvaluatedAt: TEvaluatedAt): T2DTopCell;
  published
    property Nodes: TSutraNode2D_Collection read FNodes write SetNodes;
    property Elements: TSutraElement2D_Collection read FElements
      write SetElements;
  end;

  TSutraNode3D = class(TCustomSutraNode)
  private
    FNode2D: TSutraNode2D;
    FZ: FastGEO.TFloat;
    FNode2D_Number: Integer;
    function GetNode2D_Number: integer;
    procedure SetNode2D_Number(const Value: integer);
    procedure SetZ(const Value: FastGEO.TFloat);
    function GetNode2D: TSutraNode2D;
    procedure UpdateNode2D;
  public
    procedure Assign(Source: TPersistent); override;
    procedure AssignINode(Source: INode);
    property Node2D: TSutraNode2D read GetNode2D;
  published
    property Z: FastGEO.TFloat read FZ write SetZ;
    property Node2D_Number: integer read GetNode2D_Number write SetNode2D_Number;
  end;

  TSutraNode3D_Collection = class(TCustomSutraCollection)
  private
    function GetItem(Index: integer): TSutraNode3D;
  public
    constructor Create(Model: TBaseModel);
    property Items[Index: integer]: TSutraNode3D read GetItem; default;
    function Add: TSutraNode3D;
  end;

  TSutraNodeNumber3D_Item = class(TCustomSutraNodeNumberItem)
  private
    function GetNode: TSutraNode3D;
    procedure SetNode(const Value: TSutraNode3D);
  protected
    // Set FNode to the node whose number is FStoredNodeNumber
    procedure UpdateNode; override;
  public
    property Node: TSutraNode3D read GetNode write SetNode;
  end;

  TSutraNodeNumber3D_Collection = class(TCustomSutraCollection)
  private
    function GetItem(Index: integer): TSutraNodeNumber3D_Item;
  public
    constructor Create(Model: TBaseModel);
    property Items[Index: integer]: TSutraNodeNumber3D_Item read GetItem; default;
    function Add: TSutraNodeNumber3D_Item;
  end;

  TSutraElement3D = class(TCustomSutraElement)
  private
    FNodes: TSutraNodeNumber3D_Collection;
    procedure SetNodes(const Value: TSutraNodeNumber3D_Collection);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignIElement(Source: IElement);
  published
    property Nodes: TSutraNodeNumber3D_Collection read FNodes write SetNodes;
  end;

  TSutraElement3D_Collection = class(TCustomSutraCollection)
  private
    function GetItems(Index: integer): TSutraElement3D;
  public
    constructor Create(Model: TBaseModel);
    property Items[Index: integer]: TSutraElement3D read GetItems; default;
    function Add: TSutraElement3D;
  end;

  TSutraMesh3D = class(TCustomSutraMesh)
  private
    FElements: TSutraElement3D_Collection;
    FNodes: TSutraNode3D_Collection;
    FMesh2D: TSutraMesh2D;
    FDrawingChoice: TDrawingChoice;
    procedure SetElements(const Value: TSutraElement3D_Collection);
    procedure SetNodes(const Value: TSutraNode3D_Collection);
    procedure SetMesh2D(const Value: TSutraMesh2D);
    procedure SetDrawingChoice(const Value: TDrawingChoice);
  public
    Constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    procedure Clear;
    procedure Draw(const BitMap: TBitmap32;
      const ViewDirection: TViewDirection);
    function TopContainingCell(APoint: TPoint2D;
      const EvaluatedAt: TEvaluatedAt): T2DTopCell;
    function MeshLimits(ViewDirection: TViewDirection): TGridLimit;
    function MeshBox(ViewDirection: TViewDirection): TPolygon2D;
  published
    property Nodes: TSutraNode3D_Collection read FNodes write SetNodes;
    property Elements: TSutraElement3D_Collection read FElements
      write SetElements;
    property Mesh2D: TSutraMesh2D read FMesh2D write SetMesh2D;
    property DrawingChoice: TDrawingChoice read FDrawingChoice
      write SetDrawingChoice;
  end;


implementation

uses
  frmGoPhastUnit, BigCanvasMethods, PhastModelUnit, SysUtils, GPC_Classes, Math,
  Dialogs;

resourcestring
  StrErrorGeneratingCel = 'Error generating cell for Node %d.';

{ TSutraNode2D }

procedure TSutraNode2D.Assign(Source: TPersistent);
var
  SourceNode: TSutraNode2D;
begin
  if Source is TSutraNode2D then
  begin
    SourceNode := TSutraNode2D(Source);
    Location := SourceNode.Location;
    NodeType := SourceNode.NodeType;
  end;
  inherited;
end;

procedure TSutraNode2D.AssignINode(Source: INode);
begin
  Location := Source.Location;
  Number := Source.NodeNumber;
  NodeType := Source.NodeType;
end;

constructor TSutraNode2D.Create(Collection: TCollection);
begin
  inherited;
  FElements := TSutraElement2D_List.Create;
end;

destructor TSutraNode2D.Destroy;
begin
  FElements.Free;
  inherited;
end;

procedure TSutraNode2D.DrawTop(const BitMap: TBitmap32;
  const ZoomBox: TQRbwZoomBox2; DrawingChoice: TDrawingChoice);
var
  Outline: TVertexArray;
  NodeIndex: Integer;
  Points: array of TPoint;
begin
  GetCellOutline(Outline);
  case DrawingChoice of
    dcAll:
      begin
        SetLength(Points, Length(Outline)+1);
        for NodeIndex := 0 to Length(Outline) - 1 do
        begin
          Points[NodeIndex] := ConvertTop2D_Point(ZoomBox, Outline[NodeIndex]);
        end;
        Points[Length(Points)-1] := Points[0];
        DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
          Points, True, True);
      end;
    dcEdge: ; // do nothing
    else
      Assert(False);
  end;
end;

function TSutraNode2D.EdgeNode: boolean;
begin
  result := NodeType in [ntEdge, ntSubDomain]
end;

Function ComparePointRightUp(const P1, P2: TPoint2D): integer;
begin
  result := Sign(P1.x - P2.x);
  if result = 0 then
  begin
    result := Sign(P1.y - P2.y);
  end;
end;

Function ComparePointRightDown(const P1, P2: TPoint2D): integer;
begin
  result := Sign(P1.x - P2.x);
  if result = 0 then
  begin
    result := Sign(P2.y - P1.y);
  end;
end;

Function ComparePointLeftUp(const P1, P2: TPoint2D): integer;
begin
  result := Sign(P2.x - P1.x);
  if result = 0 then
  begin
    result := Sign(P1.y - P2.y);
  end;
end;

Function ComparePointLeftDown(const P1, P2: TPoint2D): integer;
begin
  result := Sign(P2.x - P1.x);
  if result = 0 then
  begin
    result := Sign(P2.y - P1.y);
  end;
end;

function TSutraNode2D.CellIntersection(const Input: TSegment2D;
  out IntersectingSegments: TSegment2DArray): boolean;
var
  Outline: TVertexArray;
  IntersectionList: TVertexList;
  EdgeSegment: TSegment2D;
  NodeIndex: Integer;
  IntersectIndex: integer;
  APoint: TPoint2D;
  PointIndex: Integer;
begin
  { TODO -cSUTRA : Check that segments are created along the edge of the mesh. }
  result := False;
  GetCellOutline(Outline);
  IntersectionList := TVertexList.Create;
  try
    IntersectionList.Capacity := Length(Outline)+2;
    EdgeSegment[2] := Outline[Length(Outline)-1];
    for NodeIndex := 0 to Length(Outline) - 1 do
    begin
      EdgeSegment[1] := Outline[NodeIndex];
      if Collinear(EdgeSegment[1], Input[1], Input[2])
        and Collinear(EdgeSegment[2], Input[1], Input[2]) then
      begin
        IntersectionList.Add(EdgeSegment[1]);
        IntersectionList.Add(EdgeSegment[2]);
      end
      else
      if Intersect(EdgeSegment, Input, APoint.x, APoint.y) then
      begin
        IntersectionList.Add(APoint);
      end;
      EdgeSegment[2] := EdgeSegment[1];
    end;
    if (IntersectionList.Count = 0) then
    begin
      if IsInsideCell(Input[1]) then
      begin
        SetLength(IntersectingSegments, 1);
        IntersectingSegments[0] := Input;
        result := true;
      end;
      Exit;
    end;
    IntersectionList.Add(Input[1]);
    IntersectionList.Add(Input[2]);
    if Input[2].x >= Input[1].x then
    begin
      if Input[2].y >= Input[1].y then
      begin
        IntersectionList.Sort(TPoint2DComparer.Construct(ComparePointRightUp));
      end
      else
      begin
        IntersectionList.Sort(TPoint2DComparer.Construct(ComparePointRightDown));
      end;
    end
    else
    begin
      if Input[2].y >= Input[1].y then
      begin
        IntersectionList.Sort(TPoint2DComparer.Construct(ComparePointLeftUp));
      end
      else
      begin
        IntersectionList.Sort(TPoint2DComparer.Construct(ComparePointLeftDown));
      end;
    end;
    for PointIndex := IntersectionList.Count-2 downto 0 do
    begin
      if IsEqual(IntersectionList[PointIndex],
        IntersectionList[PointIndex+1], Epsilon) then
      begin
        IntersectionList.Delete(PointIndex);
      end;
    end;
    if IntersectionList.Count = 1 then
    begin
      IntersectionList.Add(IntersectionList[0]);
    end;
    SetLength(IntersectingSegments, IntersectionList.Count-1);
    IntersectIndex := 0;
    for PointIndex := 0 to IntersectionList.Count - 2 do
    begin
      APoint.x := (IntersectionList[PointIndex].x
        + IntersectionList[PointIndex+1].x)/2;
      APoint.y := (IntersectionList[PointIndex].y
        + IntersectionList[PointIndex+1].y)/2;
      if IsInsideCell(APoint) then
      begin
        IntersectingSegments[IntersectIndex] :=
          EquateSegment(IntersectionList[PointIndex],
          IntersectionList[PointIndex+1]);
        Inc(IntersectIndex);
      end;
    end;
  finally
    IntersectionList.Free;
  end;
  SetLength(IntersectingSegments, IntersectIndex);
  result := IntersectIndex > 0;
end;

procedure TSutraNode2D.GetCellOutline(var CellOutline: TVertexArray);
var
  PriorUnion: TGpcPolygonClass;
  NodePosition: Integer;
  PriorIndex: Integer;
  VertexList: TVertexList;
  SelfVertex: Tgpc_vertex;
  VertexIndex: Integer;
  NextNode: TSutraNode2D;
  Element: TSutraElement2D;
  UnionPolygon: TGpcPolygonClass;
  NodeList: TSutraNode2D_List;
  NextIndex: Integer;
  ElCenter: TPoint2D;
  EmptyPolygon: TGpcPolygonClass;
  AVertex: Tgpc_vertex;
  ElementPolygons: TGpcPolygonClass;
  ElementIndex: Integer;
  ReduceCount: Integer;
  PriorNode: TSutraNode2D;
  SelfIndex: Integer;
const
  MaxReduceCount = 10;
begin
  // For now, generate the cell outline each time.
  { TODO -cSUTRA : Generate SUTRA cell outline only when needed. }
  VertexList := TVertexList.Create;
  ElementPolygons := TGpcPolygonClass.Create;
  NodeList := TSutraNode2D_List.Create;
  try
    SelfVertex.x := X;
    SelfVertex.y := Y;
    ElementPolygons.NumberOfContours := FElements.Count;
    for ElementIndex := 0 to FElements.Count - 1 do
    begin
      Element := FElements[ElementIndex];
      ElementPolygons.VertexCount[ElementIndex] := 4;
      ElementPolygons.Vertices[ElementIndex, 0] := SelfVertex;
      SelfIndex := Element.Nodes.IndexOfNode(self);
      Assert(SelfIndex >= 0);
      if SelfIndex = Element.Nodes.Count - 1 then
      begin
        NextIndex := 0;
      end
      else
      begin
        NextIndex := SelfIndex + 1;
      end;
      NextNode := Element.Nodes[NextIndex].Node;
      NodePosition := NodeList.IndexOf(NextNode);
      if NodePosition < 0 then
      begin
        AVertex.X := (X + NextNode.X) / 2;
        AVertex.Y := (Y + NextNode.Y) / 2;
        NodeList.Add(NextNode);
        VertexList.Add(AVertex);
      end
      else
      begin
        AVertex := VertexList[NodePosition];
      end;
      ElementPolygons.Vertices[ElementIndex, 1] := AVertex;
      ElCenter := Element.Center;
      AVertex.x := ElCenter.x;
      AVertex.y := ElCenter.y;
      ElementPolygons.Vertices[ElementIndex, 2] := AVertex;
      if SelfIndex = 0 then
      begin
        PriorIndex := Element.Nodes.Count - 1;
      end
      else
      begin
        PriorIndex := SelfIndex - 1;
      end;
      PriorNode := Element.Nodes[PriorIndex].Node;
      NodePosition := NodeList.IndexOf(PriorNode);
      if NodePosition < 0 then
      begin
        AVertex.X := (X + PriorNode.X) / 2;
        AVertex.Y := (Y + PriorNode.Y) / 2;
        NodeList.Add(PriorNode);
        VertexList.Add(AVertex);
      end
      else
      begin
        AVertex := VertexList[NodePosition];
      end;
      ElementPolygons.Vertices[ElementIndex, 3] := AVertex;
    end;
    EmptyPolygon := TGpcPolygonClass.Create;
    try
      EmptyPolygon.NumberOfContours := 0;
      UnionPolygon := TGpcPolygonClass.CreateFromOperation(GPC_UNION,
        ElementPolygons, EmptyPolygon);
      try
        ReduceCount := 0;
        while UnionPolygon.NumberOfContours > 1 do
        begin
          PriorUnion := UnionPolygon;
          UnionPolygon := TGpcPolygonClass.CreateFromOperation(GPC_UNION,
            PriorUnion, EmptyPolygon);
          PriorUnion.Free;
          Inc(ReduceCount);
          if ReduceCount >= MaxReduceCount then
          begin
            {$IFDEF DEBUG}
            SetLength(CellOutline, 0);
            Beep;
            ShowMessage(IntToStr(Number));
            Exit;
            {$ELSE}
            raise Exception.Create(Format(StrErrorGeneratingCel, [Number + 1]));
            {$ENDIF}
          end;
        end;
        Assert(UnionPolygon.NumberOfContours = 1);
        SetLength(CellOutline, UnionPolygon.VertexCount[0]);
        for VertexIndex := 0 to UnionPolygon.VertexCount[0] - 1 do
        begin
          CellOutline[VertexIndex] := UnionPolygon.Vertices[0, VertexIndex];
        end;
      finally
        UnionPolygon.Free;
      end;
    finally
      EmptyPolygon.Free;
    end;
  finally
    NodeList.Free;
    ElementPolygons.Free;
    VertexList.Free;
  end;
end;

function TSutraNode2D.IsInsideCell(APoint: TPoint2D): Boolean;
var
  AVertex: Tgpc_vertex;
  CellOutline : TVertexArray;
  VertexIndex: Integer;
  PriorVertex: Tgpc_vertex;
begin
  GetCellOutline(CellOutline);

  result := false;
  PriorVertex := CellOutline[Length(CellOutline) - 1];
  for VertexIndex := 0 to Length(CellOutline) - 1 do
  begin
    AVertex := CellOutline[VertexIndex];;
    if ((APoint.Y <= AVertex.Y) = (APoint.Y > PriorVertex.Y)) and
      (APoint.X - AVertex.X - (APoint.Y - AVertex.Y) *
      (PriorVertex.X - AVertex.X) /
      (PriorVertex.Y - AVertex.Y) < 0) then
    begin
      result := not result;
    end;
    PriorVertex := AVertex;
  end;


end;

procedure TSutraNode2D.SetNodeType(const Value: TNodeType);
begin
  if FNodeType <> Value then
  begin
    BeginUpdate;
    try
      FNodeType := Value;
      InvalidateModel;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSutraNode2D.SetLocation(const Value: TPoint2D);
begin
  X := Value.x;
  Y := Value.y;
end;

procedure TSutraNode2D.SetX(const Value: FastGEO.TFloat);
begin
  SetRealProperty(FLocation.x, Value);
end;

procedure TSutraNode2D.SetY(const Value: FastGEO.TFloat);
begin
  SetRealProperty(FLocation.y, Value);
end;

{ TSutraNode2D_Collection }

function TSutraNode2D_Collection.Add: TSutraNode2D;
begin
  result := inherited Add as TSutraNode2D;
end;

constructor TSutraNode2D_Collection.Create(Model: TBaseModel);
begin
  inherited Create(TSutraNode2D, Model);
end;

procedure TCustomSutraNode.Assign(Source: TPersistent);
var
  SourceNode: TCustomSutraNode;
begin
  if Source is TCustomSutraNode then
  begin
    SourceNode := TCustomSutraNode(Source);
    Number := SourceNode.Number;
  end
  else
  begin
    inherited;
  end;
end;

procedure TCustomSutraNode.SetNumber(const Value: Integer);
begin
  SetIntegerProperty(FNumber, Value);
end;

{ TSutraNodeNumberItem }

procedure TCustomSutraNodeNumberItem.Assign(Source: TPersistent);
var
  SourceItem: TCustomSutraNodeNumberItem;
begin
  if Source is TCustomSutraNodeNumberItem then
  begin
    SourceItem := TCustomSutraNodeNumberItem(Source);
    NodeNumber := SourceItem.NodeNumber;
  end
  else
  begin
    inherited;
  end;
end;

function TCustomSutraNodeNumberItem.GetNodeNumber: integer;
begin
  if FNode = nil then
  begin
    result := FStoredNodeNumber;
  end
  else
  begin
    result := FNode.Number;
  end;
end;

procedure TCustomSutraNodeNumberItem.SetNode(const Value: TCustomSutraNode);
begin
  if FNode <> Value then
  begin
    InvalidateModel;
  end;
  FNode := Value;
  if FNode <> nil then
  begin
    FStoredNodeNumber := FNode.Number;
  end;
end;

procedure TCustomSutraNodeNumberItem.SetNodeNumber(const Value: integer);
begin
  SetIntegerProperty(FStoredNodeNumber, Value);
  UpdateNode;
end;

function TSutraNode2D_Collection.GetItem(Index: integer): TSutraNode2D;
begin
  result := inherited Items[Index] as TSutraNode2D;
end;

function TSutraNode2D_Collection.TopContainingCell(
  APoint: TPoint2D): T2DTopCell;
var
  index: Integer;
  Node: TSutraNode2D;
begin
  { TODO -cSUTRA : For now, use a simple, inefficient solution. }
  Result.Row := -1;
  Result.Col := -1;
  for index := 0 to Count - 1 do
  begin
    Node := Items[index];
    if Node.IsInsideCell(APoint) then
    begin
      Result.Row := 0;
      Result.Col := Node.Number;
      Exit;
    end;
  end;
end;

{ TSutraNodeNumber2D_Item }

function TSutraNodeNumber2D_Item.GetNode: TSutraNode2D;
begin
  result := inherited Node as TSutraNode2D;
end;

procedure TSutraNodeNumber2D_Item.SetNode(const Value: TSutraNode2D);
begin
  inherited SetNode(Value);
end;

procedure TSutraNodeNumber2D_Item.UpdateNode;
var
  LocalModel: TCustomModel;
  Mesh: TSutraMesh2D;
  NewNode: TSutraNode2D;
  Element: TSutraElement2D;
begin
  LocalModel := Model as TCustomModel;
  Mesh := LocalModel.SutraMesh.Mesh2D;
  if (FStoredNodeNumber >= 0) and (FStoredNodeNumber < Mesh.Nodes.Count) then
  begin
    NewNode := Mesh.Nodes[FStoredNodeNumber];
  end
  else
  begin
    NewNode := nil;
  end;
  if FNode <> NewNode then
  begin
    Element := (Collection as TSutraNodeNumber2D_Collection).FElement;
    if FNode <> nil then
    begin
      (FNode as TSutraNode2D).FElements.Remove(Element);
    end;
    if (NewNode <> nil) and (NewNode.FElements.IndexOf(Element) < 0) then
    begin
      NewNode.FElements.Add(Element);
    end;
    Mesh.BeginUpdate;
    try
      FNode := NewNode;
    finally
      Mesh.EndUpdate;
    end;
  end;
end;

{ TSutraNodeNumber2D_Collection }

function TSutraNodeNumber2D_Collection.Add: TSutraNodeNumber2D_Item;
begin
  result := inherited Add as TSutraNodeNumber2D_Item;
end;

constructor TSutraNodeNumber2D_Collection.Create(Model: TBaseModel; Element: TSutraElement2D);
begin
  FElement := Element;
  inherited Create(TSutraNodeNumber2D_Item, Model);
end;

function TSutraNodeNumber2D_Collection.GetItem(
  Index: integer): TSutraNodeNumber2D_Item;
begin
  result := inherited Items[Index] as TSutraNodeNumber2D_Item;
end;

function TSutraNodeNumber2D_Collection.IndexOfNode(Node: TSutraNode2D): Integer;
var
  NodeIndex: Integer;
begin
  result := -1;
  for NodeIndex := 0 to Count - 1 do
  begin
    if Items[NodeIndex].Node = Node then
    begin
      result := NodeIndex;
      Exit;
    end;
  end;
end;

{ TCustomSutraElement }

procedure TCustomSutraElement.Assign(Source: TPersistent);
var
  SourceElement: TCustomSutraElement;
begin
  if Source is TCustomSutraElement then
  begin
    SourceElement := TCustomSutraElement(Source);
    ElementNumber := SourceElement.ElementNumber;
  end
  else
  begin
    inherited;
  end;
end;

procedure TCustomSutraElement.SetElementNumber(const Value: integer);
begin
  SetIntegerProperty(FElementNumber, Value);
end;

{ TSutraElement2D }

procedure TSutraElement2D.Assign(Source: TPersistent);
var
  SourceElement: TSutraElement2D;
begin
  if Source is TSutraElement2D then
  begin
    SourceElement := TSutraElement2D(Source);
    Nodes := SourceElement.Nodes;
  end;
  inherited;
end;

procedure TSutraElement2D.AssignIElement(Source: IElement);
var
  NodeIndex: Integer;
  NodeItem: TSutraNodeNumber2D_Item;
  Node: INode;
begin
  Nodes.Clear;
  Nodes.Capacity := Source.NodeCount;
  for NodeIndex := 0 to Source.NodeCount - 1 do
  begin
    NodeItem := Nodes.Add;
    Node := Source.Nodes[NodeIndex];
    NodeItem.NodeNumber := Node.NodeNumber;
  end;
  ElementNumber := Source.ElementNumber;
end;

function TSutraElement2D.Center: TPoint2D;
var
  NodeIndex: Integer;
  Node: TSutraNode2D;
begin
  Result.x := 0;
  Result.y := 0;
  for NodeIndex := 0 to Nodes.Count - 1 do
  begin
    Node := Nodes[NodeIndex].Node;
    Result.x := Result.x + Node.X;
    Result.y := Result.y + Node.y;
  end;
  Result.x := Result.x/Nodes.Count;
  Result.y := Result.y/Nodes.Count;
end;

constructor TSutraElement2D.Create(Collection: TCollection);
begin
  inherited;
  FNodes := TSutraNodeNumber2D_Collection.Create(Model, self);
end;

destructor TSutraElement2D.Destroy;
begin
  FNodes.Free;
  inherited;
end;

procedure TSutraElement2D.DrawTop(const BitMap: TBitmap32;
  const ZoomBox: TQRbwZoomBox2; DrawingChoice: TDrawingChoice);
var
  Node1: TSutraNode2D;
  NodeIndex: Integer;
  Node2: TSutraNode2D;
  Points: array of TPoint;
  SumX: integer;
  SumY: Integer;
  NumString: string;
  Extent: TSize;
begin
  case DrawingChoice of
    dcAll:
      begin
        SetLength(Points, 5);
        SumX := 0;
        SumY := 0;
        for NodeIndex := 0 to Nodes.Count - 1 do
        begin
          Node2 := Nodes[NodeIndex].Node;
          Points[NodeIndex] := ConvertTop2D_Point(ZoomBox, Node2.Location);
          SumX := SumX + Points[NodeIndex].X;
          SumY := SumY + Points[NodeIndex].Y;
        end;
        Points[4] := Points[0];
        DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
          Points, True);
        SumX := SumX div 4;
        SumY := SumY div 4;
        NumString := InttoStr(ElementNumber);
        Extent := BitMap.TextExtent(NumString);
        BitMap.Textout(SumX - (Extent.cx div 2),
          SumY - (Extent.cy div 2), NumString);
      end;
    dcEdge:
      begin
        SetLength(Points, 2);
        Node1 := Nodes[3].Node;
        for NodeIndex := 0 to Nodes.Count - 1 do
        begin
          Node2 := Nodes[NodeIndex].Node;
          if Node1.EdgeNode and Node2.EdgeNode then
          begin
            Points[0] := ConvertTop2D_Point(ZoomBox, Node1.Location);
            Points[1] := ConvertTop2D_Point(ZoomBox, Node2.Location);
            DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
              Points, True);
          end;
          Node1 := Node2;
        end;
      end;
    else
      Assert(False);
  end;
end;

function TSutraElement2D.Intersection(const Input: TSegment2D;
  out IntersectingSegment: TSegment2D): boolean;
var
  Quad: TQuadix2D;
  NodeIndex: Integer;
begin
  for NodeIndex := 0 to Nodes.Count - 1 do
  begin
    Quad[NodeIndex+1] := Nodes[NodeIndex].Node.Location;
  end;
  result := Clip(Input, Quad, IntersectingSegment);
end;

function TSutraElement2D.IsInside(APoint: TPoint2D): Boolean;
var
  NodeIndex: Integer;
  PriorNode: TSutraNode2D;
  Node: TSutraNode2D;
begin
  result := false;
  PriorNode := Nodes[Nodes.Count - 1].Node;
  for NodeIndex := 0 to Nodes.Count - 1 do
  begin
    Node := Nodes[NodeIndex].Node;;
    if ((APoint.Y <= Node.Y) = (APoint.Y > PriorNode.Y)) and
      (APoint.X - Node.X - (APoint.Y - Node.Y) *
      (PriorNode.X - Node.X) /
      (PriorNode.Y - Node.Y) < 0) then
    begin
      result := not result;
    end;
    PriorNode := Node;
  end;
end;

procedure TSutraElement2D.SetNodes(
  const Value: TSutraNodeNumber2D_Collection);
begin
  FNodes.Assign(Value);
end;

{ TSutraElement2D_Collection }

function TSutraElement2D_Collection.Add: TSutraElement2D;
begin
  result := inherited Add as TSutraElement2D;
end;

constructor TSutraElement2D_Collection.Create(Model: TBaseModel);
begin
  inherited Create(TSutraElement2D, Model);
end;

function TSutraElement2D_Collection.GetItems(Index: integer): TSutraElement2D;
begin
  result := inherited Items[Index] as TSutraElement2D
end;

function TSutraElement2D_Collection.TopContainingElement(
  APoint: TPoint2D): T2DTopCell;
var
  index: Integer;
  Element: TSutraElement2D;
begin
  { TODO -cSUTRA : For now, use a simple, inefficient solution. }
  Result.Row := -1;
  Result.Col := -1;
  for index := 0 to Count - 1 do
  begin
    Element := Items[index];
    if Element.IsInside(APoint) then
    begin
      Result.Row := 0;
      Result.Col := Element.ElementNumber;
      Exit;
    end;
  end;
end;

{ TSutraMesh2D }

procedure TSutraMesh2D.Clear;
begin
  Nodes.Clear;
  Elements.Clear;
  DrawingChoice := dcAll;
end;

constructor TSutraMesh2D.Create(Model: TBaseModel);
begin
  inherited Create(Model);
  FNodes := TSutraNode2D_Collection.Create(Model);
  FElements := TSutraElement2D_Collection.Create(Model);
end;

destructor TSutraMesh2D.Destroy;
begin
  FElements.Free;
  FNodes.Free;
  inherited;
end;

procedure TSutraMesh2D.Draw(const BitMap: TBitmap32;
  const ViewDirection: TViewDirection);
begin
  case ViewDirection of
    vdTop:
      begin
        DrawTop(BitMap, frmGoPhast.frameTopView.ZoomBox);
      end;
    vdFront: ;
    vdSide: ;
    else Assert(False);
  end;
end;

procedure TSutraMesh2D.DrawTop(const BitMap: TBitmap32;
  const ZoomBox: TQRbwZoomBox2);
var
  ElementIndex: Integer;
  NodeIndex: Integer;
  ANode: TSutraNode2D;
  APoint: TPoint;
begin
  for ElementIndex := 0 to Elements.Count - 1 do
  begin
    Elements[ElementIndex].DrawTop(BitMap, ZoomBox, DrawingChoice);
  end;
  for NodeIndex := 0 to Nodes.Count - 1 do
  begin
    ANode := Nodes[NodeIndex];
    ANode.DrawTop(BitMap, ZoomBox, DrawingChoice);
  end;
  for NodeIndex := 0 to Nodes.Count - 1 do
  begin
    ANode := Nodes[NodeIndex];
    APoint := ConvertTop2D_Point(ZoomBox, ANode.Location);
    BitMap.Textout(APoint.X, APoint.Y, IntToStr(ANode.number));
  end;
end;

function TSutraMesh2D.MeshBox: TPolygon2D;
var
  Limits: TGridLimit;
begin
  Limits := MeshLimits;
  SetLength(result, 4);
  result[0].x := Limits.MinX;
  result[0].y := Limits.MinY;

  result[1].x := Limits.MinX;
  result[1].y := Limits.MaxY;

  result[2].x := Limits.MaxX;
  result[2].y := Limits.MaxY;

  result[3].x := Limits.MaxX;
  result[3].y := Limits.MinY;
end;

function TSutraMesh2D.MeshLimits: TGridLimit;
var
  Node: TSutraNode2D;
  NodeIndex: Integer;
begin
  result.MinX := 0;
  result.MaxX := 0;
  result.MinY := 0;
  result.MaxY := 0;
  result.MinZ := 0;
  result.MaxZ := 0;
  if Nodes.Count > 0 then
  begin
    Node := Nodes[0];
    result.MinX := Node.X;
    result.MinY := Node.Y;
    result.MaxX := Node.X;
    result.MaxY := Node.Y;
    for NodeIndex := 1 to Nodes.Count - 1 do
    begin
      Node := Nodes[NodeIndex];
      if Node.NodeType = ntEdge then
      begin
        if Node.X > result.MaxX then
        begin
          result.MaxX := Node.X
        end;
        if Node.X < result.MinX then
        begin
          result.MinX := Node.X
        end;
        if Node.Y > result.MaxY then
        begin
          result.MaxY := Node.Y
        end;
        if Node.Y < result.MinY then
        begin
          result.MinY := Node.Y
        end;
      end;
    end;
  end;

end;

function TSutraMesh2D.MeshOutline: TPolygon2D;
var
  FirstNode: TSutraNode2D;
  NodeIndex: Integer;
  OutlineCount: Integer;
  NextNode: TSutraNode2D;
  PriorNodes: TSutraNode2D_List;
  function GetNextOutlineNode(ANode: TSutraNode2D): TSutraNode2D;
  var
    ElementIndex: Integer;
    AnElement: TSutraElement2D;
    AnotherNode: TSutraNode2D;
    NodePosition: Integer;
  begin
    result := nil;
    for ElementIndex := 0 to ANode.FElements.Count - 1 do
    begin
      AnElement := ANode.FElements[ElementIndex];
      NodePosition := AnElement.Nodes.IndexOfNode(ANode);
      if NodePosition = AnElement.Nodes.Count -1 then
      begin
        AnotherNode := AnElement.Nodes[0].Node;
      end
      else
      begin
        AnotherNode := AnElement.Nodes[NodePosition+1].Node;
      end;
      if (AnotherNode.NodeType = ntEdge) and (AnotherNode <> ANode)
        and (AnotherNode <> FirstNode)
        and (PriorNodes.IndexOf(AnotherNode) < 0) then
      begin
        result := AnotherNode;
        Exit;
      end;

      if NodePosition = 0 then
      begin
        AnotherNode := AnElement.Nodes[AnElement.Nodes.Count -1].Node;
      end
      else
      begin
        AnotherNode := AnElement.Nodes[NodePosition-1].Node;
      end;
      if (AnotherNode.NodeType = ntEdge) and (AnotherNode <> ANode)
        and (AnotherNode <> FirstNode)
        and (PriorNodes.IndexOf(AnotherNode) < 0) then
      begin
        result := AnotherNode;
        Exit;
      end;
    end;
  end;
begin
  SetLength(result, Nodes.Count);
  if Nodes.Count > 0 then
  begin
    PriorNodes := TSutraNode2D_List.Create;
    try

      FirstNode := nil;
      for NodeIndex := 0 to Nodes.Count - 1 do
      begin
        if Nodes[NodeIndex].NodeType = ntEdge then
        begin
          FirstNode := Nodes[NodeIndex];
          break;
        end;
      end;
      Assert(FirstNode <> nil);
      result[0] := FirstNode.Location;
      OutlineCount := 1;
      NextNode := GetNextOutlineNode(FirstNode);
      PriorNodes.Add(NextNode);
      while NextNode <> nil do
      begin
        Assert(OutlineCount < Nodes.Count);
        result[OutlineCount] := NextNode.Location;
        NextNode := GetNextOutlineNode(NextNode);
        PriorNodes.Add(NextNode);
        if PriorNodes.Count > 4 then
        begin
          PriorNodes.Delete(0);
        end;
        Inc(OutlineCount);
      end;
      SetLength(result, OutlineCount);
    finally
      PriorNodes.Free;
    end;
  end;
end;

procedure TSutraMesh2D.SetDrawingChoice(const Value: TDrawingChoice);
begin
  if FDrawingChoice <> Value then
  begin
    FDrawingChoice := Value;
    InvalidateModel;
  end;
end;

procedure TSutraMesh2D.SetElements(const Value: TSutraElement2D_Collection);
begin
  FElements.Assign(Value);
end;

procedure TSutraMesh2D.SetNodes(const Value: TSutraNode2D_Collection);
begin
  FNodes.Assign(Value);
end;

function TSutraMesh2D.TopContainingCell(APoint: TPoint2D;
  const EvaluatedAt: TEvaluatedAt): T2DTopCell;
begin
  { TODO -cSUTRA : For now, use a simple, inefficient solution. }
  case EvaluatedAt of
    eaBlocks:
      begin
        result := Elements.TopContainingElement(APoint)
      end;
    eaNodes:
      begin
        result := Nodes.TopContainingCell(APoint)
      end;
    else Assert(False);
  end;
end;

{ TSutraNode3D }

procedure TSutraNode3D.Assign(Source: TPersistent);
var
  SourceNode: TSutraNode3D;
begin
  if Source is TSutraNode3D then
  begin
    SourceNode := TSutraNode3D(Source);
    Z := SourceNode.Z;
    Node2D_Number := SourceNode.Node2D_Number;
  end;
  inherited;
end;

procedure TSutraNode3D.AssignINode(Source: INode);
begin
  Number := Source.NodeNumber;
end;

function TSutraNode3D.GetNode2D: TSutraNode2D;
begin
  if FNode2D = nil then
  begin
    UpdateNode2D
  end;
  Result := FNode2D;
end;

function TSutraNode3D.GetNode2D_Number: integer;
begin
  if FNode2D <> nil then
  begin
    result := FNode2D.Number;
  end
  else
  begin
    result := FNode2D_Number;
  end;
end;

procedure TSutraNode3D.SetNode2D_Number(const Value: integer);
begin
  SetIntegerProperty(FNode2D_Number, Value);
  UpdateNode2D;
end;

procedure TSutraNode3D.SetZ(const Value: FastGEO.TFloat);
begin
  SetRealProperty(FZ, Value);
end;

procedure TSutraNode3D.UpdateNode2D;
var
  LocalModel: TCustomModel;
  Mesh: TSutraMesh2D;
begin
  LocalModel := Model as TCustomModel;
  Mesh := LocalModel.SutraMesh.Mesh2D;
  if (FNode2D_Number >= 0) and (FNode2D_Number < Mesh.Nodes.Count) then
  begin
    FNode2D := Mesh.Nodes[FNode2D_Number];
  end
  else
  begin
    FNode2D := nil;
  end;
end;

{ TSutraNode3D_Collection }

function TSutraNode3D_Collection.Add: TSutraNode3D;
begin
  result := inherited Add as TSutraNode3D
end;

constructor TSutraNode3D_Collection.Create(Model: TBaseModel);
begin
  inherited Create(TSutraNode3D, Model);
end;

function TSutraNode3D_Collection.GetItem(Index: integer): TSutraNode3D;
begin
  result := inherited Items[index] as TSutraNode3D
end;

{ TSutraNodeNumber3D_Collection }

function TSutraNodeNumber3D_Collection.Add: TSutraNodeNumber3D_Item;
begin
  result := inherited Add as TSutraNodeNumber3D_Item
end;

constructor TSutraNodeNumber3D_Collection.Create(Model: TBaseModel);
begin
  inherited Create(TSutraNodeNumber3D_Item, Model);
end;

function TSutraNodeNumber3D_Collection.GetItem(
  Index: integer): TSutraNodeNumber3D_Item;
begin
  result := inherited Items[Index] as TSutraNodeNumber3D_Item
end;

{ TSutraElement3D }

procedure TSutraElement3D.Assign(Source: TPersistent);
var
  SourceElement: TSutraElement3D;
begin
  if Source is TSutraElement3D then
  begin
    SourceElement := TSutraElement3D(Source);
    Nodes := SourceElement.Nodes;
  end;
  inherited;
end;

procedure TSutraElement3D.AssignIElement(Source: IElement);
var
  NodeIndex: Integer;
  NodeItem: TSutraNodeNumber3D_Item;
  Node: INode;
begin
  Nodes.Clear;
  Nodes.Capacity := Source.NodeCount;
  for NodeIndex := 0 to Source.NodeCount - 1 do
  begin
    NodeItem := Nodes.Add;
    Node := Source.Nodes[NodeIndex];
    NodeItem.NodeNumber := Node.NodeNumber;
  end;
  ElementNumber := Source.ElementNumber;
end;

constructor TSutraElement3D.Create(Collection: TCollection);
begin
  inherited;
  FNodes := TSutraNodeNumber3D_Collection.Create(Model);
end;

destructor TSutraElement3D.Destroy;
begin
  FNodes.Free;
  inherited;
end;

procedure TSutraElement3D.SetNodes(const Value: TSutraNodeNumber3D_Collection);
begin
  FNodes.Assign(Value);
end;

{ TSutraMesh3D }

procedure TSutraMesh3D.Clear;
begin
  Elements.Clear;
  Nodes.Clear;
  Mesh2D.Clear;
end;

constructor TSutraMesh3D.Create(Model: TBaseModel);
begin
  inherited Create(Model);
  FElements := TSutraElement3D_Collection.Create(Model);
  FNodes := TSutraNode3D_Collection.Create(Model);
  FMesh2D := TSutraMesh2D.Create(Model);
end;

destructor TSutraMesh3D.Destroy;
begin
  FMesh2D.Free;
  FNodes.Free;
  FElements.Free;
  inherited;
end;

procedure TSutraMesh3D.Draw(const BitMap: TBitmap32;
  const ViewDirection: TViewDirection);
begin
  Mesh2D.Draw(BitMap, ViewDirection);
end;

function TSutraMesh3D.MeshLimits(ViewDirection: TViewDirection): TGridLimit;
begin
  case ViewDirection of
    vdTop: result := Mesh2D.MeshLimits;
    vdFront: Assert(False);
    vdSide: Assert(False);
    else  Assert(False);
  end;
end;

function TSutraMesh3D.MeshBox(ViewDirection: TViewDirection): TPolygon2D;
begin
  case ViewDirection of
    vdTop: result := Mesh2D.MeshBox;
    vdFront: Assert(False);
    vdSide: Assert(False);
    else  Assert(False);
  end;
end;

procedure TSutraMesh3D.SetDrawingChoice(const Value: TDrawingChoice);
begin
  FDrawingChoice := Value;
  Mesh2D.DrawingChoice := Value;
end;

procedure TSutraMesh3D.SetElements(const Value: TSutraElement3D_Collection);
begin
  FElements.Assign(Value);
end;

procedure TSutraMesh3D.SetMesh2D(const Value: TSutraMesh2D);
begin
  FMesh2D.Assign(Value);
end;

procedure TSutraMesh3D.SetNodes(const Value: TSutraNode3D_Collection);
begin
  FNodes.Assign(Value);
end;

function TSutraMesh3D.TopContainingCell(APoint: TPoint2D;
  const EvaluatedAt: TEvaluatedAt): T2DTopCell;
begin
  result := Mesh2D.TopContainingCell(APoint, EvaluatedAt)
end;

{ TSutraNodeNumber3D_Item }

function TSutraNodeNumber3D_Item.GetNode: TSutraNode3D;
begin
  result := inherited Node as TSutraNode3D;
end;

procedure TSutraNodeNumber3D_Item.SetNode(const Value: TSutraNode3D);
begin
  inherited SetNode(Value);
end;

procedure TSutraNodeNumber3D_Item.UpdateNode;
var
  LocalModel: TCustomModel;
  Mesh: TSutraMesh3D;
begin
  LocalModel := Model as TCustomModel;
  Mesh := LocalModel.SutraMesh;
  if (FStoredNodeNumber >= 0) and (FStoredNodeNumber < Mesh.Nodes.Count) then
  begin
    FNode := Mesh.Nodes[FStoredNodeNumber];
  end
  else
  begin
    FNode := nil;
  end;
end;

{ TSutraElement3D_Collection }

function TSutraElement3D_Collection.Add: TSutraElement3D;
begin
  result := inherited Add as TSutraElement3D;
end;

constructor TSutraElement3D_Collection.Create(Model: TBaseModel);
begin
  inherited Create(TSutraElement3D, Model);
end;

function TSutraElement3D_Collection.GetItems(Index: integer): TSutraElement3D;
begin
  result := inherited Items[index] as TSutraElement3D;
end;

{ TCustomSutraMesh }

procedure TCustomSutraMesh.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCustomSutraMesh.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    frmGoPhast.EnableVisualization;
    frmGoPhast.PhastModel.InvalidateSegments
  end;
end;

//{ TCustomSutraNodeCollection }
//
//function TCustomSutraNodeCollection.Add: TSutraNode2D;
//var
//  Mesh: TSutraMesh3D;
//begin
//  Mesh := (Model as TCustomModel).SutraMesh;
//  Mesh.BeginUpdate;
//  try
//    result := inherited Add as TSutraNode2D;
//  finally
//    Mesh.EndUpdate
//  end;
//end;

{ TCustomSutraItem }

procedure TCustomSutraItem.BeginUpdate;
begin
  inherited;
  Collection.BeginUpdate;
end;

procedure TCustomSutraItem.EndUpdate;
begin
  Collection.EndUpdate;
  inherited;
end;

{ TCustomSutraCollection }

procedure TCustomSutraCollection.BeginUpdate;
var
  Mesh: TSutraMesh3D;
begin
  inherited;
  Mesh := (Model as TCustomModel).SutraMesh;
  Mesh.BeginUpdate;
end;

procedure TCustomSutraCollection.EndUpdate;
var
  Mesh: TSutraMesh3D;
begin
  inherited;
  Mesh := (Model as TCustomModel).SutraMesh;
  Mesh.EndUpdate;
end;

end.
