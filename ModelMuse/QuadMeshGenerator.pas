{ @name is used to generate a quadrilateral mesh using the algorithm described
  in
  Sarrate, Josep, and Huerta, Antonio, 2000. Efficient unstructured
  quadrilateral mesh generation. International Journal of Numerical Methods
  in Engineering. 49:1327-1350.

  as modified in
  Bastian, M., and Li, B.Q., 2003. An efficient automatic mesh generator for
  quadrilateral elements implemented using C++. Finite Elements in Analysis
  and Design 39 (2003) 905–930.

  Other References:
  Zhu, J.Z., Zienkiewciz, O.C. Hinton, E. and Wu, J. 1991. A new approach to
  the development of automatic quadrilateral mesh generation. International
  Journal for Numerical Methods in Engineering. Vol: 849-866.

  Giuliani, S. 1982. An algorithm for continuous rezoning of the hydrodynamic
  grid in arbitrary Lagrangian-Eulerian computer codes. Nuclear Engineering
  and Design. 72: 205-212
}

unit QuadMeshGenerator;

interface

uses
  Windows, Generics.Collections, Generics.Defaults, FastGEO, SysUtils,
  IntListUnit, MeshRenumbering;

Type
  TSegmentType = (stInner, stEdge, stSubDomain);
  TSixNodeClosureMethod = (cmModifiedCost, cmTemplate);
  TNodeAdjustmentMethod = (namLagrange, namGiuliani, namSarrateHuerta);

  EInvalidElement = class(Exception);

  TQuadMeshCreator = class;

  TNodeInBoundary = class;
  TNodeInBoundaryList = class(TList<TNodeInBoundary>);

  TBoundary = class;
  TBoundaryList = class(TList<TBoundary>);

  TSegment = class;
  TSegmentList = class(TList<TSegment>);

  TNode = class(TInterfacedObject, INode)
  private
    FLocation: TPoint2D;
    FNodeType: TNodeType;
    FElements: TBoundaryList;
    FSegments: TSegmentList;
    FQuadMeshCreator: TQuadMeshCreator;
    FNodeNumber: Integer;
    FDesiredSpacing: double;
    FDesiredElementCount: Integer;
    // Move the node to the centroid of the elements
    // of which the node is a part.
    procedure AdjustPositionLagrange;
    procedure AdjustPositionGiuliani;
    function ImproveTopology1: boolean;
    function ImproveTopology2: boolean;
    procedure NodeElimination;
    function ElementElimination: boolean;
    function SideElimination: boolean;
    function DiagonalSwapping: boolean;
    procedure ReplaceNodeInElement(ReplacementNode: TNode;
      AnElement: TBoundary);
    function GetActiveElementCount: Integer;
    function GetActiveElement(Index: Integer): IElement;
    function GetNodeNumber: Integer;
    procedure SetNodeNumber(Value: Integer);
    function GetLocation: TPoint2D;
    procedure SetLocation(const Value: TPoint2D);
    function GetNodeType: TNodeType;
  public
    constructor Create(QuadMeshCreator: TQuadMeshCreator;
      DesiredSpacing: double);
    destructor Destroy; override;
    property DesiredSpacing: double read FDesiredSpacing;
    property Location: TPoint2D read GetLocation write SetLocation;
    property X: double read FLocation.X write FLocation.X;
    property Y: double read FLocation.Y write FLocation.Y;
    property NodeType: TNodeType read GetNodeType;
    property ElementCount: Integer read GetActiveElementCount;
    property DesiredElementCount: Integer read FDesiredElementCount;
    property Elements[Index: Integer]: IElement read GetActiveElement;
    property NodeNumber: Integer read GetNodeNumber write SetNodeNumber;
  end;

  TNodeList = class(TList<TNode>);

  INodeComparer = IComparer<TNode>;

  TNodeAngleComparer = class(TComparer<TNode>)
  private
    FCenterNode: TNode;
  public
    function Compare(const Left, Right: TNode): Integer; override;
    constructor Create(CenterNode: TNode);
  end;

  TNodeSpacingComparer = class(TComparer<TNode>)
  public
    function Compare(const Left, Right: TNode): Integer; override;
  end;

  TSegmentObjectList = class(TObjectList<TSegment>);

  { @name is a line segment connecting two @link(TNode)s. Additional
    @link(TNode)s may be inserted along its length.
  }
  TSegment = class(TObject)
  private
    // First endpoint @link(TNode) of the segment
    FNode1: TNode;
    // Second endpoint @link(TNode) of the segment
    FNode2: TNode;
    // @link(TNode)s inserted along length of @classname
    FInnerNodes: TNodeList;
    FSegmentType: TSegmentType;
    FBoundary: TBoundary;
    FQuadMeshCreator: TQuadMeshCreator;
    // @name returns the number of @link(TNode)s that ideally would be inserted
    // along this @classname. The actual number inserted might be larger
    // in order to ensure that the number of @link(TNode)s around a boundary
    // is even.
    function NodesToInsert: Integer;
    // @name reverses the direction of this @classname including inserted nodes.
    procedure Reverse;
    // @name creates a @classname and inserts itself on the
    // @link(TNode.FSegments) of @link(Node1) and @link(Node2).
    constructor Create(Node1, Node2: TNode; SegmentType: TSegmentType;
      Boundary: TBoundary; QuadMeshCreator: TQuadMeshCreator);
    property Node1: TNode read FNode1;
    property Node2: TNode read FNode2;
    property SegmentType: TSegmentType read FSegmentType;
    // @name splits this @classname into two parts at ANode. ANode must be in
    // @link(FInnerNodes).
    function Split(ANode: TNode): TSegmentList;
    // @name creates a new @classname whose orientation is opposite this one.
    function CreateReversedSegment: TSegment;
    function Length: double;
    // @name creates evenly spaced new nodes and inserts them in
    // @link(FInnerNodes).
    procedure InsertNodes(NumberToInsert: Integer);
  public
    destructor Destroy; override;
  end;

  TNodeInBoundary = class(TObject)
  private
    FNode: TNode;
    FPosition: Integer;
    FBoundary: TBoundary;
    FSegments: TSegmentList;
    function OnSameSegment(ANode: TNodeInBoundary): boolean;
    function GetDesiredSpacing: double;
    function GetX: double;
    function GetY: double;
    function GetNodeType: TNodeType;
    function GetElementCount: Integer;
    function GetLocation: TPoint2D;
    property Position: Integer read FPosition;
    property DesiredSpacing: double read GetDesiredSpacing;
    Constructor Create(Node: TNode; Boundary: TBoundary; Segment: TSegment);
  public
    destructor Destroy; override;
    property X: double read GetX;
    property Y: double read GetY;
    property NodeType: TNodeType read GetNodeType;
    property ElementCount: Integer read GetElementCount;
    property Location: TPoint2D read GetLocation;
  end;

  TNodeInBoundaryObjectList = TObjectList<TNodeInBoundary>;

  TCost = class(TObject)
  private
    FTheta1: TFloat;
    FTheta2: TFloat;
    FPhi: double;
    FNodeDistance: TFloat;
    FNStar: double;
    F_l: double;
    FGamma: double;
    FNode1: TNodeInBoundary;
    FNode2: TNodeInBoundary;
    FQuadMeshCreator: TQuadMeshCreator;
    FVisible: boolean;
    FCost: double;
    procedure ComputeVisibility;
    procedure GetNeighborNodes(var PriorNode1, SubsequentNode1, PriorNode2,
      SubsequentNode2: TNodeInBoundary);
    procedure ComputeCost;
    Constructor Create(Node1, Node2: TNodeInBoundary;
      QuadMeshCreator: TQuadMeshCreator);
    property Visible: boolean read FVisible;
    property Cost: double read FCost;
  end;

  TNodeObjectList = class(TObjectList<TNode>);
  TCostObjectList = class(TObjectList<TCost>);
  TCost2ObjectList = class(TObjectList<TCostObjectList>);
  TCost3ObjectList = class(TObjectList<TCost2ObjectList>);

  TBoundaryObjectList = class(TObjectList<TBoundary>);

  // @name represents a series of @link(TNode)s used to generate the mesh.
  // The nodes are listed in order in @link(FNodes). The boundary may
  // be either open or closed.  Closed boundaries have the same node at the
  // beginning and end of @link(FNodes).
  TBoundary = class(TNodeInBoundaryList, IElement)
  private
    FQuadMeshCreator: TQuadMeshCreator;
    FNodes: TNodeList;
    FSegments: TSegmentObjectList;
    FSegmentType: TSegmentType;
    FParent: TBoundary;
    FSubParts: TBoundaryObjectList;
    FDesiredSpacing: double;
    FConverted: boolean;
    FElementNumber: Integer;
    FRefCount: Integer;
    function GetSubPart(Index: Integer): TBoundary;
    // @name inserts an even number of @link(TNode)s around the @classname
    procedure InsertNodesAlongBoundary;
    procedure ConvertToClosedBoundary;
    procedure RenumberNodes;
    procedure SetNodeTypes(NodeType: TNodeType);
    // @name creates @link(TNodeInBoundary TNodeInBoundaries) along it's edge.
    procedure CreateBoundaryNodes;
    procedure SplitSegmentAtNode(ANode: TNodeInBoundary);
    function SpecialCase: boolean;
    procedure Split222(FirstIndex: Integer);
    procedure Split312(FirstIndex: Integer);
    procedure Split411(FirstIndex: Integer);
    procedure Split2121(FirstIndex: Integer);
    procedure RemoveSelfFromAllNodes;
    procedure ReverseSubBoundaries;
    function Orientation: Integer;
    function IndexOfNode(ANode: TNode): Integer;
    procedure RemoveSelfFromOwnNodes;
    function GetSubPartCount: Integer;
    procedure SetCounterClockwiseOrientation;
    procedure AssignOriginalEdgeAngles;
    procedure GenerateSegments(DesiredOrientation: Integer);
    Procedure Split(List: TBoundaryList);
    constructor Create(QuadMeshCreator: TQuadMeshCreator;
      SegmentType: TSegmentType; Parent: TBoundary; DesiredSpacing: double);
    function GetActiveNodeCount: Integer;
    property SegmentType: TSegmentType read FSegmentType;
    // The spacing between neighboring nodes in the final mesh
    // should be no greater than @name.
    property DesiredSpacing: double read FDesiredSpacing;
    property SubPartCount: Integer read GetSubPartCount;
    property SubParts[Index: Integer]: TBoundary read GetSubPart;
    procedure CheckInvalidElement;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetActiveNode(Index: Integer): INode;
    function GetElementNumber: Integer;
    procedure SetElementNumber(Value: Integer);
  public
    destructor Destroy; override;
    procedure AddNode(Node: TNode);
    property Nodes[Index: Integer]: INode read GetActiveNode;
    property ElementNumber: Integer read GetElementNumber
      write SetElementNumber;
    property NodeCount: Integer read GetActiveNodeCount;
  end;

  TQuadMeshCreator = class(TInterfacedObject, IMesh)
  private
    FMinX: double;
    FMinY: double;
    FMaxX: double;
    FMaxY: double;
    // @name is the length of the diagonal of a rectangular box around the
    // the nodes making up the boundary.
    FCharacteristicLength: double;
    FBoundaries: TBoundaryObjectList;
    FCostsArray: TCost3ObjectList;
    FSixNodeClosureMethod: TSixNodeClosureMethod;
    FNodeAdjustmentMethod: TNodeAdjustmentMethod;
    FGrowthRate: double;
    FNodes: TNodeObjectList;
    FElementList: TIElementList;
    FNodeList: TINodeList;
    FBoundaryNodes: TNodeInBoundaryObjectList;
    procedure InvalidateCosts(List: TBoundary; LowestCost: TCost);
    // @name inserts nodes along boundaries to ensure that the spacing between
    // nodes is never larger than the desired spacing and that the number of
    // nodes inserted is even.
    procedure InsertNodesAlongBoundaries;
    // @name converts an open boundary to a closed one by adding the existing
    // nodes in reverse order to the boundary.
    procedure ConvertToClosedBoundaries;
    // If there is more than one boundary, @name joins them together to convert
    // them to a single boundary.
    procedure MakeSingleBoundary;
    // @name sets the @link(TNode.FNodeType) of the @link(TNode)s in the
    // outermost boundary to ntEdge and for the others to ntSubDomain.
    procedure SetNodeTypes;
    procedure AssignOriginalEdgeAngles;
    // @name computes @link(FCharacteristicLength).
    procedure ComputeCharacteristicLength;
    // @name creates @link(TSegment)s in a counterclockwise orientation
    // around the outer @link(TBoundary) and in a clockwise orientation
    // around the other boundaries.
    procedure GenerateSegments;
    // @name sets the @link(TBoundary.FSegmentType) of the outermost boundary to
    // ntEdge and for the others to ntSubDomain.
    procedure SetSegmentTypes;
    // @name creates @link(TNodeInBoundary TNodeInBoundaries)
    // along each @link(TBoundary).
    procedure CreateBoundaryNodes;
    function GetBoundaryCount: Integer;
    function GetCost(Node1, Node2: TNodeInBoundary): TCost;
    procedure SetSixNodeClosureMethod(const Value: TSixNodeClosureMethod);
    procedure SetNodeAdjustmentMethod(const Value: TNodeAdjustmentMethod);
    procedure AdjustPositionLagrange;
    procedure AdjustPositionGiuliani;
    procedure AssignDesiredSpacings;
    function GetActiveNodeCount: Integer;
    function GetNodeObject(Index: Integer): TNode;
    procedure RenumberNodes;
    function GetActiveElementCount: Integer;
    function GetActiveElement(Index: Integer): IElement;
    function GetActiveNode(Index: Integer): INode;
    property Cost[Node1, Node2: TNodeInBoundary]: TCost read GetCost;
    property BoundaryCount: Integer read GetBoundaryCount;
    // property Boundaries[index: integer]: TBoundary read GetBoundary;
    property NodeObjects[Index: Integer]: TNode read GetNodeObject;
    function ImproveTopology: boolean;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure CheckInvalidElements;
  public
    Constructor Create;
    destructor Destroy; override;
    procedure GenerateMesh;
    procedure AdjustNodes;
    function AddBoundary(DesiredSpacing: double): TBoundary;
    property SixNodeClosureMethod: TSixNodeClosureMethod
      read FSixNodeClosureMethod write SetSixNodeClosureMethod;
    property GrowthRate: double read FGrowthRate write FGrowthRate;
    property NodeAdjustmentMethod: TNodeAdjustmentMethod
      read FNodeAdjustmentMethod write SetNodeAdjustmentMethod;
    property NodeCount: Integer read GetActiveNodeCount;
    property ElementCount: Integer read GetActiveElementCount;
    property Elements[Index: Integer]: IElement read GetActiveElement;
    property Nodes[Index: Integer]: INode read GetActiveNode;
  end;

function VertexAngleRadians(x1, y1, x2, y2, x3, y3: TFloat): TFloat; overload;
function VertexAngleRadians(const Point1, Point2, Point3: TPoint2D)
  : TFloat; overload;

var
  C1: double;
  C2: double;
  C3: double;
  C4: double;
  C5: double;
  C6: double;
  AltC1: double;
  AltC2: double;
  AltC3: double;
  AltC4: double;
  AltC5: double;
  AltC6: double;
  ElementGrowthRate: double;

procedure SetDefaults;

implementation

uses
  Math, QuadTreeClass, ConvexHullUnit, Dialogs, RealListUnit;

resourcestring
  StrOneOrMoreInvalid = 'One or more invalid elements was created. ' +
    'Adjusting the mesh generation parameters may help.';

type
  TColliniears = array [0 .. 5] of boolean;

const
  PiDiv2 = Pi / 2;
  TwoPiDiv3 = 2 * Pi / 3;
  PiDiv6 = Pi / 6;
  TwoPi = 2 * Pi;

var
  InvalidElement: boolean = False;

type
  TIElementComparer = TComparer<IElement>;
  TINodeComparer = TComparer<INode>;

procedure SetDefaults;
begin
  C1 := 0.52;
  C2 := 0.17;
  C3 := 0;
  C4 := 0.17;
  C5 := 0.14;
  C6 := 0.3;
  AltC1 := 0.56;
  AltC2 := 0.33;
  AltC3 := 0.11;
  AltC4 := 0.0;
  AltC5 := 0.0;
  AltC6 := 1.2;
  ElementGrowthRate := 1.2;
end;

function BadAngle(P1, P2, P3: TPoint2D): boolean;
var
  AnAngle: TFloat;
begin
  AnAngle := VertexAngle(P1, P2, P3);
  result := (AnAngle < 0.1) and (AnAngle > -0.1);
  if not result then
  begin
    result := (AnAngle < 180.1) and (AnAngle > 179.9);
    if not result then
    begin
      result := (AnAngle > -180.1) and (AnAngle < -179.9);
    end;
  end;
end;

function CalcNodesToInsert(NStar: double): Integer;
begin
  result := Trunc(NStar * (1 + Epsilon)) - 1;
  if not IsEqual(result, NStar - 1, Epsilon) then
  begin
    Inc(result);
  end;
end;

function VertexAngleRadians(x1, y1, x2, y2, x3, y3: TFloat): TFloat;
var
  Dist: TFloat;
  InputTerm: TFloat;
begin
  (*
    Using the cosine identity:
    cosA = (b^2 + c^2 - a^2) / (2*b*c)
    A    = Cos'((b^2 + c^2 - a^2) / (2*b*c))

    Where:

    a,b and c : are edges in the triangle
    A         : is the angle at the vertex opposite edge 'a'
    aka the edge defined by the vertex <x1y1-x2y2-x3y3>

  *)
  (* Quantify coordinates *)
  x1 := x1 - x2;
  x3 := x3 - x2;
  y1 := y1 - y2;
  y3 := y3 - y2;

  (* Calculate Ley Distance *)
  Dist := (x1 * x1 + y1 * y1) * (x3 * x3 + y3 * y3);

  if IsEqual(Dist, Zero) then
    result := Zero
  else
  begin
    InputTerm := (x1 * x3 + y1 * y3) / sqrt(Dist);
    if IsEqual(InputTerm, 1.0) then
      result := Zero
    else if IsEqual(InputTerm, -1.0) then
      result := Pi
    else
      result := ArcCos(InputTerm)
  end;
end;

function VertexAngleRadians(const Point1, Point2, Point3: TPoint2D): TFloat;
begin
  result := VertexAngleRadians(Point1.X, Point1.Y, Point2.X, Point2.Y, Point3.X,
    Point3.Y);
  if Orientation(Point1, Point2, Point3) = Clockwise then
  begin
    result := 2 * Pi - result;
  end;
end;

procedure TCost.ComputeVisibility;
var
  BoundaryIndex: Integer;
  ABoundary: TBoundary;
  PriorNode1: TNodeInBoundary;
  SubsequentNode1: TNodeInBoundary;
  PriorNode2: TNodeInBoundary;
  SubsequentNode2: TNodeInBoundary;
  NodeIndex: Integer;
  Node1: TNodeInBoundary;
  Node2: TNodeInBoundary;
  Angle1: TFloat;
  Angle2: TFloat;
  Segment: TSegment2D;
  APoint: TPoint2D;
  procedure Compute(ABoundary: TBoundary);
  var
    NodeIndex: Integer;
    NodeA: TNode;
    NodeB: TNode;
    SubIndex: Integer;
    SubBoundary: TBoundary;
  begin
    for NodeIndex := 0 to ABoundary.Count - 2 do
    begin
      NodeA := ABoundary[NodeIndex].FNode;
      NodeB := ABoundary[NodeIndex + 1].FNode;
      if (NodeA <> FNode1.FNode) and (NodeA <> FNode2.FNode) and
        (NodeB <> FNode1.FNode) and (NodeB <> FNode2.FNode) then
      begin
        if Intersect(FNode1.FNode.Location, FNode2.FNode.Location,
          NodeA.Location, NodeB.Location) then
        begin
          FVisible := False;
          Exit;
        end;
        APoint := ClosestPointOnSegmentFromPoint(Segment, NodeA.Location);
        if IsEqual(NodeA.Location, APoint) then
        begin
          FVisible := False;
          Exit;
        end;
        APoint := ClosestPointOnSegmentFromPoint(Segment, NodeB.Location);
        if IsEqual(NodeB.Location, APoint) then
        begin
          FVisible := False;
          Exit;
        end;
      end;
    end;
    for SubIndex := 0 to ABoundary.FSubParts.Count - 1 do
    begin
      SubBoundary := ABoundary.FSubParts[SubIndex];
      Compute(SubBoundary);
      if not FVisible then
      begin
        Exit;
      end;
    end;
  end;

begin
  GetNeighborNodes(PriorNode1, SubsequentNode1, PriorNode2, SubsequentNode2);

  if PriorNode1.FNode <> SubsequentNode1.FNode then
  begin
    Angle1 := VertexAngleRadians(PriorNode1.FNode.Location,
      FNode1.FNode.Location, FNode2.FNode.Location);
    Angle2 := VertexAngleRadians(PriorNode1.FNode.Location,
      FNode1.FNode.Location, SubsequentNode1.FNode.Location);
    if Angle1 > Angle2 then
    begin
      FVisible := False;
      Exit;
    end;
  end;

  if PriorNode2.FNode <> SubsequentNode2.FNode then
  begin
    Angle1 := VertexAngleRadians(PriorNode2.FNode.Location,
      FNode2.FNode.Location, FNode1.FNode.Location);
    Angle2 := VertexAngleRadians(PriorNode2.FNode.Location,
      FNode2.FNode.Location, SubsequentNode2.FNode.Location);
    if Angle1 > Angle2 then
    begin
      FVisible := False;
      Exit;
    end;
  end;

  Segment[1] := FNode1.FNode.Location;
  Segment[2] := FNode2.FNode.Location;
  FVisible := True;
  if (FNode1.FBoundary = FNode2.FBoundary) then
  begin
    ABoundary := FNode1.FBoundary;
    for NodeIndex := 0 to ABoundary.Count - 2 do
    begin
      Node1 := ABoundary[NodeIndex];
      Node2 := ABoundary[NodeIndex + 1];
      if (FNode1.FNode = Node1.FNode) or (FNode1.FNode = Node2.FNode) or
        (FNode2.FNode = Node1.FNode) or (FNode2.FNode = Node2.FNode) then
      begin
        Continue;
      end;
      if Intersect(FNode1.FNode.Location, FNode2.FNode.Location,
        Node1.FNode.Location, Node2.FNode.Location) then
      begin
        FVisible := False;
        Exit;
      end;
      APoint := ClosestPointOnSegmentFromPoint(Segment, Node1.FNode.Location);
      if IsEqual(Node1.FNode.Location, APoint) then
      begin
        FVisible := False;
        Exit;
      end;
      APoint := ClosestPointOnSegmentFromPoint(Segment, Node2.FNode.Location);
      if IsEqual(Node2.FNode.Location, APoint) then
      begin
        FVisible := False;
        Exit;
      end;
    end;
  end
  else
  begin
    for BoundaryIndex := 0 to FQuadMeshCreator.FBoundaries.Count - 1 do
    begin
      ABoundary := FQuadMeshCreator.FBoundaries[BoundaryIndex];
      Compute(ABoundary);
      if not FVisible then
      begin
        Exit;
      end;
    end;
  end;

end;

procedure TCost.GetNeighborNodes(var PriorNode1, SubsequentNode1, PriorNode2,
  SubsequentNode2: TNodeInBoundary);
begin
  // Compute first factor: splitting angles.
  if FNode1.Position = 0 then
  begin
    Assert(FNode1.FBoundary.Items[FNode1.FBoundary.Count - 1] = FNode1);
    if FNode1.FBoundary.Count = 1 then
    begin
      PriorNode1 := FNode1.FBoundary.Items[0];
    end
    else
    begin
      PriorNode1 := FNode1.FBoundary.Items[FNode1.FBoundary.Count - 2];
    end;
  end
  else
  begin
    PriorNode1 := FNode1.FBoundary.Items[FNode1.Position - 1];
  end;
  if FNode1.FBoundary.Count = 1 then
  begin
    SubsequentNode1 := FNode1.FBoundary.Items[0];
  end
  else
  begin
    Assert(FNode1.Position < FNode1.FBoundary.Count - 1);
    SubsequentNode1 := FNode1.FBoundary.Items[FNode1.Position + 1];
  end;

  if FNode2.Position = 0 then
  begin
    Assert(FNode2.FBoundary.Items[FNode2.FBoundary.Count - 1] = FNode2);
    if FNode2.FBoundary.Count = 1 then
    begin
      PriorNode2 := FNode2.FBoundary.Items[0];
    end
    else
    begin
      PriorNode2 := FNode2.FBoundary.Items[FNode2.FBoundary.Count - 2];
    end;
  end
  else
  begin
    PriorNode2 := FNode2.FBoundary.Items[FNode2.Position - 1];
  end;
  if FNode2.FBoundary.Count = 1 then
  begin
    SubsequentNode2 := FNode2.FBoundary.Items[0];
  end
  else
  begin
    Assert(FNode2.Position < FNode2.FBoundary.Count - 1);
    SubsequentNode2 := FNode2.FBoundary.Items[FNode2.Position + 1];
  end;
end;

function ComputeGamma(NodeType: TNodeType; Theta: TFloat): double;
const
  StraightAngle = Pi * (1 - Epsilon);
begin
  if Theta > StraightAngle then
  begin
    if NodeType = ntInner then
    begin
      result := 0.5;
    end
    else
    begin
      result := 0;
    end;
  end
  else
  begin
    result := 1;
  end;
end;

{ TCost }

procedure TCost.ComputeCost;
// Constants from page 1334.
var
  NodeIndex: Integer;
  PriorNode1: TNodeInBoundary;
  SubsequentNode1: TNodeInBoundary;
  Angle1: TFloat;
  Angle2: TFloat;
  PriorNode2: TNodeInBoundary;
  SubsequentNode2: TNodeInBoundary;
  Angle3: TFloat;
  Angle4: TFloat;
  Zeta1: double;
  Zeta2: double;
  Psi: Extended;
  Zeta1TimesZeta2: double;
  Sigma1: double;
  Sigma2: double;
  Sigma: Extended;
  N: Integer;
  Eta: double;
  Polygon1: TPolygon2D;
  Polygon2: TPolygon2D;
  NNode: TNodeInBoundary;
  Boundary: TBoundary;
  NIndex: Integer;
  HigherNode: TNodeInBoundary;
  LowerNode: TNodeInBoundary;
  Area1: TFloat;
  Area2: TFloat;
  Alpha: double;
  ComputeAll: boolean;
  SixNodeCost: boolean;
  function GetSigma(ANode: TNodeInBoundary; Theta: TFloat): double;
{$IFNDEF New}
  var
    DeltaElement: Integer;
{$ENDIF}
  begin
    case ANode.FNode.FNodeType of
      ntInner:
        begin
          case FNode1.FNode.FElements.Count + 1 of
            3:
              begin
                result := 5.6;
                Exit;
              end;
            4:
              begin
                result := 4;
                Exit;
              end;
            5:
              begin
                result := 36;
                Exit;
              end;
            6:
              begin
                result := 60;
                Exit;
              end;
          else
            begin
              // result := 80;
              result := FNode1.FNode.FElements.Count * 12;
              Exit;
            end;
          end;
        end;
      ntEdge:
        begin
{$IFNDEF New}
          DeltaElement := FNode1.FNode.FDesiredElementCount -
            FNode1.FNode.FElements.Count + 1;
          if DeltaElement < 0 then
          begin
            DeltaElement := 0;
          end;
          case DeltaElement of
            0:
              begin
                result := 4;
              end;
            1:
              begin
                result := 36;
              end;
          else
            begin
              result := 100;
            end;
          end;
          Exit;
{$ELSE}
          if Theta < TwoPiDiv3 then
          begin
            result := 100;
            Assert(0 <= Theta);
            Exit;
          end
          else if (Theta <= TwoPi) then
          begin
            result := 0;
            Exit;
          end
          else
          begin
            result := 100;
            Exit;
          end;
{$ENDIF}
        end;
      ntSubDomain:
        begin
{$IFNDEF New}
          DeltaElement := FNode1.FNode.FDesiredElementCount -
            FNode1.FNode.FElements.Count + 1;
          if DeltaElement < 0 then
          begin
            DeltaElement := 0;
          end;
          case DeltaElement of
            0:
              begin
                result := 4;
              end;
            1:
              begin
                result := 36
              end;
          else
            begin
              result := 80;
            end;
          end;
          Exit;
{$ELSE}
          if Theta < TwoPiDiv3 then
          begin
            result := 80;
            Assert(0 <= Theta);
            Exit;
          end
          else if (Theta <= TwoPi) then
          begin
            result := 0;
            Exit;
          end
          else
          begin
            result := 80;
            Exit;
          end;
{$ENDIF}
        end;
    end;
    result := 100;
    Assert(False);
  end;

begin
  ComputeAll := FNodeDistance < 0;
  SixNodeCost := (FNode1.FBoundary = FNode2.FBoundary) and
    (FNode1.FBoundary.Count = 7);
  if ComputeAll or SixNodeCost then
  begin
    GetNeighborNodes(PriorNode1, SubsequentNode1, PriorNode2, SubsequentNode2);

    Angle1 := VertexAngleRadians(PriorNode1.FNode.Location,
      FNode1.FNode.Location, FNode2.FNode.Location);
    Angle2 := VertexAngleRadians(FNode2.FNode.Location, FNode1.FNode.Location,
      SubsequentNode1.FNode.Location);
    Angle3 := VertexAngleRadians(PriorNode2.FNode.Location,
      FNode2.FNode.Location, FNode1.FNode.Location);
    Angle4 := VertexAngleRadians(FNode1.FNode.Location, FNode2.FNode.Location,
      SubsequentNode2.FNode.Location);
    if SixNodeCost then
    begin
      FPhi := (Abs(Angle1 - PiDiv2) + Abs(Angle2 - PiDiv2) +
        Abs(Angle3 - PiDiv2) + Abs(Angle4 - PiDiv2)) / TwoPi;
      FTheta1 := Angle1 + Angle2;
      FTheta2 := Angle3 + Angle4;
    end
    else
    begin

      FTheta1 := Angle1 + Angle2;
      FTheta2 := Angle3 + Angle4;

      // Evaluate equation2.
      if (FTheta1 < PiDiv2) and (FTheta2 < PiDiv2) then
      begin
        FPhi := 1.0;
      end
      else
      begin
        // Equation 3
        Psi := (Abs(Angle1 - Angle2) + Abs(Angle3 - Angle4)) /
          (FTheta1 + FTheta2);
        if ((PiDiv2 <= FTheta1) and (FTheta1 <= TwoPiDiv3)) or
          ((PiDiv2 <= FTheta2) and (FTheta2 <= TwoPiDiv3)) then
        begin
          // Equation 4
          if ((PiDiv2 < FTheta1) and (FTheta1 < TwoPiDiv3)) then
          begin
            Zeta1 := (FTheta1 - TwoPiDiv3) / PiDiv6;
          end
          else
          begin
            Zeta1 := 1
          end;
          if ((PiDiv2 < FTheta2) and (FTheta2 < TwoPiDiv3)) then
          begin
            Zeta2 := (FTheta2 - TwoPiDiv3) / PiDiv6;
          end
          else
          begin
            Zeta2 := 1
          end;
          Zeta1TimesZeta2 := Zeta1 * Zeta2;
          FPhi := (1 - Zeta1TimesZeta2) + Zeta1TimesZeta2 * Psi;
        end
        else
        begin
          FPhi := Psi;
        end;
      end;
    end;
  end;

  // Compute second factor: Structuring index
  Sigma1 := GetSigma(FNode1, FTheta1);
  Sigma2 := GetSigma(FNode2, FTheta2);
  Sigma := (Sigma1 + Sigma2) / 200;

  // Compute third factor: Node placement error.
  if ComputeAll then
  begin
    FNodeDistance := Distance(FNode1.FNode.Location, FNode2.FNode.Location);
    FNStar := FNodeDistance /
      ((FNode1.DesiredSpacing + FNode2.DesiredSpacing) / 2);
  end;
  N := CalcNodesToInsert(FNStar);
  Assert(FNode1.FBoundary = FNode2.FBoundary);

  if Odd(N + Abs(FNode1.FPosition - FNode2.FPosition)) then
  begin
    Inc(N);
  end;

  // equation 6
  Eta := Abs(FNStar - N);

  // Compute fourth factor: splitting line length
  // equation 8
  if ComputeAll then
  begin
    F_l := FNodeDistance / FQuadMeshCreator.FCharacteristicLength;
  end;

  // Compute fifth factor: Symmetry.
  Boundary := FNode1.FBoundary;
  if FNode1.Position > FNode2.Position then
  begin
    HigherNode := FNode1;
    LowerNode := FNode2;
  end
  else
  begin
    HigherNode := FNode2;
    LowerNode := FNode1;
  end;

  SetLength(Polygon1, HigherNode.Position - LowerNode.Position + 1);
  for NodeIndex := 0 to Length(Polygon1) - 1 do
  begin
    NNode := Boundary[NodeIndex + LowerNode.Position];
    Polygon1[NodeIndex] := NNode.FNode.Location;
  end;
  SetLength(Polygon2, Boundary.Count - HigherNode.Position +
    LowerNode.Position);
  for NodeIndex := 0 to Length(Polygon2) - 1 do
  begin
    NIndex := NodeIndex + HigherNode.Position;
    if NIndex >= Boundary.Count - 1 then
    begin
      NIndex := NIndex - (Boundary.Count - 1);
    end;
    NNode := Boundary[NIndex];
    Polygon2[NodeIndex] := NNode.FNode.Location;
  end;
  Area1 := Abs(Area(Polygon1));
  Area2 := Abs(Area(Polygon2));

  Alpha := Abs(Area2 - Area1) / (Area2 + Area1);

  // RBW modification: Favor splitting at  concave and straight locations.
  if ComputeAll then
  begin
    FGamma := (ComputeGamma(FNode1.FNode.FNodeType, FTheta1) +
      ComputeGamma(FNode2.FNode.FNodeType, FTheta2)) / 2;
  end;

  if SixNodeCost then
  begin
    FCost := AltC1 * FPhi + AltC2 * Sigma + AltC3 * Eta + AltC4 * F_l + AltC5 *
      Alpha + AltC6 * FGamma;
    FNodeDistance := -1;
  end
  else
  begin
    FCost := C1 * FPhi + C2 * Sigma + C3 * Eta + C4 * F_l + C5 * Alpha +
      C6 * FGamma;
  end;
end;

constructor TCost.Create(Node1, Node2: TNodeInBoundary;
  QuadMeshCreator: TQuadMeshCreator);
begin
  inherited Create;
  FNode1 := Node1;
  FNode2 := Node2;
  FQuadMeshCreator := QuadMeshCreator;
  FNodeDistance := -1;
  ComputeVisibility;
end;

{ TOutline }

procedure TBoundary.AddNode(Node: TNode);
begin
  FNodes.Add(Node);
end;

procedure TBoundary.SetCounterClockwiseOrientation;
var
  SegIndex: Integer;
  BoundaryIndex: Integer;
begin
  if Count = 0 then
  begin
    for BoundaryIndex := 0 to SubPartCount - 1 do
    begin
      SubParts[BoundaryIndex].SetCounterClockwiseOrientation;
    end;
  end
  else
  begin
    if (Orientation <> CounterClockwise) then
    begin
      Reverse;
      FSegments.Reverse;
      if FSegments.Count >= 2 then
      begin
        if FSegments[0].Node2 <> FSegments[1].Node1 then
        begin
          for SegIndex := 0 to FSegments.Count - 1 do
          begin
            FSegments[SegIndex].Reverse;
          end;
        end;
      end;
      RenumberNodes;
    end;
  end;
end;

procedure TBoundary.SetElementNumber(Value: Integer);
begin
  FElementNumber := Value;
end;

procedure TBoundary.RemoveSelfFromOwnNodes;
var
  NodeIndex: Integer;
begin
  for NodeIndex := 0 to Count - 1 do
  begin
    Items[NodeIndex].FNode.FElements.Remove(self);
  end;
end;

procedure TBoundary.RemoveSelfFromAllNodes;
var
  NodeIndex: Integer;
  ANode: TNode;
begin
  for NodeIndex := 0 to FQuadMeshCreator.FNodes.Count - 1 do
  begin
    ANode := FQuadMeshCreator.FNodes[NodeIndex];
    ANode.FElements.Remove(self);
  end;
end;

procedure TBoundary.AssignOriginalEdgeAngles;
var
  PriorNode: TNode;
  CurrentNode: TNode;
  NextNode: TNode;
  SegmentIndex: Integer;
  ASegment: TSegment;
  NodeIndex: Integer;
  InnerNode: TNode;
  NodeAngle: double;
begin
  if FConverted then
  begin
    Exit
  end;
  PriorNode := FSegments[FSegments.Count - 1].FNode1;
  for SegmentIndex := 0 to FSegments.Count - 1 do
  begin
    ASegment := FSegments[SegmentIndex];
    CurrentNode := ASegment.Node1;
    NextNode := ASegment.Node2;
    NodeAngle := VertexAngleRadians(PriorNode.Location, CurrentNode.Location,
      NextNode.Location);
    if NodeAngle = 0 then
    begin
      NodeAngle := 2 * Pi;
    end;
    if NodeAngle < Pi / 4 then
    begin
      CurrentNode.FDesiredElementCount := 1;
    end
    else if NodeAngle < Pi + Pi / 4 then
    begin
      CurrentNode.FDesiredElementCount := 2;
    end
    else if NodeAngle < Pi * 7 / 4 then
    begin
      CurrentNode.FDesiredElementCount := 3;
    end;
    PriorNode := CurrentNode;
    for NodeIndex := 0 to ASegment.FInnerNodes.Count - 1 do
    begin
      InnerNode := ASegment.FInnerNodes[NodeIndex];
      InnerNode.FDesiredElementCount := 2;
    end;
  end;
end;

procedure TBoundary.CheckInvalidElement;
var
  ANode: TNode;
  NodeIndex: Integer;
  BoundaryNodes: TNodeList;
  Node1: TNode;
  Node0: TNode;
  Node2: TNode;
  Node3: TNode;
begin
  if Count <> 5 then
  begin
    Exit;
  end;
  BoundaryNodes := TNodeList.Create;
  try
    for NodeIndex := 0 to Count - 2 do
    begin
      ANode := Items[NodeIndex].FNode;
      if ANode.FNodeType in [ntEdge, ntSubDomain] then
      begin
        BoundaryNodes.Add(ANode);
      end;
    end;
    if BoundaryNodes.Count = 3 then
    begin
      if BadAngle(BoundaryNodes[0].FLocation, BoundaryNodes[1].FLocation,
        BoundaryNodes[2].FLocation) then
      begin
        InvalidElement := True;
      end;
    end;
  finally
    BoundaryNodes.Free;
  end;
  Node0 := Items[0].FNode;
  Node1 := Items[1].FNode;
  Node2 := Items[2].FNode;
  Node3 := Items[3].FNode;
  if not Intersect(EquateSegment(Node0.Location, Node2.Location),
    EquateSegment(Node1.Location, Node3.Location)) then
  begin
    InvalidElement := True;
  end;
end;

procedure TBoundary.ConvertToClosedBoundary;
var
  FirstSegment: TSegment;
  LastSegment: TSegment;
  SegmentIndex: Integer;
  ASegment: TSegment;
begin
  Assert(FSegments.Count > 0);
  FirstSegment := FSegments[0];
  LastSegment := FSegments[FSegments.Count - 1];
  if FirstSegment.Node1 <> LastSegment.Node2 then
  begin
    FConverted := True;
    for SegmentIndex := FSegments.Count - 1 downto 0 do
    begin
      ASegment := FSegments[SegmentIndex];
      ASegment := ASegment.CreateReversedSegment;
      FSegments.Add(ASegment);
    end;
  end;
end;

constructor TBoundary.Create(QuadMeshCreator: TQuadMeshCreator;
  SegmentType: TSegmentType; Parent: TBoundary; DesiredSpacing: double);
begin
  inherited Create;
  FParent := Parent;
  FSegmentType := SegmentType;
  FQuadMeshCreator := QuadMeshCreator;
  FNodes := TNodeList.Create;
  FSubParts := TBoundaryObjectList.Create;
  FSegments := TSegmentObjectList.Create;
  Assert(DesiredSpacing > 0);
  FDesiredSpacing := DesiredSpacing;
end;

procedure TBoundary.CreateBoundaryNodes;
var
  SegmentIndex: Integer;
  ASegment: TSegment;
  ANode: TNodeInBoundary;
  PriorSegment: TSegment;
  NodeIndex: Integer;
begin
  Assert(Count = 0);
  if FSegments.Count = 1 then
  begin
    PriorSegment := nil;
  end
  else
  begin
    PriorSegment := FSegments[FSegments.Count - 1];
    Assert(FSegments[0].Node1 = PriorSegment.Node2);
  end;
  for SegmentIndex := 0 to FSegments.Count - 1 do
  begin
    ASegment := FSegments[SegmentIndex];
    ANode := TNodeInBoundary.Create(ASegment.Node1, self, ASegment);
    ANode.FPosition := Add(ANode);
    if PriorSegment <> nil then
    begin
      Assert(PriorSegment.Node2 = ASegment.Node1);
      ANode.FSegments.Add(PriorSegment);
    end;
    for NodeIndex := 0 to ASegment.FInnerNodes.Count - 1 do
    begin
      ANode := TNodeInBoundary.Create(ASegment.FInnerNodes[NodeIndex], self,
        ASegment);
      ANode.FPosition := Add(ANode);
    end;
    PriorSegment := ASegment;
  end;
  if Count > 1 then
  begin
    Add(Items[0]);
  end;
end;

destructor TBoundary.Destroy;
begin
  FSegments.Free;
  FSubParts.Free;
  FNodes.Free;
  inherited;
end;

procedure TBoundary.GenerateSegments(DesiredOrientation: Integer);
var
  NodeIndex: Integer;
  Segment: TSegment;
  Points: TPolygon2D;
  AnOrientation: Integer;
  OutputPoints: TPolygon2D;
  Node1: TNode;
  Node2: TNode;
begin
  Assert(FSegments.Count = 0);
  if FNodes.Count = 1 then
  begin
    Segment := TSegment.Create(FNodes[0], FNodes[0], SegmentType, self,
      FQuadMeshCreator);
    FSegments.Add(Segment);
  end
  else
  begin
    if FNodes[0] = FNodes[FNodes.Count - 1] then
    begin
      SetLength(Points, FNodes.Count);
      for NodeIndex := 0 to FNodes.Count - 1 do
      begin
        Points[NodeIndex] := FNodes[NodeIndex].Location;
      end;
      ConvexHull2(Points, AnOrientation, OutputPoints);
      if (AnOrientation <> CollinearOrientation) and
        (AnOrientation <> DesiredOrientation) then
      begin
        FNodes.Reverse;
      end;
    end;
    for NodeIndex := 0 to FNodes.Count - 2 do
    begin
      Node1 := FNodes[NodeIndex];
      Node2 := FNodes[NodeIndex + 1];
      Segment := TSegment.Create(Node1, Node2, SegmentType, self,
        FQuadMeshCreator);
      FSegments.Add(Segment);
    end;
  end;
end;

function TBoundary.GetElementNumber: Integer;
begin
  result := FElementNumber;
end;

function TBoundary.GetActiveNode(Index: Integer): INode;
begin
  result := Items[Index].FNode;
end;

function TBoundary.GetActiveNodeCount: Integer;
begin
  result := Count - 1;
end;

function TBoundary.GetSubPart(Index: Integer): TBoundary;
begin
  result := FSubParts[Index];
end;

function TBoundary.GetSubPartCount: Integer;
begin
  result := FSubParts.Count;
end;

function TBoundary.IndexOfNode(ANode: TNode): Integer;
var
  NodeIndex: Integer;
begin
  result := -1;
  for NodeIndex := 0 to Count - 1 do
  begin
    if Items[NodeIndex].FNode = ANode then
    begin
      result := NodeIndex;
      break;
    end;
  end;
end;

procedure TBoundary.InsertNodesAlongBoundary;
var
  NodeDistance: TFloat;
  NumberOfNodesToInsert: Integer;
  LargestIndex: Integer;
  LargestSpacing: double;
  TotalEdgeCount: Integer;
  SegmentIndex: Integer;
  ASegment: TSegment;
begin

  Assert(FSegments.Count > 0);

  LargestSpacing := 0;
  LargestIndex := -1;
  TotalEdgeCount := 0;

  for SegmentIndex := 0 to FSegments.Count - 1 do
  begin
    NodeDistance := FSegments[SegmentIndex].Length;
    if LargestSpacing < NodeDistance then
    begin
      LargestSpacing := NodeDistance;
      LargestIndex := SegmentIndex;
    end;
    ASegment := FSegments[SegmentIndex];
    if NodeDistance > Min(ASegment.Node1.DesiredSpacing,
      ASegment.Node2.DesiredSpacing) then
    begin
      NumberOfNodesToInsert := ASegment.NodesToInsert;
      Inc(TotalEdgeCount, NumberOfNodesToInsert + 1)
    end
    else
    begin
      if NodeDistance > 0 then
      begin
        Inc(TotalEdgeCount);
      end;
    end;
  end;

  if not Odd(TotalEdgeCount) then
  begin
    LargestIndex := -1;
  end;

  for SegmentIndex := 0 to FSegments.Count - 1 do
  begin
    NodeDistance := FSegments[SegmentIndex].Length;
    ASegment := FSegments[SegmentIndex];
    if (NodeDistance > Min(ASegment.Node1.DesiredSpacing,
      ASegment.Node2.DesiredSpacing)) or (SegmentIndex = LargestIndex) then
    begin
      NumberOfNodesToInsert := ASegment.NodesToInsert;
      if SegmentIndex = LargestIndex then
      begin
        Inc(NumberOfNodesToInsert);
      end;
      Assert(NumberOfNodesToInsert >= 1);
      FSegments[SegmentIndex].InsertNodes(NumberOfNodesToInsert);
    end;
  end;

end;

procedure TBoundary.RenumberNodes;
var
  Index: Integer;
  SegIndex: Integer;
  PriorSegment: TSegment;
  ASegment: TSegment;
begin
  for Index := Count - 1 downto 0 do
  begin
    Items[Index].FPosition := Index;
  end;
  PriorSegment := FSegments[FSegments.Count - 1];
  for SegIndex := 0 to FSegments.Count - 2 do
  begin
    ASegment := FSegments[SegIndex];
    try
      Assert(ASegment.Node1 = PriorSegment.Node2);
    except
      ShowMessage(IntToStr(SegIndex));
      raise;
    end;
    PriorSegment := ASegment;
  end;
end;

procedure TBoundary.SetNodeTypes(NodeType: TNodeType);
var
  Index: Integer;
begin
  for Index := FNodes.Count - 1 downto 0 do
  begin
    FNodes[Index].FNodeType := NodeType;
  end;
end;

procedure TBoundary.Split222(FirstIndex: Integer);
var
  NewNode: TNode;
  NodeIndex: Integer;
  BoundNode1: TNodeInBoundary;
  BoundNode2: TNodeInBoundary;
  BoundNode3: TNodeInBoundary;
  procedure CreateSubBoundary(OffsetIndex: Integer);
  var
    NodeIndex: Integer;
    BoundNode1: TNodeInBoundary;
    BoundNode2: TNodeInBoundary;
    BoundNode3: TNodeInBoundary;
    SubBoundary: TBoundary;
    ASegment: TSegment;
    NewBoundNode: TNodeInBoundary;
  begin
    SubBoundary := TBoundary.Create(FQuadMeshCreator, stInner, self,
      DesiredSpacing);
    FSubParts.Add(SubBoundary);
    NodeIndex := FirstIndex + OffsetIndex;
    if NodeIndex >= Count then
    begin
      NodeIndex := NodeIndex - Count;
    end;
    BoundNode1 := Items[NodeIndex];

    Inc(NodeIndex);
    if NodeIndex >= Count then
    begin
      NodeIndex := NodeIndex - Count;
    end;
    BoundNode2 := Items[NodeIndex];

    Inc(NodeIndex);
    if NodeIndex >= Count then
    begin
      NodeIndex := NodeIndex - Count;
    end;
    BoundNode3 := Items[NodeIndex];

    ASegment := TSegment.Create(BoundNode1.FNode, BoundNode2.FNode, stInner,
      SubBoundary, FQuadMeshCreator);
    SubBoundary.FSegments.Add(ASegment);
    NewBoundNode := TNodeInBoundary.Create(BoundNode1.FNode, SubBoundary,
      ASegment);
    SubBoundary.Add(NewBoundNode);

    ASegment := TSegment.Create(BoundNode2.FNode, BoundNode3.FNode, stInner,
      SubBoundary, FQuadMeshCreator);
    SubBoundary.FSegments.Add(ASegment);
    NewBoundNode := TNodeInBoundary.Create(BoundNode2.FNode, SubBoundary,
      ASegment);
    SubBoundary.Add(NewBoundNode);

    ASegment := TSegment.Create(BoundNode3.FNode, NewNode, stInner, SubBoundary,
      FQuadMeshCreator);
    SubBoundary.FSegments.Add(ASegment);
    NewBoundNode := TNodeInBoundary.Create(BoundNode3.FNode, SubBoundary,
      ASegment);
    SubBoundary.Add(NewBoundNode);

    ASegment := TSegment.Create(NewNode, BoundNode1.FNode, stInner, SubBoundary,
      FQuadMeshCreator);
    SubBoundary.FSegments.Add(ASegment);
    NewBoundNode := TNodeInBoundary.Create(NewNode, SubBoundary, ASegment);
    SubBoundary.Add(NewBoundNode);

    SubBoundary.Add(SubBoundary.Items[0]);
  end;

begin
  // 2-2-2
  // 2-1-1-2
  // 2-2-1-1
  Assert(Count = 7);
  Delete(Count - 1);
  NewNode := TNode.Create(FQuadMeshCreator, DesiredSpacing);
  NodeIndex := FirstIndex + 1;
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BoundNode1 := Items[NodeIndex];
  Inc(NodeIndex, 2);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BoundNode2 := Items[NodeIndex];
  Inc(NodeIndex, 2);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BoundNode3 := Items[NodeIndex];

  NewNode.FLocation.X := (BoundNode1.FNode.X + BoundNode2.FNode.X +
    BoundNode3.FNode.X) / 3;
  NewNode.FLocation.Y := (BoundNode1.FNode.Y + BoundNode2.FNode.Y +
    BoundNode3.FNode.Y) / 3;

  // First element
  CreateSubBoundary(1);

  // second element
  CreateSubBoundary(3);

  // Third element
  CreateSubBoundary(5);

  RemoveSelfFromOwnNodes;
  Clear;
  FSegments.Clear;
end;

procedure TBoundary.Split312(FirstIndex: Integer);
var
  NodeIndex: Integer;
  BaseNode1: TNodeInBoundary;
  BaseNode2: TNodeInBoundary;
  BaseNode3: TNodeInBoundary;
  BaseNode4: TNodeInBoundary;
  Vec1: TVector2D;
  Vec2: TVector2D;
  VectorLength1: double;
  VectorLength2: double;
  BaseLength: TFloat;
  TriangleNode: TNodeInBoundary;
  SideNode: TNodeInBoundary;
  NewNode1: TNode;
  NewNode2: TNode;
  SubBoundary: TBoundary;
  ASegment: TSegment;
  NewBoundaryNode: TNodeInBoundary;
  Factor: double;
begin
  // 3-1-2
  // 3-1-1-1
  Assert(Count = 7);
  Delete(Count - 1);

  NodeIndex := FirstIndex;
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode1 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode2 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode3 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode4 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  TriangleNode := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  SideNode := Items[NodeIndex];

  Vec1.X := TriangleNode.FNode.X - BaseNode4.FNode.X;
  Vec1.Y := TriangleNode.FNode.Y - BaseNode4.FNode.Y;
  VectorLength1 := sqrt(Sqr(Vec1.X) + Sqr(Vec1.Y));

  Vec2.X := SideNode.FNode.X - BaseNode1.FNode.X;
  Vec2.Y := SideNode.FNode.Y - BaseNode1.FNode.Y;
  VectorLength2 := sqrt(Sqr(Vec2.X) + Sqr(Vec2.Y));

  BaseLength := Distance(BaseNode1.FNode.Location,
    BaseNode4.FNode.Location) / 3;

  Factor := Min(VectorLength1 / BaseLength, VectorLength2 / BaseLength) / 4;

  Vec1 := Scale(Vec1, Factor);
  Vec2 := Scale(Vec2, Factor);

  NewNode1 := TNode.Create(FQuadMeshCreator, DesiredSpacing);
  NewNode1.X := BaseNode3.FNode.X + Vec1.X;
  NewNode1.Y := BaseNode3.FNode.Y + Vec1.Y;

  NewNode2 := TNode.Create(FQuadMeshCreator, DesiredSpacing);
  NewNode2.X := BaseNode2.FNode.X + Vec2.X;
  NewNode2.Y := BaseNode2.FNode.Y + Vec2.Y;

  while not Intersect(EquateSegment(BaseNode2.FNode.Location,
    NewNode1.Location), EquateSegment(BaseNode3.FNode.Location,
    NewNode2.Location)) do
  begin
    Vec1 := Scale(Vec1, 0.8);
    Vec2 := Scale(Vec2, 0.8);
    NewNode1.X := BaseNode3.FNode.X + Vec1.X;
    NewNode1.Y := BaseNode3.FNode.Y + Vec1.Y;
    NewNode2.X := BaseNode2.FNode.X + Vec2.X;
    NewNode2.Y := BaseNode2.FNode.Y + Vec2.Y;
  end;

  // element 1
  SubBoundary := TBoundary.Create(FQuadMeshCreator, stInner, self,
    DesiredSpacing);
  FSubParts.Add(SubBoundary);

  ASegment := TSegment.Create(BaseNode4.FNode, BaseNode3.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode4.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(BaseNode3.FNode, NewNode1, stInner, SubBoundary,
    FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode3.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(NewNode1, TriangleNode.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(NewNode1, SubBoundary, ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(TriangleNode.FNode, BaseNode4.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(TriangleNode.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);
  SubBoundary.Add(SubBoundary.Items[0]);

  // element 2
  SubBoundary := TBoundary.Create(FQuadMeshCreator, stInner, self,
    DesiredSpacing);
  FSubParts.Add(SubBoundary);

  ASegment := TSegment.Create(BaseNode3.FNode, BaseNode2.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode3.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(BaseNode2.FNode, NewNode2, stInner, SubBoundary,
    FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode2.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(NewNode2, NewNode1, stInner, SubBoundary,
    FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(NewNode2, SubBoundary, ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(NewNode1, BaseNode3.FNode, stInner, SubBoundary,
    FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(NewNode1, SubBoundary, ASegment);
  SubBoundary.Add(NewBoundaryNode);
  SubBoundary.Add(SubBoundary.Items[0]);

  // element 3
  SubBoundary := TBoundary.Create(FQuadMeshCreator, stInner, self,
    DesiredSpacing);
  FSubParts.Add(SubBoundary);

  ASegment := TSegment.Create(BaseNode2.FNode, BaseNode1.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode2.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(BaseNode1.FNode, SideNode.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode1.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(SideNode.FNode, NewNode2, stInner, SubBoundary,
    FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(SideNode.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(NewNode2, BaseNode2.FNode, stInner, SubBoundary,
    FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(NewNode2, SubBoundary, ASegment);
  SubBoundary.Add(NewBoundaryNode);
  SubBoundary.Add(SubBoundary.Items[0]);

  // element 4
  SubBoundary := TBoundary.Create(FQuadMeshCreator, stInner, self,
    DesiredSpacing);
  FSubParts.Add(SubBoundary);

  ASegment := TSegment.Create(SideNode.FNode, TriangleNode.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(SideNode.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(TriangleNode.FNode, NewNode1, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(TriangleNode.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(NewNode1, NewNode2, stInner, SubBoundary,
    FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(NewNode1, SubBoundary, ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(NewNode2, SideNode.FNode, stInner, SubBoundary,
    FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(NewNode2, SubBoundary, ASegment);
  SubBoundary.Add(NewBoundaryNode);
  SubBoundary.Add(SubBoundary.Items[0]);

  RemoveSelfFromOwnNodes;
  Clear;
  FSegments.Clear;
end;

procedure TBoundary.Split411(FirstIndex: Integer);
var
  NodeIndex: Integer;
  BaseNode1: TNodeInBoundary;
  BaseNode2: TNodeInBoundary;
  BaseNode3: TNodeInBoundary;
  BaseNode4: TNodeInBoundary;
  BaseNode5: TNodeInBoundary;
  TriangleNode: TNodeInBoundary;
  SubBoundary: TBoundary;
  ASegment: TSegment;
  NewBoundaryNode: TNodeInBoundary;
  SubIndex: Integer;
  EdgeSegment: TSegment;
  ReverseSegment: TSegment;
  InnerNodeIndex: Integer;
  ANode: TNode;
begin
  Assert(Count = 7);
  Delete(Count - 1);
  NodeIndex := FirstIndex;
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode1 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode2 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode3 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode4 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode5 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  TriangleNode := Items[NodeIndex];

  EdgeSegment := TSegment.Create(BaseNode3.FNode, TriangleNode.FNode, stInner,
    self, FQuadMeshCreator);
  EdgeSegment.InsertNodes(2);
  ReverseSegment := EdgeSegment.CreateReversedSegment;

  // element 1
  SubBoundary := TBoundary.Create(FQuadMeshCreator, stInner, self,
    DesiredSpacing);
  FSubParts.Add(SubBoundary);

  ASegment := TSegment.Create(BaseNode5.FNode, BaseNode4.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode5.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(BaseNode4.FNode, BaseNode3.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode4.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  EdgeSegment.FBoundary := SubBoundary;
  SubBoundary.FSegments.Add(EdgeSegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode3.FNode, SubBoundary,
    EdgeSegment);
  SubBoundary.Add(NewBoundaryNode);
  for InnerNodeIndex := 0 to EdgeSegment.FInnerNodes.Count - 1 do
  begin
    ANode := EdgeSegment.FInnerNodes[InnerNodeIndex];
    NewBoundaryNode := TNodeInBoundary.Create(ANode, SubBoundary, EdgeSegment);
    SubBoundary.Add(NewBoundaryNode);
  end;

  ASegment := TSegment.Create(TriangleNode.FNode, BaseNode5.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(TriangleNode.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);
  SubBoundary.Add(SubBoundary.Items[0]);

  // element 2
  SubBoundary := TBoundary.Create(FQuadMeshCreator, stInner, self,
    DesiredSpacing);
  FSubParts.Add(SubBoundary);

  ASegment := TSegment.Create(BaseNode3.FNode, BaseNode2.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode3.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(BaseNode2.FNode, BaseNode1.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode2.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(BaseNode1.FNode, TriangleNode.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode1.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ReverseSegment.FBoundary := SubBoundary;
  SubBoundary.FSegments.Add(ReverseSegment);
  NewBoundaryNode := TNodeInBoundary.Create(TriangleNode.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);
  for InnerNodeIndex := 0 to ReverseSegment.FInnerNodes.Count - 1 do
  begin
    ANode := ReverseSegment.FInnerNodes[InnerNodeIndex];
    NewBoundaryNode := TNodeInBoundary.Create(ANode, SubBoundary,
      ReverseSegment);
    SubBoundary.Add(NewBoundaryNode);
  end;

  SubBoundary.Add(SubBoundary.Items[0]);

  RemoveSelfFromOwnNodes;
  Clear;
  FSegments.Clear;

  for SubIndex := 0 to FSubParts.Count - 1 do
  begin
    FSubParts[SubIndex].SpecialCase;
  end;
end;

procedure TBoundary.Split2121(FirstIndex: Integer);
var
  NodeIndex: Integer;
  BaseNode1: TNodeInBoundary;
  BaseNode2: TNodeInBoundary;
  BaseNode3: TNodeInBoundary;
  TopNode3: TNodeInBoundary;
  TopNode2: TNodeInBoundary;
  TopNode1: TNodeInBoundary;
  SubBoundary: TBoundary;
  ASegment: TSegment;
  NewBoundaryNode: TNodeInBoundary;
begin
  Assert(Count = 7);
  Delete(Count - 1);

  NodeIndex := FirstIndex;
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode1 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode2 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  BaseNode3 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  TopNode3 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  TopNode2 := Items[NodeIndex];

  Inc(NodeIndex);
  if NodeIndex >= Count then
  begin
    NodeIndex := NodeIndex - Count;
  end;
  TopNode1 := Items[NodeIndex];

  // element 1
  SubBoundary := TBoundary.Create(FQuadMeshCreator, stInner, self,
    DesiredSpacing);
  FSubParts.Add(SubBoundary);

  ASegment := TSegment.Create(BaseNode3.FNode, BaseNode2.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode3.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(BaseNode2.FNode, TopNode2.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode2.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(TopNode2.FNode, TopNode3.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(TopNode2.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(TopNode3.FNode, BaseNode3.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(TopNode3.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);
  SubBoundary.Add(SubBoundary.Items[0]);

  // element 2
  SubBoundary := TBoundary.Create(FQuadMeshCreator, stInner, self,
    DesiredSpacing);
  FSubParts.Add(SubBoundary);

  ASegment := TSegment.Create(BaseNode2.FNode, BaseNode1.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode2.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(BaseNode1.FNode, TopNode1.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(BaseNode1.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(TopNode1.FNode, TopNode2.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(TopNode1.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);

  ASegment := TSegment.Create(TopNode2.FNode, BaseNode2.FNode, stInner,
    SubBoundary, FQuadMeshCreator);
  SubBoundary.FSegments.Add(ASegment);
  NewBoundaryNode := TNodeInBoundary.Create(TopNode2.FNode, SubBoundary,
    ASegment);
  SubBoundary.Add(NewBoundaryNode);
  SubBoundary.Add(SubBoundary.Items[0]);

  RemoveSelfFromOwnNodes;
  Clear;
  FSegments.Clear;
end;

procedure TBoundary.ReverseSubBoundaries;
var
  SubIndex: Integer;
begin
  if Count > 0 then
  begin
    Reverse;
  end
  else
  begin
    for SubIndex := 0 to FSubParts.Count - 1 do
    begin
      FSubParts[SubIndex].ReverseSubBoundaries;
    end;
  end;
end;

function TBoundary.SpecialCase: boolean;
var
  Colinears: TColliniears;
  NodeIndex: Integer;
  Node1: TNodeInBoundary;
  Node2: TNodeInBoundary;
  Node3: TNodeInBoundary;
  ColinIndex: Integer;
  FirstIndex: Integer;
  ColinCount: Integer;
  IntList: TIntegerList;
  NodeIndex2: Integer;
  AddedLast: boolean;
  BoundNode1: TNodeInBoundary;
  BoundNode2: TNodeInBoundary;
  BoundNode3: TNodeInBoundary;
  AnOrientation: Integer;
begin
  Assert(Count = 7);
  result := False;
  for NodeIndex := 0 to Count - 3 do
  begin
    Node1 := Items[NodeIndex];
    Node2 := Items[NodeIndex + 1];
    Node3 := Items[NodeIndex + 2];
    Colinears[NodeIndex] := BadAngle(Node1.FNode.Location, Node2.FNode.Location,
      Node3.FNode.Location);
    result := result or Colinears[NodeIndex]
  end;
  NodeIndex := Count - 2;
  Node1 := Items[NodeIndex];
  Node2 := Items[NodeIndex + 1];
  Node3 := Items[1];
  Colinears[NodeIndex] := BadAngle(Node1.FNode.Location, Node2.FNode.Location,
    Node3.FNode.Location);
  result := result or Colinears[NodeIndex];
  if not result then
  begin
    Exit;
  end;
  FirstIndex := -1;
  for ColinIndex := 0 to Length(Colinears) - 1 do
  begin
    if Colinears[ColinIndex] then
    begin
      FirstIndex := ColinIndex;
      break;
    end;
  end;
  if FirstIndex = 0 then
  begin
    for ColinIndex := Length(Colinears) - 1 downto 0 do
    begin
      if not Colinears[ColinIndex] then
      begin
        FirstIndex := ColinIndex + 1;
        if FirstIndex = Length(Colinears) then
        begin
          FirstIndex := 0;
        end;
        break;
      end;
    end;
  end;
  ColinCount := 1;
  AddedLast := False;
  IntList := TIntegerList.Create;
  try
    for ColinIndex := FirstIndex to Length(Colinears) - 1 do
    begin
      if Colinears[ColinIndex] then
      begin
        Inc(ColinCount);
        AddedLast := False;
      end
      else
      begin
        IntList.Add(ColinCount);
        ColinCount := 1;
        AddedLast := True;
      end;
    end;
    for ColinIndex := 0 to FirstIndex - 1 do
    begin
      if Colinears[ColinIndex] then
      begin
        Inc(ColinCount);
        AddedLast := False;
      end
      else
      begin
        IntList.Add(ColinCount);
        ColinCount := 1;
        AddedLast := True;
      end;
    end;
    if not AddedLast then
    begin
      IntList.Add(ColinCount);
    end;
    case IntList.Count of
      3:
        begin
          case IntList[0] of
            2:
              begin
                // 2-2-2, 2-1-3, or 2-3-1
                case IntList[1] of
                  1:
                    begin
                      // 2-1-3
                      Assert(IntList[2] = 3);
                      // Convert to 3-1-2 and process again
                      Reverse;
                      SpecialCase;
                      ReverseSubBoundaries;
                    end;
                  2:
                    begin
                      // 2-2-2
                      Assert(IntList[2] = 2);
                      Split222(FirstIndex);
                    end;
                  3:
                    begin
                      // 2-3-1
                      Assert(IntList[2] = 1);
                      // Convert to 3-1-2 and process again
                      Delete(0);
                      for NodeIndex2 := 0 to FirstIndex - 1 do
                      begin
                        Add(Items[0]);
                        Delete(0);
                      end;
                      Add(Items[0]);
                      Delete(0);
                      Add(Items[0]);
                      Delete(0);
                      Add(Items[0]);
                      SpecialCase;
                    end;
                else
                  Assert(False);
                end;
              end;
            3:
              begin
                // 3-1-2 or 3-2-1
                case IntList[1] of
                  1:
                    begin
                      // 3-1-2
                      Assert(IntList[2] = 2);
                      Split312(FirstIndex);
                    end;
                  2:
                    begin
                      // 3-2-1
                      Assert(IntList[2] = 1);
                      // convert to 2-3-1 and process again
                      Reverse;
                      SpecialCase;
                      ReverseSubBoundaries;
                    end;
                else
                  Assert(False);
                end;
              end;
            4:
              begin
                // 4-1-1
                Assert(IntList[1] = 1);
                Assert(IntList[2] = 1);
                Split411(FirstIndex);
              end;
          else
            Assert(False);
          end;
        end;
      4:
        begin
          case IntList[0] of
            2:
              begin
                // 2-2-1-1, 2-1-1-2, 2-1-2-1
                case IntList[2] of
                  1:
                    begin
                      // 2-2-1-1, 2-1-1-2
                      case IntList[1] of
                        1:
                          begin
                            // 2-1-1-2
                            Assert(IntList[3] = 2);
                            Split222(FirstIndex);
                          end;
                        2:
                          begin
                            // 2-2-1-1
                            Assert(IntList[3] = 1);
                            Split222(FirstIndex);
                          end;
                      else
                        Assert(False);
                      end;
                    end;
                  2:
                    begin
                      // 2-1-2-1
                      Assert(IntList[1] = 1);
                      Assert(IntList[2] = 2);
                      Assert(IntList[3] = 1);
                      Split2121(FirstIndex);
                    end;
                else
                  Assert(False);
                end;
              end;
            3:
              begin
                // 3-1-1-1
                Assert(IntList[1] = 1);
                Assert(IntList[2] = 1);
                Assert(IntList[3] = 1);
                Split312(FirstIndex);
              end;
          else
            Assert(False);
          end;
        end;
      5:
        begin
          // 2-1-1-1-1
          Assert(IntList[0] = 2);
          Assert(IntList[1] = 1);
          Assert(IntList[2] = 1);
          Assert(IntList[3] = 1);
          Assert(IntList[4] = 1);

          BoundNode3 := Items[Count - 3];
          BoundNode2 := Items[Count - 2];
          for NodeIndex := 0 to Count - 2 do
          begin
            BoundNode1 := Items[NodeIndex];

            AnOrientation := FastGEO.Orientation(BoundNode3.FNode.Location,
              BoundNode2.FNode.Location, BoundNode1.FNode.Location);
            if AnOrientation = Clockwise then
            begin
              FirstIndex := NodeIndex - 2;
              if FirstIndex < 0 then
              begin
                FirstIndex := FirstIndex + Count - 1;
              end;
              break;
            end;
            BoundNode3 := BoundNode2;
            BoundNode2 := BoundNode1;
          end;

          Split2121(FirstIndex);
        end;
    else
      Assert(False);
    end;
  finally
    IntList.Free;
  end;

  Clear;
  FSegments.Clear;
end;

function TBoundary.Orientation: Integer;
var
  InputPoints: TPolygon2D;
  OutputPoints: TPolygon2D;
  NodeIndex: Integer;
  ANode: TNodeInBoundary;
begin
  Assert(Count > 0);
  SetLength(InputPoints, Count - 1);
  for NodeIndex := 0 to Count - 2 do
  begin
    ANode := Items[NodeIndex];
    InputPoints[NodeIndex] := ANode.FNode.Location;
  end;
  ConvexHull2(InputPoints, result, OutputPoints);
end;

function TBoundary.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    result := 0
  else
    result := E_NOINTERFACE;
end;

procedure TBoundary.Split(List: TBoundaryList);
var
  OuterNodeIndex: Integer;
  Node1: TNodeInBoundary;
  InnerNodeIndex: Integer;
  Node2: TNodeInBoundary;
  ACost: TCost;
  LowestCost: TCost;
  SubBoundary: TBoundary;
  NodeIndex: Integer;
  NewNode: TNodeInBoundary;
  SegIndex: Integer;
  ASeg: TSegment;
  PriorSegment: TSegment;
  SegmentInsertPosition1: Integer;
  SegmentInsertPosition2: Integer;
  NewSegment: TSegment;
  ReversedSegment: TSegment;
  SegmentBetweenCount: Integer;
  NodesToInsert: Integer;
  SegmentIndex: Integer;
  ASegment: TSegment;
  AnInnerNode: TNode;
  SubBoundaryIndex: Integer;
  FirstNode: TNodeInBoundary;
  ShouldSkip: boolean;
begin
  SetCounterClockwiseOrientation;

  if Count = 5 then
  begin
{$IFDEF DEBUG}
    // CheckInvalidElement;
{$ENDIF}
    Exit;
  end;
  Assert(Count >= 7);
  if (Count = 7) and (FQuadMeshCreator.SixNodeClosureMethod = cmTemplate) then
  begin
    if SpecialCase then
    begin
{$IFDEF DEBUG}
      // for SubBoundaryIndex := 0 to FSubParts.Count - 1 do
      // begin
      // FSubParts[SubBoundaryIndex].CheckInvalidElement;
      // end;
{$ENDIF}
      Exit;
    end;

  end;
  LowestCost := nil;
  // The last point is a duplicate of the first point
  for OuterNodeIndex := 0 to Count - 2 do
  begin
    Node1 := Items[OuterNodeIndex];
    for InnerNodeIndex := OuterNodeIndex + 1 to Count - 1 do
    begin
      // skip neighboring points
      if Abs(OuterNodeIndex - InnerNodeIndex) <= 1 then
      begin
        Continue;
      end;
      if (OuterNodeIndex = 0) and (InnerNodeIndex >= Count - 2) then
      begin
        Continue;
      end;
      if (OuterNodeIndex = 1) and (InnerNodeIndex = Count - 1) then
      begin
        Continue;
      end;
      Node2 := Items[InnerNodeIndex];
      if Node1.OnSameSegment(Node2) then
      begin
        Continue;
      end;
      if Node1.FNode = Node2.FNode then
      begin
        Continue;
      end;

      ACost := FQuadMeshCreator.Cost[Node1, Node2];

      if ACost.Visible then
      begin
        ACost.ComputeCost;
        if (LowestCost = nil) or (ACost.Cost < LowestCost.Cost) then
        begin
          LowestCost := ACost;
        end;
      end;
    end;
  end;
  Assert(LowestCost <> nil);

  if LowestCost.FNode1.Position < LowestCost.FNode2.Position then
  begin
    Node1 := LowestCost.FNode1;
    Node2 := LowestCost.FNode2;
  end
  else
  begin
    Node1 := LowestCost.FNode2;
    Node2 := LowestCost.FNode1;
  end;

  SplitSegmentAtNode(Node1);
  SplitSegmentAtNode(Node2);

  PriorSegment := nil;
  for SegIndex := 0 to Node1.FSegments.Count - 1 do
  begin
    ASeg := Node1.FSegments[SegIndex];
    if ASeg.Node2 = Node1.FNode then
    begin
      PriorSegment := ASeg;
      break;
    end;
  end;
  Assert(PriorSegment <> nil);
  ShouldSkip := False;
  SegmentInsertPosition1 := FSegments.IndexOf(PriorSegment) + 1;
  if SegmentInsertPosition1 = FSegments.Count then
  begin
    SegmentInsertPosition1 := 0;
    ShouldSkip := True;
  end;

  PriorSegment := nil;
  for SegIndex := 0 to Node2.FSegments.Count - 1 do
  begin
    ASeg := Node2.FSegments[SegIndex];
    if ASeg.Node1 = Node2.FNode then
    begin
      PriorSegment := ASeg;
      break;
    end;
  end;
  Assert(PriorSegment <> nil);
  SegmentInsertPosition2 := FSegments.IndexOf(PriorSegment);

  NewSegment := TSegment.Create(Node1.FNode, Node2.FNode, stInner, self,
    FQuadMeshCreator);

  SegmentBetweenCount := Node2.Position - Node1.Position;
  NodesToInsert := NewSegment.NodesToInsert;
  if not Odd(SegmentBetweenCount + NodesToInsert) then
  begin
    Inc(NodesToInsert);
  end;
  if NodesToInsert > 0 then
  begin
    NewSegment.InsertNodes(NodesToInsert);
  end;

  ReversedSegment := NewSegment.CreateReversedSegment;

  SubBoundary := TBoundary.Create(FQuadMeshCreator, stInner, self,
    (Node1.DesiredSpacing + Node2.DesiredSpacing) / 2);
  FSubParts.Add(SubBoundary);

  PriorSegment := nil;
  FirstNode := nil;

  if not ShouldSkip then
  begin
    for SegmentIndex := 0 to FSegments.Count - 1 do
    begin
      ASegment := FSegments[SegmentIndex];
      NewNode := TNodeInBoundary.Create(ASegment.Node1, SubBoundary, ASegment);
      if FirstNode = nil then
      begin
        FirstNode := NewNode;
      end;
      SubBoundary.Add(NewNode);
      if PriorSegment <> nil then
      begin
        NewNode.FSegments.Add(PriorSegment);
      end;
      SubBoundary.FSegments.Add(ASegment);
      ASegment.FBoundary := SubBoundary;
      for NodeIndex := 0 to ASegment.FInnerNodes.Count - 1 do
      begin
        AnInnerNode := ASegment.FInnerNodes[NodeIndex];
        NewNode := TNodeInBoundary.Create(AnInnerNode, SubBoundary, ASegment);
        SubBoundary.Add(NewNode);
      end;
      PriorSegment := ASegment;
      if ASegment.Node2 = Node1.FNode then
      begin
        break;
      end;
    end;
  end;

  NewNode := TNodeInBoundary.Create(NewSegment.Node1, SubBoundary, NewSegment);
  if FirstNode = nil then
  begin
    FirstNode := NewNode;
  end;
  SubBoundary.Add(NewNode);
  if PriorSegment <> nil then
  begin
    NewNode.FSegments.Add(PriorSegment);
  end;
  SubBoundary.FSegments.Add(NewSegment);
  NewSegment.FBoundary := SubBoundary;
  for NodeIndex := 0 to NewSegment.FInnerNodes.Count - 1 do
  begin
    AnInnerNode := NewSegment.FInnerNodes[NodeIndex];
    NewNode := TNodeInBoundary.Create(AnInnerNode, SubBoundary, NewSegment);
    SubBoundary.Add(NewNode);
  end;
  PriorSegment := NewSegment;

  for SegmentIndex := SegmentInsertPosition2 to FSegments.Count - 1 do
  begin
    ASegment := FSegments[SegmentIndex];
    NewNode := TNodeInBoundary.Create(ASegment.Node1, SubBoundary, ASegment);
    SubBoundary.Add(NewNode);
    if PriorSegment <> nil then
    begin
      NewNode.FSegments.Add(PriorSegment);
    end;
    SubBoundary.FSegments.Add(ASegment);
    ASegment.FBoundary := SubBoundary;
    for NodeIndex := 0 to ASegment.FInnerNodes.Count - 1 do
    begin
      AnInnerNode := ASegment.FInnerNodes[NodeIndex];
      NewNode := TNodeInBoundary.Create(AnInnerNode, SubBoundary, ASegment);
      SubBoundary.Add(NewNode);
    end;
    PriorSegment := ASegment;
  end;
  SubBoundary.Add(SubBoundary.Items[0]);
  FirstNode.FSegments.Add(PriorSegment);

  SubBoundary.RenumberNodes;

  SubBoundary := TBoundary.Create(FQuadMeshCreator, stInner, self,
    (Node1.DesiredSpacing + Node2.DesiredSpacing) / 2);
  FSubParts.Add(SubBoundary);

  NewNode := TNodeInBoundary.Create(ReversedSegment.Node1, SubBoundary,
    ReversedSegment);
  FirstNode := NewNode;
  SubBoundary.Add(NewNode);
  SubBoundary.FSegments.Add(ReversedSegment);
  ReversedSegment.FBoundary := SubBoundary;
  PriorSegment := ReversedSegment;
  for NodeIndex := 0 to ReversedSegment.FInnerNodes.Count - 1 do
  begin
    AnInnerNode := ReversedSegment.FInnerNodes[NodeIndex];
    NewNode := TNodeInBoundary.Create(AnInnerNode, SubBoundary,
      ReversedSegment);
    SubBoundary.Add(NewNode);
  end;

  for SegmentIndex := SegmentInsertPosition1 to SegmentInsertPosition2 - 1 do
  begin
    ASegment := FSegments[SegmentIndex];
    NewNode := TNodeInBoundary.Create(ASegment.Node1, SubBoundary, ASegment);
    if PriorSegment <> nil then
    begin
      NewNode.FSegments.Add(PriorSegment);
    end;
    SubBoundary.Add(NewNode);
    SubBoundary.FSegments.Add(ASegment);
    ASegment.FBoundary := SubBoundary;
    for NodeIndex := 0 to ASegment.FInnerNodes.Count - 1 do
    begin
      AnInnerNode := ASegment.FInnerNodes[NodeIndex];
      NewNode := TNodeInBoundary.Create(AnInnerNode, SubBoundary, ASegment);
      SubBoundary.Add(NewNode);
    end;
    PriorSegment := ASegment;
  end;
  SubBoundary.Add(SubBoundary.Items[0]);
  FirstNode.FSegments.Add(PriorSegment);
  SubBoundary.RenumberNodes;
  FSegments.OwnsObjects := False;
  RemoveSelfFromOwnNodes;
  FQuadMeshCreator.InvalidateCosts(self, LowestCost);

  Clear;
  FSegments.Clear;

  for SubBoundaryIndex := 0 to FSubParts.Count - 1 do
  begin
    List.Add(FSubParts[SubBoundaryIndex]);
  end;
end;

procedure TBoundary.SplitSegmentAtNode(ANode: TNodeInBoundary);
var
  ASegment: TSegment;
  ASegmentList: TSegmentList;
  SegmentPosition: Integer;
  SegmentIndex: Integer;
  SubSegment: TSegment;
  NodeIndex: Integer;
  AnotherNode: TNodeInBoundary;
  SubSeg1: TSegment;
  SubSeg2: TSegment;
  Seg1: TSegment;
  Seg2: TSegment;
begin
  if ANode.FSegments.Count = 1 then
  begin
    ASegment := ANode.FSegments[0];
    ANode.FSegments.Delete(0);
    ASegmentList := ASegment.Split(ANode.FNode);
    try
      Assert(ASegmentList.Count = 2);
      SegmentPosition := FSegments.IndexOf(ASegment);
      FSegments.Delete(SegmentPosition);
      for SegmentIndex := 0 to ASegmentList.Count - 1 do
      begin
        SubSegment := ASegmentList[SegmentIndex];
        FSegments.Insert(SegmentPosition + SegmentIndex, SubSegment);
        ANode.FSegments.Add(SubSegment);
      end;

      SubSeg1 := ASegmentList[0];
      SubSeg2 := ASegmentList[1];
      Assert(SubSeg1.Node2 = SubSeg2.Node1);
    finally
      ASegmentList.Free;
    end;

    for NodeIndex := ANode.Position - 1 downto 0 do
    begin
      AnotherNode := Items[NodeIndex];
      SegmentPosition := AnotherNode.FSegments.IndexOf(ASegment);
      if SegmentPosition >= 0 then
      begin
        AnotherNode.FSegments.Delete(SegmentPosition);
        AnotherNode.FSegments.Add(SubSeg1);
      end
      else
      begin
        break;
      end;
    end;

    for NodeIndex := ANode.Position + 1 to Count - 1 do
    begin
      AnotherNode := Items[NodeIndex];
      SegmentPosition := AnotherNode.FSegments.IndexOf(ASegment);
      if SegmentPosition >= 0 then
      begin
        AnotherNode.FSegments.Delete(SegmentPosition);
        AnotherNode.FSegments.Add(SubSeg2);
      end
      else
      begin
        break;
      end;
    end;
  end;
  Assert(ANode.FSegments.Count = 2);
  Seg1 := ANode.FSegments[0];
  Seg2 := ANode.FSegments[1];
  if Seg1.Node1 = ANode.FNode then
  begin
    Assert(Seg2.Node2 = ANode.FNode);
    ANode.FSegments.Reverse
  end
  else
  begin
    Assert(Seg1.Node2 = ANode.FNode);
    Assert(Seg2.Node1 = ANode.FNode);
  end;
end;

function TBoundary._AddRef: Integer;
begin
  Inc(FRefCount);
  result := FRefCount;
end;

function TBoundary._Release: Integer;
begin
  Dec(FRefCount);
  result := FRefCount;
  if result = 0 then
    Destroy;
end;

{ TQuadMeshCreator }

function TQuadMeshCreator.AddBoundary(DesiredSpacing: double): TBoundary;
begin
  result := TBoundary.Create(self, stEdge, nil, DesiredSpacing);
  FBoundaries.Add(result);
end;

procedure TQuadMeshCreator.AdjustNodes;
begin
  case NodeAdjustmentMethod of
    namLagrange:
      AdjustPositionLagrange;
    namGiuliani, namSarrateHuerta:
      AdjustPositionGiuliani;
  else
    Assert(False);
  end;
end;

procedure TQuadMeshCreator.AdjustPositionGiuliani;
var
  NodeIndex: Integer;
begin
  for NodeIndex := 0 to FNodes.Count - 1 do
  begin
    FNodes[NodeIndex].AdjustPositionGiuliani;
  end;
end;

procedure TQuadMeshCreator.AdjustPositionLagrange;
var
  NodeIndex: Integer;
begin
  for NodeIndex := 0 to FNodes.Count - 1 do
  begin
    FNodes[NodeIndex].AdjustPositionLagrange;
  end;
end;

procedure TQuadMeshCreator.AssignOriginalEdgeAngles;
var
  Index: Integer;
begin
  Assert(FBoundaries.Count > 0);
  for Index := 0 to FBoundaries.Count - 1 do
  begin
    FBoundaries[Index].AssignOriginalEdgeAngles;
  end;
end;

procedure TQuadMeshCreator.CheckInvalidElements;
  procedure CheckElement(Element: TBoundary);
  var
    Index: Integer;
  begin
    if Element.SubPartCount > 0 then
    begin
      for index := 0 to Element.SubPartCount - 1 do
      begin
        CheckElement(Element.SubParts[index]);
      end;
    end
    else
    begin
      Element.CheckInvalidElement;
    end;
  end;

begin
  Assert(FBoundaries.Count = 1);
  InvalidElement := False;
  CheckElement(FBoundaries[0]);
  if InvalidElement then
  begin
    raise EInvalidElement.Create(StrOneOrMoreInvalid);
  end;
end;

procedure TQuadMeshCreator.ComputeCharacteristicLength;
var
  ABoundary: TBoundary;
  ANode: TNode;
  BoundaryIndex: Integer;
  NodeIndex: Integer;
begin
  Assert(FBoundaries.Count > 0);
  ABoundary := FBoundaries[0];
  Assert(ABoundary.FNodes.Count > 0);
  ANode := ABoundary.FNodes[0];
  FMinX := ANode.X;
  FMinY := ANode.Y;
  FMaxX := FMinX;
  FMaxY := FMinY;
  for BoundaryIndex := 0 to FBoundaries.Count - 1 do
  begin
    ABoundary := FBoundaries[BoundaryIndex];
    for NodeIndex := 0 to ABoundary.FNodes.Count - 1 do
    begin
      ANode := ABoundary.FNodes[NodeIndex];
      if FMinX > ANode.X then
      begin
        FMinX := ANode.X
      end
      else if FMaxX < ANode.X then
      begin
        FMaxX := ANode.X
      end;
      if FMinY > ANode.Y then
      begin
        FMinY := ANode.Y
      end
      else if FMaxY < ANode.Y then
      begin
        FMaxY := ANode.Y
      end;
    end;
  end;
  // Equation 7.
  FCharacteristicLength := Distance(FMinX, FMinY, FMaxX, FMaxY);
end;

procedure TQuadMeshCreator.ConvertToClosedBoundaries;
var
  Index: Integer;
begin
  for Index := 0 to FBoundaries.Count - 1 do
  begin
    FBoundaries[Index].ConvertToClosedBoundary
  end;
end;

constructor TQuadMeshCreator.Create;
begin
  FNodes := TNodeObjectList.Create;
  FCostsArray := TCost3ObjectList.Create;
  FBoundaries := TBoundaryObjectList.Create;
  SixNodeClosureMethod := cmTemplate;
  FGrowthRate := ElementGrowthRate;
  FElementList := TIElementList.Create;
  FNodeList := TINodeList.Create;
  FBoundaryNodes := TNodeInBoundaryObjectList.Create;
end;

procedure TQuadMeshCreator.CreateBoundaryNodes;
var
  Index: Integer;
begin
  for Index := 0 to FBoundaries.Count - 1 do
  begin
    FBoundaries[Index].CreateBoundaryNodes;
  end;
end;

destructor TQuadMeshCreator.Destroy;
begin
  FBoundaryNodes.Free;
  FElementList.Free;
  FNodeList.Free;
  FBoundaries.Free;
  FCostsArray.Free;
  FNodes.Free;
  inherited;
end;

procedure TQuadMeshCreator.AssignDesiredSpacings;
var
  BoundaryIndex: Integer;
  ABoundary: TBoundary;
  SegmentIndex: Integer;
  ASegment: TSegment;
  NodeList: TNodeList;
  NodeIndex: Integer;
  NodeComparer: TNodeSpacingComparer;
  CompareNode: TNode;
  InnerNodeIndex: Integer;
  AnotherNode: TNode;
  Separation: TFloat;
  CurrentSpacing: double;
  Radius: double;
  OuterSpacing: double;
  ModifiedNode: boolean;
  RadiusToNode: double;
begin
  NodeList := TNodeList.Create;
  try
    for BoundaryIndex := 0 to FBoundaries.Count - 1 do
    begin
      ABoundary := FBoundaries[BoundaryIndex];
      for SegmentIndex := 0 to ABoundary.FSegments.Count - 1 do
      begin
        ASegment := ABoundary.FSegments[SegmentIndex];
        NodeList.Add(ASegment.Node1);
        for NodeIndex := 0 to ASegment.FInnerNodes.Count - 1 do
        begin
          NodeList.Add(ASegment.FInnerNodes[NodeIndex]);
        end;
      end;
    end;

    NodeComparer := TNodeSpacingComparer.Create;
    try
      NodeList.Sort(NodeComparer);

      for NodeIndex := NodeList.Count - 1 downto 1 do
      begin
        CompareNode := NodeList[NodeIndex];
        OuterSpacing := CompareNode.FDesiredSpacing;
        Radius := 0.0;
        ModifiedNode := False;
        for InnerNodeIndex := NodeIndex - 1 downto 0 do
        begin
          AnotherNode := NodeList[InnerNodeIndex];
          if CompareNode.FDesiredSpacing = AnotherNode.FDesiredSpacing then
          begin
            Continue;
          end;
          if OuterSpacing <> AnotherNode.FDesiredSpacing then
          begin
            Radius := 0.0;
            CurrentSpacing := CompareNode.DesiredSpacing;
            OuterSpacing := AnotherNode.FDesiredSpacing;
            while CurrentSpacing < AnotherNode.FDesiredSpacing do
            begin
              Radius := Radius + CurrentSpacing;
              CurrentSpacing := CurrentSpacing * GrowthRate;
              if CurrentSpacing > AnotherNode.FDesiredSpacing then
              begin
                CurrentSpacing := AnotherNode.FDesiredSpacing;
              end;
            end;
          end;

          Separation := Distance(CompareNode.Location, AnotherNode.Location);
          if Separation < Radius then
          begin
            ModifiedNode := True;
            CurrentSpacing := CompareNode.DesiredSpacing;
            RadiusToNode := 0;
            while RadiusToNode < Separation do
            begin
              RadiusToNode := RadiusToNode + CurrentSpacing;
              if RadiusToNode >= Separation then
              begin
                AnotherNode.FDesiredSpacing := CurrentSpacing;
                break;
              end;
              CurrentSpacing := CurrentSpacing * GrowthRate;
            end;
          end;
        end;
        NodeList.Delete(NodeIndex);
        if ModifiedNode then
        begin
          NodeList.Sort(NodeComparer);
        end;
      end;
    finally
      NodeComparer.Free;
    end;

  finally
    NodeList.Free;
  end;
end;

procedure TQuadMeshCreator.GenerateMesh;
var
  List: TBoundaryList;
  Index: Integer;
  ABoundary: TBoundary;
begin
  ComputeCharacteristicLength;
  SetNodeTypes;
  SetSegmentTypes;
  GenerateSegments;
  AssignDesiredSpacings;
  InsertNodesAlongBoundaries;
  ConvertToClosedBoundaries;
  CreateBoundaryNodes;
  AssignDesiredSpacings;
  AssignOriginalEdgeAngles;
  MakeSingleBoundary;
  Assert(FBoundaries.Count = 1);
  List := TBoundaryList.Create;
  try
    List.Add(FBoundaries[0]);
    Index := 0;
    while Index < List.Count do
    begin
      ABoundary := List[Index];
      ABoundary.Split(List);
      Inc(Index);
    end;

  finally
    List.Free;
  end;

  FBoundaries[0].SetCounterClockwiseOrientation;
  AdjustNodes;
  try
    CheckInvalidElements;
  except
    on EInvalidElement do
    begin
      // ignore
    end;
  end;

  repeat
  until (not ImproveTopology);

  RenumberNodes;

  if InvalidElement then
  begin
    raise EInvalidElement.Create(StrOneOrMoreInvalid);
  end;

end;

procedure TQuadMeshCreator.GenerateSegments;
var
  Index: Integer;
begin
  Assert(FBoundaries.Count > 0);
  FBoundaries[0].GenerateSegments(CounterClockwise);
  for Index := 1 to FBoundaries.Count - 1 do
  begin
    FBoundaries[Index].GenerateSegments(Clockwise);
  end;
end;

function TQuadMeshCreator.GetBoundaryCount: Integer;
begin
  result := FBoundaries.Count;
end;

function TQuadMeshCreator.GetCost(Node1, Node2: TNodeInBoundary): TCost;
var
  Index1: Integer;
  Index2: Integer;
  Temp: TNodeInBoundary;
  CostRow: TCost2ObjectList;
  CostList: TCostObjectList;
  CostIndex: Integer;
  ACost: TCost;
begin
  if Node1.FNode.FNodeNumber > Node2.FNode.FNodeNumber then
  begin
    Temp := Node1;
    Node1 := Node2;
    Node2 := Temp;
  end;
  Index1 := Node1.FNode.FNodeNumber;
  Index2 := Node2.FNode.FNodeNumber;
  Assert(Index1 <> Index2);
  while Index1 >= FCostsArray.Count do
  begin
    FCostsArray.Add(nil);
  end;
  CostRow := FCostsArray[Index1];
  if CostRow = nil then
  begin
    CostRow := TCost2ObjectList.Create;
    FCostsArray[Index1] := CostRow;
  end;
  while Index2 >= CostRow.Count do
  begin
    CostRow.Add(nil);
  end;

  CostList := CostRow[Index2];
  if CostList = nil then
  begin
    CostList := TCostObjectList.Create;
    CostRow[Index2] := CostList;
  end;
  result := nil;
  for CostIndex := 0 to CostList.Count - 1 do
  begin
    ACost := CostList[CostIndex];
    if (ACost.FNode1 = Node1) and (ACost.FNode2 = Node2) then
    begin
      result := ACost;
      Exit;
    end;
  end;
  if result = nil then
  begin
    result := TCost.Create(Node1, Node2, self);
    CostList.Add(result);
  end;
end;

function TQuadMeshCreator.GetActiveElement(Index: Integer): IElement;
begin
  result := FElementList[Index];
end;

function TQuadMeshCreator.GetActiveElementCount: Integer;
begin
  result := FElementList.Count
end;

function TQuadMeshCreator.GetNodeObject(Index: Integer): TNode;
begin
  result := FNodes[Index];
end;

function TQuadMeshCreator.GetActiveNode(Index: Integer): INode;
begin
  result := FNodeList[Index];
end;

function TQuadMeshCreator.GetActiveNodeCount: Integer;
begin
  result := FNodes.Count;
end;

function TQuadMeshCreator.ImproveTopology: boolean;
var
  NodeIndex: Integer;
  ANode: TNode;
  ShouldRemove: boolean;
begin
  result := False;
  Assert(BoundaryCount = 1);
  for NodeIndex := FNodes.Count - 1 downto 0 do
  begin
    if NodeIndex < FNodes.Count then
    begin
      ANode := FNodes[NodeIndex];
      try
        ShouldRemove := ANode.ImproveTopology1;
      except
        ShowMessage(IntToStr(NodeIndex));
        raise;
      end;
      result := result or ShouldRemove;
      if ShouldRemove then
      begin
        FNodes.Remove(ANode);
      end
      else
      begin
        result := ANode.ImproveTopology2 or result;
      end;
    end;
  end;
end;

procedure TQuadMeshCreator.InsertNodesAlongBoundaries;
var
  Index: Integer;
begin
  for Index := 0 to FBoundaries.Count - 1 do
  begin
    FBoundaries[Index].InsertNodesAlongBoundary
  end;
end;

procedure TQuadMeshCreator.InvalidateCosts(List: TBoundary; LowestCost: TCost);
var
  Index1: Integer;
  Index2: Integer;
  CostRow: TCost2ObjectList;
  procedure InvalidateCostsInBoundary;
  var
    NodeIndex: Integer;
    ANode: TNodeInBoundary;
    NodePosition: Integer;
    ACost: TCost;
    ACostList: TCostObjectList;
    CostIndex: Integer;
  begin
    for NodeIndex := 0 to List.Count - 1 do
    begin
      ANode := List[NodeIndex];
      NodePosition := ANode.FNode.FNodeNumber;
      if (NodePosition < CostRow.Count) then
      begin
        ACostList := CostRow[NodePosition];
        if ACostList <> nil then
        begin
          for CostIndex := 0 to ACostList.Count - 1 do
          begin
            ACost := ACostList[CostIndex];
            if (ACost.FNode1 = LowestCost.FNode1) or
              (ACost.FNode1 = LowestCost.FNode2) or
              (ACost.FNode2 = LowestCost.FNode1) or
              (ACost.FNode2 = LowestCost.FNode2) then
            begin
              ACost.FNodeDistance := -1;
            end;
          end;
        end;
      end;
    end;
  end;

begin
  Index1 := LowestCost.FNode1.FNode.FNodeNumber;
  if (Index1 < FCostsArray.Count) then
  begin
    CostRow := FCostsArray[Index1];
    if CostRow <> nil then
    begin
      InvalidateCostsInBoundary;
    end;
  end;
  Index2 := LowestCost.FNode2.FNode.FNodeNumber;
  if (Index2 < FCostsArray.Count) then
  begin
    CostRow := FCostsArray[Index2];
    if CostRow <> nil then
    begin
      InvalidateCostsInBoundary;
    end;
  end;
end;

procedure TQuadMeshCreator.MakeSingleBoundary;
var
  BoundaryQuadTree: TRbwQuadTree;
  FirstBoundary: TBoundary;
  NodeIndex: Integer;
  ANode: TNodeInBoundary;
  BoundaryIndex: Integer;
  ABoundary: TBoundary;
  ClosestNode: TNodeInBoundary;
  ACost: TCost;
  ClosestCost: TCost;
  ADistance: TFloat;
  ClosestDistance: TFloat;
  SubDomainNode: TNodeInBoundary;
  InsertPosition: Integer;
  BoundariesChanged: boolean;
  NewNode: TNodeInBoundary;
  OuterBoundaryNode: TNodeInBoundary;
  Seg1: TSegment;
  Seg2: TSegment;
  SegPosition: Integer;
  SubDomainSegPosition: Integer;
  NewSegment: TSegment;
  NodeDistance: double;
  NumberOfNodesToInsert: Integer;
  ReversedSegment: TSegment;
  AnInnerNode: TNode;
  SegInsertPosition: Integer;
  SegmentIndex: Integer;
begin
  Assert(FBoundaries.Count > 0);
  if FBoundaries.Count = 1 then
  begin
    Exit;
  end;
  BoundaryQuadTree := TRbwQuadTree.Create(nil);
  try
    BoundaryQuadTree.XMax := FMaxX;
    BoundaryQuadTree.XMin := FMinX;
    BoundaryQuadTree.YMax := FMaxY;
    BoundaryQuadTree.YMin := FMinY;

    FirstBoundary := FBoundaries[0];

    // Last point should be a duplicate of the first point.
    for NodeIndex := 0 to FirstBoundary.Count - 2 do
    begin
      ANode := FirstBoundary[NodeIndex];
      BoundaryQuadTree.AddPoint(ANode.FNode.X, ANode.FNode.Y, ANode);
    end;

    BoundariesChanged := True;
    while (FBoundaries.Count > 1) and BoundariesChanged do
    begin
      BoundariesChanged := False;
      for BoundaryIndex := FBoundaries.Count - 1 downto 1 do
      begin
        ABoundary := FBoundaries[BoundaryIndex];
        ClosestCost := nil;
        ClosestDistance := 0;
        for NodeIndex := 0 to ABoundary.Count - 1 do
        begin
          ANode := ABoundary[NodeIndex];
          ClosestNode := BoundaryQuadTree.NearestPointsFirstData(ANode.FNode.X,
            ANode.FNode.Y);
          ACost := Cost[ANode, ClosestNode];
          if ACost.Visible then
          begin
            ADistance := Distance(ANode.FNode.Location,
              ClosestNode.FNode.Location);
            if (ClosestCost = nil) or (ADistance < ClosestDistance) then
            begin
              ClosestCost := ACost;
              ClosestDistance := ADistance;
            end;
          end;
        end;
        if ClosestCost <> nil then
        begin
          if ClosestCost.FNode1.FBoundary = ABoundary then
          begin
            SubDomainNode := ClosestCost.FNode1;
            OuterBoundaryNode := ClosestCost.FNode2;
            Assert(OuterBoundaryNode.FBoundary = FirstBoundary);
          end
          else
          begin
            SubDomainNode := ClosestCost.FNode2;
            OuterBoundaryNode := ClosestCost.FNode1;
            Assert(SubDomainNode.FBoundary = ABoundary);
            Assert(OuterBoundaryNode.FBoundary = FirstBoundary);
          end;
          if ABoundary.Count = 1 then
          begin
            Assert(SubDomainNode.FSegments.Count = 1);
            Seg2 := SubDomainNode.FSegments[0];
          end
          else
          begin
            ABoundary.SplitSegmentAtNode(SubDomainNode);

            Assert(SubDomainNode.FSegments.Count = 2);
            Seg1 := SubDomainNode.FSegments[0];
            Seg2 := SubDomainNode.FSegments[1];
            if Seg1.FNode2 <> Seg2.FNode1 then
            begin
              SubDomainNode.FSegments.Reverse;
              Seg1 := SubDomainNode.FSegments[0];
              Seg2 := SubDomainNode.FSegments[1];
              Assert(Seg1.FNode2 = Seg2.FNode1);
            end;
          end;
          Assert(Seg2.Node1 = SubDomainNode.FNode);
          SubDomainSegPosition := ABoundary.FSegments.IndexOf(Seg2);
          Assert(SubDomainSegPosition >= 0);

          FirstBoundary.SplitSegmentAtNode(OuterBoundaryNode);
          InsertPosition := OuterBoundaryNode.Position + 1;

          Assert(OuterBoundaryNode.FSegments.Count = 2);
          Seg1 := OuterBoundaryNode.FSegments[0];
          Seg2 := OuterBoundaryNode.FSegments[1];
          if Seg1.FNode2 <> Seg2.FNode1 then
          begin
            OuterBoundaryNode.FSegments.Reverse;
            Seg1 := OuterBoundaryNode.FSegments[0];
            Seg2 := OuterBoundaryNode.FSegments[1];
            Assert(Seg1.FNode2 = Seg2.FNode1);
          end;
          Assert(Seg2.Node1 = OuterBoundaryNode.FNode);
          SegPosition := FirstBoundary.FSegments.IndexOf(Seg1);
          Assert(SegPosition >= 0);

          NewSegment := TSegment.Create(OuterBoundaryNode.FNode,
            SubDomainNode.FNode, stInner, FirstBoundary, self);
          NodeDistance := NewSegment.Length;
          if (NodeDistance > ABoundary.DesiredSpacing) or
            (NodeDistance > FirstBoundary.DesiredSpacing) then
          begin
            NumberOfNodesToInsert := NewSegment.NodesToInsert;
            if NumberOfNodesToInsert > 0 then
            begin
              NewSegment.InsertNodes(NumberOfNodesToInsert);
            end;
          end;

          ReversedSegment := NewSegment.CreateReversedSegment;

          for NodeIndex := 0 to NewSegment.FInnerNodes.Count - 1 do
          begin
            AnInnerNode := NewSegment.FInnerNodes[NodeIndex];
            NewNode := TNodeInBoundary.Create(AnInnerNode, FirstBoundary,
              NewSegment);
            FirstBoundary.Insert(InsertPosition, NewNode);
            Inc(InsertPosition);
            BoundaryQuadTree.AddPoint(NewNode.FNode.X, NewNode.FNode.Y,
              NewNode);
          end;

          for NodeIndex := SubDomainNode.Position to ABoundary.Count - 2 do
          begin
            ANode := ABoundary[NodeIndex];
            FirstBoundary.Insert(InsertPosition, ANode);
            ANode.FBoundary := FirstBoundary;
            Inc(InsertPosition);
            BoundaryQuadTree.AddPoint(ANode.FNode.X, ANode.FNode.Y, ANode);
          end;
          for NodeIndex := 0 to SubDomainNode.Position - 1 do
          begin
            ANode := ABoundary[NodeIndex];
            FirstBoundary.Insert(InsertPosition, ANode);
            ANode.FBoundary := FirstBoundary;
            Inc(InsertPosition);
            BoundaryQuadTree.AddPoint(ANode.FNode.X, ANode.FNode.Y, ANode);
          end;

          NewNode := TNodeInBoundary.Create(ReversedSegment.Node1,
            FirstBoundary, ReversedSegment);
          FirstBoundary.Insert(InsertPosition, NewNode);
          Inc(InsertPosition);
          BoundaryQuadTree.AddPoint(NewNode.FNode.X, NewNode.FNode.Y, NewNode);
          if ABoundary.Count = 1 then
          begin
            NewNode.FSegments.Insert(0, NewSegment);
            if NewNode.FNode.FSegments.IndexOf(NewSegment) < 0 then
            begin
              NewNode.FNode.FSegments.Add(NewSegment);
            end;
          end;

          for NodeIndex := 0 to ReversedSegment.FInnerNodes.Count - 1 do
          begin
            AnInnerNode := ReversedSegment.FInnerNodes[NodeIndex];
            NewNode := TNodeInBoundary.Create(AnInnerNode, FirstBoundary,
              ReversedSegment);
            FirstBoundary.Insert(InsertPosition, NewNode);
            Inc(InsertPosition);
            BoundaryQuadTree.AddPoint(NewNode.FNode.X, NewNode.FNode.Y,
              NewNode);
          end;

          NewNode := TNodeInBoundary.Create(ReversedSegment.Node2,
            FirstBoundary, ReversedSegment);
          FirstBoundary.Insert(InsertPosition, NewNode);
          BoundaryQuadTree.AddPoint(NewNode.FNode.X, NewNode.FNode.Y, NewNode);

          SegInsertPosition := SegPosition + 1;
          FirstBoundary.FSegments.Insert(SegInsertPosition, NewSegment);
          Inc(SegInsertPosition);
          if ABoundary.Count > 1 then
          begin
            for SegmentIndex := SubDomainSegPosition to ABoundary.FSegments.
              Count - 1 do
            begin
              FirstBoundary.FSegments.Insert(SegInsertPosition,
                ABoundary.FSegments[SegmentIndex]);
              Inc(SegInsertPosition);
            end;
            for SegmentIndex := 0 to SubDomainSegPosition - 1 do
            begin
              FirstBoundary.FSegments.Insert(SegInsertPosition,
                ABoundary.FSegments[SegmentIndex]);
              Inc(SegInsertPosition);
            end;
          end;
          FirstBoundary.FSegments.Insert(SegInsertPosition, ReversedSegment);

          FirstBoundary.RenumberNodes;

          ABoundary.FSegments.OwnsObjects := False;
          ABoundary.RemoveSelfFromAllNodes;

          FBoundaries.Delete(BoundaryIndex);
          BoundariesChanged := True;
        end;
      end;
    end;
    Assert(FBoundaries.Count = 1);

  finally
    BoundaryQuadTree.Free;
  end;
end;

procedure TQuadMeshCreator.RenumberNodes;
  procedure AddElements(ABoundary: TBoundary);
  var
    Index: Integer;
  begin
    if ABoundary.SubPartCount > 0 then
    begin
      for Index := 0 to ABoundary.SubPartCount - 1 do
      begin
        AddElements(ABoundary.SubParts[Index]);
      end;
      ABoundary.FSubParts.OwnsObjects := False;
      ABoundary.Free;
    end
    else
    begin
      if ABoundary.NodeCount > 0 then
      begin
        FElementList.Add(ABoundary);
      end
      else
      begin
        ABoundary.Free;
      end;
    end;
  end;

var
  NodeIndex: Integer;
begin
  Assert(FBoundaries.Count = 1);
  AddElements(FBoundaries[0]);
  FBoundaries.OwnsObjects := False;

  for NodeIndex := 0 to NodeCount - 1 do
  begin
    FNodeList.Add(NodeObjects[NodeIndex]);
  end;
  FNodes.OwnsObjects := False;

  RenumberMesh(self);

  FElementList.Sort(TIElementComparer.Construct(
    function(const L, R: IElement): Integer
    begin
      result := L.ElementNumber - R.ElementNumber;
    end));

  FNodeList.Sort(TINodeComparer.Construct(
    function(const L, R: INode): Integer
    begin
      result := L.NodeNumber - R.NodeNumber;
    end));

end;

procedure TQuadMeshCreator.SetNodeAdjustmentMethod
  (const Value: TNodeAdjustmentMethod);
begin
  FNodeAdjustmentMethod := Value;
end;

procedure TQuadMeshCreator.SetNodeTypes;
var
  Index: Integer;
begin
  Assert(FBoundaries.Count > 0);
  FBoundaries[0].SetNodeTypes(ntEdge);
  for Index := 1 to FBoundaries.Count - 1 do
  begin
    FBoundaries[Index].SetNodeTypes(ntSubDomain)
  end;
end;

procedure TQuadMeshCreator.SetSegmentTypes;
var
  Index: Integer;
begin
  Assert(FBoundaries.Count > 0);
  FBoundaries[0].FSegmentType := stEdge;
  for Index := 1 to FBoundaries.Count - 1 do
  begin
    FBoundaries[Index].FSegmentType := stSubDomain;
  end;
end;

procedure TQuadMeshCreator.SetSixNodeClosureMethod
  (const Value: TSixNodeClosureMethod);
begin
  FSixNodeClosureMethod := Value;
end;

function TQuadMeshCreator._AddRef: Integer;
begin
  result := 1;
  // do nothing
end;

function TQuadMeshCreator._Release: Integer;
begin
  result := 1;
  // do nothing
end;

{ TNode }

Type
  TTriangeleCalculationValues = record
    P: double;
    Q: double;
    b: double;
    M: TPoint2D;
    APrime: double;
  end;

procedure TNode.AdjustPositionGiuliani;
var
  ElementIndex: Integer;
  AnElement: TBoundary;
  NodeIndex: Integer;
  NeighborIndexK: Integer;
  NeighborK: TNode;
  NeighborIndexJ: Integer;
  NeighborJ: TNode;
  R: double;
  TotalArea: double;
  TotalBase: double;
  MeanHeight: double;
  MeanBase: double;
  W1: double;
  W2: Extended;
  TriVals: array of TTriangeleCalculationValues;
  S1: double;
  S2: double;
  S3: double;
  S4: double;
  S5: double;
  Index: Integer;
  L: TPoint2D;
begin
  if FNodeType <> ntInner then
  begin
    Exit;
  end;
  TotalArea := 0.0;
  TotalBase := 0.0;
  SetLength(TriVals, FElements.Count);
  for ElementIndex := 0 to FElements.Count - 1 do
  begin
    AnElement := FElements[ElementIndex];
    NodeIndex := AnElement.IndexOfNode(self);
    Assert(NodeIndex >= 0);
    NeighborIndexK := NodeIndex + 1;
    Assert(NeighborIndexK < AnElement.Count);
    NeighborK := AnElement[NeighborIndexK].FNode;

    NeighborIndexJ := NodeIndex - 1;
    if NeighborIndexJ < 0 then
    begin
      Assert(AnElement[0] = AnElement[AnElement.Count - 1]);
      NeighborIndexJ := AnElement.Count - 2;
    end;
    NeighborJ := AnElement[NeighborIndexJ].FNode;

    // Giuliani Eq. 2.5
    // APrime is the area of the triangle before the node position is changed.
    // b is the length of the base of the triangle.
    TriVals[ElementIndex].P := NeighborJ.Y - NeighborK.Y;
    TriVals[ElementIndex].Q := NeighborK.X - NeighborJ.X;
    R := NeighborJ.X * NeighborK.Y - NeighborK.X * NeighborJ.Y;
    TriVals[ElementIndex].APrime :=
      (TriVals[ElementIndex].P * X + TriVals[ElementIndex].Q * Y + R) / 2;
    TriVals[ElementIndex].b :=
      sqrt(Sqr(TriVals[ElementIndex].P) + Sqr(TriVals[ElementIndex].Q));

    TriVals[ElementIndex].M.X := (NeighborJ.X + NeighborK.X) / 2;
    TriVals[ElementIndex].M.Y := (NeighborJ.Y + NeighborK.Y) / 2;

    // Eq 2.6
    TotalArea := TotalArea + TriVals[ElementIndex].APrime;
    // Eq. 2.7
    TotalBase := TotalBase + TriVals[ElementIndex].b;

  end;
  // Eq. 2.8
  MeanHeight := 2 * TotalArea / TotalBase;
  // Eq. 2.9
  MeanBase := TotalBase / FElements.Count;

  if (MeanHeight = 0) or (MeanBase = 0) then
  begin
    Exit;
  end;

  // Eq. 2.14
  W1 := 1 / Sqr(MeanHeight);
  W2 := 4 / Sqr(MeanBase);

  S1 := 0.0;
  S2 := 0.0;
  S3 := 0.0;
  S4 := 0.0;
  S5 := 0.0;
  for Index := 0 to Length(TriVals) - 1 do
  begin
    if FQuadMeshCreator.NodeAdjustmentMethod = namSarrateHuerta then
    begin
      MeanHeight := TriVals[Index].b / 2;
    end;

    S1 := S1 + 1 / Sqr(TriVals[Index].b) *
      (W1 * Sqr(TriVals[Index].P) + W2 * Sqr(TriVals[Index].Q));
    S2 := S2 + TriVals[Index].P * TriVals[Index].Q / Sqr(TriVals[Index].b) *
      (W1 - W2);
    S3 := S3 + 1 / Sqr(TriVals[Index].b) *
      (W1 * TriVals[Index].P * (MeanHeight * TriVals[Index].b - 2 *
      TriVals[Index].APrime) - W2 * TriVals[Index].Q *
      ((X - TriVals[Index].M.X) * TriVals[Index].Q - (Y - TriVals[Index].M.Y) *
      TriVals[Index].P));
    S4 := S4 + 1 / Sqr(TriVals[Index].b) *
      (W1 * Sqr(TriVals[Index].Q) + W2 * Sqr(TriVals[Index].P));
    S5 := S5 + 1 / Sqr(TriVals[Index].b) *
      (W1 * TriVals[Index].Q * (MeanHeight * TriVals[Index].b - 2 *
      TriVals[Index].APrime) - W2 * TriVals[Index].P *
      ((Y - TriVals[Index].M.Y) * TriVals[Index].P - (X - TriVals[Index].M.X) *
      TriVals[Index].Q));
  end;

  L.X := (S3 * S4 - S2 * S5) / (S1 * S4 - Sqr(S2));
  L.Y := (S1 * S5 - S2 * S3) / (S1 * S4 - Sqr(S2));

  X := X + L.X;
  Y := Y + L.Y;

end;

procedure TNode.AdjustPositionLagrange;
var
  ElementIndex: Integer;
  AnElement: TBoundary;
  NodeIndex: Integer;
  PriorNodeIndex: Integer;
  SubsequentNodeIndex: Integer;
  PriorNode: TNodeInBoundary;
  SubsequentNode: TNodeInBoundary;
  APoly: TPolygon2D;
  FoundNodes: TNodeList;
  NodeComparer: INodeComparer;
begin
  if FNodeType <> ntInner then
  begin
    Exit;
  end;

  FoundNodes := TNodeList.Create;
  try
    for ElementIndex := 0 to FElements.Count - 1 do
    begin
      AnElement := FElements[ElementIndex];
      // The last point in AnElement is a duplicate of the first point
      // so there are really 4 nodes in the element.
      Assert(AnElement.Count = 5);
      NodeIndex := AnElement.IndexOfNode(self);
      Assert(NodeIndex >= 0);
      SubsequentNodeIndex := NodeIndex + 1;
      // because the last node is a duplicate of the first, there is no need
      // to adjust SubsequentNodeIndex for the possibility of being past the
      // end of  AnElement's nodes.
      SubsequentNode := AnElement[SubsequentNodeIndex];
      PriorNodeIndex := NodeIndex - 1;
      if PriorNodeIndex < 0 then
      begin
        PriorNodeIndex := AnElement.Count - 2;
      end;
      PriorNode := AnElement[PriorNodeIndex];
      if FoundNodes.IndexOf(PriorNode.FNode) < 0 then
      begin
        FoundNodes.Add(PriorNode.FNode);
      end;
      if FoundNodes.IndexOf(SubsequentNode.FNode) < 0 then
      begin
        FoundNodes.Add(SubsequentNode.FNode);
      end;
    end;

    NodeComparer := TNodeAngleComparer.Create(self);
    try
      FoundNodes.Sort(NodeComparer);
    finally
      NodeComparer := nil;
    end;

    SetLength(APoly, FoundNodes.Count);
    for NodeIndex := 0 to FoundNodes.Count - 1 do
    begin
      APoly[NodeIndex] := FoundNodes[NodeIndex].Location;
    end;
  finally
    FoundNodes.Free;
  end;

  if Area(APoly) <> 0 then
  begin
    FLocation := Centroid(APoly);
  end;
end;

constructor TNode.Create(QuadMeshCreator: TQuadMeshCreator;
DesiredSpacing: double);
begin
  FElements := TBoundaryList.Create;
  FSegments := TSegmentList.Create;
  FQuadMeshCreator := QuadMeshCreator;
  FNodeNumber := FQuadMeshCreator.FNodes.Add(self);
  Assert(DesiredSpacing > 0);
  FDesiredSpacing := DesiredSpacing;
  FDesiredElementCount := 4;
end;

destructor TNode.Destroy;
begin
  FSegments.Free;
  FElements.Free;
  inherited;
end;

function TNode.DiagonalSwapping: boolean;
{
  based on Zhu and others, 1991, International Journal for Numerical
  Methods in Engineering 32: 849-866.

  Original element configuration
      B
     /|\
    / | \
   /  |  \
  E   |   D
  |   |   |
  | 2 | 1 |
  |   |   |
  C   |   F
   \  |  /
    \ | /
     \|/
      A (self)

  Side A-B is eliminated and replaced by either C-D or E-F
}
var
  ElementIndex: Integer;
  Element1: TBoundary;
  NodeAIndex: Integer;
  NodeBIndex: Integer;
  NodeB: TNode;
  AB_Count: Integer;
  InnerElementIndex: Integer;
  Element2: TBoundary;
  NodeDIndex: Integer;
  NodeD: TNode;
  NodeFIndex: Integer;
  NodeF: TNode;
  NodeAIndex2: Integer;
  NodeBIndex2: Integer;
  NodeEIndex: Integer;
  NodeE: TNode;
  NodeCIndex: Integer;
  NodeC: TNode;
  CD_Count: Integer;
  EF_Count: Integer;
  StraightElement: boolean;
  BoundaryNodes: TNodeList;
begin
  result := False;
  for ElementIndex := 0 to FElements.Count - 1 do
  begin
    Element1 := FElements[ElementIndex];
    NodeAIndex := Element1.IndexOfNode(self);
    Assert(NodeAIndex >= 0);

    NodeBIndex := NodeAIndex + 1;
    if NodeBIndex >= Element1.Count then
    begin
      NodeBIndex := NodeAIndex - 3;
      Assert(NodeBIndex >= 0);
    end;
    NodeB := Element1[NodeBIndex].FNode;

    if (FNodeType <> ntInner) or (NodeB.FNodeType <> ntInner) then
    begin
      Continue;
    end
    else
    begin
      AB_Count := NodeB.FElements.Count + FElements.Count;
      if AB_Count >= FDesiredElementCount + NodeB.FDesiredElementCount + 1 then
      begin
        for InnerElementIndex := 0 to FElements.Count - 1 do
        begin
          if InnerElementIndex = ElementIndex then
          begin
            Continue;
          end;
          Element2 := FElements[InnerElementIndex];
          NodeAIndex2 := Element2.IndexOfNode(self);
          NodeBIndex2 := Element2.IndexOfNode(NodeB);
          if (NodeAIndex2 >= 0) and (NodeBIndex2 >= 0) then
          begin
            NodeDIndex := NodeAIndex + 2;
            if NodeDIndex >= Element1.Count then
            begin
              NodeDIndex := NodeAIndex - 2;
              Assert(NodeDIndex >= 0);
            end;
            NodeD := Element1[NodeDIndex].FNode;

            NodeFIndex := NodeBIndex + 2;
            if NodeFIndex >= Element1.Count then
            begin
              NodeFIndex := NodeBIndex - 2;
              Assert(NodeFIndex >= 0);
            end;
            NodeF := Element1[NodeFIndex].FNode;

            NodeEIndex := NodeAIndex2 + 2;
            if NodeEIndex >= Element2.Count then
            begin
              NodeEIndex := NodeAIndex2 - 2;
              Assert(NodeEIndex >= 0);
            end;
            NodeE := Element2[NodeEIndex].FNode;

            NodeCIndex := NodeBIndex2 + 2;
            if NodeCIndex >= Element2.Count then
            begin
              NodeCIndex := NodeBIndex2 - 2;
              Assert(NodeCIndex >= 0);
            end;
            NodeC := Element2[NodeCIndex].FNode;

            CD_Count := NodeC.FElements.Count + NodeD.FElements.Count;
            EF_Count := NodeE.FElements.Count + NodeF.FElements.Count;
            if EF_Count > CD_Count then
            begin
              if AB_Count - CD_Count >= 3 -
                (NodeC.FDesiredElementCount + NodeD.FDesiredElementCount) +
                (FDesiredElementCount + NodeB.FDesiredElementCount) then
              begin
                StraightElement :=
                  (Collinear(FLocation, NodeF.FLocation, NodeD.FLocation) and
                  Collinear(NodeC.FLocation, NodeF.FLocation, NodeD.FLocation));
                StraightElement := StraightElement or
                  (Collinear(NodeC.FLocation, NodeB.FLocation, NodeD.FLocation)
                  and Collinear(NodeE.FLocation, NodeB.FLocation,
                  NodeD.FLocation));
                if not StraightElement then
                begin
                  // Exit if the new element would be invalid.
                  if not Intersect(EquateSegment(Location, NodeD.Location),
                    EquateSegment(NodeC.Location, NodeF.Location)) then
                  begin
                    Exit;
                  end;
                  if not Intersect(EquateSegment(NodeC.Location,
                    NodeB.Location), EquateSegment(NodeE.Location,
                    NodeD.Location)) then
                  begin
                    Exit;
                  end;

                  // Exit if the new element would result in an element
                  // with 3 edge nodes in a straight line.
                  BoundaryNodes := TNodeList.Create;
                  try
                    if NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(self);
                    end;
                    if NodeC.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeC);
                    end;
                    if NodeD.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeD);
                    end;
                    if NodeF.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeF);
                    end;
                    if BoundaryNodes.Count = 3 then
                    begin
                      if BadAngle(BoundaryNodes[0].Location,
                        BoundaryNodes[1].Location, BoundaryNodes[2].Location)
                      then
                      begin
                        Exit;
                      end;
                    end;
                    BoundaryNodes.Clear;
                    if NodeB.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeB);
                    end;
                    if NodeC.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeC);
                    end;
                    if NodeD.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeD);
                    end;
                    if NodeE.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeE);
                    end;
                    if BoundaryNodes.Count = 3 then
                    begin
                      if BadAngle(BoundaryNodes[0].Location,
                        BoundaryNodes[1].Location, BoundaryNodes[2].Location)
                      then
                      begin
                        Exit;
                      end;
                    end;
                  finally
                    BoundaryNodes.Free;
                  end;
                  // replace A-B with C-D
                  NodeB.ReplaceNodeInElement(NodeC, Element1);
                  self.ReplaceNodeInElement(NodeD, Element2);
                  self.FElements.Remove(Element2);
                  NodeB.FElements.Remove(Element1);
                  NodeC.FElements.Add(Element1);
                  NodeD.FElements.Add(Element2);
                  result := True;
{$IFDEF DEBUG}
                  Element1.CheckInvalidElement;
                  Element2.CheckInvalidElement;
{$ENDIF}
                  Exit;
                end;
              end;
            end
            else
            begin
              if AB_Count - EF_Count >= 3 -
                (NodeE.FDesiredElementCount + NodeF.FDesiredElementCount) +
                (FDesiredElementCount + NodeB.FDesiredElementCount) then
              begin
                StraightElement :=
                  (Collinear(FLocation, NodeF.FLocation, NodeE.FLocation) and
                  Collinear(NodeC.FLocation, NodeF.FLocation, NodeE.FLocation));
                StraightElement := StraightElement or
                  (Collinear(NodeF.FLocation, NodeB.FLocation, NodeD.FLocation)
                  and Collinear(NodeE.FLocation, NodeB.FLocation,
                  NodeD.FLocation));
                if not StraightElement then
                begin
                  // Exit if the new element would be invalid.
                  if not Intersect(EquateSegment(Location, NodeE.Location),
                    EquateSegment(NodeC.Location, NodeF.Location)) then
                  begin
                    Exit;
                  end;
                  if not Intersect(EquateSegment(NodeB.Location,
                    NodeF.Location), EquateSegment(NodeE.Location,
                    NodeD.Location)) then
                  begin
                    Exit;
                  end;

                  // Exit if the new element would result in an element
                  // with 3 edge nodes in a straight line.
                  BoundaryNodes := TNodeList.Create;
                  try
                    if NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(self);
                    end;
                    if NodeC.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeC);
                    end;
                    if NodeE.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeE);
                    end;
                    if NodeF.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeF);
                    end;
                    if BoundaryNodes.Count = 3 then
                    begin
                      if BadAngle(BoundaryNodes[0].Location,
                        BoundaryNodes[1].Location, BoundaryNodes[2].Location)
                      then
                      begin
                        Exit;
                      end;
                    end;
                    BoundaryNodes.Clear;
                    if NodeB.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeB);
                    end;
                    if NodeD.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeD);
                    end;
                    if NodeE.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeE);
                    end;
                    if NodeF.NodeType in [ntEdge, ntSubDomain] then
                    begin
                      BoundaryNodes.Add(NodeF);
                    end;
                    if BoundaryNodes.Count = 3 then
                    begin
                      if BadAngle(BoundaryNodes[0].Location,
                        BoundaryNodes[1].Location, BoundaryNodes[2].Location)
                      then
                      begin
                        Exit;
                      end;
                    end;
                  finally
                    BoundaryNodes.Free;
                  end;
                  // replace A-B with E-F
                  NodeB.ReplaceNodeInElement(NodeF, Element2);
                  self.ReplaceNodeInElement(NodeE, Element1);
                  self.FElements.Remove(Element1);
                  NodeB.FElements.Remove(Element2);
                  NodeE.FElements.Add(Element1);
                  NodeF.FElements.Add(Element2);
                  result := True;
{$IFDEF DEBUG}
                  Element1.CheckInvalidElement;
                  Element2.CheckInvalidElement;
{$ENDIF}
                  Exit;
                end;
              end;
            end;

            break;
          end;
        end;
      end;
    end;
  end;
end;

procedure TNode.ReplaceNodeInElement(ReplacementNode: TNode;
AnElement: TBoundary);
var
  ASegment: TSegment;
  NodeIndex: Integer;
  SegmentIndex: Integer;
  BoundaryNode: TNodeInBoundary;
begin
  for SegmentIndex := 0 to AnElement.FSegments.Count - 1 do
  begin
    ASegment := AnElement.FSegments[SegmentIndex];
    if ASegment.FNode1 = self then
    begin
      ASegment.FNode1 := ReplacementNode;
    end;
    if ASegment.FNode2 = self then
    begin
      ASegment.FNode2 := ReplacementNode;
    end;
  end;
  NodeIndex := AnElement.FNodes.IndexOf(self);
  if NodeIndex >= 0 then
  begin
    AnElement.FNodes[NodeIndex] := ReplacementNode;
    if NodeIndex = 0 then
    begin
      NodeIndex := AnElement.FNodes.IndexOf(self);
      if NodeIndex >= 0 then
      begin
        AnElement.FNodes[NodeIndex] := ReplacementNode;
      end;
    end;
  end;
  for NodeIndex := 0 to AnElement.Count - 1 do
  begin
    BoundaryNode := AnElement[NodeIndex];
    if BoundaryNode.FNode = self then
    begin
      BoundaryNode.FNode := ReplacementNode;
    end;
  end;
end;

procedure TNode.NodeElimination;
var
  Element1: TBoundary;
  NodePosition1: Integer;
  Element2: TBoundary;
  NodePosition2: Integer;
  OppositeNodePosition2: Integer;
  OppositeBoundaryNode2: TNodeInBoundary;
  BoundaryNode1: TNodeInBoundary;
  ReplacementNode: TNode;
begin
  Element1 := FElements[0];
  NodePosition1 := Element1.IndexOfNode(self);
  Assert(NodePosition1 >= 0);
  BoundaryNode1 := Element1[NodePosition1];

  Element2 := FElements[1];
  NodePosition2 := Element2.IndexOfNode(self);
  Assert(NodePosition2 >= 0);
  OppositeNodePosition2 := NodePosition2 + 2;
  if OppositeNodePosition2 >= Element2.Count then
  begin
    OppositeNodePosition2 := NodePosition2 - 2;
    Assert(OppositeNodePosition2 >= 0);
  end;
  OppositeBoundaryNode2 := Element2[OppositeNodePosition2];

  ReplacementNode := OppositeBoundaryNode2.FNode;
  BoundaryNode1.FNode := ReplacementNode;
  BoundaryNode1.FNode.FElements.Add(Element1);
  Element2.RemoveSelfFromOwnNodes;
  ReplaceNodeInElement(ReplacementNode, Element1);

  Element2.FParent.FSubParts.Remove(Element2);

{$IFDEF DEBUG}
  Element1.CheckInvalidElement;
{$ENDIF}
end;

function TNode.ElementElimination: boolean;
var
  ElementIndex: Integer;
  AnElement: TBoundary;
  BoundaryNodeIndex: Integer;
  OppositeNodeIndex: Integer;
  OppositeNode: TNodeInBoundary;
  CornerIndex1: Integer;
  CornerIndex2: Integer;
  CornerNode1: TNodeInBoundary;
  CornerNode2: TNodeInBoundary;
  InnerElementIndex: Integer;
  AnotherElement: TBoundary;
  OldLocation: TPoint2D;
  NewLocation: TPoint2D;
  TestElement: TBoundary;
  Node0: TNode;
  Node1: TNode;
  Node2: TNode;
  Node3: TNode;
begin
  result := False;
  for ElementIndex := 0 to FElements.Count - 1 do
  begin
    AnElement := FElements[ElementIndex];
    BoundaryNodeIndex := AnElement.IndexOfNode(self);
    Assert(BoundaryNodeIndex >= 0);

    OppositeNodeIndex := BoundaryNodeIndex + 2;
    if OppositeNodeIndex >= AnElement.Count then
    begin
      OppositeNodeIndex := BoundaryNodeIndex - 2;
      Assert(OppositeNodeIndex >= 0);
    end;
    OppositeNode := AnElement[OppositeNodeIndex];
    if (OppositeNode.FNode.FNodeType = ntInner) and
      (OppositeNode.FNode.FElements.Count = 3) then
    begin
      Assert(AnElement[AnElement.Count - 1] = AnElement[0]);

      CornerIndex1 := BoundaryNodeIndex + 1;
      if CornerIndex1 >= AnElement.Count then
      begin
        CornerIndex1 := 1;
      end;
      CornerNode1 := AnElement[CornerIndex1];

      CornerIndex2 := BoundaryNodeIndex - 1;
      if CornerIndex2 < 0 then
      begin
        CornerIndex2 := AnElement.Count - 2;
      end;
      CornerNode2 := AnElement[CornerIndex2];

      NewLocation.X := (CornerNode1.FNode.X + CornerNode2.FNode.X) / 2;
      NewLocation.Y := (CornerNode1.FNode.Y + CornerNode2.FNode.Y) / 2;

      OldLocation := OppositeNode.FNode.FLocation;
      try
        OppositeNode.FNode.FLocation := NewLocation;
        for InnerElementIndex := 0 to OppositeNode.FNode.FElements.Count - 1 do
        begin
          TestElement := OppositeNode.FNode.FElements[InnerElementIndex];
          Node0 := TestElement[0].FNode;
          Node1 := TestElement[1].FNode;
          Node2 := TestElement[2].FNode;
          Node3 := TestElement[3].FNode;
          if not Intersect(EquateSegment(Node0.Location, Node2.Location),
            EquateSegment(Node1.Location, Node3.Location)) then
          begin
            Exit;
          end;
        end;
      finally
        OppositeNode.FNode.FLocation := OldLocation;
      end;

      OldLocation := FLocation;
      try
        FLocation := NewLocation;
        for InnerElementIndex := 0 to FElements.Count - 1 do
        begin
          TestElement := FElements[InnerElementIndex];
          Node0 := TestElement[0].FNode;
          Node1 := TestElement[1].FNode;
          Node2 := TestElement[2].FNode;
          Node3 := TestElement[3].FNode;
          if not Intersect(EquateSegment(Node0.Location, Node2.Location),
            EquateSegment(Node1.Location, Node3.Location)) then
          begin
            Exit;
          end;
        end;
      finally
        FLocation := OldLocation;
      end;

      result := True;
      OppositeNode.FNode.FLocation := NewLocation;

      for InnerElementIndex := 0 to FElements.Count - 1 do
      begin
        if InnerElementIndex = ElementIndex then
        begin
          Continue;
        end;
        AnotherElement := FElements[InnerElementIndex];
        ReplaceNodeInElement(OppositeNode.FNode, AnotherElement);
        OppositeNode.FNode.FElements.Add(AnotherElement);
{$IFDEF DEBUG}
        AnotherElement.CheckInvalidElement;
{$ENDIF}
      end;

      AnElement.RemoveSelfFromOwnNodes;
      AnElement.FParent.FSubParts.Remove(AnElement);

      break;
    end;
  end;
end;

function TNode.GetActiveElement(Index: Integer): IElement;
begin
  result := FElements[Index];
end;

function TNode.GetActiveElementCount: Integer;
begin
  result := FElements.Count;
end;

function TNode.GetLocation: TPoint2D;
begin
  result := FLocation;
end;

function TNode.GetNodeNumber: Integer;
begin
  result := FNodeNumber;
end;

function TNode.GetNodeType: TNodeType;
begin
  result := FNodeType;
end;

function TNode.ImproveTopology1: boolean;
begin
  result := False;
  if FNodeType = ntInner then
  begin
    case FElements.Count of
      2:
        begin
          NodeElimination;
          result := True;
        end;
      3:
        begin
          result := ElementElimination;
          if not result then
          begin
            result := SideElimination;
          end;
        end;
    end;
  end;
end;

function TNode.ImproveTopology2: boolean;
begin
  result := DiagonalSwapping;
end;

procedure TNode.SetLocation(const Value: TPoint2D);
begin
  FLocation := Value;
end;

procedure TNode.SetNodeNumber(Value: Integer);
begin
  FNodeNumber := Value;
end;

function TNode.SideElimination: boolean;
{
         ElementToKeep2
               NodeG
               /   \
            /        \
         /             \
      NodeC           NodeD
         \             /
           \         /
             \     /
              NodeB
                |
ElementToDel1   |  ElementToDelete2
                |
             A = self
              /  \
            /       \
          /           \
       NodeE         NodeF
         \             /
           \         /
             \     /
              NodeH
         ElementToKeep1

  In ElementToKeep1 replace NodeB with NodeF
  In ElementToKeep2 replace self with NodeC
  OR
  In ElementToKeep1 replace NodeB with NodeE
  In ElementToKeep2 replace self with NodeD

  Delete ElementToDelete1 and ElementToDelete2
  Delete NodeB and self.
}
var
  ElementIndex: Integer;
  AnElement: TBoundary;
  BoundaryNodeIndex: Integer;
  NodeBIndex: Integer;
  NodeB: TNode;
  InnerElementIndex: Integer;
  FirstElementToDelete: TBoundary;
  NextNodeIndex2: Integer;
  NextElementIndex: Integer;
  SecondElementToDelete: TBoundary;
  ElementToKeep1: TBoundary;
  ElementToKeep2: TBoundary;
  NodeCIndex: Integer;
  NodeC: TNode;
  NodeDIndex: Integer;
  NodeD: TNode;
  NodeEIndex: Integer;
  NodeE: TNode;
  NodeFIndex: Integer;
  NodeF: TNode;
  CFCount: Integer;
  DECount: Integer;
  NodeBIndexToDelete: Integer;
  procedure HandleSideToBeDeleted;
  var
    KeepEleIndex: Integer;
    BoundaryNodes: TNodeList;
    NodeIndex: Integer;
    ANode: TNode;
    CanHandleCF: boolean;
    CanHandleDE: boolean;
    NodeGIndex: Integer;
    NodeG: TNode;
    NodeHIndex: Integer;
    NodeH: TNode;
  begin
    NextElementIndex := 3 - ElementIndex - InnerElementIndex;
    Assert(NextElementIndex < FElements.Count);
    SecondElementToDelete := FElements[ElementIndex];
    Assert((FirstElementToDelete = AnElement) or
      (SecondElementToDelete = AnElement));

    ElementToKeep1 := FElements[NextElementIndex];

    ElementToKeep2 := nil;
    for KeepEleIndex := 0 to NodeB.FElements.Count - 1 do
    begin
      if (NodeB.FElements[KeepEleIndex] <> FirstElementToDelete) and
        (NodeB.FElements[KeepEleIndex] <> SecondElementToDelete) then
      begin
        ElementToKeep2 := NodeB.FElements[KeepEleIndex];
        break;
      end;
    end;
    Assert(ElementToKeep2 <> nil);

    NodeCIndex := FirstElementToDelete.IndexOfNode(self);
    Assert(NodeCIndex >= 0);
    Inc(NodeCIndex, 2);
    if NodeCIndex >= FirstElementToDelete.Count then
    begin
      Dec(NodeCIndex, 4);
      Assert(NodeCIndex >= 0);
    end;
    NodeC := FirstElementToDelete[NodeCIndex].FNode;

    NodeDIndex := SecondElementToDelete.IndexOfNode(self);
    Assert(NodeDIndex >= 0);
    Inc(NodeDIndex, 2);
    if NodeDIndex >= SecondElementToDelete.Count then
    begin
      Dec(NodeDIndex, 4);
      Assert(NodeDIndex >= 0);
    end;
    NodeD := SecondElementToDelete[NodeDIndex].FNode;

    NodeEIndex := FirstElementToDelete.IndexOfNode(NodeB);
    Assert(NodeEIndex >= 0);
    Inc(NodeEIndex, 2);
    if NodeEIndex >= FirstElementToDelete.Count then
    begin
      Dec(NodeEIndex, 4);
      Assert(NodeEIndex >= 0);
    end;
    NodeE := FirstElementToDelete[NodeEIndex].FNode;

    NodeFIndex := SecondElementToDelete.IndexOfNode(NodeB);
    Assert(NodeFIndex >= 0);
    Inc(NodeFIndex, 2);
    if NodeFIndex >= SecondElementToDelete.Count then
    begin
      Dec(NodeFIndex, 4);
      Assert(NodeFIndex >= 0);
    end;
    NodeF := SecondElementToDelete[NodeFIndex].FNode;

    NodeGIndex := ElementToKeep2.IndexOfNode(NodeB);
    Assert(NodeGIndex >= 0);
    Inc(NodeGIndex, 2);
    if NodeGIndex >= ElementToKeep2.Count then
    begin
      Dec(NodeGIndex, 4);
      Assert(NodeGIndex >= 0);
    end;
    NodeG := ElementToKeep2[NodeGIndex].FNode;

    NodeHIndex := ElementToKeep1.IndexOfNode(self);
    Assert(NodeHIndex >= 0);
    Inc(NodeHIndex, 2);
    if NodeHIndex >= ElementToKeep1.Count then
    begin
      Dec(NodeHIndex, 4);
      Assert(NodeHIndex >= 0);
    end;
    NodeH := ElementToKeep1[NodeHIndex].FNode;

    BoundaryNodes := TNodeList.Create;
    try
      CanHandleCF := Intersect(EquateSegment(NodeG.Location, NodeF.Location),
        EquateSegment(NodeC.Location, NodeD.Location));
      CanHandleCF := CanHandleCF and
        Intersect(EquateSegment(NodeC.Location, NodeH.Location),
        EquateSegment(NodeE.Location, NodeF.Location));
      if CanHandleCF then
      begin
        for NodeIndex := 0 to ElementToKeep1.Count - 2 do
        begin
          ANode := ElementToKeep1[NodeIndex].FNode;
          if (ANode <> self) and (ANode.NodeType in [ntEdge, ntSubDomain]) then
          begin
            BoundaryNodes.Add(ANode);
          end;
        end;
        if NodeC.NodeType in [ntEdge, ntSubDomain] then
        begin
          BoundaryNodes.Add(NodeC);
        end;
        if BoundaryNodes.Count = 3 then
        begin
          CanHandleCF := not BadAngle(BoundaryNodes[0].Location,
            BoundaryNodes[1].Location, BoundaryNodes[2].Location)
        end
        else
        begin
          CanHandleCF := True;
        end;
        if CanHandleCF then
        begin
          BoundaryNodes.Clear;
          for NodeIndex := 0 to ElementToKeep2.Count - 2 do
          begin
            ANode := ElementToKeep2[NodeIndex].FNode;
            if (ANode <> NodeB) and (ANode.NodeType in [ntEdge, ntSubDomain])
            then
            begin
              BoundaryNodes.Add(ANode);
            end;
          end;
          if NodeF.NodeType in [ntEdge, ntSubDomain] then
          begin
            BoundaryNodes.Add(NodeF);
          end;
          if BoundaryNodes.Count = 3 then
          begin
            CanHandleCF := not BadAngle(BoundaryNodes[0].Location,
              BoundaryNodes[1].Location, BoundaryNodes[2].Location)
          end
          else
          begin
            CanHandleCF := True;
          end;
        end;
      end;

      CanHandleDE := Intersect(EquateSegment(NodeG.Location, NodeE.Location),
        EquateSegment(NodeC.Location, NodeD.Location));
      CanHandleDE := CanHandleDE and
        Intersect(EquateSegment(NodeD.Location, NodeH.Location),
        EquateSegment(NodeE.Location, NodeF.Location));
      if CanHandleDE then
      begin

        BoundaryNodes.Clear;
        for NodeIndex := 0 to ElementToKeep1.Count - 2 do
        begin
          ANode := ElementToKeep1[NodeIndex].FNode;
          if (ANode <> self) and (ANode.NodeType in [ntEdge, ntSubDomain]) then
          begin
            BoundaryNodes.Add(ANode);
          end;
        end;
        if NodeD.NodeType in [ntEdge, ntSubDomain] then
        begin
          BoundaryNodes.Add(NodeD);
        end;
        if BoundaryNodes.Count = 3 then
        begin
          CanHandleDE := not BadAngle(BoundaryNodes[0].Location,
            BoundaryNodes[1].Location, BoundaryNodes[2].Location)
        end
        else
        begin
          CanHandleDE := True;
        end;
        if CanHandleDE then
        begin
          BoundaryNodes.Clear;
          for NodeIndex := 0 to ElementToKeep2.Count - 2 do
          begin
            ANode := ElementToKeep2[NodeIndex].FNode;
            if (ANode <> NodeB) and (ANode.NodeType in [ntEdge, ntSubDomain])
            then
            begin
              BoundaryNodes.Add(ANode);
            end;
          end;
          if NodeE.NodeType in [ntEdge, ntSubDomain] then
          begin
            BoundaryNodes.Add(NodeE);
          end;
          if BoundaryNodes.Count = 3 then
          begin
            CanHandleDE := not BadAngle(BoundaryNodes[0].Location,
              BoundaryNodes[1].Location, BoundaryNodes[2].Location)
          end
          else
          begin
            CanHandleDE := True;
          end;
        end;
      end;
    finally
      BoundaryNodes.Free;
    end;

    if CanHandleDE and CanHandleCF then
    begin
      CFCount := NodeC.ElementCount - NodeC.FDesiredElementCount +
        NodeF.ElementCount - NodeF.FDesiredElementCount;
      DECount := NodeD.ElementCount - NodeD.FDesiredElementCount +
        NodeE.ElementCount - NodeE.FDesiredElementCount;

      if CFCount <= DECount then
      begin
        // Make new edge joining nodes C and F
        NodeB.ReplaceNodeInElement(NodeF, ElementToKeep2);
        NodeF.FElements.Add(ElementToKeep2);
        ReplaceNodeInElement(NodeC, ElementToKeep1);
        NodeC.FElements.Add(ElementToKeep1);
      end
      else
      begin
        // Make new edge joining nodes D and E
        NodeB.ReplaceNodeInElement(NodeE, ElementToKeep2);
        NodeE.FElements.Add(ElementToKeep2);
        ReplaceNodeInElement(NodeD, ElementToKeep1);
        NodeD.FElements.Add(ElementToKeep1);
      end;
    end
    else if CanHandleCF then
    begin
      // Make new edge joining nodes C and F
      NodeB.ReplaceNodeInElement(NodeF, ElementToKeep2);
      NodeF.FElements.Add(ElementToKeep2);
      ReplaceNodeInElement(NodeC, ElementToKeep1);
      NodeC.FElements.Add(ElementToKeep1);
    end
    else if CanHandleDE then
    begin
      // Make new edge joining nodes D and E
      NodeB.ReplaceNodeInElement(NodeE, ElementToKeep2);
      NodeE.FElements.Add(ElementToKeep2);
      ReplaceNodeInElement(NodeD, ElementToKeep1);
      NodeD.FElements.Add(ElementToKeep1);
    end
    else
    begin
      Exit;
    end;

    FirstElementToDelete.RemoveSelfFromOwnNodes;
    FirstElementToDelete.FParent.FSubParts.Remove(FirstElementToDelete);

    SecondElementToDelete.RemoveSelfFromOwnNodes;
    SecondElementToDelete.FParent.FSubParts.Remove(SecondElementToDelete);

    NodeBIndexToDelete := FQuadMeshCreator.FNodes.IndexOf(NodeB);
    Assert(NodeBIndexToDelete >= 0);
    FQuadMeshCreator.FNodes.Delete(NodeBIndexToDelete);

    result := True;
{$IFDEF DEBUG}
    ElementToKeep1.CheckInvalidElement;
    ElementToKeep2.CheckInvalidElement;
{$ENDIF}
  end;

begin
  result := False;
  Assert(FElements.Count = 3);
  if FNodeType <> ntInner then
  begin
    Exit;
  end;
  for ElementIndex := 0 to FElements.Count - 1 do
  begin
    AnElement := FElements[ElementIndex];
    BoundaryNodeIndex := AnElement.IndexOfNode(self);
    Assert(BoundaryNodeIndex >= 0);

    NodeBIndex := BoundaryNodeIndex + 1;
    Assert(NodeBIndex < AnElement.Count);

    NodeB := AnElement[NodeBIndex].FNode;
    if (NodeB.FNodeType = ntInner) and (NodeB.FElements.Count = 3) then
    begin
      for InnerElementIndex := 0 to FElements.Count - 1 do
      begin
        if InnerElementIndex = ElementIndex then
        begin
          Continue
        end;
        FirstElementToDelete := FElements[InnerElementIndex];
        NextNodeIndex2 := FirstElementToDelete.IndexOfNode(NodeB);
        if NextNodeIndex2 >= 0 then
        begin
          HandleSideToBeDeleted;
          if result then
          begin
            Exit;
          end;
        end;
      end;
    end;

    NodeBIndex := BoundaryNodeIndex - 1;
    if NodeBIndex < 0 then
    begin
      NodeBIndex := AnElement.Count - 2;
      Assert(NodeBIndex >= 0);
    end;
    NodeB := AnElement[NodeBIndex].FNode;
    if (NodeB.FNodeType = ntInner) and (NodeB.FElements.Count = 3) then
    begin
      for InnerElementIndex := 0 to FElements.Count - 1 do
      begin
        if InnerElementIndex = ElementIndex then
        begin
          Continue
        end;
        FirstElementToDelete := FElements[InnerElementIndex];
        NextNodeIndex2 := FirstElementToDelete.IndexOfNode(NodeB);
        if NextNodeIndex2 >= 0 then
        begin
          HandleSideToBeDeleted;
          if result then
          begin
            Exit;
          end;
        end;
      end;
    end;

  end;
end;

{ TNodeInBoundary }

constructor TNodeInBoundary.Create(Node: TNode; Boundary: TBoundary;
Segment: TSegment);
begin
  FNode := Node;
  Assert(Boundary <> nil);
  Assert(Segment <> nil);
  FSegments := TSegmentList.Create;
  if FNode.FElements.IndexOf(Boundary) < 0 then
  begin
    FNode.FElements.Add(Boundary);
    FNode.FSegments.Add(Segment);
  end;
  FBoundary := Boundary;
  FSegments.Add(Segment);
  FBoundary.FQuadMeshCreator.FBoundaryNodes.Add(self);
end;

destructor TNodeInBoundary.Destroy;
begin
  FSegments.Free;
  inherited;
end;

function TNodeInBoundary.GetDesiredSpacing: double;
begin
  result := FNode.DesiredSpacing;
end;

function TNodeInBoundary.GetElementCount: Integer;
begin
  result := FNode.ElementCount;
end;

function TNodeInBoundary.GetLocation: TPoint2D;
begin
  result := FNode.Location;
end;

function TNodeInBoundary.GetNodeType: TNodeType;
begin
  result := FNode.NodeType;
end;

function TNodeInBoundary.GetX: double;
begin
  result := FNode.X;
end;

function TNodeInBoundary.GetY: double;
begin
  result := FNode.Y;
end;

function TNodeInBoundary.OnSameSegment(ANode: TNodeInBoundary): boolean;
var
  SegmentIndex: Integer;
  ASegment: TSegment;
begin
  result := False;
  for SegmentIndex := 0 to FSegments.Count - 1 do
  begin
    ASegment := FSegments[SegmentIndex];
    if ANode.FSegments.IndexOf(ASegment) >= 0 then
    begin
      result := True;
      Exit;
    end;
  end;
end;

{ TSegment }

function TSegment.NodesToInsert: Integer;
var
  NodeDistance: TFloat;
  NStar: Extended;
  NodeSpacing: double;
  Count: Integer;
  LastSpacing: double;
  ElapsedDistance: double;
  NextDistance: double;
begin
  NodeDistance := Distance(Node1.Location, Node2.Location);

  if Node1.DesiredSpacing = Node2.DesiredSpacing then
  begin
    NStar := NodeDistance / Node1.DesiredSpacing;
  end
  else
  begin
    if Node1.DesiredSpacing < Node2.DesiredSpacing then
    begin
      NodeSpacing := Node1.DesiredSpacing;
      LastSpacing := Node2.DesiredSpacing;
    end
    else
    begin
      NodeSpacing := Node2.DesiredSpacing;
      LastSpacing := Node1.DesiredSpacing;
    end;
    Count := 0;
    ElapsedDistance := 0;
    repeat
      NextDistance := ElapsedDistance + NodeSpacing;
      if NextDistance >= NodeDistance then
      begin
        break;
      end;
      ElapsedDistance := NextDistance;
      Inc(Count);
      if NodeSpacing < LastSpacing then
      begin
        NodeSpacing := NodeSpacing * FQuadMeshCreator.GrowthRate;
      end;
    until (NodeSpacing >= LastSpacing);

    NStar := Count + (NodeDistance - ElapsedDistance) / LastSpacing;
  end;

  result := CalcNodesToInsert(NStar);
end;

procedure TSegment.Reverse;
var
  Temp: TNode;
begin
  Temp := FNode1;
  FNode1 := FNode2;
  FNode2 := Temp;
  FInnerNodes.Reverse;
end;

constructor TSegment.Create(Node1, Node2: TNode; SegmentType: TSegmentType;
Boundary: TBoundary; QuadMeshCreator: TQuadMeshCreator);
begin
  FNode1 := Node1;
  FNode2 := Node2;
  FSegmentType := SegmentType;
  FQuadMeshCreator := QuadMeshCreator;
  FBoundary := Boundary;
  FInnerNodes := TNodeList.Create;
  FNode1.FSegments.Add(self);
  FNode2.FSegments.Add(self);
end;

function TSegment.CreateReversedSegment: TSegment;
var
  NodeIndex: Integer;
  ANode: TNode;
begin
  result := TSegment.Create(Node2, Node1, SegmentType, FBoundary,
    FQuadMeshCreator);
  result.FInnerNodes.Capacity := FInnerNodes.Count;
  for NodeIndex := FInnerNodes.Count - 1 downto 0 do
  begin
    ANode := FInnerNodes[NodeIndex];
    result.FInnerNodes.Add(ANode);
    ANode.FSegments.Add(self);
  end;
end;

destructor TSegment.Destroy;
begin
  FInnerNodes.Free;
  inherited;
end;

procedure TSegment.InsertNodes(NumberToInsert: Integer);
var
  DeltaX: double;
  DeltaY: double;
  NewNodeIndex: Integer;
  NewNode: TNode;
  NodeDistance: TFloat;
  Distances, Spacings: TRealList;
  NodeSpacing: double;
  LastSpacing: double;
  ElapsedDistance: double;
  Factor: double;
  NodeIndex: Integer;
  procedure InsertNode;
  begin
    case SegmentType of
      stInner:
        NewNode.FNodeType := ntInner;
      stEdge:
        NewNode.FNodeType := ntEdge;
      stSubDomain:
        NewNode.FNodeType := ntSubDomain;
    else
      Assert(False);
    end;
    FInnerNodes.Add(NewNode);
  end;

begin
  Assert(NumberToInsert >= 1);
  Assert(FInnerNodes.Count = 0);
  DeltaX := Node2.X - Node1.X;
  DeltaY := Node2.Y - Node1.Y;
  if Node1.DesiredSpacing = Node2.DesiredSpacing then
  begin
    for NewNodeIndex := 1 to NumberToInsert do
    begin
      NewNode := TNode.Create(FQuadMeshCreator, Node1.DesiredSpacing);
      NewNode.X := Node1.X + DeltaX / (NumberToInsert + 1) * NewNodeIndex;
      NewNode.Y := Node1.Y + DeltaY / (NumberToInsert + 1) * NewNodeIndex;
      InsertNode;
    end;
  end
  else
  begin
    NodeDistance := Distance(Node1.Location, Node2.Location);
    Distances := TRealList.Create;
    Spacings := TRealList.Create;
    try
      if Node1.DesiredSpacing > Node2.DesiredSpacing then
      begin
        NodeSpacing := Node2.DesiredSpacing;
        LastSpacing := Node1.DesiredSpacing
      end
      else
      begin
        NodeSpacing := Node1.DesiredSpacing;
        LastSpacing := Node2.DesiredSpacing
      end;

      ElapsedDistance := 0;
      while ElapsedDistance < NodeDistance do
      begin
        ElapsedDistance := ElapsedDistance + NodeSpacing;
        Distances.Add(ElapsedDistance);
        Spacings.Add(NodeSpacing);
        if NodeSpacing < LastSpacing then
        begin
          NodeSpacing := NodeSpacing * FQuadMeshCreator.GrowthRate;
          if NodeSpacing > LastSpacing then
          begin
            NodeSpacing := LastSpacing;
          end;
        end;
      end;

      While Distances.Count < NumberToInsert do
      begin
        ElapsedDistance := ElapsedDistance + NodeSpacing;
        Distances.Add(ElapsedDistance);
        Spacings.Add(NodeSpacing);
        if NodeSpacing < LastSpacing then
        begin
          NodeSpacing := NodeSpacing * FQuadMeshCreator.GrowthRate;
          if NodeSpacing > LastSpacing then
          begin
            NodeSpacing := LastSpacing;
          end;
        end;
      end;
      if Distances.Count > NumberToInsert then
      begin
        Distances.Count := NumberToInsert;
        Spacings.Count := NumberToInsert;
      end
      else
      begin
        ElapsedDistance := ElapsedDistance + NodeSpacing;
      end;
      if Node1.DesiredSpacing > Node2.DesiredSpacing then
      begin
        Distances.Reverse;
        Spacings.Reverse;
        for NodeIndex := 0 to Distances.Count - 1 do
        begin
          Distances[NodeIndex] := ElapsedDistance - Distances[NodeIndex];
        end;
      end;

      for NewNodeIndex := 0 to Distances.Count - 1 do
      begin
        Factor := Distances[NewNodeIndex] / ElapsedDistance;
        Assert(Factor > Epsilon);
        Assert(Factor < 1 - Epsilon);
        NewNode := TNode.Create(FQuadMeshCreator, Spacings[NewNodeIndex]);
        NewNode.X := Node1.X + DeltaX * Factor;
        NewNode.Y := Node1.Y + DeltaY * Factor;
        InsertNode;
      end;

    finally
      Distances.Free;
      Spacings.Free;
    end;
  end;

end;

function TSegment.Length: double;
begin
  if FNode1 = FNode2 then
  begin
    result := 0;
  end
  else
  begin
    result := Distance(FNode1.Location, FNode2.Location);
  end;
end;

function TSegment.Split(ANode: TNode): TSegmentList;
var
  Position: Integer;
  SubSeg: TSegment;
  NodeIndex: Integer;
  AnotherNode: TNode;
begin
  Position := FInnerNodes.IndexOf(ANode);
  Assert(Position >= 0);
  result := TSegmentList.Create;
  SubSeg := TSegment.Create(Node1, ANode, SegmentType, FBoundary,
    FQuadMeshCreator);
  result.Add(SubSeg);
  for NodeIndex := 0 to Position - 1 do
  begin
    AnotherNode := FInnerNodes[NodeIndex];
    AnotherNode.FSegments.Remove(self);
    SubSeg.FInnerNodes.Add(AnotherNode);
    if AnotherNode.FSegments.IndexOf(SubSeg) < 0 then
    begin
      AnotherNode.FSegments.Add(SubSeg);
    end;

  end;
  SubSeg := TSegment.Create(ANode, Node2, SegmentType, FBoundary,
    FQuadMeshCreator);
  result.Add(SubSeg);
  for NodeIndex := Position + 1 to FInnerNodes.Count - 1 do
  begin
    AnotherNode := FInnerNodes[NodeIndex];
    AnotherNode.FSegments.Remove(self);
    SubSeg.FInnerNodes.Add(AnotherNode);
    if AnotherNode.FSegments.IndexOf(SubSeg) < 0 then
    begin
      AnotherNode.FSegments.Add(SubSeg);
    end;
  end;
  Node1.FSegments.Remove(self);
  Node2.FSegments.Remove(self);
  ANode.FSegments.Remove(self);
end;

{ TNodeComparer }

function TNodeAngleComparer.Compare(const Left, Right: TNode): Integer;
var
  Angle1: double;
  Angle2: double;
begin
  Angle1 := ArcTan2(Right.Y - FCenterNode.Y, Right.X - FCenterNode.X);
  Angle2 := ArcTan2(Left.Y - FCenterNode.Y, Left.X - FCenterNode.X);
  result := Sign(Angle2 - Angle1);
end;

constructor TNodeAngleComparer.Create(CenterNode: TNode);
begin
  FCenterNode := CenterNode;
  Assert(FCenterNode <> nil);
end;

{ TNodeSpacingComparer }

function TNodeSpacingComparer.Compare(const Left, Right: TNode): Integer;
begin
  result := Sign(Right.DesiredSpacing - Left.DesiredSpacing);
end;

initialization

SetDefaults;

end.
