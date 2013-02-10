unit SutraMeshUnit;

interface

uses
  Windows, FastGEO, Classes, GoPhastTypes, GR32, ZoomBox2, MeshRenumbering,
  AbstractGridUnit, Generics.Collections, gpc, Generics.Defaults, Types,
  DataSetUnit, Graphics, SubscriptionUnit;

Type
  TDrawingChoice = (dcAll, dcEdge);
  TMeshType = (mt2D, mt3D);

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
    constructor Create(Collection: TCollection); override;
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
    procedure DrawTop(const BitMap: TBitmap32;
      const ZoomBox: TQRbwZoomBox2; DrawingChoice: TDrawingChoice;
      DataArray: TDataArray; SelectedLayer: integer; StringValues : TStringList);
//    function ThreeDNodeNumber: integer;
    function DisplayNumber: integer;
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
    procedure GetCellOutline(var CellOutline: TVertexArray);
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

  TSutraMesh2D = class;

  TSutraNode2D_Collection = class(TCustomSutraCollection)
  private
    FMesh2D: TSutraMesh2D;
    function GetItem(Index: integer): TSutraNode2D;
  public
    constructor Create(Model: TBaseModel; ParentMesh: TSutraMesh2D);
    property Items[Index: integer]: TSutraNode2D read GetItem; default;
    function Add: TSutraNode2D;
    function TopContainingCell(APoint: TPoint2D): T2DTopCell;
    property Mesh2D: TSutraMesh2D read FMesh2D;
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
    procedure SetElementNumber(Value: integer);
    function GetElementNumber: integer;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property ElementNumber: integer read GetElementNumber write SetElementNumber;
  end;

  TSutraElement2D = class(TCustomSutraElement)
  private
    FNodes: TSutraNodeNumber2D_Collection;
    procedure SetNodes(const Value: TSutraNodeNumber2D_Collection);
    procedure DrawTop(const BitMap: TBitmap32;
      const ZoomBox: TQRbwZoomBox2; DrawingChoice: TDrawingChoice;
      DataArray: TDataArray; SelectedLayer: integer; StringValues : TStringList;
      DrawElementNumber: boolean);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignIElement(Source: IElement);
    function IsInside(APoint: TPoint2D): Boolean;
    function Center: TPoint2D;
    function Intersection(const Input: TSegment2D;
      out IntersectingSegment: TSegment2D): boolean;
    function Edge(Index: integer): TSegment2D;
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

  TCustomSutraMesh = class abstract(TCustomDiscretization)
  protected
    FUpdateCount: Integer;
  public
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
  end;

  ISutraNode2DComparer = IComparer<TSutraNode2D>;

  TSutraNode2DComparer = class(TComparer<TSutraNode2D>)
  private
    FStartPoint: TPoint2D;
    FAngle: Double;
  public
    function Compare(const Left, Right: TSutraNode2D): Integer; override;
    constructor Create(CrossSectionAngle: Double; StartPoint: TPoint2D);
  end;

  ISutraElement2DComparer = IComparer<TSutraElement2D>;

  TSutraElement2DComparer = class(TComparer<TSutraElement2D>)
  private
    FStartPoint: TPoint2D;
    FAngle: Double;
  public
    function Compare(const Left, Right: TSutraElement2D): Integer; override;
    constructor Create(CrossSectionAngle: Double; StartPoint: TPoint2D);
  end;

  TSutraMesh3D = class;

  TSutraMesh2D = class (TCustomSutraMesh)
  private
    FElements: TSutraElement2D_Collection;
    FNodes: TSutraNode2D_Collection;
    FElementDrawingChoice: TDrawingChoice;
    FTopDataSet: TDataArray;
    FThreeDDataSet: TDataArray;
    FSelectedLayer: integer;
    FTopGridObserver: TObserver;
    FDrawNodeNumbers: boolean;
    FDrawElementNumbers: boolean;
    FNodeDrawingChoice: TDrawingChoice;
    FMesh3D: TSutraMesh3D;
    procedure SetElements(const Value: TSutraElement2D_Collection);
    procedure SetNodes(const Value: TSutraNode2D_Collection);
    procedure DrawTop(const BitMap: TBitmap32;
      const ZoomBox: TQRbwZoomBox2);
    procedure SetElementDrawingChoice(const Value: TDrawingChoice);
    function MeshLimits: TGridLimit;
    function MeshBox: TPolygon2D;
    function MeshOutline: TPolygon2D;
    procedure SetThreeDDataSet(const Value: TDataArray);
    procedure SetTopDataSet(const Value: TDataArray);
    procedure SetSelectedLayer(const Value: integer);
    procedure GetNodesOnSegment(Segement: TSegment2D;
      NodesOnSegment: TSutraNode2D_List);
    procedure GetElementsOnSegment(Segment: TSegment2D;
      ElementsOnSegment: TSutraElement2D_List);
    procedure SetTopGridObserver(const Value: TObserver);
    procedure SetDrawNodeNumbers(const Value: boolean);
    procedure SetDrawElementNumbers(const Value: boolean);
    procedure SetNodeDrawingChoice(const Value: TDrawingChoice);
  protected
    procedure CalculateMinMax(DataSet: TDataArray;
      var MinMaxInitialized: boolean; var MinMax: TMinMax;
      StringValues: TStringList); override;
  public
    procedure GetMinMax(var MinMax: TMinMax; DataSet: TDataArray;
      StringValues: TStringList); override;
    Constructor Create(Model: TBaseModel; ParentMesh: TSutraMesh3D);
    destructor Destroy; override;
    procedure Draw(const BitMap: TBitmap32);
    procedure Clear;
    property ElementDrawingChoice: TDrawingChoice read FElementDrawingChoice
      write SetElementDrawingChoice;
    property NodeDrawingChoice: TDrawingChoice read FNodeDrawingChoice
      write SetNodeDrawingChoice;
    property DrawNodeNumbers: boolean read FDrawNodeNumbers
      write SetDrawNodeNumbers;
    property DrawElementNumbers: boolean read FDrawElementNumbers
      write SetDrawElementNumbers;
    function TopContainingCellOrElement(APoint: TPoint2D;
      const EvaluatedAt: TEvaluatedAt): T2DTopCell;
    property ThreeDDataSet: TDataArray read FThreeDDataSet
      write SetThreeDDataSet;
    property TopDataSet: TDataArray read FTopDataSet write SetTopDataSet;
    property SelectedLayer: integer read FSelectedLayer write SetSelectedLayer;
    property TopGridObserver: TObserver read FTopGridObserver
      write SetTopGridObserver;
    procedure EndUpdate; override;
    property Mesh3D: TSutraMesh3D read FMesh3D;
  published
    property Nodes: TSutraNode2D_Collection read FNodes write SetNodes;
    property Elements: TSutraElement2D_Collection read FElements
      write SetElements;
  end;

  TSutraElement3D = class;
  TSutraElement3DList = class;

  TSutraNode3D = class(TCustomSutraNode, INode)
  private
    FNode2D: TSutraNode2D;
    FZ: FastGEO.TFloat;
    FNode2D_Number: Integer;
    FElements: TSutraElement3DList;
    FActiveElements: TSutraElement3DList;
    FActive: Boolean;
    function GetNode2D_Number: integer;
    procedure SetNode2D_Number(const Value: integer);
    procedure SetZ(const Value: FastGEO.TFloat);
    function GetNode2D: TSutraNode2D;
    procedure UpdateNode2D;
    function GetX: TFloat;
    function GetY: TFloat;
    procedure UpdateActiveElementList;
  protected
//    function GetElementCount: integer;
    function GetActiveElementCount: integer;
//    function GetElement(Index: Integer): IElement;
    function GetActiveElement(Index: Integer): IElement;
    function GetNodeNumber: Integer;
    procedure SetNodeNumber(Value: Integer);
    function GetLocation: TPoint2D;
    procedure SetLocation(const Value: TPoint2D);
    function GetNodeType: TNodeType;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    procedure Assign(Source: TPersistent); override;
    procedure AssignINode(Source: INode);
    property Node2D: TSutraNode2D read GetNode2D;
    property X: TFloat read GetX;
    property Y: TFloat read GetY;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function NodeLocation: TPoint3D;
  published
    property Z: FastGEO.TFloat read FZ write SetZ;
    property Node2D_Number: integer read GetNode2D_Number write SetNode2D_Number;
    property Active: Boolean read FActive write FActive;
  end;

  TSutraNode3D_List = TList<TSutraNode3D>;

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
    function IndexOfNode(Node: TSutraNode3D): Integer;
  end;

  TQuadPair2D = array[0..1] of TQuadix2D;
  TQuadPair3D = array[0..1] of TQuadix3D;
  TQuadPair3DList = TList<TQuadPair3D>;

  TSutraElement3D = class(TCustomSutraElement, IElement)
  private
    FNodes: TSutraNodeNumber3D_Collection;
    FActiveNodes: TSutraNode3D_List;
    FActive: Boolean;
    procedure SetNodes(const Value: TSutraNodeNumber3D_Collection);
    procedure UpdateNodes;
    function GetCenterLocation: TPoint3d;
    procedure UpdateActiveNodeList;
  protected
//    function GetNode(Index: Integer): INode;
//    function GetNodeCount: integer;
    function GetActiveNode(Index: Integer): INode;
    function GetActiveNodeCount: integer;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignIElement(Source: IElement);
    function UpperElevation: double;
    function LowerElevation: double;
    function CenterElevation: double;
    function CellSection(Node: TSutraNode3D): TQuadPair3D;
    function ElementShape: TQuadPair3D;
    property CenterLocation: TPoint3d read GetCenterLocation;
  published
    property Nodes: TSutraNodeNumber3D_Collection read FNodes write SetNodes;
    property Active: Boolean read FActive write FActive;
  end;

  TSutraElement3DList = class(TList<TSutraElement3D>);

  TSutraElement3D_Collection = class(TCustomSutraCollection)
  private
    function GetItems(Index: integer): TSutraElement3D;
  public
    constructor Create(Model: TBaseModel);
    property Items[Index: integer]: TSutraElement3D read GetItems; default;
    function Add: TSutraElement3D;
  end;

  TCrossSection = class(TGoPhastPersistent)
  private
    FStartX: Double;
    FStartY: double;
    FEndX: Double;
    FEndY: double;
    FColor: TColor;
    procedure SetEndX(const Value: Double);
    procedure SetEndY(const Value: double);
    procedure SetStartX(const Value: Double);
    procedure SetStartY(const Value: double);
    function GetEndPoint: TPoint2D;
    function GetStartPoint: TPoint2D;
    procedure SetEndPoint(const Value: TPoint2D);
    procedure SetStartPoint(const Value: TPoint2D);
    procedure SetColor(const Value: TColor);
    function GetSegement: TSegment2D;
    procedure SetSegement(const Value: TSegment2D);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel);
    property StartPoint: TPoint2D read GetStartPoint write SetStartPoint;
    property EndPoint: TPoint2D read GetEndPoint write SetEndPoint;
    property Segment: TSegment2D read GetSegement write SetSegement;
    procedure Draw(const BitMap: TBitmap32);
    function Angle: Double;
  published
    property StartX: Double read FStartX write SetStartX;
    property EndX: Double read FEndX write SetEndX;
    property StartY: double read FStartY write SetStartY;
    property EndY: double read FEndY write SetEndY;
    property Color: TColor read FColor write SetColor default clFuchsia;
  end;

  TLimits = record
    UpperLimit: Double;
    LowerLimit: Double;
  end;

  TCellElementPolygons2D = array of array of TPolygon2D;
  TLimitsArray = array of array of TLimits;

  TSutraMesh3D = class (TCustomSutraMesh, IMesh)
  private
    FElements: TSutraElement3D_Collection;
    FNodes: TSutraNode3D_Collection;
    FMesh2D: TSutraMesh2D;
    FElementDrawingChoice: TDrawingChoice;
    FMeshType: TMeshType;
    FNodeArray: array of array of TSutraNode3D;
    FElementArray: array of array of TSutraElement3D;
    FMeshUpdate: integer;
    FCanDraw: boolean;
    FCrossSection: TCrossSection;
    FThreeDGridObserver: TObserver;
    FElevationsNeedUpdating: boolean;
    FUpdatingElevations: boolean;
    FNodeDrawingChoice: TDrawingChoice;
    FOnSelectedLayerChange: TNotifyEvent;
    FActiveNodes: TSutraNode3D_List;
    FActiveElements: TSutraElement3DList;
    procedure SetElements(const Value: TSutraElement3D_Collection);
    procedure SetNodes(const Value: TSutraNode3D_Collection);
    procedure SetMesh2D(const Value: TSutraMesh2D);
    procedure SetElementDrawingChoice(const Value: TDrawingChoice);
    procedure SetMeshType(const Value: TMeshType);
    function GetNodeArrayMember(Layer, Col: Integer): TSutraNode3D;
    procedure SetThreeDDataSet(const Value: TDataArray);
    procedure SetTopDataSet(const Value: TDataArray);
    function GetCanDraw: boolean;
    procedure SetCanDraw(const Value: boolean);
    function GetThreeDDataSet: TDataArray;
    function GetTopDataSet: TDataArray;
    procedure SetSelectedLayer(Value: integer);
    function GetLayerCount: Integer;
    function GetSelectedLayer: integer;
    function GetElementArrayMember(Layer, Col: Integer): TSutraElement3D;
    procedure SetCrossSection(const Value: TCrossSection);
    procedure DrawFront(const BitMap: TBitmap32);
    procedure DrawPointsOnCrossSection(BitMap: TBitmap32);
    function GetTopGridObserver: TObserver;
    procedure SetThreeDGridObserver(const Value: TObserver);
    procedure SetTopGridObserver(const Value: TObserver);
    procedure CheckUpdateElevations;
    procedure UpdateElevations;
    procedure SetNodeDrawingChoice(const Value: TDrawingChoice);
  protected
    procedure CalculateMinMax(DataSet: TDataArray;
      var MinMaxInitialized: Boolean; var MinMax: TMinMax;
      StringValues: TStringList); override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
//    function GetNode(Index: Integer): INode;
//    function GetNodeCount: integer;
//    function GetElementCount: integer;
//    function GetElement(Index: Integer): IElement;
    function GetActiveNodeCount: integer;
    function GetActiveNode(Index: Integer): INode;
    function GetActiveElementCount: integer;
    function GetActiveElement(Index: Integer): IElement;
  public
    property ActiveNodeCount: Integer read GetActiveNodeCount;
    property ActiveElementCount: Integer read GetActiveElementCount;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    property ElevationsNeedUpdating: boolean read FElevationsNeedUpdating
      write FElevationsNeedUpdating;
    Constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    procedure Clear;
    procedure Draw(const BitMap: TBitmap32;
      const ViewDirection: TViewDirection);
    function TopContainingCellOrElement(APoint: TPoint2D;
      const EvaluatedAt: TEvaluatedAt): T2DTopCell;
    function MeshLimits(ViewDirection: TViewDirection): TGridLimit;
    function MeshBox(ViewDirection: TViewDirection): TPolygon2D;
    property NodeArray[Layer: Integer; Col: Integer]: TSutraNode3D
      read GetNodeArrayMember;
    property ElementArray[Layer: Integer; Col: Integer]: TSutraElement3D
      read GetElementArrayMember;
    property ThreeDDataSet: TDataArray read GetThreeDDataSet
      write SetThreeDDataSet;
    property TopDataSet: TDataArray read GetTopDataSet write SetTopDataSet;
    procedure MeshChanged;
    property CanDraw: boolean read GetCanDraw write SetCanDraw;
    // @name is the number of layers of elements.
    property LayerCount: Integer read GetLayerCount;
    procedure UpdateElementsInNodes;
    function FrontPolygons(Angle: Double;
      EvaluatedAt: TEvaluatedAt; out Limits: TLimitsArray): TCellElementPolygons2D;
    procedure GetMinMax(var MinMax: TMinMax; DataSet: TDataArray;
      StringValues: TStringList); override;
    property TopGridObserver: TObserver read GetTopGridObserver
      write SetTopGridObserver;
    property ThreeDGridObserver: TObserver read FThreeDGridObserver
      write SetThreeDGridObserver;
    procedure EndUpdate; override;
    property OnSelectedLayerChange: TNotifyEvent read FOnSelectedLayerChange
      write FOnSelectedLayerChange;
  published
    property Nodes: TSutraNode3D_Collection read FNodes write SetNodes;
    property Elements: TSutraElement3D_Collection read FElements
      write SetElements;
    property Mesh2D: TSutraMesh2D read FMesh2D write SetMesh2D;
    property ElementDrawingChoice: TDrawingChoice read FElementDrawingChoice
      write SetElementDrawingChoice;
    property NodeDrawingChoice: TDrawingChoice read FNodeDrawingChoice
      write SetNodeDrawingChoice;
    property MeshType: TMeshType read FMeshType write SetMeshType stored True;
    property SelectedLayer: integer read GetSelectedLayer
      write SetSelectedLayer stored True;
    property CrossSection: TCrossSection read FCrossSection
      write SetCrossSection;
  end;

Function ComparePointRightUp(const P1, P2: TPoint2D): integer;
Function ComparePointRightDown(const P1, P2: TPoint2D): integer;
Function ComparePointLeftUp(const P1, P2: TPoint2D): integer;
Function ComparePointLeftDown(const P1, P2: TPoint2D): integer;

implementation

uses
  frmGoPhastUnit, BigCanvasMethods, PhastModelUnit, SysUtils, GPC_Classes, Math,
  Dialogs, LayerStructureUnit, RbwParser, GR32_Polygons;

resourcestring
  StrErrorGeneratingCel = 'Error generating cell for Node %d.';

procedure GetDataSetValue(DataArray: TDataArray;
  Layer, Column: integer; StringValues : TStringList;
  var ShowColor: Boolean; var Fraction: double);
var
  AValue: Double;
  AValueInt: integer;
  ValueStr: string;
  SIndex: Integer;
begin

  ShowColor := DataArray.IsValue[Layer, 0, Column]
    and DataArray.ColorGridValueOK(Layer, 0, Column);
  if ShowColor then
  begin
    case DataArray.DataType of
      rdtDouble:
        begin
          AValue := DataArray.RealData[Layer, 0, Column];
          if DataArray.Limits.LogTransform  then
          begin
            if AValue > 0 then
            begin
              if DataArray.MaxReal <> DataArray.MinRealPositive then
              begin
                Fraction := (Log10(DataArray.MaxReal-Log10(AValue)))
                  /(Log10(DataArray.MaxReal) - Log10(DataArray.MinRealPositive));
              end
              else
              begin
                Fraction := 0.5;
              end;
            end
            else
            begin
              ShowColor := False;
            end;
          end
          else
          begin
            if DataArray.MaxReal <> DataArray.MinReal then
            begin
              Fraction := (DataArray.MaxReal-AValue)/
                (DataArray.MaxReal - DataArray.MinReal);
            end
            else
            begin
              Fraction := 0.5;
            end;
          end;
        end;
      rdtInteger:
        begin
          if DataArray.MaxInteger <> DataArray.MinInteger then
          begin
            AValueInt := DataArray.IntegerData[Layer, 0, Column];
            Fraction := (DataArray.MaxInteger-AValueInt)
              /(DataArray.MaxInteger - DataArray.MinInteger);
          end
          else
          begin
            Fraction := 0.5;
          end;
        end;
      rdtBoolean:
        begin
          if DataArray.MaxBoolean <> DataArray.MinBoolean then
          begin
            if DataArray.BooleanData[Layer, 0, Column] then
            begin
              Fraction := 1;
            end
            else
            begin
              Fraction := 0;
            end;
          end
          else
          begin
            Fraction := 0.5;
          end;
        end;
      rdtString:
        begin
          if StringValues.Count = 0 then
          begin
            ShowColor := False
          end
          else if StringValues.Count = 1 then
          begin
            ValueStr := DataArray.StringData[
              Layer, 0, Column];
            if StringValues.IndexOf(ValueStr) >= 0 then
            begin
              Fraction := 0.5;
            end
            else
            begin
              ShowColor := False;
            end;
          end
          else
          begin
            ValueStr := DataArray.StringData[Layer, 0, Column];
            SIndex := StringValues.IndexOf(ValueStr);
            if SIndex >= 0 then
            begin
              Fraction := (1-(SIndex / (StringValues.Count - 1)));
            end
            else
            begin
              ShowColor := False;
            end;
          end;
        end;
      else Assert(False);
    end;
  end;

end;

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

function TSutraNode2D.DisplayNumber: integer;
var
  Mesh: TSutraMesh3D;
  Node3D: TSutraNode3D;
begin
  Mesh := (Collection as  TSutraNode2D_Collection).Mesh2D.Mesh3D;
  if Mesh.MeshType = mt3D then
  begin
    Node3D := Mesh.NodeArray[Mesh.SelectedLayer, Number];
    result := Node3D.Number + 1;
//    result := ThreeDNodeNumber + 1;
  end
  else
  begin
    result := Number + 1;
  end;
end;

procedure TSutraNode2D.DrawTop(const BitMap: TBitmap32;
  const ZoomBox: TQRbwZoomBox2; DrawingChoice: TDrawingChoice;
  DataArray: TDataArray; SelectedLayer: integer; StringValues : TStringList);
var
  Outline: TVertexArray;
  NodeIndex: Integer;
  Points: GoPhastTypes.TPointArray;
  Fraction: double;
  AColor: TColor;
  Dummy: TPolygon32;
  ShowColor: Boolean;
  APoint: TPoint;
  NumString: string;
begin
  Dummy := nil;
  GetCellOutline(Outline);
  if (DataArray <> nil) or (DrawingChoice = dcAll) then
  begin
    SetLength(Points, Length(Outline)+1);
    for NodeIndex := 0 to Length(Outline) - 1 do
    begin
      Points[NodeIndex] := ConvertTop2D_Point(ZoomBox, Outline[NodeIndex]);
    end;
    Points[Length(Points)-1] := Points[0];
//    Column := Self.Number;
    if DataArray <> nil then
    begin
      GetDataSetValue(DataArray, SelectedLayer, Self.Number, StringValues,
        ShowColor, Fraction);
      if ShowColor then
      begin
        AColor := frmGoPhast.PhastModel.GridColorParameters.FracToColor(Fraction);
        DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
          0, Points, Dummy, False, True);
      end;
    end;

  end;
  case DrawingChoice of
    dcAll:
      begin
        DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
          Points, True, True);
      end;
    dcEdge: ; // do nothing
    else
      Assert(False);
  end;

//  if DrawNumbers then
  begin
    APoint := ConvertTop2D_Point(ZoomBox, Location);
    NumString := InttoStr(DisplayNumber);
    BitMap.Textout(APoint.X+1, APoint.Y+1, NumString);

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
    AVertex := CellOutline[VertexIndex];
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

//function TSutraNode2D.ThreeDNodeNumber: integer;
//var
//  Mesh: TSutraMesh3D;
//  Node3D: TSutraNode3D;
//begin
//  Mesh := (Collection as  TSutraNode2D_Collection).Mesh2D.Mesh3D;
//  Node3D := Mesh.NodeArray[Mesh.SelectedLayer, Number];
//  result := Node3D.Number;
//end;

{ TSutraNode2D_Collection }

function TSutraNode2D_Collection.Add: TSutraNode2D;
begin
  result := inherited Add as TSutraNode2D;
end;

constructor TSutraNode2D_Collection.Create(Model: TBaseModel; ParentMesh: TSutraMesh2D);
begin
  inherited Create(TSutraNode2D, Model);
  FMesh2D := ParentMesh;
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

constructor TCustomSutraNode.Create(Collection: TCollection);
begin
  inherited;
  FNumber := -1;
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
  Mesh := LocalModel.Mesh.Mesh2D;
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

constructor TSutraNodeNumber2D_Collection.Create(Model: TBaseModel;
  Element: TSutraElement2D);
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

function TCustomSutraElement.GetElementNumber: integer;
begin
  result := FElementNumber;
end;

procedure TCustomSutraElement.SetElementNumber(Value: integer);
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
  Assert(Nodes.Count = 4);
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
  const ZoomBox: TQRbwZoomBox2; DrawingChoice: TDrawingChoice;
  DataArray: TDataArray; SelectedLayer: integer; StringValues : TStringList;
  DrawElementNumber: boolean);
var
  Node1: TSutraNode2D;
  NodeIndex: Integer;
  Node2: TSutraNode2D;
  Points: GoPhastTypes.TPointArray;
  SumX: integer;
  SumY: Integer;
  NumString: string;
  Extent: TSize;
  Fraction: Double;
  AColor: TColor;
  Dummy: TPolygon32;
  ShowColor: Boolean;
begin
  Dummy := nil;
  SumX := 0;
  SumY := 0;
  if (DataArray <> nil) or (DrawingChoice = dcAll) or DrawElementNumber then
  begin
    SetLength(Points, 5);
    Assert(Nodes.Count = 4);
    for NodeIndex := 0 to Nodes.Count - 1 do
    begin
      Node2 := Nodes[NodeIndex].Node;
      Points[NodeIndex] := ConvertTop2D_Point(ZoomBox, Node2.Location);
      SumX := SumX + Points[NodeIndex].X;
      SumY := SumY + Points[NodeIndex].Y;
    end;
    Points[4] := Points[0];
    if DataArray <> nil then
    begin
      GetDataSetValue(DataArray, SelectedLayer, Self.ElementNumber,
        StringValues, ShowColor, Fraction);
      if ShowColor then
      begin
        AColor := frmGoPhast.PhastModel.GridColorParameters.FracToColor(Fraction);
        DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
          0, Points, Dummy, False, True);
      end;
    end;
  end;

  case DrawingChoice of
    dcAll:
      begin
        DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
          Points, True);
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

  if DrawElementNumber then
  begin
    SumX := SumX div 4;
    SumY := SumY div 4;
    NumString := InttoStr(ElementNumber);
    Extent := BitMap.TextExtent(NumString);
    BitMap.Textout(SumX - (Extent.cx div 2),
      SumY - (Extent.cy div 2), NumString);
  end;
end;

function TSutraElement2D.Edge(Index: integer): TSegment2D;
begin
  Assert((Index >= 0) and (Index <= 3));
  Result[1] := Nodes[Index].Node.Location;
  Inc(Index);
  if Index >= 4 then
  begin
    Index := 0;
  end;
  Result[2] := Nodes[Index].Node.Location;
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
    Node := Nodes[NodeIndex].Node;
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

procedure TSutraMesh2D.CalculateMinMax(DataSet: TDataArray;
  var MinMaxInitialized: boolean; var MinMax: TMinMax;
  StringValues: TStringList);
begin
  Assert(False);

end;

procedure TSutraMesh2D.Clear;
begin
  Nodes.Clear;
  Elements.Clear;
  ElementDrawingChoice := dcAll;
end;

constructor TSutraMesh2D.Create(Model: TBaseModel; ParentMesh: TSutraMesh3D);
var
  LocalModel: TCustomModel;
begin
  inherited Create(Model);
  FMesh3D := ParentMesh;
  FNodes := TSutraNode2D_Collection.Create(Model, self);
  FElements := TSutraElement2D_Collection.Create(Model);
  LocalModel := Model as TCustomModel;
  FTopGridObserver := LocalModel.TopGridObserver;
  FTopGridObserver.OnUpToDateSet := LocalModel.OnTopSutraMeshChanged;
end;

destructor TSutraMesh2D.Destroy;
begin
  FElements.Free;
  FNodes.Free;
  inherited;
end;

procedure TSutraMesh2D.Draw(const BitMap: TBitmap32);
begin
  DrawTop(BitMap, frmGoPhast.frameTopView.ZoomBox);
end;

procedure TSutraMesh2D.DrawTop(const BitMap: TBitmap32;
  const ZoomBox: TQRbwZoomBox2);
var
  ElementIndex: Integer;
  NodeIndex: Integer;
  ANode: TSutraNode2D;
  APoint: TPoint;
  ColorDataArray: TDataArray;
  Layer: Integer;
  StringValues : TStringList;
  LayerIndex: integer;
  RowIndex: integer;
  ColIndex: integer;
  Node3D: TSutraNode3D;
  ElementLayer: Integer;
  Element2D: TSutraElement2D;
  Element3D: TSutraElement3D;
  DrawNode: Boolean;
  DrawElement: Boolean;
begin
  StringValues := TStringList.Create;
  try
    ColorDataArray := nil;
    if ThreeDDataSet <> nil then
    begin
      ColorDataArray := ThreeDDataSet;
    end
    else if TopDataSet <> nil then
    begin
      ColorDataArray := TopDataSet;
    end;
    Layer := 0;
    if ColorDataArray <> nil then
    begin
      ColorDataArray.Initialize;
      case ColorDataArray.Orientation of
        dsoTop: Layer := 0;
        dso3D: Layer := SelectedLayer;
        else Assert(False);
      end;
      if ColorDataArray.DataType = rdtString then
      begin
        StringValues.Sorted := True;
        StringValues.Duplicates := dupIgnore;
        StringValues.CaseSensitive := True;
        StringValues.Capacity := ColorDataArray.LayerCount *
          ColorDataArray.RowCount * ColorDataArray.ColumnCount;
        for LayerIndex := 0 to ColorDataArray.LayerCount - 1 do
        begin
          for RowIndex := 0 to ColorDataArray.RowCount - 1 do
          begin
            for ColIndex := 0 to ColorDataArray.ColumnCount - 1 do
            begin
              if ColorDataArray.IsValue[LayerIndex, RowIndex, ColIndex] then
              begin
                StringValues.Add(ColorDataArray.StringData[
                  LayerIndex, RowIndex, ColIndex]);
              end;
            end;
          end;
        end;
      end;
    end;

    ElementLayer := SelectedLayer;
    if ElementLayer >= FMesh3D.LayerCount then
    begin
      ElementLayer := FMesh3D.LayerCount-1;
    end;
    for ElementIndex := 0 to Elements.Count - 1 do
    begin
      Element2D := Elements[ElementIndex];
      DrawElement := True;
      if FMesh3D.MeshType = mt3d then
      begin
        Element3D := FMesh3D.elementArray[ElementLayer,
          Element2D.ElementNumber];
        DrawElement := Element3D.Active;
      end;
      if DrawElement then
      begin
        if (ColorDataArray <> nil)
          and (ColorDataArray.EvaluatedAt = eaBlocks) then
        begin
          Element2D.DrawTop(BitMap, ZoomBox, ElementDrawingChoice,
            ColorDataArray, Layer, StringValues, DrawElementNumbers);
        end
        else
        begin
          Element2D.DrawTop(BitMap, ZoomBox, ElementDrawingChoice, nil,
            Layer, StringValues, DrawElementNumbers);
        end;
      end;
    end;
    for NodeIndex := 0 to Nodes.Count - 1 do
    begin
      ANode := Nodes[NodeIndex];
      DrawNode := True;
      if FMesh3D.MeshType = mt3d then
      begin
        Node3D := FMesh3D.NodeArray[SelectedLayer, ANode.Number];
        DrawNode := Node3D.Active;
      end;
      if DrawNode then
      begin
        if (ColorDataArray <> nil) and (ColorDataArray.EvaluatedAt = eaNodes) then
        begin
          ANode.DrawTop(BitMap, ZoomBox, NodeDrawingChoice, ColorDataArray,
            Layer, StringValues);
        end
        else
        begin
          ANode.DrawTop(BitMap, ZoomBox, NodeDrawingChoice, nil, Layer, StringValues);
        end;
      end;
    end;
    for NodeIndex := 0 to Nodes.Count - 1 do
    begin
      ANode := Nodes[NodeIndex];
      if DrawNodeNumbers then
      begin
        APoint := ConvertTop2D_Point(ZoomBox, ANode.Location);
        BitMap.Textout(APoint.X, APoint.Y, IntToStr(ANode.number));
      end;
    end;
  finally
    StringValues.free;
  end;
end;

procedure TSutraMesh2D.EndUpdate;
begin
  inherited;
  if (FUpdateCount = 0) then
  begin
    frmGoPhast.PhastModel.DataArrayManager.InvalidateAllDataSets;
    if Assigned(TopGridObserver) then
    begin
      TopGridObserver.UpToDate := False;
      TopGridObserver.UpToDate := True;
    end;
  end;
end;

procedure TSutraMesh2D.GetElementsOnSegment(Segment: TSegment2D;
  ElementsOnSegment: TSutraElement2D_List);
var
  ElementIndex: Integer;
  AnElement: TSutraElement2D;
  EdgeIndex: Integer;
  ElementComparer: ISutraElement2DComparer;
begin
  ElementsOnSegment.Clear;
  for ElementIndex := 0 to Elements.Count - 1 do
  begin
    AnElement := Elements[ElementIndex];
    for EdgeIndex := 0 to 3 do
    begin
      if Intersect(AnElement.Edge(EdgeIndex), Segment) then
      begin
        if ElementsOnSegment.IndexOf(AnElement) < 0 then
        begin
          ElementsOnSegment.Add(AnElement);
        end;
      end;
    end;
  end;

  ElementComparer := TSutraElement2DComparer.Create(
    ArcTan2(Segment[2].y - Segment[1].y, Segment[2].x - Segment[1].x),
    Segment[1]);
  try
    ElementsOnSegment.Sort(ElementComparer);
  finally
    ElementComparer := nil;
  end;
end;

procedure TSutraMesh2D.GetMinMax(var MinMax: TMinMax; DataSet: TDataArray;
  StringValues: TStringList);
begin
  Assert(False);
end;

procedure TSutraMesh2D.GetNodesOnSegment(Segement: TSegment2D;
  NodesOnSegment: TSutraNode2D_List);
var
  ElementIndex: Integer;
  AnElement: TSutraElement2D;
  EdgeIndex: Integer;
  IntersectPoint: TPoint2D;
  NodeIndex: Integer;
  Node1: TSutraNode2D;
  Node2: TSutraNode2D;
  NodeComparer: ISutraNode2DComparer;
  function NodeOK(Node: TSutraNode2D): boolean;
  var
    LayerIndex: Integer;
  begin
    result := NodesOnSegment.IndexOf(Node) < 0;
    if result then
    begin
      result := False;
      for LayerIndex := 0 to FMesh3D.LayerCount - 1 do
      begin
        result := FMesh3D.NodeArray[LayerIndex, Node.Number].Active;
        if result then
        begin
          Exit;
        end;
      end;
    end;
  end;
begin
  NodesOnSegment.Clear;
  for ElementIndex := 0 to Elements.Count - 1 do
  begin
    AnElement := Elements[ElementIndex];
    for EdgeIndex := 0 to 3 do
    begin
      if Intersect(AnElement.Edge(EdgeIndex), Segement,
        IntersectPoint.x, IntersectPoint.y) then
      begin

        NodeIndex := EdgeIndex;
        Node1 := AnElement.Nodes[NodeIndex].Node;
        Inc(NodeIndex);
        if NodeIndex >= 4 then
        begin
          NodeIndex := 0;
        end;
        Node2 := AnElement.Nodes[NodeIndex].Node;

        if Distance(Node1.Location, IntersectPoint) <
          Distance(Node2.Location, IntersectPoint) then
        begin
          if NodeOK(Node1) then
          begin
            NodesOnSegment.Add(Node1);
          end;
        end
        else
        begin
          if NodeOK(Node2) then
          begin
            NodesOnSegment.Add(Node2);
          end;
        end;
      end;
    end;
  end;

  NodeComparer := TSutraNode2DComparer.Create(
    ArcTan2(Segement[2].y - Segement[1].y, Segement[2].x - Segement[1].x),
    Segement[1]);
  try
    NodesOnSegment.Sort(NodeComparer);
  finally
    NodeComparer := nil;
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

procedure TSutraMesh2D.SetDrawElementNumbers(const Value: boolean);
begin
  FDrawElementNumbers := Value;
end;

procedure TSutraMesh2D.SetElementDrawingChoice(const Value: TDrawingChoice);
begin
  if FElementDrawingChoice <> Value then
  begin
    FElementDrawingChoice := Value;
    InvalidateModel;
  end;
end;

procedure TSutraMesh2D.SetDrawNodeNumbers(const Value: boolean);
begin
  FDrawNodeNumbers := Value;
end;

procedure TSutraMesh2D.SetElements(const Value: TSutraElement2D_Collection);
begin
  FElements.Assign(Value);
end;

procedure TSutraMesh2D.SetNodeDrawingChoice(const Value: TDrawingChoice);
begin
  FNodeDrawingChoice := Value;
end;

procedure TSutraMesh2D.SetNodes(const Value: TSutraNode2D_Collection);
begin
  FNodes.Assign(Value);
end;

procedure TSutraMesh2D.SetSelectedLayer(const Value: integer);
begin
  FSelectedLayer := Value;
end;

function TSutraMesh2D.TopContainingCellOrElement(APoint: TPoint2D;
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

constructor TSutraNode3D.Create(Collection: TCollection);
begin
  inherited;
  FActive := False;
  FElements:= TSutraElement3DList.Create;
  FActiveElements:= TSutraElement3DList.Create;
end;

destructor TSutraNode3D.Destroy;
begin
  FActiveElements.Free;
  FElements.Free;
  inherited;
end;

function TSutraNode3D.GetActiveElement(Index: Integer): IElement;
begin
  result := FActiveElements[Index];
end;

function TSutraNode3D.GetActiveElementCount: integer;
begin
  result := FActiveElements.Count;
end;

//function TSutraNode3D.GetElement(Index: Integer): IElement;
//begin
//  result := FElements[Index];
//end;

//function TSutraNode3D.GetElementCount: integer;
//begin
//  result := FElements.Count;
//end;

function TSutraNode3D.GetLocation: TPoint2D;
begin
  result.x := X;
  result.y := Y;
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

function TSutraNode3D.GetNodeNumber: Integer;
begin
  result := Number;
end;

function TSutraNode3D.GetNodeType: TNodeType;
begin
  result := ntInner;
end;

function TSutraNode3D.GetX: TFloat;
begin
  result := Node2D.X
end;

function TSutraNode3D.GetY: TFloat;
begin
  result := Node2D.Y
end;

function TSutraNode3D.NodeLocation: TPoint3D;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

function TSutraNode3D.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TSutraNode3D.SetLocation(const Value: TPoint2D);
begin
  Node2D.X := Value.x;
  Node2D.Y := Value.y;
end;

procedure TSutraNode3D.SetNode2D_Number(const Value: integer);
begin
  SetIntegerProperty(FNode2D_Number, Value);
  UpdateNode2D;
end;

procedure TSutraNode3D.SetNodeNumber(Value: Integer);
begin
  Number := Value;
end;

procedure TSutraNode3D.SetZ(const Value: FastGEO.TFloat);
begin
  SetRealProperty(FZ, Value);
end;

procedure TSutraNode3D.UpdateActiveElementList;
var
  ElementIndex: Integer;
  AnElement: TSutraElement3D;
begin
  FActiveElements.Clear;
  for ElementIndex := 0 to FElements.Count - 1 do
  begin
    AnElement := FElements[ElementIndex];
    if AnElement.Active then
    begin
      FActiveElements.Add(AnElement);
    end;
  end;
end;

procedure TSutraNode3D.UpdateNode2D;
var
  LocalModel: TCustomModel;
  Mesh: TSutraMesh2D;
begin
  LocalModel := Model as TCustomModel;
  Mesh := LocalModel.Mesh.Mesh2D;
  if (FNode2D_Number >= 0) and (FNode2D_Number < Mesh.Nodes.Count) then
  begin
    FNode2D := Mesh.Nodes[FNode2D_Number];
  end
  else
  begin
    FNode2D := nil;
  end;
end;

function TSutraNode3D._AddRef: Integer;
begin
  result := -1;
end;

function TSutraNode3D._Release: Integer;
begin
  result := -1;
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

function TSutraNodeNumber3D_Collection.IndexOfNode(Node: TSutraNode3D): Integer;
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

function Average2Point3D(const Point1, Point2: TPoint3D): TPoint3D;
begin
  Result.x := (Point1.x + Point2.x)/2;
  Result.y := (Point1.y + Point2.y)/2;
  Result.z := (Point1.z + Point2.z)/2;
end;

function Average4Point3D(const Point1, Point2, Point3, Point4: TPoint3D): TPoint3D;
begin
  Result.x := (Point1.x + Point2.x + Point3.x + Point4.x)/4;
  Result.y := (Point1.y + Point2.y + Point3.y + Point4.y)/4;
  Result.z := (Point1.z + Point2.z + Point3.z + Point4.z)/4;
end;

function QuadPairsToPolygon(QP: TQuadPair3DList; Angle: Double;
  out PerpendicularLimit: TLimits): TPolygon2D;
var
  Projection: array of TQuadPair2D;
  P1, Old_resultP, resultP: TGpcPolygonClass;
  QuadIndex: Integer;
  PointIndex: Integer;
  PointDistance: Extended;
  PointAngle: Extended;
  ListIndex: Integer;
  APoint: TPoint3D;
  QuadPair: TQuadPair3D;
  NodeIndex: Integer;
  Count: Integer;
  PerpendicularDistance: Extended;
  FoundFirst: Boolean;
  MinX, MaxX, MinY, MaxY: double;
  Epsilon: double;
  Index: Integer;
  ContourIndex: Integer;
  Area: double;
begin
  Assert(QP.Count > 0);
  Old_resultP := nil;
  P1 := TGpcPolygonClass.Create;
  try
    P1.NumberOfContours := 1;
    P1.VertexCount[0] := 4;
    Old_resultP := TGpcPolygonClass.Create;
    resultP := Old_resultP;

    FoundFirst := False;
    MinX := 0;
    MaxX := 0;
    MinY := 0;
    MaxY := 0;
    SetLength(Projection, QP.Count);
    for ListIndex := 0 to QP.Count - 1 do
    begin
      QuadPair := QP[ListIndex];
      for QuadIndex := 0 to 1 do
      begin
        for PointIndex := 1 to 4 do
        begin
          Projection[ListIndex,QuadIndex,PointIndex].y :=
            QuadPair[QuadIndex,PointIndex].z;
          APoint := QuadPair[QuadIndex,PointIndex];
          PointDistance := Sqrt(Sqr(APoint.x) + Sqr(APoint.y));
          PointAngle := ArcTan2(APoint.y, APoint.x);
          Projection[ListIndex,QuadIndex,PointIndex].x :=
            Cos(PointAngle-Angle)*PointDistance;
          PerpendicularDistance := Sin(PointAngle-Angle)*PointDistance;
          if FoundFirst then
          begin
            if PerpendicularDistance > PerpendicularLimit.UpperLimit then
            begin
              PerpendicularLimit.UpperLimit := PerpendicularDistance;
            end;
            if PerpendicularDistance < PerpendicularLimit.LowerLimit then
            begin
              PerpendicularLimit.LowerLimit := PerpendicularDistance;
            end;
            if MinX > Projection[ListIndex,QuadIndex,PointIndex].x then
            begin
              MinX := Projection[ListIndex,QuadIndex,PointIndex].x;
            end;
            if MaxX < Projection[ListIndex,QuadIndex,PointIndex].x then
            begin
              MaxX := Projection[ListIndex,QuadIndex,PointIndex].x;
            end;
            if MinY > Projection[ListIndex,QuadIndex,PointIndex].y then
            begin
              MinY := Projection[ListIndex,QuadIndex,PointIndex].y;
            end;
            if MaxY < Projection[ListIndex,QuadIndex,PointIndex].y then
            begin
              MaxY := Projection[ListIndex,QuadIndex,PointIndex].y;
            end;
          end
          else
          begin
            FoundFirst := True;
            PerpendicularLimit.UpperLimit := PerpendicularDistance;
            PerpendicularLimit.LowerLimit := PerpendicularDistance;
            MinX := Projection[ListIndex,QuadIndex,PointIndex].x;
            MaxX := Projection[ListIndex,QuadIndex,PointIndex].x;
            MinY := Projection[ListIndex,QuadIndex,PointIndex].y;
            MaxY := Projection[ListIndex,QuadIndex,PointIndex].y;
          end;
        end;
      end;
      Epsilon := Sqr(Max(MaxX-MinX, MaxY-MinY)/1000);

      P1.Vertices[0,0] := Projection[ListIndex,0,1];
      P1.Vertices[0,1] := Projection[ListIndex,0,2];
      P1.Vertices[0,2] := Projection[ListIndex,0,3];
      P1.Vertices[0,3] := Projection[ListIndex,0,4];

      if Abs(P1.ContourArea(0)) > Epsilon then
      begin
        resultP := TGpcPolygonClass.CreateFromOperation(GPC_UNION, P1, Old_resultP);
        Old_resultP.Free;
        Old_resultP := resultP;
      end;

      P1.Vertices[0,0] := Projection[ListIndex,1,1];
      P1.Vertices[0,1] := Projection[ListIndex,1,2];
      P1.Vertices[0,2] := Projection[ListIndex,1,3];
      P1.Vertices[0,3] := Projection[ListIndex,1,4];

      if Abs(P1.ContourArea(0)) > Epsilon then
      begin
        resultP := TGpcPolygonClass.CreateFromOperation(GPC_UNION, P1, Old_resultP);
        Old_resultP.Free;
        Old_resultP := resultP;
      end;

      P1.Vertices[0,0] := Projection[ListIndex,0,1];
      P1.Vertices[0,1] := Projection[ListIndex,0,2];
      P1.Vertices[0,2] := Projection[ListIndex,1,2];
      P1.Vertices[0,3] := Projection[ListIndex,1,1];

      if Abs(P1.ContourArea(0)) > Epsilon then
      begin
        resultP := TGpcPolygonClass.CreateFromOperation(GPC_UNION, P1, Old_resultP);
        Old_resultP.Free;
        Old_resultP := resultP;
      end;

      P1.Vertices[0,0] := Projection[ListIndex,0,2];
      P1.Vertices[0,1] := Projection[ListIndex,0,3];
      P1.Vertices[0,2] := Projection[ListIndex,1,3];
      P1.Vertices[0,3] := Projection[ListIndex,1,2];

      if Abs(P1.ContourArea(0)) > Epsilon then
      begin
        resultP := TGpcPolygonClass.CreateFromOperation(GPC_UNION, P1, Old_resultP);
        Old_resultP.Free;
        Old_resultP := resultP;
      end;

      P1.Vertices[0,0] := Projection[ListIndex,0,3];
      P1.Vertices[0,1] := Projection[ListIndex,0,4];
      P1.Vertices[0,2] := Projection[ListIndex,1,4];
      P1.Vertices[0,3] := Projection[ListIndex,1,3];

      if Abs(P1.ContourArea(0)) > Epsilon then
      begin
        resultP := TGpcPolygonClass.CreateFromOperation(GPC_UNION, P1, Old_resultP);
        Old_resultP.Free;
        Old_resultP := resultP;
      end;

      P1.Vertices[0,0] := Projection[ListIndex,0,4];
      P1.Vertices[0,1] := Projection[ListIndex,0,1];
      P1.Vertices[0,2] := Projection[ListIndex,1,1];
      P1.Vertices[0,3] := Projection[ListIndex,1,4];

      if Abs(P1.ContourArea(0)) > Epsilon then
      begin
        resultP := TGpcPolygonClass.CreateFromOperation(GPC_UNION, P1, Old_resultP);
        Old_resultP.Free;
        Old_resultP := resultP;
      end;
    end;

    P1.NumberOfContours := 0;

    Count := 0;
    while Old_resultP.NumberOfContours > 1 do
    begin
      resultP := TGpcPolygonClass.CreateFromOperation(GPC_UNION, P1, Old_resultP);
      Old_resultP.Free;
      Old_resultP := resultP;
      Inc(Count);
      if (Count > 100) then
      begin
        Break;
      end;
    end;

    Area := 0;
    ContourIndex := 0;
    for Index := 0 to resultP.NumberOfContours - 1 do
    begin
      if Index = 0 then
      begin
        Area := Abs(resultP.ContourArea(Index));
      end
      else
      begin
        if Area < Abs(resultP.ContourArea(Index)) then
        begin
          Assert(Area = 0);
          Area := Abs(resultP.ContourArea(Index));
          ContourIndex := Index;
        end;
      end;
    end;

    SetLength(Result, resultP.VertexCount[ContourIndex]);
    for NodeIndex := 0 to resultP.VertexCount[ContourIndex] - 1 do
    begin
      Result[NodeIndex] := resultP.Vertices[ContourIndex,NodeIndex];
    end;
  finally
    P1.Free;
    Old_resultP.Free;
  end;



end;

function TSutraElement3D.CellSection(Node: TSutraNode3D): TQuadPair3D;
var
  NodePostion: Integer;
  LayerAdd: Integer;
  NextNode: Integer;
  PriorNode: Integer;
begin
  NodePostion := Nodes.IndexOfNode(Node);
  Assert(NodePostion >= 0);
  Assert(NodePostion <= 7);
  if NodePostion >= 4 then
  begin
    LayerAdd := 4;
    Dec(NodePostion, 4);
  end
  else
  begin
    LayerAdd := 0;
  end;

  if NodePostion = 3 then
  begin
    NextNode := 0;
  end
  else
  begin
    NextNode := NodePostion + 1;
  end;

  if NodePostion = 0 then
  begin
    PriorNode := 3;
  end
  else
  begin
    PriorNode := NodePostion - 1;
  end;

  Result[0][1] := Node.NodeLocation;
  Result[0][2] := Average2Point3D(
    Node.NodeLocation,
    Nodes[LayerAdd+NextNode].Node.NodeLocation);
  Result[0][3] := Average4Point3D(
    Nodes[LayerAdd].Node.NodeLocation,
    Nodes[LayerAdd+1].Node.NodeLocation,
    Nodes[LayerAdd+2].Node.NodeLocation,
    Nodes[LayerAdd+2].Node.NodeLocation);
  Result[0][4] := Average2Point3D(
    Node.NodeLocation,
    Nodes[LayerAdd+PriorNode].Node.NodeLocation);

  Result[1][1] := Nodes[4-LayerAdd+NodePostion].Node.NodeLocation;
  Result[1][2] := Average2Point3D(
    Nodes[4-LayerAdd+NodePostion].Node.NodeLocation,
    Nodes[4-LayerAdd+NextNode].Node.NodeLocation);
  Result[1][3] := Average4Point3D(
    Nodes[4-LayerAdd].Node.NodeLocation,
    Nodes[4-LayerAdd+1].Node.NodeLocation,
    Nodes[4-LayerAdd+2].Node.NodeLocation,
    Nodes[4-LayerAdd+2].Node.NodeLocation);
  Result[1][4] := Average2Point3D(
    Nodes[4-LayerAdd+NodePostion].Node.NodeLocation,
    Nodes[4-LayerAdd+PriorNode].Node.NodeLocation);

  Result[1][1] := Average2Point3D(Result[0][1], Result[1][1]);
  Result[1][2] := Average2Point3D(Result[0][2], Result[1][2]);
  Result[1][3] := Average2Point3D(Result[0][3], Result[1][3]);
  Result[1][4] := Average2Point3D(Result[0][4], Result[1][4]);
end;

function TSutraElement3D.CenterElevation: double;
var
  NodeIndex: Integer;
  ANode: TSutraNode3D;
begin
  Assert(Nodes.Count = 8);
  result := 0;
  for NodeIndex := 0 to 7 do
  begin
    ANode := Nodes[NodeIndex].Node;
    result := result + ANode.Z;
  end;
  result := result/8;
end;

constructor TSutraElement3D.Create(Collection: TCollection);
begin
  inherited;
  FNodes := TSutraNodeNumber3D_Collection.Create(Model);
  FActiveNodes := TSutraNode3D_List.Create;
end;

destructor TSutraElement3D.Destroy;
begin
  FActiveNodes.Free;
  FNodes.Free;
  inherited;
end;

function TSutraElement3D.ElementShape: TQuadPair3D;
begin
  Result[0][1] := Nodes[0].Node.NodeLocation;
  Result[0][2] := Nodes[1].Node.NodeLocation;
  Result[0][3] := Nodes[2].Node.NodeLocation;
  Result[0][4] := Nodes[3].Node.NodeLocation;
  Result[1][1] := Nodes[4].Node.NodeLocation;
  Result[1][2] := Nodes[5].Node.NodeLocation;
  Result[1][3] := Nodes[6].Node.NodeLocation;
  Result[1][4] := Nodes[7].Node.NodeLocation;
end;

function TSutraElement3D.GetActiveNode(Index: Integer): INode;
begin
  result := FActiveNodes[Index];
end;

function TSutraElement3D.GetActiveNodeCount: integer;
begin
  result := FActiveNodes.Count;
end;

function TSutraElement3D.GetCenterLocation: TPoint3d;
var
  NodeIndex: Integer;
  ANode: TSutraNode3D;
begin
  Assert(Nodes.Count = 8);
  Result.x := 0;
  Result.y := 0;
  Result.z := 0;
  for NodeIndex := 0 to 7 do
  begin
    ANode := Nodes[NodeIndex].Node;
    result.x := result.x + ANode.X;
    result.y := result.y + ANode.y;
    result.z := result.z + ANode.z;
  end;
  result.x := result.x/8;
  result.y := result.y/8;
  result.z := result.z/8;
end;

//function TSutraElement3D.GetNode(Index: Integer): INode;
//begin
//  result := Nodes[Index].Node;
//end;
//
//function TSutraElement3D.GetNodeCount: integer;
//begin
//  result := Nodes.Count;
//end;

function TSutraElement3D.LowerElevation: double;
var
  NodeIndex: Integer;
  ANode: TSutraNode3D;
begin
  Assert(Nodes.Count = 8);
  result := 0;
  for NodeIndex := 4 to 7 do
  begin
    ANode := Nodes[NodeIndex].Node;
    result := result + ANode.Z;
  end;
  result := result/4;
end;

function TSutraElement3D.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

procedure TSutraElement3D.SetNodes(const Value: TSutraNodeNumber3D_Collection);
begin
  FNodes.Assign(Value);
end;

procedure TSutraElement3D.UpdateActiveNodeList;
var
  Node: TSutraNode3D;
  NodeIndex: Integer;
begin
  FActiveNodes.Clear;
  for NodeIndex := 0 to Nodes.Count - 1 do
  begin
    Node := Nodes[NodeIndex].Node;
    if Node.Active then
    begin
      FActiveNodes.Add(Node);
    end;
  end;
end;

procedure TSutraElement3D.UpdateNodes;
var
  Node: TSutraNode3D;
  NodeIndex: Integer;
begin
  for NodeIndex := 0 to Nodes.Count - 1 do
  begin
    Node := Nodes[NodeIndex].Node;
    Node.FElements.Add(Self);
  end;
end;

function TSutraElement3D.UpperElevation: double;
var
  NodeIndex: Integer;
  ANode: TSutraNode3D;
begin
  Assert(Nodes.Count = 8);
  result := 0;
  for NodeIndex := 0 to 3 do
  begin
    ANode := Nodes[NodeIndex].Node;
    result := result + ANode.Z;
  end;
  result := result/4;
end;

function TSutraElement3D._AddRef: Integer;
begin
  result := -1;
end;

function TSutraElement3D._Release: Integer;
begin
  result := -1;
end;

{ TSutraMesh3D }

procedure TSutraMesh3D.CalculateMinMax(DataSet: TDataArray;
  var MinMaxInitialized: Boolean; var MinMax: TMinMax;
  StringValues: TStringList);
begin
  SetMinMax(DataSet, MinMaxInitialized, MinMax, StringValues,
    DataSet.LayerCount, DataSet.RowCount, DataSet.ColumnCount);
  if DataSet.Limits.LogTransform and (MinMax.RMinPositive > 0)
    and (MinMax.RMax > 0) then
  begin
    MinMax.LogRMin := Log10(MinMax.RMinPositive);
    MinMax.LogRMax := Log10(MinMax.RMax);
  end
  else
  begin
    MinMax.LogRMin := 0;
    MinMax.LogRMax := 0;
  end;
  if DataSet.Datatype = rdtString then
  begin
    if StringValues.Count > 0 then
    begin
      MinMax.SMin := StringValues[0];
      MinMax.SMax := StringValues[StringValues.Count-1];
    end;
  end;

end;

procedure TSutraMesh3D.CheckUpdateElevations;
begin
  if ElevationsNeedUpdating then
  begin
    ElevationsNeedUpdating := False;
    ThreeDGridObserver.UpToDate := True;
    UpdateElevations;
  end;
end;

procedure TSutraMesh3D.Clear;
begin
  BeginUpdate;
  try
    Elements.Clear;
    Nodes.Clear;
    Mesh2D.Clear;
  finally
    EndUpdate;
  end;
end;

constructor TSutraMesh3D.Create(Model: TBaseModel);
var
  LocalModel: TCustomModel;
begin
  inherited Create(Model);
  FElevationsNeedUpdating := True;
  FCrossSection := TCrossSection.Create(Model);
  FElements := TSutraElement3D_Collection.Create(Model);
  FNodes := TSutraNode3D_Collection.Create(Model);
  FMesh2D := TSutraMesh2D.Create(Model, self);
  FActiveNodes := TSutraNode3D_List.Create;
  FActiveElements := TSutraElement3DList.Create;


  FMeshType := mt3D;
  LocalModel := Model as TCustomModel;
  FThreeDGridObserver := LocalModel.ThreeDGridObserver;
  FThreeDGridObserver.OnUpToDateSet := LocalModel.OnTopSutraMeshChanged;
end;

destructor TSutraMesh3D.Destroy;
begin
  FActiveElements.Free;
  FActiveNodes.Free;
  FMesh2D.Free;
  FNodes.Free;
  FElements.Free;
  FCrossSection.Free;
  inherited;
end;

function Average2Point(const Point1, Point2: TPoint2D): TPoint2D;
begin
  Result.x := (Point1.x + Point2.x)/2;
  Result.y := (Point1.y + Point2.y)/2;
end;

function Average4Point(const Point1, Point2, Point3, Point4: TPoint2D): TPoint2D;
begin
  Result.x := (Point1.x + Point2.x + Point3.x + Point4.x)/4;
  Result.y := (Point1.y + Point2.y + Point3.y + Point4.y)/4;
end;

procedure TSutraMesh3D.DrawFront(const BitMap: TBitmap32);
var
  NodeList: TSutraNode2D_List;
  Node2D_Index: Integer;
  Node2D: TSutraNode2D;
  LayerIndex: Integer;
  Node3D: TSutraNode3D;
  SegmentAngle: double;
  Angle: double;
  X_Float: double;
  Points: GoPhastTypes.TPointArray;
  Points2: GoPhastTypes.TPointArray;
  X_Int: Integer;
  ZoomBox: TQRbwZoomBox2;
  ElementList: TSutraElement2D_List;
  Element2D: TSutraElement2D;
  Element3D: TSutraElement3D;
  ARect: TRect;
  Element2D_Index: integer;
  ElCenter: TPoint2D;
  ZInt: Integer;
  ElementOutlines: array of array of TPoint;
  ElementOutlinesFloat: array of array of TPoint2D;
  TwoDElements: array of TSutraElement2D;
  YInt: Integer;
  ElementIndex: Integer;
  PriorNode2D: TSutraNode2D;
  Index1: Integer;
  Index2: Integer;
  Delta: Integer;
  CellOutlines: array of array of TPoint;
  StringValues: TStringlist;
  RowIndex: Integer;
  ColIndex: Integer;
  ShowColor: Boolean;
  Fraction: Double;
  AColor: TColor;
  Dummy: TPolygon32;
  Origin: TPoint2D;
  StartPoint: TPoint2D;
  CrossSectionLine: TLine2D;
  NumString: string;
  function Point2DtoPoint(const APoint: TPoint2D): TPoint;
  begin
    Result.X := ZoomBox.XCoord(APoint.x);
    Result.Y := ZoomBox.YCoord(APoint.y);
  end;
begin
  Dummy := nil;
  NodeList := TSutraNode2D_List.Create;
  ElementList := TSutraElement2D_List.Create;
  try
    Mesh2D.GetNodesOnSegment(CrossSection.Segment, NodeList);
    if NodeList.Count = 0 then
    begin
      Exit;
    end;

    SetLength(ElementOutlines, LayerCount+1, NodeList.Count);
    SetLength(ElementOutlinesFloat, LayerCount+1, NodeList.Count);

    SegmentAngle := CrossSection.Angle;
    SetLength(Points, 5);

    ZoomBox := frmGoPhast.frameFrontView.ZoomBox;

    Origin.x := 0;
    Origin.y := 0;
    CrossSectionLine[1] := CrossSection.StartPoint;
    CrossSectionLine[2] := CrossSection.EndPoint;
    StartPoint := ClosestPointOnLineFromPoint(CrossSectionLine,Origin);

    // Determine the locations of the outlines of the elements in cross section.
    for Node2D_Index := 0 to NodeList.Count - 1 do
    begin
      Node2D := NodeList[Node2D_Index];
      Angle := ArcTan2(Node2D.y - StartPoint.y,
        Node2D.x - StartPoint.x) - SegmentAngle;
      X_Float := Distance(StartPoint, Node2D.Location)*Cos(Angle)
        + StartPoint.x;
      X_Int := ZoomBox.XCoord(X_Float);
      for LayerIndex := 0 to LayerCount do
      begin
        Node3D := NodeArray[LayerIndex, Node2D.Number];
        YInt := ZoomBox.YCoord(Node3D.Z);
        ElementOutlines[LayerIndex, Node2D_Index].X := X_Int;
        ElementOutlines[LayerIndex, Node2D_Index].y := YInt;

        ElementOutlinesFloat[LayerIndex, Node2D_Index].X := X_Float;
        ElementOutlinesFloat[LayerIndex, Node2D_Index].y := Node3D.Z;
      end;
    end;

    Mesh2D.GetElementsOnSegment(CrossSection.Segment, ElementList);

    // Determine the 2D elements to use for drawing the mesh.
    SetLength(TwoDElements, NodeList.Count-1);
    for ElementIndex := 0 to Length(TwoDElements) - 1 do
    begin
      TwoDElements[ElementIndex] := nil;
    end;

    PriorNode2D := NodeList[0];
    for Node2D_Index := 1 to NodeList.Count - 1 do
    begin
      Node2D := NodeList[Node2D_Index];
      for Element2D_Index := 0 to ElementList.Count - 1 do
      begin
        Element2D := ElementList[Element2D_Index];
        Index1 := Element2D.Nodes.IndexOfNode(PriorNode2D);
        if (Index1 >= 0) then
        begin
          Index2 := Element2D.Nodes.IndexOfNode(Node2D);
          if  (Index2 >= 0) then
          begin
            if TwoDElements[Node2D_Index-1] = nil then
            begin
              TwoDElements[Node2D_Index-1] := Element2D;
            end
            else
            begin
              // Use the element that is further back
              Delta := Index2 - Index1;
              if (Delta = 1) or (Delta = -3) then
              begin
                TwoDElements[Node2D_Index-1] := Element2D;
              end;
            end;
          end;
        end;
      end;
      PriorNode2D := Node2D;
    end;

    if (ThreeDDataSet <> nil) and (ThreeDDataSet.Orientation = dso3D) then
    begin

      StringValues := TStringlist.Create;
      try
        ThreeDDataSet.Initialize;
        if ThreeDDataSet.DataType = rdtString then
        begin
          StringValues.Sorted := True;
          StringValues.Duplicates := dupIgnore;
          StringValues.CaseSensitive := True;
          StringValues.Capacity := ThreeDDataSet.LayerCount *
            ThreeDDataSet.RowCount * ThreeDDataSet.ColumnCount;
          for LayerIndex := 0 to ThreeDDataSet.LayerCount - 1 do
          begin
            for RowIndex := 0 to ThreeDDataSet.RowCount - 1 do
            begin
              for ColIndex := 0 to ThreeDDataSet.ColumnCount - 1 do
              begin
                if ThreeDDataSet.IsValue[LayerIndex, RowIndex, ColIndex] then
                begin
                  StringValues.Add(ThreeDDataSet.StringData[
                    LayerIndex, RowIndex, ColIndex]);
                end;
              end;
            end;
          end;
        end;
      finally
        StringValues.Free;
      end;

      case ThreeDDataSet.EvaluatedAt of
        eaBlocks:
          begin
            for Node2D_Index := 0 to NodeList.Count - 2 do
            begin
              Element2D := TwoDElements[Node2D_Index];
              if Element2D = nil then
              begin
                Continue;
              end;

              for LayerIndex := 0 to LayerCount - 1 do
              begin
                if not ElementArray[LayerIndex,
                  Element2D.ElementNumber].Active then
                begin
                  Continue;
                end;

                Points[0] := ElementOutlines[LayerIndex, Node2D_Index];
                Points[1] := ElementOutlines[LayerIndex+1, Node2D_Index];
                Points[2] := ElementOutlines[LayerIndex+1, Node2D_Index+1];
                Points[3] := ElementOutlines[LayerIndex, Node2D_Index+1];
                Points[4] := Points[0];

                GetDataSetValue(ThreeDDataSet, LayerIndex,
                  Element2D.ElementNumber, StringValues, ShowColor, Fraction);
                if ShowColor then
                begin
                  AColor := frmGoPhast.PhastModel.GridColorParameters.
                    FracToColor(Fraction);
                  DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
                    0, Points, Dummy, False, True);
                end;
              end;
            end;

          end;
        eaNodes:
          begin
            // Determine the outlines of the cells.
            SetLength(CellOutlines, LayerCount+2, NodeList.Count*2-1);
            // Set the corners at the top right and lower right.
            CellOutlines[0,NodeList.Count*2-2] :=
              Point2DtoPoint(ElementOutlinesFloat[0,NodeList.Count-1]);
            CellOutlines[LayerCount+1,NodeList.Count*2-2] :=
              Point2DtoPoint(ElementOutlinesFloat[LayerCount,NodeList.Count-1]);

            // Set the locations of the cell edge at the top and bottom edges
            for Index2 := 0 to NodeList.Count - 2 do
            begin
              CellOutlines[0,Index2*2] := Point2DtoPoint(ElementOutlinesFloat[0, Index2]);
              CellOutlines[0,Index2*2+1] := Point2DtoPoint(Average2Point(
                ElementOutlinesFloat[0,Index2+1], ElementOutlinesFloat[0, Index2]));

              CellOutlines[LayerCount+1,Index2*2] := Point2DtoPoint(ElementOutlinesFloat[LayerCount, Index2]);
              CellOutlines[LayerCount+1,Index2*2+1] := Point2DtoPoint(Average2Point(
                ElementOutlinesFloat[LayerCount,Index2+1], ElementOutlinesFloat[LayerCount, Index2]));
            end;

            // Set the locations of the cell corners at the right edge.
            for Index1 := 1 to LayerCount do
            begin
              CellOutlines[Index1,NodeList.Count*2-2] := Point2DtoPoint(Average2Point(
                ElementOutlinesFloat[Index1,NodeList.Count-1],
                ElementOutlinesFloat[Index1-1,NodeList.Count-1]));
            end;

            // Set the location of the cell corners in the interior
            for Index1 := 1 to LayerCount do
            begin
              for Index2 := 0 to NodeList.Count - 2 do
              begin
                CellOutlines[Index1,Index2*2] := Point2DtoPoint(Average2Point(
                  ElementOutlinesFloat[Index1-1,Index2],
                  ElementOutlinesFloat[Index1, Index2]));

                CellOutlines[Index1,Index2*2+1] := Point2DtoPoint(Average4Point(
                  ElementOutlinesFloat[Index1-1,Index2+1],
                  ElementOutlinesFloat[Index1, Index2+1],
                  ElementOutlinesFloat[Index1-1,Index2],
                  ElementOutlinesFloat[Index1, Index2]));
              end;
            end;

            SetLength(Points,5);
            SetLength(Points2,7);
            for Node2D_Index := 0 to NodeList.Count-1 do
            begin
              Node2D := NodeList[Node2D_Index];
              for LayerIndex := 0 to LayerCount do
              begin
                if not NodeArray[LayerIndex, Node2D.Number].Active then
                begin
                  Continue;
                end;

                GetDataSetValue(ThreeDDataSet, LayerIndex,
                  Node2D.Number, StringValues, ShowColor, Fraction);
                if ShowColor then
                begin
                  AColor := frmGoPhast.PhastModel.GridColorParameters.
                    FracToColor(Fraction);
                  if (Node2D_Index = 0) or (TwoDElements[Node2D_Index-1] = nil) then
                  begin
                    Points[0] := CellOutlines[LayerIndex, Node2D_Index*2];
                    Points[1] := CellOutlines[LayerIndex, Node2D_Index*2+1];
                    Points[2] := CellOutlines[LayerIndex+1, Node2D_Index*2+1];
                    Points[3] := CellOutlines[LayerIndex+1, Node2D_Index*2];
                    Points[4] := Points[0];

                    DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
                      0, Points, Dummy, False, True);
                  end
                  else if (Node2D_Index = NodeList.Count-1)
                    or (TwoDElements[Node2D_Index] = nil) then
                  begin
                    Points[0] := CellOutlines[LayerIndex, Node2D_Index*2-1];
                    Points[1] := CellOutlines[LayerIndex, Node2D_Index*2];
                    Points[2] := CellOutlines[LayerIndex+1, Node2D_Index*2];
                    Points[3] := CellOutlines[LayerIndex+1, Node2D_Index*2-1];
                    Points[4] := Points[0];

                    DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
                      0, Points, Dummy, False, True);
                  end
                  else
                  begin
                    Points2[0] := CellOutlines[LayerIndex, Node2D_Index*2-1];
                    Points2[1] := CellOutlines[LayerIndex, Node2D_Index*2];
                    Points2[2] := CellOutlines[LayerIndex, Node2D_Index*2+1];
                    Points2[3] := CellOutlines[LayerIndex+1, Node2D_Index*2+1];
                    Points2[4] := CellOutlines[LayerIndex+1, Node2D_Index*2];
                    Points2[5] := CellOutlines[LayerIndex+1, Node2D_Index*2-1];
                    Points2[6] := Points2[0];

                    DrawBigPolygon32(BitMap, Color32(AColor), Color32(AColor),
                      0, Points2, Dummy, False, True);
                  end;
                end;
              end;

            end;
          end
        else Assert(False);
      end;
    end;

    SetLength(Points,5);
    // Draw the outlines of the elements.
    for Node2D_Index := 0 to NodeList.Count - 2 do
    begin
      Element2D := TwoDElements[Node2D_Index];
      if Element2D = nil then
      begin
        Continue;
      end;


      // draw all but the selected layer.
      for LayerIndex := 0 to LayerCount - 1 do
      begin
        if LayerIndex = SelectedLayer then
        begin
          Continue;
        end;
        if not ElementArray[LayerIndex, Element2D.ElementNumber].Active then
        begin
          Continue;
        end;

        Points[0] := ElementOutlines[LayerIndex, Node2D_Index];
        Points[1] := ElementOutlines[LayerIndex+1, Node2D_Index];
        Points[2] := ElementOutlines[LayerIndex+1, Node2D_Index+1];
        Points[3] := ElementOutlines[LayerIndex, Node2D_Index+1];
        Points[4] := Points[0];

        DrawBigPolyline32(BitMap, clBlack32, OrdinaryGridLineThickness,
          Points, True);
      end;

      // draw the selected layer last using a different color.
      if SelectedLayer < LayerCount then
      begin
        if not ElementArray[SelectedLayer, Element2D.ElementNumber].Active then
        begin
          Continue;
        end;
        Points[0] := ElementOutlines[SelectedLayer, Node2D_Index];
        Points[1] := ElementOutlines[SelectedLayer+1, Node2D_Index];
        Points[2] := ElementOutlines[SelectedLayer+1, Node2D_Index+1];
        Points[3] := ElementOutlines[SelectedLayer, Node2D_Index+1];
        Points[4] := Points[0];
      end
      else
      begin
        if not ElementArray[SelectedLayer-1, Element2D.ElementNumber].Active then
        begin
          Continue;
        end;
        Points[0] := ElementOutlines[SelectedLayer-1, Node2D_Index];
        Points[1] := ElementOutlines[SelectedLayer, Node2D_Index];
        Points[2] := ElementOutlines[SelectedLayer, Node2D_Index+1];
        Points[3] := ElementOutlines[SelectedLayer-1, Node2D_Index+1];
        Points[4] := Points[0];
      end;

      DrawBigPolyline32(BitMap, Color32(CrossSection.Color),
        OrdinaryGridLineThickness, Points, True);
    end;

//    if DrawNodeNumber then
    begin
      for Node2D_Index := 0 to NodeList.Count - 1 do
      begin
        Node2D := NodeList[Node2D_Index];
        Angle := ArcTan2(Node2D.y - StartPoint.y,
          Node2D.x - StartPoint.x) - SegmentAngle;
        X_Float := Distance(StartPoint, Node2D.Location)*Cos(Angle)
          + StartPoint.x;
        X_Int := ZoomBox.XCoord(X_Float);
        for LayerIndex := 0 to LayerCount do
        begin
          Node3D := NodeArray[LayerIndex, Node2D.Number];
          if Node3D.Active then
          begin
            YInt := ZoomBox.YCoord(Node3D.Z);

            NumString := InttoStr(Node3D.Number+1);
  //          Extent := BitMap.TextExtent(NumString);
            BitMap.Textout(X_Int+1, YInt+1, NumString);
          end;

        end;
      end;
    end;


    // Draw dots at the center of each element intersected by the cross section.
    SetLength(Points, 5);
    for Element2D_Index := 0 to ElementList.Count - 1 do
    begin
      Element2D := ElementList[Element2D_Index];


      ElCenter := Element2D.Center;
      Angle := ArcTan2(ElCenter.y - StartPoint.y,
        ElCenter.x - StartPoint.x) - SegmentAngle;
      X_Float := Distance(StartPoint, ElCenter)*Cos(Angle)
        + StartPoint.x;
      X_Int := ZoomBox.XCoord(X_Float);
      ARect.Left := X_Int-1;
      ARect.Right := X_Int+1;
      for LayerIndex := 0 to LayerCount - 1 do
      begin
        Element3D := ElementArray[LayerIndex, Element2D.ElementNumber];
        if Element3D.active then
        begin
          ZInt := ZoomBox.YCoord(Element3D.CenterElevation);
          ARect.Top := ZInt-1;
          ARect.Bottom := ZInt+1;
          if LayerIndex = SelectedLayer then
          begin
            DrawBigRectangle32(BitMap, Color32(CrossSection.Color),
              Color32(CrossSection.Color), OrdinaryGridLineThickness, ARect);
          end
          else
          begin
            DrawBigRectangle32(BitMap, clBlack32, clBlack32,
              OrdinaryGridLineThickness, ARect);
          end;
        end;
      end;
    end;
  finally
    ElementList.Free;
    NodeList.Free;
  end;
end;

procedure TSutraMesh3D.DrawPointsOnCrossSection(BitMap: TBitmap32);
var
  NodesOnSegment: TSutraNode2D_List;
  Points: GoPhastTypes.TPointArray;
  NodeIndex: Integer;
  ANode: TSutraNode2D;
  ZoomBox: TQRbwZoomBox2;
  ARect: TRect;
  ElList: TSutraElement2D_List;
  ElementIndex: Integer;
  AnElement2D: TSutraElement2D;
  ElCenter: TPoint2D;
  ElementLayer: Integer;
  AnElement3D: TSutraElement3D;
begin
  ZoomBox := frmGoPhast.frameTopView.ZoomBox;
  NodesOnSegment := TSutraNode2D_List.Create;
  try
    Mesh2D.GetNodesOnSegment(CrossSection.Segment, NodesOnSegment);
    SetLength(Points, NodesOnSegment.Count);
    for NodeIndex := 0 to NodesOnSegment.Count - 1 do
    begin
      ANode := NodesOnSegment[NodeIndex];
      Points[NodeIndex].X := ZoomBox.XCoord(ANode.X);
      Points[NodeIndex].Y := ZoomBox.YCoord(ANode.Y);
    end;
    DrawBigPolyline32(BitMap, Color32(CrossSection.Color), ThickGridLineThickness,
      Points, True);
//    for NodeIndex := 0 to Length(Points) - 1 do
//    begin
//      ARect.Left := Points[NodeIndex].X -2;
//      ARect.Right := Points[NodeIndex].X +2;
//      ARect.Top := Points[NodeIndex].Y -2;
//      ARect.Bottom := Points[NodeIndex].Y +2;
//      DrawBigRectangle32(BitMap, Color32(CrossSection.Color), Color32(CrossSection.Color),
//        OrdinaryGridLineThickness, ARect);
//    end;
  finally
    NodesOnSegment.Free;
  end;

  ElementLayer := SelectedLayer;
  if ElementLayer >= LayerCount then
  begin
    ElementLayer := LayerCount-1;
  end;
  ElList := TSutraElement2D_List.Create;
  try
    Mesh2D.GetElementsOnSegment(CrossSection.Segment, ElList);
    for ElementIndex := 0 to ElList.Count - 1 do
    begin
      AnElement2D := ElList[ElementIndex];
      AnElement3D := ElementArray[ElementLayer, AnElement2D.ElementNumber];
      if AnElement3D.Active then
      begin
        ElCenter := AnElement2D.Center;
        ARect.Left := ZoomBox.XCoord(ElCenter.x)-2;
        ARect.Top := ZoomBox.YCoord(ElCenter.y)-2;
        ARect.Right := ARect.Left + 4 ;
        ARect.Bottom := ARect.Top + 4;
        DrawBigRectangle32(BitMap, Color32(CrossSection.Color), Color32(CrossSection.Color),
          OrdinaryGridLineThickness, ARect);
      end;
    end;
  finally
    ElList.Free;
  end;
end;

procedure TSutraMesh3D.EndUpdate;
begin
  inherited;
  if (FUpdateCount = 0) then
  begin
    frmGoPhast.PhastModel.DataArrayManager.InvalidateAll3DDataSets;
  end;
end;

procedure TSutraMesh3D.Draw(const BitMap: TBitmap32;
  const ViewDirection: TViewDirection);
begin
  if FUpdateCount > 0 then
  begin
    Exit;
  end;
  CheckUpdateElevations;
  case ViewDirection of
    vdTop:
      begin
        Mesh2D.Draw(BitMap);
        if MeshType = mt3D then
        begin
          CrossSection.Draw(BitMap);
          DrawPointsOnCrossSection(BitMap);
        end;
      end;
    vdFront:
      begin
        if (MeshType = mt3D) and (LayerCount > 0) then
        begin
          DrawFront(BitMap);
        end;
      end;
    vdSide: ;
    else Assert(False);
  end;
end;

function TSutraMesh3D.GetActiveElement(Index: Integer): IElement;
begin
  result := FActiveElements[Index];
end;

function TSutraMesh3D.GetActiveElementCount: integer;
begin
  result := FActiveElements.Count;
end;

function TSutraMesh3D.GetActiveNode(Index: Integer): INode;
begin
  result := FActiveNodes[Index];
end;

function TSutraMesh3D.GetActiveNodeCount: integer;
begin
  result := FActiveNodes.Count;
end;

function TSutraMesh3D.GetCanDraw: boolean;
begin
  result := FCanDraw and (frmGoPhast.PhastModel.DataSetUpdateCount = 0);
end;

//function TSutraMesh3D.GetElement(Index: Integer): IElement;
//begin
//  result := Elements[Index];
//end;

function TSutraMesh3D.GetElementArrayMember(Layer,
  Col: Integer): TSutraElement3D;
begin
  CheckUpdateElevations;
  result := FElementArray[Layer, Col];
end;

//function TSutraMesh3D.GetElementCount: integer;
//begin
//  result := Elements.Count;
//end;

function TSutraMesh3D.FrontPolygons(Angle: Double;
  EvaluatedAt: TEvaluatedAt; out Limits: TLimitsArray): TCellElementPolygons2D;
var
  LayerIndex: Integer;
  ElementIndex: Integer;
  QuadPairList: TQuadPair3DList;
  Element: TSutraElement3D;
  Node: TSutraNode3D;
  NodeIndex: Integer;
begin
  result := nil;
  QuadPairList := TQuadPair3DList.Create;
  try
    case EvaluatedAt of
      eaBlocks:
        begin
          SetLength(result, Length(FElementArray), Length(FElementArray[0]));
          SetLength(Limits, Length(FElementArray), Length(FElementArray[0]));
          for LayerIndex := 0 to Length(FElementArray) - 1 do
          begin
            for ElementIndex := 0 to Length(FElementArray[0]) - 1 do
            begin
              Element := ElementArray[LayerIndex, ElementIndex];
              if Element.Active then
              begin
                QuadPairList.Clear;
                QuadPairList.Add(Element.ElementShape);
                try
                  Result[LayerIndex,ElementIndex] :=
                    QuadPairsToPolygon(QuadPairList, Angle, Limits[LayerIndex,ElementIndex]);
                except
                  ShowMessage(IntToStr(ElementIndex) + ' ' + IntToStr(LayerIndex));
                  raise;
                end;
              end
              else
              begin
                SetLength(Result[LayerIndex,ElementIndex], 0);
              end;
            end;
          end;
        end;
      eaNodes:
        begin
          SetLength(result, Length(FNodeArray), Length(FNodeArray[0]));
          SetLength(Limits, Length(FNodeArray), Length(FNodeArray[0]));
          for LayerIndex := 0 to Length(FNodeArray) - 1 do
          begin
            for NodeIndex := 0 to Length(FNodeArray[0]) - 1 do
            begin
              Node := NodeArray[LayerIndex, NodeIndex];
              if Node.Active then
              begin
                QuadPairList.Clear;
                QuadPairList.Capacity := Node.FElements.Count;
                for ElementIndex := 0 to Node.FElements.Count - 1 do
                begin
                  QuadPairList.Add(Node.FElements[ElementIndex].CellSection(Node));
                end;
                Result[LayerIndex,NodeIndex] :=
                  QuadPairsToPolygon(QuadPairList, Angle,
                  Limits[LayerIndex,NodeIndex]);
              end
              else
              begin
                SetLength(Result[LayerIndex,NodeIndex], 0);
              end;
            end;
          end;
        end;
      else Assert(False);
    end;
  finally
    QuadPairList.Free;
  end;
end;

function TSutraMesh3D.GetLayerCount: Integer;
begin
  if frmGoPhast.PhastModel.SutraLayerStructure <> nil then
  begin
    result := frmGoPhast.PhastModel.SutraLayerStructure.LayerCount;
  end
  else
  begin
    result := 0;
  end;

end;

procedure TSutraMesh3D.GetMinMax(var MinMax: TMinMax; DataSet: TDataArray;
  StringValues: TStringList);
var
  MinMaxInitialized: Boolean;
begin
  MinMax.RMinPositive := 0;
  MinMaxInitialized := False;
  StringValues.Sorted := True;
  StringValues.Duplicates := dupIgnore;
  StringValues.CaseSensitive := True;
  StringValues.Capacity := DataSet.LayerCount*DataSet.RowCount*DataSet.ColumnCount;
  CalculateMinMax(DataSet, MinMaxInitialized, MinMax, StringValues);

end;

//function TSutraMesh3D.GetNode(Index: Integer): INode;
//begin
//  result := Nodes[Index];
//end;

function TSutraMesh3D.GetNodeArrayMember(Layer, Col: Integer): TSutraNode3D;
begin
//  CheckUpdateElevations;
  result := FNodeArray[Layer, Col];
end;

//function TSutraMesh3D.GetNodeCount: integer;
//begin
//  result := Nodes.Count;
//end;

function TSutraMesh3D.GetSelectedLayer: integer;
begin
  result := Mesh2D.SelectedLayer;
end;

function TSutraMesh3D.GetThreeDDataSet: TDataArray;
begin
  result := Mesh2D.ThreeDDataSet;
end;

function TSutraMesh3D.GetTopDataSet: TDataArray;
begin
  result := Mesh2D.TopDataSet;
end;

function TSutraMesh3D.GetTopGridObserver: TObserver;
begin
  result := Mesh2D.TopGridObserver;
end;

function TSutraMesh3D.MeshLimits(ViewDirection: TViewDirection): TGridLimit;
var
  Node: TSutraNode3D;
  NodeIndex: Integer;
begin
  CheckUpdateElevations;
  case ViewDirection of
    vdTop: result := Mesh2D.MeshLimits;
    vdFront:
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
          result.MinZ := Node.Z;
          result.MaxX := Node.X;
          result.MaxZ := Node.Z;
          for NodeIndex := 1 to Nodes.Count - 1 do
          begin
            Node := Nodes[NodeIndex];
            if Node.X > result.MaxX then
            begin
              result.MaxX := Node.X
            end;
            if Node.X < result.MinX then
            begin
              result.MinX := Node.X
            end;
            if Node.Z > result.MaxZ then
            begin
              result.MaxZ := Node.Z
            end;
            if Node.Z < result.MinZ then
            begin
              result.MinZ := Node.Z
            end;
          end;
        end;

      end;
    vdSide: Assert(False);
    else  Assert(False);
  end;
end;

function TSutraMesh3D.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
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

procedure TSutraMesh3D.MeshChanged;
begin
  if FMeshUpdate > 0 then Exit;

  CanDraw := True;
//  if (LayerCount <= 0) or (RowCount <= 0) or (ColumnCount <= 0) then
//  begin
//    TopDataSet := nil;
//    FrontDataSet := nil;
//    SideDataSet := nil;
//    ThreeDDataSet := nil;
//
//    TopContourDataSet := nil;
//    FrontContourDataSet := nil;
//    SideContourDataSet := nil;
//    ThreeDContourDataSet := nil;
//  end;

//  FBlockGlGrid := nil;
//  FreeAndNil(FBlockGridCache);
//  FNodeGlGrid := nil;
//  FreeAndNil(FNodeGridCache);
//
//  FTopElementContourGrid := nil;
//  FTopNodeContourGrid := nil;
//  FFrontElementContourGrid := nil;
//  FFrontNodeContourGrid := nil;
//  FSideElementContourGrid := nil;
//  FSideNodeContourGrid := nil;
//  FRecordedShell := False;
//  FRecordedSideGrid := False;
//  FRecordedFrontGrid := False;
//  FRecordedTopGrid := False;

//  if FModel is TPhastModel then
//  begin
//    TPhastModel(FModel).InvalidateMapping;
//  end;

  if frmGoPhast.frame3DView.glWidModelView <> nil then
  begin
//    FNeedToRedraw3d := True;
  end;
  frmGoPhast.EnableVisualization;
//  ViewsChanged;
end;

procedure TSutraMesh3D.SetCanDraw(const Value: boolean);
begin
  FCanDraw := Value;
end;

procedure TSutraMesh3D.SetCrossSection(const Value: TCrossSection);
begin
  FCrossSection.Assign(Value);
end;

procedure TSutraMesh3D.SetElementDrawingChoice(const Value: TDrawingChoice);
begin
  FElementDrawingChoice := Value;
  Mesh2D.ElementDrawingChoice := Value;
end;

procedure TSutraMesh3D.SetElements(const Value: TSutraElement3D_Collection);
begin
  FElements.Assign(Value);
end;

procedure TSutraMesh3D.SetMesh2D(const Value: TSutraMesh2D);
begin
  FMesh2D.Assign(Value);
end;

procedure TSutraMesh3D.SetMeshType(const Value: TMeshType);
begin
  if FMeshType <> Value then
  begin
    FMeshType := Value;
    ElevationsNeedUpdating := True;
    frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
    frmGoPhast.frameFrontView.ZoomBox.InvalidateImage32;
    frmGoPhast.frameSideView.ZoomBox.InvalidateImage32;
    InvalidateModel;
  end;
end;

procedure TSutraMesh3D.SetNodeDrawingChoice(const Value: TDrawingChoice);
begin
  FNodeDrawingChoice := Value;
  Mesh2D.NodeDrawingChoice := Value;
end;

procedure TSutraMesh3D.SetNodes(const Value: TSutraNode3D_Collection);
begin
  FNodes.Assign(Value);
end;

procedure TSutraMesh3D.SetSelectedLayer(Value: integer);
begin
  if Value < 0 then
  begin
    Value := 0;
  end
  else
  begin
    if LayerCount > 0 then
    begin
      case frmGoPhast.frameTopView.EvaluatedAt of
        eaBlocks:
          begin
            if Value > LayerCount - 1 then
            begin
              Value := LayerCount - 1;
            end;
          end;
        eaNodes:
          begin
            if Value > LayerCount then
            begin
              Value := LayerCount;
            end;
          end;
      else
        Assert(False);
      end;
    end;
  end;
  if Mesh2D.SelectedLayer <> Value then
  begin
    Mesh2D.SelectedLayer := Value;
//    if not (csLoading in frmGoPhast.PhastModel.ComponentState) then
//    begin
//      FDisplayLayer := Value;
//    end;
//    FRecordedTopGrid := False;
    if Assigned(FOnSelectedLayerChange) then
    begin
      FOnSelectedLayerChange(self);
    end;
  end;
end;

procedure TSutraMesh3D.SetThreeDDataSet(const Value: TDataArray);
begin
  frmGoPhast.TopDiscretizationChanged := True;
  frmGoPhast.FrontDiscretizationChanged := True;
  frmGoPhast.SideDiscretizationChanged := True;
  Mesh2D.ThreeDDataSet := Value;
end;

procedure TSutraMesh3D.SetThreeDGridObserver(const Value: TObserver);
begin
  FThreeDGridObserver := Value;
  if Assigned(FThreeDGridObserver) then
  begin
    FThreeDGridObserver.UpToDate := False;
    FThreeDGridObserver.UpToDate := True;
  end;
end;

procedure TSutraMesh3D.SetTopDataSet(const Value: TDataArray);
begin
  Mesh2D.TopDataSet := Value;
end;

procedure TSutraMesh3D.SetTopGridObserver(const Value: TObserver);
begin
  Mesh2D.TopGridObserver := Value;
end;

procedure TSutraMesh2D.SetThreeDDataSet(const Value: TDataArray);
begin
  if FThreeDDataSet <> Value then
  begin
    FThreeDDataSet := Value;
    frmGoPhast.InvalidateImage32AllViews;

//    NeedToRecalculate3DCellColors := True;
//    GridChanged;
  end;
end;

procedure TSutraMesh2D.SetTopDataSet(const Value: TDataArray);
begin
//  FTopDataSet := Value;
  Assert((Value = nil) or (Value.Orientation in [dsoTop, dso3D]));
  if FTopDataSet <> Value then
  begin
    FTopDataSet := Value;
    frmGoPhast.TopDiscretizationChanged := True;
    frmGoPhast.InvalidateImage32AllViews;
//    NeedToRecalculateTopCellColors := True;
//
//    // This ensures that the selected layer
//    // is still valid for the new data set.
//    frmGoPhast.Grid.SelectedLayer := frmGoPhast.Grid.SelectedLayer;
//
//    frmGoPhast.frameTopView.ItemChange(nil);
//    frmGoPhast.frameTopView.ZoomBox.Image32.Invalidate;
  end;
end;

procedure TSutraMesh2D.SetTopGridObserver(const Value: TObserver);
begin
  FTopGridObserver := Value;
  if Assigned(FTopGridObserver) then
  begin
    FTopGridObserver.UpToDate := False;
    FTopGridObserver.UpToDate := True;
  end;
end;

function TSutraMesh3D.TopContainingCellOrElement(APoint: TPoint2D;
  const EvaluatedAt: TEvaluatedAt): T2DTopCell;
begin
  result := Mesh2D.TopContainingCellOrElement(APoint, EvaluatedAt)
end;

procedure TSutraMesh3D.UpdateElementsInNodes;
var
  NodeIndex: Integer;
  ElementIndex: Integer;
begin
  for NodeIndex := 0 to Nodes.Count - 1 do
  begin
    Nodes[NodeIndex].FElements.Clear;
  end;
  for ElementIndex := 0 to Elements.Count - 1 do
  begin
    Elements[ElementIndex].UpdateNodes;
  end;
end;

procedure TSutraMesh3D.UpdateElevations;
var
  LocalModel: TPhastModel;
  LayerIndex: Integer;
  ColIndex: Integer;
  NodeLayerCount: Integer;
  LayerGroupIndex: Integer;
  LayerGroup: TSutraLayerGroup;
  DataArray: TDataArray;
  PriorLayerIndex: Integer;
  ANode: TSutraNode3D;
  IntermediateLayerIndex: Integer;
  HigherNode: TSutraNode3D;
  IntIndex: Integer;
  MiddleNode: TSutraNode3D;
  LayerFractions: TDoubleDynArray;
  UnitHeight: TFloat;
  Fraction: double;
  ElementLayerCount: integer;
  AnElement: TSutraElement3D;
  Element2D: TSutraElement2D;
  NodeItem: TSutraNodeNumber3D_Item;
  NodeIndex: Integer;
  ANode2D: TSutraNodeNumber2D_Item;
  Node3D: TSutraNode3D;
  Limits: TGridLimit;
  Width: Double;
  MidY: Extended;
  Active: Boolean;
  Node3D_1: TSutraNodeNumber3D_Item;
  Node3D_2: TSutraNodeNumber3D_Item;
  ElementIndex: Integer;
  NodeActive: Boolean;
  TempActive: Boolean;
begin
  if FUpdatingElevations then
  begin
    Exit;
  end;
  FUpdatingElevations := True;
  try
    frmGoPhast.PhastModel.UpdateDataSetDimensions;
    frmGoPhast.PhastModel.InvalidateScreenObjects;

    Elements.Clear;
    Nodes.Clear;
    BeginUpdate;
    try
      if Mesh2D.Nodes.Count = 0 then
      begin
        Exit;
      end;
      if MeshType = mt2D then
      begin
        Exit;
      end
      else
      begin
    //    try
        LocalModel := Model as TPhastModel;
        NodeLayerCount := LocalModel.SutraLayerStructure.NodeLayerCount;
        SetLength(FNodeArray, NodeLayerCount, Mesh2D.Nodes.Count);
        for LayerIndex := 0 to NodeLayerCount - 1 do
        begin
          for ColIndex := 0 to Mesh2D.Nodes.Count - 1 do
          begin
            FNodeArray[LayerIndex, ColIndex] := Nodes.Add;
            FNodeArray[LayerIndex, ColIndex].Node2D_Number := ColIndex;
          end;
        end;
        LayerIndex := -1;
        for LayerGroupIndex := 0 to LocalModel.SutraLayerStructure.Count - 1 do
        begin
          LayerGroup := LocalModel.SutraLayerStructure[LayerGroupIndex];
          LayerFractions := LocalModel.LayerFractions(LayerGroup);
          DataArray := LocalModel.DataArrayManager.GetDataSetByName(
            LayerGroup.DataArrayName);
          Assert(DataArray.Orientation = dsoTop);
          DataArray.Initialize;
          PriorLayerIndex := LayerIndex;
          LayerIndex := LayerIndex + LayerGroup.LayerCount;
          Assert(Length(LayerFractions) = LayerGroup.LayerCount-1);
          for ColIndex := 0 to Mesh2D.Nodes.Count - 1 do
          begin
            ANode := FNodeArray[LayerIndex, ColIndex];
            ANode.Z := DataArray.RealData[0, 0, ColIndex];
            if PriorLayerIndex >= 0 then
            begin
              HigherNode := FNodeArray[PriorLayerIndex, ColIndex];
              UnitHeight := HigherNode.Z - ANode.Z;
              if UnitHeight < 0 then
              begin
                ANode.Z := HigherNode.Z;
                UnitHeight := 0;
              end;
              for IntermediateLayerIndex := 0 to Length(LayerFractions) - 1 do
              begin
                IntIndex := PriorLayerIndex+IntermediateLayerIndex+1;
                Assert(IntIndex < LayerIndex);
                MiddleNode := FNodeArray[IntIndex, ColIndex];
                Fraction := LayerFractions[IntermediateLayerIndex];
                MiddleNode.Z := ANode.Z + UnitHeight*Fraction;
              end;
            end;
          end;
        end;
      end;

      for NodeIndex := 0 to Nodes.Count - 1 do
      begin
        Node3D := Nodes[NodeIndex];
        Node3D.Active := False;
      end;

      ElementLayerCount := LocalModel.SutraLayerStructure.ElementLayerCount;
        SetLength(FElementArray, ElementLayerCount, Mesh2D.Elements.Count);
      for LayerIndex := 0 to ElementLayerCount - 1 do
      begin
        for ColIndex := 0 to Mesh2D.Elements.Count - 1 do
        begin
          Element2D := Mesh2D.Elements[ColIndex];
          AnElement := Elements.Add;
          FElementArray[LayerIndex, ColIndex] := AnElement;
          for NodeIndex := 0 to Element2D.Nodes.Count - 1 do
          begin
            ANode2D := Element2D.Nodes[NodeIndex];
            Node3D := FNodeArray[LayerIndex, ANode2D.NodeNumber];
            NodeItem := AnElement.Nodes.Add;
            NodeItem.Node := Node3D;
          end;
          for NodeIndex := 0 to Element2D.Nodes.Count - 1 do
          begin
            ANode2D := Element2D.Nodes[NodeIndex];
            Node3D := FNodeArray[LayerIndex+1, ANode2D.NodeNumber];
            NodeItem := AnElement.Nodes.Add;
            NodeItem.Node := Node3D;
          end;
          Active := True;
          for NodeIndex := 0 to Element2D.Nodes.Count - 1 do
          begin
//            ANode2D := Element2D.Nodes[NodeIndex];
            Node3D_1 := AnElement.Nodes[NodeIndex];
            Node3D_2 := AnElement.Nodes[NodeIndex + Element2D.Nodes.Count];
            NodeActive := (Node3D_1.Node.Z > Node3D_2.Node.Z);
            Node3D_1.Node.Active := NodeActive;
            Node3D_2.Node.Active := NodeActive;
            if not NodeActive then
            begin
              Active := False;
            end;
          end;
          AnElement.Active := Active;
//          if Active then
//          begin
//            for NodeIndex := 0 to AnElement.Nodes.Count - 1 do
//            begin
//              AnElement.Nodes[NodeIndex].Node.Active := True;
//            end;
//          end;
        end;
      end;




      Limits := MeshLimits(vdTop);
      Width := Limits.MaxX - Limits.MinX;
      MidY := (Limits.MaxY + Limits.MinY)/2;
      CrossSection.StartX := Limits.MinX - Width/10;
      CrossSection.EndX := Limits.MaxX + Width/10;
      CrossSection.StartY := MidY;
      CrossSection.EndY := MidY;
    finally
      EndUpdate;
    end;
    UpdateElementsInNodes;

    for NodeIndex := 0 to Nodes.Count - 1 do
    begin
      Node3D := Nodes[NodeIndex];
      if Node3D.Active then
      begin
        TempActive := False;
        for ElementIndex := 0 to Node3D.FElements.Count - 1 do
        begin
          AnElement := Node3D.FElements[ElementIndex];
          if AnElement.Active then
          begin
            TempActive := True;
            break;
          end;
        end;
        Node3D.Active := TempActive;
      end;

    end;

    FActiveNodes.Clear;
    for NodeIndex := 0 to Nodes.Count - 1 do
    begin
      ANode := Nodes[NodeIndex];
      ANode.UpdateActiveElementList;
      if ANode.Active then
      begin
        FActiveNodes.Add(ANode);
      end;
    end;

    FActiveElements.Clear;
    for ElementIndex := 0 to Elements.Count - 1 do
    begin
      AnElement := Elements[ElementIndex];
      AnElement.UpdateActiveNodeList;
      if AnElement.Active then
      begin
        FActiveElements.Add(AnElement);
      end;
    end;

    RenumberMesh(self);

  finally
    FUpdatingElevations := False;
  end;
end;

function TSutraMesh3D._AddRef: Integer;
begin
  Result := -1;
end;

function TSutraMesh3D._Release: Integer;
begin
  Result := -1;
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
  Mesh := LocalModel.Mesh;
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
    frmGoPhast.PhastModel.InvalidateSegments;
    frmGoPhast.EnableVisualization;
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

{ TCrossSection }

function TCrossSection.Angle: Double;
begin
  result := ArcTan2(EndY-StartY, EndX-StartX);
end;

procedure TCrossSection.Assign(Source: TPersistent);
var
  SourceCS: TCrossSection;
begin
  if Source is TCrossSection then
  begin
    SourceCS := TCrossSection(Source);
    Segment := SourceCS.Segment;
    Color := SourceCS.Color;
  end
  else
  begin
    inherited;
  end;
end;

constructor TCrossSection.Create(Model: TBaseModel);
begin
  inherited;
  FColor := clFuchsia;
end;

procedure TCrossSection.Draw(const BitMap: TBitmap32);
var
  Points: GoPhastTypes.TPointArray;
  ZoomBox: TQRbwZoomBox2;
begin
  SetLength(Points, 2);
  ZoomBox := frmGoPhast.frameTopView.ZoomBox;
  Points[0] := ConvertTop2D_Point(ZoomBox, StartPoint);
  Points[1] := ConvertTop2D_Point(ZoomBox, EndPoint);
  DrawBigPolyline32(BitMap, Color32(Color), OrdinaryGridLineThickness,
    Points, True);
end;

function TCrossSection.GetEndPoint: TPoint2D;
begin
  Result.x := EndX;
  Result.y := EndY;
end;

function TCrossSection.GetSegement: TSegment2D;
begin
  result[1] := StartPoint;
  result[2] := EndPoint;
end;

function TCrossSection.GetStartPoint: TPoint2D;
begin
  Result.x := StartX;
  Result.y := StartY;
end;

procedure TCrossSection.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    InvalidateModel;
  end;
end;

procedure TCrossSection.SetEndPoint(const Value: TPoint2D);
begin
  EndX := Value.x;
  EndY := Value.y;
end;

procedure TCrossSection.SetEndX(const Value: Double);
begin
  SetRealProperty(FEndX, Value);
end;

procedure TCrossSection.SetEndY(const Value: double);
begin
  SetRealProperty(FEndY, Value);
end;

procedure TCrossSection.SetSegement(const Value: TSegment2D);
begin
  StartPoint := Value[1];
  EndPoint := Value[2];
end;

procedure TCrossSection.SetStartPoint(const Value: TPoint2D);
begin
  StartX := Value.x;
  StartY := Value.y;
end;

procedure TCrossSection.SetStartX(const Value: Double);
begin
  SetRealProperty(FStartX, Value);
end;

procedure TCrossSection.SetStartY(const Value: double);
begin
  SetRealProperty(FStartY, Value);
end;

{ TSutraNode2Comparer }

function TSutraNode2DComparer.Compare(const Left, Right: TSutraNode2D): Integer;
var
  Angle: double;
  LeftX: TFloat;
  RightX: TFloat;
begin
  Angle := ArcTan2(Left.y - FStartPoint.y, Left.x - FStartPoint.x) - FAngle;
  LeftX := Distance(FStartPoint, Left.Location)*Cos(Angle);
  Angle := ArcTan2(Right.y - FStartPoint.y, Right.x - FStartPoint.x) - FAngle;
  RightX := Distance(FStartPoint, Right.Location)*Cos(Angle);
  result := Sign(LeftX - RightX);
end;

constructor TSutraNode2DComparer.Create(CrossSectionAngle: Double;
  StartPoint: TPoint2D);
begin
  FStartPoint := StartPoint;
  FAngle := CrossSectionAngle;
end;

{ TSutraElement2DComparer }

function TSutraElement2DComparer.Compare(const Left,
  Right: TSutraElement2D): Integer;
var
  Angle: double;
  LeftX: TFloat;
  RightX: TFloat;
  CenterPoint: TPoint2D;
begin
  CenterPoint := Left.Center;
  Angle := ArcTan2(CenterPoint.y - FStartPoint.y, CenterPoint.x - FStartPoint.x) - FAngle;
  LeftX := Distance(FStartPoint, CenterPoint)*Cos(Angle);

  CenterPoint := Right.Center;
  Angle := ArcTan2(CenterPoint.y - FStartPoint.y, CenterPoint.x - FStartPoint.x) - FAngle;
  RightX := Distance(FStartPoint, CenterPoint)*Cos(Angle);

  result := Sign(LeftX - RightX);
end;

constructor TSutraElement2DComparer.Create(CrossSectionAngle: Double;
  StartPoint: TPoint2D);
begin
  FStartPoint := StartPoint;
  FAngle := CrossSectionAngle;
end;

end.
